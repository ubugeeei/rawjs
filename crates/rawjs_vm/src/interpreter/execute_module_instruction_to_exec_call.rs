pub(super) fn execute_module_instruction(
    vm: &mut Vm,
    instr: Instruction,
) -> Result<InstructionOutcome> {
    match instr {
        Instruction::ImportModule(source_idx) => {
            let source = current_constant_string(vm, source_idx)?;
            let ns = vm.execute_module(&source)?;
            vm.push(JsValue::Object(ns));
        }
        Instruction::ImportModuleDynamic => {
            import_module_dynamic(vm)?;
        }
        Instruction::ImportMeta => {
            let import_meta = vm.create_import_meta_object()?;
            vm.push(JsValue::Object(import_meta));
        }
        Instruction::ImportBinding(name_idx) => {
            let name = current_constant_string(vm, name_idx)?;
            let ns = vm.pop()?;
            let value = match &ns {
                JsValue::Object(ptr) => ptr.borrow().get_property(&name),
                _ => JsValue::Undefined,
            };
            vm.push(value);
        }
        Instruction::ExportBinding(name_idx) => export_binding(vm, name_idx)?,
        Instruction::ExportDefault => export_default(vm)?,
        Instruction::CreateGenerator => {
            return Err(RawJsError::internal_error(
                "CreateGenerator should not be executed directly",
            ));
        }
        Instruction::Yield => return Ok(InstructionOutcome::Handled(exec_yield(vm)?)),
        Instruction::Await => return Ok(InstructionOutcome::Handled(exec_await(vm)?)),
        Instruction::DisposeResource(slot) => exec_dispose_resource(vm, slot)?,
        Instruction::AsyncDisposeResource(_) => {
            return Err(RawJsError::internal_error(
                "AsyncDisposeResource instruction reached outside async frame",
            ));
        }
        _ => return Ok(InstructionOutcome::Unhandled),
    }
    Ok(InstructionOutcome::Handled(None))
}

pub(super) fn finish_frame_return(vm: &mut Vm, mut return_value: JsValue) -> JsValue {
    let depth = vm.call_stack.len();
    if matches!(
        vm.construct_frames.last(), Some((construct_depth, _)) if * construct_depth ==
        depth
    ) {
        let (_, target) = vm.construct_frames.pop().unwrap();
        if !return_value.is_object() {
            return_value = target;
        }
    }
    let frame = vm.call_stack.pop().unwrap();
    vm.value_stack.truncate(frame.base);
    return_value
}

pub(super) fn create_constructor_target(
    vm: &mut Vm,
    callee_ptr: &rawjs_runtime::GcPtr<JsObject>,
) -> JsValue {
    let constructor_prototype = {
        let callee = callee_ptr.borrow();
        match callee.get_property("prototype") {
            JsValue::Object(proto) => Some(proto),
            _ => vm.object_prototype.clone(),
        }
    };
    let obj = match constructor_prototype {
        Some(proto) => JsObject::with_prototype(proto),
        None => JsObject::ordinary(),
    };
    let ptr = vm.heap.alloc(obj);
    JsValue::Object(ptr)
}

/// Execute a function call with `argc` arguments on the stack.
///
/// Stack layout before call:
///   `[..., function, arg0, arg1, ..., argN-1]`
pub(crate) fn exec_call(vm: &mut Vm, argc: usize, this_value: JsValue) -> Result<()> {
    let stack_len = vm.value_stack.len();
    if stack_len < argc + 1 {
        return Err(RawJsError::internal_error("stack underflow in Call"));
    }
    let args: Vec<JsValue> = vm.value_stack.drain(stack_len - argc..).collect();
    let callee = vm.pop()?;
    if is_promise_constructor(vm, &callee) {
        return exec_promise_constructor(vm, &args);
    }
    if is_eval_function(vm, &callee) {
        return exec_eval(vm, &args);
    }
    match &callee {
        JsValue::Object(ptr) => {
            let func = {
                let obj = ptr.borrow();
                match &obj.internal {
                    ObjectInternal::Function(f) => Some(f.clone()),
                    _ => None,
                }
            };
            if let Some(func) = func {
                match &func.kind {
                    rawjs_runtime::FunctionKind::Native(native_fn) => {
                        let native_fn = *native_fn;
                        vm.heap.calling_fn = Some(ptr.clone());
                        let result = native_fn(&mut vm.heap, &this_value, &args)?;
                        vm.heap.calling_fn = None;
                        vm.push(result);
                        Ok(())
                    }
                    rawjs_runtime::FunctionKind::Bytecode { chunk_index } => {
                        let chunk_index = *chunk_index;
                        if vm.chunks[chunk_index].is_generator && vm.chunks[chunk_index].is_async {
                            return exec_create_async_generator(
                                vm,
                                chunk_index,
                                &args,
                                func.upvalues.clone(),
                                this_value,
                            );
                        }
                        if vm.chunks[chunk_index].is_generator {
                            return exec_create_generator(
                                vm,
                                chunk_index,
                                &args,
                                func.upvalues.clone(),
                                this_value,
                            );
                        }
                        if vm.chunks[chunk_index].is_async {
                            return exec_async_call(
                                vm,
                                chunk_index,
                                &args,
                                func.upvalues.clone(),
                                this_value,
                            );
                        }
                        let count = vm.bump_execution_count(chunk_index);
                        if count == JIT_THRESHOLD {
                            let _ = vm.try_jit_compile(chunk_index);
                        }
                        let param_count = vm.chunks[chunk_index].param_count as usize;
                        let local_count = vm.chunks[chunk_index].local_count as usize;
                        let slot_count = local_count.max(param_count);
                        let mut locals = vec![JsValue::Undefined; slot_count];
                        for (i, arg) in args.iter().enumerate() {
                            if i < slot_count {
                                locals[i] = arg.clone();
                            }
                        }
                        let frame = CallFrame {
                            chunk_index,
                            ip: 0,
                            base: vm.value_stack.len(),
                            locals,
                            arguments: args.clone(),
                            arguments_object: None,
                            callee: Some(ptr.clone()),
                            is_strict: vm.chunks[chunk_index].is_strict,
                            upvalues: func.upvalues.clone(),
                            this_value,
                        };
                        vm.call_stack.push(frame);
                        if count >= JIT_THRESHOLD && vm.has_jit(chunk_index) {
                            let result = unsafe { vm.call_jit_new(chunk_index) };
                            if result != 0 {
                                return Err(vm.jit_error.take().unwrap_or_else(|| {
                                    RawJsError::internal_error("JIT execution failed")
                                }));
                            }
                            return Ok(());
                        }
                        Ok(())
                    }
                }
            } else {
                Err(RawJsError::type_error("object is not a function"))
            }
        }
        _ => Err(RawJsError::type_error(format!(
            "{} is not a function",
            callee.type_of()
        ))),
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
