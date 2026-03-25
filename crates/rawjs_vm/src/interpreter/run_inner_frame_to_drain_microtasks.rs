/// Run the interpreter until the call stack returns to `target_depth`.
///
/// This is a generalized version of `run_module_frame` used by the JIT
/// when a stub (e.g. `stub_call`) needs the interpreter to execute a
/// pushed call frame to completion.
pub fn run_inner_frame(vm: &mut Vm, target_depth: usize) -> Result<()> {
    loop {
        if vm.call_stack.len() <= target_depth {
            return Ok(());
        }
        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;
        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            let implicit_return = finish_frame_return(vm, JsValue::Undefined);
            if vm.call_stack.len() <= target_depth {
                return Ok(());
            }
            vm.push(implicit_return);
            continue;
        }
        let instruction = vm.chunks[chunk_index].instructions[ip];
        vm.call_stack[frame_idx].ip = ip + 1;
        match execute_instruction(vm, instruction) {
            Ok(Some(_value)) => {
                if vm.call_stack.len() <= target_depth {
                    return Ok(());
                }
            }
            Ok(None) => {}
            Err(err) => {
                ensure_thrown_value(vm, &err);
                if !unwind_exception(vm, &err)? {
                    return Err(err);
                }
            }
        }
    }
}

/// Drain all pending microtasks (Promise reactions).
pub(super) fn drain_microtasks(vm: &mut Vm) -> Result<()> {
    loop {
        let tasks: Vec<_> = vm.heap.pending_microtasks.drain(..).collect();
        if tasks.is_empty() {
            break;
        }
        for task in tasks {
            if let JsValue::Object(fn_ptr) = &task.callback {
                let func = {
                    let obj = fn_ptr.borrow();
                    match &obj.internal {
                        ObjectInternal::Function(f) => Some(f.clone()),
                        _ => None,
                    }
                };
                if let Some(func) = func {
                    let async_gen_ptr = {
                        let fn_obj = fn_ptr.borrow();
                        let gen_val = fn_obj.get_property("__async_gen_ptr__");
                        match gen_val {
                            JsValue::Object(ptr) => Some(ptr),
                            _ => None,
                        }
                    };
                    if let Some(gen_ptr) = async_gen_ptr {
                        let is_throw = func.name == "asyncReject";
                        let _ = async_resume(vm, &gen_ptr, task.arg, is_throw);
                        continue;
                    }
                    match &func.kind {
                        rawjs_runtime::FunctionKind::Native(native_fn) => {
                            let native_fn = *native_fn;
                            vm.heap.calling_fn = Some(fn_ptr.clone());
                            let result = native_fn(&mut vm.heap, &JsValue::Undefined, &[task.arg]);
                            vm.heap.calling_fn = None;
                            settle_microtask_target(vm, task.target_promise.clone(), result)?;
                        }
                        rawjs_runtime::FunctionKind::Bytecode { chunk_index } => {
                            let chunk_index = *chunk_index;
                            let param_count = vm.chunks[chunk_index].param_count as usize;
                            let local_count = vm.chunks[chunk_index].local_count as usize;
                            let slot_count = local_count.max(param_count);
                            let mut locals = vec![JsValue::Undefined; slot_count];
                            if !locals.is_empty() {
                                locals[0] = task.arg.clone();
                            }
                            let frame = CallFrame {
                                chunk_index,
                                ip: 0,
                                base: vm.value_stack.len(),
                                locals,
                                arguments: vec![task.arg.clone()],
                                arguments_object: None,
                                callee: Some(fn_ptr.clone()),
                                is_strict: vm.chunks[chunk_index].is_strict,
                                upvalues: func.upvalues.clone(),
                                this_value: JsValue::Undefined,
                            };
                            vm.call_stack.push(frame);
                            let result = run_microtask_frame(vm);
                            settle_microtask_target(vm, task.target_promise.clone(), result)?;
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
