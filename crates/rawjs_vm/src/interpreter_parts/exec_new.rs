pub(crate) fn exec_new(vm: &mut Vm, argc: usize) -> Result<()> {
    let stack_len = vm.value_stack.len();
    if stack_len < argc + 1 {
        return Err(RawJsError::internal_error("stack underflow in New"));
    }
    let args: Vec<JsValue> = vm.value_stack.drain(stack_len - argc..).collect();
    let callee = vm.pop()?;
    if is_promise_constructor(vm, &callee) {
        return exec_promise_constructor(vm, &args);
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
            let Some(func) = func else {
                return Err(RawJsError::type_error("object is not a constructor"));
            };
            let this_value = create_constructor_target(vm, ptr);
            match &func.kind {
                rawjs_runtime::FunctionKind::Native(native_fn) => {
                    let native_fn = *native_fn;
                    vm.heap.calling_fn = Some(ptr.clone());
                    let result = native_fn(&mut vm.heap, &this_value, &args)?;
                    vm.heap.calling_fn = None;
                    if result.is_object() {
                        vm.push(result);
                    } else {
                        vm.push(this_value);
                    }
                    Ok(())
                }
                rawjs_runtime::FunctionKind::Bytecode { chunk_index } => {
                    let chunk_index = *chunk_index;
                    if vm.chunks[chunk_index].is_generator || vm.chunks[chunk_index].is_async {
                        return Err(RawJsError::type_error("function is not a constructor"));
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
                        this_value: this_value.clone(),
                    };
                    vm.call_stack.push(frame);
                    vm.construct_frames
                        .push((vm.call_stack.len(), this_value.clone()));
                    if count >= JIT_THRESHOLD && vm.has_jit(chunk_index) {
                        let result = unsafe { vm.call_jit_new(chunk_index) };
                        if result != 0 {
                            return Err(vm.jit_error.take().unwrap_or_else(|| {
                                RawJsError::internal_error("JIT execution failed")
                            }));
                        }
                    }
                    Ok(())
                }
            }
        }
        _ => Err(RawJsError::type_error(format!(
            "{} is not a constructor",
            callee.type_of()
        ))),
    }
}
