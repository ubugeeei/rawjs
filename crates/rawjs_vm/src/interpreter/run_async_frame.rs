/// Run the interpreter for an async function frame until it awaits, returns, or throws.
pub(super) fn run_async_frame(
    vm: &mut Vm,
    target_depth: usize,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    result_promise: &Option<rawjs_runtime::GcPtr<JsObject>>,
) -> Result<()> {
    let try_stack_base = vm.try_stack.len();
    loop {
        if vm.call_stack.len() <= target_depth {
            if let Some(promise) = result_promise {
                let result = vm.value_stack.pop().unwrap_or(JsValue::Undefined);
                clear_generator_result_promise(gen_ptr);
                rawjs_runtime::builtins::resolve_promise_with_heap(promise, result, &mut vm.heap);
            }
            return Ok(());
        }
        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;
        let is_async_frame = vm.call_stack.len() == target_depth + 1;
        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            if !is_async_frame {
                let implicit_return = finish_frame_return(vm, JsValue::Undefined);
                vm.push(implicit_return);
                continue;
            }
            let frame = vm.call_stack.pop().unwrap();
            vm.value_stack.truncate(frame.base);
            let result_value = if vm.chunks[chunk_index].is_generator {
                make_iterator_result(vm, JsValue::Undefined, true)
            } else {
                JsValue::Undefined
            };
            {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                    state.result_promise = None;
                }
            }
            if let Some(ref promise) = result_promise {
                rawjs_runtime::builtins::resolve_promise_with_heap(
                    promise,
                    result_value,
                    &mut vm.heap,
                );
            }
            return Ok(());
        }
        let instruction = vm.chunks[chunk_index].instructions[ip];
        vm.call_stack[frame_idx].ip = ip + 1;
        match instruction {
            Instruction::Return => {
                if !is_async_frame {
                    match execute_instruction(vm, instruction) {
                        Ok(Some(_value)) => return Ok(()),
                        Ok(None) => continue,
                        Err(err) => {
                            ensure_thrown_value(vm, &err);
                            if !unwind_exception(vm, &err)? {
                                return Err(err);
                            }
                            continue;
                        }
                    }
                }
                let return_value = if vm.value_stack.len() > vm.call_stack.last().unwrap().base {
                    vm.pop()?
                } else {
                    JsValue::Undefined
                };
                let result_value = if vm.chunks[chunk_index].is_generator {
                    make_iterator_result(vm, return_value, true)
                } else {
                    return_value
                };
                let frame = vm.call_stack.pop().unwrap();
                vm.value_stack.truncate(frame.base);
                {
                    let mut gen_obj = gen_ptr.borrow_mut();
                    if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                        state.status = GeneratorStatus::Completed;
                        state.result_promise = None;
                    }
                }
                if let Some(ref promise) = result_promise {
                    rawjs_runtime::builtins::resolve_promise_with_heap(
                        promise,
                        result_value,
                        &mut vm.heap,
                    );
                }
                return Ok(());
            }
            Instruction::Await => {
                if !is_async_frame {
                    return Err(RawJsError::internal_error(
                        "Await instruction reached outside async frame",
                    ));
                }
                let awaited_value = vm.pop()?;
                suspend_async_frame(vm, gen_ptr, try_stack_base, awaited_value)?;
                return Ok(());
            }
            Instruction::AsyncDisposeResource(slot) => {
                if !is_async_frame {
                    return Err(RawJsError::internal_error(
                        "AsyncDisposeResource instruction reached outside async frame",
                    ));
                }
                match prepare_dispose_resource(vm, slot, true)? {
                    DisposeResult::Complete => continue,
                    DisposeResult::Await(awaited_value) => {
                        suspend_async_frame(vm, gen_ptr, try_stack_base, awaited_value)?;
                        return Ok(());
                    }
                }
            }
            _ => match execute_instruction(vm, instruction) {
                Ok(Some(_value)) => {
                    return Ok(());
                }
                Ok(None) => {}
                Err(err) => {
                    ensure_thrown_value(vm, &err);
                    if !unwind_exception(vm, &err)? {
                        return Err(err);
                    }
                }
            },
        }
    }
}

use super::*;
