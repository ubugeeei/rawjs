/// Run the interpreter for a generator frame until it yields or returns.
pub(super) fn run_generator_frame(vm: &mut Vm, target_depth: usize) -> Result<JsValue> {
    loop {
        if vm.call_stack.len() <= target_depth {
            return vm.pop().or(Ok(JsValue::Undefined));
        }
        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;
        let is_target_frame = vm.call_stack.len() == target_depth + 1;
        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            if !is_target_frame {
                let implicit_return = finish_frame_return(vm, JsValue::Undefined);
                vm.push(implicit_return);
                continue;
            }
            let frame = vm.call_stack.pop().unwrap();
            vm.value_stack.truncate(frame.base);
            if let JsValue::Object(ref gen_ptr) = frame.this_value {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                }
            }
            return Ok(make_iterator_result(vm, JsValue::Undefined, true));
        }
        let instruction = vm.chunks[chunk_index].instructions[ip];
        vm.call_stack[frame_idx].ip = ip + 1;
        match instruction {
            Instruction::Return => {
                if !is_target_frame {
                    match execute_instruction(vm, instruction) {
                        Ok(Some(value)) => {
                            vm.push(value);
                        }
                        Ok(None) => {}
                        Err(err) => {
                            ensure_thrown_value(vm, &err);
                            if !unwind_exception(vm, &err)? {
                                return Err(err);
                            }
                        }
                    }
                    continue;
                }
                let return_value = if vm.value_stack.len() > vm.call_stack.last().unwrap().base {
                    vm.pop()?
                } else {
                    JsValue::Undefined
                };
                let frame = vm.call_stack.pop().unwrap();
                vm.value_stack.truncate(frame.base);
                if let JsValue::Object(ref gen_ptr) = frame.this_value {
                    let mut gen_obj = gen_ptr.borrow_mut();
                    if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                        state.status = GeneratorStatus::Completed;
                    }
                }
                return Ok(make_iterator_result(vm, return_value, true));
            }
            Instruction::Yield => {
                exec_yield(vm)?;
                return vm.pop().or(Ok(JsValue::Undefined));
            }
            _ => match execute_instruction(vm, instruction) {
                Ok(Some(value)) => {
                    return Ok(make_iterator_result(vm, value, true));
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

/// Implement generator `.return(value)`.
pub(super) fn generator_return(
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    vm: &mut Vm,
    value: JsValue,
) -> JsValue {
    let mut gen_obj = gen_ptr.borrow_mut();
    if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
        state.status = GeneratorStatus::Completed;
    }
    drop(gen_obj);
    make_iterator_result(vm, value, true)
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
