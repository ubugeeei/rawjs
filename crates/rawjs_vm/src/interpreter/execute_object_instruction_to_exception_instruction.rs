pub(super) fn execute_object_instruction(
    vm: &mut Vm,
    instr: Instruction,
) -> Result<InstructionOutcome> {
    match instr {
        Instruction::CreateObject => {
            let object = create_ordinary_object(vm);
            vm.push(JsValue::Object(object));
        }
        Instruction::CreateArray(count) => {
            let array = create_array_object(vm, count)?;
            vm.push(JsValue::Object(array));
        }
        Instruction::GetProperty(name_idx) => {
            let name = current_constant_string(vm, name_idx)?;
            let obj_val = vm.pop()?;
            let value = get_property_value(vm, &obj_val, &name)?;
            vm.push(value);
        }
        Instruction::SetProperty(name_idx) => {
            let name = current_constant_string(vm, name_idx)?;
            let value = vm.pop()?;
            let obj_val = vm.pop()?;
            let is_strict = current_frame_is_strict(vm);
            set_property_value(vm, &obj_val, &name, &value, is_strict)?;
            mirror_global_property(vm, &obj_val, &name, &value);
            vm.push(value);
        }
        Instruction::GetIndex => {
            let key = vm.pop()?.to_js_string();
            let obj_val = vm.pop()?;
            let value = get_property_value(vm, &obj_val, &key)?;
            vm.push(value);
        }
        Instruction::SetIndex => {
            let value = vm.pop()?;
            let key = vm.pop()?.to_js_string();
            let obj_val = vm.pop()?;
            let is_strict = current_frame_is_strict(vm);
            set_property_value(vm, &obj_val, &key, &value, is_strict)?;
            mirror_global_property(vm, &obj_val, &key, &value);
            vm.push(value);
        }
        Instruction::GetComputed => {
            let key = vm.pop()?;
            let obj_val = vm.pop()?;
            let value = get_computed_value(vm, obj_val, key)?;
            vm.push(value);
        }
        Instruction::SetComputed => {
            let value = vm.pop()?;
            let key = vm.pop()?;
            let obj_val = vm.pop()?;
            set_computed_value(vm, obj_val, key, value)?;
        }
        Instruction::LoadArguments => {
            load_arguments_object(vm)?;
        }
        _ => return Ok(InstructionOutcome::Unhandled),
    }
    Ok(InstructionOutcome::Handled(None))
}

pub(super) fn execute_exception_instruction(
    vm: &mut Vm,
    instr: Instruction,
) -> Result<InstructionOutcome> {
    match instr {
        Instruction::Throw => {
            let value = vm.pop()?;
            vm.thrown_value = Some(value.clone());
            return Err(RawJsError::internal_error(format!("{}", value)));
        }
        Instruction::EnterTry(catch_offset, finally_offset) => {
            let frame = vm.call_stack.last().unwrap();
            let current_ip = frame.ip;
            let catch_ip = offset_to_ip(current_ip, catch_offset);
            let finally_ip = offset_to_ip(current_ip, finally_offset);
            vm.try_stack.push(TryContext {
                catch_ip,
                finally_ip,
                stack_depth: vm.value_stack.len(),
                call_depth: vm.call_stack.len(),
            });
        }
        Instruction::LeaveTry => {
            vm.try_stack.pop();
        }
        Instruction::In => {
            let obj_val = vm.pop()?;
            let key = vm.pop()?.to_js_string();
            let result = match &obj_val {
                JsValue::Object(ptr) => ptr.borrow().has_own_property(&key),
                _ => false,
            };
            vm.push(JsValue::Boolean(result));
        }
        Instruction::Instanceof => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Boolean(lhs.instance_of(&rhs)));
        }
        _ => return Ok(InstructionOutcome::Unhandled),
    }
    Ok(InstructionOutcome::Handled(None))
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
