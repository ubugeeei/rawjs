pub(super) fn execute_operator_instruction(
    vm: &mut Vm,
    instr: Instruction,
) -> Result<InstructionOutcome> {
    match instr {
        Instruction::Add => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(js_add(&lhs, &rhs));
        }
        Instruction::Sub => binary_number_op(vm, |lhs, rhs| lhs - rhs)?,
        Instruction::Mul => binary_number_op(vm, |lhs, rhs| lhs * rhs)?,
        Instruction::Div => binary_number_op(vm, |lhs, rhs| lhs / rhs)?,
        Instruction::Mod => binary_number_op(vm, |lhs, rhs| lhs % rhs)?,
        Instruction::Exp => binary_number_op(vm, |lhs, rhs| lhs.powf(rhs))?,
        Instruction::BitAnd => binary_i32_op(vm, |lhs, rhs| lhs & rhs)?,
        Instruction::BitOr => binary_i32_op(vm, |lhs, rhs| lhs | rhs)?,
        Instruction::BitXor => binary_i32_op(vm, |lhs, rhs| lhs ^ rhs)?,
        Instruction::Shl => shift_i32_op(vm, |lhs, rhs| lhs << rhs)?,
        Instruction::Shr => shift_i32_op(vm, |lhs, rhs| lhs >> rhs)?,
        Instruction::UShr => shift_u32_op(vm)?,
        Instruction::Eq => binary_bool_op(vm, |lhs, rhs| lhs.abstract_eq(&rhs))?,
        Instruction::StrictEq => binary_bool_op(vm, |lhs, rhs| lhs.strict_eq(&rhs))?,
        Instruction::Ne => binary_bool_op(vm, |lhs, rhs| !lhs.abstract_eq(&rhs))?,
        Instruction::StrictNe => binary_bool_op(vm, |lhs, rhs| !lhs.strict_eq(&rhs))?,
        Instruction::Lt => relational_bool_op(vm, JsValue::lt)?,
        Instruction::Le => relational_bool_op(vm, JsValue::le)?,
        Instruction::Gt => relational_bool_op(vm, JsValue::gt)?,
        Instruction::Ge => relational_bool_op(vm, JsValue::ge)?,
        Instruction::Not => unary_bool_op(vm, |value| !value.to_boolean())?,
        Instruction::BitNot => unary_number_op(vm, |value| (!value.to_int32()) as f64)?,
        Instruction::Neg => unary_number_op(vm, |value| -value.to_number())?,
        Instruction::Pos => unary_number_op(vm, |value| value.to_number())?,
        Instruction::TypeOf => {
            let val = vm.pop()?;
            vm.push(JsValue::string(val.type_of()));
        }
        Instruction::Void => {
            vm.pop()?;
            vm.push(JsValue::Undefined);
        }
        Instruction::Delete => {
            vm.pop()?;
            vm.push(JsValue::Boolean(true));
        }
        Instruction::DeleteName(name_idx) => {
            let name = current_constant_string(vm, name_idx)?;
            vm.delete_global(&name);
            vm.push(JsValue::Boolean(true));
        }
        Instruction::DeleteProperty => {
            let key = vm.pop()?;
            let obj_val = vm.pop()?;
            let deleted = delete_property(vm, obj_val, key);
            vm.push(JsValue::Boolean(deleted));
        }
        _ => return Ok(InstructionOutcome::Unhandled),
    }
    Ok(InstructionOutcome::Handled(None))
}

pub(super) fn execute_control_instruction(
    vm: &mut Vm,
    instr: Instruction,
) -> Result<InstructionOutcome> {
    match instr {
        Instruction::Jump(offset) => jump_relative(vm, offset),
        Instruction::JumpIfFalse(offset) => jump_if(vm, offset, false)?,
        Instruction::JumpIfTrue(offset) => jump_if(vm, offset, true)?,
        Instruction::JumpIfNullish(offset) => jump_if_nullish(vm, offset)?,
        Instruction::Call(argc) => {
            exec_call(vm, argc as usize, vm.global_this_value())?;
        }
        Instruction::New(argc) => {
            exec_new(vm, argc as usize)?;
        }
        Instruction::CallMethod(argc) => execute_call_method(vm, argc as usize)?,
        Instruction::Return => return Ok(InstructionOutcome::Handled(exec_return(vm)?)),
        Instruction::CreateClosure(const_idx) => {
            exec_create_closure(vm, const_idx)?;
        }
        _ => return Ok(InstructionOutcome::Unhandled),
    }
    Ok(InstructionOutcome::Handled(None))
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
