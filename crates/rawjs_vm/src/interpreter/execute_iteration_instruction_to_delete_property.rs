pub(super) fn execute_iteration_instruction(
    vm: &mut Vm,
    instr: Instruction,
) -> Result<InstructionOutcome> {
    match instr {
        Instruction::GetIterator => {
            let iterable = vm.pop()?;
            let iterator = create_iterator_value(vm, iterable)?;
            vm.push(iterator);
        }
        Instruction::IteratorNext => iterator_next(vm)?,
        Instruction::IteratorDone(exit_offset) => iterator_done(vm, exit_offset)?,
        Instruction::ForInInit => init_for_in(vm)?,
        Instruction::ForInNext(exit_offset) => advance_for_in(vm, exit_offset)?,
        Instruction::PostfixIncrement => postfix_update(vm, 1.0)?,
        Instruction::PostfixDecrement => postfix_update(vm, -1.0)?,
        _ => return Ok(InstructionOutcome::Unhandled),
    }
    Ok(InstructionOutcome::Handled(None))
}

pub(super) fn current_constant_string(vm: &Vm, index: u16) -> Result<String> {
    let frame = vm.call_stack.last().unwrap();
    get_constant_string(&vm.chunks[frame.chunk_index], index)
}

pub(super) fn current_frame_is_strict(vm: &Vm) -> bool {
    vm.call_stack
        .last()
        .map(|frame| frame.is_strict)
        .unwrap_or(false)
}

pub(super) fn binary_number_op<F>(vm: &mut Vm, op: F) -> Result<()>
where
    F: FnOnce(f64, f64) -> f64,
{
    let rhs = vm.pop()?;
    let lhs = vm.pop()?;
    vm.push(JsValue::Number(op(lhs.to_number(), rhs.to_number())));
    Ok(())
}

pub(super) fn binary_i32_op<F>(vm: &mut Vm, op: F) -> Result<()>
where
    F: FnOnce(i32, i32) -> i32,
{
    let rhs = vm.pop()?;
    let lhs = vm.pop()?;
    vm.push(JsValue::Number(op(lhs.to_int32(), rhs.to_int32()) as f64));
    Ok(())
}

pub(super) fn shift_i32_op<F>(vm: &mut Vm, op: F) -> Result<()>
where
    F: FnOnce(i32, u32) -> i32,
{
    let rhs = vm.pop()?;
    let lhs = vm.pop()?;
    let shift = rhs.to_uint32() & 0x1f;
    vm.push(JsValue::Number(op(lhs.to_int32(), shift) as f64));
    Ok(())
}

pub(super) fn shift_u32_op(vm: &mut Vm) -> Result<()> {
    let rhs = vm.pop()?;
    let lhs = vm.pop()?;
    let shift = rhs.to_uint32() & 0x1f;
    vm.push(JsValue::Number((lhs.to_uint32() >> shift) as f64));
    Ok(())
}

pub(super) fn binary_bool_op<F>(vm: &mut Vm, op: F) -> Result<()>
where
    F: FnOnce(JsValue, JsValue) -> bool,
{
    let rhs = vm.pop()?;
    let lhs = vm.pop()?;
    vm.push(JsValue::Boolean(op(lhs, rhs)));
    Ok(())
}

pub(super) fn relational_bool_op(vm: &mut Vm, op: fn(&JsValue, &JsValue) -> JsValue) -> Result<()> {
    let rhs = vm.pop()?;
    let lhs = vm.pop()?;
    vm.push(op(&lhs, &rhs));
    Ok(())
}

pub(super) fn unary_bool_op<F>(vm: &mut Vm, op: F) -> Result<()>
where
    F: FnOnce(JsValue) -> bool,
{
    let value = vm.pop()?;
    vm.push(JsValue::Boolean(op(value)));
    Ok(())
}

pub(super) fn unary_number_op<F>(vm: &mut Vm, op: F) -> Result<()>
where
    F: FnOnce(JsValue) -> f64,
{
    let value = vm.pop()?;
    vm.push(JsValue::Number(op(value)));
    Ok(())
}

pub(super) fn delete_property(vm: &mut Vm, obj_val: JsValue, key: JsValue) -> bool {
    match (&obj_val, &key) {
        (JsValue::Object(ptr), JsValue::Symbol(sym)) => {
            ptr.borrow_mut().symbol_properties.remove(&sym.id).is_some()
        }
        (JsValue::Object(ptr), _) => {
            let key_str = key.to_string_value();
            let deleted = ptr.borrow_mut().delete_property(&key_str);
            if let Some(global_obj) = &vm.global_object {
                if ptr.ptr_eq(global_obj) {
                    vm.globals.remove(&key_str);
                }
            }
            deleted
        }
        _ => true,
    }
}

use super::*;
