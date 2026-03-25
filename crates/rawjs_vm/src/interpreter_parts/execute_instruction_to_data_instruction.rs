/// Execute a single instruction.
///
/// Returns:
///   - `Ok(Some(value))` when the *top-level* frame executes a `Return`,
///     signalling the end of the program.
///   - `Ok(None)` for normal continuation.
///   - `Err(e)` for runtime errors (type errors, reference errors, etc.).
fn execute_instruction(vm: &mut Vm, instr: Instruction) -> Result<Option<JsValue>> {
    for outcome in [
        execute_data_instruction(vm, instr)?,
        execute_operator_instruction(vm, instr)?,
        execute_control_instruction(vm, instr)?,
        execute_object_instruction(vm, instr)?,
        execute_exception_instruction(vm, instr)?,
        execute_iteration_instruction(vm, instr)?,
        execute_module_instruction(vm, instr)?,
    ] {
        if let InstructionOutcome::Handled(result) = outcome {
            return Ok(result);
        }
    }
    Err(RawJsError::internal_error(format!(
        "unsupported instruction dispatch: {instr:?}"
    )))
}

fn execute_data_instruction(vm: &mut Vm, instr: Instruction) -> Result<InstructionOutcome> {
    match instr {
        Instruction::LoadConst(idx) => {
            let frame = vm.call_stack.last().unwrap();
            let chunk = &vm.chunks[frame.chunk_index];
            let value = constant_to_value(&chunk.constants[idx as usize]);
            vm.push(value);
        }
        Instruction::Undefined => vm.push(JsValue::Undefined),
        Instruction::Null => vm.push(JsValue::Null),
        Instruction::True => vm.push(JsValue::Boolean(true)),
        Instruction::False => vm.push(JsValue::Boolean(false)),
        Instruction::This => {
            let this_val = vm.call_stack.last().unwrap().this_value.clone();
            vm.push(this_val);
        }
        Instruction::LoadLocal(idx) => {
            let value = vm.call_stack.last().unwrap().locals[idx as usize].clone();
            vm.push(value);
        }
        Instruction::StoreLocal(idx) => {
            let value = vm.pop()?;
            let slot = idx as usize;
            let arguments_object = {
                let frame = vm.call_stack.last_mut().unwrap();
                if slot >= frame.locals.len() {
                    frame.locals.resize(slot + 1, JsValue::Undefined);
                }
                frame.locals[slot] = value.clone();
                frame.arguments_object.clone()
            };
            if let Some(arguments_obj) = arguments_object {
                sync_arguments_object_from_local(arguments_obj, slot as u16, value);
            }
        }
        Instruction::LoadGlobal(name_idx) => {
            let name = current_constant_string(vm, name_idx)?;
            let value = vm
                .get_global(&name)
                .cloned()
                .ok_or_else(|| RawJsError::reference_error(format!("{name} is not defined")))?;
            vm.push(value);
        }
        Instruction::LoadGlobalOrUndefined(name_idx) => {
            let name = current_constant_string(vm, name_idx)?;
            vm.push(vm.get_global(&name).cloned().unwrap_or(JsValue::Undefined));
        }
        Instruction::StoreGlobal(name_idx) => {
            let name = current_constant_string(vm, name_idx)?;
            let value = vm.pop()?;
            let is_strict = current_frame_is_strict(vm);
            if is_strict && vm.get_global(&name).is_none() {
                return Err(RawJsError::reference_error(format!(
                    "{name} is not defined"
                )));
            }
            vm.set_global(name, value);
        }
        Instruction::InitGlobal(name_idx) => {
            let name = current_constant_string(vm, name_idx)?;
            let value = vm.pop()?;
            vm.set_global(name, value);
        }
        Instruction::LoadUpvalue(idx) => {
            let value = vm.call_stack.last().unwrap().upvalues[idx as usize].get();
            vm.push(value);
        }
        Instruction::StoreUpvalue(idx) => {
            let value = vm.pop()?;
            vm.call_stack.last().unwrap().upvalues[idx as usize].set(value);
        }
        Instruction::Pop => {
            vm.pop()?;
        }
        Instruction::Dup => {
            vm.push(vm.peek()?.clone());
        }
        _ => return Ok(InstructionOutcome::Unhandled),
    }
    Ok(InstructionOutcome::Handled(None))
}
