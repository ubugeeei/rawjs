/// Execute a `Return` instruction.
///
/// Pops the return value, pops the call frame, restores the value stack,
/// and pushes the return value back.  If this was the top-level frame,
/// returns `Ok(Some(value))`.
pub(super) fn exec_return(vm: &mut Vm) -> Result<Option<JsValue>> {
    let return_value = if vm.value_stack.len() > vm.call_stack.last().unwrap().base {
        vm.pop()?
    } else {
        JsValue::Undefined
    };
    let return_value = finish_frame_return(vm, return_value);
    if vm.call_stack.is_empty() {
        return Ok(Some(return_value));
    }
    vm.push(return_value);
    Ok(None)
}

/// Execute a `CreateClosure` instruction.
pub(crate) fn exec_create_closure(vm: &mut Vm, const_idx: u16) -> Result<()> {
    let func_clone = {
        let frame = vm.call_stack.last().unwrap();
        let chunk = &vm.chunks[frame.chunk_index];
        let constant = &chunk.constants[const_idx as usize];
        match constant {
            Constant::Function(func_chunk) => func_chunk.as_ref().clone(),
            _ => {
                return Err(RawJsError::internal_error(
                    "CreateClosure: constant is not a Function",
                ));
            }
        }
    };
    let chunk_index = vm.add_chunk(func_clone);
    let upvalue_descs = vm.chunks[chunk_index].upvalue_descriptors.clone();
    let upvalues: Vec<Upvalue> = {
        let frame = vm.call_stack.last().unwrap();
        upvalue_descs
            .iter()
            .map(|desc| {
                if desc.is_local {
                    let idx = desc.index as usize;
                    let value = if idx < frame.locals.len() {
                        frame.locals[idx].clone()
                    } else {
                        JsValue::Undefined
                    };
                    Upvalue::new(value)
                } else {
                    let idx = desc.index as usize;
                    if idx < frame.upvalues.len() {
                        frame.upvalues[idx].clone()
                    } else {
                        Upvalue::new(JsValue::Undefined)
                    }
                }
            })
            .collect()
    };
    let name = vm.chunks[chunk_index].name.clone();
    let func_obj = JsObject::function(chunk_index, upvalues, name);
    let ptr = vm.heap.alloc(func_obj);
    if let Some(ref proto) = vm.function_prototype {
        ptr.borrow_mut().set_prototype(Some(proto.clone()));
    } else if let Some(ref proto) = vm.object_prototype {
        ptr.borrow_mut().set_prototype(Some(proto.clone()));
    }
    if let JsValue::Object(func_proto) = ptr.borrow().get_property("prototype") {
        if let Some(ref proto) = vm.object_prototype {
            func_proto.borrow_mut().set_prototype(Some(proto.clone()));
        }
        func_proto.borrow_mut().define_property(
            "constructor".to_string(),
            Property::builtin(JsValue::Object(ptr.clone())),
        );
    }
    vm.push(JsValue::Object(ptr));
    Ok(())
}

/// Attempt to unwind the call/try stacks to find a catch handler.
///
/// Returns `true` if a handler was found and execution can continue;
/// `false` if the exception is unhandled.
pub(super) fn unwind_exception(vm: &mut Vm, err: &RawJsError) -> Result<bool> {
    while let Some(ctx) = vm.try_stack.last().cloned() {
        while vm.call_stack.len() > ctx.call_depth {
            if matches!(
                vm.construct_frames.last(), Some((construct_depth, _)) if *
                construct_depth == vm.call_stack.len()
            ) {
                vm.construct_frames.pop();
            }
            let frame = vm.call_stack.pop().unwrap();
            vm.value_stack.truncate(frame.base);
        }
        vm.value_stack.truncate(ctx.stack_depth);
        vm.try_stack.pop();
        if let Some(catch_ip) = ctx.catch_ip {
            let thrown = vm
                .thrown_value
                .take()
                .unwrap_or_else(|| JsValue::string(err.message.as_str()));
            vm.push(thrown);
            if let Some(frame) = vm.call_stack.last_mut() {
                frame.ip = catch_ip;
            }
            return Ok(true);
        }
        if let Some(finally_ip) = ctx.finally_ip {
            if let Some(frame) = vm.call_stack.last_mut() {
                frame.ip = finally_ip;
            }
            return Ok(true);
        }
    }
    Ok(false)
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
