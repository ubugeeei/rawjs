/// Try to intercept a generator method call (.next, .return, .throw).
///
/// Returns `Some(result)` if the call was intercepted, `None` otherwise.
pub(super) fn try_generator_method_call(
    vm: &mut Vm,
    receiver: &JsValue,
    method: &JsValue,
    args: &[JsValue],
) -> Result<Option<JsValue>> {
    let gen_ptr = match receiver {
        JsValue::Object(ptr) => {
            let is_gen = matches!(ptr.borrow().internal, ObjectInternal::Generator(_));
            if is_gen {
                ptr.clone()
            } else {
                return Ok(None);
            }
        }
        _ => return Ok(None),
    };
    let is_async_generator = {
        let gen_obj = gen_ptr.borrow();
        match &gen_obj.internal {
            ObjectInternal::Generator(state) => vm.chunks[state.chunk_index].is_async,
            _ => false,
        }
    };
    let method_name = match method {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            if let ObjectInternal::Function(f) = &obj.internal {
                f.name.clone()
            } else {
                return Ok(None);
            }
        }
        _ => return Ok(None),
    };
    match method_name.as_str() {
        "next" => {
            let arg = args.first().cloned().unwrap_or(JsValue::Undefined);
            let result = if is_async_generator {
                async_generator_next(vm, &gen_ptr, arg)?
            } else {
                generator_next(vm, &gen_ptr, arg)?
            };
            Ok(Some(result))
        }
        "return" => {
            let arg = args.first().cloned().unwrap_or(JsValue::Undefined);
            let result = if is_async_generator {
                async_generator_return(vm, &gen_ptr, arg)
            } else {
                generator_return(&gen_ptr, vm, arg)
            };
            Ok(Some(result))
        }
        "throw" => {
            let arg = args.first().cloned().unwrap_or(JsValue::Undefined);
            let result = if is_async_generator {
                async_generator_throw(vm, &gen_ptr, arg)
            } else {
                generator_throw(vm, &gen_ptr, arg)?
            };
            Ok(Some(result))
        }
        _ => Ok(None),
    }
}

/// Resume a generator via `.next(value)`.
///
/// If `SuspendedStart`, creates a fresh CallFrame and runs until Yield/Return.
/// If `SuspendedYield`, restores the saved frame and pushes `value` before resuming.
/// If `Completed`, returns `{ value: undefined, done: true }`.
pub(super) fn generator_next(
    vm: &mut Vm,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    value: JsValue,
) -> Result<JsValue> {
    let (_status, chunk_index, ip, locals, arguments, upvalues, saved_stack, _this_value) = {
        let mut gen_obj = gen_ptr.borrow_mut();
        let state = match &mut gen_obj.internal {
            ObjectInternal::Generator(s) => s,
            _ => return Err(RawJsError::type_error("not a generator")),
        };
        match state.status {
            GeneratorStatus::Completed => {
                return Ok(make_iterator_result(vm, JsValue::Undefined, true));
            }
            GeneratorStatus::Executing => {
                return Err(RawJsError::type_error("Generator is already executing"));
            }
            _ => {}
        }
        state.status = GeneratorStatus::Executing;
        (
            state.status.clone(),
            state.chunk_index,
            state.ip,
            state.locals.clone(),
            state.arguments.clone(),
            state.upvalues.clone(),
            state.saved_stack.clone(),
            state.this_value.clone(),
        )
    };
    let base = vm.value_stack.len();
    for sv in &saved_stack {
        vm.push(sv.clone());
    }
    let start_ip = if ip > 0 {
        vm.push(value);
        ip
    } else {
        0
    };
    let frame = CallFrame {
        chunk_index,
        ip: start_ip,
        base,
        locals,
        arguments,
        arguments_object: None,
        callee: None,
        is_strict: vm.chunks[chunk_index].is_strict,
        upvalues,
        this_value: JsValue::Object(gen_ptr.clone()),
    };
    vm.call_stack.push(frame);
    let target_depth = vm.call_stack.len() - 1;
    let result = run_generator_frame(vm, target_depth);
    match result {
        Ok(gen_result) => Ok(gen_result),
        Err(err) => {
            let mut gen_obj = gen_ptr.borrow_mut();
            if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                state.status = GeneratorStatus::Completed;
            }
            Err(err)
        }
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
