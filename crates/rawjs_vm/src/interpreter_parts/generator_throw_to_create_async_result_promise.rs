/// Implement generator `.throw(error)`.
fn generator_throw(
    _vm: &mut Vm,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    error: JsValue,
) -> Result<JsValue> {
    let status = {
        let gen_obj = gen_ptr.borrow();
        match &gen_obj.internal {
            ObjectInternal::Generator(state) => state.status.clone(),
            _ => return Err(RawJsError::type_error("not a generator")),
        }
    };
    match status {
        GeneratorStatus::SuspendedStart | GeneratorStatus::Completed => {
            {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                }
            }
            Err(RawJsError::internal_error(format!("{}", error)))
        }
        GeneratorStatus::SuspendedYield => {
            {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                }
            }
            Err(RawJsError::internal_error(format!("{}", error)))
        }
        GeneratorStatus::Executing => Err(RawJsError::type_error("Generator is already executing")),
    }
}

fn async_generator_next(
    vm: &mut Vm,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    value: JsValue,
) -> Result<JsValue> {
    let saved_stack_len = vm.value_stack.len();
    let status = {
        let gen_obj = gen_ptr.borrow();
        match &gen_obj.internal {
            ObjectInternal::Generator(state) => state.status.clone(),
            _ => return Err(RawJsError::type_error("not a generator")),
        }
    };
    let promise_ptr = alloc_promise(vm);
    if status == GeneratorStatus::Completed {
        let result = make_iterator_result(vm, JsValue::Undefined, true);
        rawjs_runtime::builtins::resolve_promise_with_heap(&promise_ptr, result, &mut vm.heap);
        return Ok(JsValue::Object(promise_ptr));
    }
    {
        let mut gen_obj = gen_ptr.borrow_mut();
        if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
            state.result_promise = Some(promise_ptr.clone());
        }
    }
    if let Err(err) = async_resume(vm, gen_ptr, value, false) {
        let reason = vm
            .thrown_value
            .take()
            .unwrap_or_else(|| JsValue::string(err.message.as_str()));
        clear_generator_result_promise(gen_ptr);
        rawjs_runtime::builtins::reject_promise_with_heap(&promise_ptr, reason, &mut vm.heap);
    }
    vm.value_stack.truncate(saved_stack_len);
    Ok(JsValue::Object(promise_ptr))
}

fn async_generator_return(
    vm: &mut Vm,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    value: JsValue,
) -> JsValue {
    let promise_ptr = alloc_promise(vm);
    let result = generator_return(gen_ptr, vm, value);
    clear_generator_result_promise(gen_ptr);
    rawjs_runtime::builtins::resolve_promise_with_heap(&promise_ptr, result, &mut vm.heap);
    JsValue::Object(promise_ptr)
}

fn async_generator_throw(
    vm: &mut Vm,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    error: JsValue,
) -> JsValue {
    let promise_ptr = alloc_promise(vm);
    clear_generator_result_promise(gen_ptr);
    rawjs_runtime::builtins::reject_promise_with_heap(&promise_ptr, error, &mut vm.heap);
    JsValue::Object(promise_ptr)
}

/// Create a `{ value, done }` iterator result object.
fn make_iterator_result(vm: &mut Vm, value: JsValue, done: bool) -> JsValue {
    let mut obj = JsObject::ordinary();
    obj.set_property("value".to_string(), value);
    obj.set_property("done".to_string(), JsValue::Boolean(done));
    JsValue::Object(vm.heap.alloc(obj))
}

enum DisposeResult {
    Complete,
    Await(JsValue),
}

fn create_async_result_promise(vm: &mut Vm) -> GcPtr<JsObject> {
    vm.create_promise_object()
}
