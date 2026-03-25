fn reject_promise_impl(promise_ptr: &GcPtr<JsObject>, value: JsValue, mut heap: Option<&mut Heap>) {
    let reactions = {
        let mut obj = promise_ptr.borrow_mut();
        match &mut obj.internal {
            ObjectInternal::Promise(ref mut state) => {
                if state.status != PromiseStatus::Pending {
                    return;
                }
                state.status = PromiseStatus::Rejected;
                state.value = value.clone();
                let reactions = std::mem::take(&mut state.reject_reactions);
                state.fulfill_reactions.clear();
                reactions
            }
            _ => return,
        }
    };
    for reaction in reactions {
        if let Some(result_promise) = &reaction.result_promise {
            if let Some(handler) = reaction.handler {
                if let Some(ref mut h) = heap {
                    h.pending_microtasks.push(MicroTask {
                        callback: handler.clone(),
                        arg: value.clone(),
                        target_promise: Some(result_promise.clone()),
                    });
                } else {
                    result_promise
                        .borrow_mut()
                        .set_property("__pending_value__".to_string(), value.clone());
                }
            } else {
                reject_promise_impl(result_promise, value.clone(), None);
            }
        }
    }
}

/// NativeFn for Promise.resolve(value)
pub fn promise_resolve_static(
    heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let value = args.first().cloned().unwrap_or(JsValue::Undefined);
    let promise = heap.alloc(JsObject::promise());
    resolve_promise_internal(&promise, value);
    Ok(JsValue::Object(promise))
}

/// NativeFn for Promise.reject(reason)
pub fn promise_reject_static(
    heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let reason = args.first().cloned().unwrap_or(JsValue::Undefined);
    let promise = heap.alloc(JsObject::promise());
    reject_promise_internal(&promise, reason);
    Ok(JsValue::Object(promise))
}

/// NativeFn for resolve function passed to Promise executor.
/// The promise ID is stored in the `__promise_id__` property of the function object.
pub fn promise_resolve_fn(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let value = args.first().cloned().unwrap_or(JsValue::Undefined);
    if let Some(ref fn_ptr) = heap.calling_fn.clone() {
        let id_val = fn_ptr.borrow().get_property("__promise_id__");
        if let JsValue::Number(id) = id_val {
            let id = id as u64;
            if let Some(promise_ptr) = heap.promise_targets.get(&id).cloned() {
                let reactions = {
                    let mut obj = promise_ptr.borrow_mut();
                    match &mut obj.internal {
                        ObjectInternal::Promise(ref mut state) => {
                            if state.status != PromiseStatus::Pending {
                                return Ok(JsValue::Undefined);
                            }
                            state.status = PromiseStatus::Fulfilled;
                            state.value = value.clone();
                            let reactions = std::mem::take(&mut state.fulfill_reactions);
                            state.reject_reactions.clear();
                            reactions
                        }
                        _ => Vec::new(),
                    }
                };
                for reaction in reactions {
                    if let Some(handler) = reaction.handler {
                        heap.pending_microtasks.push(MicroTask {
                            callback: handler,
                            arg: value.clone(),
                            target_promise: reaction.result_promise.clone(),
                        });
                    } else if let Some(result_promise) = &reaction.result_promise {
                        resolve_promise_internal(result_promise, value.clone());
                    }
                }
            }
        }
    }
    Ok(JsValue::Undefined)
}
