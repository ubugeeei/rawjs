/// NativeFn for reject function passed to Promise executor.
pub fn promise_reject_fn(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let reason = args.first().cloned().unwrap_or(JsValue::Undefined);
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
                            state.status = PromiseStatus::Rejected;
                            state.value = reason.clone();
                            let reactions = std::mem::take(&mut state.reject_reactions);
                            state.fulfill_reactions.clear();
                            reactions
                        }
                        _ => Vec::new(),
                    }
                };
                for reaction in reactions {
                    if let Some(handler) = reaction.handler {
                        heap.pending_microtasks.push(MicroTask {
                            callback: handler,
                            arg: reason.clone(),
                            target_promise: reaction.result_promise.clone(),
                        });
                    } else if let Some(result_promise) = &reaction.result_promise {
                        reject_promise_internal(result_promise, reason.clone());
                    }
                }
            }
        }
    }
    Ok(JsValue::Undefined)
}

/// Placeholder NativeFn for Promise constructor — the real work is done in the VM.
pub fn promise_constructor_placeholder(
    _heap: &mut Heap,
    _this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    Err(RawJsError::type_error("Promise constructor requires 'new'"))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    pub(super) fn test_promise_resolve_static() {
        let mut heap = Heap::new();
        let result =
            promise_resolve_static(&mut heap, &JsValue::Undefined, &[JsValue::Number(42.0)])
                .unwrap();
        if let JsValue::Object(ptr) = &result {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Promise(state) => {
                    assert_eq!(state.status, PromiseStatus::Fulfilled);
                    assert_eq!(state.value, JsValue::Number(42.0));
                }
                _ => panic!("Expected Promise"),
            }
        } else {
            panic!("Expected Object");
        }
    }
    #[test]
    pub(super) fn test_promise_reject_static() {
        let mut heap = Heap::new();
        let result =
            promise_reject_static(&mut heap, &JsValue::Undefined, &[JsValue::string("err")])
                .unwrap();
        if let JsValue::Object(ptr) = &result {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Promise(state) => {
                    assert_eq!(state.status, PromiseStatus::Rejected);
                    assert_eq!(state.value, JsValue::string("err"));
                }
                _ => panic!("Expected Promise"),
            }
        } else {
            panic!("Expected Object");
        }
    }
    #[test]
    pub(super) fn test_promise_then_on_fulfilled() {
        let mut heap = Heap::new();
        let promise = heap.alloc(JsObject::promise());
        resolve_promise_internal(&promise, JsValue::Number(10.0));
        let promise_val = JsValue::Object(promise);
        let result = promise_then(&mut heap, &promise_val, &[]).unwrap();
        if let JsValue::Object(ptr) = &result {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Promise(state) => {
                    assert_eq!(state.status, PromiseStatus::Fulfilled);
                    assert_eq!(state.value, JsValue::Number(10.0));
                }
                _ => panic!("Expected Promise"),
            }
        }
    }
    #[test]
    pub(super) fn test_promise_catch_on_rejected() {
        let mut heap = Heap::new();
        let promise = heap.alloc(JsObject::promise());
        reject_promise_internal(&promise, JsValue::string("error"));
        let promise_val = JsValue::Object(promise);
        let result = promise_catch(&mut heap, &promise_val, &[]).unwrap();
        if let JsValue::Object(ptr) = &result {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Promise(state) => {
                    assert_eq!(state.status, PromiseStatus::Rejected);
                    assert_eq!(state.value, JsValue::string("error"));
                }
                _ => panic!("Expected Promise"),
            }
        }
    }
}

#[allow(unused_imports)]
use super::*;
