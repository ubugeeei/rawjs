use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap, MicroTask};
use crate::object::{
    JsObject, ObjectInternal, PromiseReaction, PromiseReactionKind, PromiseStatus,
};
use crate::value::JsValue;

use super::helpers::set_native;

/// Create the Promise prototype object with then/catch/finally methods.
pub fn create_promise_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut proto = JsObject::ordinary();

    set_native(&mut proto, "then", promise_then);
    set_native(&mut proto, "catch", promise_catch);
    set_native(&mut proto, "finally", promise_finally);

    heap.alloc(proto)
}

/// Public wrapper so the VM can call .then() on a promise directly.
pub fn promise_then_internal(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    promise_then(heap, this, args)
}

/// Promise.prototype.then(onFulfilled, onRejected)
fn promise_then(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let on_fulfilled = args.first().cloned().unwrap_or(JsValue::Undefined);
    let on_rejected = args.get(1).cloned().unwrap_or(JsValue::Undefined);

    let this_ptr = match this {
        JsValue::Object(ptr) => ptr.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Promise.prototype.then called on non-Promise",
            ))
        }
    };

    // Create the result promise
    let result_promise = heap.alloc(JsObject::promise());
    let result_ptr = result_promise.clone();

    let fulfill_handler = if on_fulfilled.is_function() {
        Some(on_fulfilled)
    } else {
        None
    };
    let reject_handler = if on_rejected.is_function() {
        Some(on_rejected)
    } else {
        None
    };

    let status = {
        let obj = this_ptr.borrow();
        match &obj.internal {
            ObjectInternal::Promise(state) => state.status.clone(),
            _ => {
                return Err(RawJsError::type_error(
                    "Promise.prototype.then called on non-Promise",
                ))
            }
        }
    };

    match status {
        PromiseStatus::Pending => {
            // Add reactions to the promise
            let mut obj = this_ptr.borrow_mut();
            if let ObjectInternal::Promise(ref mut state) = obj.internal {
                state.fulfill_reactions.push(PromiseReaction {
                    handler: fulfill_handler,
                    kind: PromiseReactionKind::Fulfill,
                    result_promise: Some(result_ptr.clone()),
                });
                state.reject_reactions.push(PromiseReaction {
                    handler: reject_handler,
                    kind: PromiseReactionKind::Reject,
                    result_promise: Some(result_ptr.clone()),
                });
            }
        }
        PromiseStatus::Fulfilled => {
            let value = {
                let obj = this_ptr.borrow();
                match &obj.internal {
                    ObjectInternal::Promise(state) => state.value.clone(),
                    _ => JsValue::Undefined,
                }
            };
            if let Some(handler) = fulfill_handler {
                // Store promise ID for the microtask to resolve result_promise
                let id = heap.next_promise_id;
                heap.next_promise_id += 1;
                heap.promise_targets.insert(id, result_ptr.clone());
                // Store the handler info on result_promise
                result_ptr
                    .borrow_mut()
                    .set_property("__pending_handler__".to_string(), handler.clone());
                result_ptr
                    .borrow_mut()
                    .set_property("__pending_id__".to_string(), JsValue::Number(id as f64));
                heap.pending_microtasks.push(MicroTask {
                    callback: handler,
                    arg: value,
                });
            } else {
                // Identity: resolve result_promise with the same value
                resolve_promise_internal(&result_ptr, value);
            }
        }
        PromiseStatus::Rejected => {
            let value = {
                let obj = this_ptr.borrow();
                match &obj.internal {
                    ObjectInternal::Promise(state) => state.value.clone(),
                    _ => JsValue::Undefined,
                }
            };
            if let Some(handler) = reject_handler {
                let id = heap.next_promise_id;
                heap.next_promise_id += 1;
                heap.promise_targets.insert(id, result_ptr.clone());
                result_ptr
                    .borrow_mut()
                    .set_property("__pending_handler__".to_string(), handler.clone());
                result_ptr
                    .borrow_mut()
                    .set_property("__pending_id__".to_string(), JsValue::Number(id as f64));
                heap.pending_microtasks.push(MicroTask {
                    callback: handler,
                    arg: value,
                });
            } else {
                // Identity: reject result_promise with the same reason
                reject_promise_internal(&result_ptr, value);
            }
        }
    }

    Ok(JsValue::Object(result_ptr))
}

/// Promise.prototype.catch(onRejected) — shorthand for .then(undefined, onRejected)
fn promise_catch(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let on_rejected = args.first().cloned().unwrap_or(JsValue::Undefined);
    promise_then(heap, this, &[JsValue::Undefined, on_rejected])
}

/// Promise.prototype.finally(onFinally)
fn promise_finally(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let on_finally = args.first().cloned().unwrap_or(JsValue::Undefined);
    // Simplified: treat finally as then(onFinally, onFinally)
    promise_then(heap, this, &[on_finally.clone(), on_finally])
}

/// Resolve a promise internally (set status + trigger reactions).
///
/// When called without a Heap reference, reaction handlers are stored as
/// properties on the result_promise for the VM to pick up later.
/// Prefer `resolve_promise_with_heap` when a Heap is available so that
/// reactions are enqueued as microtasks immediately.
pub fn resolve_promise_internal(promise_ptr: &GcPtr<JsObject>, value: JsValue) {
    resolve_promise_impl(promise_ptr, value, None);
}

/// Resolve a promise and enqueue reaction handlers as microtasks.
pub fn resolve_promise_with_heap(promise_ptr: &GcPtr<JsObject>, value: JsValue, heap: &mut Heap) {
    resolve_promise_impl(promise_ptr, value, Some(heap));
}

fn resolve_promise_impl(
    promise_ptr: &GcPtr<JsObject>,
    value: JsValue,
    mut heap: Option<&mut Heap>,
) {
    let reactions = {
        let mut obj = promise_ptr.borrow_mut();
        match &mut obj.internal {
            ObjectInternal::Promise(ref mut state) => {
                if state.status != PromiseStatus::Pending {
                    return; // Already settled
                }
                state.status = PromiseStatus::Fulfilled;
                state.value = value.clone();
                let reactions = std::mem::take(&mut state.fulfill_reactions);
                state.reject_reactions.clear();
                reactions
            }
            _ => return,
        }
    };

    for reaction in reactions {
        if let Some(result_promise) = &reaction.result_promise {
            if let Some(handler) = reaction.handler {
                if let Some(ref mut h) = heap {
                    // Enqueue handler as a microtask.
                    h.pending_microtasks.push(MicroTask {
                        callback: handler.clone(),
                        arg: value.clone(),
                    });
                    // Store result_promise info for chaining.
                    result_promise
                        .borrow_mut()
                        .set_property("__pending_handler__".to_string(), handler);
                    result_promise
                        .borrow_mut()
                        .set_property("__pending_value__".to_string(), value.clone());
                } else {
                    // No heap: store handler info for the VM to use later.
                    result_promise
                        .borrow_mut()
                        .set_property("__pending_handler__".to_string(), handler);
                    result_promise
                        .borrow_mut()
                        .set_property("__pending_value__".to_string(), value.clone());
                }
            } else {
                // Identity: pass value through
                resolve_promise_impl(result_promise, value.clone(), None);
            }
        }
    }
}

/// Reject a promise internally (set status + trigger reactions).
pub fn reject_promise_internal(promise_ptr: &GcPtr<JsObject>, value: JsValue) {
    reject_promise_impl(promise_ptr, value, None);
}

/// Reject a promise and enqueue reaction handlers as microtasks.
pub fn reject_promise_with_heap(promise_ptr: &GcPtr<JsObject>, value: JsValue, heap: &mut Heap) {
    reject_promise_impl(promise_ptr, value, Some(heap));
}

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
                    });
                    result_promise
                        .borrow_mut()
                        .set_property("__pending_handler__".to_string(), handler);
                    result_promise
                        .borrow_mut()
                        .set_property("__pending_value__".to_string(), value.clone());
                } else {
                    result_promise
                        .borrow_mut()
                        .set_property("__pending_handler__".to_string(), handler);
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

    // Find the promise ID from the calling function's __promise_id__ property
    // The VM sets heap.calling_fn before calling native functions
    if let Some(ref fn_ptr) = heap.calling_fn.clone() {
        let id_val = fn_ptr.borrow().get_property("__promise_id__");
        if let JsValue::Number(id) = id_val {
            let id = id as u64;
            if let Some(promise_ptr) = heap.promise_targets.get(&id).cloned() {
                // Enqueue microtasks for pending reactions
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
                        // Enqueue as microtask with result_promise info
                        if let Some(result_promise) = &reaction.result_promise {
                            let id = heap.next_promise_id;
                            heap.next_promise_id += 1;
                            heap.promise_targets.insert(id, result_promise.clone());
                            result_promise.borrow_mut().set_property(
                                "__pending_id__".to_string(),
                                JsValue::Number(id as f64),
                            );
                        }
                        heap.pending_microtasks.push(MicroTask {
                            callback: handler,
                            arg: value.clone(),
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
                        if let Some(result_promise) = &reaction.result_promise {
                            let id = heap.next_promise_id;
                            heap.next_promise_id += 1;
                            heap.promise_targets.insert(id, result_promise.clone());
                            result_promise.borrow_mut().set_property(
                                "__pending_id__".to_string(),
                                JsValue::Number(id as f64),
                            );
                        }
                        heap.pending_microtasks.push(MicroTask {
                            callback: handler,
                            arg: reason.clone(),
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
    fn test_promise_resolve_static() {
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
    fn test_promise_reject_static() {
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
    fn test_promise_then_on_fulfilled() {
        let mut heap = Heap::new();
        // Create a resolved promise
        let promise = heap.alloc(JsObject::promise());
        resolve_promise_internal(&promise, JsValue::Number(10.0));
        let promise_val = JsValue::Object(promise);

        // Call then with no handlers (identity)
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
    fn test_promise_catch_on_rejected() {
        let mut heap = Heap::new();
        let promise = heap.alloc(JsObject::promise());
        reject_promise_internal(&promise, JsValue::string("error"));
        let promise_val = JsValue::Object(promise);

        // Call catch with no handler (identity)
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
