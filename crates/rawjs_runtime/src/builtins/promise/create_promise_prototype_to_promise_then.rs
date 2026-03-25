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
pub(super) fn promise_then(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let on_fulfilled = args.first().cloned().unwrap_or(JsValue::Undefined);
    let on_rejected = args.get(1).cloned().unwrap_or(JsValue::Undefined);
    let this_ptr = match this {
        JsValue::Object(ptr) => ptr.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Promise.prototype.then called on non-Promise",
            ));
        }
    };
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
                ));
            }
        }
    };
    match status {
        PromiseStatus::Pending => {
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
                heap.pending_microtasks.push(MicroTask {
                    callback: handler,
                    arg: value,
                    target_promise: Some(result_ptr.clone()),
                });
            } else {
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
                heap.pending_microtasks.push(MicroTask {
                    callback: handler,
                    arg: value,
                    target_promise: Some(result_ptr.clone()),
                });
            } else {
                reject_promise_internal(&result_ptr, value);
            }
        }
    }
    Ok(JsValue::Object(result_ptr))
}

use super::*;
