/// Promise.prototype.catch(onRejected) — shorthand for .then(undefined, onRejected)
pub(super) fn promise_catch(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let on_rejected = args.first().cloned().unwrap_or(JsValue::Undefined);
    promise_then(heap, this, &[JsValue::Undefined, on_rejected])
}

/// Promise.prototype.finally(onFinally)
pub(super) fn promise_finally(
    heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let on_finally = args.first().cloned().unwrap_or(JsValue::Undefined);
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

pub(super) fn resolve_promise_impl(
    promise_ptr: &GcPtr<JsObject>,
    value: JsValue,
    mut heap: Option<&mut Heap>,
) {
    let reactions = {
        let mut obj = promise_ptr.borrow_mut();
        match &mut obj.internal {
            ObjectInternal::Promise(ref mut state) => {
                if state.status != PromiseStatus::Pending {
                    return;
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

use super::*;
