pub(super) fn sync_arguments_object_from_local(
    arguments_obj: rawjs_runtime::GcPtr<JsObject>,
    slot: u16,
    value: JsValue,
) {
    let mapped_index = {
        let obj = arguments_obj.borrow();
        match &obj.internal {
            ObjectInternal::ArgumentsObject(mapped_slots) => mapped_slots
                .iter()
                .position(|mapped_slot| *mapped_slot == Some(slot)),
            _ => None,
        }
    };
    if let Some(index) = mapped_index {
        arguments_obj
            .borrow_mut()
            .set_property(index.to_string(), value);
    }
}

pub(super) fn sync_local_from_arguments_object(
    vm: &mut Vm,
    arguments_obj: &rawjs_runtime::GcPtr<JsObject>,
    name: &str,
    value: JsValue,
) {
    let Ok(index) = name.parse::<usize>() else {
        return;
    };
    let mapped_slot = {
        let obj = arguments_obj.borrow();
        match &obj.internal {
            ObjectInternal::ArgumentsObject(mapped_slots) => {
                mapped_slots.get(index).copied().flatten()
            }
            _ => None,
        }
    };
    if let Some(slot) = mapped_slot {
        if let Some(frame) = vm.call_stack.last_mut() {
            let slot = slot as usize;
            if slot >= frame.locals.len() {
                frame.locals.resize(slot + 1, JsValue::Undefined);
            }
            frame.locals[slot] = value;
        }
    }
}

/// Execute `new Promise(executor)` — creates a Promise, builds resolve/reject,
/// calls executor synchronously, returns the Promise.
pub(super) fn exec_promise_constructor(vm: &mut Vm, args: &[JsValue]) -> Result<()> {
    let executor = args.first().cloned().unwrap_or(JsValue::Undefined);
    if !executor.is_function() {
        return Err(RawJsError::type_error("Promise resolver is not a function"));
    }
    let promise_ptr = vm.heap.alloc(JsObject::promise());
    if let Some(ref proto) = vm.promise_prototype {
        promise_ptr.borrow_mut().set_prototype(Some(proto.clone()));
    }
    let promise_id = vm.heap.next_promise_id;
    vm.heap.next_promise_id += 1;
    vm.heap
        .promise_targets
        .insert(promise_id, promise_ptr.clone());
    let mut resolve_fn =
        JsObject::native_function("resolve", rawjs_runtime::builtins::promise_resolve_fn);
    resolve_fn.define_property(
        "__promise_id__".to_string(),
        Property::builtin(JsValue::Number(promise_id as f64)),
    );
    let resolve_ptr = vm.heap.alloc(resolve_fn);
    let mut reject_fn =
        JsObject::native_function("reject", rawjs_runtime::builtins::promise_reject_fn);
    reject_fn.define_property(
        "__promise_id__".to_string(),
        Property::builtin(JsValue::Number(promise_id as f64)),
    );
    let reject_ptr = vm.heap.alloc(reject_fn);
    vm.push(executor);
    vm.push(JsValue::Object(resolve_ptr));
    vm.push(JsValue::Object(reject_ptr));
    exec_call(vm, 2, JsValue::Undefined)?;
    let initial_stack_len = vm.value_stack.len();
    let saved_call_depth = vm.call_stack.len();
    if saved_call_depth > 1 {
        let result = run_executor_frame(vm, saved_call_depth - 1);
        match result {
            Ok(_) => {
                while vm.value_stack.len() > initial_stack_len {
                    vm.value_stack.pop();
                }
            }
            Err(err) => {
                while vm.call_stack.len() >= saved_call_depth {
                    let frame = vm.call_stack.pop().unwrap();
                    vm.value_stack.truncate(frame.base);
                }
                vm.value_stack.truncate(initial_stack_len);
                let reason = vm
                    .thrown_value
                    .take()
                    .unwrap_or_else(|| JsValue::string(err.message.as_str()));
                rawjs_runtime::builtins::reject_promise_internal(&promise_ptr, reason);
            }
        }
    } else if vm.value_stack.len() > initial_stack_len {
        vm.pop()?;
    }
    vm.push(JsValue::Object(promise_ptr));
    Ok(())
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
