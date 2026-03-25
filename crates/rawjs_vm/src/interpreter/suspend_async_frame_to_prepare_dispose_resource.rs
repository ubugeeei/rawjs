pub(super) fn suspend_async_frame(
    vm: &mut Vm,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    try_stack_base: usize,
    awaited_value: JsValue,
) -> Result<()> {
    let frame = vm.call_stack.pop().unwrap();
    let saved_stack: Vec<JsValue> = vm.value_stack.drain(frame.base..).collect();
    let saved_try_entries: Vec<_> = vm
        .try_stack
        .drain(try_stack_base..)
        .map(|ctx| {
            (
                ctx.catch_ip,
                ctx.finally_ip,
                ctx.stack_depth,
                ctx.call_depth,
            )
        })
        .collect();
    {
        let mut gen_obj = gen_ptr.borrow_mut();
        if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
            state.status = GeneratorStatus::SuspendedYield;
            state.ip = frame.ip;
            state.locals = frame.locals;
            state.upvalues = frame.upvalues;
            state.saved_stack = saved_stack;
            state.saved_try_stack = saved_try_entries;
        }
    }
    let is_promise = match &awaited_value {
        JsValue::Object(ptr) => {
            matches!(ptr.borrow().internal, ObjectInternal::Promise(_))
        }
        _ => false,
    };
    if is_promise {
        let gen_ptr_clone = gen_ptr.clone();
        let on_fulfilled = create_async_resume_fn(vm, gen_ptr_clone.clone(), false);
        let on_rejected = create_async_resume_fn(vm, gen_ptr_clone, true);
        let then_args = [JsValue::Object(on_fulfilled), JsValue::Object(on_rejected)];
        rawjs_runtime::builtins::promise_then_internal(&mut vm.heap, &awaited_value, &then_args)?;
        return Ok(());
    }
    let gen_ptr_clone = gen_ptr.clone();
    let on_fulfilled = create_async_resume_fn(vm, gen_ptr_clone, false);
    vm.heap.pending_microtasks.push(rawjs_runtime::MicroTask {
        callback: JsValue::Object(on_fulfilled),
        arg: awaited_value,
        target_promise: None,
    });
    Ok(())
}

/// Create a native function that, when called, resumes the async generator.
///
/// The resume function is stored with `__async_gen_ptr__` property pointing
/// to the generator's GcPtr so the VM can find it.
pub(super) fn create_async_resume_fn(
    vm: &mut Vm,
    gen_ptr: rawjs_runtime::GcPtr<JsObject>,
    is_throw: bool,
) -> rawjs_runtime::GcPtr<JsObject> {
    let callback: rawjs_runtime::NativeFn = if is_throw {
        async_reject_callback
    } else {
        async_fulfill_callback
    };
    let name = if is_throw {
        "asyncReject"
    } else {
        "asyncFulfill"
    };
    let mut fn_obj = JsObject::native_function(name, callback);
    fn_obj.set_property("__async_gen_ptr__".to_string(), JsValue::Object(gen_ptr));
    vm.heap.alloc(fn_obj)
}

/// Dispose a resource stored in a local slot by calling its [Symbol.dispose]() method.
/// If the local is null or undefined, this is a no-op.
pub(crate) fn exec_dispose_resource(vm: &mut Vm, slot: u16) -> Result<()> {
    match prepare_dispose_resource(vm, slot, false)? {
        DisposeResult::Complete => Ok(()),
        DisposeResult::Await(_) => Err(RawJsError::internal_error(
            "DisposeResource unexpectedly required awaiting",
        )),
    }
}

pub(super) fn prepare_dispose_resource(
    vm: &mut Vm,
    slot: u16,
    prefer_async: bool,
) -> Result<DisposeResult> {
    let frame = vm
        .call_stack
        .last()
        .ok_or_else(|| RawJsError::internal_error("DisposeResource: no call frame"))?;
    let resource = frame.locals[slot as usize].clone();
    if resource.is_nullish() {
        return Ok(DisposeResult::Complete);
    }
    let (dispose_fn, should_await) = lookup_dispose_method(&resource, prefer_async)?;
    let result = call_zero_arg_method(vm, dispose_fn, resource)?;
    if should_await {
        return Ok(DisposeResult::Await(result));
    }
    Ok(DisposeResult::Complete)
}

use super::*;
