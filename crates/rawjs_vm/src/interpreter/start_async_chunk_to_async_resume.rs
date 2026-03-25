pub(super) fn start_async_chunk(
    vm: &mut Vm,
    chunk_index: usize,
    locals: Vec<JsValue>,
    arguments: Vec<JsValue>,
    upvalues: Vec<Upvalue>,
    this_value: JsValue,
) -> GcPtr<JsObject> {
    let promise_ptr = create_async_result_promise(vm);
    let state = GeneratorState {
        status: GeneratorStatus::SuspendedStart,
        chunk_index,
        ip: 0,
        locals,
        arguments,
        upvalues,
        saved_stack: Vec::new(),
        this_value,
        result_promise: Some(promise_ptr.clone()),
        saved_try_stack: Vec::new(),
    };
    let mut gen_obj = JsObject::ordinary();
    gen_obj.internal = ObjectInternal::Generator(state);
    let gen_ptr = vm.heap.alloc(gen_obj);
    let run_result = async_resume(vm, &gen_ptr, JsValue::Undefined, false);
    if let Err(err) = run_result {
        let reason = vm
            .thrown_value
            .take()
            .unwrap_or_else(|| JsValue::string(err.message.as_str()));
        rawjs_runtime::builtins::reject_promise_with_heap(&promise_ptr, reason, &mut vm.heap);
    }
    promise_ptr
}

pub(crate) fn execute_async_top_level(vm: &mut Vm, chunk_index: usize) -> Result<JsValue> {
    let local_count = vm.chunks[chunk_index].local_count as usize;
    let locals = vec![JsValue::Undefined; local_count];
    let promise_ptr = start_async_chunk(
        vm,
        chunk_index,
        locals,
        Vec::new(),
        Vec::new(),
        JsValue::Undefined,
    );
    drain_microtasks(vm)?;
    let (status, value) = {
        let promise = promise_ptr.borrow();
        match &promise.internal {
            ObjectInternal::Promise(state) => (state.status.clone(), state.value.clone()),
            _ => {
                return Err(RawJsError::internal_error(
                    "async top-level execution did not produce a Promise",
                ));
            }
        }
    };
    match status {
        PromiseStatus::Fulfilled => Ok(value),
        PromiseStatus::Rejected => Err(RawJsError::internal_error(format!(
            "top-level await rejected: {}",
            value
        ))),
        PromiseStatus::Pending => Err(RawJsError::internal_error("top-level await did not settle")),
    }
}

/// Create and execute an async function call.
///
/// Async functions return a Promise. The function body runs synchronously
/// until the first `Await` or `Return`.
pub(super) fn exec_async_call(
    vm: &mut Vm,
    chunk_index: usize,
    args: &[JsValue],
    upvalues: Vec<Upvalue>,
    this_value: JsValue,
) -> Result<()> {
    let param_count = vm.chunks[chunk_index].param_count as usize;
    let local_count = vm.chunks[chunk_index].local_count as usize;
    let slot_count = local_count.max(param_count);
    let mut locals = vec![JsValue::Undefined; slot_count];
    for (i, arg) in args.iter().enumerate() {
        if i < slot_count {
            locals[i] = arg.clone();
        }
    }
    let promise_ptr =
        start_async_chunk(vm, chunk_index, locals, args.to_vec(), upvalues, this_value);
    vm.push(JsValue::Object(promise_ptr));
    Ok(())
}

/// Resume an async function's internal generator.
///
/// If `is_throw` is true, the resumed value is thrown into the generator
/// (used when the awaited promise is rejected).
pub(super) fn async_resume(
    vm: &mut Vm,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    value: JsValue,
    is_throw: bool,
) -> Result<()> {
    let (
        chunk_index,
        ip,
        locals,
        arguments,
        upvalues,
        saved_stack,
        result_promise,
        saved_try_entries,
    ) = {
        let mut gen_obj = gen_ptr.borrow_mut();
        let state = match &mut gen_obj.internal {
            ObjectInternal::Generator(s) => s,
            _ => return Err(RawJsError::type_error("not an async generator")),
        };
        match state.status {
            GeneratorStatus::Completed => return Ok(()),
            GeneratorStatus::Executing => {
                return Err(RawJsError::type_error(
                    "async function is already executing",
                ));
            }
            _ => {}
        }
        state.status = GeneratorStatus::Executing;
        (
            state.chunk_index,
            state.ip,
            state.locals.clone(),
            state.arguments.clone(),
            state.upvalues.clone(),
            state.saved_stack.clone(),
            state.result_promise.clone(),
            std::mem::take(&mut state.saved_try_stack),
        )
    };
    let base = vm.value_stack.len();
    for sv in &saved_stack {
        vm.push(sv.clone());
    }
    let start_ip = if ip > 0 {
        vm.push(value.clone());
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
    for (catch_ip, finally_ip, stack_depth, call_depth) in saved_try_entries {
        vm.try_stack.push(TryContext {
            catch_ip,
            finally_ip,
            stack_depth,
            call_depth,
        });
    }
    let target_depth = vm.call_stack.len() - 1;
    if is_throw {
        vm.thrown_value = Some(value);
        let throw_err = RawJsError::internal_error("async throw");
        if unwind_exception(vm, &throw_err)? {
        } else {
            let reason = vm.thrown_value.take().unwrap_or(JsValue::Undefined);
            while vm.call_stack.len() > target_depth {
                let frame = vm.call_stack.pop().unwrap();
                vm.value_stack.truncate(frame.base);
            }
            {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                }
            }
            if let Some(ref promise) = result_promise {
                rawjs_runtime::builtins::reject_promise_with_heap(promise, reason, &mut vm.heap);
            }
            return Ok(());
        }
    }
    let result = run_async_frame(vm, target_depth, gen_ptr, &result_promise);
    match result {
        Ok(()) => Ok(()),
        Err(err) => {
            let reason = vm
                .thrown_value
                .take()
                .unwrap_or_else(|| JsValue::string(err.message.as_str()));
            while vm.call_stack.len() > target_depth {
                let frame = vm.call_stack.pop().unwrap();
                vm.value_stack.truncate(frame.base);
            }
            {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                }
            }
            if let Some(ref promise) = result_promise {
                rawjs_runtime::builtins::reject_promise_with_heap(promise, reason, &mut vm.heap);
            }
            Ok(())
        }
    }
}

use super::*;
