pub(super) fn lookup_dispose_method(
    resource: &JsValue,
    prefer_async: bool,
) -> Result<(JsValue, bool)> {
    let JsValue::Object(ptr) = resource else {
        return Err(RawJsError::type_error(
            "Resource is not an object and cannot be disposed",
        ));
    };
    let object = ptr.borrow();
    if prefer_async {
        let async_dispose = object.get_symbol_property(rawjs_runtime::value::SYMBOL_ASYNC_DISPOSE);
        if !async_dispose.is_undefined() {
            return Ok((async_dispose, true));
        }
    }
    let dispose = object.get_symbol_property(rawjs_runtime::value::SYMBOL_DISPOSE);
    if !dispose.is_undefined() {
        return Ok((dispose, false));
    }
    let symbol_name = if prefer_async {
        "[Symbol.asyncDispose] or [Symbol.dispose]"
    } else {
        "[Symbol.dispose]"
    };
    Err(RawJsError::type_error(format!(
        "Resource does not have a {} method",
        symbol_name
    )))
}

pub(super) fn call_zero_arg_method(
    vm: &mut Vm,
    callee: JsValue,
    this_value: JsValue,
) -> Result<JsValue> {
    let base_depth = vm.call_stack.len();
    let base_stack_len = vm.value_stack.len();
    vm.push(callee);
    exec_call(vm, 0, this_value)?;
    if vm.call_stack.len() > base_depth {
        run_inner_frame(vm, base_depth)?;
    }
    if vm.value_stack.len() <= base_stack_len {
        return Ok(JsValue::Undefined);
    }
    vm.pop()
}

/// This is called from the normal instruction dispatch for async frames
/// that are running within the main interpreter loop (e.g., during the
/// initial synchronous phase of an async function).
pub(crate) fn exec_await(vm: &mut Vm) -> Result<Option<JsValue>> {
    let _ = vm.pop()?;
    Err(RawJsError::internal_error(
        "Await instruction reached outside async frame",
    ))
}

/// Native callback for async fulfill (called when awaited promise resolves).
pub(super) fn async_fulfill_callback(
    heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> rawjs_common::Result<JsValue> {
    let value = args.first().cloned().unwrap_or(JsValue::Undefined);
    let gen_ptr = {
        let calling_fn = heap
            .calling_fn
            .as_ref()
            .ok_or_else(|| RawJsError::internal_error("async callback: no calling_fn"))?;
        let fn_obj = calling_fn.borrow();
        let gen_val = fn_obj.get_property("__async_gen_ptr__");
        match gen_val {
            JsValue::Object(ptr) => ptr,
            _ => {
                return Err(RawJsError::internal_error(
                    "async callback: __async_gen_ptr__ not found",
                ));
            }
        }
    };
    gen_ptr
        .borrow_mut()
        .set_property("__async_resume_value__".to_string(), value);
    gen_ptr.borrow_mut().set_property(
        "__async_resume_throw__".to_string(),
        JsValue::Boolean(false),
    );
    gen_ptr.borrow_mut().set_property(
        "__async_resume_pending__".to_string(),
        JsValue::Boolean(true),
    );
    Ok(JsValue::Undefined)
}

/// Native callback for async reject (called when awaited promise rejects).
pub(super) fn async_reject_callback(
    heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> rawjs_common::Result<JsValue> {
    let value = args.first().cloned().unwrap_or(JsValue::Undefined);
    let gen_ptr = {
        let calling_fn = heap
            .calling_fn
            .as_ref()
            .ok_or_else(|| RawJsError::internal_error("async callback: no calling_fn"))?;
        let fn_obj = calling_fn.borrow();
        let gen_val = fn_obj.get_property("__async_gen_ptr__");
        match gen_val {
            JsValue::Object(ptr) => ptr,
            _ => {
                return Err(RawJsError::internal_error(
                    "async callback: __async_gen_ptr__ not found",
                ));
            }
        }
    };
    gen_ptr
        .borrow_mut()
        .set_property("__async_resume_value__".to_string(), value);
    gen_ptr
        .borrow_mut()
        .set_property("__async_resume_throw__".to_string(), JsValue::Boolean(true));
    gen_ptr.borrow_mut().set_property(
        "__async_resume_pending__".to_string(),
        JsValue::Boolean(true),
    );
    Ok(JsValue::Undefined)
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
