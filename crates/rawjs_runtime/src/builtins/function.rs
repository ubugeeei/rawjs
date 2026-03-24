use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};
use crate::object::JsObject;
use crate::value::JsValue;

pub fn create_function_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    heap.alloc(JsObject::ordinary())
}

pub fn function_constructor_placeholder(
    _heap: &mut Heap,
    _this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    Err(RawJsError::type_error(
        "Function constructor is not supported",
    ))
}
