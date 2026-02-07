use crate::gc::{GcPtr, Heap};
use crate::object::JsObject;
use crate::value::JsValue;

use super::helpers::set_native;

/// Create Generator.prototype with next, return, throw methods.
///
/// The actual logic for these methods is implemented in the VM interpreter
/// (because it needs access to call frames). These native stubs serve as
/// placeholders so prototype chain lookups find the right methods.
/// The VM intercepts calls to generator prototype methods.
pub fn create_generator_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();

    set_native(&mut obj, "next", generator_next_placeholder);
    set_native(&mut obj, "return", generator_return_placeholder);
    set_native(&mut obj, "throw", generator_throw_placeholder);

    heap.alloc(obj)
}

/// Placeholder -- the real logic is in the VM interpreter.
fn generator_next_placeholder(
    _heap: &mut Heap,
    _this: &JsValue,
    _args: &[JsValue],
) -> rawjs_common::Result<JsValue> {
    Err(rawjs_common::RawJsError::internal_error(
        "generator.next() should be intercepted by the VM",
    ))
}

fn generator_return_placeholder(
    _heap: &mut Heap,
    _this: &JsValue,
    _args: &[JsValue],
) -> rawjs_common::Result<JsValue> {
    Err(rawjs_common::RawJsError::internal_error(
        "generator.return() should be intercepted by the VM",
    ))
}

fn generator_throw_placeholder(
    _heap: &mut Heap,
    _this: &JsValue,
    _args: &[JsValue],
) -> rawjs_common::Result<JsValue> {
    Err(rawjs_common::RawJsError::internal_error(
        "generator.throw() should be intercepted by the VM",
    ))
}
