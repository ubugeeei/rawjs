use crate::gc::GcPtr;
use crate::object::{JsObject, NativeFn, Property};
use crate::value::JsValue;

/// Attach a native method to an object.
pub fn set_native(obj: &mut JsObject, name: &str, func: NativeFn) {
    let fn_obj = JsObject::native_function(name, func);
    obj.define_property(
        name.to_string(),
        Property::builtin(JsValue::Object(GcPtr::new(fn_obj))),
    );
}

/// Helper: extract the backing Vec<JsValue> from `this` if it is an array.
pub fn get_this_array_elements(this: &JsValue) -> rawjs_common::Result<Vec<JsValue>> {
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                crate::object::ObjectInternal::Array(elems) => Ok(elems.clone()),
                _ => Err(rawjs_common::RawJsError::type_error("not an array")),
            }
        }
        _ => Err(rawjs_common::RawJsError::type_error("not an array")),
    }
}

/// Helper: get the string value from `this`.
pub fn get_this_string(this: &JsValue) -> String {
    this.to_string_value()
}
