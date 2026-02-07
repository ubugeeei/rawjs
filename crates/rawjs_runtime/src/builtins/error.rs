use rawjs_common::Result;

use crate::gc::{GcPtr, Heap};
use crate::object::{JsObject, NativeFn};
use crate::value::JsValue;

pub fn create_error_constructor(heap: &mut Heap, name: &str) -> GcPtr<JsObject> {
    let native_fn: NativeFn = match name {
        "TypeError" => error_ctor_type,
        "ReferenceError" => error_ctor_reference,
        "SyntaxError" => error_ctor_syntax,
        "RangeError" => error_ctor_range,
        _ => error_ctor_generic,
    };

    let func = JsObject::native_function(name, native_fn);
    heap.alloc(func)
}

fn make_error_object(heap: &mut Heap, name: &str, args: &[JsValue]) -> Result<JsValue> {
    let message = args
        .first()
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let mut obj = JsObject::error(message.clone());
    obj.set_property("name".to_string(), JsValue::string(name));
    obj.set_property("message".to_string(), JsValue::string(message));
    let ptr = heap.alloc(obj);
    Ok(JsValue::Object(ptr))
}

fn error_ctor_generic(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    make_error_object(heap, "Error", args)
}

fn error_ctor_type(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    make_error_object(heap, "TypeError", args)
}

fn error_ctor_reference(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    make_error_object(heap, "ReferenceError", args)
}

fn error_ctor_syntax(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    make_error_object(heap, "SyntaxError", args)
}

fn error_ctor_range(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    make_error_object(heap, "RangeError", args)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_constructor() {
        let mut heap = Heap::new();
        let err_ctor = create_error_constructor(&mut heap, "TypeError");
        assert!(err_ctor.borrow().is_function());
        let result =
            error_ctor_type(&mut heap, &JsValue::Undefined, &[JsValue::string("oops")]).unwrap();
        if let JsValue::Object(ptr) = &result {
            let obj = ptr.borrow();
            assert_eq!(obj.get_property("name"), JsValue::string("TypeError"));
            assert_eq!(obj.get_property("message"), JsValue::string("oops"));
        } else {
            panic!("Expected error object");
        }
    }
}
