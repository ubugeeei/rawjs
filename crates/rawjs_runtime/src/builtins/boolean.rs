use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};
use crate::object::{JsObject, ObjectInternal};
use crate::value::JsValue;

use super::helpers::set_native;

pub fn create_boolean_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();

    set_native(&mut obj, "toString", boolean_to_string);
    set_native(&mut obj, "valueOf", boolean_value_of);

    heap.alloc(obj)
}

pub fn boolean_constructor(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let value = args.first().map(JsValue::to_boolean).unwrap_or(false);

    let is_construct_call = match (this, heap.calling_fn.clone()) {
        (JsValue::Object(this_ptr), Some(fn_ptr)) => {
            let prototype = match fn_ptr.borrow().get_property("prototype") {
                JsValue::Object(proto) => Some(proto),
                _ => None,
            };
            match (this_ptr.borrow().prototype.clone(), prototype) {
                (Some(actual), Some(expected)) => actual.ptr_eq(&expected),
                _ => false,
            }
        }
        _ => false,
    };

    if is_construct_call {
        if let JsValue::Object(this_ptr) = this {
            this_ptr.borrow_mut().internal = ObjectInternal::BooleanObject(value);
        }
        return Ok(this.clone());
    }

    Ok(JsValue::Boolean(value))
}

fn get_this_boolean(this: &JsValue) -> Result<bool> {
    match this {
        JsValue::Boolean(value) => Ok(*value),
        JsValue::Object(ptr) => match &ptr.borrow().internal {
            ObjectInternal::BooleanObject(value) => Ok(*value),
            _ => Err(RawJsError::type_error(
                "Boolean.prototype method requires a boolean",
            )),
        },
        _ => Err(RawJsError::type_error(
            "Boolean.prototype method requires a boolean",
        )),
    }
}

fn boolean_to_string(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::string(if get_this_boolean(this)? {
        "true"
    } else {
        "false"
    }))
}

fn boolean_value_of(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::Boolean(get_this_boolean(this)?))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_boolean_constructor_returns_primitive_when_called() {
        let mut heap = Heap::new();
        assert_eq!(
            boolean_constructor(
                &mut heap,
                &JsValue::Undefined,
                &[JsValue::string("non-empty")]
            )
            .unwrap(),
            JsValue::Boolean(true)
        );
    }

    #[test]
    fn test_boolean_value_of_supports_boxed_boolean() {
        let mut heap = Heap::new();
        let mut obj = JsObject::ordinary();
        obj.internal = ObjectInternal::BooleanObject(false);
        let ptr = heap.alloc(obj);
        assert_eq!(
            boolean_value_of(&mut heap, &JsValue::Object(ptr), &[]).unwrap(),
            JsValue::Boolean(false)
        );
    }
}
