use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};

use crate::object::{JsObject, ObjectInternal, Property};

use crate::value::JsValue;

use super::helpers::set_native;

/// Create the Set prototype object with all Set.prototype methods.
pub fn create_set_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut proto = JsObject::ordinary();
    set_native(&mut proto, "add", set_add);
    set_native(&mut proto, "has", set_has);
    set_native(&mut proto, "delete", set_delete);
    set_native(&mut proto, "clear", set_clear);
    set_native(&mut proto, "forEach", set_for_each);
    set_native(&mut proto, "keys", set_values);
    set_native(&mut proto, "values", set_values);
    set_native(&mut proto, "entries", set_entries);
    heap.alloc(proto)
}

/// `Set()` / `new Set()` constructor — creates a new Set, optionally from an iterable.
pub fn set_constructor(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let mut values: Vec<JsValue> = Vec::new();
    if let Some(JsValue::Object(ptr)) = args.first() {
        let obj = ptr.borrow();
        if let ObjectInternal::Array(elements) = &obj.internal {
            for elem in elements {
                if !values.iter().any(|v| same_value_zero(v, elem)) {
                    values.push(elem.clone());
                }
            }
        }
    }
    let mut set_obj = JsObject::set();
    set_obj.internal = ObjectInternal::Set(values);
    let ptr = heap.alloc(set_obj);
    update_set_size(&ptr);
    Ok(JsValue::Object(ptr))
}

/// Helper: update the `size` property on a Set object.
pub(super) fn update_set_size(ptr: &GcPtr<JsObject>) {
    let len = {
        let obj = ptr.borrow();
        match &obj.internal {
            ObjectInternal::Set(values) => values.len(),
            _ => 0,
        }
    };
    ptr.borrow_mut().define_property(
        "size".to_string(),
        Property::readonly(JsValue::Number(len as f64)),
    );
}

/// SameValueZero comparison (like strict_eq but treats NaN === NaN).
pub(super) fn same_value_zero(a: &JsValue, b: &JsValue) -> bool {
    match (a, b) {
        (JsValue::Number(x), JsValue::Number(y)) => {
            if x.is_nan() && y.is_nan() {
                true
            } else {
                x == y
            }
        }
        _ => a.strict_eq(b),
    }
}

/// Helper: check if a Set contains a value.
pub(super) fn set_contains(values: &[JsValue], needle: &JsValue) -> Option<usize> {
    values.iter().position(|v| same_value_zero(v, needle))
}

pub(super) fn set_add(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let value = args.first().cloned().unwrap_or(JsValue::Undefined);
    match this {
        JsValue::Object(ptr) => {
            {
                let mut obj = ptr.borrow_mut();
                match &mut obj.internal {
                    ObjectInternal::Set(values) => {
                        if set_contains(values, &value).is_none() {
                            values.push(value);
                        }
                    }
                    _ => {
                        return Err(RawJsError::type_error(
                            "Set.prototype.add called on non-Set",
                        ));
                    }
                }
            }
            update_set_size(ptr);
            Ok(this.clone())
        }
        _ => Err(RawJsError::type_error(
            "Set.prototype.add called on non-Set",
        )),
    }
}

pub(super) fn set_has(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let value = args.first().unwrap_or(&JsValue::Undefined);
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Set(values) => {
                    Ok(JsValue::Boolean(set_contains(values, value).is_some()))
                }
                _ => Err(RawJsError::type_error(
                    "Set.prototype.has called on non-Set",
                )),
            }
        }
        _ => Err(RawJsError::type_error(
            "Set.prototype.has called on non-Set",
        )),
    }
}

use super::*;
