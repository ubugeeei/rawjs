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
    set_native(&mut proto, "keys", set_values); // keys === values for Set
    set_native(&mut proto, "values", set_values);
    set_native(&mut proto, "entries", set_entries);

    heap.alloc(proto)
}

/// `Set()` / `new Set()` constructor — creates a new Set, optionally from an iterable.
pub fn set_constructor(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let mut values: Vec<JsValue> = Vec::new();

    // If an argument is provided and is an array, initialize from it.
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
fn update_set_size(ptr: &GcPtr<JsObject>) {
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
fn same_value_zero(a: &JsValue, b: &JsValue) -> bool {
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
fn set_contains(values: &[JsValue], needle: &JsValue) -> Option<usize> {
    values.iter().position(|v| same_value_zero(v, needle))
}

// ---------------------------------------------------------------------------
// Set.prototype methods
// ---------------------------------------------------------------------------

fn set_add(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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
                        ))
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

fn set_has(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn set_delete(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let value = args.first().unwrap_or(&JsValue::Undefined);
    match this {
        JsValue::Object(ptr) => {
            let removed = {
                let mut obj = ptr.borrow_mut();
                match &mut obj.internal {
                    ObjectInternal::Set(values) => {
                        if let Some(idx) = set_contains(values, value) {
                            values.remove(idx);
                            true
                        } else {
                            false
                        }
                    }
                    _ => {
                        return Err(RawJsError::type_error(
                            "Set.prototype.delete called on non-Set",
                        ))
                    }
                }
            };
            if removed {
                update_set_size(ptr);
            }
            Ok(JsValue::Boolean(removed))
        }
        _ => Err(RawJsError::type_error(
            "Set.prototype.delete called on non-Set",
        )),
    }
}

fn set_clear(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Object(ptr) => {
            {
                let mut obj = ptr.borrow_mut();
                match &mut obj.internal {
                    ObjectInternal::Set(values) => values.clear(),
                    _ => {
                        return Err(RawJsError::type_error(
                            "Set.prototype.clear called on non-Set",
                        ))
                    }
                }
            }
            update_set_size(ptr);
            Ok(JsValue::Undefined)
        }
        _ => Err(RawJsError::type_error(
            "Set.prototype.clear called on non-Set",
        )),
    }
}

fn set_for_each(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    // forEach requires calling a JS function callback, which needs VM access.
    // Simplified stub for now.
    Ok(JsValue::Undefined)
}

fn set_values(heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Set(values) => {
                    let arr = heap.alloc(JsObject::array(values.clone()));
                    Ok(JsValue::Object(arr))
                }
                _ => Err(RawJsError::type_error(
                    "Set.prototype.values called on non-Set",
                )),
            }
        }
        _ => Err(RawJsError::type_error(
            "Set.prototype.values called on non-Set",
        )),
    }
}

fn set_entries(heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Set(values) => {
                    let pairs: Vec<JsValue> = values
                        .iter()
                        .map(|v| {
                            let pair = vec![v.clone(), v.clone()];
                            JsValue::Object(GcPtr::new(JsObject::array(pair)))
                        })
                        .collect();
                    let arr = heap.alloc(JsObject::array(pairs));
                    Ok(JsValue::Object(arr))
                }
                _ => Err(RawJsError::type_error(
                    "Set.prototype.entries called on non-Set",
                )),
            }
        }
        _ => Err(RawJsError::type_error(
            "Set.prototype.entries called on non-Set",
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_set_basic() {
        let mut heap = Heap::new();
        let set_val = set_constructor(&mut heap, &JsValue::Undefined, &[]).unwrap();

        // add
        set_add(&mut heap, &set_val, &[JsValue::Number(1.0)]).unwrap();
        set_add(&mut heap, &set_val, &[JsValue::Number(2.0)]).unwrap();
        set_add(&mut heap, &set_val, &[JsValue::Number(1.0)]).unwrap(); // duplicate

        // has
        let has = set_has(&mut heap, &set_val, &[JsValue::Number(1.0)]).unwrap();
        assert_eq!(has, JsValue::Boolean(true));
        let has = set_has(&mut heap, &set_val, &[JsValue::Number(3.0)]).unwrap();
        assert_eq!(has, JsValue::Boolean(false));

        // size should be 2 (duplicate rejected)
        if let JsValue::Object(ptr) = &set_val {
            assert_eq!(ptr.borrow().get_property("size"), JsValue::Number(2.0));
        }
    }

    #[test]
    fn test_set_delete() {
        let mut heap = Heap::new();
        let set_val = set_constructor(&mut heap, &JsValue::Undefined, &[]).unwrap();
        set_add(&mut heap, &set_val, &[JsValue::string("x")]).unwrap();

        let deleted = set_delete(&mut heap, &set_val, &[JsValue::string("x")]).unwrap();
        assert_eq!(deleted, JsValue::Boolean(true));

        let has = set_has(&mut heap, &set_val, &[JsValue::string("x")]).unwrap();
        assert_eq!(has, JsValue::Boolean(false));
    }

    #[test]
    fn test_set_clear() {
        let mut heap = Heap::new();
        let set_val = set_constructor(&mut heap, &JsValue::Undefined, &[]).unwrap();
        set_add(&mut heap, &set_val, &[JsValue::Number(1.0)]).unwrap();
        set_add(&mut heap, &set_val, &[JsValue::Number(2.0)]).unwrap();

        set_clear(&mut heap, &set_val, &[]).unwrap();
        if let JsValue::Object(ptr) = &set_val {
            assert_eq!(ptr.borrow().get_property("size"), JsValue::Number(0.0));
        }
    }

    #[test]
    fn test_set_from_array() {
        let mut heap = Heap::new();
        let arr = JsObject::array(vec![
            JsValue::Number(1.0),
            JsValue::Number(2.0),
            JsValue::Number(2.0),
            JsValue::Number(3.0),
        ]);
        let arr_ptr = heap.alloc(arr);

        let set_val =
            set_constructor(&mut heap, &JsValue::Undefined, &[JsValue::Object(arr_ptr)]).unwrap();
        if let JsValue::Object(ptr) = &set_val {
            assert_eq!(ptr.borrow().get_property("size"), JsValue::Number(3.0));
        }
    }

    #[test]
    fn test_set_values_entries() {
        let mut heap = Heap::new();
        let set_val = set_constructor(&mut heap, &JsValue::Undefined, &[]).unwrap();
        set_add(&mut heap, &set_val, &[JsValue::Number(1.0)]).unwrap();
        set_add(&mut heap, &set_val, &[JsValue::Number(2.0)]).unwrap();

        let values = set_values(&mut heap, &set_val, &[]).unwrap();
        if let JsValue::Object(ptr) = values {
            let obj = ptr.borrow();
            assert_eq!(obj.array_length(), 2);
        }

        let entries = set_entries(&mut heap, &set_val, &[]).unwrap();
        if let JsValue::Object(ptr) = entries {
            let obj = ptr.borrow();
            assert_eq!(obj.array_length(), 2);
        }
    }
}
