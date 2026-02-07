use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};
use crate::object::{JsObject, ObjectInternal, Property};
use crate::value::JsValue;

use super::helpers::set_native;

/// Create the Map prototype object with all Map.prototype methods.
pub fn create_map_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut proto = JsObject::ordinary();

    set_native(&mut proto, "get", map_get);
    set_native(&mut proto, "set", map_set);
    set_native(&mut proto, "has", map_has);
    set_native(&mut proto, "delete", map_delete);
    set_native(&mut proto, "clear", map_clear);
    set_native(&mut proto, "forEach", map_for_each);
    set_native(&mut proto, "keys", map_keys);
    set_native(&mut proto, "values", map_values);
    set_native(&mut proto, "entries", map_entries);

    heap.alloc(proto)
}

/// `Map()` / `new Map()` constructor — creates a new Map, optionally from an iterable.
pub fn map_constructor(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let mut entries: Vec<(JsValue, JsValue)> = Vec::new();

    // If an argument is provided and is an array of [key, value] pairs, initialize from it.
    if let Some(JsValue::Object(ptr)) = args.first() {
        let obj = ptr.borrow();
        if let ObjectInternal::Array(elements) = &obj.internal {
            for elem in elements {
                if let JsValue::Object(pair_ptr) = elem {
                    let pair_obj = pair_ptr.borrow();
                    if let ObjectInternal::Array(pair) = &pair_obj.internal {
                        let key = pair.first().cloned().unwrap_or(JsValue::Undefined);
                        let value = pair.get(1).cloned().unwrap_or(JsValue::Undefined);
                        entries.push((key, value));
                    }
                }
            }
        }
    }

    let mut map_obj = JsObject::map();
    map_obj.internal = ObjectInternal::Map(entries);
    let ptr = heap.alloc(map_obj);

    // Set the size property
    update_map_size(&ptr);

    Ok(JsValue::Object(ptr))
}

/// Helper: update the `size` property on a Map object.
fn update_map_size(ptr: &GcPtr<JsObject>) {
    let len = {
        let obj = ptr.borrow();
        match &obj.internal {
            ObjectInternal::Map(entries) => entries.len(),
            _ => 0,
        }
    };
    ptr.borrow_mut().define_property(
        "size".to_string(),
        Property::readonly(JsValue::Number(len as f64)),
    );
}

/// Helper: find the index of a key in a Map using SameValueZero.
fn find_map_key(entries: &[(JsValue, JsValue)], key: &JsValue) -> Option<usize> {
    entries.iter().position(|(k, _)| same_value_zero(k, key))
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

// ---------------------------------------------------------------------------
// Map.prototype methods
// ---------------------------------------------------------------------------

fn map_get(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let key = args.first().unwrap_or(&JsValue::Undefined);
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Map(entries) => {
                    if let Some(idx) = find_map_key(entries, key) {
                        Ok(entries[idx].1.clone())
                    } else {
                        Ok(JsValue::Undefined)
                    }
                }
                _ => Err(RawJsError::type_error(
                    "Map.prototype.get called on non-Map",
                )),
            }
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.get called on non-Map",
        )),
    }
}

fn map_set(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let key = args.first().cloned().unwrap_or(JsValue::Undefined);
    let value = args.get(1).cloned().unwrap_or(JsValue::Undefined);
    match this {
        JsValue::Object(ptr) => {
            {
                let mut obj = ptr.borrow_mut();
                match &mut obj.internal {
                    ObjectInternal::Map(entries) => {
                        if let Some(idx) = find_map_key(entries, &key) {
                            entries[idx].1 = value;
                        } else {
                            entries.push((key, value));
                        }
                    }
                    _ => {
                        return Err(RawJsError::type_error(
                            "Map.prototype.set called on non-Map",
                        ))
                    }
                }
            }
            update_map_size(ptr);
            Ok(this.clone())
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.set called on non-Map",
        )),
    }
}

fn map_has(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let key = args.first().unwrap_or(&JsValue::Undefined);
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Map(entries) => {
                    Ok(JsValue::Boolean(find_map_key(entries, key).is_some()))
                }
                _ => Err(RawJsError::type_error(
                    "Map.prototype.has called on non-Map",
                )),
            }
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.has called on non-Map",
        )),
    }
}

fn map_delete(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let key = args.first().unwrap_or(&JsValue::Undefined);
    match this {
        JsValue::Object(ptr) => {
            let removed = {
                let mut obj = ptr.borrow_mut();
                match &mut obj.internal {
                    ObjectInternal::Map(entries) => {
                        if let Some(idx) = find_map_key(entries, key) {
                            entries.remove(idx);
                            true
                        } else {
                            false
                        }
                    }
                    _ => {
                        return Err(RawJsError::type_error(
                            "Map.prototype.delete called on non-Map",
                        ))
                    }
                }
            };
            if removed {
                update_map_size(ptr);
            }
            Ok(JsValue::Boolean(removed))
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.delete called on non-Map",
        )),
    }
}

fn map_clear(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Object(ptr) => {
            {
                let mut obj = ptr.borrow_mut();
                match &mut obj.internal {
                    ObjectInternal::Map(entries) => entries.clear(),
                    _ => {
                        return Err(RawJsError::type_error(
                            "Map.prototype.clear called on non-Map",
                        ))
                    }
                }
            }
            update_map_size(ptr);
            Ok(JsValue::Undefined)
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.clear called on non-Map",
        )),
    }
}

fn map_for_each(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    // forEach requires calling a JS function callback, which needs VM access.
    // For now, return undefined (a simplified stub).
    Ok(JsValue::Undefined)
}

fn map_keys(heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Map(entries) => {
                    let keys: Vec<JsValue> = entries.iter().map(|(k, _)| k.clone()).collect();
                    let arr = heap.alloc(JsObject::array(keys));
                    Ok(JsValue::Object(arr))
                }
                _ => Err(RawJsError::type_error(
                    "Map.prototype.keys called on non-Map",
                )),
            }
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.keys called on non-Map",
        )),
    }
}

fn map_values(heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Map(entries) => {
                    let values: Vec<JsValue> = entries.iter().map(|(_, v)| v.clone()).collect();
                    let arr = heap.alloc(JsObject::array(values));
                    Ok(JsValue::Object(arr))
                }
                _ => Err(RawJsError::type_error(
                    "Map.prototype.values called on non-Map",
                )),
            }
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.values called on non-Map",
        )),
    }
}

fn map_entries(heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Map(entries) => {
                    let pairs: Vec<JsValue> = entries
                        .iter()
                        .map(|(k, v)| {
                            let pair = vec![k.clone(), v.clone()];
                            JsValue::Object(GcPtr::new(JsObject::array(pair)))
                        })
                        .collect();
                    let arr = heap.alloc(JsObject::array(pairs));
                    Ok(JsValue::Object(arr))
                }
                _ => Err(RawJsError::type_error(
                    "Map.prototype.entries called on non-Map",
                )),
            }
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.entries called on non-Map",
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_basic() {
        let mut heap = Heap::new();
        let map_val = map_constructor(&mut heap, &JsValue::Undefined, &[]).unwrap();

        // set and get
        map_set(
            &mut heap,
            &map_val,
            &[JsValue::string("a"), JsValue::Number(1.0)],
        )
        .unwrap();
        let result = map_get(&mut heap, &map_val, &[JsValue::string("a")]).unwrap();
        assert_eq!(result, JsValue::Number(1.0));

        // has
        let has = map_has(&mut heap, &map_val, &[JsValue::string("a")]).unwrap();
        assert_eq!(has, JsValue::Boolean(true));
        let has = map_has(&mut heap, &map_val, &[JsValue::string("b")]).unwrap();
        assert_eq!(has, JsValue::Boolean(false));
    }

    #[test]
    fn test_map_delete() {
        let mut heap = Heap::new();
        let map_val = map_constructor(&mut heap, &JsValue::Undefined, &[]).unwrap();
        map_set(
            &mut heap,
            &map_val,
            &[JsValue::string("x"), JsValue::Number(42.0)],
        )
        .unwrap();

        let deleted = map_delete(&mut heap, &map_val, &[JsValue::string("x")]).unwrap();
        assert_eq!(deleted, JsValue::Boolean(true));

        let result = map_get(&mut heap, &map_val, &[JsValue::string("x")]).unwrap();
        assert_eq!(result, JsValue::Undefined);
    }

    #[test]
    fn test_map_clear() {
        let mut heap = Heap::new();
        let map_val = map_constructor(&mut heap, &JsValue::Undefined, &[]).unwrap();
        map_set(
            &mut heap,
            &map_val,
            &[JsValue::string("a"), JsValue::Number(1.0)],
        )
        .unwrap();
        map_set(
            &mut heap,
            &map_val,
            &[JsValue::string("b"), JsValue::Number(2.0)],
        )
        .unwrap();

        map_clear(&mut heap, &map_val, &[]).unwrap();
        let has = map_has(&mut heap, &map_val, &[JsValue::string("a")]).unwrap();
        assert_eq!(has, JsValue::Boolean(false));
    }

    #[test]
    fn test_map_size() {
        let mut heap = Heap::new();
        let map_val = map_constructor(&mut heap, &JsValue::Undefined, &[]).unwrap();

        // Initial size is 0.
        if let JsValue::Object(ptr) = &map_val {
            assert_eq!(ptr.borrow().get_property("size"), JsValue::Number(0.0));
        }

        map_set(
            &mut heap,
            &map_val,
            &[JsValue::string("a"), JsValue::Number(1.0)],
        )
        .unwrap();
        if let JsValue::Object(ptr) = &map_val {
            assert_eq!(ptr.borrow().get_property("size"), JsValue::Number(1.0));
        }
    }

    #[test]
    fn test_map_overwrite() {
        let mut heap = Heap::new();
        let map_val = map_constructor(&mut heap, &JsValue::Undefined, &[]).unwrap();
        map_set(
            &mut heap,
            &map_val,
            &[JsValue::string("a"), JsValue::Number(1.0)],
        )
        .unwrap();
        map_set(
            &mut heap,
            &map_val,
            &[JsValue::string("a"), JsValue::Number(2.0)],
        )
        .unwrap();

        let result = map_get(&mut heap, &map_val, &[JsValue::string("a")]).unwrap();
        assert_eq!(result, JsValue::Number(2.0));

        // Size should still be 1 after overwrite.
        if let JsValue::Object(ptr) = &map_val {
            assert_eq!(ptr.borrow().get_property("size"), JsValue::Number(1.0));
        }
    }

    #[test]
    fn test_map_keys_values_entries() {
        let mut heap = Heap::new();
        let map_val = map_constructor(&mut heap, &JsValue::Undefined, &[]).unwrap();
        map_set(
            &mut heap,
            &map_val,
            &[JsValue::string("a"), JsValue::Number(1.0)],
        )
        .unwrap();
        map_set(
            &mut heap,
            &map_val,
            &[JsValue::string("b"), JsValue::Number(2.0)],
        )
        .unwrap();

        let keys = map_keys(&mut heap, &map_val, &[]).unwrap();
        if let JsValue::Object(ptr) = keys {
            let obj = ptr.borrow();
            assert_eq!(obj.array_length(), 2);
        }

        let values = map_values(&mut heap, &map_val, &[]).unwrap();
        if let JsValue::Object(ptr) = values {
            let obj = ptr.borrow();
            assert_eq!(obj.array_length(), 2);
        }

        let entries = map_entries(&mut heap, &map_val, &[]).unwrap();
        if let JsValue::Object(ptr) = entries {
            let obj = ptr.borrow();
            assert_eq!(obj.array_length(), 2);
        }
    }

    #[test]
    fn test_same_value_zero_nan() {
        assert!(same_value_zero(
            &JsValue::Number(f64::NAN),
            &JsValue::Number(f64::NAN)
        ));
        assert!(same_value_zero(
            &JsValue::Number(0.0),
            &JsValue::Number(0.0)
        ));
        assert!(!same_value_zero(
            &JsValue::Number(0.0),
            &JsValue::Number(1.0)
        ));
    }
}
