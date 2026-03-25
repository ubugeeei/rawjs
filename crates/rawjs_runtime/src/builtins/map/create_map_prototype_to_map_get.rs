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
    update_map_size(&ptr);
    Ok(JsValue::Object(ptr))
}

/// Helper: update the `size` property on a Map object.
pub(super) fn update_map_size(ptr: &GcPtr<JsObject>) {
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
pub(super) fn find_map_key(entries: &[(JsValue, JsValue)], key: &JsValue) -> Option<usize> {
    entries.iter().position(|(k, _)| same_value_zero(k, key))
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

pub(super) fn map_get(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

use super::*;
