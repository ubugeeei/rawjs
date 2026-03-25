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
        map_set(
            &mut heap,
            &map_val,
            &[JsValue::string("a"), JsValue::Number(1.0)],
        )
        .unwrap();
        let result = map_get(&mut heap, &map_val, &[JsValue::string("a")]).unwrap();
        assert_eq!(result, JsValue::Number(1.0));
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
