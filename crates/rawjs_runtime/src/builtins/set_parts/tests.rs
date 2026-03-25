#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_set_basic() {
        let mut heap = Heap::new();
        let set_val = set_constructor(&mut heap, &JsValue::Undefined, &[]).unwrap();
        set_add(&mut heap, &set_val, &[JsValue::Number(1.0)]).unwrap();
        set_add(&mut heap, &set_val, &[JsValue::Number(2.0)]).unwrap();
        set_add(&mut heap, &set_val, &[JsValue::Number(1.0)]).unwrap();
        let has = set_has(&mut heap, &set_val, &[JsValue::Number(1.0)]).unwrap();
        assert_eq!(has, JsValue::Boolean(true));
        let has = set_has(&mut heap, &set_val, &[JsValue::Number(3.0)]).unwrap();
        assert_eq!(has, JsValue::Boolean(false));
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
