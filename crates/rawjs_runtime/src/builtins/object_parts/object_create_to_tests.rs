fn object_create(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let proto = args.first().unwrap_or(&JsValue::Null);
    let obj = match proto {
        JsValue::Null => JsObject::ordinary(),
        JsValue::Object(ptr) => JsObject::with_prototype(ptr.clone()),
        _ => {
            return Err(RawJsError::type_error(
                "Object prototype may only be an Object or null",
            ));
        }
    };
    let ptr = heap.alloc_object(obj);
    Ok(JsValue::Object(ptr))
}

fn object_get_prototype_of(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Object.getPrototypeOf called on non-object",
            ));
        }
    };
    let prototype = target.borrow().prototype.clone();
    Ok(match prototype {
        Some(ptr) => JsValue::Object(ptr),
        None => JsValue::Null,
    })
}

fn object_set_prototype_of(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Object.setPrototypeOf called on non-object",
            ));
        }
    };
    let prototype = match args.get(1) {
        Some(JsValue::Object(ptr)) => Some(ptr.clone()),
        Some(JsValue::Null) => None,
        _ => {
            return Err(RawJsError::type_error(
                "Object prototype must be an object or null",
            ));
        }
    };
    target.borrow_mut().set_prototype(prototype);
    Ok(JsValue::Object(target))
}

fn object_prevent_extensions(
    _heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        Some(other) => return Ok(other.clone()),
        None => return Ok(JsValue::Undefined),
    };
    target.borrow_mut().extensible = false;
    Ok(JsValue::Object(target))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_object_has_own_property_builtin() {
        let mut heap = Heap::new();
        let mut obj = JsObject::ordinary();
        obj.set_property("x".to_string(), JsValue::Number(1.0));
        let ptr = heap.alloc(obj);
        let this = JsValue::Object(ptr);
        assert_eq!(
            object_has_own_property(&mut heap, &this, &[JsValue::string("x")]).unwrap(),
            JsValue::Boolean(true)
        );
        assert_eq!(
            object_has_own_property(&mut heap, &this, &[JsValue::string("y")]).unwrap(),
            JsValue::Boolean(false)
        );
    }
    #[test]
    fn test_object_keys_builtin() {
        let mut heap = Heap::new();
        let mut obj = JsObject::ordinary();
        obj.set_property("a".to_string(), JsValue::Number(1.0));
        obj.set_property("b".to_string(), JsValue::Number(2.0));
        let ptr = heap.alloc(obj);
        let result = object_keys(&mut heap, &JsValue::Undefined, &[JsValue::Object(ptr)]).unwrap();
        if let JsValue::Object(arr_ptr) = result {
            let arr = arr_ptr.borrow();
            assert_eq!(arr.array_length(), 2);
        } else {
            panic!("Expected array");
        }
    }
    #[test]
    fn test_object_define_property_builtin() {
        let mut heap = Heap::new();
        let target = heap.alloc(JsObject::ordinary());
        let mut descriptor = JsObject::ordinary();
        descriptor.set_property("writable".to_string(), JsValue::Boolean(false));
        let descriptor = heap.alloc(descriptor);
        let result = object_define_property(
            &mut heap,
            &JsValue::Undefined,
            &[
                JsValue::Object(target.clone()),
                JsValue::string("x"),
                JsValue::Object(descriptor),
            ],
        )
        .unwrap();
        assert!(matches!(result, JsValue::Object(_)));
        let prop = target.borrow();
        let desc = prop.properties.get("x").unwrap();
        assert!(!desc.writable);
        assert!(desc.value.is_undefined());
    }
    #[test]
    fn test_object_get_prototype_of_and_prevent_extensions() {
        let mut heap = Heap::new();
        let proto = heap.alloc(JsObject::ordinary());
        let target = heap.alloc(JsObject::with_prototype(proto.clone()));
        let actual_proto = object_get_prototype_of(
            &mut heap,
            &JsValue::Undefined,
            &[JsValue::Object(target.clone())],
        )
        .unwrap();
        match actual_proto {
            JsValue::Object(ptr) => assert!(ptr.ptr_eq(&proto)),
            _ => panic!("Expected prototype object"),
        }
        object_prevent_extensions(
            &mut heap,
            &JsValue::Undefined,
            &[JsValue::Object(target.clone())],
        )
        .unwrap();
        assert!(!target.borrow().extensible);
        assert!(!target
            .borrow_mut()
            .try_set_property("x".to_string(), JsValue::Number(1.0)));
    }
    #[test]
    fn test_object_set_prototype_of_builtin() {
        let mut heap = Heap::new();
        let target = heap.alloc(JsObject::ordinary());
        let proto = heap.alloc(JsObject::ordinary());
        let result = object_set_prototype_of(
            &mut heap,
            &JsValue::Undefined,
            &[
                JsValue::Object(target.clone()),
                JsValue::Object(proto.clone()),
            ],
        )
        .unwrap();
        assert_eq!(result, JsValue::Object(target.clone()));
        let actual_proto = target.borrow().prototype.clone().unwrap();
        assert!(actual_proto.ptr_eq(&proto));
    }
    #[test]
    fn test_object_get_own_property_descriptor_builtin() {
        let mut heap = Heap::new();
        let target = heap.alloc(JsObject::ordinary());
        target.borrow_mut().define_property(
            "x".to_string(),
            crate::object::Property::builtin(JsValue::Number(1.0)),
        );
        let result = object_get_own_property_descriptor(
            &mut heap,
            &JsValue::Undefined,
            &[JsValue::Object(target), JsValue::string("x")],
        )
        .unwrap();
        let JsValue::Object(desc) = result else {
            panic!("expected descriptor object");
        };
        let desc = desc.borrow();
        assert_eq!(desc.get_property("value"), JsValue::Number(1.0));
        assert_eq!(desc.get_property("writable"), JsValue::Boolean(true));
        assert_eq!(desc.get_property("enumerable"), JsValue::Boolean(false));
        assert_eq!(desc.get_property("configurable"), JsValue::Boolean(true));
    }
}
