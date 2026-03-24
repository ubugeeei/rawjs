use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};
use crate::object::{JsObject, ObjectInternal};
use crate::value::JsValue;

use super::helpers::set_native;

// ============================================================================
// Object.prototype
// ============================================================================

pub fn create_object_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();

    set_native(&mut obj, "hasOwnProperty", object_has_own_property);
    set_native(&mut obj, "toString", object_to_string);
    set_native(&mut obj, "valueOf", object_value_of);
    set_native(&mut obj, "isPrototypeOf", object_is_prototype_of);
    set_native(
        &mut obj,
        "propertyIsEnumerable",
        object_property_is_enumerable,
    );

    heap.alloc(obj)
}

fn object_has_own_property(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let key = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    match this {
        JsValue::Object(obj) => {
            let has = obj.borrow().has_own_property(&key);
            Ok(JsValue::Boolean(has))
        }
        _ => Ok(JsValue::Boolean(false)),
    }
}

fn object_to_string(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let tag = match this {
        JsValue::Undefined => "Undefined",
        JsValue::Null => "Null",
        JsValue::Boolean(_) => "Boolean",
        JsValue::Number(_) => "Number",
        JsValue::String(_) => "String",
        JsValue::Symbol(_) => "Symbol",
        JsValue::Object(obj) => {
            let o = obj.borrow();
            match &o.internal {
                ObjectInternal::Array(_) => "Array",
                ObjectInternal::Function(_) => "Function",
                ObjectInternal::StringObject(_) => "String",
                ObjectInternal::Error(_) => "Error",
                ObjectInternal::Iterator(_) => "Iterator",
                ObjectInternal::Map(_) => "Map",
                ObjectInternal::Set(_) => "Set",
                ObjectInternal::Promise(_) => "Promise",
                ObjectInternal::Generator(_) => "Generator",
                ObjectInternal::Ordinary => "Object",
            }
        }
    };
    Ok(JsValue::string(format!("[object {}]", tag)))
}

fn object_value_of(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(this.clone())
}

fn object_is_prototype_of(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => return Ok(JsValue::Boolean(false)),
    };
    let this_ptr = match this {
        JsValue::Object(ptr) => ptr.clone(),
        _ => return Ok(JsValue::Boolean(false)),
    };

    let mut current = target.borrow().prototype.clone();
    while let Some(proto) = current {
        if proto.ptr_eq(&this_ptr) {
            return Ok(JsValue::Boolean(true));
        }
        current = proto.borrow().prototype.clone();
    }
    Ok(JsValue::Boolean(false))
}

fn object_property_is_enumerable(
    _heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let key = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    match this {
        JsValue::Object(obj) => {
            let o = obj.borrow();
            if let Some(prop) = o.properties.get(&key) {
                Ok(JsValue::Boolean(prop.enumerable))
            } else {
                Ok(JsValue::Boolean(false))
            }
        }
        _ => Ok(JsValue::Boolean(false)),
    }
}

// ============================================================================
// Global Object constructor methods
// ============================================================================

pub fn create_object_constructor(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::native_function("Object", object_constructor);

    set_native(&mut obj, "keys", object_keys);
    set_native(&mut obj, "values", object_values);
    set_native(&mut obj, "entries", object_entries);
    set_native(&mut obj, "assign", object_assign);
    set_native(&mut obj, "defineProperty", object_define_property);
    set_native(&mut obj, "freeze", object_freeze);
    set_native(&mut obj, "create", object_create);
    set_native(&mut obj, "getPrototypeOf", object_get_prototype_of);
    set_native(&mut obj, "preventExtensions", object_prevent_extensions);

    heap.alloc(obj)
}

fn object_constructor(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    match args.first() {
        Some(JsValue::Object(ptr)) => Ok(JsValue::Object(ptr.clone())),
        Some(JsValue::Null) | Some(JsValue::Undefined) | None => {
            let ptr = heap.alloc_object(JsObject::ordinary());
            if let JsValue::Object(this_ptr) = this {
                ptr.borrow_mut().prototype = this_ptr.borrow().prototype.clone();
            }
            Ok(JsValue::Object(ptr))
        }
        Some(value) => Ok(value.clone()),
    }
}

fn object_keys(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => return Err(RawJsError::type_error("Object.keys called on non-object")),
    };
    let keys: Vec<JsValue> = target
        .borrow()
        .own_enumerable_keys()
        .into_iter()
        .map(|k| JsValue::string(k.as_str()))
        .collect();
    let arr = heap.alloc_array(keys);
    Ok(JsValue::Object(arr))
}

fn object_values(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => return Err(RawJsError::type_error("Object.values called on non-object")),
    };
    let obj = target.borrow();
    let values: Vec<JsValue> = obj
        .own_enumerable_keys()
        .into_iter()
        .map(|k| obj.get_property(&k))
        .collect();
    drop(obj);
    let arr = heap.alloc_array(values);
    Ok(JsValue::Object(arr))
}

fn object_entries(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Object.entries called on non-object",
            ))
        }
    };
    let obj = target.borrow();
    let entries: Vec<JsValue> = obj
        .own_enumerable_keys()
        .into_iter()
        .map(|k| {
            let val = obj.get_property(&k);
            let pair = vec![JsValue::string(k.as_str()), val];
            JsValue::Object(GcPtr::new(JsObject::array(pair)))
        })
        .collect();
    drop(obj);
    let arr = heap.alloc_array(entries);
    Ok(JsValue::Object(arr))
}

fn object_assign(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => return Err(RawJsError::type_error("Object.assign called on non-object")),
    };
    for source_val in &args[1..] {
        if let JsValue::Object(source_ptr) = source_val {
            let source = source_ptr.borrow();
            let keys = source.own_enumerable_keys();
            let pairs: Vec<(String, JsValue)> = keys
                .iter()
                .map(|k| (k.clone(), source.get_property(k)))
                .collect();
            drop(source);
            let mut tgt = target.borrow_mut();
            for (key, val) in pairs {
                tgt.set_property(key, val);
            }
        }
    }
    Ok(JsValue::Object(target))
}

fn object_define_property(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Object.defineProperty called on non-object",
            ))
        }
    };
    let key = args
        .get(1)
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    let descriptor = match args.get(2) {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Property description must be an object",
            ))
        }
    };

    let descriptor = descriptor.borrow();
    let getter = descriptor.get_property("get");
    let setter = descriptor.get_property("set");
    let prop = if !getter.is_undefined() || !setter.is_undefined() {
        crate::object::Property::accessor(
            (!getter.is_undefined()).then_some(getter),
            (!setter.is_undefined()).then_some(setter),
            descriptor.get_property("enumerable").to_boolean(),
            descriptor.get_property("configurable").to_boolean(),
        )
    } else {
        crate::object::Property {
            value: descriptor.get_property("value"),
            get: None,
            set: None,
            writable: descriptor.get_property("writable").to_boolean(),
            enumerable: descriptor.get_property("enumerable").to_boolean(),
            configurable: descriptor.get_property("configurable").to_boolean(),
        }
    };
    drop(descriptor);

    target.borrow_mut().define_property(key, prop);
    Ok(JsValue::Object(target))
}

fn object_freeze(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        Some(other) => return Ok(other.clone()),
        None => return Ok(JsValue::Undefined),
    };
    let mut obj = target.borrow_mut();
    let keys: Vec<String> = obj.properties.keys().cloned().collect();
    for key in keys {
        if let Some(prop) = obj.properties.get_mut(&key) {
            prop.writable = false;
            prop.configurable = false;
        }
    }
    drop(obj);
    Ok(JsValue::Object(target))
}

fn object_create(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let proto = args.first().unwrap_or(&JsValue::Null);
    let obj = match proto {
        JsValue::Null => JsObject::ordinary(),
        JsValue::Object(ptr) => JsObject::with_prototype(ptr.clone()),
        _ => {
            return Err(RawJsError::type_error(
                "Object prototype may only be an Object or null",
            ))
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
            ))
        }
    };
    let prototype = target.borrow().prototype.clone();
    Ok(match prototype {
        Some(ptr) => JsValue::Object(ptr),
        None => JsValue::Null,
    })
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
}
