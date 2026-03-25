pub(super) fn object_define_property(
    _heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Object.defineProperty called on non-object",
            ));
        }
    };
    let key = args.get(1).unwrap_or(&JsValue::Undefined).to_string_value();
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

pub(super) fn object_get_own_property_descriptor(
    heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Object.getOwnPropertyDescriptor called on non-object",
            ));
        }
    };
    let key = args.get(1).unwrap_or(&JsValue::Undefined).to_string_value();
    let Some(desc) = target.borrow().get_own_property_descriptor(&key) else {
        return Ok(JsValue::Undefined);
    };
    let mut descriptor = JsObject::ordinary();
    if let JsValue::Object(object_ctor) = this {
        if let JsValue::Object(object_proto) = object_ctor.borrow().get_property("prototype") {
            descriptor.set_prototype(Some(object_proto));
        }
    }
    if let Some(getter) = desc.get {
        descriptor.define_property("get".to_string(), crate::object::Property::data(getter));
    }
    if let Some(setter) = desc.set {
        descriptor.define_property("set".to_string(), crate::object::Property::data(setter));
    }
    if descriptor.properties.is_empty() {
        descriptor.define_property(
            "value".to_string(),
            crate::object::Property::data(desc.value),
        );
        descriptor.define_property(
            "writable".to_string(),
            crate::object::Property::data(JsValue::Boolean(desc.writable)),
        );
    }
    descriptor.define_property(
        "enumerable".to_string(),
        crate::object::Property::data(JsValue::Boolean(desc.enumerable)),
    );
    descriptor.define_property(
        "configurable".to_string(),
        crate::object::Property::data(JsValue::Boolean(desc.configurable)),
    );
    Ok(JsValue::Object(heap.alloc(descriptor)))
}

pub(super) fn object_freeze(
    _heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
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

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
