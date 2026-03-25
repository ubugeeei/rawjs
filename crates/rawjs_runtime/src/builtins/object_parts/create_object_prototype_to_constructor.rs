use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};

use crate::object::{JsObject, ObjectInternal};

use crate::value::JsValue;

use super::helpers::set_native;

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
                ObjectInternal::ArgumentsObject(_) => "Arguments",
                ObjectInternal::BooleanObject(_) => "Boolean",
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

pub fn create_object_constructor(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::native_function("Object", object_constructor);
    set_native(&mut obj, "keys", object_keys);
    set_native(&mut obj, "values", object_values);
    set_native(&mut obj, "entries", object_entries);
    set_native(&mut obj, "assign", object_assign);
    set_native(&mut obj, "defineProperty", object_define_property);
    set_native(
        &mut obj,
        "getOwnPropertyDescriptor",
        object_get_own_property_descriptor,
    );
    set_native(&mut obj, "freeze", object_freeze);
    set_native(&mut obj, "create", object_create);
    set_native(&mut obj, "getPrototypeOf", object_get_prototype_of);
    set_native(&mut obj, "setPrototypeOf", object_set_prototype_of);
    set_native(&mut obj, "preventExtensions", object_prevent_extensions);
    heap.alloc(obj)
}
