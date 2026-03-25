use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};

use crate::object::{JsObject, ObjectInternal, Property};

use crate::value::JsValue;

use super::helpers::{get_this_array_elements, set_native};

pub fn create_array_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();
    set_native(&mut obj, "push", array_push);
    set_native(&mut obj, "pop", array_pop);
    set_native(&mut obj, "shift", array_shift);
    set_native(&mut obj, "unshift", array_unshift);
    set_native(&mut obj, "indexOf", array_index_of);
    set_native(&mut obj, "includes", array_includes);
    set_native(&mut obj, "join", array_join);
    set_native(&mut obj, "slice", array_slice);
    set_native(&mut obj, "splice", array_splice);
    set_native(&mut obj, "concat", array_concat);
    set_native(&mut obj, "reverse", array_reverse);
    set_native(&mut obj, "forEach", array_for_each);
    set_native(&mut obj, "map", array_map);
    set_native(&mut obj, "filter", array_filter);
    set_native(&mut obj, "reduce", array_reduce);
    set_native(&mut obj, "find", array_find);
    set_native(&mut obj, "findIndex", array_find_index);
    set_native(&mut obj, "every", array_every);
    set_native(&mut obj, "some", array_some);
    set_native(&mut obj, "flat", array_flat);
    set_native(&mut obj, "fill", array_fill);
    set_native(&mut obj, "toString", array_to_string);
    set_native(&mut obj, "isArray", array_is_array);
    heap.alloc(obj)
}

pub fn array_constructor(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let elements = if args.len() == 1 {
        match args.first().unwrap() {
            JsValue::Number(n) if *n >= 0.0 && n.fract() == 0.0 => {
                vec![JsValue::Undefined; *n as usize]
            }
            other => vec![other.clone()],
        }
    } else {
        args.to_vec()
    };
    let ptr = heap.alloc(JsObject::array(elements));
    if let JsValue::Object(this_ptr) = this {
        ptr.borrow_mut()
            .set_prototype(this_ptr.borrow().prototype.clone());
    }
    Ok(JsValue::Object(ptr))
}

fn array_push(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let ptr = match this {
        JsValue::Object(p) => p.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Array.prototype.push called on non-object",
            ));
        }
    };
    let mut obj = ptr.borrow_mut();
    if let ObjectInternal::Array(ref mut elements) = obj.internal {
        for arg in args {
            elements.push(arg.clone());
        }
        let len = elements.len() as f64;
        obj.properties
            .insert("length".to_string(), Property::data(JsValue::Number(len)));
        Ok(JsValue::Number(len))
    } else {
        Err(RawJsError::type_error("not an array"))
    }
}

fn array_pop(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let ptr = match this {
        JsValue::Object(p) => p.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Array.prototype.pop called on non-object",
            ));
        }
    };
    let mut obj = ptr.borrow_mut();
    if let ObjectInternal::Array(ref mut elements) = obj.internal {
        let val = elements.pop().unwrap_or(JsValue::Undefined);
        let len = elements.len() as f64;
        obj.properties
            .insert("length".to_string(), Property::data(JsValue::Number(len)));
        Ok(val)
    } else {
        Err(RawJsError::type_error("not an array"))
    }
}

fn array_shift(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let ptr = match this {
        JsValue::Object(p) => p.clone(),
        _ => return Err(RawJsError::type_error("not an array")),
    };
    let mut obj = ptr.borrow_mut();
    if let ObjectInternal::Array(ref mut elements) = obj.internal {
        if elements.is_empty() {
            return Ok(JsValue::Undefined);
        }
        let val = elements.remove(0);
        let len = elements.len() as f64;
        obj.properties
            .insert("length".to_string(), Property::data(JsValue::Number(len)));
        Ok(val)
    } else {
        Err(RawJsError::type_error("not an array"))
    }
}
