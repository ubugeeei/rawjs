use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};

use crate::object::{JsObject, ObjectInternal, Property};

use crate::value::JsValue;

use super::helpers::{get_this_string, set_native};

pub fn create_string_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();
    set_native(&mut obj, "charAt", string_char_at);
    set_native(&mut obj, "charCodeAt", string_char_code_at);
    set_native(&mut obj, "indexOf", string_index_of);
    set_native(&mut obj, "lastIndexOf", string_last_index_of);
    set_native(&mut obj, "slice", string_slice);
    set_native(&mut obj, "substring", string_substring);
    set_native(&mut obj, "toUpperCase", string_to_upper_case);
    set_native(&mut obj, "toLowerCase", string_to_lower_case);
    set_native(&mut obj, "trim", string_trim);
    set_native(&mut obj, "trimStart", string_trim_start);
    set_native(&mut obj, "trimEnd", string_trim_end);
    set_native(&mut obj, "split", string_split);
    set_native(&mut obj, "includes", string_includes);
    set_native(&mut obj, "startsWith", string_starts_with);
    set_native(&mut obj, "endsWith", string_ends_with);
    set_native(&mut obj, "replace", string_replace);
    set_native(&mut obj, "repeat", string_repeat);
    set_native(&mut obj, "padStart", string_pad_start);
    set_native(&mut obj, "padEnd", string_pad_end);
    set_native(&mut obj, "concat", string_concat);
    set_native(&mut obj, "toString", string_to_string);
    set_native(&mut obj, "valueOf", string_value_of);
    heap.alloc(obj)
}

pub fn string_constructor(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let value = args
        .first()
        .map(|arg| arg.to_string_value())
        .unwrap_or_default();
    let is_construct_call = match (this, heap.calling_fn.clone()) {
        (JsValue::Object(this_ptr), Some(fn_ptr)) => {
            let prototype = match fn_ptr.borrow().get_property("prototype") {
                JsValue::Object(proto) => Some(proto),
                _ => None,
            };
            match (this_ptr.borrow().prototype.clone(), prototype) {
                (Some(actual), Some(expected)) => actual.ptr_eq(&expected),
                _ => false,
            }
        }
        _ => false,
    };
    if is_construct_call {
        if let JsValue::Object(this_ptr) = this {
            let mut obj = this_ptr.borrow_mut();
            obj.internal = ObjectInternal::StringObject(value.clone());
            obj.define_property(
                "length".to_string(),
                Property::readonly_builtin(JsValue::Number(value.chars().count() as f64)),
            );
        }
        return Ok(this.clone());
    }
    Ok(JsValue::string(value))
}

fn string_char_at(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let idx = args.first().map(|v| v.to_number() as usize).unwrap_or(0);
    match s.chars().nth(idx) {
        Some(ch) => Ok(JsValue::string(ch.to_string())),
        None => Ok(JsValue::string("")),
    }
}

fn string_char_code_at(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let idx = args.first().map(|v| v.to_number() as usize).unwrap_or(0);
    match s.chars().nth(idx) {
        Some(ch) => Ok(JsValue::Number(ch as u32 as f64)),
        None => Ok(JsValue::Number(f64::NAN)),
    }
}

fn string_index_of(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let search = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    let from = args
        .get(1)
        .map(|v| {
            let n = v.to_number() as i64;
            n.max(0) as usize
        })
        .unwrap_or(0);
    if from > s.len() {
        return Ok(JsValue::Number(-1.0));
    }
    match s[from..].find(&search) {
        Some(pos) => Ok(JsValue::Number((from + pos) as f64)),
        None => Ok(JsValue::Number(-1.0)),
    }
}

fn string_last_index_of(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let search = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    match s.rfind(&search) {
        Some(pos) => Ok(JsValue::Number(pos as f64)),
        None => Ok(JsValue::Number(-1.0)),
    }
}
