use rawjs_common::Result;

use crate::gc::{GcPtr, Heap};

use crate::object::{JsObject, ObjectInternal};

use crate::value::JsValue;

use super::helpers::set_native;

pub fn create_json_object(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();
    set_native(&mut obj, "stringify", json_stringify);
    set_native(&mut obj, "parse", json_parse);
    heap.alloc(obj)
}

pub(super) fn json_stringify(
    _heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let value = args.first().unwrap_or(&JsValue::Undefined);
    let indent = args.get(2).map(|v| {
        if v.is_number() {
            let n = v.to_number() as usize;
            " ".repeat(n.min(10))
        } else if v.is_string() {
            let s = v.to_string_value();
            s[..s.len().min(10)].to_string()
        } else {
            String::new()
        }
    });
    match stringify_value(value, &indent.unwrap_or_default(), 0) {
        Some(s) => Ok(JsValue::string(s)),
        None => Ok(JsValue::Undefined),
    }
}

pub(super) fn stringify_value(value: &JsValue, indent: &str, depth: usize) -> Option<String> {
    match value {
        JsValue::Undefined => None,
        JsValue::Null => Some("null".to_string()),
        JsValue::Boolean(true) => Some("true".to_string()),
        JsValue::Boolean(false) => Some("false".to_string()),
        JsValue::Number(n) => {
            if n.is_nan() || n.is_infinite() {
                Some("null".to_string())
            } else {
                Some(crate::value::number_to_string(*n))
            }
        }
        JsValue::String(s) => Some(json_quote_string(s)),
        JsValue::Symbol(_) => None,
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Array(elements) => {
                    if elements.is_empty() {
                        return Some("[]".to_string());
                    }
                    let has_indent = !indent.is_empty();
                    let mut parts = Vec::new();
                    for elem in elements {
                        let s = stringify_value(elem, indent, depth + 1)
                            .unwrap_or_else(|| "null".to_string());
                        parts.push(s);
                    }
                    if has_indent {
                        let inner_indent = indent.repeat(depth + 1);
                        let outer_indent = indent.repeat(depth);
                        let items = parts
                            .iter()
                            .map(|s| format!("{}{}", inner_indent, s))
                            .collect::<Vec<_>>()
                            .join(",\n");
                        Some(format!("[\n{}\n{}]", items, outer_indent))
                    } else {
                        Some(format!("[{}]", parts.join(",")))
                    }
                }
                ObjectInternal::Function(_) => None,
                _ => {
                    let keys = obj.own_enumerable_keys();
                    if keys.is_empty() {
                        return Some("{}".to_string());
                    }
                    let has_indent = !indent.is_empty();
                    let mut parts = Vec::new();
                    for key in &keys {
                        let val = obj.get_property(key);
                        if let Some(v_str) = stringify_value(&val, indent, depth + 1) {
                            parts.push((json_quote_string(key), v_str));
                        }
                    }
                    if parts.is_empty() {
                        return Some("{}".to_string());
                    }
                    if has_indent {
                        let inner_indent = indent.repeat(depth + 1);
                        let outer_indent = indent.repeat(depth);
                        let items = parts
                            .iter()
                            .map(|(k, v)| format!("{}{}: {}", inner_indent, k, v))
                            .collect::<Vec<_>>()
                            .join(",\n");
                        Some(format!("{{\n{}\n{}}}", items, outer_indent))
                    } else {
                        let items = parts
                            .iter()
                            .map(|(k, v)| format!("{}:{}", k, v))
                            .collect::<Vec<_>>()
                            .join(",");
                        Some(format!("{{{}}}", items))
                    }
                }
            }
        }
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
