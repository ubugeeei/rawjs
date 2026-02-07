use rawjs_common::{RawJsError, Result};

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

fn json_stringify(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn stringify_value(value: &JsValue, indent: &str, depth: usize) -> Option<String> {
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
        JsValue::Symbol(_) => None, // Symbols are omitted in JSON.stringify
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

pub fn json_quote_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 2);
    result.push('"');
    for ch in s.chars() {
        match ch {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\u{0008}' => result.push_str("\\b"),
            '\u{000C}' => result.push_str("\\f"),
            c if (c as u32) < 0x20 => {
                result.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => result.push(c),
        }
    }
    result.push('"');
    result
}

fn json_parse(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let text = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    let trimmed = text.trim();

    parse_json_value(heap, trimmed)
        .map(|(val, _rest)| val)
        .ok_or_else(|| RawJsError::syntax_error("Unexpected end of JSON input", None))
}

fn parse_json_value<'a>(heap: &mut Heap, input: &'a str) -> Option<(JsValue, &'a str)> {
    let input = input.trim_start();
    if input.is_empty() {
        return None;
    }

    match input.as_bytes()[0] {
        b'"' => parse_json_string(input).map(|(s, rest)| (JsValue::string(s), rest)),
        b'{' => parse_json_object(heap, input),
        b'[' => parse_json_array(heap, input),
        b't' if input.starts_with("true") => Some((JsValue::Boolean(true), &input[4..])),
        b'f' if input.starts_with("false") => Some((JsValue::Boolean(false), &input[5..])),
        b'n' if input.starts_with("null") => Some((JsValue::Null, &input[4..])),
        b'-' | b'0'..=b'9' => parse_json_number(input),
        _ => None,
    }
}

fn parse_json_string(input: &str) -> Option<(String, &str)> {
    if !input.starts_with('"') {
        return None;
    }
    let mut result = String::new();
    let mut chars = input[1..].char_indices();
    while let Some((i, ch)) = chars.next() {
        match ch {
            '"' => {
                return Some((result, &input[i + 2..]));
            }
            '\\' => {
                if let Some((_, esc)) = chars.next() {
                    match esc {
                        '"' => result.push('"'),
                        '\\' => result.push('\\'),
                        '/' => result.push('/'),
                        'n' => result.push('\n'),
                        'r' => result.push('\r'),
                        't' => result.push('\t'),
                        'b' => result.push('\u{0008}'),
                        'f' => result.push('\u{000C}'),
                        'u' => {
                            let mut hex = String::with_capacity(4);
                            for _ in 0..4 {
                                if let Some((_, h)) = chars.next() {
                                    hex.push(h);
                                } else {
                                    return None;
                                }
                            }
                            if let Ok(code) = u32::from_str_radix(&hex, 16) {
                                if let Some(c) = char::from_u32(code) {
                                    result.push(c);
                                }
                            }
                        }
                        _ => {
                            result.push('\\');
                            result.push(esc);
                        }
                    }
                }
            }
            _ => result.push(ch),
        }
    }
    None
}

fn parse_json_number(input: &str) -> Option<(JsValue, &str)> {
    let mut end = 0;
    let bytes = input.as_bytes();
    if end < bytes.len() && bytes[end] == b'-' {
        end += 1;
    }
    while end < bytes.len() && bytes[end].is_ascii_digit() {
        end += 1;
    }
    if end < bytes.len() && bytes[end] == b'.' {
        end += 1;
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
    }
    if end < bytes.len() && (bytes[end] == b'e' || bytes[end] == b'E') {
        end += 1;
        if end < bytes.len() && (bytes[end] == b'+' || bytes[end] == b'-') {
            end += 1;
        }
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
    }
    let num_str = &input[..end];
    match num_str.parse::<f64>() {
        Ok(n) => Some((JsValue::Number(n), &input[end..])),
        Err(_) => None,
    }
}

fn parse_json_object<'a>(heap: &mut Heap, input: &'a str) -> Option<(JsValue, &'a str)> {
    if !input.starts_with('{') {
        return None;
    }
    let mut rest = input[1..].trim_start();
    let mut obj = JsObject::ordinary();

    if let Some(rest_after) = rest.strip_prefix('}') {
        let ptr = heap.alloc_object(obj);
        return Some((JsValue::Object(ptr), rest_after));
    }

    loop {
        let (key, after_key) = parse_json_string(rest.trim_start())?;
        rest = after_key.trim_start();

        if !rest.starts_with(':') {
            return None;
        }
        rest = rest[1..].trim_start();

        let (val, after_val) = parse_json_value(heap, rest)?;
        rest = after_val.trim_start();

        obj.set_property(key, val);

        if rest.starts_with(',') {
            rest = rest[1..].trim_start();
        } else if rest.starts_with('}') {
            rest = &rest[1..];
            break;
        } else {
            return None;
        }
    }

    let ptr = heap.alloc_object(obj);
    Some((JsValue::Object(ptr), rest))
}

fn parse_json_array<'a>(heap: &mut Heap, input: &'a str) -> Option<(JsValue, &'a str)> {
    if !input.starts_with('[') {
        return None;
    }
    let mut rest = input[1..].trim_start();
    let mut elements = Vec::new();

    if let Some(rest_after) = rest.strip_prefix(']') {
        let ptr = heap.alloc_array(elements);
        return Some((JsValue::Object(ptr), rest_after));
    }

    loop {
        let (val, after_val) = parse_json_value(heap, rest)?;
        rest = after_val.trim_start();
        elements.push(val);

        if rest.starts_with(',') {
            rest = rest[1..].trim_start();
        } else if rest.starts_with(']') {
            rest = &rest[1..];
            break;
        } else {
            return None;
        }
    }

    let ptr = heap.alloc_array(elements);
    Some((JsValue::Object(ptr), rest))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_stringify_basic() {
        let mut heap = Heap::new();
        assert_eq!(
            json_stringify(&mut heap, &JsValue::Undefined, &[JsValue::Number(42.0)]).unwrap(),
            JsValue::string("42")
        );
        assert_eq!(
            json_stringify(&mut heap, &JsValue::Undefined, &[JsValue::string("hello")]).unwrap(),
            JsValue::string("\"hello\"")
        );
        assert_eq!(
            json_stringify(&mut heap, &JsValue::Undefined, &[JsValue::Null]).unwrap(),
            JsValue::string("null")
        );
        assert_eq!(
            json_stringify(&mut heap, &JsValue::Undefined, &[JsValue::Boolean(true)]).unwrap(),
            JsValue::string("true")
        );
    }

    #[test]
    fn test_json_stringify_object() {
        let mut heap = Heap::new();
        let mut obj = JsObject::ordinary();
        obj.set_property("x".to_string(), JsValue::Number(1.0));
        obj.set_property("y".to_string(), JsValue::Number(2.0));
        let ptr = heap.alloc(obj);
        let result =
            json_stringify(&mut heap, &JsValue::Undefined, &[JsValue::Object(ptr)]).unwrap();
        let s = result.to_string_value();
        assert!(s.contains("\"x\":1"));
        assert!(s.contains("\"y\":2"));
    }

    #[test]
    fn test_json_stringify_array() {
        let mut heap = Heap::new();
        let arr = heap.alloc(JsObject::array(vec![
            JsValue::Number(1.0),
            JsValue::Number(2.0),
            JsValue::Number(3.0),
        ]));
        let result =
            json_stringify(&mut heap, &JsValue::Undefined, &[JsValue::Object(arr)]).unwrap();
        assert_eq!(result, JsValue::string("[1,2,3]"));
    }

    #[test]
    fn test_json_parse_basic() {
        let mut heap = Heap::new();
        assert_eq!(
            json_parse(&mut heap, &JsValue::Undefined, &[JsValue::string("42")]).unwrap(),
            JsValue::Number(42.0)
        );
        assert_eq!(
            json_parse(
                &mut heap,
                &JsValue::Undefined,
                &[JsValue::string("\"hello\"")]
            )
            .unwrap(),
            JsValue::string("hello")
        );
        assert_eq!(
            json_parse(&mut heap, &JsValue::Undefined, &[JsValue::string("true")]).unwrap(),
            JsValue::Boolean(true)
        );
        assert_eq!(
            json_parse(&mut heap, &JsValue::Undefined, &[JsValue::string("null")]).unwrap(),
            JsValue::Null
        );
    }

    #[test]
    fn test_json_parse_array() {
        let mut heap = Heap::new();
        let result = json_parse(
            &mut heap,
            &JsValue::Undefined,
            &[JsValue::string("[1, 2, 3]")],
        )
        .unwrap();
        assert!(result.is_array());
        if let JsValue::Object(ptr) = &result {
            let obj = ptr.borrow();
            assert_eq!(obj.array_length(), 3);
        }
    }

    #[test]
    fn test_json_parse_object() {
        let mut heap = Heap::new();
        let result = json_parse(
            &mut heap,
            &JsValue::Undefined,
            &[JsValue::string("{\"x\": 1, \"y\": 2}")],
        )
        .unwrap();
        assert!(result.is_object());
        if let JsValue::Object(ptr) = &result {
            let obj = ptr.borrow();
            assert_eq!(obj.get_property("x"), JsValue::Number(1.0));
            assert_eq!(obj.get_property("y"), JsValue::Number(2.0));
        }
    }

    #[test]
    fn test_json_quote_string_escapes() {
        assert_eq!(json_quote_string("hello"), "\"hello\"");
        assert_eq!(json_quote_string("say \"hi\""), "\"say \\\"hi\\\"\"");
        assert_eq!(json_quote_string("line\nnewline"), "\"line\\nnewline\"");
        assert_eq!(json_quote_string("tab\there"), "\"tab\\there\"");
    }
}
