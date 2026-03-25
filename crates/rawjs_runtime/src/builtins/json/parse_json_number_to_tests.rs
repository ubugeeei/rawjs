pub(super) fn parse_json_number(input: &str) -> Option<(JsValue, &str)> {
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

pub(super) fn parse_json_object<'a>(heap: &mut Heap, input: &'a str) -> Option<(JsValue, &'a str)> {
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

pub(super) fn parse_json_array<'a>(heap: &mut Heap, input: &'a str) -> Option<(JsValue, &'a str)> {
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
    pub(super) fn test_json_stringify_basic() {
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
    pub(super) fn test_json_stringify_object() {
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
    pub(super) fn test_json_stringify_array() {
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
    pub(super) fn test_json_parse_basic() {
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
    pub(super) fn test_json_parse_array() {
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
    pub(super) fn test_json_parse_object() {
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
    pub(super) fn test_json_quote_string_escapes() {
        assert_eq!(json_quote_string("hello"), "\"hello\"");
        assert_eq!(json_quote_string("say \"hi\""), "\"say \\\"hi\\\"\"");
        assert_eq!(json_quote_string("line\nnewline"), "\"line\\nnewline\"");
        assert_eq!(json_quote_string("tab\there"), "\"tab\\there\"");
    }
}

use super::*;
