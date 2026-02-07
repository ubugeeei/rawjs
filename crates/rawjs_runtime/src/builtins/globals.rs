use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};
use crate::object::JsObject;
use crate::value::JsValue;

pub fn create_global_functions(heap: &mut Heap) -> Vec<(&'static str, GcPtr<JsObject>)> {
    let mut fns = Vec::new();

    macro_rules! global_fn {
        ($name:expr, $func:ident) => {
            fns.push(($name, heap.alloc(JsObject::native_function($name, $func))));
        };
    }

    global_fn!("isNaN", global_is_nan);
    global_fn!("isFinite", global_is_finite);
    global_fn!("parseInt", global_parse_int);
    global_fn!("parseFloat", global_parse_float);
    global_fn!("encodeURIComponent", global_encode_uri_component);
    global_fn!("decodeURIComponent", global_decode_uri_component);

    fns
}

fn global_is_nan(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Boolean(n.is_nan()))
}

fn global_is_finite(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Boolean(n.is_finite()))
}

fn global_parse_int(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    let trimmed = s.trim_start();

    let radix = args.get(1).map(|v| v.to_number() as u32).unwrap_or(0);

    let (negative, rest) = if let Some(r) = trimmed.strip_prefix('-') {
        (true, r)
    } else if let Some(r) = trimmed.strip_prefix('+') {
        (false, r)
    } else {
        (false, trimmed)
    };

    let (radix, rest) = if radix == 0 || radix == 16 {
        if rest.starts_with("0x") || rest.starts_with("0X") {
            (16, &rest[2..])
        } else if radix == 0 {
            (10, rest)
        } else {
            (radix, rest)
        }
    } else {
        (radix, rest)
    };

    if !(2..=36).contains(&radix) {
        return Ok(JsValue::Number(f64::NAN));
    }

    let mut result: f64 = 0.0;
    let mut found_digit = false;
    for ch in rest.chars() {
        match ch.to_digit(radix) {
            Some(d) => {
                result = result * (radix as f64) + (d as f64);
                found_digit = true;
            }
            None => break,
        }
    }

    if !found_digit {
        return Ok(JsValue::Number(f64::NAN));
    }

    if negative {
        result = -result;
    }
    Ok(JsValue::Number(result))
}

fn global_parse_float(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    let trimmed = s.trim_start();

    if trimmed.starts_with("Infinity") || trimmed.starts_with("+Infinity") {
        return Ok(JsValue::Number(f64::INFINITY));
    }
    if trimmed.starts_with("-Infinity") {
        return Ok(JsValue::Number(f64::NEG_INFINITY));
    }

    let mut end = 0;
    let bytes = trimmed.as_bytes();
    if end < bytes.len() && (bytes[end] == b'-' || bytes[end] == b'+') {
        end += 1;
    }
    let mut has_digits = false;
    while end < bytes.len() && bytes[end].is_ascii_digit() {
        end += 1;
        has_digits = true;
    }
    if end < bytes.len() && bytes[end] == b'.' {
        end += 1;
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
            has_digits = true;
        }
    }
    if has_digits && end < bytes.len() && (bytes[end] == b'e' || bytes[end] == b'E') {
        let save = end;
        end += 1;
        if end < bytes.len() && (bytes[end] == b'+' || bytes[end] == b'-') {
            end += 1;
        }
        let mut has_exp_digits = false;
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
            has_exp_digits = true;
        }
        if !has_exp_digits {
            end = save;
        }
    }
    if !has_digits {
        return Ok(JsValue::Number(f64::NAN));
    }

    match trimmed[..end].parse::<f64>() {
        Ok(n) => Ok(JsValue::Number(n)),
        Err(_) => Ok(JsValue::Number(f64::NAN)),
    }
}

fn global_encode_uri_component(
    _heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let s = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    let mut encoded = String::new();
    for byte in s.bytes() {
        match byte {
            b'A'..=b'Z'
            | b'a'..=b'z'
            | b'0'..=b'9'
            | b'-'
            | b'_'
            | b'.'
            | b'!'
            | b'~'
            | b'*'
            | b'\''
            | b'('
            | b')' => encoded.push(byte as char),
            _ => encoded.push_str(&format!("%{:02X}", byte)),
        }
    }
    Ok(JsValue::string(encoded))
}

fn global_decode_uri_component(
    _heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let s = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    let mut decoded = Vec::new();
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            if let Ok(byte) = u8::from_str_radix(&s[i + 1..i + 3], 16) {
                decoded.push(byte);
                i += 3;
                continue;
            }
        }
        decoded.push(bytes[i]);
        i += 1;
    }
    match String::from_utf8(decoded) {
        Ok(s) => Ok(JsValue::string(s)),
        Err(_) => Err(RawJsError::type_error("URI malformed")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_global_is_nan() {
        let mut heap = Heap::new();
        assert_eq!(
            global_is_nan(&mut heap, &JsValue::Undefined, &[JsValue::Number(f64::NAN)]).unwrap(),
            JsValue::Boolean(true)
        );
        assert_eq!(
            global_is_nan(&mut heap, &JsValue::Undefined, &[JsValue::Number(1.0)]).unwrap(),
            JsValue::Boolean(false)
        );
        assert_eq!(
            global_is_nan(&mut heap, &JsValue::Undefined, &[JsValue::string("hello")]).unwrap(),
            JsValue::Boolean(true)
        );
    }

    #[test]
    fn test_global_parse_int() {
        let mut heap = Heap::new();
        assert_eq!(
            global_parse_int(&mut heap, &JsValue::Undefined, &[JsValue::string("42")]).unwrap(),
            JsValue::Number(42.0)
        );
        assert_eq!(
            global_parse_int(&mut heap, &JsValue::Undefined, &[JsValue::string("0xFF")]).unwrap(),
            JsValue::Number(255.0)
        );
        assert_eq!(
            global_parse_int(
                &mut heap,
                &JsValue::Undefined,
                &[JsValue::string("1010"), JsValue::Number(2.0)]
            )
            .unwrap(),
            JsValue::Number(10.0)
        );
        assert!(
            global_parse_int(&mut heap, &JsValue::Undefined, &[JsValue::string("abc")])
                .unwrap()
                .is_nan()
        );
    }

    #[test]
    fn test_global_parse_float() {
        let mut heap = Heap::new();
        assert_eq!(
            global_parse_float(&mut heap, &JsValue::Undefined, &[JsValue::string("3.14")]).unwrap(),
            JsValue::Number(3.14)
        );
        assert_eq!(
            global_parse_float(&mut heap, &JsValue::Undefined, &[JsValue::string("42abc")])
                .unwrap(),
            JsValue::Number(42.0)
        );
    }

    #[test]
    fn test_encode_decode_uri_component() {
        let mut heap = Heap::new();
        let encoded = global_encode_uri_component(
            &mut heap,
            &JsValue::Undefined,
            &[JsValue::string("hello world!")],
        )
        .unwrap();
        assert_eq!(encoded, JsValue::string("hello%20world!"));

        let decoded = global_decode_uri_component(
            &mut heap,
            &JsValue::Undefined,
            &[JsValue::string("hello%20world!")],
        )
        .unwrap();
        assert_eq!(decoded, JsValue::string("hello world!"));
    }
}
