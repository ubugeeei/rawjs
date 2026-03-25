pub(super) fn global_encode_uri_component(
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

pub(super) fn global_decode_uri_component(
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

pub(super) fn global_print(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let rendered: Vec<String> = args
        .iter()
        .map(|arg| arg.to_js_string().to_string())
        .collect();
    println!("{}", rendered.join(" "));
    Ok(JsValue::Undefined)
}

pub(super) fn global_eval_placeholder(
    _heap: &mut Heap,
    _this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    Err(RawJsError::type_error("eval dispatch was not intercepted"))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    pub(super) fn test_global_is_nan() {
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
    pub(super) fn test_global_parse_int() {
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
    pub(super) fn test_global_parse_float() {
        let mut heap = Heap::new();
        assert_eq!(
            global_parse_float(&mut heap, &JsValue::Undefined, &[JsValue::string("2.5")]).unwrap(),
            JsValue::Number(2.5)
        );
        assert_eq!(
            global_parse_float(&mut heap, &JsValue::Undefined, &[JsValue::string("42abc")])
                .unwrap(),
            JsValue::Number(42.0)
        );
    }
    #[test]
    pub(super) fn test_encode_decode_uri_component() {
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

#[allow(unused_imports)]
use super::*;
