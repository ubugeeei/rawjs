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
