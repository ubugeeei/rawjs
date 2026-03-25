pub(super) fn string_replace(
    _heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let s = get_this_string(this);
    let search = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    let replacement = args.get(1).unwrap_or(&JsValue::Undefined).to_string_value();
    let result = if let Some(pos) = s.find(&search) {
        format!("{}{}{}", &s[..pos], replacement, &s[pos + search.len()..])
    } else {
        s
    };
    Ok(JsValue::string(result))
}

pub(super) fn string_repeat(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let count = args.first().map(|v| v.to_number()).unwrap_or(0.0);
    if count < 0.0 || count.is_infinite() {
        return Err(RawJsError::range_error("Invalid count value"));
    }
    let count = count as usize;
    Ok(JsValue::string(s.repeat(count)))
}

pub(super) fn string_pad_start(
    _heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let s = get_this_string(this);
    let target_len = args.first().map(|v| v.to_number() as usize).unwrap_or(0);
    let fill = args
        .get(1)
        .map(|v| v.to_string_value())
        .unwrap_or_else(|| " ".to_string());
    if s.len() >= target_len || fill.is_empty() {
        return Ok(JsValue::string(s));
    }
    let pad_len = target_len - s.len();
    let pad: String = fill.chars().cycle().take(pad_len).collect();
    Ok(JsValue::string(format!("{}{}", pad, s)))
}

pub(super) fn string_pad_end(
    _heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let s = get_this_string(this);
    let target_len = args.first().map(|v| v.to_number() as usize).unwrap_or(0);
    let fill = args
        .get(1)
        .map(|v| v.to_string_value())
        .unwrap_or_else(|| " ".to_string());
    if s.len() >= target_len || fill.is_empty() {
        return Ok(JsValue::string(s));
    }
    let pad_len = target_len - s.len();
    let pad: String = fill.chars().cycle().take(pad_len).collect();
    Ok(JsValue::string(format!("{}{}", s, pad)))
}

pub(super) fn string_concat(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let mut s = get_this_string(this);
    for arg in args {
        s.push_str(&arg.to_string_value());
    }
    Ok(JsValue::string(s))
}

pub(super) fn string_to_string(
    _heap: &mut Heap,
    this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    Ok(JsValue::string(get_this_string(this)))
}

pub(super) fn string_value_of(
    _heap: &mut Heap,
    this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    Ok(JsValue::string(get_this_string(this)))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    pub(super) fn test_string_char_at() {
        let mut heap = Heap::new();
        let s = JsValue::string("hello");
        assert_eq!(
            string_char_at(&mut heap, &s, &[JsValue::Number(0.0)]).unwrap(),
            JsValue::string("h")
        );
        assert_eq!(
            string_char_at(&mut heap, &s, &[JsValue::Number(4.0)]).unwrap(),
            JsValue::string("o")
        );
        assert_eq!(
            string_char_at(&mut heap, &s, &[JsValue::Number(10.0)]).unwrap(),
            JsValue::string("")
        );
    }
    #[test]
    pub(super) fn test_string_index_of_method() {
        let mut heap = Heap::new();
        let s = JsValue::string("hello world");
        assert_eq!(
            string_index_of(&mut heap, &s, &[JsValue::string("world")]).unwrap(),
            JsValue::Number(6.0)
        );
        assert_eq!(
            string_index_of(&mut heap, &s, &[JsValue::string("xyz")]).unwrap(),
            JsValue::Number(-1.0)
        );
    }
    #[test]
    pub(super) fn test_string_slice_method() {
        let mut heap = Heap::new();
        let s = JsValue::string("hello world");
        assert_eq!(
            string_slice(&mut heap, &s, &[JsValue::Number(0.0), JsValue::Number(5.0)]).unwrap(),
            JsValue::string("hello")
        );
        assert_eq!(
            string_slice(&mut heap, &s, &[JsValue::Number(-5.0)]).unwrap(),
            JsValue::string("world")
        );
    }
    #[test]
    pub(super) fn test_string_to_upper_lower() {
        let mut heap = Heap::new();
        let s = JsValue::string("Hello");
        assert_eq!(
            string_to_upper_case(&mut heap, &s, &[]).unwrap(),
            JsValue::string("HELLO")
        );
        assert_eq!(
            string_to_lower_case(&mut heap, &s, &[]).unwrap(),
            JsValue::string("hello")
        );
    }
    #[test]
    pub(super) fn test_string_trim_method() {
        let mut heap = Heap::new();
        let s = JsValue::string("  hello  ");
        assert_eq!(
            string_trim(&mut heap, &s, &[]).unwrap(),
            JsValue::string("hello")
        );
    }
    #[test]
    pub(super) fn test_string_split_method() {
        let mut heap = Heap::new();
        let s = JsValue::string("a,b,c");
        let result = string_split(&mut heap, &s, &[JsValue::string(",")]).unwrap();
        if let JsValue::Object(ptr) = result {
            let obj = ptr.borrow();
            assert_eq!(obj.array_length(), 3);
            assert_eq!(obj.get_property("0"), JsValue::string("a"));
            assert_eq!(obj.get_property("1"), JsValue::string("b"));
            assert_eq!(obj.get_property("2"), JsValue::string("c"));
        } else {
            panic!("Expected array");
        }
    }
    #[test]
    pub(super) fn test_string_includes_method() {
        let mut heap = Heap::new();
        let s = JsValue::string("hello world");
        assert_eq!(
            string_includes(&mut heap, &s, &[JsValue::string("world")]).unwrap(),
            JsValue::Boolean(true)
        );
        assert_eq!(
            string_includes(&mut heap, &s, &[JsValue::string("xyz")]).unwrap(),
            JsValue::Boolean(false)
        );
    }
    #[test]
    pub(super) fn test_string_starts_ends_with() {
        let mut heap = Heap::new();
        let s = JsValue::string("hello world");
        assert_eq!(
            string_starts_with(&mut heap, &s, &[JsValue::string("hello")]).unwrap(),
            JsValue::Boolean(true)
        );
        assert_eq!(
            string_ends_with(&mut heap, &s, &[JsValue::string("world")]).unwrap(),
            JsValue::Boolean(true)
        );
    }
    #[test]
    pub(super) fn test_string_replace_method() {
        let mut heap = Heap::new();
        let s = JsValue::string("hello world");
        assert_eq!(
            string_replace(
                &mut heap,
                &s,
                &[JsValue::string("world"), JsValue::string("rust")]
            )
            .unwrap(),
            JsValue::string("hello rust")
        );
    }
    #[test]
    pub(super) fn test_string_repeat_method() {
        let mut heap = Heap::new();
        let s = JsValue::string("ab");
        assert_eq!(
            string_repeat(&mut heap, &s, &[JsValue::Number(3.0)]).unwrap(),
            JsValue::string("ababab")
        );
    }
    #[test]
    pub(super) fn test_string_pad_start() {
        let mut heap = Heap::new();
        let s = JsValue::string("5");
        assert_eq!(
            string_pad_start(&mut heap, &s, &[JsValue::Number(3.0), JsValue::string("0")]).unwrap(),
            JsValue::string("005")
        );
    }
}

#[allow(unused_imports)]
use super::*;
