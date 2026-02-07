use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};
use crate::object::JsObject;
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

fn string_slice(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len() as i64;

    let start_raw = args.first().map(|v| v.to_number() as i64).unwrap_or(0);
    let end_raw = args.get(1).map(|v| v.to_number() as i64).unwrap_or(len);

    let start = if start_raw < 0 {
        (len + start_raw).max(0) as usize
    } else {
        (start_raw as usize).min(chars.len())
    };
    let end = if end_raw < 0 {
        (len + end_raw).max(0) as usize
    } else {
        (end_raw as usize).min(chars.len())
    };

    if start >= end {
        return Ok(JsValue::string(""));
    }
    let result: String = chars[start..end].iter().collect();
    Ok(JsValue::string(result))
}

fn string_substring(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len();

    let start_raw = args
        .first()
        .map(|v| v.to_number().max(0.0) as usize)
        .unwrap_or(0);
    let end_raw = args
        .get(1)
        .map(|v| v.to_number().max(0.0) as usize)
        .unwrap_or(len);

    let start = start_raw.min(len);
    let end = end_raw.min(len);

    let (a, b) = if start > end {
        (end, start)
    } else {
        (start, end)
    };

    let result: String = chars[a..b].iter().collect();
    Ok(JsValue::string(result))
}

fn string_to_upper_case(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    Ok(JsValue::string(s.to_uppercase()))
}

fn string_to_lower_case(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    Ok(JsValue::string(s.to_lowercase()))
}

fn string_trim(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    Ok(JsValue::string(s.trim()))
}

fn string_trim_start(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    Ok(JsValue::string(s.trim_start()))
}

fn string_trim_end(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    Ok(JsValue::string(s.trim_end()))
}

fn string_split(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let separator = args.first().unwrap_or(&JsValue::Undefined);

    let parts: Vec<JsValue> = if separator.is_undefined() {
        vec![JsValue::string(s)]
    } else {
        let sep = separator.to_string_value();
        if sep.is_empty() {
            s.chars().map(|c| JsValue::string(c.to_string())).collect()
        } else {
            s.split(&sep).map(JsValue::string).collect()
        }
    };

    let limit = args.get(1).map(|v| v.to_number() as usize);
    let parts = match limit {
        Some(n) => parts.into_iter().take(n).collect(),
        None => parts,
    };

    let arr = heap.alloc_array(parts);
    Ok(JsValue::Object(arr))
}

fn string_includes(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let search = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    let from = args.get(1).map(|v| v.to_number() as usize).unwrap_or(0);
    if from > s.len() {
        return Ok(JsValue::Boolean(false));
    }
    Ok(JsValue::Boolean(s[from..].contains(&search)))
}

fn string_starts_with(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let search = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    let pos = args.get(1).map(|v| v.to_number() as usize).unwrap_or(0);
    if pos > s.len() {
        return Ok(JsValue::Boolean(false));
    }
    Ok(JsValue::Boolean(s[pos..].starts_with(&search)))
}

fn string_ends_with(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let search = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    let end_pos = args
        .get(1)
        .map(|v| (v.to_number() as usize).min(s.len()))
        .unwrap_or(s.len());
    Ok(JsValue::Boolean(s[..end_pos].ends_with(&search)))
}

fn string_replace(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn string_repeat(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    let count = args.first().map(|v| v.to_number()).unwrap_or(0.0);
    if count < 0.0 || count.is_infinite() {
        return Err(RawJsError::range_error("Invalid count value"));
    }
    let count = count as usize;
    Ok(JsValue::string(s.repeat(count)))
}

fn string_pad_start(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn string_pad_end(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn string_concat(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let mut s = get_this_string(this);
    for arg in args {
        s.push_str(&arg.to_string_value());
    }
    Ok(JsValue::string(s))
}

fn string_to_string(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::string(get_this_string(this)))
}

fn string_value_of(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::string(get_this_string(this)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_char_at() {
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
    fn test_string_index_of_method() {
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
    fn test_string_slice_method() {
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
    fn test_string_to_upper_lower() {
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
    fn test_string_trim_method() {
        let mut heap = Heap::new();
        let s = JsValue::string("  hello  ");
        assert_eq!(
            string_trim(&mut heap, &s, &[]).unwrap(),
            JsValue::string("hello")
        );
    }

    #[test]
    fn test_string_split_method() {
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
    fn test_string_includes_method() {
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
    fn test_string_starts_ends_with() {
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
    fn test_string_replace_method() {
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
    fn test_string_repeat_method() {
        let mut heap = Heap::new();
        let s = JsValue::string("ab");
        assert_eq!(
            string_repeat(&mut heap, &s, &[JsValue::Number(3.0)]).unwrap(),
            JsValue::string("ababab")
        );
    }

    #[test]
    fn test_string_pad_start() {
        let mut heap = Heap::new();
        let s = JsValue::string("5");
        assert_eq!(
            string_pad_start(&mut heap, &s, &[JsValue::Number(3.0), JsValue::string("0")]).unwrap(),
            JsValue::string("005")
        );
    }
}
