pub(super) fn string_slice(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

pub(super) fn string_substring(
    _heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
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

pub(super) fn string_to_upper_case(
    _heap: &mut Heap,
    this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    let s = get_this_string(this);
    Ok(JsValue::string(s.to_uppercase()))
}

pub(super) fn string_to_lower_case(
    _heap: &mut Heap,
    this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    let s = get_this_string(this);
    Ok(JsValue::string(s.to_lowercase()))
}

pub(super) fn string_trim(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let s = get_this_string(this);
    Ok(JsValue::string(s.trim()))
}

pub(super) fn string_trim_start(
    _heap: &mut Heap,
    this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    let s = get_this_string(this);
    Ok(JsValue::string(s.trim_start()))
}

pub(super) fn string_trim_end(
    _heap: &mut Heap,
    this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    let s = get_this_string(this);
    Ok(JsValue::string(s.trim_end()))
}

pub(super) fn string_split(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

pub(super) fn string_includes(
    _heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
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

pub(super) fn string_starts_with(
    _heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
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

pub(super) fn string_ends_with(
    _heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
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

use super::*;
