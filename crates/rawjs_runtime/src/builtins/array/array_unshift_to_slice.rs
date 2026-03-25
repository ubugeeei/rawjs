pub(super) fn array_unshift(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let ptr = match this {
        JsValue::Object(p) => p.clone(),
        _ => return Err(RawJsError::type_error("not an array")),
    };
    let mut obj = ptr.borrow_mut();
    if let ObjectInternal::Array(ref mut elements) = obj.internal {
        for (i, arg) in args.iter().enumerate() {
            elements.insert(i, arg.clone());
        }
        let len = elements.len() as f64;
        obj.properties
            .insert("length".to_string(), Property::data(JsValue::Number(len)));
        Ok(JsValue::Number(len))
    } else {
        Err(RawJsError::type_error("not an array"))
    }
}

pub(super) fn array_index_of(
    _heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let elements = get_this_array_elements(this)?;
    let search = args.first().unwrap_or(&JsValue::Undefined);
    let from = args.get(1).map(|v| v.to_number() as i64).unwrap_or(0);
    let len = elements.len() as i64;
    let start = if from < 0 {
        (len + from).max(0) as usize
    } else {
        from as usize
    };
    for (i, elem) in elements.iter().enumerate().skip(start) {
        if elem.strict_eq(search) {
            return Ok(JsValue::Number(i as f64));
        }
    }
    Ok(JsValue::Number(-1.0))
}

pub(super) fn array_includes(
    _heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let elements = get_this_array_elements(this)?;
    let search = args.first().unwrap_or(&JsValue::Undefined);
    let from = args.get(1).map(|v| v.to_number() as i64).unwrap_or(0);
    let len = elements.len() as i64;
    let start = if from < 0 {
        (len + from).max(0) as usize
    } else {
        from as usize
    };
    for elem in elements.iter().skip(start) {
        if elem.strict_eq(search) || (elem.is_nan() && search.is_nan()) {
            return Ok(JsValue::Boolean(true));
        }
    }
    Ok(JsValue::Boolean(false))
}

pub(super) fn array_join(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let elements = get_this_array_elements(this)?;
    let sep = match args.first() {
        Some(JsValue::Undefined) | None => ",".to_string(),
        Some(v) => v.to_string_value(),
    };
    let parts: Vec<String> = elements
        .iter()
        .map(|v| {
            if v.is_nullish() {
                String::new()
            } else {
                v.to_string_value()
            }
        })
        .collect();
    Ok(JsValue::string(parts.join(&sep)))
}

pub(super) fn array_slice(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let elements = get_this_array_elements(this)?;
    let len = elements.len() as i64;
    let start_raw = args.first().map(|v| v.to_number() as i64).unwrap_or(0);
    let end_raw = args.get(1).map(|v| v.to_number() as i64).unwrap_or(len);
    let start = if start_raw < 0 {
        (len + start_raw).max(0) as usize
    } else {
        (start_raw as usize).min(elements.len())
    };
    let end = if end_raw < 0 {
        (len + end_raw).max(0) as usize
    } else {
        (end_raw as usize).min(elements.len())
    };
    let sliced = if start < end {
        elements[start..end].to_vec()
    } else {
        Vec::new()
    };
    let arr = heap.alloc_array(sliced);
    Ok(JsValue::Object(arr))
}

use super::*;
