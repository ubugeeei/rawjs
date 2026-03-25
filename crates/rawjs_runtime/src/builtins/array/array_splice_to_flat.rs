pub(super) fn array_splice(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let ptr = match this {
        JsValue::Object(p) => p.clone(),
        _ => return Err(RawJsError::type_error("not an array")),
    };
    let mut obj = ptr.borrow_mut();
    if let ObjectInternal::Array(ref mut elements) = obj.internal {
        let len = elements.len() as i64;
        let start_raw = args.first().map(|v| v.to_number() as i64).unwrap_or(0);
        let start = if start_raw < 0 {
            (len + start_raw).max(0) as usize
        } else {
            (start_raw as usize).min(elements.len())
        };
        let delete_count = args
            .get(1)
            .map(|v| (v.to_number() as i64).max(0) as usize)
            .unwrap_or(elements.len().saturating_sub(start));
        let delete_count = delete_count.min(elements.len().saturating_sub(start));
        let removed: Vec<JsValue> = elements.drain(start..start + delete_count).collect();
        if args.len() > 2 {
            for (i, item) in args[2..].iter().enumerate() {
                elements.insert(start + i, item.clone());
            }
        }
        let new_len = elements.len() as f64;
        obj.properties.insert(
            "length".to_string(),
            Property::data(JsValue::Number(new_len)),
        );
        let arr = heap.alloc_array(removed);
        Ok(JsValue::Object(arr))
    } else {
        Err(RawJsError::type_error("not an array"))
    }
}

pub(super) fn array_concat(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let mut result = get_this_array_elements(this)?;
    for arg in args {
        match arg {
            JsValue::Object(ptr) => {
                let obj = ptr.borrow();
                if let ObjectInternal::Array(ref elems) = obj.internal {
                    result.extend(elems.iter().cloned());
                } else {
                    result.push(arg.clone());
                }
            }
            _ => result.push(arg.clone()),
        }
    }
    let arr = heap.alloc_array(result);
    Ok(JsValue::Object(arr))
}

pub(super) fn array_reverse(
    _heap: &mut Heap,
    this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    let ptr = match this {
        JsValue::Object(p) => p.clone(),
        _ => return Err(RawJsError::type_error("not an array")),
    };
    let mut obj = ptr.borrow_mut();
    if let ObjectInternal::Array(ref mut elements) = obj.internal {
        elements.reverse();
        drop(obj);
        Ok(this.clone())
    } else {
        Err(RawJsError::type_error("not an array"))
    }
}

pub(super) fn array_for_each(
    _heap: &mut Heap,
    _this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    Ok(JsValue::Undefined)
}

pub(super) fn array_map(heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let arr = heap.alloc_array(Vec::new());
    Ok(JsValue::Object(arr))
}

pub(super) fn array_filter(heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let arr = heap.alloc_array(Vec::new());
    Ok(JsValue::Object(arr))
}

pub(super) fn array_reduce(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    Ok(args.get(1).cloned().unwrap_or(JsValue::Undefined))
}

pub(super) fn array_find(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::Undefined)
}

pub(super) fn array_find_index(
    _heap: &mut Heap,
    _this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    Ok(JsValue::Number(-1.0))
}

pub(super) fn array_every(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::Boolean(true))
}

pub(super) fn array_some(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::Boolean(false))
}

pub(super) fn array_flat(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let elements = get_this_array_elements(this)?;
    let depth = args
        .first()
        .map(|v| v.to_number() as i64)
        .unwrap_or(1)
        .max(0) as usize;
    let result = flatten_array(&elements, depth);
    let arr = heap.alloc_array(result);
    Ok(JsValue::Object(arr))
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
