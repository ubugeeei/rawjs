use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};
use crate::object::{JsObject, ObjectInternal, Property};
use crate::value::JsValue;

use super::helpers::{get_this_array_elements, set_native};

pub fn create_array_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();

    set_native(&mut obj, "push", array_push);
    set_native(&mut obj, "pop", array_pop);
    set_native(&mut obj, "shift", array_shift);
    set_native(&mut obj, "unshift", array_unshift);
    set_native(&mut obj, "indexOf", array_index_of);
    set_native(&mut obj, "includes", array_includes);
    set_native(&mut obj, "join", array_join);
    set_native(&mut obj, "slice", array_slice);
    set_native(&mut obj, "splice", array_splice);
    set_native(&mut obj, "concat", array_concat);
    set_native(&mut obj, "reverse", array_reverse);
    set_native(&mut obj, "forEach", array_for_each);
    set_native(&mut obj, "map", array_map);
    set_native(&mut obj, "filter", array_filter);
    set_native(&mut obj, "reduce", array_reduce);
    set_native(&mut obj, "find", array_find);
    set_native(&mut obj, "findIndex", array_find_index);
    set_native(&mut obj, "every", array_every);
    set_native(&mut obj, "some", array_some);
    set_native(&mut obj, "flat", array_flat);
    set_native(&mut obj, "fill", array_fill);
    set_native(&mut obj, "toString", array_to_string);
    set_native(&mut obj, "isArray", array_is_array);

    heap.alloc(obj)
}

fn array_push(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let ptr = match this {
        JsValue::Object(p) => p.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Array.prototype.push called on non-object",
            ))
        }
    };
    let mut obj = ptr.borrow_mut();
    if let ObjectInternal::Array(ref mut elements) = obj.internal {
        for arg in args {
            elements.push(arg.clone());
        }
        let len = elements.len() as f64;
        obj.properties
            .insert("length".to_string(), Property::data(JsValue::Number(len)));
        Ok(JsValue::Number(len))
    } else {
        Err(RawJsError::type_error("not an array"))
    }
}

fn array_pop(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let ptr = match this {
        JsValue::Object(p) => p.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Array.prototype.pop called on non-object",
            ))
        }
    };
    let mut obj = ptr.borrow_mut();
    if let ObjectInternal::Array(ref mut elements) = obj.internal {
        let val = elements.pop().unwrap_or(JsValue::Undefined);
        let len = elements.len() as f64;
        obj.properties
            .insert("length".to_string(), Property::data(JsValue::Number(len)));
        Ok(val)
    } else {
        Err(RawJsError::type_error("not an array"))
    }
}

fn array_shift(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let ptr = match this {
        JsValue::Object(p) => p.clone(),
        _ => return Err(RawJsError::type_error("not an array")),
    };
    let mut obj = ptr.borrow_mut();
    if let ObjectInternal::Array(ref mut elements) = obj.internal {
        if elements.is_empty() {
            return Ok(JsValue::Undefined);
        }
        let val = elements.remove(0);
        let len = elements.len() as f64;
        obj.properties
            .insert("length".to_string(), Property::data(JsValue::Number(len)));
        Ok(val)
    } else {
        Err(RawJsError::type_error("not an array"))
    }
}

fn array_unshift(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn array_index_of(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn array_includes(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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
        // includes uses SameValueZero (NaN === NaN).
        if elem.strict_eq(search) || (elem.is_nan() && search.is_nan()) {
            return Ok(JsValue::Boolean(true));
        }
    }
    Ok(JsValue::Boolean(false))
}

fn array_join(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn array_slice(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn array_splice(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn array_concat(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn array_reverse(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
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

fn array_for_each(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::Undefined)
}

fn array_map(heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let arr = heap.alloc_array(Vec::new());
    Ok(JsValue::Object(arr))
}

fn array_filter(heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let arr = heap.alloc_array(Vec::new());
    Ok(JsValue::Object(arr))
}

fn array_reduce(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    Ok(args.get(1).cloned().unwrap_or(JsValue::Undefined))
}

fn array_find(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::Undefined)
}

fn array_find_index(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::Number(-1.0))
}

fn array_every(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::Boolean(true))
}

fn array_some(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::Boolean(false))
}

fn array_flat(heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn flatten_array(elements: &[JsValue], depth: usize) -> Vec<JsValue> {
    let mut result = Vec::new();
    for elem in elements {
        if depth > 0 {
            if let JsValue::Object(ptr) = elem {
                let obj = ptr.borrow();
                if let ObjectInternal::Array(ref inner) = obj.internal {
                    result.extend(flatten_array(inner, depth - 1));
                    continue;
                }
            }
        }
        result.push(elem.clone());
    }
    result
}

fn array_fill(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let ptr = match this {
        JsValue::Object(p) => p.clone(),
        _ => return Err(RawJsError::type_error("not an array")),
    };
    let mut obj = ptr.borrow_mut();
    if let ObjectInternal::Array(ref mut elements) = obj.internal {
        let fill_value = args.first().unwrap_or(&JsValue::Undefined).clone();
        let len = elements.len() as i64;
        let start_raw = args.get(1).map(|v| v.to_number() as i64).unwrap_or(0);
        let end_raw = args.get(2).map(|v| v.to_number() as i64).unwrap_or(len);
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
        for elem in elements.iter_mut().take(end).skip(start) {
            *elem = fill_value.clone();
        }
        drop(obj);
        Ok(this.clone())
    } else {
        Err(RawJsError::type_error("not an array"))
    }
}

fn array_to_string(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    let elements = get_this_array_elements(this)?;
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
    Ok(JsValue::string(parts.join(",")))
}

fn array_is_array(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let val = args.first().unwrap_or(&JsValue::Undefined);
    Ok(JsValue::Boolean(val.is_array()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_array_push_pop() {
        let mut heap = Heap::new();
        let arr_ptr = heap.alloc(JsObject::array(vec![JsValue::Number(1.0)]));
        let arr = JsValue::Object(arr_ptr.clone());

        let len = array_push(&mut heap, &arr, &[JsValue::Number(2.0)]).unwrap();
        assert_eq!(len, JsValue::Number(2.0));

        let popped = array_pop(&mut heap, &arr, &[]).unwrap();
        assert_eq!(popped, JsValue::Number(2.0));

        let popped2 = array_pop(&mut heap, &arr, &[]).unwrap();
        assert_eq!(popped2, JsValue::Number(1.0));

        let popped3 = array_pop(&mut heap, &arr, &[]).unwrap();
        assert_eq!(popped3, JsValue::Undefined);
    }

    #[test]
    fn test_array_index_of_builtin() {
        let mut heap = Heap::new();
        let arr_ptr = heap.alloc(JsObject::array(vec![
            JsValue::Number(10.0),
            JsValue::Number(20.0),
            JsValue::Number(30.0),
        ]));
        let arr = JsValue::Object(arr_ptr);

        assert_eq!(
            array_index_of(&mut heap, &arr, &[JsValue::Number(20.0)]).unwrap(),
            JsValue::Number(1.0)
        );
        assert_eq!(
            array_index_of(&mut heap, &arr, &[JsValue::Number(99.0)]).unwrap(),
            JsValue::Number(-1.0)
        );
    }

    #[test]
    fn test_array_join_builtin() {
        let mut heap = Heap::new();
        let arr_ptr = heap.alloc(JsObject::array(vec![
            JsValue::string("a"),
            JsValue::string("b"),
            JsValue::string("c"),
        ]));
        let arr = JsValue::Object(arr_ptr);

        assert_eq!(
            array_join(&mut heap, &arr, &[JsValue::string("-")]).unwrap(),
            JsValue::string("a-b-c")
        );
        assert_eq!(
            array_join(&mut heap, &arr, &[]).unwrap(),
            JsValue::string("a,b,c")
        );
    }

    #[test]
    fn test_array_slice_builtin() {
        let mut heap = Heap::new();
        let arr_ptr = heap.alloc(JsObject::array(vec![
            JsValue::Number(1.0),
            JsValue::Number(2.0),
            JsValue::Number(3.0),
            JsValue::Number(4.0),
        ]));
        let arr = JsValue::Object(arr_ptr);

        let result = array_slice(
            &mut heap,
            &arr,
            &[JsValue::Number(1.0), JsValue::Number(3.0)],
        )
        .unwrap();
        if let JsValue::Object(ptr) = result {
            let obj = ptr.borrow();
            assert_eq!(obj.array_length(), 2);
            assert_eq!(obj.get_property("0"), JsValue::Number(2.0));
            assert_eq!(obj.get_property("1"), JsValue::Number(3.0));
        } else {
            panic!("Expected array");
        }
    }

    #[test]
    fn test_array_concat_builtin() {
        let mut heap = Heap::new();
        let arr1 = heap.alloc(JsObject::array(vec![JsValue::Number(1.0)]));
        let arr2 = heap.alloc(JsObject::array(vec![JsValue::Number(2.0)]));
        let this = JsValue::Object(arr1);

        let result = array_concat(&mut heap, &this, &[JsValue::Object(arr2)]).unwrap();
        if let JsValue::Object(ptr) = result {
            let obj = ptr.borrow();
            assert_eq!(obj.array_length(), 2);
        } else {
            panic!("Expected array");
        }
    }

    #[test]
    fn test_array_shift_unshift() {
        let mut heap = Heap::new();
        let arr_ptr = heap.alloc(JsObject::array(vec![
            JsValue::Number(1.0),
            JsValue::Number(2.0),
            JsValue::Number(3.0),
        ]));
        let arr = JsValue::Object(arr_ptr.clone());

        let shifted = array_shift(&mut heap, &arr, &[]).unwrap();
        assert_eq!(shifted, JsValue::Number(1.0));
        assert_eq!(arr_ptr.borrow().array_length(), 2);

        let new_len = array_unshift(&mut heap, &arr, &[JsValue::Number(0.0)]).unwrap();
        assert_eq!(new_len, JsValue::Number(3.0));
        assert_eq!(arr_ptr.borrow().get_index(0), JsValue::Number(0.0));
    }

    #[test]
    fn test_array_includes_builtin() {
        let mut heap = Heap::new();
        let arr_ptr = heap.alloc(JsObject::array(vec![
            JsValue::Number(1.0),
            JsValue::nan(),
            JsValue::Number(3.0),
        ]));
        let arr = JsValue::Object(arr_ptr);

        assert_eq!(
            array_includes(&mut heap, &arr, &[JsValue::Number(1.0)]).unwrap(),
            JsValue::Boolean(true)
        );
        assert_eq!(
            array_includes(&mut heap, &arr, &[JsValue::nan()]).unwrap(),
            JsValue::Boolean(true)
        );
        assert_eq!(
            array_includes(&mut heap, &arr, &[JsValue::Number(99.0)]).unwrap(),
            JsValue::Boolean(false)
        );
    }

    #[test]
    fn test_array_reverse_builtin() {
        let mut heap = Heap::new();
        let arr_ptr = heap.alloc(JsObject::array(vec![
            JsValue::Number(1.0),
            JsValue::Number(2.0),
            JsValue::Number(3.0),
        ]));
        let arr = JsValue::Object(arr_ptr.clone());

        array_reverse(&mut heap, &arr, &[]).unwrap();
        let obj = arr_ptr.borrow();
        assert_eq!(obj.get_index(0), JsValue::Number(3.0));
        assert_eq!(obj.get_index(1), JsValue::Number(2.0));
        assert_eq!(obj.get_index(2), JsValue::Number(1.0));
    }

    #[test]
    fn test_array_fill_builtin() {
        let mut heap = Heap::new();
        let arr_ptr = heap.alloc(JsObject::array(vec![
            JsValue::Number(1.0),
            JsValue::Number(2.0),
            JsValue::Number(3.0),
        ]));
        let arr = JsValue::Object(arr_ptr.clone());

        array_fill(
            &mut heap,
            &arr,
            &[
                JsValue::Number(0.0),
                JsValue::Number(1.0),
                JsValue::Number(3.0),
            ],
        )
        .unwrap();
        let obj = arr_ptr.borrow();
        assert_eq!(obj.get_index(0), JsValue::Number(1.0));
        assert_eq!(obj.get_index(1), JsValue::Number(0.0));
        assert_eq!(obj.get_index(2), JsValue::Number(0.0));
    }
}
