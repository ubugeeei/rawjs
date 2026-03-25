pub(super) fn flatten_array(elements: &[JsValue], depth: usize) -> Vec<JsValue> {
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

pub(super) fn array_fill(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

pub(super) fn array_to_string(
    _heap: &mut Heap,
    this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
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

pub fn array_is_array(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let val = args.first().unwrap_or(&JsValue::Undefined);
    Ok(JsValue::Boolean(val.is_array()))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    pub(super) fn test_array_push_pop() {
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
    pub(super) fn test_array_index_of_builtin() {
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
    pub(super) fn test_array_join_builtin() {
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
    pub(super) fn test_array_slice_builtin() {
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
    pub(super) fn test_array_concat_builtin() {
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
    pub(super) fn test_array_shift_unshift() {
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
    pub(super) fn test_array_includes_builtin() {
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
    pub(super) fn test_array_reverse_builtin() {
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
    pub(super) fn test_array_fill_builtin() {
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

use super::*;
