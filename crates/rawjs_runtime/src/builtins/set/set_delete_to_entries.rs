pub(super) fn set_delete(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let value = args.first().unwrap_or(&JsValue::Undefined);
    match this {
        JsValue::Object(ptr) => {
            let removed = {
                let mut obj = ptr.borrow_mut();
                match &mut obj.internal {
                    ObjectInternal::Set(values) => {
                        if let Some(idx) = set_contains(values, value) {
                            values.remove(idx);
                            true
                        } else {
                            false
                        }
                    }
                    _ => {
                        return Err(RawJsError::type_error(
                            "Set.prototype.delete called on non-Set",
                        ));
                    }
                }
            };
            if removed {
                update_set_size(ptr);
            }
            Ok(JsValue::Boolean(removed))
        }
        _ => Err(RawJsError::type_error(
            "Set.prototype.delete called on non-Set",
        )),
    }
}

pub(super) fn set_clear(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Object(ptr) => {
            {
                let mut obj = ptr.borrow_mut();
                match &mut obj.internal {
                    ObjectInternal::Set(values) => values.clear(),
                    _ => {
                        return Err(RawJsError::type_error(
                            "Set.prototype.clear called on non-Set",
                        ));
                    }
                }
            }
            update_set_size(ptr);
            Ok(JsValue::Undefined)
        }
        _ => Err(RawJsError::type_error(
            "Set.prototype.clear called on non-Set",
        )),
    }
}

pub(super) fn set_for_each(
    _heap: &mut Heap,
    _this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    Ok(JsValue::Undefined)
}

pub(super) fn set_values(heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Set(values) => {
                    let arr = heap.alloc(JsObject::array(values.clone()));
                    Ok(JsValue::Object(arr))
                }
                _ => Err(RawJsError::type_error(
                    "Set.prototype.values called on non-Set",
                )),
            }
        }
        _ => Err(RawJsError::type_error(
            "Set.prototype.values called on non-Set",
        )),
    }
}

pub(super) fn set_entries(heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Set(values) => {
                    let pairs: Vec<JsValue> = values
                        .iter()
                        .map(|v| {
                            let pair = vec![v.clone(), v.clone()];
                            JsValue::Object(GcPtr::new(JsObject::array(pair)))
                        })
                        .collect();
                    let arr = heap.alloc(JsObject::array(pairs));
                    Ok(JsValue::Object(arr))
                }
                _ => Err(RawJsError::type_error(
                    "Set.prototype.entries called on non-Set",
                )),
            }
        }
        _ => Err(RawJsError::type_error(
            "Set.prototype.entries called on non-Set",
        )),
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
