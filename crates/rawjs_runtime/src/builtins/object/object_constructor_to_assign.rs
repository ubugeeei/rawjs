pub(super) fn object_constructor(
    heap: &mut Heap,
    this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    match args.first() {
        Some(JsValue::Object(ptr)) => Ok(JsValue::Object(ptr.clone())),
        Some(JsValue::Null) | Some(JsValue::Undefined) | None => {
            let ptr = heap.alloc_object(JsObject::ordinary());
            if let JsValue::Object(this_ptr) = this {
                ptr.borrow_mut()
                    .set_prototype(this_ptr.borrow().prototype.clone());
            }
            Ok(JsValue::Object(ptr))
        }
        Some(value) => Ok(value.clone()),
    }
}

pub(super) fn object_keys(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => return Err(RawJsError::type_error("Object.keys called on non-object")),
    };
    let keys: Vec<JsValue> = target
        .borrow()
        .own_enumerable_keys()
        .into_iter()
        .map(|k| JsValue::string(k.as_str()))
        .collect();
    let arr = heap.alloc_array(keys);
    Ok(JsValue::Object(arr))
}

pub(super) fn object_values(heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => return Err(RawJsError::type_error("Object.values called on non-object")),
    };
    let obj = target.borrow();
    let values: Vec<JsValue> = obj
        .own_enumerable_keys()
        .into_iter()
        .map(|k| obj.get_property(&k))
        .collect();
    drop(obj);
    let arr = heap.alloc_array(values);
    Ok(JsValue::Object(arr))
}

pub(super) fn object_entries(
    heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => {
            return Err(RawJsError::type_error(
                "Object.entries called on non-object",
            ))
        }
    };
    let obj = target.borrow();
    let entries: Vec<JsValue> = obj
        .own_enumerable_keys()
        .into_iter()
        .map(|k| {
            let val = obj.get_property(&k);
            let pair = vec![JsValue::string(k.as_str()), val];
            JsValue::Object(GcPtr::new(JsObject::array(pair)))
        })
        .collect();
    drop(obj);
    let arr = heap.alloc_array(entries);
    Ok(JsValue::Object(arr))
}

pub(super) fn object_assign(
    _heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let target = match args.first() {
        Some(JsValue::Object(ptr)) => ptr.clone(),
        _ => return Err(RawJsError::type_error("Object.assign called on non-object")),
    };
    for source_val in &args[1..] {
        if let JsValue::Object(source_ptr) = source_val {
            let source = source_ptr.borrow();
            let keys = source.own_enumerable_keys();
            let pairs: Vec<(String, JsValue)> = keys
                .iter()
                .map(|k| (k.clone(), source.get_property(k)))
                .collect();
            drop(source);
            let mut tgt = target.borrow_mut();
            for (key, val) in pairs {
                tgt.set_property(key, val);
            }
        }
    }
    Ok(JsValue::Object(target))
}

use super::*;
