fn map_set(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let key = args.first().cloned().unwrap_or(JsValue::Undefined);
    let value = args.get(1).cloned().unwrap_or(JsValue::Undefined);
    match this {
        JsValue::Object(ptr) => {
            {
                let mut obj = ptr.borrow_mut();
                match &mut obj.internal {
                    ObjectInternal::Map(entries) => {
                        if let Some(idx) = find_map_key(entries, &key) {
                            entries[idx].1 = value;
                        } else {
                            entries.push((key, value));
                        }
                    }
                    _ => {
                        return Err(RawJsError::type_error(
                            "Map.prototype.set called on non-Map",
                        ));
                    }
                }
            }
            update_map_size(ptr);
            Ok(this.clone())
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.set called on non-Map",
        )),
    }
}

fn map_has(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let key = args.first().unwrap_or(&JsValue::Undefined);
    match this {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Map(entries) => {
                    Ok(JsValue::Boolean(find_map_key(entries, key).is_some()))
                }
                _ => Err(RawJsError::type_error(
                    "Map.prototype.has called on non-Map",
                )),
            }
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.has called on non-Map",
        )),
    }
}

fn map_delete(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let key = args.first().unwrap_or(&JsValue::Undefined);
    match this {
        JsValue::Object(ptr) => {
            let removed = {
                let mut obj = ptr.borrow_mut();
                match &mut obj.internal {
                    ObjectInternal::Map(entries) => {
                        if let Some(idx) = find_map_key(entries, key) {
                            entries.remove(idx);
                            true
                        } else {
                            false
                        }
                    }
                    _ => {
                        return Err(RawJsError::type_error(
                            "Map.prototype.delete called on non-Map",
                        ));
                    }
                }
            };
            if removed {
                update_map_size(ptr);
            }
            Ok(JsValue::Boolean(removed))
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.delete called on non-Map",
        )),
    }
}

fn map_clear(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Object(ptr) => {
            {
                let mut obj = ptr.borrow_mut();
                match &mut obj.internal {
                    ObjectInternal::Map(entries) => entries.clear(),
                    _ => {
                        return Err(RawJsError::type_error(
                            "Map.prototype.clear called on non-Map",
                        ));
                    }
                }
            }
            update_map_size(ptr);
            Ok(JsValue::Undefined)
        }
        _ => Err(RawJsError::type_error(
            "Map.prototype.clear called on non-Map",
        )),
    }
}

fn map_for_each(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::Undefined)
}
