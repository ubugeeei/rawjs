pub(super) fn load_arguments_object(vm: &mut Vm) -> Result<()> {
    if let Some(arguments_obj) = vm
        .call_stack
        .last()
        .and_then(|frame| frame.arguments_object.clone())
    {
        vm.push(JsValue::Object(arguments_obj));
        return Ok(());
    }
    let frame = vm.call_stack.last().unwrap();
    let args = frame.arguments.clone();
    let locals = frame.locals.clone();
    let is_strict = frame.is_strict;
    let callee = frame.callee.clone();
    let param_count = vm.chunks[frame.chunk_index].param_count as usize;
    let mut obj = JsObject::ordinary();
    if let Some(proto) = &vm.object_prototype {
        obj.set_prototype(Some(proto.clone()));
    }
    obj.internal = ObjectInternal::ArgumentsObject(
        (0..args.len())
            .map(|index| (!is_strict && index < param_count).then_some(index as u16))
            .collect(),
    );
    obj.define_property(
        "length".to_string(),
        rawjs_runtime::Property::builtin(JsValue::Number(args.len() as f64)),
    );
    for (index, arg) in args.into_iter().enumerate() {
        let value = if !is_strict && index < locals.len() && index < param_count {
            locals[index].clone()
        } else {
            arg
        };
        obj.set_property(index.to_string(), value);
    }
    if is_strict {
        let thrower = create_strict_arguments_thrower(vm);
        obj.define_property(
            "callee".to_string(),
            rawjs_runtime::Property::accessor(
                Some(JsValue::Object(thrower.clone())),
                Some(JsValue::Object(thrower)),
                false,
                false,
            ),
        );
    } else if let Some(callee) = callee {
        obj.define_property(
            "callee".to_string(),
            rawjs_runtime::Property::builtin(JsValue::Object(callee)),
        );
    }
    let arguments_obj = vm.heap.alloc(obj);
    vm.call_stack.last_mut().unwrap().arguments_object = Some(arguments_obj.clone());
    vm.push(JsValue::Object(arguments_obj));
    Ok(())
}

pub(super) fn offset_to_ip(current_ip: usize, offset: i32) -> Option<usize> {
    (offset != 0).then_some(((current_ip as i64) + (offset as i64)) as usize)
}

pub(super) fn create_iterator_value(vm: &mut Vm, obj_val: JsValue) -> Result<JsValue> {
    if let JsValue::Object(ref ptr) = obj_val {
        if matches!(ptr.borrow().internal, ObjectInternal::Generator(_)) {
            return Ok(obj_val);
        }
    }
    let values: Vec<JsValue> = match &obj_val {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                ObjectInternal::Array(elements) => elements.clone(),
                ObjectInternal::Set(set_values) => set_values.clone(),
                ObjectInternal::Map(entries) => entries
                    .iter()
                    .map(|(k, v)| {
                        JsValue::Object(GcPtr::new(JsObject::array(vec![k.clone(), v.clone()])))
                    })
                    .collect(),
                _ => {
                    return Err(RawJsError::type_error(format!(
                        "{} is not iterable",
                        obj_val.type_of()
                    )));
                }
            }
        }
        JsValue::String(s) => s
            .chars()
            .map(|c| JsValue::string(c.to_string().as_str()))
            .collect(),
        _ => {
            return Err(RawJsError::type_error(format!(
                "{} is not iterable",
                obj_val.type_of()
            )));
        }
    };
    Ok(JsValue::Object(vm.heap.alloc(JsObject::iterator(values))))
}

use super::*;
