fn jump_relative(vm: &mut Vm, offset: i32) {
    let frame = vm.call_stack.last_mut().unwrap();
    frame.ip = ((frame.ip as i64) + (offset as i64)) as usize;
}

fn jump_if(vm: &mut Vm, offset: i32, expected: bool) -> Result<()> {
    let value = vm.pop()?;
    if value.to_boolean() == expected {
        jump_relative(vm, offset);
    }
    Ok(())
}

fn jump_if_nullish(vm: &mut Vm, offset: i32) -> Result<()> {
    if vm.pop()?.is_nullish() {
        jump_relative(vm, offset);
    }
    Ok(())
}

fn execute_call_method(vm: &mut Vm, argc: usize) -> Result<()> {
    let stack_len = vm.value_stack.len();
    if stack_len < argc + 2 {
        return Err(RawJsError::internal_error("stack underflow in CallMethod"));
    }
    let args: Vec<JsValue> = vm.value_stack.drain(stack_len - argc..).collect();
    let method = vm.pop()?;
    let receiver = vm.pop()?;
    if let Some(result) = try_generator_method_call(vm, &receiver, &method, &args)? {
        vm.push(result);
        return Ok(());
    }
    vm.push(method);
    for arg in args {
        vm.push(arg);
    }
    exec_call(vm, argc, receiver)
}

fn create_ordinary_object(vm: &mut Vm) -> GcPtr<JsObject> {
    let ptr = vm.heap.alloc(JsObject::ordinary());
    if let Some(ref proto) = vm.object_prototype {
        ptr.borrow_mut().set_prototype(Some(proto.clone()));
    }
    ptr
}

fn create_array_object(vm: &mut Vm, count: u16) -> Result<GcPtr<JsObject>> {
    let count = count as usize;
    let stack_len = vm.value_stack.len();
    if count > stack_len {
        return Err(RawJsError::internal_error("stack underflow in CreateArray"));
    }
    let elements: Vec<JsValue> = vm.value_stack.drain(stack_len - count..).collect();
    let ptr = vm.heap.alloc(JsObject::array(elements));
    if let Some(ref proto) = vm.array_prototype {
        ptr.borrow_mut().set_prototype(Some(proto.clone()));
    }
    Ok(ptr)
}

fn mirror_global_property(vm: &mut Vm, obj_val: &JsValue, key: &str, value: &JsValue) {
    if let (JsValue::Object(ptr), Some(global_obj)) = (obj_val, &vm.global_object) {
        if ptr.ptr_eq(global_obj) {
            vm.globals.insert(key.to_string(), value.clone());
        }
    }
}

fn get_computed_value(vm: &mut Vm, obj_val: JsValue, key: JsValue) -> Result<JsValue> {
    if let JsValue::Symbol(sym) = key {
        return Ok(match &obj_val {
            JsValue::Object(ptr) => ptr.borrow().get_symbol_property(sym.id),
            _ => JsValue::Undefined,
        });
    }
    let key_str = key.to_js_string();
    get_property_value(vm, &obj_val, &key_str)
}

fn set_computed_value(vm: &mut Vm, obj_val: JsValue, key: JsValue, value: JsValue) -> Result<()> {
    if let JsValue::Symbol(sym) = key {
        if let JsValue::Object(ref ptr) = obj_val {
            ptr.borrow_mut().set_symbol_property(sym.id, value.clone());
        }
        vm.push(value);
        return Ok(());
    }
    let key_str = key.to_js_string();
    let is_strict = current_frame_is_strict(vm);
    set_property_value(vm, &obj_val, &key_str, &value, is_strict)?;
    mirror_global_property(vm, &obj_val, &key_str, &value);
    vm.push(value);
    Ok(())
}
