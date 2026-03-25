pub(super) fn iterator_next(vm: &mut Vm) -> Result<()> {
    let iter_val = vm.peek()?.clone();
    match &iter_val {
        JsValue::Object(ptr) => {
            if matches!(ptr.borrow().internal, ObjectInternal::Generator(_)) {
                let result = generator_next(vm, ptr, JsValue::Undefined)?;
                let (value, done) = if let JsValue::Object(p) = &result {
                    let obj = p.borrow();
                    (
                        obj.get_property("value"),
                        obj.get_property("done").to_boolean(),
                    )
                } else {
                    (JsValue::Undefined, true)
                };
                vm.push(value);
                vm.push(JsValue::Boolean(done));
                return Ok(());
            }
            let mut obj = ptr.borrow_mut();
            match &mut obj.internal {
                ObjectInternal::Iterator(state) => {
                    let (value, done) = state.advance();
                    drop(obj);
                    vm.push(value);
                    vm.push(JsValue::Boolean(done));
                    Ok(())
                }
                _ => Err(RawJsError::type_error("not an iterator")),
            }
        }
        _ => Err(RawJsError::type_error("not an iterator")),
    }
}

pub(super) fn iterator_done(vm: &mut Vm, exit_offset: i32) -> Result<()> {
    if vm.pop()?.to_boolean() {
        vm.pop()?;
        jump_relative(vm, exit_offset);
    }
    Ok(())
}

pub(super) fn init_for_in(vm: &mut Vm) -> Result<()> {
    let obj_val = vm.pop()?;
    let keys: Vec<JsValue> = match &obj_val {
        JsValue::Object(ptr) => ptr
            .borrow()
            .own_enumerable_keys()
            .into_iter()
            .map(|k| JsValue::string(k.as_str()))
            .collect(),
        _ => Vec::new(),
    };
    let keys_obj = vm.heap.alloc(JsObject::array(keys));
    vm.push(JsValue::Object(keys_obj));
    vm.push(JsValue::Number(0.0));
    Ok(())
}

pub(super) fn advance_for_in(vm: &mut Vm, exit_offset: i32) -> Result<()> {
    let index_val = vm.pop()?;
    let index = index_val.to_number() as usize;
    let keys_val = vm.peek()?.clone();
    let len = match &keys_val {
        JsValue::Object(ptr) => ptr.borrow().array_length(),
        _ => 0,
    };
    if index >= len {
        vm.push(index_val);
        jump_relative(vm, exit_offset);
        return Ok(());
    }
    let key = match &keys_val {
        JsValue::Object(ptr) => ptr.borrow().get_element(index),
        _ => JsValue::Undefined,
    };
    vm.push(JsValue::Number((index + 1) as f64));
    vm.push(key);
    Ok(())
}

pub(super) fn postfix_update(vm: &mut Vm, delta: f64) -> Result<()> {
    let value = vm.pop()?;
    let number = value.to_number();
    vm.push(JsValue::Number(number));
    vm.push(JsValue::Number(number + delta));
    Ok(())
}

pub(super) fn import_module_dynamic(vm: &mut Vm) -> Result<()> {
    let specifier = vm.pop()?;
    let promise_ptr = vm.create_promise_object();
    let source = specifier.to_string_value();
    match vm.execute_module(&source) {
        Ok(ns) => rawjs_runtime::builtins::resolve_promise_with_heap(
            &promise_ptr,
            JsValue::Object(ns),
            &mut vm.heap,
        ),
        Err(err) => rawjs_runtime::builtins::reject_promise_with_heap(
            &promise_ptr,
            JsValue::string(err.to_string()),
            &mut vm.heap,
        ),
    }
    vm.push(JsValue::Object(promise_ptr));
    Ok(())
}

pub(super) fn export_binding(vm: &mut Vm, name_idx: u16) -> Result<()> {
    let name = current_constant_string(vm, name_idx)?;
    let value = vm.pop()?;
    if let Some(ref exports) = vm.module_exports.clone() {
        exports.borrow_mut().set_property(name, value);
    }
    Ok(())
}

pub(super) fn export_default(vm: &mut Vm) -> Result<()> {
    let value = vm.pop()?;
    if let Some(ref exports) = vm.module_exports.clone() {
        exports
            .borrow_mut()
            .set_property("default".to_string(), value);
    }
    Ok(())
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
