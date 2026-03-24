use super::{finish, pop_value, set_error, vm_ref};
use crate::Vm;
use rawjs_common::RawJsError;
use rawjs_runtime::{GcPtr, JsObject, JsValue, ObjectInternal};

fn iterator_values(value: &JsValue) -> Result<Vec<JsValue>, RawJsError> {
    match value {
        JsValue::Object(ptr) => {
            let borrowed = ptr.borrow();
            match &borrowed.internal {
                ObjectInternal::Array(elements) => Ok(elements.clone()),
                ObjectInternal::Set(values) => Ok(values.clone()),
                ObjectInternal::Map(entries) => Ok(entries
                    .iter()
                    .map(|(key, value)| {
                        let pair = vec![key.clone(), value.clone()];
                        JsValue::Object(GcPtr::new(JsObject::array(pair)))
                    })
                    .collect()),
                _ => Err(RawJsError::type_error(format!(
                    "{} is not iterable",
                    value.type_of()
                ))),
            }
        }
        JsValue::String(value) => Ok(value
            .chars()
            .map(|ch| JsValue::string(ch.to_string().as_str()))
            .collect()),
        _ => Err(RawJsError::type_error(format!(
            "{} is not iterable",
            value.type_of()
        ))),
    }
}

#[no_mangle]
pub extern "C" fn stub_get_iterator(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).and_then(|object| {
        let iterator = JsObject::iterator(iterator_values(&object)?);
        let ptr = vm.heap.alloc(iterator);
        vm.push(JsValue::Object(ptr));
        Ok(())
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_iterator_next(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let iterator = match vm.peek() {
        Ok(value) => value.clone(),
        Err(err) => return set_error(vm as *mut Vm, err),
    };

    match iterator {
        JsValue::Object(ptr) => {
            let mut object = ptr.borrow_mut();
            match &mut object.internal {
                ObjectInternal::Iterator(state) => {
                    let (value, done) = state.advance();
                    drop(object);
                    vm.push(value);
                    vm.push(JsValue::Boolean(done));
                    0
                }
                _ => set_error(vm as *mut Vm, RawJsError::type_error("not an iterator")),
            }
        }
        _ => set_error(vm as *mut Vm, RawJsError::type_error("not an iterator")),
    }
}

#[no_mangle]
pub extern "C" fn stub_iterator_done_check(vm: *mut Vm, offset: i32) -> u32 {
    let vm = vm_ref(vm);
    let done = match pop_value(vm) {
        Ok(done) => done,
        Err(_) => return 0x80000000,
    };

    if done.to_boolean() {
        let _ = pop_value(vm);
        let frame = vm.call_stack.last_mut().unwrap();
        frame.ip = ((frame.ip as i64) + offset as i64) as usize;
        1
    } else {
        0
    }
}

#[no_mangle]
pub extern "C" fn stub_for_in_init(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).map(|object| {
        let keys = match object {
            JsValue::Object(ptr) => ptr
                .borrow()
                .own_enumerable_keys()
                .into_iter()
                .map(|key| JsValue::string(key.as_str()))
                .collect(),
            _ => Vec::new(),
        };
        let ptr = vm.heap.alloc(JsObject::array(keys));
        vm.push(JsValue::Object(ptr));
        vm.push(JsValue::Number(0.0));
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_for_in_next(vm: *mut Vm, offset: i32) -> u32 {
    let vm = vm_ref(vm);
    let index_value = match pop_value(vm) {
        Ok(index) => index,
        Err(_) => return 0x80000000,
    };
    let index = index_value.to_number() as usize;
    let keys = match vm.peek() {
        Ok(value) => value.clone(),
        Err(_) => return 0x80000000,
    };

    let length = match &keys {
        JsValue::Object(ptr) => ptr.borrow().array_length(),
        _ => 0,
    };
    if index >= length {
        vm.push(index_value);
        let frame = vm.call_stack.last_mut().unwrap();
        frame.ip = ((frame.ip as i64) + offset as i64) as usize;
        return 1;
    }

    let key = match &keys {
        JsValue::Object(ptr) => ptr.borrow().get_element(index),
        _ => JsValue::Undefined,
    };
    vm.push(JsValue::Number((index + 1) as f64));
    vm.push(key);
    0
}
