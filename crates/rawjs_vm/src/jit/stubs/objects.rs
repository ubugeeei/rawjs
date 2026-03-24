use super::{chunk_string_constant, finish, pop_value, set_error, vm_ref};
use crate::Vm;
use rawjs_common::RawJsError;
use rawjs_runtime::{JsObject, JsValue};

fn push_property_value(vm: &mut Vm, object: JsValue, key: String) -> Result<(), RawJsError> {
    let value = crate::interpreter::get_property_value(vm, &object, &key)?;
    vm.push(value);
    Ok(())
}

fn set_property_value(
    vm: &mut Vm,
    object: JsValue,
    key: String,
    value: JsValue,
) -> Result<(), RawJsError> {
    let is_strict = vm
        .call_stack
        .last()
        .map(|frame| frame.is_strict)
        .unwrap_or(false);
    crate::interpreter::set_property_value(vm, &object, &key, &value, is_strict)?;
    vm.push(value);
    Ok(())
}

#[no_mangle]
pub extern "C" fn stub_create_object(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let ptr = vm.heap.alloc(JsObject::ordinary());
    vm.push(JsValue::Object(ptr));
    0
}

#[no_mangle]
pub extern "C" fn stub_create_array(vm: *mut Vm, count: u32) -> u32 {
    let vm = vm_ref(vm);
    let count = count as usize;
    let stack_len = vm.value_stack.len();
    if count > stack_len {
        return set_error(
            vm as *mut Vm,
            RawJsError::internal_error("stack underflow in CreateArray"),
        );
    }

    let elements: Vec<JsValue> = vm.value_stack.drain(stack_len - count..).collect();
    let ptr = vm.heap.alloc(JsObject::array(elements));
    vm.push(JsValue::Object(ptr));
    0
}

#[no_mangle]
pub extern "C" fn stub_get_property(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = match chunk_string_constant(vm, idx) {
        Ok(name) => name,
        Err(err) => return set_error(vm as *mut Vm, err),
    };
    let result = pop_value(vm).and_then(|object| push_property_value(vm, object, name));
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_set_property(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = match chunk_string_constant(vm, idx) {
        Ok(name) => name,
        Err(err) => return set_error(vm as *mut Vm, err),
    };
    let result = pop_value(vm).and_then(|value| {
        pop_value(vm).and_then(|object| set_property_value(vm, object, name, value))
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_get_index(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).and_then(|index| {
        pop_value(vm)
            .and_then(|object| push_property_value(vm, object, index.to_js_string().to_string()))
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_set_index(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).and_then(|value| {
        pop_value(vm).and_then(|index| {
            pop_value(vm).and_then(|object| {
                set_property_value(vm, object, index.to_js_string().to_string(), value)
            })
        })
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_get_computed(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).and_then(|key| {
        pop_value(vm)
            .and_then(|object| push_property_value(vm, object, key.to_js_string().to_string()))
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_set_computed(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).and_then(|value| {
        pop_value(vm).and_then(|key| {
            pop_value(vm).and_then(|object| {
                set_property_value(vm, object, key.to_js_string().to_string(), value)
            })
        })
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_create_closure(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let result = crate::interpreter::exec_create_closure(vm, idx as u16);
    finish(vm, result)
}
