use super::{chunk_string_constant, pop_value, set_error, vm_ref};
use crate::Vm;
use rawjs_runtime::JsValue;

#[no_mangle]
pub extern "C" fn stub_load_const(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let frame = vm.call_stack.last().unwrap();
    let chunk = &vm.chunks[frame.chunk_index];
    let value = crate::interpreter::constant_to_value(&chunk.constants[idx as usize]);
    vm.push(value);
    0
}

#[no_mangle]
pub extern "C" fn stub_load_local(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let value = {
        let frame = vm.call_stack.last().unwrap();
        frame.locals[idx as usize].clone()
    };
    vm.push(value);
    0
}

#[no_mangle]
pub extern "C" fn stub_store_local(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    match pop_value(vm) {
        Ok(value) => {
            let frame = vm.call_stack.last_mut().unwrap();
            let slot = idx as usize;
            if slot >= frame.locals.len() {
                frame.locals.resize(slot + 1, JsValue::Undefined);
            }
            frame.locals[slot] = value;
            0
        }
        Err(err) => set_error(vm as *mut Vm, err),
    }
}

#[no_mangle]
pub extern "C" fn stub_load_global(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = match chunk_string_constant(vm, idx) {
        Ok(name) => name,
        Err(err) => return set_error(vm as *mut Vm, err),
    };
    let value = vm.get_global(&name).cloned().unwrap_or(JsValue::Undefined);
    vm.push(value);
    0
}

#[no_mangle]
pub extern "C" fn stub_store_global(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = match chunk_string_constant(vm, idx) {
        Ok(name) => name,
        Err(err) => return set_error(vm as *mut Vm, err),
    };
    match pop_value(vm) {
        Ok(value) => {
            vm.set_global(name, value);
            0
        }
        Err(err) => set_error(vm as *mut Vm, err),
    }
}

#[no_mangle]
pub extern "C" fn stub_load_upvalue(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let value = {
        let frame = vm.call_stack.last().unwrap();
        frame.upvalues[idx as usize].get()
    };
    vm.push(value);
    0
}

#[no_mangle]
pub extern "C" fn stub_store_upvalue(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    match pop_value(vm) {
        Ok(value) => {
            let frame = vm.call_stack.last().unwrap();
            frame.upvalues[idx as usize].set(value);
            0
        }
        Err(err) => set_error(vm as *mut Vm, err),
    }
}

#[no_mangle]
pub extern "C" fn stub_pop(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match pop_value(vm) {
        Ok(_) => 0,
        Err(err) => set_error(vm as *mut Vm, err),
    }
}

#[no_mangle]
pub extern "C" fn stub_dup(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match vm.peek() {
        Ok(value) => {
            vm.push(value.clone());
            0
        }
        Err(err) => set_error(vm as *mut Vm, err),
    }
}

#[no_mangle]
pub extern "C" fn stub_push_undefined(vm: *mut Vm) -> u32 {
    vm_ref(vm).push(JsValue::Undefined);
    0
}

#[no_mangle]
pub extern "C" fn stub_push_null(vm: *mut Vm) -> u32 {
    vm_ref(vm).push(JsValue::Null);
    0
}

#[no_mangle]
pub extern "C" fn stub_push_true(vm: *mut Vm) -> u32 {
    vm_ref(vm).push(JsValue::Boolean(true));
    0
}

#[no_mangle]
pub extern "C" fn stub_push_false(vm: *mut Vm) -> u32 {
    vm_ref(vm).push(JsValue::Boolean(false));
    0
}

#[no_mangle]
pub extern "C" fn stub_push_this(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let value = vm.call_stack.last().unwrap().this_value.clone();
    vm.push(value);
    0
}
