use super::{finish, pop_pair, vm_ref};
use crate::Vm;
use rawjs_common::RawJsError;
use rawjs_runtime::JsValue;

fn binary_boolean(
    vm: &mut Vm,
    op: impl FnOnce(&JsValue, &JsValue) -> bool,
) -> Result<(), RawJsError> {
    let (lhs, rhs) = pop_pair(vm)?;
    vm.push(JsValue::Boolean(op(&lhs, &rhs)));
    Ok(())
}

fn binary_int32(vm: &mut Vm, op: impl FnOnce(i32, i32) -> i32) -> Result<(), RawJsError> {
    let (lhs, rhs) = pop_pair(vm)?;
    vm.push(JsValue::Number(op(lhs.to_int32(), rhs.to_int32()) as f64));
    Ok(())
}

#[no_mangle]
pub extern "C" fn stub_eq(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_boolean(vm, |lhs, rhs| lhs.abstract_eq(rhs));
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_strict_eq(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_boolean(vm, |lhs, rhs| lhs.strict_eq(rhs));
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_ne(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_boolean(vm, |lhs, rhs| !lhs.abstract_eq(rhs));
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_strict_ne(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_boolean(vm, |lhs, rhs| !lhs.strict_eq(rhs));
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_lt(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_boolean(vm, |lhs, rhs| lhs.to_number() < rhs.to_number());
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_le(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_boolean(vm, |lhs, rhs| lhs.to_number() <= rhs.to_number());
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_gt(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_boolean(vm, |lhs, rhs| lhs.to_number() > rhs.to_number());
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_ge(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_boolean(vm, |lhs, rhs| lhs.to_number() >= rhs.to_number());
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_bit_and(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_int32(vm, |lhs, rhs| lhs & rhs);
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_bit_or(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_int32(vm, |lhs, rhs| lhs | rhs);
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_bit_xor(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_int32(vm, |lhs, rhs| lhs ^ rhs);
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_shl(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_pair(vm).map(|(lhs, rhs)| {
        let shift = rhs.to_uint32() & 0x1f;
        vm.push(JsValue::Number((lhs.to_int32() << shift) as f64));
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_shr(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_pair(vm).map(|(lhs, rhs)| {
        let shift = rhs.to_uint32() & 0x1f;
        vm.push(JsValue::Number((lhs.to_int32() >> shift) as f64));
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_ushr(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_pair(vm).map(|(lhs, rhs)| {
        let shift = rhs.to_uint32() & 0x1f;
        vm.push(JsValue::Number((lhs.to_uint32() >> shift) as f64));
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_in(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_pair(vm).map(|(key, object)| {
        let has_key = match object {
            JsValue::Object(ptr) => ptr.borrow().has_own_property(&key.to_js_string()),
            _ => false,
        };
        vm.push(JsValue::Boolean(has_key));
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_instanceof(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_pair(vm).map(|_| {
        vm.push(JsValue::Boolean(false));
    });
    finish(vm, result)
}
