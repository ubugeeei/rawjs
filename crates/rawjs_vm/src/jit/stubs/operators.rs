use super::{finish, pop_pair, pop_value, vm_ref};
use crate::Vm;
use rawjs_common::RawJsError;
use rawjs_runtime::JsValue;

fn binary_numeric(vm: &mut Vm, op: impl FnOnce(f64, f64) -> f64) -> Result<(), RawJsError> {
    let (lhs, rhs) = pop_pair(vm)?;
    vm.push(JsValue::Number(op(lhs.to_number(), rhs.to_number())));
    Ok(())
}

fn unary_numeric(vm: &mut Vm, op: impl FnOnce(f64) -> f64) -> Result<(), RawJsError> {
    let value = pop_value(vm)?;
    vm.push(JsValue::Number(op(value.to_number())));
    Ok(())
}

#[no_mangle]
pub extern "C" fn stub_add(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_pair(vm).map(|(lhs, rhs)| {
        vm.push(crate::interpreter::js_add(&lhs, &rhs));
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_sub(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_numeric(vm, |lhs, rhs| lhs - rhs);
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_mul(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_numeric(vm, |lhs, rhs| lhs * rhs);
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_div(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_numeric(vm, |lhs, rhs| lhs / rhs);
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_modulo(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_numeric(vm, |lhs, rhs| lhs % rhs);
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_exp(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = binary_numeric(vm, |lhs, rhs| lhs.powf(rhs));
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_neg(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = unary_numeric(vm, |value| -value);
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_pos(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = unary_numeric(vm, |value| value);
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_not(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).map(|value| {
        vm.push(JsValue::Boolean(!value.to_boolean()));
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_bit_not(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).map(|value| {
        vm.push(JsValue::Number((!value.to_int32()) as f64));
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_typeof(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).map(|value| {
        vm.push(JsValue::string(value.type_of()));
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_void(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).map(|_| {
        vm.push(JsValue::Undefined);
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_delete(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).map(|_| {
        vm.push(JsValue::Boolean(true));
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_postfix_inc(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).map(|value| {
        let number = value.to_number();
        vm.push(JsValue::Number(number));
        vm.push(JsValue::Number(number + 1.0));
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_postfix_dec(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let result = pop_value(vm).map(|value| {
        let number = value.to_number();
        vm.push(JsValue::Number(number));
        vm.push(JsValue::Number(number - 1.0));
    });
    finish(vm, result)
}
