use super::{finish, pop_value, set_error, vm_ref};
use crate::Vm;
use rawjs_common::RawJsError;
use rawjs_runtime::JsValue;

fn run_call(vm: &mut Vm, argc: usize, receiver: JsValue) -> Result<(), RawJsError> {
    let target_depth = vm.call_stack.len();
    crate::interpreter::exec_call(vm, argc, receiver)?;
    if vm.call_stack.len() > target_depth {
        crate::interpreter::run_inner_frame(vm, target_depth)?;
    }
    Ok(())
}

#[no_mangle]
pub extern "C" fn stub_call(vm: *mut Vm, argc: u32) -> u32 {
    let vm = vm_ref(vm);
    let result = run_call(vm, argc as usize, JsValue::Undefined);
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_call_method(vm: *mut Vm, argc: u32) -> u32 {
    let vm = vm_ref(vm);
    let argc = argc as usize;
    let stack_len = vm.value_stack.len();
    if stack_len < argc + 2 {
        return set_error(
            vm as *mut Vm,
            RawJsError::internal_error("stack underflow in CallMethod"),
        );
    }

    let args: Vec<JsValue> = vm.value_stack.drain(stack_len - argc..).collect();
    let result = pop_value(vm).and_then(|method| {
        pop_value(vm).and_then(|receiver| {
            vm.push(method);
            for arg in args {
                vm.push(arg);
            }
            run_call(vm, argc, receiver)
        })
    });
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_return(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let base = vm.call_stack.last().unwrap().base;
    let return_value = if vm.value_stack.len() > base {
        pop_value(vm).unwrap_or(JsValue::Undefined)
    } else {
        JsValue::Undefined
    };

    let frame = vm.call_stack.pop().unwrap();
    vm.value_stack.truncate(frame.base);
    if !vm.call_stack.is_empty() {
        vm.push(return_value);
    }
    2
}

#[no_mangle]
pub extern "C" fn stub_pop_and_test_truthy(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match pop_value(vm) {
        Ok(value) if value.to_boolean() => 1,
        Ok(_) | Err(_) => 0,
    }
}

#[no_mangle]
pub extern "C" fn stub_pop_and_test_nullish(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match pop_value(vm) {
        Ok(value) if value.is_nullish() => 1,
        Ok(_) | Err(_) => 0,
    }
}
