use super::{finish, pop_value, set_error, vm_ref};
use crate::Vm;
use rawjs_common::RawJsError;

#[no_mangle]
pub extern "C" fn stub_throw(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let value = match pop_value(vm) {
        Ok(value) => value,
        Err(err) => return set_error(vm as *mut Vm, err),
    };
    vm.thrown_value = Some(value.clone());
    set_error(vm as *mut Vm, RawJsError::internal_error(value.to_string()))
}

#[no_mangle]
pub extern "C" fn stub_enter_try(vm: *mut Vm, catch_offset: i32, finally_offset: i32) -> u32 {
    let vm = vm_ref(vm);
    let current_ip = vm.call_stack.last().unwrap().ip;
    let catch_ip = if catch_offset != 0 {
        Some(((current_ip as i64) + catch_offset as i64) as usize)
    } else {
        None
    };
    let finally_ip = if finally_offset != 0 {
        Some(((current_ip as i64) + finally_offset as i64) as usize)
    } else {
        None
    };

    vm.try_stack.push(crate::TryContext {
        catch_ip,
        finally_ip,
        stack_depth: vm.value_stack.len(),
        call_depth: vm.call_stack.len(),
    });
    0
}

#[no_mangle]
pub extern "C" fn stub_leave_try(vm: *mut Vm) -> u32 {
    vm_ref(vm).try_stack.pop();
    0
}

#[no_mangle]
pub extern "C" fn stub_create_generator(vm: *mut Vm) -> u32 {
    set_error(
        vm,
        RawJsError::internal_error("CreateGenerator should not be executed directly"),
    )
}

#[no_mangle]
pub extern "C" fn stub_yield(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match crate::interpreter::exec_yield(vm) {
        Ok(_) => 2,
        Err(err) => set_error(vm as *mut Vm, err),
    }
}

#[no_mangle]
pub extern "C" fn stub_await(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match crate::interpreter::exec_await(vm) {
        Ok(_) => 2,
        Err(err) => set_error(vm as *mut Vm, err),
    }
}

#[no_mangle]
pub extern "C" fn stub_dispose_resource(vm: *mut Vm, slot: u32) -> u32 {
    let vm = vm_ref(vm);
    let result = crate::interpreter::exec_dispose_resource(vm, slot as u16);
    finish(vm, result)
}

#[no_mangle]
pub extern "C" fn stub_async_dispose_resource(vm: *mut Vm, _slot: u32) -> u32 {
    set_error(
        vm,
        RawJsError::internal_error("AsyncDisposeResource should not execute in the baseline JIT"),
    )
}
