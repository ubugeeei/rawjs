use super::{chunk_string_constant, pop_value, set_error, vm_ref};
use crate::Vm;
use rawjs_runtime::{builtins, JsValue};

#[no_mangle]
pub extern "C" fn stub_import_module(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let source = match chunk_string_constant(vm, idx) {
        Ok(source) => source,
        Err(err) => return set_error(vm as *mut Vm, err),
    };
    match vm.execute_module(&source) {
        Ok(namespace) => {
            vm.push(JsValue::Object(namespace));
            0
        }
        Err(err) => set_error(vm as *mut Vm, err),
    }
}

#[no_mangle]
pub extern "C" fn stub_import_module_dynamic(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let specifier = match pop_value(vm) {
        Ok(value) => value,
        Err(err) => return set_error(vm as *mut Vm, err),
    };

    let promise = vm.create_promise_object();
    let source = specifier.to_string_value();
    match vm.execute_module(&source) {
        Ok(namespace) => {
            builtins::resolve_promise_with_heap(&promise, JsValue::Object(namespace), &mut vm.heap)
        }
        Err(err) => builtins::reject_promise_with_heap(
            &promise,
            JsValue::string(err.to_string()),
            &mut vm.heap,
        ),
    }

    vm.push(JsValue::Object(promise));
    0
}

#[no_mangle]
pub extern "C" fn stub_import_meta(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match vm.create_import_meta_object() {
        Ok(meta) => {
            vm.push(JsValue::Object(meta));
            0
        }
        Err(err) => set_error(vm as *mut Vm, err),
    }
}

#[no_mangle]
pub extern "C" fn stub_import_binding(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = match chunk_string_constant(vm, idx) {
        Ok(name) => name,
        Err(err) => return set_error(vm as *mut Vm, err),
    };
    let namespace = match pop_value(vm) {
        Ok(namespace) => namespace,
        Err(err) => return set_error(vm as *mut Vm, err),
    };

    let value = match namespace {
        JsValue::Object(ptr) => ptr.borrow().get_property(&name),
        _ => JsValue::Undefined,
    };
    vm.push(value);
    0
}

#[no_mangle]
pub extern "C" fn stub_export_binding(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = match chunk_string_constant(vm, idx) {
        Ok(name) => name,
        Err(err) => return set_error(vm as *mut Vm, err),
    };
    let value = match pop_value(vm) {
        Ok(value) => value,
        Err(err) => return set_error(vm as *mut Vm, err),
    };

    if let Some(exports) = vm.module_exports.clone() {
        exports.borrow_mut().set_property(name, value);
    }
    0
}

#[no_mangle]
pub extern "C" fn stub_export_default(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let value = match pop_value(vm) {
        Ok(value) => value,
        Err(err) => return set_error(vm as *mut Vm, err),
    };

    if let Some(exports) = vm.module_exports.clone() {
        exports
            .borrow_mut()
            .set_property("default".to_string(), value);
    }
    0
}
