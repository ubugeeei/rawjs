use crate::Vm;
use rawjs_bytecode::Constant;
use rawjs_common::RawJsError;
use rawjs_runtime::JsValue;

pub(crate) fn vm_ref<'a>(vm: *mut Vm) -> &'a mut Vm {
    unsafe { &mut *vm }
}

pub(crate) fn set_error(vm: *mut Vm, err: RawJsError) -> u32 {
    let vm = vm_ref(vm);
    vm.jit_error = Some(err);
    1
}

pub(crate) fn finish(vm: &mut Vm, result: Result<(), RawJsError>) -> u32 {
    match result {
        Ok(()) => 0,
        Err(err) => set_error(vm as *mut Vm, err),
    }
}

pub(crate) fn pop_value(vm: &mut Vm) -> Result<JsValue, RawJsError> {
    vm.pop()
}

pub(crate) fn pop_pair(vm: &mut Vm) -> Result<(JsValue, JsValue), RawJsError> {
    let rhs = vm.pop()?;
    let lhs = vm.pop()?;
    Ok((lhs, rhs))
}

pub(crate) fn chunk_string_constant(vm: &Vm, idx: u32) -> Result<String, RawJsError> {
    let frame = vm.call_stack.last().unwrap();
    let chunk = &vm.chunks[frame.chunk_index];
    match &chunk.constants[idx as usize] {
        Constant::String(value) => Ok(value.clone()),
        _ => Err(RawJsError::internal_error("expected string constant")),
    }
}
