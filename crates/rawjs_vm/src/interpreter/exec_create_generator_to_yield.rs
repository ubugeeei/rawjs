/// Create a generator object from a generator function call.
///
/// Instead of pushing a CallFrame and executing, we freeze the initial state
/// into a `GeneratorState` wrapped in a JS object with the generator prototype.
pub(super) fn exec_create_generator(
    vm: &mut Vm,
    chunk_index: usize,
    args: &[JsValue],
    upvalues: Vec<Upvalue>,
    this_value: JsValue,
) -> Result<()> {
    let param_count = vm.chunks[chunk_index].param_count as usize;
    let local_count = vm.chunks[chunk_index].local_count as usize;
    let slot_count = local_count.max(param_count);
    let mut locals = vec![JsValue::Undefined; slot_count];
    for (i, arg) in args.iter().enumerate() {
        if i < slot_count {
            locals[i] = arg.clone();
        }
    }
    let state = GeneratorState {
        status: GeneratorStatus::SuspendedStart,
        chunk_index,
        ip: 0,
        locals,
        arguments: args.to_vec(),
        upvalues,
        saved_stack: Vec::new(),
        this_value,
        result_promise: None,
        saved_try_stack: Vec::new(),
    };
    let mut gen_obj = JsObject::ordinary();
    gen_obj.internal = ObjectInternal::Generator(state);
    if let Some(ref proto) = vm.generator_prototype {
        gen_obj.set_prototype(Some(proto.clone()));
    }
    let ptr = vm.heap.alloc(gen_obj);
    vm.push(JsValue::Object(ptr));
    Ok(())
}

pub(super) fn exec_create_async_generator(
    vm: &mut Vm,
    chunk_index: usize,
    args: &[JsValue],
    upvalues: Vec<Upvalue>,
    this_value: JsValue,
) -> Result<()> {
    exec_create_generator(vm, chunk_index, args, upvalues, this_value)
}

pub(super) fn alloc_promise(vm: &mut Vm) -> rawjs_runtime::GcPtr<JsObject> {
    let promise_ptr = vm.heap.alloc(JsObject::promise());
    if let Some(ref proto) = vm.promise_prototype {
        promise_ptr.borrow_mut().set_prototype(Some(proto.clone()));
    }
    promise_ptr
}

pub(super) fn clear_generator_result_promise(gen_ptr: &rawjs_runtime::GcPtr<JsObject>) {
    let mut gen_obj = gen_ptr.borrow_mut();
    if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
        state.result_promise = None;
    }
}

/// Execute a `Yield` instruction.
///
/// Saves the current frame state into the generator's `GeneratorState`,
/// pops the frame, and pushes `{ value, done: false }` to the caller.
pub(crate) fn exec_yield(vm: &mut Vm) -> Result<Option<JsValue>> {
    let yielded_value = vm.pop()?;
    let frame = vm.call_stack.pop().unwrap();
    let saved_stack: Vec<JsValue> = vm.value_stack.drain(frame.base..).collect();
    if let JsValue::Object(ref gen_ptr) = frame.this_value {
        let mut gen_obj = gen_ptr.borrow_mut();
        if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
            state.status = GeneratorStatus::SuspendedYield;
            state.ip = frame.ip;
            state.locals = frame.locals;
            state.upvalues = frame.upvalues;
            state.saved_stack = saved_stack;
        }
    }
    let result = make_iterator_result(vm, yielded_value, false);
    vm.push(result);
    Ok(None)
}

use super::*;
