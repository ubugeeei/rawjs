pub(super) fn raw_error_to_value(vm: &mut Vm, err: &RawJsError) -> JsValue {
    use rawjs_common::error::ErrorKind;
    let ctor_name = match err.kind {
        ErrorKind::SyntaxError => "SyntaxError",
        ErrorKind::TypeError => "TypeError",
        ErrorKind::ReferenceError => "ReferenceError",
        ErrorKind::RangeError => "RangeError",
        ErrorKind::InternalError => "Error",
    };
    let mut obj = JsObject::typed_error(ctor_name, err.message.clone());
    if let Some(JsValue::Object(ctor)) = vm.get_global(ctor_name).cloned() {
        obj.define_property(
            "constructor".to_string(),
            Property::builtin(JsValue::Object(ctor.clone())),
        );
        if let JsValue::Object(proto) = ctor.borrow().get_property("prototype") {
            obj.set_prototype(Some(proto));
        }
    }
    JsValue::Object(vm.heap.alloc(obj))
}

pub(super) fn ensure_thrown_value(vm: &mut Vm, err: &RawJsError) {
    if vm.thrown_value.is_none() {
        vm.thrown_value = Some(raw_error_to_value(vm, err));
    }
}

/// Convert a compile-time `Constant` to a runtime `JsValue`.
pub(crate) fn constant_to_value(constant: &Constant) -> JsValue {
    match constant {
        Constant::Number(n) => JsValue::Number(*n),
        Constant::String(s) => JsValue::string(s.as_str()),
        Constant::Boolean(b) => JsValue::Boolean(*b),
        Constant::Null => JsValue::Null,
        Constant::Undefined => JsValue::Undefined,
        Constant::Function(_) => JsValue::Undefined,
    }
}

/// Look up a constant string by index in a chunk's constant pool.
pub(super) fn get_constant_string(chunk: &Chunk, idx: u16) -> Result<String> {
    match &chunk.constants[idx as usize] {
        Constant::String(s) => Ok(s.clone()),
        other => Err(RawJsError::internal_error(format!(
            "expected string constant at index {}, found {}",
            idx, other
        ))),
    }
}

/// Check if a callee value is the global Promise constructor.
pub(super) fn is_promise_constructor(vm: &Vm, callee: &JsValue) -> bool {
    if let Some(global_promise) = vm.get_global("Promise") {
        if let (JsValue::Object(a), JsValue::Object(b)) = (callee, global_promise) {
            return a.ptr_eq(b);
        }
    }
    false
}

pub(super) fn is_eval_function(vm: &Vm, callee: &JsValue) -> bool {
    if let Some(global_eval) = vm.get_global("eval") {
        if let (JsValue::Object(a), JsValue::Object(b)) = (callee, global_eval) {
            return a.ptr_eq(b);
        }
    }
    false
}

pub(super) fn exec_eval(vm: &mut Vm, args: &[JsValue]) -> Result<()> {
    let Some(source) = args.first() else {
        vm.push(JsValue::Undefined);
        return Ok(());
    };
    if !source.is_string() {
        vm.push(source.clone());
        return Ok(());
    }
    let mut eval_source = source.to_string_value();
    if vm
        .call_stack
        .last()
        .map(|frame| frame.is_strict)
        .unwrap_or(false)
    {
        eval_source = format!("\"use strict\";\n{eval_source}");
    }
    let program = parse_program(&eval_source)
        .map_err(|err| RawJsError::syntax_error(format!("{err}"), None))?;
    let chunk = rawjs_bytecode::compile(&program)
        .map_err(|err| RawJsError::syntax_error(format!("{err}"), None))?;
    let chunk_index = vm.add_chunk(chunk);
    let local_count = vm.chunks[chunk_index].local_count as usize;
    let frame = CallFrame {
        chunk_index,
        ip: 0,
        base: vm.value_stack.len(),
        locals: vec![JsValue::Undefined; local_count],
        arguments: Vec::new(),
        arguments_object: None,
        callee: None,
        is_strict: vm.chunks[chunk_index].is_strict,
        upvalues: Vec::new(),
        this_value: vm.global_this_value(),
    };
    vm.call_stack.push(frame);
    Ok(())
}

pub(super) fn strict_arguments_thrower(
    _heap: &mut Heap,
    _this: &JsValue,
    _args: &[JsValue],
) -> Result<JsValue> {
    Err(RawJsError::type_error(
        "Cannot access arguments.callee in strict mode",
    ))
}

pub(super) fn create_strict_arguments_thrower(vm: &mut Vm) -> rawjs_runtime::GcPtr<JsObject> {
    let ptr = vm.heap.alloc(JsObject::native_function(
        "ThrowTypeError",
        strict_arguments_thrower,
    ));
    if let Some(proto) = &vm.function_prototype {
        ptr.borrow_mut().set_prototype(Some(proto.clone()));
    }
    ptr
}

use super::*;
