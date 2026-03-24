use super::helpers::jit_execute_via_vm;
use rawjs_bytecode::{Chunk, Constant, Instruction};
use rawjs_runtime::JsValue;

#[test]
fn test_jit_return_const() {
    let mut chunk = Chunk::new("jit_test");
    let idx = chunk.add_constant(Constant::Number(42.0)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_jit_add_params() {
    let mut chunk = Chunk::new("add");
    chunk.param_count = 2;
    chunk.local_count = 2;
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::LoadLocal(1));
    chunk.emit(Instruction::Add);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[JsValue::Number(10.0), JsValue::Number(32.0)]);
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_jit_arithmetic() {
    let mut chunk = Chunk::new("arith");
    chunk.param_count = 3;
    chunk.local_count = 3;
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::LoadLocal(1));
    chunk.emit(Instruction::Sub);
    chunk.emit(Instruction::LoadLocal(2));
    chunk.emit(Instruction::Mul);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(
        chunk,
        &[
            JsValue::Number(10.0),
            JsValue::Number(3.0),
            JsValue::Number(6.0),
        ],
    );
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_jit_neg() {
    let mut chunk = Chunk::new("neg");
    chunk.param_count = 1;
    chunk.local_count = 1;
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::Neg);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[JsValue::Number(7.0)]);
    assert_eq!(result, JsValue::Number(-7.0));
}

#[test]
fn test_jit_div() {
    let mut chunk = Chunk::new("div");
    chunk.param_count = 2;
    chunk.local_count = 2;
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::LoadLocal(1));
    chunk.emit(Instruction::Div);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[JsValue::Number(84.0), JsValue::Number(2.0)]);
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_jit_store_load_local() {
    let mut chunk = Chunk::new("local");
    chunk.param_count = 1;
    chunk.local_count = 2;
    let ten = chunk.add_constant(Constant::Number(10.0)).unwrap();
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::LoadConst(ten));
    chunk.emit(Instruction::Add);
    chunk.emit(Instruction::StoreLocal(1));
    chunk.emit(Instruction::LoadLocal(1));
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[JsValue::Number(32.0)]);
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_jit_comparison_lt() {
    let mut chunk = Chunk::new("lt");
    chunk.param_count = 2;
    chunk.local_count = 2;
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::LoadLocal(1));
    chunk.emit(Instruction::Lt);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk.clone(), &[JsValue::Number(3.0), JsValue::Number(5.0)]);
    assert_eq!(result, JsValue::Boolean(true));

    let result = jit_execute_via_vm(chunk, &[JsValue::Number(5.0), JsValue::Number(3.0)]);
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_jit_simple_branch() {
    let mut chunk = Chunk::new("min");
    chunk.param_count = 2;
    chunk.local_count = 2;
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::LoadLocal(1));
    chunk.emit(Instruction::Lt);
    chunk.emit(Instruction::JumpIfFalse(2));
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::Return);
    chunk.emit(Instruction::LoadLocal(1));
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk.clone(), &[JsValue::Number(3.0), JsValue::Number(7.0)]);
    assert_eq!(result, JsValue::Number(3.0));

    let result = jit_execute_via_vm(chunk, &[JsValue::Number(9.0), JsValue::Number(2.0)]);
    assert_eq!(result, JsValue::Number(2.0));
}
