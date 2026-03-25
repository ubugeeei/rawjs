use super::helpers::jit_execute_via_vm;
use crate::jit::JitCompiler;
use rawjs_bytecode::{Chunk, Constant, Instruction};
use rawjs_runtime::JsValue;

#[test]
fn test_jit_return_zero_const() {
    let mut chunk = Chunk::new("zero");
    let idx = chunk.add_constant(Constant::Number(0.0)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(0.0));
}

#[test]
fn test_jit_return_negative_const() {
    let mut chunk = Chunk::new("neg_const");
    let idx = chunk.add_constant(Constant::Number(-2.5)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(-2.5));
}

#[test]
fn test_jit_string_concat() {
    let mut chunk = Chunk::new("concat");
    let hello = chunk
        .add_constant(Constant::String("hello ".to_string()))
        .unwrap();
    let world = chunk
        .add_constant(Constant::String("world".to_string()))
        .unwrap();
    chunk.emit(Instruction::LoadConst(hello));
    chunk.emit(Instruction::LoadConst(world));
    chunk.emit(Instruction::Add);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::string("hello world"));
}

#[test]
fn test_jit_create_object() {
    let mut chunk = Chunk::new("obj");
    chunk.emit(Instruction::CreateObject);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert!(result.is_object());
}

#[test]
fn test_jit_modulo() {
    let mut chunk = Chunk::new("mod");
    chunk.param_count = 2;
    chunk.local_count = 2;
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::LoadLocal(1));
    chunk.emit(Instruction::Mod);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[JsValue::Number(10.0), JsValue::Number(3.0)]);
    assert_eq!(result, JsValue::Number(1.0));
}

#[test]
fn test_jit_boolean_not() {
    let mut chunk = Chunk::new("not");
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::Not);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_jit_typeof() {
    let mut chunk = Chunk::new("typeof");
    let idx = chunk.add_constant(Constant::Number(1.0)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::TypeOf);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::string("number"));
}

#[test]
fn test_jit_strict_eq() {
    let mut chunk = Chunk::new("strict_eq");
    let idx = chunk.add_constant(Constant::Number(1.0)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::StrictEq);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_bitwise_and() {
    let mut chunk = Chunk::new("bitand");
    let lhs = chunk.add_constant(Constant::Number(0b1100 as f64)).unwrap();
    let rhs = chunk.add_constant(Constant::Number(0b1010 as f64)).unwrap();
    chunk.emit(Instruction::LoadConst(lhs));
    chunk.emit(Instruction::LoadConst(rhs));
    chunk.emit(Instruction::BitAnd);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(0b1000 as f64));
}

#[test]
fn test_jit_accepts_enter_try() {
    let mut chunk = Chunk::new("try");
    chunk.emit(Instruction::EnterTry(2, 0));
    chunk.emit(Instruction::LeaveTry);
    chunk.emit(Instruction::Return);

    assert!(JitCompiler::compile(&chunk).is_some());
}
