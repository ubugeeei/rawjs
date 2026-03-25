use super::*;

use rawjs_bytecode::{Chunk, Constant, Instruction};

use std::fs;

use std::time::{SystemTime, UNIX_EPOCH};

/// Helper: build a chunk that pushes a number constant and returns it.
fn make_return_number(n: f64) -> Chunk {
    let mut chunk = Chunk::new("<test>");
    let idx = chunk.add_constant(Constant::Number(n)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::Return);
    chunk
}

fn execute_source(source: &str) -> Vm {
    execute_source_result(source).0
}

fn execute_source_result(source: &str) -> (Vm, JsValue) {
    let program = rawjs_parser::parse(source).unwrap();
    let chunk = rawjs_bytecode::compile(&program).unwrap();
    let mut vm = Vm::new();
    let result = vm.execute(chunk).unwrap();
    (vm, result)
}

#[test]
fn test_vm_new_has_globals() {
    let vm = Vm::new();
    assert!(vm.get_global("undefined").is_some());
    assert!(vm.get_global("NaN").is_some());
    assert!(vm.get_global("Infinity").is_some());
    assert!(vm.get_global("console").is_some());
    assert!(vm.get_global("Math").is_some());
    assert!(vm.get_global("Object").is_some());
    assert!(vm.get_global("Array").is_some());
}

#[test]
fn test_execute_return_number() {
    let mut vm = Vm::new();
    let result = vm.execute(make_return_number(42.0)).unwrap();
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_execute_simple_add() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let a = chunk.add_constant(Constant::Number(10.0)).unwrap();
    let b = chunk.add_constant(Constant::Number(32.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::Add);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_execute_string_concat() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let a = chunk
        .add_constant(Constant::String("hello ".to_string()))
        .unwrap();
    let b = chunk
        .add_constant(Constant::String("world".to_string()))
        .unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::Add);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::string("hello world"));
}

#[test]
fn test_execute_comparison() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let a = chunk.add_constant(Constant::Number(5.0)).unwrap();
    let b = chunk.add_constant(Constant::Number(3.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::Gt);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}
