#[test]
fn test_execute_local_vars() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    chunk.local_count = 1;
    let val = chunk.add_constant(Constant::Number(99.0)).unwrap();
    chunk.emit(Instruction::LoadConst(val));
    chunk.emit(Instruction::StoreLocal(0));
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Number(99.0));
}

#[test]
fn test_execute_jump_if_false() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let one = chunk.add_constant(Constant::Number(1.0)).unwrap();
    let two = chunk.add_constant(Constant::Number(2.0)).unwrap();
    chunk.emit(Instruction::False);
    let jmp = chunk.emit(Instruction::JumpIfFalse(0));
    chunk.emit(Instruction::LoadConst(one));
    chunk.emit(Instruction::Return);
    chunk.emit(Instruction::LoadConst(two));
    chunk.emit(Instruction::Return);
    chunk.patch_jump(jmp, 2);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Number(2.0));
}

#[test]
fn test_execute_create_object() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    chunk.emit(Instruction::CreateObject);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert!(result.is_object());
}

#[test]
fn test_execute_create_array() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let a = chunk.add_constant(Constant::Number(1.0)).unwrap();
    let b = chunk.add_constant(Constant::Number(2.0)).unwrap();
    let c = chunk.add_constant(Constant::Number(3.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::LoadConst(c));
    chunk.emit(Instruction::CreateArray(3));
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert!(result.is_object());
    if let JsValue::Object(obj) = &result {
        assert!(obj.borrow().is_array());
        assert_eq!(obj.borrow().as_array().unwrap().len(), 3);
    }
}

#[test]
fn test_execute_unary_neg() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let val = chunk.add_constant(Constant::Number(7.0)).unwrap();
    chunk.emit(Instruction::LoadConst(val));
    chunk.emit(Instruction::Neg);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Number(-7.0));
}

#[test]
fn test_execute_member_postfix_increment_uses_numeric_coercion() {
    let vm = execute_source("var __map = { foo: 'bar' }; __map.foo++;");
    let map = vm.get_global("__map").unwrap().clone();
    let JsValue::Object(obj) = map else {
        panic!("expected object");
    };
    let value = obj.borrow().get_property("foo");
    match value {
        JsValue::Number(n) => assert!(n.is_nan()),
        other => panic!("expected NaN number, got {other}"),
    }
}

#[test]
fn test_execute_accessor_property_setter() {
    let vm = execute_source(
        "var o = {}; var v = 1; Object.defineProperty(o, 'b', { get: function () { return v; }, set: function (value) { v = value; } }); o.b = 11;",
    );
    assert_eq!(vm.get_global("v"), Some(&JsValue::Number(11.0)));
}

#[test]
fn test_execute_typeof() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let val = chunk.add_constant(Constant::Number(1.0)).unwrap();
    chunk.emit(Instruction::LoadConst(val));
    chunk.emit(Instruction::TypeOf);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::string("number"));
}

#[test]
fn test_execute_dup_and_pop() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let val = chunk.add_constant(Constant::Number(5.0)).unwrap();
    chunk.emit(Instruction::LoadConst(val));
    chunk.emit(Instruction::Dup);
    chunk.emit(Instruction::Pop);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Number(5.0));
}

#[allow(unused_imports)]
use super::*;
