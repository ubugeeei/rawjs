#[test]
fn test_execute_logical_nullish_assignment() {
    let vm = execute_source("let value = null; out = (value ??= 42); out2 = value;");
    assert_eq!(vm.get_global("out"), Some(&JsValue::Number(42.0)));
    assert_eq!(vm.get_global("out2"), Some(&JsValue::Number(42.0)));
}

#[test]
fn test_execute_logical_assignment_member_evaluates_left_once() {
    let vm = execute_source(
        "objCalls = 0; keyCalls = 0; holder = { value: 0 }; function getObj() { objCalls += 1; return holder; } function getKey() { keyCalls += 1; return 'value'; } out = (getObj()[getKey()] ||= 1); out2 = objCalls; out3 = keyCalls; out4 = holder.value;",
    );
    assert_eq!(vm.get_global("out"), Some(&JsValue::Number(1.0)));
    assert_eq!(vm.get_global("out2"), Some(&JsValue::Number(1.0)));
    assert_eq!(vm.get_global("out3"), Some(&JsValue::Number(1.0)));
    assert_eq!(vm.get_global("out4"), Some(&JsValue::Number(1.0)));
}

#[test]
fn test_execute_try_catch() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    chunk.local_count = 1;
    let msg_idx = chunk
        .add_constant(Constant::String("oops".to_string()))
        .unwrap();
    let result_idx = chunk.add_constant(Constant::Number(999.0)).unwrap();
    chunk.emit(Instruction::EnterTry(2, 0));
    chunk.emit(Instruction::LoadConst(msg_idx));
    chunk.emit(Instruction::Throw);
    chunk.emit(Instruction::StoreLocal(0));
    chunk.emit(Instruction::LeaveTry);
    chunk.emit(Instruction::LoadConst(result_idx));
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Number(999.0));
}

#[test]
fn test_execute_set_get_property() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    chunk.local_count = 1;
    let name_idx = chunk
        .add_constant(Constant::String("x".to_string()))
        .unwrap();
    let val_idx = chunk.add_constant(Constant::Number(42.0)).unwrap();
    chunk.emit(Instruction::CreateObject);
    chunk.emit(Instruction::Dup);
    chunk.emit(Instruction::StoreLocal(0));
    chunk.emit(Instruction::LoadConst(val_idx));
    chunk.emit(Instruction::SetProperty(name_idx));
    chunk.emit(Instruction::Pop);
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::GetProperty(name_idx));
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_execute_void() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let val = chunk.add_constant(Constant::Number(42.0)).unwrap();
    chunk.emit(Instruction::LoadConst(val));
    chunk.emit(Instruction::Void);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert!(result.is_undefined());
}

#[test]
fn test_execute_not() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::Not);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_execute_strict_eq() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let a = chunk.add_constant(Constant::Number(1.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::StrictEq);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_add_chunk_returns_index() {
    let mut vm = Vm::new();
    let a = vm.add_chunk(Chunk::new("a"));
    let b = vm.add_chunk(Chunk::new("b"));
    assert_eq!(a, 0);
    assert_eq!(b, 1);
}

#[test]
fn test_execute_new_function_constructor_uses_prototype() {
    let vm = execute_source(
        r#"
            function Test262Error(message) {
              this.message = message;
            }
            Test262Error.prototype.answer = 42;
            let err = new Test262Error("boom");
            result = err.answer;
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_execute_number_static_constants_are_read_only() {
    let vm = execute_source(
        r#"
            Number.NaN = 1;
            result = Number.NaN === 1;
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Boolean(false));
}

#[allow(unused_imports)]
use super::*;
