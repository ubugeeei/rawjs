use super::*;
use rawjs_bytecode::{Chunk, Constant, Instruction};
use rawjs_runtime::JsValue;

/// Helper: compile + run via full VM integration (like exec_call would).
fn jit_execute_via_vm(chunk: Chunk, args: &[JsValue]) -> JsValue {
    let func = JitCompiler::compile(&chunk).expect("compilation should succeed");

    let mut vm = crate::Vm::new();
    let chunk_index = vm.add_chunk(chunk);

    // Push a "base" frame so that stub_return has somewhere to push the result.
    let base_chunk = {
        let mut c = Chunk::new("<base>");
        c.emit(Instruction::Return);
        c
    };
    let base_idx = vm.add_chunk(base_chunk);
    vm.call_stack.push(crate::CallFrame {
        chunk_index: base_idx,
        ip: 0,
        base: 0,
        locals: Vec::new(),
        upvalues: Vec::new(),
        this_value: JsValue::Undefined,
    });

    // Now push the actual function frame.
    let param_count = vm.chunks[chunk_index].param_count as usize;
    let local_count = vm.chunks[chunk_index].local_count as usize;
    let slot_count = local_count.max(param_count);

    let mut locals = vec![JsValue::Undefined; slot_count];
    for (i, arg) in args.iter().enumerate() {
        if i < slot_count {
            locals[i] = arg.clone();
        }
    }

    let frame = crate::CallFrame {
        chunk_index,
        ip: 0,
        base: vm.value_stack.len(),
        locals,
        upvalues: Vec::new(),
        this_value: JsValue::Undefined,
    };
    vm.call_stack.push(frame);

    let result = unsafe { func.call_vm(&mut vm as *mut crate::Vm) };
    assert_eq!(result, 0, "JIT execution should succeed (got {})", result);

    // The return value should be on the value_stack.
    vm.value_stack.pop().unwrap_or(JsValue::Undefined)
}

// =========================================================================
// Tests
// =========================================================================

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
    // (10 - 3) * 6 = 42
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

    // 3 < 5 => true
    let result = jit_execute_via_vm(chunk.clone(), &[JsValue::Number(3.0), JsValue::Number(5.0)]);
    assert_eq!(result, JsValue::Boolean(true));

    // 5 < 3 => false
    let result = jit_execute_via_vm(chunk, &[JsValue::Number(5.0), JsValue::Number(3.0)]);
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_jit_simple_branch() {
    // if (a < b) return a; else return b;  (min function)
    let mut chunk = Chunk::new("min");
    chunk.param_count = 2;
    chunk.local_count = 2;

    chunk.emit(Instruction::LoadLocal(0)); // 0
    chunk.emit(Instruction::LoadLocal(1)); // 1
    chunk.emit(Instruction::Lt); // 2
    chunk.emit(Instruction::JumpIfFalse(2)); // 3 -- if false, skip 2
    chunk.emit(Instruction::LoadLocal(0)); // 4
    chunk.emit(Instruction::Return); // 5
    chunk.emit(Instruction::LoadLocal(1)); // 6
    chunk.emit(Instruction::Return); // 7

    // min(3, 7) = 3
    let result = jit_execute_via_vm(chunk.clone(), &[JsValue::Number(3.0), JsValue::Number(7.0)]);
    assert_eq!(result, JsValue::Number(3.0));

    // min(9, 2) = 2
    let result = jit_execute_via_vm(chunk, &[JsValue::Number(9.0), JsValue::Number(2.0)]);
    assert_eq!(result, JsValue::Number(2.0));
}

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
    let idx = chunk.add_constant(Constant::Number(-3.14)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(-3.14));
}

#[test]
fn test_jit_string_concat() {
    let mut chunk = Chunk::new("concat");
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
    let val = chunk.add_constant(Constant::Number(1.0)).unwrap();
    chunk.emit(Instruction::LoadConst(val));
    chunk.emit(Instruction::TypeOf);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::string("number"));
}

#[test]
fn test_jit_strict_eq() {
    let mut chunk = Chunk::new("strict_eq");
    let a = chunk.add_constant(Constant::Number(1.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::StrictEq);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_bitwise_and() {
    let mut chunk = Chunk::new("bitand");
    let a = chunk.add_constant(Constant::Number(0b1100 as f64)).unwrap();
    let b = chunk.add_constant(Constant::Number(0b1010 as f64)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
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
