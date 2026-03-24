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
        arguments: Vec::new(),
        arguments_object: None,
        callee: None,
        is_strict: false,
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
        arguments: Vec::new(),
        arguments_object: None,
        callee: None,
        is_strict: false,
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

#[test]
fn test_jsvalue_layout() {
    use std::mem;
    let size = mem::size_of::<JsValue>();
    let align = mem::align_of::<JsValue>();
    eprintln!("sizeof(JsValue) = {size}, alignof = {align}");

    let num = JsValue::Number(std::f64::consts::PI);
    let undef = JsValue::Undefined;
    let null_v = JsValue::Null;
    let bool_v = JsValue::Boolean(true);

    let pi = std::f64::consts::PI.to_le_bytes();

    unsafe {
        let np = &num as *const _ as *const u8;
        let up = &undef as *const _ as *const u8;
        let nlp = &null_v as *const _ as *const u8;
        let bp = &bool_v as *const _ as *const u8;

        eprint!("Number:    ");
        for i in 0..size {
            eprint!("{:02x} ", *np.add(i));
        }
        eprintln!();
        eprint!("Undefined: ");
        for i in 0..size {
            eprint!("{:02x} ", *up.add(i));
        }
        eprintln!();
        eprint!("Null:      ");
        for i in 0..size {
            eprint!("{:02x} ", *nlp.add(i));
        }
        eprintln!();
        eprint!("Bool(t):   ");
        for i in 0..size {
            eprint!("{:02x} ", *bp.add(i));
        }
        eprintln!();

        for i in 0..size.saturating_sub(7) {
            let slice = std::slice::from_raw_parts(np.add(i), 8);
            if slice == &pi {
                eprintln!("f64_offset = {i}");
            }
        }

        // Check tag byte 0
        eprintln!(
            "tag[0]: undef={}, null={}, bool={}, num={}",
            *up, *nlp, *bp, *np
        );
    }

    // With #[repr(C, u8)], we expect:
    // tag at byte 0, f64 at byte 8, size 32
    assert_eq!(size, 32, "JsValue size should be 32 with #[repr(C, u8)]");
}

// =========================================================================
// Boolean comparison tests (inline JIT fast path)
// =========================================================================

#[test]
fn test_jit_boolean_strict_eq() {
    let mut chunk = Chunk::new("bool_eq");
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::StrictEq);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_boolean_strict_eq_false() {
    let mut chunk = Chunk::new("bool_eq_f");
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::False);
    chunk.emit(Instruction::StrictEq);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_jit_boolean_strict_ne() {
    let mut chunk = Chunk::new("bool_ne");
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::False);
    chunk.emit(Instruction::StrictNe);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_different_types_strict_eq() {
    // Number vs Boolean → false
    let mut chunk = Chunk::new("diff_eq");
    let idx = chunk.add_constant(Constant::Number(1.0)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::StrictEq);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_jit_undefined_strict_eq() {
    let mut chunk = Chunk::new("undef_eq");
    chunk.emit(Instruction::Undefined);
    chunk.emit(Instruction::Undefined);
    chunk.emit(Instruction::StrictEq);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_null_strict_eq() {
    let mut chunk = Chunk::new("null_eq");
    chunk.emit(Instruction::Null);
    chunk.emit(Instruction::Null);
    chunk.emit(Instruction::StrictEq);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_null_undefined_strict_ne() {
    // null !== undefined → true (different types)
    let mut chunk = Chunk::new("null_undef_ne");
    chunk.emit(Instruction::Null);
    chunk.emit(Instruction::Undefined);
    chunk.emit(Instruction::StrictNe);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_boolean_lt() {
    // false < true → true
    let mut chunk = Chunk::new("bool_lt");
    chunk.emit(Instruction::False);
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::Lt);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk.clone(), &[]);
    assert_eq!(result, JsValue::Boolean(true));

    // true < false → false
    let mut chunk2 = Chunk::new("bool_lt2");
    chunk2.emit(Instruction::True);
    chunk2.emit(Instruction::False);
    chunk2.emit(Instruction::Lt);
    chunk2.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk2, &[]);
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_jit_boolean_ge() {
    // true >= false → true
    let mut chunk = Chunk::new("bool_ge");
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::False);
    chunk.emit(Instruction::Ge);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));

    // false >= false → true
    let mut chunk2 = Chunk::new("bool_ge2");
    chunk2.emit(Instruction::False);
    chunk2.emit(Instruction::False);
    chunk2.emit(Instruction::Ge);
    chunk2.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk2, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

// =========================================================================
// Inline push literal tests
// =========================================================================

#[test]
fn test_jit_push_undefined() {
    let mut chunk = Chunk::new("undef");
    chunk.emit(Instruction::Undefined);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Undefined);
}

#[test]
fn test_jit_push_null() {
    let mut chunk = Chunk::new("null");
    chunk.emit(Instruction::Null);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Null);
}

#[test]
fn test_jit_push_true() {
    let mut chunk = Chunk::new("true");
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_push_false() {
    let mut chunk = Chunk::new("false");
    chunk.emit(Instruction::False);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(false));
}

// =========================================================================
// Inline Pop tests
// =========================================================================

#[test]
fn test_jit_pop_primitive() {
    // Push two numbers, pop one, return the other
    let mut chunk = Chunk::new("pop");
    let a = chunk.add_constant(Constant::Number(42.0)).unwrap();
    let b = chunk.add_constant(Constant::Number(99.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::Pop);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(42.0));
}

// =========================================================================
// Inline Dup tests
// =========================================================================

#[test]
fn test_jit_dup_primitive() {
    let mut chunk = Chunk::new("dup");
    let a = chunk.add_constant(Constant::Number(7.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::Dup);
    chunk.emit(Instruction::Add);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(14.0)); // 7 + 7
}

#[test]
fn test_jit_dup_boolean() {
    let mut chunk = Chunk::new("dup_bool");
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::Dup);
    chunk.emit(Instruction::StrictEq);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

// =========================================================================
// Inline Void tests
// =========================================================================

#[test]
fn test_jit_void_number() {
    let mut chunk = Chunk::new("void");
    let a = chunk.add_constant(Constant::Number(42.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::Void);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Undefined);
}

#[test]
fn test_jit_void_boolean() {
    let mut chunk = Chunk::new("void_bool");
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::Void);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Undefined);
}

// =========================================================================
// Inline Delete tests
// =========================================================================

#[test]
fn test_jit_delete_number() {
    let mut chunk = Chunk::new("del");
    let a = chunk.add_constant(Constant::Number(42.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::Delete);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

// =========================================================================
// Inline Not tests (expanded)
// =========================================================================

#[test]
fn test_jit_not_true() {
    let mut chunk = Chunk::new("not_t");
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::Not);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_jit_not_false() {
    let mut chunk = Chunk::new("not_f");
    chunk.emit(Instruction::False);
    chunk.emit(Instruction::Not);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_not_number_zero() {
    // !0 → true
    let mut chunk = Chunk::new("not_zero");
    let idx = chunk.add_constant(Constant::Number(0.0)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::Not);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_not_number_nonzero() {
    // !42 → false
    let mut chunk = Chunk::new("not_nonzero");
    let idx = chunk.add_constant(Constant::Number(42.0)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::Not);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_jit_not_number_nan() {
    // !NaN → true
    let mut chunk = Chunk::new("not_nan");
    let idx = chunk.add_constant(Constant::Number(f64::NAN)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::Not);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_not_undefined() {
    // !undefined → true
    let mut chunk = Chunk::new("not_undef");
    chunk.emit(Instruction::Undefined);
    chunk.emit(Instruction::Not);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_not_null() {
    // !null → true
    let mut chunk = Chunk::new("not_null");
    chunk.emit(Instruction::Null);
    chunk.emit(Instruction::Not);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

// =========================================================================
// Inline Pos tests
// =========================================================================

#[test]
fn test_jit_pos_number() {
    // +42 → 42 (noop)
    let mut chunk = Chunk::new("pos_num");
    let idx = chunk.add_constant(Constant::Number(42.0)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::Pos);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_jit_pos_true() {
    // +true → 1
    let mut chunk = Chunk::new("pos_true");
    chunk.emit(Instruction::True);
    chunk.emit(Instruction::Pos);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(1.0));
}

#[test]
fn test_jit_pos_false() {
    // +false → 0
    let mut chunk = Chunk::new("pos_false");
    chunk.emit(Instruction::False);
    chunk.emit(Instruction::Pos);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(0.0));
}

#[test]
fn test_jit_pos_null() {
    // +null → 0
    let mut chunk = Chunk::new("pos_null");
    chunk.emit(Instruction::Null);
    chunk.emit(Instruction::Pos);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(0.0));
}

#[test]
fn test_jit_pos_undefined() {
    // +undefined → NaN
    let mut chunk = Chunk::new("pos_undef");
    chunk.emit(Instruction::Undefined);
    chunk.emit(Instruction::Pos);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    match result {
        JsValue::Number(n) => assert!(n.is_nan(), "expected NaN, got {}", n),
        other => panic!("expected Number(NaN), got {:?}", other),
    }
}

// =========================================================================
// Inline BitNot tests
// =========================================================================

#[test]
fn test_jit_bitnot() {
    // ~5 → -6
    let mut chunk = Chunk::new("bitnot");
    let idx = chunk.add_constant(Constant::Number(5.0)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::BitNot);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(-6.0));
}

#[test]
fn test_jit_bitnot_neg() {
    // ~(-1) → 0
    let mut chunk = Chunk::new("bitnot_neg");
    let idx = chunk.add_constant(Constant::Number(-1.0)).unwrap();
    chunk.emit(Instruction::LoadConst(idx));
    chunk.emit(Instruction::BitNot);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(0.0));
}

// =========================================================================
// Inline BitOr, BitXor tests
// =========================================================================

#[test]
fn test_jit_bitwise_or() {
    // 0b1100 | 0b1010 → 0b1110
    let mut chunk = Chunk::new("bitor");
    let a = chunk.add_constant(Constant::Number(0b1100 as f64)).unwrap();
    let b = chunk.add_constant(Constant::Number(0b1010 as f64)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::BitOr);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(0b1110 as f64));
}

#[test]
fn test_jit_bitwise_xor() {
    // 0b1100 ^ 0b1010 → 0b0110
    let mut chunk = Chunk::new("bitxor");
    let a = chunk.add_constant(Constant::Number(0b1100 as f64)).unwrap();
    let b = chunk.add_constant(Constant::Number(0b1010 as f64)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::BitXor);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(0b0110 as f64));
}

// =========================================================================
// Inline Shl, Shr, UShr tests
// =========================================================================

#[test]
fn test_jit_shl() {
    // 1 << 3 → 8
    let mut chunk = Chunk::new("shl");
    let a = chunk.add_constant(Constant::Number(1.0)).unwrap();
    let b = chunk.add_constant(Constant::Number(3.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::Shl);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(8.0));
}

#[test]
fn test_jit_shr() {
    // -8 >> 1 → -4
    let mut chunk = Chunk::new("shr");
    let a = chunk.add_constant(Constant::Number(-8.0)).unwrap();
    let b = chunk.add_constant(Constant::Number(1.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::Shr);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(-4.0));
}

#[test]
fn test_jit_ushr() {
    // -1 >>> 0 → 4294967295 (0xFFFFFFFF)
    let mut chunk = Chunk::new("ushr");
    let a = chunk.add_constant(Constant::Number(-1.0)).unwrap();
    let b = chunk.add_constant(Constant::Number(0.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::UShr);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(4294967295.0));
}

// =========================================================================
// Inline PostfixInc/Dec tests
// =========================================================================

#[test]
fn test_jit_postfix_inc() {
    // PostfixIncrement: push old, push old+1
    // Stack: old(=5), new(=6) → return new (TOS)
    let mut chunk = Chunk::new("postinc");
    let a = chunk.add_constant(Constant::Number(5.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::PostfixIncrement);
    chunk.emit(Instruction::Return); // returns TOS = new = 6

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(6.0));
}

#[test]
fn test_jit_postfix_inc_old_value() {
    // PostfixIncrement: push old, push new → pop new, return old
    let mut chunk = Chunk::new("postinc_old");
    let a = chunk.add_constant(Constant::Number(5.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::PostfixIncrement);
    chunk.emit(Instruction::Pop); // pop new (6)
    chunk.emit(Instruction::Return); // return old (5)

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(5.0));
}

#[test]
fn test_jit_postfix_dec() {
    let mut chunk = Chunk::new("postdec");
    let a = chunk.add_constant(Constant::Number(5.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::PostfixDecrement);
    chunk.emit(Instruction::Return); // returns TOS = new = 4

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(4.0));
}

// =========================================================================
// Inline Exp test
// =========================================================================

#[test]
fn test_jit_exp() {
    // 2 ** 10 → 1024
    let mut chunk = Chunk::new("exp");
    let a = chunk.add_constant(Constant::Number(2.0)).unwrap();
    let b = chunk.add_constant(Constant::Number(10.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::Exp);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(1024.0));
}

// =========================================================================
// Inline Add number fast path test
// =========================================================================

#[test]
fn test_jit_add_numbers_inline() {
    let mut chunk = Chunk::new("add_num");
    let a = chunk.add_constant(Constant::Number(1.5)).unwrap();
    let b = chunk.add_constant(Constant::Number(2.5)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::Add);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(4.0));
}

// =========================================================================
// Inline Mod number test
// =========================================================================

#[test]
fn test_jit_mod_inline() {
    // 7 % 3 → 1
    let mut chunk = Chunk::new("mod_inline");
    let a = chunk.add_constant(Constant::Number(7.0)).unwrap();
    let b = chunk.add_constant(Constant::Number(3.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::Mod);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(1.0));
}

// =========================================================================
// Le, Gt comparison tests
// =========================================================================

#[test]
fn test_jit_le() {
    let mut chunk = Chunk::new("le");
    chunk.param_count = 2;
    chunk.local_count = 2;
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::LoadLocal(1));
    chunk.emit(Instruction::Le);
    chunk.emit(Instruction::Return);

    // 3 <= 5 → true
    let result = jit_execute_via_vm(chunk.clone(), &[JsValue::Number(3.0), JsValue::Number(5.0)]);
    assert_eq!(result, JsValue::Boolean(true));

    // 5 <= 5 → true
    let result = jit_execute_via_vm(chunk.clone(), &[JsValue::Number(5.0), JsValue::Number(5.0)]);
    assert_eq!(result, JsValue::Boolean(true));

    // 6 <= 5 → false
    let result = jit_execute_via_vm(chunk, &[JsValue::Number(6.0), JsValue::Number(5.0)]);
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_jit_gt() {
    let mut chunk = Chunk::new("gt");
    chunk.param_count = 2;
    chunk.local_count = 2;
    chunk.emit(Instruction::LoadLocal(0));
    chunk.emit(Instruction::LoadLocal(1));
    chunk.emit(Instruction::Gt);
    chunk.emit(Instruction::Return);

    // 5 > 3 → true
    let result = jit_execute_via_vm(chunk.clone(), &[JsValue::Number(5.0), JsValue::Number(3.0)]);
    assert_eq!(result, JsValue::Boolean(true));

    // 3 > 5 → false
    let result = jit_execute_via_vm(chunk, &[JsValue::Number(3.0), JsValue::Number(5.0)]);
    assert_eq!(result, JsValue::Boolean(false));
}

// =========================================================================
// Phase A: Pop/Dup/Void/Delete with heap types (String, Object)
// =========================================================================

#[test]
fn test_jit_pop_string() {
    // Push a number, push a string via LoadConst, pop the string, return the number
    let mut chunk = Chunk::new("pop_str");
    let a = chunk.add_constant(Constant::Number(42.0)).unwrap();
    let b = chunk
        .add_constant(Constant::String("hello".to_string()))
        .unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::Pop); // pop string (inline Rc::drop)
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_jit_pop_object() {
    // Push a number, create object, pop object, return number
    let mut chunk = Chunk::new("pop_obj");
    let a = chunk.add_constant(Constant::Number(99.0)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::CreateObject);
    chunk.emit(Instruction::Pop); // pop object (inline Rc::drop)
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Number(99.0));
}

#[test]
fn test_jit_dup_string() {
    // Push a string, dup it, pop one, verify the other is still correct
    let mut chunk = Chunk::new("dup_str");
    let a = chunk
        .add_constant(Constant::String("hello".to_string()))
        .unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::Dup); // dup string (inline Rc::clone)
    chunk.emit(Instruction::Pop); // pop one copy
    chunk.emit(Instruction::Return); // return the other

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::string("hello"));
}

#[test]
fn test_jit_dup_object() {
    // Create object, dup, pop one, return the other
    let mut chunk = Chunk::new("dup_obj");
    chunk.emit(Instruction::CreateObject);
    chunk.emit(Instruction::Dup); // dup object (inline Rc::clone)
    chunk.emit(Instruction::Pop); // pop one copy
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert!(result.is_object());
}

#[test]
fn test_jit_void_string() {
    // void "hello" → undefined
    let mut chunk = Chunk::new("void_str");
    let a = chunk
        .add_constant(Constant::String("hello".to_string()))
        .unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::Void); // Rc::drop + overwrite with Undefined
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Undefined);
}

#[test]
fn test_jit_void_object() {
    // void {} → undefined
    let mut chunk = Chunk::new("void_obj");
    chunk.emit(Instruction::CreateObject);
    chunk.emit(Instruction::Void); // Rc::drop + overwrite with Undefined
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Undefined);
}

#[test]
fn test_jit_delete_string() {
    // delete "hello" → true
    let mut chunk = Chunk::new("del_str");
    let a = chunk
        .add_constant(Constant::String("hello".to_string()))
        .unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::Delete); // Rc::drop + overwrite with Boolean(true)
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_delete_object() {
    // delete {} → true
    let mut chunk = Chunk::new("del_obj");
    chunk.emit(Instruction::CreateObject);
    chunk.emit(Instruction::Delete);
    chunk.emit(Instruction::Return);

    let result = jit_execute_via_vm(chunk, &[]);
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_jit_rc_layout_probe() {
    // Verify the Rc layout probing works
    use super::aarch64::layout;
    let strong_off = layout::rc_strong_offset();
    let back_off = layout::rc_str_data_back_offset();
    eprintln!(
        "Rc probe: strong_offset={}, str_data_back_offset={}",
        strong_off, back_off
    );
    // strong_count should be at offset 0 (standard Rust Rc layout)
    assert_eq!(strong_off, 0);
    // data_back_offset should be > 0 (header before data)
    assert!(back_off > 0);
}
