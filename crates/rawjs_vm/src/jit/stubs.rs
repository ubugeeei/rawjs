//! Stub functions for the stub-based baseline JIT.
//!
//! Each function is `extern "C"` and operates directly on the VM's
//! value_stack and call frame locals.  Return value convention:
//!   - 0 = success
//!   - 1 = error (details in `vm.jit_error`)
//!   - 2 = frame done (Return instruction completed this JIT frame)

use crate::Vm;
use rawjs_common::RawJsError;
use rawjs_runtime::JsValue;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

#[inline]
fn vm_ref<'a>(vm: *mut Vm) -> &'a mut Vm {
    unsafe { &mut *vm }
}

#[inline]
fn set_error(vm: *mut Vm, err: RawJsError) -> u32 {
    let vm = vm_ref(vm);
    vm.jit_error = Some(err);
    1
}

// ---------------------------------------------------------------------------
// Phase 3: Stack operations & literals
// ---------------------------------------------------------------------------

#[no_mangle]
pub extern "C" fn stub_load_const(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let frame = vm.call_stack.last().unwrap();
    let chunk = &vm.chunks[frame.chunk_index];
    let value = super::super::interpreter::constant_to_value(&chunk.constants[idx as usize]);
    vm.push(value);
    0
}

#[no_mangle]
pub extern "C" fn stub_load_local(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let value = {
        let frame = vm.call_stack.last().unwrap();
        frame.locals[idx as usize].clone()
    };
    vm.push(value);
    0
}

#[no_mangle]
pub extern "C" fn stub_store_local(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    match vm.pop() {
        Ok(value) => {
            let frame = vm.call_stack.last_mut().unwrap();
            let slot = idx as usize;
            if slot >= frame.locals.len() {
                frame.locals.resize(slot + 1, JsValue::Undefined);
            }
            frame.locals[slot] = value;
            0
        }
        Err(e) => set_error(vm as *mut Vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_load_global(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = {
        let frame = vm.call_stack.last().unwrap();
        let chunk = &vm.chunks[frame.chunk_index];
        match &chunk.constants[idx as usize] {
            rawjs_bytecode::Constant::String(s) => s.clone(),
            _ => {
                return set_error(
                    vm as *mut Vm,
                    RawJsError::internal_error("expected string constant"),
                )
            }
        }
    };
    let value = vm.get_global(&name).cloned().unwrap_or(JsValue::Undefined);
    vm.push(value);
    0
}

#[no_mangle]
pub extern "C" fn stub_store_global(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = {
        let frame = vm.call_stack.last().unwrap();
        let chunk = &vm.chunks[frame.chunk_index];
        match &chunk.constants[idx as usize] {
            rawjs_bytecode::Constant::String(s) => s.clone(),
            _ => {
                return set_error(
                    vm as *mut Vm,
                    RawJsError::internal_error("expected string constant"),
                )
            }
        }
    };
    match vm.pop() {
        Ok(value) => {
            vm.set_global(name, value);
            0
        }
        Err(e) => set_error(vm as *mut Vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_load_upvalue(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let value = {
        let frame = vm.call_stack.last().unwrap();
        frame.upvalues[idx as usize].get()
    };
    vm.push(value);
    0
}

#[no_mangle]
pub extern "C" fn stub_store_upvalue(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    match vm.pop() {
        Ok(value) => {
            let frame = vm.call_stack.last().unwrap();
            frame.upvalues[idx as usize].set(value);
            0
        }
        Err(e) => set_error(vm as *mut Vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_pop(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match vm.pop() {
        Ok(_) => 0,
        Err(e) => set_error(vm as *mut Vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_dup(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match vm.peek() {
        Ok(v) => {
            let v = v.clone();
            vm.push(v);
            0
        }
        Err(e) => set_error(vm as *mut Vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_push_undefined(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    vm.push(JsValue::Undefined);
    0
}

#[no_mangle]
pub extern "C" fn stub_push_null(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    vm.push(JsValue::Null);
    0
}

#[no_mangle]
pub extern "C" fn stub_push_true(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    vm.push(JsValue::Boolean(true));
    0
}

#[no_mangle]
pub extern "C" fn stub_push_false(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    vm.push(JsValue::Boolean(false));
    0
}

#[no_mangle]
pub extern "C" fn stub_push_this(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let this_val = vm.call_stack.last().unwrap().this_value.clone();
    vm.push(this_val);
    0
}

// ---------------------------------------------------------------------------
// Phase 4: Arithmetic, comparison, bitwise, unary
// ---------------------------------------------------------------------------

#[no_mangle]
pub extern "C" fn stub_add(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let result = crate::interpreter::js_add(&lhs, &rhs);
    vm.push(result);
    0
}

#[no_mangle]
pub extern "C" fn stub_sub(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Number(lhs.to_number() - rhs.to_number()));
    0
}

#[no_mangle]
pub extern "C" fn stub_mul(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Number(lhs.to_number() * rhs.to_number()));
    0
}

#[no_mangle]
pub extern "C" fn stub_div(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Number(lhs.to_number() / rhs.to_number()));
    0
}

#[no_mangle]
pub extern "C" fn stub_modulo(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Number(lhs.to_number() % rhs.to_number()));
    0
}

#[no_mangle]
pub extern "C" fn stub_exp(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Number(lhs.to_number().powf(rhs.to_number())));
    0
}

#[no_mangle]
pub extern "C" fn stub_neg(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Number(-val.to_number()));
    0
}

#[no_mangle]
pub extern "C" fn stub_pos(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Number(val.to_number()));
    0
}

#[no_mangle]
pub extern "C" fn stub_not(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Boolean(!val.to_boolean()));
    0
}

#[no_mangle]
pub extern "C" fn stub_bit_not(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Number((!val.to_int32()) as f64));
    0
}

#[no_mangle]
pub extern "C" fn stub_typeof(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::string(val.type_of()));
    0
}

#[no_mangle]
pub extern "C" fn stub_void(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match vm.pop() {
        Ok(_) => {
            vm.push(JsValue::Undefined);
            0
        }
        Err(e) => set_error(vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_delete(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match vm.pop() {
        Ok(_) => {
            vm.push(JsValue::Boolean(true));
            0
        }
        Err(e) => set_error(vm, e),
    }
}

// Comparison stubs

#[no_mangle]
pub extern "C" fn stub_eq(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Boolean(lhs.abstract_eq(&rhs)));
    0
}

#[no_mangle]
pub extern "C" fn stub_strict_eq(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Boolean(lhs.strict_eq(&rhs)));
    0
}

#[no_mangle]
pub extern "C" fn stub_ne(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Boolean(!lhs.abstract_eq(&rhs)));
    0
}

#[no_mangle]
pub extern "C" fn stub_strict_ne(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Boolean(!lhs.strict_eq(&rhs)));
    0
}

#[no_mangle]
pub extern "C" fn stub_lt(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Boolean(lhs.to_number() < rhs.to_number()));
    0
}

#[no_mangle]
pub extern "C" fn stub_le(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Boolean(lhs.to_number() <= rhs.to_number()));
    0
}

#[no_mangle]
pub extern "C" fn stub_gt(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Boolean(lhs.to_number() > rhs.to_number()));
    0
}

#[no_mangle]
pub extern "C" fn stub_ge(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Boolean(lhs.to_number() >= rhs.to_number()));
    0
}

// Bitwise stubs

#[no_mangle]
pub extern "C" fn stub_bit_and(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Number((lhs.to_int32() & rhs.to_int32()) as f64));
    0
}

#[no_mangle]
pub extern "C" fn stub_bit_or(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Number((lhs.to_int32() | rhs.to_int32()) as f64));
    0
}

#[no_mangle]
pub extern "C" fn stub_bit_xor(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Number((lhs.to_int32() ^ rhs.to_int32()) as f64));
    0
}

#[no_mangle]
pub extern "C" fn stub_shl(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let shift = rhs.to_uint32() & 0x1f;
    vm.push(JsValue::Number((lhs.to_int32() << shift) as f64));
    0
}

#[no_mangle]
pub extern "C" fn stub_shr(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let shift = rhs.to_uint32() & 0x1f;
    vm.push(JsValue::Number((lhs.to_int32() >> shift) as f64));
    0
}

#[no_mangle]
pub extern "C" fn stub_ushr(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let shift = rhs.to_uint32() & 0x1f;
    vm.push(JsValue::Number((lhs.to_uint32() >> shift) as f64));
    0
}

// Relational stubs

#[no_mangle]
pub extern "C" fn stub_in(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let obj_val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let key = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let key_str = key.to_js_string();
    let result = match &obj_val {
        JsValue::Object(ptr) => ptr.borrow().has_own_property(&key_str),
        _ => false,
    };
    vm.push(JsValue::Boolean(result));
    0
}

#[no_mangle]
pub extern "C" fn stub_instanceof(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let _rhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let _lhs = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(JsValue::Boolean(false));
    0
}

// Postfix stubs

#[no_mangle]
pub extern "C" fn stub_postfix_inc(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let n = val.to_number();
    vm.push(JsValue::Number(n));
    vm.push(JsValue::Number(n + 1.0));
    0
}

#[no_mangle]
pub extern "C" fn stub_postfix_dec(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let n = val.to_number();
    vm.push(JsValue::Number(n));
    vm.push(JsValue::Number(n - 1.0));
    0
}

// ---------------------------------------------------------------------------
// Phase 5: Object & array stubs
// ---------------------------------------------------------------------------

#[no_mangle]
pub extern "C" fn stub_create_object(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let obj = rawjs_runtime::JsObject::ordinary();
    let ptr = vm.heap.alloc(obj);
    vm.push(JsValue::Object(ptr));
    0
}

#[no_mangle]
pub extern "C" fn stub_create_array(vm: *mut Vm, count: u32) -> u32 {
    let vm = vm_ref(vm);
    let count = count as usize;
    let stack_len = vm.value_stack.len();
    if count > stack_len {
        return set_error(
            vm,
            RawJsError::internal_error("stack underflow in CreateArray"),
        );
    }
    let elements: Vec<JsValue> = vm.value_stack.drain(stack_len - count..).collect();
    let obj = rawjs_runtime::JsObject::array(elements);
    let ptr = vm.heap.alloc(obj);
    vm.push(JsValue::Object(ptr));
    0
}

#[no_mangle]
pub extern "C" fn stub_get_property(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = {
        let frame = vm.call_stack.last().unwrap();
        let chunk = &vm.chunks[frame.chunk_index];
        match &chunk.constants[idx as usize] {
            rawjs_bytecode::Constant::String(s) => s.clone(),
            _ => return set_error(vm, RawJsError::internal_error("expected string constant")),
        }
    };
    let obj_val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    match crate::interpreter::get_property_value(vm, &obj_val, &name) {
        Ok(result) => {
            vm.push(result);
            0
        }
        Err(e) => set_error(vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_set_property(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = {
        let frame = vm.call_stack.last().unwrap();
        let chunk = &vm.chunks[frame.chunk_index];
        match &chunk.constants[idx as usize] {
            rawjs_bytecode::Constant::String(s) => s.clone(),
            _ => return set_error(vm, RawJsError::internal_error("expected string constant")),
        }
    };
    let value = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let obj_val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    match crate::interpreter::set_property_value(&obj_val, &name, &value) {
        Ok(()) => {
            vm.push(value);
            0
        }
        Err(e) => set_error(vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_get_index(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let index = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let obj_val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let key = index.to_js_string();
    match crate::interpreter::get_property_value(vm, &obj_val, &key) {
        Ok(result) => {
            vm.push(result);
            0
        }
        Err(e) => set_error(vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_set_index(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let value = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let index = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let obj_val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let key = index.to_js_string();
    match crate::interpreter::set_property_value(&obj_val, &key, &value) {
        Ok(()) => {
            vm.push(value);
            0
        }
        Err(e) => set_error(vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_get_computed(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let key = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let obj_val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let key_str = key.to_js_string();
    match crate::interpreter::get_property_value(vm, &obj_val, &key_str) {
        Ok(result) => {
            vm.push(result);
            0
        }
        Err(e) => set_error(vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_set_computed(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let value = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let key = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let obj_val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let key_str = key.to_js_string();
    match crate::interpreter::set_property_value(&obj_val, &key_str, &value) {
        Ok(()) => {
            vm.push(value);
            0
        }
        Err(e) => set_error(vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_create_closure(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    match crate::interpreter::exec_create_closure(vm, idx as u16) {
        Ok(()) => 0,
        Err(e) => set_error(vm, e),
    }
}

// ---------------------------------------------------------------------------
// Phase 6: Function call stubs
// ---------------------------------------------------------------------------

#[no_mangle]
pub extern "C" fn stub_call(vm: *mut Vm, argc: u32) -> u32 {
    let vm = vm_ref(vm);
    let target_depth = vm.call_stack.len();
    match crate::interpreter::exec_call(vm, argc as usize, JsValue::Undefined) {
        Ok(()) => {
            // If exec_call pushed a new frame (bytecode function), run it to completion.
            if vm.call_stack.len() > target_depth {
                match crate::interpreter::run_inner_frame(vm, target_depth) {
                    Ok(()) => 0,
                    Err(e) => set_error(vm, e),
                }
            } else {
                // Native function already returned — result is on the stack.
                0
            }
        }
        Err(e) => set_error(vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_call_method(vm: *mut Vm, argc: u32) -> u32 {
    let vm = vm_ref(vm);
    let argc = argc as usize;
    let stack_len = vm.value_stack.len();
    if stack_len < argc + 2 {
        return set_error(
            vm,
            RawJsError::internal_error("stack underflow in CallMethod"),
        );
    }
    let args: Vec<JsValue> = vm.value_stack.drain(stack_len - argc..).collect();
    let method = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let receiver = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.push(method);
    for arg in args {
        vm.push(arg);
    }
    let target_depth = vm.call_stack.len();
    match crate::interpreter::exec_call(vm, argc, receiver) {
        Ok(()) => {
            if vm.call_stack.len() > target_depth {
                match crate::interpreter::run_inner_frame(vm, target_depth) {
                    Ok(()) => 0,
                    Err(e) => set_error(vm, e),
                }
            } else {
                0
            }
        }
        Err(e) => set_error(vm, e),
    }
}

/// Return stub: pops the JIT frame's return value and frame.
/// Returns 2 to signal "this JIT frame is done".
#[no_mangle]
pub extern "C" fn stub_return(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let return_value = if vm.value_stack.len() > vm.call_stack.last().unwrap().base {
        match vm.pop() {
            Ok(v) => v,
            Err(_) => JsValue::Undefined,
        }
    } else {
        JsValue::Undefined
    };

    let frame = vm.call_stack.pop().unwrap();
    vm.value_stack.truncate(frame.base);

    if !vm.call_stack.is_empty() {
        vm.push(return_value);
    }
    // Return 2 = "frame done"
    2
}

// ---------------------------------------------------------------------------
// Jump helper stub
// ---------------------------------------------------------------------------

/// Pop the top of stack and return 0 for falsy, 1 for truthy.
/// This is used by JumpIfFalse / JumpIfTrue in the JIT.
#[no_mangle]
pub extern "C" fn stub_pop_and_test_truthy(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match vm.pop() {
        Ok(val) => {
            if val.to_boolean() {
                1
            } else {
                0
            }
        }
        Err(_) => 0, // treat underflow as falsy
    }
}

// ---------------------------------------------------------------------------
// Phase 7: Iteration & ESM stubs
// ---------------------------------------------------------------------------

#[no_mangle]
pub extern "C" fn stub_get_iterator(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let obj_val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };

    let values: Vec<JsValue> = match &obj_val {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            match &obj.internal {
                rawjs_runtime::ObjectInternal::Array(elements) => elements.clone(),
                rawjs_runtime::ObjectInternal::Set(set_values) => set_values.clone(),
                rawjs_runtime::ObjectInternal::Map(entries) => entries
                    .iter()
                    .map(|(k, v)| {
                        let pair = vec![k.clone(), v.clone()];
                        JsValue::Object(rawjs_runtime::GcPtr::new(rawjs_runtime::JsObject::array(
                            pair,
                        )))
                    })
                    .collect(),
                _ => {
                    return set_error(
                        vm,
                        RawJsError::type_error(format!("{} is not iterable", obj_val.type_of())),
                    );
                }
            }
        }
        JsValue::String(s) => s
            .chars()
            .map(|c| JsValue::string(c.to_string().as_str()))
            .collect(),
        _ => {
            return set_error(
                vm,
                RawJsError::type_error(format!("{} is not iterable", obj_val.type_of())),
            );
        }
    };
    let iter_obj = rawjs_runtime::JsObject::iterator(values);
    let ptr = vm.heap.alloc(iter_obj);
    vm.push(JsValue::Object(ptr));
    0
}

#[no_mangle]
pub extern "C" fn stub_iterator_next(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let iter_val = match vm.peek() {
        Ok(v) => v.clone(),
        Err(e) => return set_error(vm, e),
    };
    match &iter_val {
        JsValue::Object(ptr) => {
            let mut obj = ptr.borrow_mut();
            match &mut obj.internal {
                rawjs_runtime::ObjectInternal::Iterator(state) => {
                    let (value, done) = state.advance();
                    drop(obj);
                    vm.push(value);
                    vm.push(JsValue::Boolean(done));
                    0
                }
                _ => set_error(vm, RawJsError::type_error("not an iterator")),
            }
        }
        _ => set_error(vm, RawJsError::type_error("not an iterator")),
    }
}

/// Check if iterator is done: pop `done`, if true pop value & adjust IP.
/// Returns: 0 = not done, 1 = done (JIT should jump), 0x80000000 = error.
#[no_mangle]
pub extern "C" fn stub_iterator_done_check(vm: *mut Vm, offset: i32) -> u32 {
    let vm = vm_ref(vm);
    let done = match vm.pop() {
        Ok(v) => v,
        Err(_) => return 0x80000000,
    };
    if done.to_boolean() {
        let _ = vm.pop(); // pop value (undefined)
                          // Adjust IP for the jump
        let frame = vm.call_stack.last_mut().unwrap();
        frame.ip = ((frame.ip as i64) + (offset as i64)) as usize;
        1 // done
    } else {
        0 // not done
    }
}

#[no_mangle]
pub extern "C" fn stub_for_in_init(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let obj_val = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let keys: Vec<JsValue> = match &obj_val {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            obj.own_enumerable_keys()
                .into_iter()
                .map(|k| JsValue::string(k.as_str()))
                .collect()
        }
        _ => Vec::new(),
    };
    let arr_obj = rawjs_runtime::JsObject::array(keys);
    let arr_ptr = vm.heap.alloc(arr_obj);
    vm.push(JsValue::Object(arr_ptr));
    vm.push(JsValue::Number(0.0));
    0
}

/// ForInNext: returns 0=more, 1=done (JIT should jump), 0x80000000=error.
#[no_mangle]
pub extern "C" fn stub_for_in_next(vm: *mut Vm, offset: i32) -> u32 {
    let vm = vm_ref(vm);
    let index_val = match vm.pop() {
        Ok(v) => v,
        Err(_) => return 0x80000000,
    };
    let index = index_val.to_number() as usize;
    let keys_val = match vm.peek() {
        Ok(v) => v.clone(),
        Err(_) => return 0x80000000,
    };

    let len = match &keys_val {
        JsValue::Object(ptr) => ptr.borrow().array_length(),
        _ => 0,
    };

    if index >= len {
        vm.push(index_val);
        let frame = vm.call_stack.last_mut().unwrap();
        frame.ip = ((frame.ip as i64) + (offset as i64)) as usize;
        1 // done
    } else {
        let key = match &keys_val {
            JsValue::Object(ptr) => ptr.borrow().get_element(index),
            _ => JsValue::Undefined,
        };
        vm.push(JsValue::Number((index + 1) as f64));
        vm.push(key);
        0 // more
    }
}

// ESM stubs

#[no_mangle]
pub extern "C" fn stub_import_module(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let source = {
        let frame = vm.call_stack.last().unwrap();
        let chunk = &vm.chunks[frame.chunk_index];
        match &chunk.constants[idx as usize] {
            rawjs_bytecode::Constant::String(s) => s.clone(),
            _ => return set_error(vm, RawJsError::internal_error("expected string constant")),
        }
    };
    match vm.execute_module(&source) {
        Ok(ns) => {
            vm.push(JsValue::Object(ns));
            0
        }
        Err(e) => set_error(vm, e),
    }
}

#[no_mangle]
pub extern "C" fn stub_import_binding(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = {
        let frame = vm.call_stack.last().unwrap();
        let chunk = &vm.chunks[frame.chunk_index];
        match &chunk.constants[idx as usize] {
            rawjs_bytecode::Constant::String(s) => s.clone(),
            _ => return set_error(vm, RawJsError::internal_error("expected string constant")),
        }
    };
    let ns = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    let value = match &ns {
        JsValue::Object(ptr) => ptr.borrow().get_property(&name),
        _ => JsValue::Undefined,
    };
    vm.push(value);
    0
}

#[no_mangle]
pub extern "C" fn stub_export_binding(vm: *mut Vm, idx: u32) -> u32 {
    let vm = vm_ref(vm);
    let name = {
        let frame = vm.call_stack.last().unwrap();
        let chunk = &vm.chunks[frame.chunk_index];
        match &chunk.constants[idx as usize] {
            rawjs_bytecode::Constant::String(s) => s.clone(),
            _ => return set_error(vm, RawJsError::internal_error("expected string constant")),
        }
    };
    let value = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    if let Some(ref exports) = vm.module_exports.clone() {
        exports.borrow_mut().set_property(name, value);
    }
    0
}

#[no_mangle]
pub extern "C" fn stub_export_default(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let value = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    if let Some(ref exports) = vm.module_exports.clone() {
        exports
            .borrow_mut()
            .set_property("default".to_string(), value);
    }
    0
}

// ---------------------------------------------------------------------------
// Phase 8: Exception handling stubs
// ---------------------------------------------------------------------------

#[no_mangle]
pub extern "C" fn stub_throw(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    let value = match vm.pop() {
        Ok(v) => v,
        Err(e) => return set_error(vm, e),
    };
    vm.thrown_value = Some(value.clone());
    let msg = format!("{}", value);
    set_error(vm, RawJsError::internal_error(msg))
}

#[no_mangle]
pub extern "C" fn stub_enter_try(vm: *mut Vm, catch_offset: i32, finally_offset: i32) -> u32 {
    let vm = vm_ref(vm);
    let frame = vm.call_stack.last().unwrap();
    let current_ip = frame.ip;

    let catch_ip = if catch_offset != 0 {
        Some(((current_ip as i64) + (catch_offset as i64)) as usize)
    } else {
        None
    };
    let finally_ip = if finally_offset != 0 {
        Some(((current_ip as i64) + (finally_offset as i64)) as usize)
    } else {
        None
    };

    vm.try_stack.push(crate::TryContext {
        catch_ip,
        finally_ip,
        stack_depth: vm.value_stack.len(),
        call_depth: vm.call_stack.len(),
    });
    0
}

#[no_mangle]
pub extern "C" fn stub_leave_try(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    vm.try_stack.pop();
    0
}

// ---------------------------------------------------------------------------
// Phase 9: Generator / async / dispose stubs
// ---------------------------------------------------------------------------

/// CreateGenerator should not be executed directly; generator creation
/// is handled in exec_call. If reached, it's an internal error.
#[no_mangle]
pub extern "C" fn stub_create_generator(vm: *mut Vm) -> u32 {
    set_error(
        vm,
        RawJsError::internal_error("CreateGenerator should not be executed directly"),
    )
}

/// Yield: saves the generator frame and returns to the caller.
/// Returns 2 (frame done) because exec_yield pops the current frame.
#[no_mangle]
pub extern "C" fn stub_yield(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match crate::interpreter::exec_yield(vm) {
        Ok(_) => 2, // frame done
        Err(e) => set_error(vm as *mut Vm, e),
    }
}

/// Await: should only be reached inside an async frame (run_async_frame).
/// In the normal execution path, this is an error.
/// Returns 2 (frame done) if it ever does something useful.
#[no_mangle]
pub extern "C" fn stub_await(vm: *mut Vm) -> u32 {
    let vm = vm_ref(vm);
    match crate::interpreter::exec_await(vm) {
        Ok(_) => 2, // frame done
        Err(e) => set_error(vm as *mut Vm, e),
    }
}

/// DisposeResource: calls [Symbol.dispose]() on the local at the given slot.
/// Null/undefined locals are silently skipped.
#[no_mangle]
pub extern "C" fn stub_dispose_resource(vm: *mut Vm, slot: u32) -> u32 {
    let vm = vm_ref(vm);
    match crate::interpreter::exec_dispose_resource(vm, slot as u16) {
        Ok(()) => 0,
        Err(e) => set_error(vm as *mut Vm, e),
    }
}
