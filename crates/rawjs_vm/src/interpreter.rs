//! Bytecode interpreter for the rawjs VM.
//!
//! This module implements the main dispatch loop that fetches, decodes, and
//! executes bytecode instructions one at a time.  When a function's
//! invocation count crosses the JIT threshold the interpreter will attempt to
//! compile it to native code and call the JIT version on subsequent
//! invocations.

use rawjs_bytecode::{Chunk, Constant, Instruction};
use rawjs_common::{RawJsError, Result};
use rawjs_runtime::{
    GeneratorState, GeneratorStatus, Heap, JsObject, JsValue, ObjectInternal, Property, Upvalue,
};

use crate::{CallFrame, TryContext, Vm, JIT_THRESHOLD};

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the interpreter loop until the top-level frame returns or an
/// unhandled exception occurs.
pub fn run(vm: &mut Vm) -> Result<JsValue> {
    let base_depth = vm.call_stack.len().saturating_sub(1);

    loop {
        // Safety: the call stack must never be empty while we are running.
        if vm.call_stack.is_empty() {
            drain_microtasks(vm)?;
            return Ok(JsValue::Undefined);
        }

        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;

        // Bounds check -- if we've run past the end of the chunk, treat as
        // implicit return of `undefined`.
        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            // Pop frame and push undefined as implicit return.
            let frame = vm.call_stack.pop().unwrap();
            vm.value_stack.truncate(frame.base);
            if vm.call_stack.is_empty() {
                drain_microtasks(vm)?;
                return Ok(JsValue::Undefined);
            }
            vm.push(JsValue::Undefined);

            // Drain microtasks when returning to base frame
            if vm.call_stack.len() <= base_depth + 1 {
                drain_microtasks(vm)?;
            }
            continue;
        }

        // Fetch the instruction (copy -- `Instruction` is Copy).
        let instruction = vm.chunks[chunk_index].instructions[ip];

        // Advance IP *before* executing so that jump offsets are relative
        // to the *next* instruction (as documented).
        vm.call_stack[frame_idx].ip = ip + 1;

        match execute_instruction(vm, instruction) {
            Ok(Some(value)) => {
                // A top-level return -- we are done.
                drain_microtasks(vm)?;
                return Ok(value);
            }
            Ok(None) => {
                // Normal execution -- continue loop.
            }
            Err(err) => {
                // Try to unwind into a catch handler.
                if !unwind_exception(vm, &err)? {
                    return Err(err);
                }
                // If unwinding succeeded, the thrown value is on the stack
                // and IP has been adjusted -- continue the loop.
            }
        }
    }
}

/// Run the interpreter loop for a module frame only.
///
/// This is similar to `run()` but it returns as soon as the module's
/// frame is done (i.e., the call stack returns to the depth it was at
/// before the module frame was pushed).  This prevents the interpreter
/// from accidentally continuing execution in the caller's frame.
pub fn run_module_frame(vm: &mut Vm) -> Result<()> {
    // The module frame is the one we just pushed.  We want to run until
    // the call stack shrinks below this level.
    let target_depth = vm.call_stack.len() - 1;

    loop {
        if vm.call_stack.len() <= target_depth {
            // Module frame has returned — done.
            return Ok(());
        }

        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;

        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            let frame = vm.call_stack.pop().unwrap();
            vm.value_stack.truncate(frame.base);
            if vm.call_stack.len() <= target_depth {
                return Ok(());
            }
            vm.push(JsValue::Undefined);
            continue;
        }

        let instruction = vm.chunks[chunk_index].instructions[ip];
        vm.call_stack[frame_idx].ip = ip + 1;

        match execute_instruction(vm, instruction) {
            Ok(Some(_value)) => {
                // The module's top-level frame returned.  Since exec_return
                // already popped the frame and we may have reached
                // target_depth, check and return.
                if vm.call_stack.len() <= target_depth {
                    return Ok(());
                }
                // Otherwise some nested call returned to top level
                // which shouldn't happen, but continue gracefully.
            }
            Ok(None) => {}
            Err(err) => {
                if !unwind_exception(vm, &err)? {
                    return Err(err);
                }
            }
        }
    }
}

/// Run the interpreter until the call stack returns to `target_depth`.
///
/// This is a generalized version of `run_module_frame` used by the JIT
/// when a stub (e.g. `stub_call`) needs the interpreter to execute a
/// pushed call frame to completion.
pub fn run_inner_frame(vm: &mut Vm, target_depth: usize) -> Result<()> {
    loop {
        if vm.call_stack.len() <= target_depth {
            return Ok(());
        }

        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;

        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            let frame = vm.call_stack.pop().unwrap();
            vm.value_stack.truncate(frame.base);
            if vm.call_stack.len() <= target_depth {
                return Ok(());
            }
            vm.push(JsValue::Undefined);
            continue;
        }

        let instruction = vm.chunks[chunk_index].instructions[ip];
        vm.call_stack[frame_idx].ip = ip + 1;

        match execute_instruction(vm, instruction) {
            Ok(Some(_value)) => {
                if vm.call_stack.len() <= target_depth {
                    return Ok(());
                }
            }
            Ok(None) => {}
            Err(err) => {
                if !unwind_exception(vm, &err)? {
                    return Err(err);
                }
            }
        }
    }
}

/// Drain all pending microtasks (Promise reactions).
fn drain_microtasks(vm: &mut Vm) -> Result<()> {
    loop {
        let tasks: Vec<_> = vm.heap.pending_microtasks.drain(..).collect();
        if tasks.is_empty() {
            break;
        }
        for task in tasks {
            // Call the callback with the argument
            if let JsValue::Object(fn_ptr) = &task.callback {
                let func = {
                    let obj = fn_ptr.borrow();
                    match &obj.internal {
                        ObjectInternal::Function(f) => Some(f.clone()),
                        _ => None,
                    }
                };
                if let Some(func) = func {
                    // Check if this is an async resume callback.
                    let async_gen_ptr = {
                        let fn_obj = fn_ptr.borrow();
                        let gen_val = fn_obj.get_property("__async_gen_ptr__");
                        match gen_val {
                            JsValue::Object(ptr) => Some(ptr),
                            _ => None,
                        }
                    };

                    if let Some(gen_ptr) = async_gen_ptr {
                        // This is an async resume callback — resume directly.
                        let is_throw = func.name == "asyncReject";
                        let _ = async_resume(vm, &gen_ptr, task.arg, is_throw);
                        continue;
                    }

                    match &func.kind {
                        rawjs_runtime::FunctionKind::Native(native_fn) => {
                            let native_fn = *native_fn;
                            vm.heap.calling_fn = Some(fn_ptr.clone());
                            let result = native_fn(&mut vm.heap, &JsValue::Undefined, &[task.arg])?;
                            vm.heap.calling_fn = None;
                            let _ = result;
                        }
                        rawjs_runtime::FunctionKind::Bytecode { chunk_index } => {
                            let chunk_index = *chunk_index;
                            let param_count = vm.chunks[chunk_index].param_count as usize;
                            let local_count = vm.chunks[chunk_index].local_count as usize;
                            let slot_count = local_count.max(param_count);

                            let mut locals = vec![JsValue::Undefined; slot_count];
                            if !locals.is_empty() {
                                locals[0] = task.arg.clone();
                            }

                            let frame = CallFrame {
                                chunk_index,
                                ip: 0,
                                base: vm.value_stack.len(),
                                locals,
                                upvalues: func.upvalues.clone(),
                                this_value: JsValue::Undefined,
                            };
                            vm.call_stack.push(frame);

                            let result = run_microtask_frame(vm)?;
                            let _ = result;
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

/// Run a Promise executor frame to completion.
/// Unlike run_microtask_frame, errors from the executor are caught (not unwound)
/// so the caller can reject the promise.
fn run_executor_frame(vm: &mut Vm, target_depth: usize) -> Result<JsValue> {
    loop {
        if vm.call_stack.is_empty() || vm.call_stack.len() <= target_depth {
            return Ok(JsValue::Undefined);
        }

        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;

        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            let frame = vm.call_stack.pop().unwrap();
            vm.value_stack.truncate(frame.base);
            if vm.call_stack.len() <= target_depth {
                return Ok(JsValue::Undefined);
            }
            vm.push(JsValue::Undefined);
            continue;
        }

        let instruction = vm.chunks[chunk_index].instructions[ip];
        vm.call_stack[frame_idx].ip = ip + 1;

        match execute_instruction(vm, instruction) {
            Ok(Some(value)) => return Ok(value),
            Ok(None) => {}
            Err(err) => {
                // Try to unwind within the executor's own try/catch
                if unwind_exception(vm, &err)? {
                    // Handler found within executor — continue
                    continue;
                }
                // No handler — propagate to caller for promise rejection
                return Err(err);
            }
        }

        if vm.call_stack.len() <= target_depth {
            return Ok(JsValue::Undefined);
        }
    }
}

/// Run a single microtask frame to completion.
fn run_microtask_frame(vm: &mut Vm) -> Result<JsValue> {
    let target_depth = vm.call_stack.len() - 1;

    loop {
        if vm.call_stack.is_empty() || vm.call_stack.len() <= target_depth {
            return Ok(JsValue::Undefined);
        }

        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;

        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            let frame = vm.call_stack.pop().unwrap();
            vm.value_stack.truncate(frame.base);
            if vm.call_stack.len() <= target_depth {
                return Ok(JsValue::Undefined);
            }
            vm.push(JsValue::Undefined);
            continue;
        }

        let instruction = vm.chunks[chunk_index].instructions[ip];
        vm.call_stack[frame_idx].ip = ip + 1;

        match execute_instruction(vm, instruction) {
            Ok(Some(value)) => {
                // Return from the microtask
                return Ok(value);
            }
            Ok(None) => {}
            Err(err) => {
                if !unwind_exception(vm, &err)? {
                    return Err(err);
                }
            }
        }

        // If we've returned to the target depth, the microtask is done
        if vm.call_stack.len() <= target_depth {
            return Ok(JsValue::Undefined);
        }
    }
}

// ---------------------------------------------------------------------------
// Instruction dispatch
// ---------------------------------------------------------------------------

/// Execute a single instruction.
///
/// Returns:
///   - `Ok(Some(value))` when the *top-level* frame executes a `Return`,
///     signalling the end of the program.
///   - `Ok(None)` for normal continuation.
///   - `Err(e)` for runtime errors (type errors, reference errors, etc.).
fn execute_instruction(vm: &mut Vm, instr: Instruction) -> Result<Option<JsValue>> {
    match instr {
        // =================================================================
        // Constants & literals
        // =================================================================
        Instruction::LoadConst(idx) => {
            let frame = vm.call_stack.last().unwrap();
            let chunk = &vm.chunks[frame.chunk_index];
            let value = constant_to_value(&chunk.constants[idx as usize]);
            vm.push(value);
        }

        Instruction::Undefined => vm.push(JsValue::Undefined),
        Instruction::Null => vm.push(JsValue::Null),
        Instruction::True => vm.push(JsValue::Boolean(true)),
        Instruction::False => vm.push(JsValue::Boolean(false)),
        Instruction::This => {
            let this_val = vm.call_stack.last().unwrap().this_value.clone();
            vm.push(this_val);
        }

        // =================================================================
        // Local variables
        // =================================================================
        Instruction::LoadLocal(idx) => {
            let value = {
                let frame = vm.call_stack.last().unwrap();
                frame.locals[idx as usize].clone()
            };
            vm.push(value);
        }
        Instruction::StoreLocal(idx) => {
            let value = vm.pop()?;
            let frame = vm.call_stack.last_mut().unwrap();
            let slot = idx as usize;
            if slot >= frame.locals.len() {
                frame.locals.resize(slot + 1, JsValue::Undefined);
            }
            frame.locals[slot] = value;
        }

        // =================================================================
        // Global variables
        // =================================================================
        Instruction::LoadGlobal(name_idx) => {
            let name = {
                let frame = vm.call_stack.last().unwrap();
                get_constant_string(&vm.chunks[frame.chunk_index], name_idx)?
            };
            let value = vm.get_global(&name).cloned().unwrap_or(JsValue::Undefined);
            vm.push(value);
        }
        Instruction::StoreGlobal(name_idx) => {
            let name = {
                let frame = vm.call_stack.last().unwrap();
                get_constant_string(&vm.chunks[frame.chunk_index], name_idx)?
            };
            let value = vm.pop()?;
            vm.set_global(name, value);
        }

        // =================================================================
        // Upvalues
        // =================================================================
        Instruction::LoadUpvalue(idx) => {
            let value = {
                let frame = vm.call_stack.last().unwrap();
                frame.upvalues[idx as usize].get()
            };
            vm.push(value);
        }
        Instruction::StoreUpvalue(idx) => {
            let value = vm.pop()?;
            let frame = vm.call_stack.last().unwrap();
            frame.upvalues[idx as usize].set(value);
        }

        // =================================================================
        // Stack manipulation
        // =================================================================
        Instruction::Pop => {
            vm.pop()?;
        }
        Instruction::Dup => {
            let value = vm.peek()?.clone();
            vm.push(value);
        }

        // =================================================================
        // Arithmetic
        // =================================================================
        Instruction::Add => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            let result = js_add(&lhs, &rhs);
            vm.push(result);
        }
        Instruction::Sub => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Number(lhs.to_number() - rhs.to_number()));
        }
        Instruction::Mul => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Number(lhs.to_number() * rhs.to_number()));
        }
        Instruction::Div => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Number(lhs.to_number() / rhs.to_number()));
        }
        Instruction::Mod => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Number(lhs.to_number() % rhs.to_number()));
        }
        Instruction::Exp => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Number(lhs.to_number().powf(rhs.to_number())));
        }

        // =================================================================
        // Bitwise
        // =================================================================
        Instruction::BitAnd => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Number((lhs.to_int32() & rhs.to_int32()) as f64));
        }
        Instruction::BitOr => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Number((lhs.to_int32() | rhs.to_int32()) as f64));
        }
        Instruction::BitXor => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Number((lhs.to_int32() ^ rhs.to_int32()) as f64));
        }
        Instruction::Shl => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            let shift = rhs.to_uint32() & 0x1f;
            vm.push(JsValue::Number((lhs.to_int32() << shift) as f64));
        }
        Instruction::Shr => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            let shift = rhs.to_uint32() & 0x1f;
            vm.push(JsValue::Number((lhs.to_int32() >> shift) as f64));
        }
        Instruction::UShr => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            let shift = rhs.to_uint32() & 0x1f;
            vm.push(JsValue::Number((lhs.to_uint32() >> shift) as f64));
        }

        // =================================================================
        // Comparison
        // =================================================================
        Instruction::Eq => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Boolean(lhs.abstract_eq(&rhs)));
        }
        Instruction::StrictEq => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Boolean(lhs.strict_eq(&rhs)));
        }
        Instruction::Ne => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Boolean(!lhs.abstract_eq(&rhs)));
        }
        Instruction::StrictNe => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Boolean(!lhs.strict_eq(&rhs)));
        }
        Instruction::Lt => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Boolean(lhs.to_number() < rhs.to_number()));
        }
        Instruction::Le => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Boolean(lhs.to_number() <= rhs.to_number()));
        }
        Instruction::Gt => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Boolean(lhs.to_number() > rhs.to_number()));
        }
        Instruction::Ge => {
            let rhs = vm.pop()?;
            let lhs = vm.pop()?;
            vm.push(JsValue::Boolean(lhs.to_number() >= rhs.to_number()));
        }

        // =================================================================
        // Unary operators
        // =================================================================
        Instruction::Not => {
            let val = vm.pop()?;
            vm.push(JsValue::Boolean(!val.to_boolean()));
        }
        Instruction::BitNot => {
            let val = vm.pop()?;
            vm.push(JsValue::Number((!val.to_int32()) as f64));
        }
        Instruction::Neg => {
            let val = vm.pop()?;
            vm.push(JsValue::Number(-val.to_number()));
        }
        Instruction::Pos => {
            let val = vm.pop()?;
            vm.push(JsValue::Number(val.to_number()));
        }
        Instruction::TypeOf => {
            let val = vm.pop()?;
            vm.push(JsValue::string(val.type_of()));
        }
        Instruction::Void => {
            vm.pop()?;
            vm.push(JsValue::Undefined);
        }
        Instruction::Delete => {
            // Simplified: always evaluates to `true` and discards the value.
            vm.pop()?;
            vm.push(JsValue::Boolean(true));
        }

        // =================================================================
        // Control flow
        // =================================================================
        Instruction::Jump(offset) => {
            let frame = vm.call_stack.last_mut().unwrap();
            frame.ip = ((frame.ip as i64) + (offset as i64)) as usize;
        }
        Instruction::JumpIfFalse(offset) => {
            let val = vm.pop()?;
            if !val.to_boolean() {
                let frame = vm.call_stack.last_mut().unwrap();
                frame.ip = ((frame.ip as i64) + (offset as i64)) as usize;
            }
        }
        Instruction::JumpIfTrue(offset) => {
            let val = vm.pop()?;
            if val.to_boolean() {
                let frame = vm.call_stack.last_mut().unwrap();
                frame.ip = ((frame.ip as i64) + (offset as i64)) as usize;
            }
        }

        // =================================================================
        // Function calls
        // =================================================================
        Instruction::Call(argc) => {
            exec_call(vm, argc as usize, JsValue::Undefined)?;
        }
        Instruction::CallMethod(argc) => {
            // Stack: [..., receiver, method, arg0, arg1, ...]
            // Pop args, then method, then receiver.
            let argc = argc as usize;
            let stack_len = vm.value_stack.len();
            if stack_len < argc + 2 {
                return Err(RawJsError::internal_error("stack underflow in CallMethod"));
            }
            let args: Vec<JsValue> = vm.value_stack.drain(stack_len - argc..).collect();
            let method = vm.pop()?;
            let receiver = vm.pop()?;

            // Intercept generator .next()/.return()/.throw() calls.
            if let Some(result) = try_generator_method_call(vm, &receiver, &method, &args)? {
                vm.push(result);
                return Ok(None);
            }

            // Push method back then args, so exec_call sees [method, args...]
            vm.push(method);
            for arg in args {
                vm.push(arg);
            }
            exec_call(vm, argc, receiver)?;
        }
        Instruction::Return => {
            return exec_return(vm);
        }
        Instruction::CreateClosure(const_idx) => {
            exec_create_closure(vm, const_idx)?;
        }

        // =================================================================
        // Object / array creation
        // =================================================================
        Instruction::CreateObject => {
            let obj = JsObject::ordinary();
            let ptr = vm.heap.alloc(obj);
            vm.push(JsValue::Object(ptr));
        }
        Instruction::CreateArray(count) => {
            let count = count as usize;
            let stack_len = vm.value_stack.len();
            if count > stack_len {
                return Err(RawJsError::internal_error("stack underflow in CreateArray"));
            }
            let elements: Vec<JsValue> = vm.value_stack.drain(stack_len - count..).collect();
            let obj = JsObject::array(elements);
            let ptr = vm.heap.alloc(obj);
            vm.push(JsValue::Object(ptr));
        }

        // =================================================================
        // Property access
        // =================================================================
        Instruction::GetProperty(name_idx) => {
            let name = {
                let frame = vm.call_stack.last().unwrap();
                get_constant_string(&vm.chunks[frame.chunk_index], name_idx)?
            };
            let obj_val = vm.pop()?;
            let result = get_property_value(vm, &obj_val, &name)?;
            vm.push(result);
        }
        Instruction::SetProperty(name_idx) => {
            let name = {
                let frame = vm.call_stack.last().unwrap();
                get_constant_string(&vm.chunks[frame.chunk_index], name_idx)?
            };
            let value = vm.pop()?;
            let obj_val = vm.pop()?;
            set_property_value(&obj_val, &name, &value)?;
            vm.push(value);
        }
        Instruction::GetIndex => {
            let index = vm.pop()?;
            let obj_val = vm.pop()?;
            let key = index.to_js_string();
            let result = get_property_value(vm, &obj_val, &key)?;
            vm.push(result);
        }
        Instruction::SetIndex => {
            let value = vm.pop()?;
            let index = vm.pop()?;
            let obj_val = vm.pop()?;
            let key = index.to_js_string();
            set_property_value(&obj_val, &key, &value)?;
            vm.push(value);
        }
        Instruction::GetComputed => {
            let key = vm.pop()?;
            let obj_val = vm.pop()?;
            // Handle Symbol keys specially.
            if let JsValue::Symbol(ref sym) = key {
                let result = match &obj_val {
                    JsValue::Object(ptr) => ptr.borrow().get_symbol_property(sym.id),
                    _ => JsValue::Undefined,
                };
                vm.push(result);
            } else {
                let key_str = key.to_js_string();
                let result = get_property_value(vm, &obj_val, &key_str)?;
                vm.push(result);
            }
        }
        Instruction::SetComputed => {
            let value = vm.pop()?;
            let key = vm.pop()?;
            let obj_val = vm.pop()?;
            // Handle Symbol keys specially.
            if let JsValue::Symbol(ref sym) = key {
                if let JsValue::Object(ref ptr) = obj_val {
                    ptr.borrow_mut().set_symbol_property(sym.id, value.clone());
                }
                vm.push(value);
            } else {
                let key_str = key.to_js_string();
                set_property_value(&obj_val, &key_str, &value)?;
                vm.push(value);
            }
        }

        // =================================================================
        // Exception handling
        // =================================================================
        Instruction::Throw => {
            let value = vm.pop()?;
            // Save the thrown JS value so catch handlers can access it.
            vm.thrown_value = Some(value.clone());
            let msg = format!("{}", value);
            return Err(RawJsError::internal_error(msg));
        }
        Instruction::EnterTry(catch_offset, finally_offset) => {
            let frame = vm.call_stack.last().unwrap();
            let current_ip = frame.ip; // already advanced past EnterTry

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

            vm.try_stack.push(TryContext {
                catch_ip,
                finally_ip,
                stack_depth: vm.value_stack.len(),
                call_depth: vm.call_stack.len(),
            });
        }
        Instruction::LeaveTry => {
            vm.try_stack.pop();
        }

        // =================================================================
        // Relational keywords
        // =================================================================
        Instruction::In => {
            let obj_val = vm.pop()?;
            let key = vm.pop()?;
            let key_str = key.to_js_string();
            let result = match &obj_val {
                JsValue::Object(ptr) => ptr.borrow().has_own_property(&key_str),
                _ => false,
            };
            vm.push(JsValue::Boolean(result));
        }
        Instruction::Instanceof => {
            let _rhs = vm.pop()?;
            let _lhs = vm.pop()?;
            // Simplified: always false for now.
            vm.push(JsValue::Boolean(false));
        }

        // =================================================================
        // Iterator (for-of)
        // =================================================================
        Instruction::GetIterator => {
            let obj_val = vm.pop()?;
            // Generators are already iterators — return as-is.
            if let JsValue::Object(ref ptr) = obj_val {
                let is_gen = matches!(ptr.borrow().internal, ObjectInternal::Generator(_));
                if is_gen {
                    vm.push(obj_val);
                    return Ok(None);
                }
            }
            // Create an iterator from the object.
            // For arrays: iterate over elements.
            // For strings: iterate over characters.
            // For Sets: iterate over values.
            // For Maps: iterate over [key, value] pairs.
            let values: Vec<JsValue> = match &obj_val {
                JsValue::Object(ptr) => {
                    let obj = ptr.borrow();
                    match &obj.internal {
                        ObjectInternal::Array(elements) => elements.clone(),
                        ObjectInternal::Set(set_values) => set_values.clone(),
                        ObjectInternal::Map(entries) => entries
                            .iter()
                            .map(|(k, v)| {
                                let pair = vec![k.clone(), v.clone()];
                                JsValue::Object(rawjs_runtime::GcPtr::new(JsObject::array(pair)))
                            })
                            .collect(),
                        _ => {
                            return Err(RawJsError::type_error(format!(
                                "{} is not iterable",
                                obj_val.type_of()
                            )));
                        }
                    }
                }
                JsValue::String(s) => s
                    .chars()
                    .map(|c| JsValue::string(c.to_string().as_str()))
                    .collect(),
                _ => {
                    return Err(RawJsError::type_error(format!(
                        "{} is not iterable",
                        obj_val.type_of()
                    )));
                }
            };
            let iter_obj = JsObject::iterator(values);
            let ptr = vm.heap.alloc(iter_obj);
            vm.push(JsValue::Object(ptr));
        }
        Instruction::IteratorNext => {
            // Stack: [..., iterator] -> [..., iterator, result_value, done_bool]
            let iter_val = vm.peek()?.clone();
            match &iter_val {
                JsValue::Object(ptr) => {
                    // Check if it's a generator — call .next()
                    let is_gen = {
                        let obj = ptr.borrow();
                        matches!(obj.internal, ObjectInternal::Generator(_))
                    };
                    if is_gen {
                        let result = generator_next(vm, ptr, JsValue::Undefined)?;
                        // result is {value, done} object
                        let (value, done) = if let JsValue::Object(p) = &result {
                            let obj = p.borrow();
                            (
                                obj.get_property("value"),
                                obj.get_property("done").to_boolean(),
                            )
                        } else {
                            (JsValue::Undefined, true)
                        };
                        vm.push(value);
                        vm.push(JsValue::Boolean(done));
                    } else {
                        let mut obj = ptr.borrow_mut();
                        match &mut obj.internal {
                            ObjectInternal::Iterator(state) => {
                                let (value, done) = state.advance();
                                drop(obj);
                                vm.push(value);
                                vm.push(JsValue::Boolean(done));
                            }
                            _ => {
                                return Err(RawJsError::type_error("not an iterator"));
                            }
                        }
                    }
                }
                _ => {
                    return Err(RawJsError::type_error("not an iterator"));
                }
            }
        }
        Instruction::IteratorDone(exit_offset) => {
            // Stack: [..., iterator, value, done]
            let done = vm.pop()?;
            if done.to_boolean() {
                // Done: pop the value, leave iterator on stack for cleanup.
                vm.pop()?; // pop value (undefined)
                let frame = vm.call_stack.last_mut().unwrap();
                frame.ip = ((frame.ip as i64) + (exit_offset as i64)) as usize;
            }
            // Not done: stack is [..., iterator, value] — value is ready for use.
        }

        // =================================================================
        // For-in iteration
        // =================================================================
        Instruction::ForInInit => {
            let obj_val = vm.pop()?;
            // Collect all enumerable keys from the object (and its prototype chain).
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
            let arr_obj = JsObject::array(keys);
            let arr_ptr = vm.heap.alloc(arr_obj);
            vm.push(JsValue::Object(arr_ptr)); // keys array
            vm.push(JsValue::Number(0.0)); // starting index
        }
        Instruction::ForInNext(exit_offset) => {
            // Stack: [..., keys_array, index]
            let index_val = vm.pop()?;
            let index = index_val.to_number() as usize;
            let keys_val = vm.peek()?.clone();

            let len = match &keys_val {
                JsValue::Object(ptr) => {
                    let obj = ptr.borrow();
                    obj.array_length()
                }
                _ => 0,
            };

            if index >= len {
                // Iteration complete. Push index back (stack: [..., keys_array, index])
                // and jump to exit.
                vm.push(index_val);
                let frame = vm.call_stack.last_mut().unwrap();
                frame.ip = ((frame.ip as i64) + (exit_offset as i64)) as usize;
            } else {
                // Get the key at `index`.
                let key = match &keys_val {
                    JsValue::Object(ptr) => {
                        let obj = ptr.borrow();
                        obj.get_element(index)
                    }
                    _ => JsValue::Undefined,
                };
                // Push updated index, then the key.
                // Stack: [..., keys_array, index+1, key]
                vm.push(JsValue::Number((index + 1) as f64));
                vm.push(key);
            }
        }

        // =================================================================
        // Postfix increment / decrement
        // =================================================================
        Instruction::PostfixIncrement => {
            let val = vm.pop()?;
            let n = val.to_number();
            // Push the *original* value -- the variable update is the
            // compiler's responsibility via a subsequent StoreLocal/StoreGlobal.
            vm.push(JsValue::Number(n));
            vm.push(JsValue::Number(n + 1.0));
        }
        Instruction::PostfixDecrement => {
            let val = vm.pop()?;
            let n = val.to_number();
            vm.push(JsValue::Number(n));
            vm.push(JsValue::Number(n - 1.0));
        }

        // =================================================================
        // ESM: import / export
        // =================================================================
        Instruction::ImportModule(source_idx) => {
            let source = {
                let frame = vm.call_stack.last().unwrap();
                get_constant_string(&vm.chunks[frame.chunk_index], source_idx)?
            };
            let ns = vm.execute_module(&source)?;
            vm.push(JsValue::Object(ns));
        }
        Instruction::ImportBinding(name_idx) => {
            let name = {
                let frame = vm.call_stack.last().unwrap();
                get_constant_string(&vm.chunks[frame.chunk_index], name_idx)?
            };
            let ns = vm.pop()?;
            let value = match &ns {
                JsValue::Object(ptr) => ptr.borrow().get_property(&name),
                _ => JsValue::Undefined,
            };
            vm.push(value);
        }
        Instruction::ExportBinding(name_idx) => {
            let name = {
                let frame = vm.call_stack.last().unwrap();
                get_constant_string(&vm.chunks[frame.chunk_index], name_idx)?
            };
            let value = vm.pop()?;
            if let Some(ref exports) = vm.module_exports.clone() {
                exports.borrow_mut().set_property(name, value);
            }
        }
        Instruction::ExportDefault => {
            let value = vm.pop()?;
            if let Some(ref exports) = vm.module_exports.clone() {
                exports
                    .borrow_mut()
                    .set_property("default".to_string(), value);
            }
        }

        // =================================================================
        // Generator / async
        // =================================================================
        Instruction::CreateGenerator => {
            // Should not be reached; generator creation is handled in exec_call.
            return Err(RawJsError::internal_error(
                "CreateGenerator should not be executed directly",
            ));
        }
        Instruction::Yield => {
            return exec_yield(vm);
        }
        Instruction::Await => {
            return exec_await(vm);
        }
        Instruction::DisposeResource(slot) => {
            exec_dispose_resource(vm, slot)?;
        }
    }

    Ok(None)
}

// ---------------------------------------------------------------------------
// Call / return helpers
// ---------------------------------------------------------------------------

/// Execute a function call with `argc` arguments on the stack.
///
/// Stack layout before call:
///   `[..., function, arg0, arg1, ..., argN-1]`
pub(crate) fn exec_call(vm: &mut Vm, argc: usize, this_value: JsValue) -> Result<()> {
    let stack_len = vm.value_stack.len();
    if stack_len < argc + 1 {
        return Err(RawJsError::internal_error("stack underflow in Call"));
    }

    // Collect arguments (right-to-left off the stack).
    let args: Vec<JsValue> = vm.value_stack.drain(stack_len - argc..).collect();

    // Pop the callee.
    let callee = vm.pop()?;

    // Check if callee is the Promise constructor
    if is_promise_constructor(vm, &callee) {
        return exec_promise_constructor(vm, &args);
    }

    match &callee {
        JsValue::Object(ptr) => {
            let func = {
                let obj = ptr.borrow();
                match &obj.internal {
                    ObjectInternal::Function(f) => Some(f.clone()),
                    _ => None,
                }
            };

            if let Some(func) = func {
                match &func.kind {
                    rawjs_runtime::FunctionKind::Native(native_fn) => {
                        let native_fn = *native_fn;
                        vm.heap.calling_fn = Some(ptr.clone());
                        let result = native_fn(&mut vm.heap, &this_value, &args)?;
                        vm.heap.calling_fn = None;
                        vm.push(result);
                        Ok(())
                    }
                    rawjs_runtime::FunctionKind::Bytecode { chunk_index } => {
                        let chunk_index = *chunk_index;

                        // Check if this is a generator function.
                        if vm.chunks[chunk_index].is_generator && !vm.chunks[chunk_index].is_async {
                            return exec_create_generator(
                                vm,
                                chunk_index,
                                &args,
                                func.upvalues.clone(),
                                this_value,
                            );
                        }

                        // Check if this is an async function.
                        if vm.chunks[chunk_index].is_async {
                            return exec_async_call(
                                vm,
                                chunk_index,
                                &args,
                                func.upvalues.clone(),
                                this_value,
                            );
                        }

                        // Track execution count and try JIT if threshold reached.
                        let count = vm.bump_execution_count(chunk_index);
                        if count == JIT_THRESHOLD {
                            let _ = vm.try_jit_compile(chunk_index);
                        }

                        // Set up a new call frame.
                        let param_count = vm.chunks[chunk_index].param_count as usize;
                        let local_count = vm.chunks[chunk_index].local_count as usize;
                        let slot_count = local_count.max(param_count);

                        let mut locals = vec![JsValue::Undefined; slot_count];
                        for (i, arg) in args.iter().enumerate() {
                            if i < slot_count {
                                locals[i] = arg.clone();
                            }
                        }

                        let frame = CallFrame {
                            chunk_index,
                            ip: 0,
                            base: vm.value_stack.len(),
                            locals,
                            upvalues: func.upvalues.clone(),
                            this_value,
                        };
                        vm.call_stack.push(frame);

                        // If a JIT version exists, run it immediately
                        // (the frame is already set up; the JIT will execute
                        // it and stub_return will pop it).
                        if count >= JIT_THRESHOLD && vm.has_jit(chunk_index) {
                            let result = unsafe { vm.call_jit_new(chunk_index) };
                            if result != 0 {
                                return Err(vm.jit_error.take().unwrap_or_else(|| {
                                    RawJsError::internal_error("JIT execution failed")
                                }));
                            }
                            return Ok(());
                        }

                        Ok(())
                    }
                }
            } else {
                Err(RawJsError::type_error("object is not a function"))
            }
        }
        _ => Err(RawJsError::type_error(format!(
            "{} is not a function",
            callee.type_of()
        ))),
    }
}

/// Execute a `Return` instruction.
///
/// Pops the return value, pops the call frame, restores the value stack,
/// and pushes the return value back.  If this was the top-level frame,
/// returns `Ok(Some(value))`.
fn exec_return(vm: &mut Vm) -> Result<Option<JsValue>> {
    let return_value = if vm.value_stack.len() > vm.call_stack.last().unwrap().base {
        vm.pop()?
    } else {
        JsValue::Undefined
    };

    let frame = vm.call_stack.pop().unwrap();
    vm.value_stack.truncate(frame.base);

    if vm.call_stack.is_empty() {
        // Top-level return.
        return Ok(Some(return_value));
    }

    // Push the return value onto the caller's stack.
    vm.push(return_value);
    Ok(None)
}

/// Execute a `CreateClosure` instruction.
pub(crate) fn exec_create_closure(vm: &mut Vm, const_idx: u16) -> Result<()> {
    // First, clone the function chunk out of the constant pool so we can
    // release the immutable borrow on `vm` before calling `add_chunk`.
    let func_clone = {
        let frame = vm.call_stack.last().unwrap();
        let chunk = &vm.chunks[frame.chunk_index];
        let constant = &chunk.constants[const_idx as usize];
        match constant {
            Constant::Function(func_chunk) => (**func_chunk).clone(),
            _ => {
                return Err(RawJsError::internal_error(
                    "CreateClosure: constant is not a Function",
                ));
            }
        }
    };

    // Now register the cloned chunk (mutable borrow is safe here).
    let chunk_index = vm.add_chunk(func_clone);

    // Capture upvalues from the current frame using the upvalue descriptors.
    let upvalue_descs = vm.chunks[chunk_index].upvalue_descriptors.clone();
    let upvalues: Vec<Upvalue> = {
        let frame = vm.call_stack.last().unwrap();
        upvalue_descs
            .iter()
            .map(|desc| {
                if desc.is_local {
                    // Capture a local from the current frame.
                    // Create a new shared Upvalue cell so the closure and
                    // any future closures share the same mutable cell.
                    let idx = desc.index as usize;
                    let value = if idx < frame.locals.len() {
                        frame.locals[idx].clone()
                    } else {
                        JsValue::Undefined
                    };
                    Upvalue::new(value)
                } else {
                    // Capture an upvalue from the current frame's upvalues.
                    // Clone the Rc so both parent and child share the cell.
                    let idx = desc.index as usize;
                    if idx < frame.upvalues.len() {
                        frame.upvalues[idx].clone()
                    } else {
                        Upvalue::new(JsValue::Undefined)
                    }
                }
            })
            .collect()
    };

    let name = vm.chunks[chunk_index].name.clone();
    let func_obj = JsObject::function(chunk_index, upvalues, name);
    let ptr = vm.heap.alloc(func_obj);
    vm.push(JsValue::Object(ptr));
    Ok(())
}

// ---------------------------------------------------------------------------
// Exception unwinding
// ---------------------------------------------------------------------------

/// Attempt to unwind the call/try stacks to find a catch handler.
///
/// Returns `true` if a handler was found and execution can continue;
/// `false` if the exception is unhandled.
fn unwind_exception(vm: &mut Vm, err: &RawJsError) -> Result<bool> {
    // Walk the try stack from top to bottom looking for a handler whose
    // call depth is reachable.
    while let Some(ctx) = vm.try_stack.last().cloned() {
        // Unwind call frames until we reach the frame that set up the try.
        while vm.call_stack.len() > ctx.call_depth {
            let frame = vm.call_stack.pop().unwrap();
            vm.value_stack.truncate(frame.base);
        }

        // Restore the value stack depth.
        vm.value_stack.truncate(ctx.stack_depth);

        vm.try_stack.pop();

        if let Some(catch_ip) = ctx.catch_ip {
            // Push the original thrown JS value onto the stack.
            let thrown = vm
                .thrown_value
                .take()
                .unwrap_or_else(|| JsValue::string(err.message.as_str()));
            vm.push(thrown);

            // Jump to the catch handler.
            if let Some(frame) = vm.call_stack.last_mut() {
                frame.ip = catch_ip;
            }
            return Ok(true);
        }

        if let Some(finally_ip) = ctx.finally_ip {
            // Jump to finally handler.
            if let Some(frame) = vm.call_stack.last_mut() {
                frame.ip = finally_ip;
            }
            return Ok(true);
        }
    }

    Ok(false)
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

/// Convert a compile-time `Constant` to a runtime `JsValue`.
pub(crate) fn constant_to_value(constant: &Constant) -> JsValue {
    match constant {
        Constant::Number(n) => JsValue::Number(*n),
        Constant::String(s) => JsValue::string(s.as_str()),
        Constant::Boolean(b) => JsValue::Boolean(*b),
        Constant::Null => JsValue::Null,
        Constant::Undefined => JsValue::Undefined,
        Constant::Function(_) => {
            // Function constants should be loaded via CreateClosure.
            JsValue::Undefined
        }
    }
}

/// Look up a constant string by index in a chunk's constant pool.
fn get_constant_string(chunk: &Chunk, idx: u16) -> Result<String> {
    match &chunk.constants[idx as usize] {
        Constant::String(s) => Ok(s.clone()),
        other => Err(RawJsError::internal_error(format!(
            "expected string constant at index {}, found {}",
            idx, other
        ))),
    }
}

/// Check if a callee value is the global Promise constructor.
fn is_promise_constructor(vm: &Vm, callee: &JsValue) -> bool {
    if let Some(global_promise) = vm.get_global("Promise") {
        if let (JsValue::Object(a), JsValue::Object(b)) = (callee, global_promise) {
            return a.ptr_eq(b);
        }
    }
    false
}

/// Execute `new Promise(executor)` — creates a Promise, builds resolve/reject,
/// calls executor synchronously, returns the Promise.
fn exec_promise_constructor(vm: &mut Vm, args: &[JsValue]) -> Result<()> {
    let executor = args.first().cloned().unwrap_or(JsValue::Undefined);

    if !executor.is_function() {
        return Err(RawJsError::type_error("Promise resolver is not a function"));
    }

    // Allocate the promise object
    let promise_ptr = vm.heap.alloc(JsObject::promise());
    if let Some(ref proto) = vm.promise_prototype {
        promise_ptr.borrow_mut().prototype = Some(proto.clone());
    }

    // Register in promise_targets
    let promise_id = vm.heap.next_promise_id;
    vm.heap.next_promise_id += 1;
    vm.heap
        .promise_targets
        .insert(promise_id, promise_ptr.clone());

    // Create resolve function with __promise_id__
    let mut resolve_fn =
        JsObject::native_function("resolve", rawjs_runtime::builtins::promise_resolve_fn);
    resolve_fn.define_property(
        "__promise_id__".to_string(),
        Property::builtin(JsValue::Number(promise_id as f64)),
    );
    let resolve_ptr = vm.heap.alloc(resolve_fn);

    // Create reject function with __promise_id__
    let mut reject_fn =
        JsObject::native_function("reject", rawjs_runtime::builtins::promise_reject_fn);
    reject_fn.define_property(
        "__promise_id__".to_string(),
        Property::builtin(JsValue::Number(promise_id as f64)),
    );
    let reject_ptr = vm.heap.alloc(reject_fn);

    // Call the executor with (resolve, reject)
    // Push executor, then args, then call
    vm.push(executor);
    vm.push(JsValue::Object(resolve_ptr));
    vm.push(JsValue::Object(reject_ptr));
    exec_call(vm, 2, JsValue::Undefined)?;

    // The executor may have pushed a return value on the stack if it was native,
    // or set up a frame if bytecode. For bytecode executors, we need the return
    // value to be discarded and replaced with the promise.
    // If the executor was native, pop its return value and push the promise.
    // If bytecode, we need to wait for the frame to complete... but the executor
    // runs synchronously. Since exec_call pushes a frame for bytecode, we need
    // to execute that frame now.

    // Check if a new frame was pushed (bytecode executor)
    // We need to run the executor frame to completion before returning the promise.
    let initial_stack_len = vm.value_stack.len();
    let saved_call_depth = vm.call_stack.len();
    if saved_call_depth > 1 {
        // A bytecode frame was pushed — run it to completion
        let result = run_executor_frame(vm, saved_call_depth - 1);
        match result {
            Ok(_) => {
                while vm.value_stack.len() > initial_stack_len {
                    vm.value_stack.pop();
                }
            }
            Err(err) => {
                // Executor threw — clean up and reject the promise
                // Ensure we've unwound back to our frame
                while vm.call_stack.len() >= saved_call_depth {
                    let frame = vm.call_stack.pop().unwrap();
                    vm.value_stack.truncate(frame.base);
                }
                vm.value_stack.truncate(initial_stack_len);
                let reason = vm
                    .thrown_value
                    .take()
                    .unwrap_or_else(|| JsValue::string(err.message.as_str()));
                rawjs_runtime::builtins::reject_promise_internal(&promise_ptr, reason);
            }
        }
    } else {
        // Native executor already ran — pop its return value
        if vm.value_stack.len() > initial_stack_len {
            vm.pop()?;
        }
    }

    vm.push(JsValue::Object(promise_ptr));
    Ok(())
}

/// JavaScript `+` operator: string concatenation if either operand is a
/// string, otherwise numeric addition.
pub(crate) fn js_add(lhs: &JsValue, rhs: &JsValue) -> JsValue {
    match (lhs, rhs) {
        (JsValue::String(a), JsValue::String(b)) => {
            let mut s = String::with_capacity(a.len() + b.len());
            s.push_str(a);
            s.push_str(b);
            JsValue::string(s.as_str())
        }
        (JsValue::String(a), _) => {
            let b_str = rhs.to_js_string();
            let mut s = String::with_capacity(a.len() + b_str.len());
            s.push_str(a);
            s.push_str(&b_str);
            JsValue::string(s.as_str())
        }
        (_, JsValue::String(b)) => {
            let a_str = lhs.to_js_string();
            let mut s = String::with_capacity(a_str.len() + b.len());
            s.push_str(&a_str);
            s.push_str(b);
            JsValue::string(s.as_str())
        }
        _ => JsValue::Number(lhs.to_number() + rhs.to_number()),
    }
}

/// Get a property from a JsValue (must be an object).
pub(crate) fn get_property_value(vm: &Vm, obj_val: &JsValue, name: &str) -> Result<JsValue> {
    match obj_val {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            let val = obj.get_property(name);
            if !val.is_undefined() {
                return Ok(val);
            }
            // If the object is an array and property wasn't found, check array prototype.
            if obj.is_array() {
                if name == "length" {
                    return Ok(JsValue::Number(obj.array_length() as f64));
                }
                drop(obj);
                if let Some(ref proto) = vm.array_prototype {
                    let result = proto.borrow().get_property(name);
                    if !result.is_undefined() {
                        return Ok(result);
                    }
                }
                return Ok(JsValue::Undefined);
            }
            // Check Map prototype.
            if matches!(obj.internal, ObjectInternal::Map(_)) {
                drop(obj);
                if let Some(ref proto) = vm.map_prototype {
                    let result = proto.borrow().get_property(name);
                    if !result.is_undefined() {
                        return Ok(result);
                    }
                }
                return Ok(val);
            }
            // Check Set prototype.
            if matches!(obj.internal, ObjectInternal::Set(_)) {
                drop(obj);
                if let Some(ref proto) = vm.set_prototype {
                    let result = proto.borrow().get_property(name);
                    if !result.is_undefined() {
                        return Ok(result);
                    }
                }
                return Ok(val);
            }
            // Check Promise prototype.
            if matches!(obj.internal, ObjectInternal::Promise(_)) {
                drop(obj);
                if let Some(ref proto) = vm.promise_prototype {
                    let result = proto.borrow().get_property(name);
                    if !result.is_undefined() {
                        return Ok(result);
                    }
                }
                return Ok(val);
            }
            // Check Generator prototype.
            if matches!(obj.internal, ObjectInternal::Generator(_)) {
                drop(obj);
                if let Some(ref proto) = vm.generator_prototype {
                    let result = proto.borrow().get_property(name);
                    if !result.is_undefined() {
                        return Ok(result);
                    }
                }
                return Ok(val);
            }
            Ok(val)
        }
        JsValue::String(s) => {
            // String property access: .length and numeric indices.
            match name {
                "length" => Ok(JsValue::Number(s.len() as f64)),
                _ => {
                    if let Ok(idx) = name.parse::<usize>() {
                        if idx < s.len() {
                            let ch = s.chars().nth(idx).unwrap_or('\0');
                            return Ok(JsValue::string(ch.to_string().as_str()));
                        }
                    }
                    // Check string prototype for methods.
                    if let Some(ref proto) = vm.string_prototype {
                        let result = proto.borrow().get_property(name);
                        if !result.is_undefined() {
                            return Ok(result);
                        }
                    }
                    Ok(JsValue::Undefined)
                }
            }
        }
        JsValue::Number(_) => {
            // Check number prototype for methods.
            if let Some(ref proto) = vm.number_prototype {
                let result = proto.borrow().get_property(name);
                if !result.is_undefined() {
                    return Ok(result);
                }
            }
            Ok(JsValue::Undefined)
        }
        JsValue::Symbol(sym) => {
            // description property
            if name == "description" {
                return Ok(match &sym.description {
                    Some(desc) => JsValue::string(desc.as_ref()),
                    None => JsValue::Undefined,
                });
            }
            // Check symbol prototype for methods.
            if let Some(ref proto) = vm.symbol_prototype {
                let result = proto.borrow().get_property(name);
                if !result.is_undefined() {
                    return Ok(result);
                }
            }
            Ok(JsValue::Undefined)
        }
        _ => Ok(JsValue::Undefined),
    }
}

/// Set a property on a JsValue (must be an object).
pub(crate) fn set_property_value(obj_val: &JsValue, name: &str, value: &JsValue) -> Result<()> {
    match obj_val {
        JsValue::Object(ptr) => {
            ptr.borrow_mut()
                .set_property(name.to_string(), value.clone());
            Ok(())
        }
        _ => Err(RawJsError::type_error(format!(
            "cannot set property '{}' of {}",
            name,
            obj_val.type_of()
        ))),
    }
}

// ---------------------------------------------------------------------------
// Generator helpers
// ---------------------------------------------------------------------------

/// Create a generator object from a generator function call.
///
/// Instead of pushing a CallFrame and executing, we freeze the initial state
/// into a `GeneratorState` wrapped in a JS object with the generator prototype.
fn exec_create_generator(
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
        upvalues,
        saved_stack: Vec::new(),
        this_value,
        result_promise: None,
        saved_try_stack: Vec::new(),
    };

    let mut gen_obj = JsObject::ordinary();
    gen_obj.internal = ObjectInternal::Generator(state);
    if let Some(ref proto) = vm.generator_prototype {
        gen_obj.prototype = Some(proto.clone());
    }

    let ptr = vm.heap.alloc(gen_obj);
    vm.push(JsValue::Object(ptr));
    Ok(())
}

/// Execute a `Yield` instruction.
///
/// Saves the current frame state into the generator's `GeneratorState`,
/// pops the frame, and pushes `{ value, done: false }` to the caller.
pub(crate) fn exec_yield(vm: &mut Vm) -> Result<Option<JsValue>> {
    let yielded_value = vm.pop()?;

    // Find the generator object that owns this frame.
    // The generator's chunk_index matches the current frame's chunk_index.
    let frame = vm.call_stack.pop().unwrap();

    // Save the remaining value_stack segment for this frame.
    let saved_stack: Vec<JsValue> = vm.value_stack.drain(frame.base..).collect();

    // Find the generator object and update its state.
    // The generator was created with this chunk_index; we stashed it
    // as `this_value` or we need to find it. The generator object is the
    // `this_value` on the frame that called .next() — which is the frame
    // before this one. But we need the actual generator pointer.
    //
    // Strategy: walk the heap... no, that's expensive.  Instead, the
    // generator's GcPtr is stored on the caller's stack as the receiver.
    // When .next() was intercepted, we saved the gen_ptr in a thread-local.
    //
    // Simpler approach: store the generator ptr in the frame. But we don't
    // have that field yet. Instead, use the this_value which holds the gen ptr.
    if let JsValue::Object(ref gen_ptr) = frame.this_value {
        let mut gen_obj = gen_ptr.borrow_mut();
        if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
            state.status = GeneratorStatus::SuspendedYield;
            state.ip = frame.ip; // IP already advanced past Yield
            state.locals = frame.locals;
            state.upvalues = frame.upvalues;
            state.saved_stack = saved_stack;
        }
    }

    // Push {value, done: false} to the caller's stack.
    let result = make_iterator_result(vm, yielded_value, false);
    vm.push(result);

    Ok(None)
}

/// Try to intercept a generator method call (.next, .return, .throw).
///
/// Returns `Some(result)` if the call was intercepted, `None` otherwise.
fn try_generator_method_call(
    vm: &mut Vm,
    receiver: &JsValue,
    method: &JsValue,
    args: &[JsValue],
) -> Result<Option<JsValue>> {
    // Check if receiver is a generator.
    let gen_ptr = match receiver {
        JsValue::Object(ptr) => {
            let is_gen = matches!(ptr.borrow().internal, ObjectInternal::Generator(_));
            if is_gen {
                ptr.clone()
            } else {
                return Ok(None);
            }
        }
        _ => return Ok(None),
    };

    // Check if method is a native function from the generator prototype.
    let method_name = match method {
        JsValue::Object(ptr) => {
            let obj = ptr.borrow();
            if let ObjectInternal::Function(f) = &obj.internal {
                f.name.clone()
            } else {
                return Ok(None);
            }
        }
        _ => return Ok(None),
    };

    match method_name.as_str() {
        "next" => {
            let arg = args.first().cloned().unwrap_or(JsValue::Undefined);
            let result = generator_next(vm, &gen_ptr, arg)?;
            Ok(Some(result))
        }
        "return" => {
            let arg = args.first().cloned().unwrap_or(JsValue::Undefined);
            let result = generator_return(&gen_ptr, vm, arg);
            Ok(Some(result))
        }
        "throw" => {
            let arg = args.first().cloned().unwrap_or(JsValue::Undefined);
            let result = generator_throw(vm, &gen_ptr, arg)?;
            Ok(Some(result))
        }
        _ => Ok(None),
    }
}

/// Resume a generator via `.next(value)`.
///
/// If `SuspendedStart`, creates a fresh CallFrame and runs until Yield/Return.
/// If `SuspendedYield`, restores the saved frame and pushes `value` before resuming.
/// If `Completed`, returns `{ value: undefined, done: true }`.
fn generator_next(
    vm: &mut Vm,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    value: JsValue,
) -> Result<JsValue> {
    // Extract the current status and relevant data.
    let (_status, chunk_index, ip, locals, upvalues, saved_stack, _this_value) = {
        let mut gen_obj = gen_ptr.borrow_mut();
        let state = match &mut gen_obj.internal {
            ObjectInternal::Generator(s) => s,
            _ => return Err(RawJsError::type_error("not a generator")),
        };
        match state.status {
            GeneratorStatus::Completed => {
                return Ok(make_iterator_result(vm, JsValue::Undefined, true));
            }
            GeneratorStatus::Executing => {
                return Err(RawJsError::type_error("Generator is already executing"));
            }
            _ => {}
        }
        state.status = GeneratorStatus::Executing;
        (
            state.status.clone(),
            state.chunk_index,
            state.ip,
            state.locals.clone(),
            state.upvalues.clone(),
            state.saved_stack.clone(),
            state.this_value.clone(),
        )
    };

    // Build a CallFrame. The this_value of the frame holds the generator
    // pointer so that exec_yield can find it.
    let base = vm.value_stack.len();

    // Restore saved stack segment.
    for sv in &saved_stack {
        vm.push(sv.clone());
    }

    // For SuspendedYield, push the received value onto the stack
    // (it becomes the result of the yield expression).
    let start_ip = if ip > 0 {
        // SuspendedYield: push value as result of yield expression
        vm.push(value);
        ip
    } else {
        // SuspendedStart: start from beginning
        0
    };

    let frame = CallFrame {
        chunk_index,
        ip: start_ip,
        base,
        locals,
        upvalues,
        this_value: JsValue::Object(gen_ptr.clone()),
    };
    vm.call_stack.push(frame);

    // Run until the frame yields or returns.
    let target_depth = vm.call_stack.len() - 1;
    let result = run_generator_frame(vm, target_depth);

    match result {
        Ok(gen_result) => Ok(gen_result),
        Err(err) => {
            // Generator threw — mark as completed.
            let mut gen_obj = gen_ptr.borrow_mut();
            if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                state.status = GeneratorStatus::Completed;
            }
            Err(err)
        }
    }
}

/// Run the interpreter for a generator frame until it yields or returns.
fn run_generator_frame(vm: &mut Vm, target_depth: usize) -> Result<JsValue> {
    loop {
        if vm.call_stack.len() <= target_depth {
            // Frame was popped by exec_yield or exec_return.
            // The result ({value, done}) is on the value_stack.
            return vm.pop().or(Ok(JsValue::Undefined));
        }

        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;

        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            // Implicit return undefined — generator is done.
            let frame = vm.call_stack.pop().unwrap();
            vm.value_stack.truncate(frame.base);

            // Mark generator as completed.
            if let JsValue::Object(ref gen_ptr) = frame.this_value {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                }
            }

            return Ok(make_iterator_result(vm, JsValue::Undefined, true));
        }

        let instruction = vm.chunks[chunk_index].instructions[ip];
        vm.call_stack[frame_idx].ip = ip + 1;

        match instruction {
            Instruction::Return => {
                // Generator return: pop return value, mark completed, return {value, done: true}
                let return_value = if vm.value_stack.len() > vm.call_stack.last().unwrap().base {
                    vm.pop()?
                } else {
                    JsValue::Undefined
                };

                let frame = vm.call_stack.pop().unwrap();
                vm.value_stack.truncate(frame.base);

                // Mark generator as completed.
                if let JsValue::Object(ref gen_ptr) = frame.this_value {
                    let mut gen_obj = gen_ptr.borrow_mut();
                    if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                        state.status = GeneratorStatus::Completed;
                    }
                }

                return Ok(make_iterator_result(vm, return_value, true));
            }
            Instruction::Yield => {
                // exec_yield pops the frame and pushes {value, done: false}
                exec_yield(vm)?;
                // The result is now on the stack; pop it to return.
                return vm.pop().or(Ok(JsValue::Undefined));
            }
            _ => {
                // Normal instruction dispatch.
                match execute_instruction(vm, instruction) {
                    Ok(Some(value)) => {
                        // Top-level return within a generator (shouldn't happen normally).
                        return Ok(make_iterator_result(vm, value, true));
                    }
                    Ok(None) => {}
                    Err(err) => {
                        if !unwind_exception(vm, &err)? {
                            return Err(err);
                        }
                    }
                }
            }
        }
    }
}

/// Implement generator `.return(value)`.
fn generator_return(
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    vm: &mut Vm,
    value: JsValue,
) -> JsValue {
    let mut gen_obj = gen_ptr.borrow_mut();
    if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
        state.status = GeneratorStatus::Completed;
    }
    drop(gen_obj);
    make_iterator_result(vm, value, true)
}

/// Implement generator `.throw(error)`.
fn generator_throw(
    _vm: &mut Vm,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    error: JsValue,
) -> Result<JsValue> {
    let status = {
        let gen_obj = gen_ptr.borrow();
        match &gen_obj.internal {
            ObjectInternal::Generator(state) => state.status.clone(),
            _ => return Err(RawJsError::type_error("not a generator")),
        }
    };

    match status {
        GeneratorStatus::SuspendedStart | GeneratorStatus::Completed => {
            // Mark as completed and throw.
            {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                }
            }
            Err(RawJsError::internal_error(format!("{}", error)))
        }
        GeneratorStatus::SuspendedYield => {
            // Resume the generator and throw the error into it.
            // For simplicity, mark as completed and throw.
            {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                }
            }
            Err(RawJsError::internal_error(format!("{}", error)))
        }
        GeneratorStatus::Executing => Err(RawJsError::type_error("Generator is already executing")),
    }
}

/// Create a `{ value, done }` iterator result object.
fn make_iterator_result(vm: &mut Vm, value: JsValue, done: bool) -> JsValue {
    let mut obj = JsObject::ordinary();
    obj.set_property("value".to_string(), value);
    obj.set_property("done".to_string(), JsValue::Boolean(done));
    JsValue::Object(vm.heap.alloc(obj))
}

// ---------------------------------------------------------------------------
// Async function stubs (Phase 2)
// ---------------------------------------------------------------------------

/// Create and execute an async function call.
///
/// Async functions return a Promise. The function body runs synchronously
/// until the first `Await` or `Return`.
fn exec_async_call(
    vm: &mut Vm,
    chunk_index: usize,
    args: &[JsValue],
    upvalues: Vec<Upvalue>,
    this_value: JsValue,
) -> Result<()> {
    // Create the result promise that the caller will receive.
    let promise_ptr = vm.heap.alloc(JsObject::promise());
    if let Some(ref proto) = vm.promise_prototype {
        promise_ptr.borrow_mut().prototype = Some(proto.clone());
    }

    // Set up locals with arguments.
    let param_count = vm.chunks[chunk_index].param_count as usize;
    let local_count = vm.chunks[chunk_index].local_count as usize;
    let slot_count = local_count.max(param_count);
    let mut locals = vec![JsValue::Undefined; slot_count];
    for (i, arg) in args.iter().enumerate() {
        if i < slot_count {
            locals[i] = arg.clone();
        }
    }

    // Create an internal generator state for the async function.
    // We store the result_promise so that exec_await and Return can
    // resolve/reject it.
    let state = GeneratorState {
        status: GeneratorStatus::SuspendedStart,
        chunk_index,
        ip: 0,
        locals,
        upvalues,
        saved_stack: Vec::new(),
        this_value,
        result_promise: Some(promise_ptr.clone()),
        saved_try_stack: Vec::new(),
    };

    let mut gen_obj = JsObject::ordinary();
    gen_obj.internal = ObjectInternal::Generator(state);
    let gen_ptr = vm.heap.alloc(gen_obj);

    // Run the async function body synchronously until it hits Await or returns.
    let run_result = async_resume(vm, &gen_ptr, JsValue::Undefined, false);

    match run_result {
        Ok(()) => {} // The async function suspended or completed.
        Err(err) => {
            // The async function threw before first await — reject the promise.
            let reason = vm
                .thrown_value
                .take()
                .unwrap_or_else(|| JsValue::string(err.message.as_str()));
            rawjs_runtime::builtins::reject_promise_with_heap(&promise_ptr, reason, &mut vm.heap);
        }
    }

    vm.push(JsValue::Object(promise_ptr));
    Ok(())
}

/// Resume an async function's internal generator.
///
/// If `is_throw` is true, the resumed value is thrown into the generator
/// (used when the awaited promise is rejected).
fn async_resume(
    vm: &mut Vm,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    value: JsValue,
    is_throw: bool,
) -> Result<()> {
    // Extract generator state.
    let (chunk_index, ip, locals, upvalues, saved_stack, result_promise, saved_try_entries) = {
        let mut gen_obj = gen_ptr.borrow_mut();
        let state = match &mut gen_obj.internal {
            ObjectInternal::Generator(s) => s,
            _ => return Err(RawJsError::type_error("not an async generator")),
        };
        match state.status {
            GeneratorStatus::Completed => return Ok(()),
            GeneratorStatus::Executing => {
                return Err(RawJsError::type_error(
                    "async function is already executing",
                ));
            }
            _ => {}
        }
        state.status = GeneratorStatus::Executing;
        (
            state.chunk_index,
            state.ip,
            state.locals.clone(),
            state.upvalues.clone(),
            state.saved_stack.clone(),
            state.result_promise.clone(),
            std::mem::take(&mut state.saved_try_stack),
        )
    };

    let base = vm.value_stack.len();

    // Restore saved stack segment.
    for sv in &saved_stack {
        vm.push(sv.clone());
    }

    // Push the resumed value onto the stack (result of await expression).
    let start_ip = if ip > 0 {
        vm.push(value.clone());
        ip
    } else {
        0
    };

    let frame = CallFrame {
        chunk_index,
        ip: start_ip,
        base,
        locals,
        upvalues,
        this_value: JsValue::Object(gen_ptr.clone()),
    };
    vm.call_stack.push(frame);

    // Restore try_stack entries that were saved when the async function suspended.
    for (catch_ip, finally_ip, stack_depth, call_depth) in saved_try_entries {
        vm.try_stack.push(TryContext {
            catch_ip,
            finally_ip,
            stack_depth,
            call_depth,
        });
    }

    // Run until Await, Return, or exception.
    let target_depth = vm.call_stack.len() - 1;

    // If is_throw, simulate throwing the value inside the async function.
    // This allows try/catch blocks inside the async function to handle the error.
    if is_throw {
        vm.thrown_value = Some(value);
        let throw_err = RawJsError::internal_error("async throw");
        if unwind_exception(vm, &throw_err)? {
            // There was a try/catch that handled the exception.
            // Continue executing the async frame from the catch handler.
        } else {
            // No try/catch — reject the result promise.
            let reason = vm.thrown_value.take().unwrap_or(JsValue::Undefined);
            while vm.call_stack.len() > target_depth {
                let frame = vm.call_stack.pop().unwrap();
                vm.value_stack.truncate(frame.base);
            }
            {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                }
            }
            if let Some(ref promise) = result_promise {
                rawjs_runtime::builtins::reject_promise_with_heap(promise, reason, &mut vm.heap);
            }
            return Ok(());
        }
    }

    let result = run_async_frame(vm, target_depth, gen_ptr, &result_promise);

    match result {
        Ok(()) => Ok(()),
        Err(err) => {
            // Async function threw — reject the result promise.
            let reason = vm
                .thrown_value
                .take()
                .unwrap_or_else(|| JsValue::string(err.message.as_str()));

            // Clean up: pop the async frame if it's still on the stack.
            while vm.call_stack.len() > target_depth {
                let frame = vm.call_stack.pop().unwrap();
                vm.value_stack.truncate(frame.base);
            }

            {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                }
            }
            if let Some(ref promise) = result_promise {
                rawjs_runtime::builtins::reject_promise_with_heap(promise, reason, &mut vm.heap);
            }
            Ok(())
        }
    }
}

/// Run the interpreter for an async function frame until it awaits, returns, or throws.
fn run_async_frame(
    vm: &mut Vm,
    target_depth: usize,
    gen_ptr: &rawjs_runtime::GcPtr<JsObject>,
    result_promise: &Option<rawjs_runtime::GcPtr<JsObject>>,
) -> Result<()> {
    let try_stack_base = vm.try_stack.len();
    loop {
        if vm.call_stack.len() <= target_depth {
            // Frame was popped (by exec_return or exec_await).
            return Ok(());
        }

        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;

        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            // Implicit return undefined — async function is done.
            let frame = vm.call_stack.pop().unwrap();
            vm.value_stack.truncate(frame.base);

            // Mark generator as completed and resolve the promise.
            {
                let mut gen_obj = gen_ptr.borrow_mut();
                if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                    state.status = GeneratorStatus::Completed;
                }
            }
            if let Some(ref promise) = result_promise {
                rawjs_runtime::builtins::resolve_promise_with_heap(
                    promise,
                    JsValue::Undefined,
                    &mut vm.heap,
                );
            }
            return Ok(());
        }

        let instruction = vm.chunks[chunk_index].instructions[ip];
        vm.call_stack[frame_idx].ip = ip + 1;

        match instruction {
            Instruction::Return => {
                // Async return: resolve the result promise.
                let return_value = if vm.value_stack.len() > vm.call_stack.last().unwrap().base {
                    vm.pop()?
                } else {
                    JsValue::Undefined
                };

                let frame = vm.call_stack.pop().unwrap();
                vm.value_stack.truncate(frame.base);

                // Mark generator as completed.
                {
                    let mut gen_obj = gen_ptr.borrow_mut();
                    if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                        state.status = GeneratorStatus::Completed;
                    }
                }

                // Resolve the result promise with the return value.
                if let Some(ref promise) = result_promise {
                    rawjs_runtime::builtins::resolve_promise_with_heap(
                        promise,
                        return_value,
                        &mut vm.heap,
                    );
                }
                return Ok(());
            }
            Instruction::Await => {
                // Save the frame state and schedule resumption.
                let awaited_value = vm.pop()?;
                let frame = vm.call_stack.pop().unwrap();
                let saved_stack: Vec<JsValue> = vm.value_stack.drain(frame.base..).collect();

                // Save try_stack entries that belong to this async frame.
                let saved_try_entries: Vec<_> = vm
                    .try_stack
                    .drain(try_stack_base..)
                    .map(|ctx| {
                        (
                            ctx.catch_ip,
                            ctx.finally_ip,
                            ctx.stack_depth,
                            ctx.call_depth,
                        )
                    })
                    .collect();

                // Save state back into the generator.
                {
                    let mut gen_obj = gen_ptr.borrow_mut();
                    if let ObjectInternal::Generator(ref mut state) = gen_obj.internal {
                        state.status = GeneratorStatus::SuspendedYield;
                        state.ip = frame.ip; // Already past Await
                        state.locals = frame.locals;
                        state.upvalues = frame.upvalues;
                        state.saved_stack = saved_stack;
                        state.saved_try_stack = saved_try_entries;
                    }
                }

                // If awaited_value is a Promise, register .then callbacks.
                // If not, treat as immediately resolved.
                let is_promise = match &awaited_value {
                    JsValue::Object(ptr) => {
                        matches!(ptr.borrow().internal, ObjectInternal::Promise(_))
                    }
                    _ => false,
                };

                if is_promise {
                    // Create native onFulfilled/onRejected callbacks that
                    // enqueue microtasks to resume the async function.
                    let gen_ptr_clone = gen_ptr.clone();
                    let on_fulfilled = create_async_resume_fn(
                        vm,
                        gen_ptr_clone.clone(),
                        false, // not throw
                    );
                    let on_rejected = create_async_resume_fn(
                        vm,
                        gen_ptr_clone,
                        true, // throw
                    );

                    // Call promise.then(onFulfilled, onRejected)
                    let then_args = [JsValue::Object(on_fulfilled), JsValue::Object(on_rejected)];
                    rawjs_runtime::builtins::promise_then_internal(
                        &mut vm.heap,
                        &awaited_value,
                        &then_args,
                    )?;
                } else {
                    // Not a promise — resume immediately with the value.
                    // Enqueue a microtask to resume.
                    let gen_ptr_clone = gen_ptr.clone();
                    let on_fulfilled = create_async_resume_fn(vm, gen_ptr_clone, false);
                    vm.heap.pending_microtasks.push(rawjs_runtime::MicroTask {
                        callback: JsValue::Object(on_fulfilled),
                        arg: awaited_value,
                    });
                }

                return Ok(());
            }
            _ => {
                // Normal instruction dispatch.
                match execute_instruction(vm, instruction) {
                    Ok(Some(_value)) => {
                        // Top-level return within async function.
                        return Ok(());
                    }
                    Ok(None) => {}
                    Err(err) => {
                        if !unwind_exception(vm, &err)? {
                            return Err(err);
                        }
                    }
                }
            }
        }
    }
}

/// Create a native function that, when called, resumes the async generator.
///
/// The resume function is stored with `__async_gen_ptr__` property pointing
/// to the generator's GcPtr so the VM can find it.
fn create_async_resume_fn(
    vm: &mut Vm,
    gen_ptr: rawjs_runtime::GcPtr<JsObject>,
    is_throw: bool,
) -> rawjs_runtime::GcPtr<JsObject> {
    let callback: rawjs_runtime::NativeFn = if is_throw {
        async_reject_callback
    } else {
        async_fulfill_callback
    };

    let name = if is_throw {
        "asyncReject"
    } else {
        "asyncFulfill"
    };
    let mut fn_obj = JsObject::native_function(name, callback);
    fn_obj.set_property("__async_gen_ptr__".to_string(), JsValue::Object(gen_ptr));
    vm.heap.alloc(fn_obj)
}

/// Execute an `Await` instruction.
///
/// Dispose a resource stored in a local slot by calling its [Symbol.dispose]() method.
/// If the local is null or undefined, this is a no-op.
pub(crate) fn exec_dispose_resource(vm: &mut Vm, slot: u16) -> Result<()> {
    let frame = vm
        .call_stack
        .last()
        .ok_or_else(|| RawJsError::internal_error("DisposeResource: no call frame"))?;
    let resource = frame.locals[slot as usize].clone();

    // null/undefined -> no-op
    if resource.is_null() || resource.is_undefined() {
        return Ok(());
    }

    // Look up [Symbol.dispose] on the resource.
    let dispose_fn = match &resource {
        JsValue::Object(ptr) => ptr
            .borrow()
            .get_symbol_property(rawjs_runtime::value::SYMBOL_DISPOSE),
        _ => JsValue::Undefined,
    };

    if dispose_fn.is_undefined() {
        return Err(RawJsError::type_error(
            "Resource does not have a [Symbol.dispose] method",
        ));
    }

    // Call the dispose method with `this = resource`.
    vm.push(dispose_fn);
    exec_call(vm, 0, resource)?;

    // Discard the return value if any.
    if vm.value_stack.len() > vm.call_stack.last().map_or(0, |f| f.base) {
        let _ = vm.pop()?;
    }

    Ok(())
}

/// This is called from the normal instruction dispatch for async frames
/// that are running within the main interpreter loop (e.g., during the
/// initial synchronous phase of an async function).
pub(crate) fn exec_await(vm: &mut Vm) -> Result<Option<JsValue>> {
    // The Await instruction is handled directly by run_async_frame.
    // If we reach here, it means an await was used outside of an async frame,
    // which should not happen (caught by the compiler).
    let _ = vm.pop()?;
    Err(RawJsError::internal_error(
        "Await instruction reached outside async frame",
    ))
}

/// Native callback for async fulfill (called when awaited promise resolves).
fn async_fulfill_callback(
    heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> rawjs_common::Result<JsValue> {
    let value = args.first().cloned().unwrap_or(JsValue::Undefined);

    // Find the generator ptr from the calling function.
    let gen_ptr = {
        let calling_fn = heap
            .calling_fn
            .as_ref()
            .ok_or_else(|| RawJsError::internal_error("async callback: no calling_fn"))?;
        let fn_obj = calling_fn.borrow();
        let gen_val = fn_obj.get_property("__async_gen_ptr__");
        match gen_val {
            JsValue::Object(ptr) => ptr,
            _ => {
                return Err(RawJsError::internal_error(
                    "async callback: __async_gen_ptr__ not found",
                ))
            }
        }
    };

    // We can't directly resume the generator from a native function because
    // we don't have access to the VM. Instead, we enqueue a special microtask
    // that the VM will pick up to resume the async function.
    //
    // Store the resume info as a property on the generator.
    gen_ptr
        .borrow_mut()
        .set_property("__async_resume_value__".to_string(), value);
    gen_ptr.borrow_mut().set_property(
        "__async_resume_throw__".to_string(),
        JsValue::Boolean(false),
    );
    gen_ptr.borrow_mut().set_property(
        "__async_resume_pending__".to_string(),
        JsValue::Boolean(true),
    );

    Ok(JsValue::Undefined)
}

/// Native callback for async reject (called when awaited promise rejects).
fn async_reject_callback(
    heap: &mut Heap,
    _this: &JsValue,
    args: &[JsValue],
) -> rawjs_common::Result<JsValue> {
    let value = args.first().cloned().unwrap_or(JsValue::Undefined);

    let gen_ptr = {
        let calling_fn = heap
            .calling_fn
            .as_ref()
            .ok_or_else(|| RawJsError::internal_error("async callback: no calling_fn"))?;
        let fn_obj = calling_fn.borrow();
        let gen_val = fn_obj.get_property("__async_gen_ptr__");
        match gen_val {
            JsValue::Object(ptr) => ptr,
            _ => {
                return Err(RawJsError::internal_error(
                    "async callback: __async_gen_ptr__ not found",
                ))
            }
        }
    };

    gen_ptr
        .borrow_mut()
        .set_property("__async_resume_value__".to_string(), value);
    gen_ptr
        .borrow_mut()
        .set_property("__async_resume_throw__".to_string(), JsValue::Boolean(true));
    gen_ptr.borrow_mut().set_property(
        "__async_resume_pending__".to_string(),
        JsValue::Boolean(true),
    );

    Ok(JsValue::Undefined)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_js_add_numbers() {
        let result = js_add(&JsValue::Number(1.0), &JsValue::Number(2.0));
        assert_eq!(result, JsValue::Number(3.0));
    }

    #[test]
    fn test_js_add_strings() {
        let result = js_add(&JsValue::string("foo"), &JsValue::string("bar"));
        assert_eq!(result, JsValue::string("foobar"));
    }

    #[test]
    fn test_js_add_string_number() {
        let result = js_add(&JsValue::string("x"), &JsValue::Number(1.0));
        assert_eq!(result, JsValue::string("x1"));
    }

    #[test]
    fn test_js_add_number_string() {
        let result = js_add(&JsValue::Number(1.0), &JsValue::string("x"));
        assert_eq!(result, JsValue::string("1x"));
    }

    #[test]
    fn test_constant_to_value() {
        assert_eq!(
            constant_to_value(&Constant::Number(3.14)),
            JsValue::Number(3.14)
        );
        assert_eq!(
            constant_to_value(&Constant::String("hi".into())),
            JsValue::string("hi")
        );
        assert_eq!(
            constant_to_value(&Constant::Boolean(true)),
            JsValue::Boolean(true)
        );
        assert_eq!(constant_to_value(&Constant::Null), JsValue::Null);
        assert_eq!(constant_to_value(&Constant::Undefined), JsValue::Undefined);
    }

    #[test]
    fn test_get_constant_string_ok() {
        let mut chunk = Chunk::new("test");
        chunk.add_constant(Constant::String("hello".into()));
        assert_eq!(get_constant_string(&chunk, 0).unwrap(), "hello");
    }

    #[test]
    fn test_get_constant_string_err() {
        let mut chunk = Chunk::new("test");
        chunk.add_constant(Constant::Number(1.0));
        assert!(get_constant_string(&chunk, 0).is_err());
    }
}
