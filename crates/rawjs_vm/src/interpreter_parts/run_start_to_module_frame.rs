use rawjs_bytecode::{Chunk, Constant, Instruction};

use rawjs_common::{RawJsError, Result};

use rawjs_parser::parse as parse_program;

use rawjs_runtime::{
    GcPtr, GeneratorState, GeneratorStatus, Heap, JsObject, JsValue, ObjectInternal, PromiseStatus,
    Property, Upvalue,
};

use crate::{CallFrame, TryContext, Vm, JIT_THRESHOLD};

/// Run the interpreter loop until the top-level frame returns or an
/// unhandled exception occurs.
pub fn run(vm: &mut Vm) -> Result<JsValue> {
    let base_depth = vm.call_stack.len().saturating_sub(1);
    loop {
        if vm.call_stack.is_empty() {
            drain_microtasks(vm)?;
            return Ok(JsValue::Undefined);
        }
        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;
        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            let implicit_return = finish_frame_return(vm, JsValue::Undefined);
            if vm.call_stack.is_empty() {
                drain_microtasks(vm)?;
                return Ok(implicit_return);
            }
            vm.push(implicit_return);
            if vm.call_stack.len() <= base_depth + 1 {
                drain_microtasks(vm)?;
            }
            continue;
        }
        let instruction = vm.chunks[chunk_index].instructions[ip];
        vm.call_stack[frame_idx].ip = ip + 1;
        match execute_instruction(vm, instruction) {
            Ok(Some(value)) => {
                drain_microtasks(vm)?;
                return Ok(value);
            }
            Ok(None) => {}
            Err(err) => {
                ensure_thrown_value(vm, &err);
                if !unwind_exception(vm, &err)? {
                    return Err(err);
                }
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
    let target_depth = vm.call_stack.len() - 1;
    loop {
        if vm.call_stack.len() <= target_depth {
            return Ok(());
        }
        let frame_idx = vm.call_stack.len() - 1;
        let chunk_index = vm.call_stack[frame_idx].chunk_index;
        let ip = vm.call_stack[frame_idx].ip;
        let instr_count = vm.chunks[chunk_index].instructions.len();
        if ip >= instr_count {
            let implicit_return = finish_frame_return(vm, JsValue::Undefined);
            if vm.call_stack.len() <= target_depth {
                return Ok(());
            }
            vm.push(implicit_return);
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
                ensure_thrown_value(vm, &err);
                if !unwind_exception(vm, &err)? {
                    return Err(err);
                }
            }
        }
    }
}
