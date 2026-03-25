pub(super) fn settle_microtask_target(
    vm: &mut Vm,
    target_promise: Option<rawjs_runtime::GcPtr<JsObject>>,
    result: Result<JsValue>,
) -> Result<()> {
    let Some(target_promise) = target_promise else {
        let _ = result?;
        return Ok(());
    };
    match result {
        Ok(value) => {
            rawjs_runtime::builtins::resolve_promise_with_heap(
                &target_promise,
                value,
                &mut vm.heap,
            );
        }
        Err(err) => {
            let reason = vm
                .thrown_value
                .take()
                .unwrap_or_else(|| JsValue::string(err.message.as_str()));
            rawjs_runtime::builtins::reject_promise_with_heap(
                &target_promise,
                reason,
                &mut vm.heap,
            );
        }
    }
    Ok(())
}

/// Run a Promise executor frame to completion.
/// Unlike run_microtask_frame, errors from the executor are caught (not unwound)
/// so the caller can reject the promise.
pub(super) fn run_executor_frame(vm: &mut Vm, target_depth: usize) -> Result<JsValue> {
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
                ensure_thrown_value(vm, &err);
                if unwind_exception(vm, &err)? {
                    continue;
                }
                return Err(err);
            }
        }
        if vm.call_stack.len() <= target_depth {
            return Ok(JsValue::Undefined);
        }
    }
}

/// Run a single microtask frame to completion.
pub(super) fn run_microtask_frame(vm: &mut Vm) -> Result<JsValue> {
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
        if vm.call_stack.len() <= target_depth {
            return Ok(JsValue::Undefined);
        }
    }
}

pub(super) enum InstructionOutcome {
    Unhandled,
    Handled(Option<JsValue>),
}

use super::*;
