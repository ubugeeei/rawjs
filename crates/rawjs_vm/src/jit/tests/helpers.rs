use crate::jit::JitCompiler;
use rawjs_bytecode::{Chunk, Instruction};
use rawjs_runtime::JsValue;

pub(super) fn jit_execute_via_vm(chunk: Chunk, args: &[JsValue]) -> JsValue {
    let function = JitCompiler::compile(&chunk).expect("compilation should succeed");

    let mut vm = crate::Vm::new();
    let chunk_index = vm.add_chunk(chunk);
    let base_chunk = {
        let mut chunk = Chunk::new("<base>");
        chunk.emit(Instruction::Return);
        chunk
    };
    let base_index = vm.add_chunk(base_chunk);
    vm.call_stack.push(crate::CallFrame {
        chunk_index: base_index,
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

    let param_count = vm.chunks[chunk_index].param_count as usize;
    let local_count = vm.chunks[chunk_index].local_count as usize;
    let slot_count = local_count.max(param_count);
    let mut locals = vec![JsValue::Undefined; slot_count];
    for (index, arg) in args.iter().enumerate() {
        if index < slot_count {
            locals[index] = arg.clone();
        }
    }

    vm.call_stack.push(crate::CallFrame {
        chunk_index,
        ip: 0,
        base: vm.value_stack.len(),
        locals,
        arguments: args.to_vec(),
        arguments_object: None,
        callee: None,
        is_strict: vm.chunks[chunk_index].is_strict,
        upvalues: Vec::new(),
        this_value: JsValue::Undefined,
    });

    let result = unsafe { function.call_vm(&mut vm as *mut crate::Vm) };
    assert_eq!(result, 0, "JIT execution should succeed (got {result})");
    vm.value_stack.pop().unwrap_or(JsValue::Undefined)
}
