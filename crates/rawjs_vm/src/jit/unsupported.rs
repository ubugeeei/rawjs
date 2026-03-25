use crate::Vm;
use rawjs_bytecode::Chunk;

pub struct JitCompiler;

impl JitCompiler {
    /// Unsupported backends fall back to the interpreter.
    pub fn compile(_chunk: &Chunk) -> Option<JitFunction> {
        None
    }
}

/// Stub JIT function that can never be constructed on unsupported backends.
pub struct JitFunction {
    _private: (),
}

impl JitFunction {
    /// # Safety
    ///
    /// This stub exists only to satisfy the shared JIT interface on
    /// unsupported targets. It must never be called for real execution.
    pub unsafe fn call_vm(&self, _vm: *mut Vm) -> u32 {
        1
    }
}
