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
    pub unsafe fn call_vm(&self, _vm: *mut Vm) -> u32 {
        1
    }
}
