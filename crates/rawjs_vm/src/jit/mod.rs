//! Stub-based Baseline JIT compiler for the rawjs VM (macOS Apple Silicon).
//!
//! Each bytecode instruction is compiled to a `bl` (branch-with-link) call
//! to a Rust `extern "C"` stub function that performs the actual work on
//! the VM's value_stack and locals.
//!
//! # ABI
//!
//! JIT compiled function signature: `extern "C" fn(*mut Vm) -> u32`
//!   - x19 (callee-saved): `*mut Vm` (saved in prologue)
//!   - Return value: w0 = 0 (success) / 1 (error) / 2 (frame done via Return)
//!
//! # Memory model (macOS)
//!
//! Apple Silicon requires the MAP_JIT flag for pages that are both writable
//! and executable.  We use `pthread_jit_write_protect_np(0)` to enable
//! writing, emit the code, flush the instruction cache with
//! `sys_icache_invalidate`, then re-enable write protection with
//! `pthread_jit_write_protect_np(1)`.

use crate::Vm;
use rawjs_bytecode::Chunk;
#[cfg(target_arch = "aarch64")]
use rawjs_bytecode::Instruction;

pub mod stubs;

// =========================================================================
// aarch64 implementation
// =========================================================================

#[cfg(target_arch = "aarch64")]
mod aarch64;

#[cfg(target_arch = "aarch64")]
use aarch64 as inner;

// =========================================================================
// Stub implementation for non-aarch64 platforms
// =========================================================================

#[cfg(not(target_arch = "aarch64"))]
mod inner {
    use super::*;

    pub struct JitCompiler;

    impl JitCompiler {
        /// On non-aarch64 platforms, compilation always fails.
        pub fn compile(_chunk: &Chunk) -> Option<JitFunction> {
            None
        }
    }

    /// Stub JitFunction that can never be constructed on non-aarch64.
    pub struct JitFunction {
        _private: (),
    }

    impl JitFunction {
        pub unsafe fn call_vm(&self, _vm: *mut Vm) -> u32 {
            1
        }
    }
}

// =========================================================================
// Re-exports
// =========================================================================

pub use inner::JitCompiler;
pub use inner::JitFunction;

// =========================================================================
// Tests (aarch64 only -- the JIT stub always returns None)
// =========================================================================

#[cfg(all(test, target_arch = "aarch64"))]
mod tests;
