//! Stub-based baseline JIT compiler for the rawjs VM.
//!
//! Native code generation is currently implemented for `aarch64`.
//! `x86_64` and `riscv64` are explicit targets in the build matrix, but
//! still fall back to the interpreter until dedicated backends land.

pub mod stubs;

#[cfg(target_arch = "aarch64")]
mod aarch64;
#[cfg(any(
    target_arch = "x86_64",
    target_arch = "riscv64",
    not(any(
        target_arch = "aarch64",
        target_arch = "x86_64",
        target_arch = "riscv64",
    )),
))]
mod unsupported;

#[cfg(target_arch = "x86_64")]
mod x86_64;

#[cfg(target_arch = "riscv64")]
mod riscv64;

#[cfg(target_arch = "aarch64")]
use aarch64 as inner;

#[cfg(target_arch = "x86_64")]
use x86_64 as inner;

#[cfg(target_arch = "riscv64")]
use riscv64 as inner;

#[cfg(not(any(
    target_arch = "aarch64",
    target_arch = "x86_64",
    target_arch = "riscv64",
)))]
use unsupported as inner;

pub use inner::JitCompiler;
pub use inner::JitFunction;

#[cfg(all(test, target_arch = "aarch64"))]
mod tests;
