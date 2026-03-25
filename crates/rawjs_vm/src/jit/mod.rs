//! Stub-based baseline JIT compiler for the rawjs VM.
//!
//! Native code generation is implemented for `aarch64`, `x86_64`, and
//! `riscv64`, all using the same stub-call baseline strategy.

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
#[allow(dead_code)]
mod unsupported;

#[allow(dead_code)]
mod x86_64;

#[allow(dead_code)]
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

#[cfg(all(
    test,
    any(
        target_arch = "aarch64",
        target_arch = "x86_64",
        target_arch = "riscv64"
    ),
))]
mod tests;
