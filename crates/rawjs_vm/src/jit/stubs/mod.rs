//! Stub functions for the stub-based baseline JIT.
//!
//! Each function is `extern "C"` and operates directly on the VM state.

mod calls;
mod common;
mod comparisons;
mod control;
mod iteration;
mod modules;
mod objects;
mod operators;
mod stack;

pub(crate) use common::{chunk_string_constant, finish, pop_pair, pop_value, set_error, vm_ref};

pub use calls::*;
pub use comparisons::*;
pub use control::*;
pub use iteration::*;
pub use modules::*;
pub use objects::*;
pub use operators::*;
pub use stack::*;
