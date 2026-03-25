mod expressions;
mod statements;
pub(crate) use crate::chunk::{Chunk, Constant};
pub(crate) use crate::opcode::Instruction;
pub(crate) use rawjs_ast::*;
pub(crate) use rawjs_common::{RawJsError, Result};

#[path = "shared/local_to_compiler.rs"]
mod local_to_compiler;
pub(crate) use self::local_to_compiler::*;
pub use self::local_to_compiler::{Compiler, UpvalueDesc};
#[path = "shared/compile_program_to_resolve_local.rs"]
mod compile_program_to_resolve_local;
pub(crate) use self::compile_program_to_resolve_local::*;
#[path = "shared/resolve_local_storage_to_hoist_var_declarations.rs"]
mod resolve_local_storage_to_hoist_var_declarations;
pub(crate) use self::resolve_local_storage_to_hoist_var_declarations::*;
#[path = "shared/hoist_var_declarations_in_statement_to_pop_loop_context_and_patch_breaks.rs"]
mod hoist_var_declarations_in_statement_to_pop_loop_context_and_patch_breaks;
pub(crate) use self::hoist_var_declarations_in_statement_to_pop_loop_context_and_patch_breaks::*;
#[path = "shared/has_use_strict_directive_to_pattern_to_name.rs"]
mod has_use_strict_directive_to_pattern_to_name;
pub(crate) use self::has_use_strict_directive_to_pattern_to_name::*;
#[cfg(test)]
mod tests;
