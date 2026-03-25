mod expressions;
mod statements;
#[allow(unused_imports)]
use crate::chunk::{Chunk, Constant};
#[allow(unused_imports)]
use crate::opcode::Instruction;
#[allow(unused_imports)]
use rawjs_ast::*;
#[allow(unused_imports)]
use rawjs_common::{RawJsError, Result};

#[path = "shared/local_to_compiler.rs"]
mod local_to_compiler;
#[allow(unused_imports)]
use self::local_to_compiler::*;
#[allow(unused_imports)]
pub use self::local_to_compiler::{Compiler, UpvalueDesc};
#[path = "shared/compile_program_to_resolve_local.rs"]
mod compile_program_to_resolve_local;
#[allow(unused_imports)]
use self::compile_program_to_resolve_local::*;
#[path = "shared/resolve_local_storage_to_hoist_var_declarations.rs"]
mod resolve_local_storage_to_hoist_var_declarations;
#[allow(unused_imports)]
use self::resolve_local_storage_to_hoist_var_declarations::*;
#[path = "shared/hoist_var_declarations_in_statement_to_pop_loop_context_and_patch_breaks.rs"]
mod hoist_var_declarations_in_statement_to_pop_loop_context_and_patch_breaks;
#[allow(unused_imports)]
use self::hoist_var_declarations_in_statement_to_pop_loop_context_and_patch_breaks::*;
#[path = "shared/has_use_strict_directive_to_pattern_to_name.rs"]
mod has_use_strict_directive_to_pattern_to_name;
#[allow(unused_imports)]
use self::has_use_strict_directive_to_pattern_to_name::*;
#[cfg(test)]
mod tests;
