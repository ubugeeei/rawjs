#[allow(unused_imports)]
use super::{
    expression_to_property_name, pattern_to_name, Compiler, LocalStorage, ParentLocal,
    ParentUpvalue,
};
pub(crate) use crate::chunk::Constant;
pub(crate) use crate::opcode::Instruction;
pub(crate) use rawjs_ast::*;
pub(crate) use rawjs_common::{RawJsError, Result};

mod compile_expression_to_identifier_load;
pub(crate) use self::compile_expression_to_identifier_load::*;
mod compile_identifier_store_to_emit_optional_jump;
pub(crate) use self::compile_identifier_store_to_emit_optional_jump::*;
mod emit_optional_undefined_result_to_compile_arrow_function;
pub(crate) use self::emit_optional_undefined_result_to_compile_arrow_function::*;
mod compile_function_body_to_await;
pub(crate) use self::compile_function_body_to_await::*;
mod compile_unary_to_update;
pub(crate) use self::compile_unary_to_update::*;
mod compile_binary_to_logical;
pub(crate) use self::compile_binary_to_logical::*;
mod compile_assignment_to_import_expression;
pub(crate) use self::compile_assignment_to_import_expression::*;
mod compile_call_to_sequence;
pub(crate) use self::compile_call_to_sequence::*;
