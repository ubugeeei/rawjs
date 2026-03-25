pub(crate) use super::{expression_to_property_name, Compiler, LocalStorage};
pub(crate) use crate::opcode::Instruction;
pub(crate) use rawjs_ast::*;
pub(crate) use rawjs_common::{RawJsError, Result};

mod compile_statement_to_variable_declaration;
pub(crate) use self::compile_statement_to_variable_declaration::*;
mod compile_destructuring_declaration_to_do_while;
pub(crate) use self::compile_destructuring_declaration_to_do_while::*;
mod compile_for_start_to_in;
pub(crate) use self::compile_for_start_to_in::*;
mod compile_for_of_to_continue;
pub(crate) use self::compile_for_of_to_continue::*;
mod compile_switch_to_try;
pub(crate) use self::compile_switch_to_try::*;
mod compile_labeled_to_class_body;
pub(crate) use self::compile_labeled_to_class_body::*;
mod compile_import_to_export;
pub(crate) use self::compile_import_to_export::*;
