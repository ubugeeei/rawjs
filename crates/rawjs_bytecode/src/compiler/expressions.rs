#[allow(unused_imports)]
use super::{
    expression_to_property_name, pattern_to_name, Compiler, LocalStorage, ParentLocal,
    ParentUpvalue,
};
#[allow(unused_imports)]
use crate::chunk::Constant;
#[allow(unused_imports)]
use crate::opcode::Instruction;
#[allow(unused_imports)]
use rawjs_ast::*;
#[allow(unused_imports)]
use rawjs_common::{RawJsError, Result};

#[path = "expressions/compile_expression_to_identifier_load.rs"]
mod compile_expression_to_identifier_load;
#[allow(unused_imports)]
use self::compile_expression_to_identifier_load::*;
#[path = "expressions/compile_identifier_store_to_emit_optional_jump.rs"]
mod compile_identifier_store_to_emit_optional_jump;
#[allow(unused_imports)]
use self::compile_identifier_store_to_emit_optional_jump::*;
#[path = "expressions/emit_optional_undefined_result_to_compile_arrow_function.rs"]
mod emit_optional_undefined_result_to_compile_arrow_function;
#[allow(unused_imports)]
use self::emit_optional_undefined_result_to_compile_arrow_function::*;
#[path = "expressions/compile_function_body_to_await.rs"]
mod compile_function_body_to_await;
#[allow(unused_imports)]
use self::compile_function_body_to_await::*;
#[path = "expressions/compile_unary_to_update.rs"]
mod compile_unary_to_update;
#[allow(unused_imports)]
use self::compile_unary_to_update::*;
#[path = "expressions/compile_binary_to_logical.rs"]
mod compile_binary_to_logical;
#[allow(unused_imports)]
use self::compile_binary_to_logical::*;
#[path = "expressions/compile_assignment_to_import_expression.rs"]
mod compile_assignment_to_import_expression;
#[allow(unused_imports)]
use self::compile_assignment_to_import_expression::*;
#[path = "expressions/compile_call_to_sequence.rs"]
mod compile_call_to_sequence;
#[allow(unused_imports)]
use self::compile_call_to_sequence::*;
