#[allow(unused_imports)]
use super::Parser;
#[allow(unused_imports)]
use rawjs_ast::*;
#[allow(unused_imports)]
use rawjs_common::{RawJsError, Result};
#[allow(unused_imports)]
use rawjs_lexer::TokenKind;

#[path = "expressions/parse_expression_to_async_arrow_function.rs"]
mod parse_expression_to_async_arrow_function;
#[allow(unused_imports)]
use self::parse_expression_to_async_arrow_function::*;
#[path = "expressions/parse_assignment_operator_to_bitwise_and_expression.rs"]
mod parse_assignment_operator_to_bitwise_and_expression;
#[allow(unused_imports)]
use self::parse_assignment_operator_to_bitwise_and_expression::*;
#[path = "expressions/parse_equality_expression_to_exponentiation_expression.rs"]
mod parse_equality_expression_to_exponentiation_expression;
#[allow(unused_imports)]
use self::parse_equality_expression_to_exponentiation_expression::*;
#[path = "expressions/parse_unary_expression_to_postfix_expression.rs"]
mod parse_unary_expression_to_postfix_expression;
#[allow(unused_imports)]
use self::parse_unary_expression_to_postfix_expression::*;
#[path = "expressions/parse_left_hand_side_expression_to_new_expression.rs"]
mod parse_left_hand_side_expression_to_new_expression;
#[allow(unused_imports)]
use self::parse_left_hand_side_expression_to_new_expression::*;
#[path = "expressions/parse_arguments_to_primary_expression.rs"]
mod parse_arguments_to_primary_expression;
#[allow(unused_imports)]
use self::parse_arguments_to_primary_expression::*;
#[path = "expressions/parse_import_primary_expression_to_array_expression.rs"]
mod parse_import_primary_expression_to_array_expression;
#[allow(unused_imports)]
use self::parse_import_primary_expression_to_array_expression::*;
#[path = "expressions/parse_object_expression.rs"]
mod parse_object_expression;
#[allow(unused_imports)]
use self::parse_object_expression::*;
