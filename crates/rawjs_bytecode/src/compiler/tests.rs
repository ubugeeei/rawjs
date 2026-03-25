#[allow(unused_imports)]
use super::*;
#[allow(unused_imports)]
use rawjs_common::SourceLocation;

#[path = "tests/loc_to_compile_top_level_await_marks_program_async.rs"]
mod loc_to_compile_top_level_await_marks_program_async;
#[allow(unused_imports)]
use self::loc_to_compile_top_level_await_marks_program_async::*;
#[path = "tests/compile_await_in_sync_function_still_errors_to_binary_add.rs"]
mod compile_await_in_sync_function_still_errors_to_binary_add;
#[allow(unused_imports)]
use self::compile_await_in_sync_function_still_errors_to_binary_add::*;
#[path = "tests/compile_all_binary_ops_to_variable_no_init.rs"]
mod compile_all_binary_ops_to_variable_no_init;
#[allow(unused_imports)]
use self::compile_all_binary_ops_to_variable_no_init::*;
#[path = "tests/compile_variable_read_to_do_while_loop.rs"]
mod compile_variable_read_to_do_while_loop;
#[allow(unused_imports)]
use self::compile_variable_read_to_do_while_loop::*;
#[path = "tests/compile_for_loop_to_function_declaration.rs"]
mod compile_for_loop_to_function_declaration;
#[allow(unused_imports)]
use self::compile_for_loop_to_function_declaration::*;
#[path = "tests/compile_function_body_correct_to_computed_member.rs"]
mod compile_function_body_correct_to_computed_member;
#[allow(unused_imports)]
use self::compile_function_body_correct_to_computed_member::*;
#[path = "tests/compile_optional_call_to_conditional_expression.rs"]
mod compile_optional_call_to_conditional_expression;
#[allow(unused_imports)]
use self::compile_optional_call_to_conditional_expression::*;
#[path = "tests/compile_logical_and_to_compound_assignment.rs"]
mod compile_logical_and_to_compound_assignment;
#[allow(unused_imports)]
use self::compile_logical_and_to_compound_assignment::*;
#[path = "tests/compile_logical_or_assignment_to_sequence.rs"]
mod compile_logical_or_assignment_to_sequence;
#[allow(unused_imports)]
use self::compile_logical_or_assignment_to_sequence::*;
#[path = "tests/compile_block_scoping_to_disassemble_output.rs"]
mod compile_block_scoping_to_disassemble_output;
#[allow(unused_imports)]
use self::compile_block_scoping_to_disassemble_output::*;
#[path = "tests/compile_nested_functions_to_delete_member.rs"]
mod compile_nested_functions_to_delete_member;
#[allow(unused_imports)]
use self::compile_nested_functions_to_delete_member::*;
#[path = "tests/compile_delete_non_member_to_labeled_while.rs"]
mod compile_delete_non_member_to_labeled_while;
#[allow(unused_imports)]
use self::compile_delete_non_member_to_labeled_while::*;
#[path = "tests/compile_repl_to_last_expression_result.rs"]
mod compile_repl_to_last_expression_result;
#[allow(unused_imports)]
use self::compile_repl_to_last_expression_result::*;
