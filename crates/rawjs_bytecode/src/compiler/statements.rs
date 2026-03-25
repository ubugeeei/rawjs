#[allow(unused_imports)]
use super::{expression_to_property_name, Compiler, LocalStorage};
#[allow(unused_imports)]
use crate::opcode::Instruction;
#[allow(unused_imports)]
use rawjs_ast::*;
#[allow(unused_imports)]
use rawjs_common::{RawJsError, Result};

#[path = "statements/compile_statement_to_variable_declaration.rs"]
mod compile_statement_to_variable_declaration;
#[allow(unused_imports)]
use self::compile_statement_to_variable_declaration::*;
#[path = "statements/compile_destructuring_declaration_to_do_while.rs"]
mod compile_destructuring_declaration_to_do_while;
#[allow(unused_imports)]
use self::compile_destructuring_declaration_to_do_while::*;
#[path = "statements/compile_for_start_to_in.rs"]
mod compile_for_start_to_in;
#[allow(unused_imports)]
use self::compile_for_start_to_in::*;
#[path = "statements/compile_for_of_to_continue.rs"]
mod compile_for_of_to_continue;
#[allow(unused_imports)]
use self::compile_for_of_to_continue::*;
#[path = "statements/compile_switch_to_try.rs"]
mod compile_switch_to_try;
#[allow(unused_imports)]
use self::compile_switch_to_try::*;
#[path = "statements/compile_labeled_to_class_body.rs"]
mod compile_labeled_to_class_body;
#[allow(unused_imports)]
use self::compile_labeled_to_class_body::*;
#[path = "statements/compile_import_to_export.rs"]
mod compile_import_to_export;
#[allow(unused_imports)]
use self::compile_import_to_export::*;
