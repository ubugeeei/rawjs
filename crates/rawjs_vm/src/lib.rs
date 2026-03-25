#![doc = " The rawjs virtual machine."]
#![doc = ""]
#![doc = " This crate contains:"]
#![doc = " - A bytecode **interpreter** that executes `Chunk` programs from `rawjs_bytecode`."]
#![doc = " - An aarch64 **JIT compiler** (macOS Apple Silicon) that compiles hot functions"]
#![doc = "   to native machine code."]
#![doc = " - Matching `x86_64` and `riscv64` baseline JIT backends built around the same stub-call model."]
pub mod interpreter;
pub mod jit;
#[allow(unused_imports)]
use crate::jit::JitFunction;
#[allow(unused_imports)]
use rawjs_bytecode::Chunk;
#[allow(unused_imports)]
use rawjs_common::{RawJsError, Result};
#[allow(unused_imports)]
use rawjs_runtime::{GcPtr, Heap, JsObject, JsValue, ObjectInternal, Property, Upvalue};
#[allow(unused_imports)]
use std::collections::{HashMap, HashSet};
#[allow(unused_imports)]
use std::path::{Path, PathBuf};

#[path = "lib/j_i_t_t_h_r_e_s_h_o_l_d_to_vm.rs"]
mod j_i_t_t_h_r_e_s_h_o_l_d_to_vm;
#[allow(unused_imports)]
pub use self::j_i_t_t_h_r_e_s_h_o_l_d_to_vm::Vm;
#[allow(unused_imports)]
pub(crate) use self::j_i_t_t_h_r_e_s_h_o_l_d_to_vm::JIT_THRESHOLD;
#[allow(unused_imports)]
use self::j_i_t_t_h_r_e_s_h_o_l_d_to_vm::*;
#[path = "lib/vm_new_to_init_host_globals.rs"]
mod vm_new_to_init_host_globals;
#[allow(unused_imports)]
use self::vm_new_to_init_host_globals::*;
#[path = "lib/init_primitive_constructor_globals_to_boolean_global.rs"]
mod init_primitive_constructor_globals_to_boolean_global;
#[allow(unused_imports)]
use self::init_primitive_constructor_globals_to_boolean_global::*;
#[path = "lib/init_number_global_to_collection_and_symbol_globals.rs"]
mod init_number_global_to_collection_and_symbol_globals;
#[allow(unused_imports)]
use self::init_number_global_to_collection_and_symbol_globals::*;
#[path = "lib/init_misc_globals_to_repair_builtin_function_prototypes_in_value.rs"]
mod init_misc_globals_to_repair_builtin_function_prototypes_in_value;
#[allow(unused_imports)]
use self::init_misc_globals_to_repair_builtin_function_prototypes_in_value::*;
#[path = "lib/repair_builtin_function_prototypes_in_object_to_global_this_value.rs"]
mod repair_builtin_function_prototypes_in_object_to_global_this_value;
#[allow(unused_imports)]
use self::repair_builtin_function_prototypes_in_object_to_global_this_value::*;
#[path = "lib/create_promise_object_to_peek.rs"]
mod create_promise_object_to_peek;
#[allow(unused_imports)]
use self::create_promise_object_to_peek::*;
#[path = "lib/execute_module.rs"]
mod execute_module;
#[allow(unused_imports)]
use self::execute_module::*;
#[cfg(test)]
#[path = "lib/tests.rs"]
mod tests;
