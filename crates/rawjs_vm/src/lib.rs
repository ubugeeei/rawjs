#![doc = " The rawjs virtual machine."]
#![doc = ""]
#![doc = " This crate contains:"]
#![doc = " - A bytecode **interpreter** that executes `Chunk` programs from `rawjs_bytecode`."]
#![doc = " - An aarch64 **JIT compiler** (macOS Apple Silicon) that compiles hot functions"]
#![doc = "   to native machine code."]
#![doc = " - Matching `x86_64` and `riscv64` baseline JIT backends built around the same stub-call model."]
pub mod interpreter;
pub mod jit;
include!("lib_parts/j_i_t_t_h_r_e_s_h_o_l_d_to_vm.rs");
include!("lib_parts/vm_new_to_init_host_globals.rs");
include!("lib_parts/init_primitive_constructor_globals_to_boolean_global.rs");
include!("lib_parts/init_number_global_to_collection_and_symbol_globals.rs");
include!("lib_parts/init_misc_globals_to_repair_builtin_function_prototypes_in_value.rs");
include!("lib_parts/repair_builtin_function_prototypes_in_object_to_global_this_value.rs");
include!("lib_parts/create_promise_object_to_peek.rs");
include!("lib_parts/execute_module.rs");
include!("lib_parts/tests.rs");
