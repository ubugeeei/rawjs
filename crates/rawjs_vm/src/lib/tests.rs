#[allow(unused_imports)]
use super::*;
#[allow(unused_imports)]
use rawjs_bytecode::{Chunk, Constant, Instruction};
#[allow(unused_imports)]
use std::fs;
#[allow(unused_imports)]
use std::time::{SystemTime, UNIX_EPOCH};

#[path = "tests/make_return_number_to_test_execute_comparison.rs"]
mod make_return_number_to_test_execute_comparison;
#[allow(unused_imports)]
use self::make_return_number_to_test_execute_comparison::*;
#[path = "tests/test_execute_local_vars_to_dup_and_pop.rs"]
mod test_execute_local_vars_to_dup_and_pop;
#[allow(unused_imports)]
use self::test_execute_local_vars_to_dup_and_pop::*;
#[path = "tests/test_execute_global_store_load_to_import_meta_url.rs"]
mod test_execute_global_store_load_to_import_meta_url;
#[allow(unused_imports)]
use self::test_execute_global_store_load_to_import_meta_url::*;
#[path = "tests/test_execute_dynamic_import_to_logical_and_assignment_short_circuits.rs"]
mod test_execute_dynamic_import_to_logical_and_assignment_short_circuits;
#[allow(unused_imports)]
use self::test_execute_dynamic_import_to_logical_and_assignment_short_circuits::*;
#[path = "tests/test_execute_logical_nullish_assignment_to_number_static_constants_are_read_only.rs"]
mod test_execute_logical_nullish_assignment_to_number_static_constants_are_read_only;
#[allow(unused_imports)]
use self::test_execute_logical_nullish_assignment_to_number_static_constants_are_read_only::*;
#[path = "tests/test_execute_new_array_length_to_native_methods_inherit_function_prototype_call.rs"]
mod test_execute_new_array_length_to_native_methods_inherit_function_prototype_call;
#[allow(unused_imports)]
use self::test_execute_new_array_length_to_native_methods_inherit_function_prototype_call::*;
#[path = "tests/test_execute_descriptor_shim_can_call_native_object_methods_to_nested_function_uses_own_arguments_object.rs"]
mod test_execute_descriptor_shim_can_call_native_object_methods_to_nested_function_uses_own_arguments_object;
#[allow(unused_imports)]
use self::test_execute_descriptor_shim_can_call_native_object_methods_to_nested_function_uses_own_arguments_object::*;
#[path = "tests/test_execute_async_generator_method_next_resolves_to_generator_next_survives_nested_call.rs"]
mod test_execute_async_generator_method_next_resolves_to_generator_next_survives_nested_call;
#[allow(unused_imports)]
use self::test_execute_async_generator_method_next_resolves_to_generator_next_survives_nested_call::*;
