#![doc = " Bytecode interpreter for the rawjs VM."]
#![doc = ""]
#![doc = " This module implements the main dispatch loop that fetches, decodes, and"]
#![doc = " executes bytecode instructions one at a time.  When a function's"]
#![doc = " invocation count crosses the JIT threshold the interpreter will attempt to"]
#![doc = " compile it to native code and call the JIT version on subsequent"]
#![doc = " invocations."]
include!("interpreter_parts/run_start_to_module_frame.rs");
include!("interpreter_parts/run_inner_frame_to_drain_microtasks.rs");
include!("interpreter_parts/settle_microtask_target_to_instruction_outcome.rs");
include!("interpreter_parts/execute_instruction_to_data_instruction.rs");
include!("interpreter_parts/execute_operator_instruction_to_control_instruction.rs");
include!("interpreter_parts/execute_object_instruction_to_exception_instruction.rs");
include!("interpreter_parts/execute_iteration_instruction_to_delete_property.rs");
include!("interpreter_parts/jump_relative_to_set_computed_value.rs");
include!("interpreter_parts/load_arguments_object_to_create_iterator_value.rs");
include!("interpreter_parts/iterator_next_to_export_default.rs");
include!("interpreter_parts/execute_module_instruction_to_exec_call.rs");
include!("interpreter_parts/exec_new.rs");
include!("interpreter_parts/exec_return_to_unwind_exception.rs");
include!("interpreter_parts/raw_error_to_value_to_create_strict_arguments_thrower.rs");
include!("interpreter_parts/sync_arguments_object_from_local_to_exec_promise_constructor.rs");
include!("interpreter_parts/js_add_to_get_property_value.rs");
include!("interpreter_parts/set_property_value.rs");
include!("interpreter_parts/exec_create_generator_to_yield.rs");
include!("interpreter_parts/try_generator_method_call_to_generator_next.rs");
include!("interpreter_parts/run_generator_frame_to_generator_return.rs");
include!("interpreter_parts/generator_throw_to_create_async_result_promise.rs");
include!("interpreter_parts/start_async_chunk_to_async_resume.rs");
include!("interpreter_parts/run_async_frame.rs");
include!("interpreter_parts/suspend_async_frame_to_prepare_dispose_resource.rs");
include!("interpreter_parts/lookup_dispose_method_to_async_reject_callback.rs");
include!("interpreter_parts/tests.rs");
