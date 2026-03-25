#![doc = " Bytecode interpreter for the rawjs VM."]
#![doc = ""]
#![doc = " This module implements the main dispatch loop that fetches, decodes, and"]
#![doc = " executes bytecode instructions one at a time.  When a function's"]
#![doc = " invocation count crosses the JIT threshold the interpreter will attempt to"]
#![doc = " compile it to native code and call the JIT version on subsequent"]
#![doc = " invocations."]
#[allow(unused_imports)]
use crate::{CallFrame, TryContext, Vm, JIT_THRESHOLD};
#[allow(unused_imports)]
use rawjs_bytecode::{Chunk, Constant, Instruction};
#[allow(unused_imports)]
use rawjs_common::{RawJsError, Result};
#[allow(unused_imports)]
use rawjs_parser::parse as parse_program;
#[allow(unused_imports)]
use rawjs_runtime::{
    GcPtr, GeneratorState, GeneratorStatus, Heap, JsObject, JsValue, ObjectInternal, PromiseStatus,
    Property, Upvalue,
};

#[path = "interpreter/run_start_to_module_frame.rs"]
mod run_start_to_module_frame;
#[allow(unused_imports)]
pub(crate) use self::run_start_to_module_frame::*;
#[path = "interpreter/run_inner_frame_to_drain_microtasks.rs"]
mod run_inner_frame_to_drain_microtasks;
#[allow(unused_imports)]
pub(crate) use self::run_inner_frame_to_drain_microtasks::*;
#[path = "interpreter/settle_microtask_target_to_instruction_outcome.rs"]
mod settle_microtask_target_to_instruction_outcome;
#[allow(unused_imports)]
pub(crate) use self::settle_microtask_target_to_instruction_outcome::*;
#[path = "interpreter/execute_instruction_to_data_instruction.rs"]
mod execute_instruction_to_data_instruction;
#[allow(unused_imports)]
pub(crate) use self::execute_instruction_to_data_instruction::*;
#[path = "interpreter/execute_operator_instruction_to_control_instruction.rs"]
mod execute_operator_instruction_to_control_instruction;
#[allow(unused_imports)]
pub(crate) use self::execute_operator_instruction_to_control_instruction::*;
#[path = "interpreter/execute_object_instruction_to_exception_instruction.rs"]
mod execute_object_instruction_to_exception_instruction;
#[allow(unused_imports)]
pub(crate) use self::execute_object_instruction_to_exception_instruction::*;
#[path = "interpreter/execute_iteration_instruction_to_delete_property.rs"]
mod execute_iteration_instruction_to_delete_property;
#[allow(unused_imports)]
pub(crate) use self::execute_iteration_instruction_to_delete_property::*;
#[path = "interpreter/jump_relative_to_set_computed_value.rs"]
mod jump_relative_to_set_computed_value;
#[allow(unused_imports)]
pub(crate) use self::jump_relative_to_set_computed_value::*;
#[path = "interpreter/load_arguments_object_to_create_iterator_value.rs"]
mod load_arguments_object_to_create_iterator_value;
#[allow(unused_imports)]
pub(crate) use self::load_arguments_object_to_create_iterator_value::*;
#[path = "interpreter/iterator_next_to_export_default.rs"]
mod iterator_next_to_export_default;
#[allow(unused_imports)]
pub(crate) use self::iterator_next_to_export_default::*;
#[path = "interpreter/execute_module_instruction_to_exec_call.rs"]
mod execute_module_instruction_to_exec_call;
#[allow(unused_imports)]
pub(crate) use self::execute_module_instruction_to_exec_call::*;
#[path = "interpreter/exec_new.rs"]
mod exec_new;
#[allow(unused_imports)]
pub(crate) use self::exec_new::*;
#[path = "interpreter/exec_return_to_unwind_exception.rs"]
mod exec_return_to_unwind_exception;
#[allow(unused_imports)]
pub(crate) use self::exec_return_to_unwind_exception::*;
#[path = "interpreter/raw_error_to_value_to_create_strict_arguments_thrower.rs"]
mod raw_error_to_value_to_create_strict_arguments_thrower;
#[allow(unused_imports)]
pub(crate) use self::raw_error_to_value_to_create_strict_arguments_thrower::*;
#[path = "interpreter/sync_arguments_object_from_local_to_exec_promise_constructor.rs"]
mod sync_arguments_object_from_local_to_exec_promise_constructor;
#[allow(unused_imports)]
pub(crate) use self::sync_arguments_object_from_local_to_exec_promise_constructor::*;
#[path = "interpreter/js_add_to_get_property_value.rs"]
mod js_add_to_get_property_value;
#[allow(unused_imports)]
pub(crate) use self::js_add_to_get_property_value::*;
#[path = "interpreter/set_property_value.rs"]
mod set_property_value;
#[allow(unused_imports)]
pub(crate) use self::set_property_value::*;
#[path = "interpreter/exec_create_generator_to_yield.rs"]
mod exec_create_generator_to_yield;
#[allow(unused_imports)]
pub(crate) use self::exec_create_generator_to_yield::*;
#[path = "interpreter/try_generator_method_call_to_generator_next.rs"]
mod try_generator_method_call_to_generator_next;
#[allow(unused_imports)]
pub(crate) use self::try_generator_method_call_to_generator_next::*;
#[path = "interpreter/run_generator_frame_to_generator_return.rs"]
mod run_generator_frame_to_generator_return;
#[allow(unused_imports)]
pub(crate) use self::run_generator_frame_to_generator_return::*;
#[path = "interpreter/generator_throw_to_create_async_result_promise.rs"]
mod generator_throw_to_create_async_result_promise;
#[allow(unused_imports)]
pub(crate) use self::generator_throw_to_create_async_result_promise::*;
#[path = "interpreter/start_async_chunk_to_async_resume.rs"]
mod start_async_chunk_to_async_resume;
#[allow(unused_imports)]
pub(crate) use self::start_async_chunk_to_async_resume::*;
#[path = "interpreter/run_async_frame.rs"]
mod run_async_frame;
#[allow(unused_imports)]
pub(crate) use self::run_async_frame::*;
#[path = "interpreter/suspend_async_frame_to_prepare_dispose_resource.rs"]
mod suspend_async_frame_to_prepare_dispose_resource;
#[allow(unused_imports)]
pub(crate) use self::suspend_async_frame_to_prepare_dispose_resource::*;
#[path = "interpreter/lookup_dispose_method_to_async_reject_callback.rs"]
mod lookup_dispose_method_to_async_reject_callback;
#[allow(unused_imports)]
pub(crate) use self::lookup_dispose_method_to_async_reject_callback::*;
#[path = "interpreter/tests.rs"]
mod tests;
#[allow(unused_imports)]
pub(crate) use self::tests::*;
