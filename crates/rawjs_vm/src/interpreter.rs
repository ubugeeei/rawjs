//! Bytecode interpreter for the rawjs VM.
//!
//! This module implements the main dispatch loop that fetches, decodes, and
//! executes bytecode instructions one at a time.  When a function's
//! invocation count crosses the JIT threshold the interpreter will attempt to
//! compile it to native code and call the JIT version on subsequent
//! invocations.
pub(crate) use crate::{CallFrame, TryContext, Vm, JIT_THRESHOLD};
pub(crate) use rawjs_bytecode::{Chunk, Constant, Instruction};
pub(crate) use rawjs_common::{RawJsError, Result};
pub(crate) use rawjs_parser::parse as parse_program;
#[allow(unused_imports)]
use rawjs_runtime::{
    GcPtr, GeneratorState, GeneratorStatus, Heap, JsObject, JsValue, ObjectInternal, PromiseStatus,
    Property, Upvalue,
};

mod run_start_to_module_frame;
pub(crate) use self::run_start_to_module_frame::*;
mod run_inner_frame_to_drain_microtasks;
pub(crate) use self::run_inner_frame_to_drain_microtasks::*;
mod settle_microtask_target_to_instruction_outcome;
pub(crate) use self::settle_microtask_target_to_instruction_outcome::*;
mod execute_instruction_to_data_instruction;
pub(crate) use self::execute_instruction_to_data_instruction::*;
mod execute_operator_instruction_to_control_instruction;
pub(crate) use self::execute_operator_instruction_to_control_instruction::*;
mod execute_object_instruction_to_exception_instruction;
pub(crate) use self::execute_object_instruction_to_exception_instruction::*;
mod execute_iteration_instruction_to_delete_property;
pub(crate) use self::execute_iteration_instruction_to_delete_property::*;
mod jump_relative_to_set_computed_value;
pub(crate) use self::jump_relative_to_set_computed_value::*;
mod load_arguments_object_to_create_iterator_value;
pub(crate) use self::load_arguments_object_to_create_iterator_value::*;
mod iterator_next_to_export_default;
pub(crate) use self::iterator_next_to_export_default::*;
mod execute_module_instruction_to_exec_call;
pub(crate) use self::execute_module_instruction_to_exec_call::*;
mod exec_new;
pub(crate) use self::exec_new::*;
mod exec_return_to_unwind_exception;
pub(crate) use self::exec_return_to_unwind_exception::*;
mod raw_error_to_value_to_create_strict_arguments_thrower;
pub(crate) use self::raw_error_to_value_to_create_strict_arguments_thrower::*;
mod sync_arguments_object_from_local_to_exec_promise_constructor;
pub(crate) use self::sync_arguments_object_from_local_to_exec_promise_constructor::*;
mod js_add_to_get_property_value;
pub(crate) use self::js_add_to_get_property_value::*;
mod set_property_value;
pub(crate) use self::set_property_value::*;
mod exec_create_generator_to_yield;
pub(crate) use self::exec_create_generator_to_yield::*;
mod try_generator_method_call_to_generator_next;
pub(crate) use self::try_generator_method_call_to_generator_next::*;
mod run_generator_frame_to_generator_return;
pub(crate) use self::run_generator_frame_to_generator_return::*;
mod generator_throw_to_create_async_result_promise;
pub(crate) use self::generator_throw_to_create_async_result_promise::*;
mod start_async_chunk_to_async_resume;
pub(crate) use self::start_async_chunk_to_async_resume::*;
mod run_async_frame;
pub(crate) use self::run_async_frame::*;
mod suspend_async_frame_to_prepare_dispose_resource;
pub(crate) use self::suspend_async_frame_to_prepare_dispose_resource::*;
mod lookup_dispose_method_to_async_reject_callback;
pub(crate) use self::lookup_dispose_method_to_async_reject_callback::*;
mod tests;
pub(crate) use self::tests::*;
