mod helpers;

pub mod array;
pub mod console;
pub mod error;
pub mod generator;
pub mod globals;
pub mod json;
pub mod map;
pub mod math;
pub mod number;
pub mod object;
pub mod promise;
pub mod set;
pub mod string;
pub mod symbol;

pub use array::create_array_prototype;
pub use console::create_console_object;
pub use error::create_error_constructor;
pub use globals::create_global_functions;
pub use json::create_json_object;
pub use map::{create_map_prototype, map_constructor};
pub use math::create_math_object;
pub use number::create_number_prototype;
pub use object::{create_object_constructor, create_object_prototype};
pub use promise::{
    create_promise_prototype, promise_constructor_placeholder, promise_reject_fn,
    promise_reject_static, promise_resolve_fn, promise_resolve_static, promise_then_internal,
    reject_promise_internal, reject_promise_with_heap, resolve_promise_internal,
    resolve_promise_with_heap,
};
pub use set::{create_set_prototype, set_constructor};
pub use string::create_string_prototype;
pub use symbol::{create_symbol_constructor, create_symbol_prototype, symbol_call};
