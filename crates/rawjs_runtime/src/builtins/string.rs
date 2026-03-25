pub(crate) use super::helpers;
pub(crate) use super::helpers::{get_this_string, set_native};
pub(crate) use crate::gc::{GcPtr, Heap};
pub(crate) use crate::object::{JsObject, ObjectInternal, Property};
pub(crate) use crate::value::JsValue;
pub(crate) use rawjs_common::{RawJsError, Result};

mod create_string_prototype_to_string_last_index_of;
pub use self::create_string_prototype_to_string_last_index_of::*;
pub(crate) use self::create_string_prototype_to_string_last_index_of::*;
mod string_slice_to_ends_with;
pub use self::string_slice_to_ends_with::*;
pub(crate) use self::string_slice_to_ends_with::*;
mod string_replace_to_tests;
pub use self::string_replace_to_tests::*;
pub(crate) use self::string_replace_to_tests::*;
