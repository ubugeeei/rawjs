#[allow(unused_imports)]
use super::helpers;
#[allow(unused_imports)]
use super::helpers::{get_this_string, set_native};
#[allow(unused_imports)]
use crate::gc::{GcPtr, Heap};
#[allow(unused_imports)]
use crate::object::{JsObject, ObjectInternal, Property};
#[allow(unused_imports)]
use crate::value::JsValue;
#[allow(unused_imports)]
use rawjs_common::{RawJsError, Result};

#[path = "string/create_string_prototype_to_string_last_index_of.rs"]
mod create_string_prototype_to_string_last_index_of;
#[allow(unused_imports)]
pub use self::create_string_prototype_to_string_last_index_of::*;
#[allow(unused_imports)]
use self::create_string_prototype_to_string_last_index_of::*;
#[path = "string/string_slice_to_ends_with.rs"]
mod string_slice_to_ends_with;
#[allow(unused_imports)]
pub use self::string_slice_to_ends_with::*;
#[allow(unused_imports)]
use self::string_slice_to_ends_with::*;
#[path = "string/string_replace_to_tests.rs"]
mod string_replace_to_tests;
#[allow(unused_imports)]
pub use self::string_replace_to_tests::*;
#[allow(unused_imports)]
use self::string_replace_to_tests::*;
