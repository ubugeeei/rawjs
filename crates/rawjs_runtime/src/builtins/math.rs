#[allow(unused_imports)]
use super::helpers;
#[allow(unused_imports)]
use super::helpers::set_native;
#[allow(unused_imports)]
use crate::gc::{GcPtr, Heap};
#[allow(unused_imports)]
use crate::object::{JsObject, Property};
#[allow(unused_imports)]
use crate::value::JsValue;
#[allow(unused_imports)]
use rawjs_common::Result;
#[allow(unused_imports)]
use std::sync::atomic::{AtomicU64, Ordering};

#[path = "math/create_math_object_to_math_min.rs"]
mod create_math_object_to_math_min;
#[allow(unused_imports)]
pub use self::create_math_object_to_math_min::*;
#[allow(unused_imports)]
use self::create_math_object_to_math_min::*;
#[path = "math/math_max_to_tests.rs"]
mod math_max_to_tests;
#[allow(unused_imports)]
pub use self::math_max_to_tests::*;
#[allow(unused_imports)]
use self::math_max_to_tests::*;
