pub(crate) use super::helpers;
pub(crate) use super::helpers::set_native;
pub(crate) use crate::gc::{GcPtr, Heap};
pub(crate) use crate::object::{JsObject, ObjectInternal};
pub(crate) use crate::value::JsValue;
pub(crate) use rawjs_common::{RawJsError, Result};

mod create_object_prototype_to_constructor;
pub use self::create_object_prototype_to_constructor::*;
pub(crate) use self::create_object_prototype_to_constructor::*;
mod object_constructor_to_assign;
pub use self::object_constructor_to_assign::*;
pub(crate) use self::object_constructor_to_assign::*;
mod object_define_property_to_freeze;
pub use self::object_define_property_to_freeze::*;
pub(crate) use self::object_define_property_to_freeze::*;
mod object_create_to_tests;
pub use self::object_create_to_tests::*;
pub(crate) use self::object_create_to_tests::*;
