#[allow(unused_imports)]
use super::helpers;
#[allow(unused_imports)]
use super::helpers::set_native;
#[allow(unused_imports)]
use crate::gc::{GcPtr, Heap};
#[allow(unused_imports)]
use crate::object::{JsObject, ObjectInternal};
#[allow(unused_imports)]
use crate::value::JsValue;
#[allow(unused_imports)]
use rawjs_common::{RawJsError, Result};

#[path = "object/create_object_prototype_to_constructor.rs"]
mod create_object_prototype_to_constructor;
#[allow(unused_imports)]
pub use self::create_object_prototype_to_constructor::*;
#[allow(unused_imports)]
use self::create_object_prototype_to_constructor::*;
#[path = "object/object_constructor_to_assign.rs"]
mod object_constructor_to_assign;
#[allow(unused_imports)]
pub use self::object_constructor_to_assign::*;
#[allow(unused_imports)]
use self::object_constructor_to_assign::*;
#[path = "object/object_define_property_to_freeze.rs"]
mod object_define_property_to_freeze;
#[allow(unused_imports)]
pub use self::object_define_property_to_freeze::*;
#[allow(unused_imports)]
use self::object_define_property_to_freeze::*;
#[path = "object/object_create_to_tests.rs"]
mod object_create_to_tests;
#[allow(unused_imports)]
pub use self::object_create_to_tests::*;
#[allow(unused_imports)]
use self::object_create_to_tests::*;
