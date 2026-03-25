#[allow(unused_imports)]
use crate::object::JsObject;
#[allow(unused_imports)]
use crate::value::JsValue;
#[allow(unused_imports)]
use std::cell::RefCell;
#[allow(unused_imports)]
use std::collections::HashMap;
#[allow(unused_imports)]
use std::fmt;
#[allow(unused_imports)]
use std::rc::Rc;

#[path = "gc/micro_task_to_ref_count.rs"]
mod micro_task_to_ref_count;
#[allow(unused_imports)]
use self::micro_task_to_ref_count::*;
#[allow(unused_imports)]
pub use self::micro_task_to_ref_count::{GcPtr, MicroTask};
#[path = "gc/ptr_eq_to_collect.rs"]
mod ptr_eq_to_collect;
#[allow(unused_imports)]
pub use self::ptr_eq_to_collect::Heap;
#[allow(unused_imports)]
use self::ptr_eq_to_collect::*;
#[path = "gc/force_collect_to_tests.rs"]
mod force_collect_to_tests;
#[allow(unused_imports)]
use self::force_collect_to_tests::*;
