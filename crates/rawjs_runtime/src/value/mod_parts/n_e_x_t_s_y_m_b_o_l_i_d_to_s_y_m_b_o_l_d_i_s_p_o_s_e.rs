use std::fmt;

use std::rc::Rc;

use std::sync::atomic::{AtomicU64, Ordering};

use crate::gc::GcPtr;

use crate::object::JsObject;

/// A unique Symbol identifier.
static NEXT_SYMBOL_ID: AtomicU64 = AtomicU64::new(100);

/// Well-known symbol IDs.
pub const SYMBOL_ITERATOR: u64 = 1;

pub const SYMBOL_TO_PRIMITIVE: u64 = 2;

pub const SYMBOL_HAS_INSTANCE: u64 = 3;

pub const SYMBOL_TO_STRING_TAG: u64 = 4;

pub const SYMBOL_SPECIES: u64 = 5;

pub const SYMBOL_DISPOSE: u64 = 6;
