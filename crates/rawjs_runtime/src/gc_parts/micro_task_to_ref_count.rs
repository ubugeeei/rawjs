use std::cell::RefCell;

use std::collections::HashMap;

use std::fmt;

use std::rc::Rc;

use crate::object::JsObject;

use crate::value::JsValue;

/// A microtask to be executed by the VM event loop.
#[derive(Debug, Clone)]
pub struct MicroTask {
    pub callback: JsValue,
    pub arg: JsValue,
    pub target_promise: Option<GcPtr<JsObject>>,
}

/// A garbage-collected pointer.
///
/// For the initial implementation this is backed by `Rc<RefCell<T>>`.
/// This provides shared ownership with interior mutability, which is
/// sufficient to get the engine running. A true mark-and-sweep collector
/// can replace this later via the [`GcHeap`] trait.
#[derive(Debug)]
pub struct GcPtr<T> {
    inner: Rc<RefCell<T>>,
}

impl<T> GcPtr<T> {
    #[doc = " Create a new GC pointer wrapping `value`."]
    pub fn new(value: T) -> Self {
        GcPtr {
            inner: Rc::new(RefCell::new(value)),
        }
    }
}

impl<T> GcPtr<T> {
    #[doc = " Borrow the inner value immutably."]
    pub fn borrow(&self) -> std::cell::Ref<'_, T> {
        self.inner.borrow()
    }
}

impl<T> GcPtr<T> {
    #[doc = " Borrow the inner value mutably."]
    pub fn borrow_mut(&self) -> std::cell::RefMut<'_, T> {
        self.inner.borrow_mut()
    }
}

impl<T> GcPtr<T> {
    #[doc = " Return the number of strong references."]
    pub fn ref_count(&self) -> usize {
        Rc::strong_count(&self.inner)
    }
}
