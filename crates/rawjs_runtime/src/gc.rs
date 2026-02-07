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
    /// Create a new GC pointer wrapping `value`.
    pub fn new(value: T) -> Self {
        GcPtr {
            inner: Rc::new(RefCell::new(value)),
        }
    }

    /// Borrow the inner value immutably.
    pub fn borrow(&self) -> std::cell::Ref<'_, T> {
        self.inner.borrow()
    }

    /// Borrow the inner value mutably.
    pub fn borrow_mut(&self) -> std::cell::RefMut<'_, T> {
        self.inner.borrow_mut()
    }

    /// Return the number of strong references.
    pub fn ref_count(&self) -> usize {
        Rc::strong_count(&self.inner)
    }

    /// Check pointer identity (same allocation).
    pub fn ptr_eq(&self, other: &GcPtr<T>) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl<T> Clone for GcPtr<T> {
    fn clone(&self) -> Self {
        GcPtr {
            inner: Rc::clone(&self.inner),
        }
    }
}

impl<T: PartialEq> PartialEq for GcPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl<T: fmt::Display> fmt::Display for GcPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner.borrow())
    }
}

/// The heap that owns all GC-managed allocations.
///
/// Currently this is a simple list of allocations; the `Rc<RefCell<T>>`
/// backing handles lifetime management via reference counting. When
/// all `GcPtr` handles to an object are dropped the object is freed
/// automatically.
///
/// The explicit `allocations` vec keeps a strong reference so that
/// objects referenced only from other GC objects (cycles aside) stay
/// alive until `collect()` is called.
pub struct Heap {
    /// All live object allocations.  We keep a strong `GcPtr` to each
    /// one; during collection we drop the entries that are no longer
    /// reachable from roots.
    allocations: Vec<GcPtr<JsObject>>,
    /// Number of allocations before next collection attempt.
    next_gc: usize,
    /// Growth factor for `next_gc`.
    growth_factor: usize,

    /// The currently calling function object (set by VM before NativeFn call).
    pub calling_fn: Option<GcPtr<JsObject>>,
    /// Pending microtasks (Promise reactions).
    pub pending_microtasks: Vec<MicroTask>,
    /// Map from promise ID to promise object (for resolve/reject functions).
    pub promise_targets: HashMap<u64, GcPtr<JsObject>>,
    /// Next promise ID counter.
    pub next_promise_id: u64,
}

impl Heap {
    /// Default initial threshold before first GC.
    const INITIAL_THRESHOLD: usize = 256;

    pub fn new() -> Self {
        Heap {
            allocations: Vec::new(),
            next_gc: Self::INITIAL_THRESHOLD,
            growth_factor: 2,
            calling_fn: None,
            pending_microtasks: Vec::new(),
            promise_targets: HashMap::new(),
            next_promise_id: 0,
        }
    }

    /// Allocate a new `JsObject` on the heap and return a `GcPtr` to it.
    pub fn alloc(&mut self, object: JsObject) -> GcPtr<JsObject> {
        let ptr = GcPtr::new(object);
        self.allocations.push(ptr.clone());
        ptr
    }

    /// Return the number of tracked allocations.
    pub fn allocation_count(&self) -> usize {
        self.allocations.len()
    }

    /// Should we attempt collection?
    pub fn should_collect(&self) -> bool {
        self.allocations.len() >= self.next_gc
    }

    /// Simple collection: drop allocations whose only strong reference
    /// is the one held by the heap itself (i.e. ref_count == 1 means
    /// nobody else references it).
    ///
    /// This is a conservative sweep that works with `Rc` backing.
    /// It will NOT collect reference cycles between GC objects; a
    /// proper tracing collector would replace this.
    pub fn collect(&mut self) {
        let before = self.allocations.len();
        self.allocations.retain(|ptr| ptr.ref_count() > 1);
        let after = self.allocations.len();
        let _freed = before - after;

        // Adjust next threshold.
        self.next_gc = (after * self.growth_factor).max(Self::INITIAL_THRESHOLD);
    }

    /// Force a full collection and return the number of freed objects.
    pub fn force_collect(&mut self) -> usize {
        let before = self.allocations.len();
        self.collect();
        before - self.allocations.len()
    }
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for Heap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Heap")
            .field("allocations", &self.allocations.len())
            .field("next_gc", &self.next_gc)
            .finish()
    }
}

impl Heap {
    pub fn alloc_object(&mut self, object: JsObject) -> GcPtr<JsObject> {
        self.alloc(object)
    }

    pub fn alloc_ordinary(&mut self) -> GcPtr<JsObject> {
        self.alloc_object(JsObject::ordinary())
    }

    pub fn alloc_array(&mut self, elements: Vec<JsValue>) -> GcPtr<JsObject> {
        self.alloc_object(JsObject::array(elements))
    }

    pub fn alloc_error(&mut self, message: String) -> GcPtr<JsObject> {
        self.alloc_object(JsObject::error(message))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::JsObject;

    #[test]
    fn test_alloc_and_collect() {
        let mut heap = Heap::new();
        let ptr = heap.alloc(JsObject::ordinary());
        assert_eq!(heap.allocation_count(), 1);

        // ptr is still alive so collection should keep it.
        heap.collect();
        assert_eq!(heap.allocation_count(), 1);

        // Drop the external reference.
        drop(ptr);
        let freed = heap.force_collect();
        assert_eq!(freed, 1);
        assert_eq!(heap.allocation_count(), 0);
    }

    #[test]
    fn test_gc_ptr_clone_and_eq() {
        let a = GcPtr::new(42);
        let b = a.clone();
        assert!(a.ptr_eq(&b));
        assert_eq!(a.ref_count(), 2);
    }

    #[test]
    fn test_heap_should_collect() {
        let mut heap = Heap::new();
        for _ in 0..256 {
            let _ = heap.alloc(JsObject::ordinary());
        }
        assert!(heap.should_collect());
    }

    #[test]
    fn test_multiple_alloc_and_partial_collect() {
        let mut heap = Heap::new();
        let a = heap.alloc(JsObject::ordinary());
        let _b = heap.alloc(JsObject::ordinary());
        let c = heap.alloc(JsObject::ordinary());
        assert_eq!(heap.allocation_count(), 3);

        // Drop b, keep a and c
        // _b goes out of scope after this block
        drop(_b);
        heap.collect();
        // a and c still referenced, b only had heap ref (rc=1), so freed
        assert_eq!(heap.allocation_count(), 2);

        drop(a);
        drop(c);
        heap.collect();
        assert_eq!(heap.allocation_count(), 0);
    }

    #[test]
    fn test_gc_ptr_borrow() {
        let ptr = GcPtr::new(10i32);
        assert_eq!(*ptr.borrow(), 10);
        *ptr.borrow_mut() = 20;
        assert_eq!(*ptr.borrow(), 20);
    }

    #[test]
    fn test_heap_default() {
        let heap = Heap::default();
        assert_eq!(heap.allocation_count(), 0);
        assert!(!heap.should_collect());
    }
}
