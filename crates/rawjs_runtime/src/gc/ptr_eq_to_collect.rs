impl<T> GcPtr<T> {
    #[doc = " Check pointer identity (same allocation)."]
    pub fn ptr_eq(&self, other: &GcPtr<T>) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl<T> GcPtr<T> {
    #[doc = " Return a stable address for identity-based maps and sets."]
    pub fn addr(&self) -> usize {
        Rc::as_ptr(&self.inner) as usize
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
    pub(super) allocations: Vec<GcPtr<JsObject>>,
    /// Number of allocations before next collection attempt.
    pub(super) next_gc: usize,
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
    #[doc = " Default initial threshold before first GC."]
    const INITIAL_THRESHOLD: usize = 256;
}

impl Heap {
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
}

impl Heap {
    #[doc = " Allocate a new `JsObject` on the heap and return a `GcPtr` to it."]
    pub fn alloc(&mut self, object: JsObject) -> GcPtr<JsObject> {
        let ptr = GcPtr::new(object);
        self.allocations.push(ptr.clone());
        ptr
    }
}

impl Heap {
    #[doc = " Return the number of tracked allocations."]
    pub fn allocation_count(&self) -> usize {
        self.allocations.len()
    }
}

impl Heap {
    #[doc = " Should we attempt collection?"]
    pub fn should_collect(&self) -> bool {
        self.allocations.len() >= self.next_gc
    }
}

impl Heap {
    #[doc = " Simple collection: drop allocations whose only strong reference"]
    #[doc = " is the one held by the heap itself (i.e. ref_count == 1 means"]
    #[doc = " nobody else references it)."]
    #[doc = ""]
    #[doc = " This is a conservative sweep that works with `Rc` backing."]
    #[doc = " It will NOT collect reference cycles between GC objects; a"]
    #[doc = " proper tracing collector would replace this."]
    pub fn collect(&mut self) {
        let before = self.allocations.len();
        self.allocations.retain(|ptr| ptr.ref_count() > 1);
        let after = self.allocations.len();
        let _freed = before - after;
        self.next_gc = (after * self.growth_factor).max(Self::INITIAL_THRESHOLD);
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
