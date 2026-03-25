impl Heap {
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
}

impl Heap {
    pub fn alloc_ordinary(&mut self) -> GcPtr<JsObject> {
        self.alloc_object(JsObject::ordinary())
    }
}

impl Heap {
    pub fn alloc_array(&mut self, elements: Vec<JsValue>) -> GcPtr<JsObject> {
        self.alloc_object(JsObject::array(elements))
    }
}

impl Heap {
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
        heap.collect();
        assert_eq!(heap.allocation_count(), 1);
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
        drop(_b);
        heap.collect();
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

use super::*;
