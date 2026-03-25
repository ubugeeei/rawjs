impl Vm {
    pub(crate) fn create_promise_object(&mut self) -> GcPtr<JsObject> {
        let promise_ptr = self.heap.alloc(JsObject::promise());
        if let Some(ref proto) = self.promise_prototype {
            promise_ptr.borrow_mut().set_prototype(Some(proto.clone()));
        }
        promise_ptr
    }
}

impl Vm {
    pub(crate) fn create_import_meta_object(&mut self) -> Result<GcPtr<JsObject>> {
        let current_path = self.current_file_path.clone().ok_or_else(|| {
            RawJsError::type_error("import.meta is only available while executing a module")
        })?;
        let meta_ptr = self.heap.alloc(JsObject::ordinary());
        meta_ptr
            .borrow_mut()
            .set_property("url".to_string(), JsValue::string(current_path));
        Ok(meta_ptr)
    }
}

impl Vm {
    #[doc = " Increment the execution count for a chunk and return the new count."]
    pub(crate) fn bump_execution_count(&mut self, chunk_index: usize) -> u32 {
        let count = self.execution_counts.entry(chunk_index).or_insert(0);
        *count += 1;
        *count
    }
}

impl Vm {
    #[doc = " Check if a JIT-compiled version exists for a chunk."]
    pub(crate) fn has_jit(&self, chunk_index: usize) -> bool {
        self.jit_cache.contains_key(&chunk_index)
    }
}

impl Vm {
    #[doc = " Attempt to JIT-compile a chunk.  Returns `true` on success."]
    pub(crate) fn try_jit_compile(&mut self, chunk_index: usize) -> bool {
        if self.jit_cache.contains_key(&chunk_index) {
            return true;
        }
        let chunk = &self.chunks[chunk_index];
        match jit::JitCompiler::compile(chunk) {
            Some(func) => {
                self.jit_cache.insert(chunk_index, func);
                true
            }
            None => false,
        }
    }
}

impl Vm {
    #[doc = " Execute a JIT-compiled function for the current top-of-stack call frame."]
    #[doc = ""]
    #[doc = " The caller must have already pushed a `CallFrame` onto `call_stack`."]
    #[doc = " The JIT function will execute the frame via stub calls and pop it"]
    #[doc = " on return."]
    #[doc = ""]
    #[doc = " Returns `Ok(())` on success (result is on `value_stack`)."]
    #[doc = " Returns `Err` on failure (details in `jit_error`)."]
    #[doc = ""]
    #[doc = " # Safety"]
    #[doc = ""]
    #[doc = " The JIT function pointer must have been produced by `JitCompiler` and"]
    #[doc = " must still be mapped and valid."]
    pub(crate) unsafe fn call_jit_new(&mut self, chunk_index: usize) -> u32 {
        if let Some(func) = self.jit_cache.get(&chunk_index) {
            let func_ptr = func.code;
            let func: extern "C" fn(*mut Vm) -> u32 = std::mem::transmute(func_ptr);
            func(self as *mut Vm)
        } else {
            1
        }
    }
}

impl Vm {
    #[inline]
    pub(crate) fn push(&mut self, value: JsValue) {
        self.value_stack.push(value);
    }
}

impl Vm {
    #[inline]
    pub(crate) fn pop(&mut self) -> Result<JsValue> {
        self.value_stack
            .pop()
            .ok_or_else(|| RawJsError::internal_error("stack underflow"))
    }
}

impl Vm {
    #[inline]
    pub(crate) fn peek(&self) -> Result<&JsValue> {
        self.value_stack
            .last()
            .ok_or_else(|| RawJsError::internal_error("stack underflow"))
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
