use super::Vm;

pub struct JitFunction {
    pub(crate) code: *const u8,
    pub(super) size: usize,
}

unsafe impl Send for JitFunction {}
unsafe impl Sync for JitFunction {}

impl JitFunction {
    /// # Safety
    ///
    /// `vm` must be a valid, non-null pointer to a live [`Vm`] for the duration
    /// of the native JIT call, and `self.code` must point to executable code
    /// generated for the `extern "C" fn(*mut Vm) -> u32` ABI.
    pub unsafe fn call_vm(&self, vm: *mut Vm) -> u32 {
        let function: extern "C" fn(*mut Vm) -> u32 = unsafe { std::mem::transmute(self.code) };
        function(vm)
    }
}

impl Drop for JitFunction {
    fn drop(&mut self) {
        unsafe {
            super::memory::release(self.code, self.size);
        }
    }
}
