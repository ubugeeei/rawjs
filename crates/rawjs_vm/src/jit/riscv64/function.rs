use super::Vm;

pub struct JitFunction {
    pub(crate) code: *const u8,
    pub(super) size: usize,
}

unsafe impl Send for JitFunction {}
unsafe impl Sync for JitFunction {}

impl JitFunction {
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
