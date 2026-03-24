use super::{JitCompiler, JitFunction};

extern "C" {
    fn mmap(addr: *mut u8, len: usize, prot: i32, flags: i32, fd: i32, offset: i64) -> *mut u8;
    fn munmap(addr: *mut u8, len: usize) -> i32;
    fn pthread_jit_write_protect_np(enabled: i32);
    fn sys_icache_invalidate(addr: *mut u8, size: usize);
}

const PROT_READ: i32 = 0x01;
const PROT_WRITE: i32 = 0x02;
const PROT_EXEC: i32 = 0x04;
const MAP_PRIVATE: i32 = 0x0002;
const MAP_ANON: i32 = 0x1000;
const MAP_JIT: i32 = 0x0800;
const MAP_FAILED: *mut u8 = !0usize as *mut u8;

impl JitCompiler {
    pub(crate) fn finalize(self) -> JitFunction {
        let size = self.code.len();
        let page_size = 16_384usize;
        let alloc_size = (size + page_size - 1) & !(page_size - 1);
        let ptr = unsafe {
            let ptr = mmap(
                std::ptr::null_mut(),
                alloc_size,
                PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE | MAP_ANON | MAP_JIT,
                -1,
                0,
            );
            assert_ne!(ptr, MAP_FAILED, "mmap failed for JIT code");
            ptr
        };

        unsafe {
            pthread_jit_write_protect_np(0);
            std::ptr::copy_nonoverlapping(self.code.as_ptr(), ptr, size);
            pthread_jit_write_protect_np(1);
            sys_icache_invalidate(ptr, size);
        }

        JitFunction {
            code: ptr,
            size: alloc_size,
        }
    }
}

pub(super) unsafe fn release(code: *const u8, size: usize) {
    if !code.is_null() && size > 0 {
        unsafe {
            munmap(code as *mut u8, size);
        }
    }
}
