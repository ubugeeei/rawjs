use super::{JitCompiler, JitFunction};

extern "C" {
    fn mmap(addr: *mut u8, len: usize, prot: i32, flags: i32, fd: i32, offset: i64) -> *mut u8;
    fn mprotect(addr: *mut u8, len: usize, prot: i32) -> i32;
    fn munmap(addr: *mut u8, len: usize) -> i32;
}

const PROT_READ: i32 = 0x01;
const PROT_WRITE: i32 = 0x02;
const PROT_EXEC: i32 = 0x04;
const MAP_PRIVATE: i32 = 0x0002;
#[cfg(target_os = "macos")]
const MAP_ANON: i32 = 0x1000;
#[cfg(not(target_os = "macos"))]
const MAP_ANON: i32 = 0x0020;
const MAP_FAILED: *mut u8 = !0usize as *mut u8;

impl JitCompiler {
    pub(crate) fn finalize(self) -> JitFunction {
        let size = self.code.len();
        let page_size = 4096usize;
        let alloc_size = (size + page_size - 1) & !(page_size - 1);
        let ptr = unsafe {
            let ptr = mmap(
                std::ptr::null_mut(),
                alloc_size,
                PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANON,
                -1,
                0,
            );
            assert_ne!(ptr, MAP_FAILED, "mmap failed for JIT code");
            std::ptr::copy_nonoverlapping(self.code.as_ptr(), ptr, size);
            assert_eq!(
                mprotect(ptr, alloc_size, PROT_READ | PROT_EXEC),
                0,
                "mprotect failed for JIT code",
            );
            ptr
        };
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
