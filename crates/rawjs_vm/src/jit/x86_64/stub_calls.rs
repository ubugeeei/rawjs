use super::{JitCompiler, ERROR_EXIT_SENTINEL, FRAME_DONE_SENTINEL};

impl JitCompiler {
    pub(crate) fn emit_stub_call_0(&mut self, stub: extern "C" fn(*mut super::Vm) -> u32) {
        self.emit_mov_rdi_rbx();
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_error_check();
    }

    pub(crate) fn emit_stub_call_1<T>(
        &mut self,
        stub: extern "C" fn(*mut super::Vm, T) -> u32,
        operand: u32,
    ) {
        self.emit_mov_rdi_rbx();
        self.emit_mov_esi_imm32(operand);
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_error_check();
    }

    pub(crate) fn emit_stub_call_2i(
        &mut self,
        stub: extern "C" fn(*mut super::Vm, i32, i32) -> u32,
        op1: i32,
        op2: i32,
    ) {
        self.emit_mov_rdi_rbx();
        self.emit_mov_esi_imm32(op1 as u32);
        self.emit_mov_edx_imm32(op2 as u32);
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_error_check();
    }

    pub(crate) fn emit_stub_call_0_return(&mut self, stub: extern "C" fn(*mut super::Vm) -> u32) {
        self.emit_mov_rdi_rbx();
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_cmp_eax_imm8(2);
        let frame_done_patch = self.emit_je_placeholder();
        self.jump_patches
            .push((frame_done_patch, FRAME_DONE_SENTINEL));
        self.emit_error_check();
    }

    fn emit_error_check(&mut self) {
        self.emit_test_eax_eax();
        let patch_offset = self.emit_jne_placeholder();
        self.jump_patches.push((patch_offset, ERROR_EXIT_SENTINEL));
    }
}
