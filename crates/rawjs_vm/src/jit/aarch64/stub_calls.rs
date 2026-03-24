use super::{reg, JitCompiler, ERROR_EXIT_SENTINEL, FRAME_DONE_SENTINEL};

impl JitCompiler {
    pub(crate) fn emit_stub_call_0(&mut self, stub: extern "C" fn(*mut super::Vm) -> u32) {
        self.emit_mov_reg(reg::X0, reg::X19);
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_error_check();
    }

    pub(crate) fn emit_stub_call_1<T>(
        &mut self,
        stub: extern "C" fn(*mut super::Vm, T) -> u32,
        operand: u32,
    ) {
        self.emit_mov_reg(reg::X0, reg::X19);
        self.emit_mov_imm32(reg::X1, operand);
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_error_check();
    }

    pub(crate) fn emit_stub_call_2i(
        &mut self,
        stub: extern "C" fn(*mut super::Vm, i32, i32) -> u32,
        op1: i32,
        op2: i32,
    ) {
        self.emit_mov_reg(reg::X0, reg::X19);
        self.emit_load_i32(reg::X1, op1);
        self.emit_load_i32(reg::X2, op2);
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_error_check();
    }

    pub(crate) fn emit_stub_call_0_return(&mut self, stub: extern "C" fn(*mut super::Vm) -> u32) {
        self.emit_mov_reg(reg::X0, reg::X19);
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_cmp_imm(reg::X0, 2);
        let frame_done_patch = self.code.len();
        self.emit_u32(0x54000000);
        self.jump_patches
            .push((frame_done_patch, FRAME_DONE_SENTINEL));
        self.emit_error_check();
    }

    fn emit_error_check(&mut self) {
        let patch_offset = self.code.len();
        self.emit_u32(0x35000000);
        self.jump_patches.push((patch_offset, ERROR_EXIT_SENTINEL));
    }
}
