use super::{reg, JitCompiler, ERROR_EXIT_SENTINEL, FRAME_DONE_SENTINEL};

impl JitCompiler {
    pub(crate) fn emit_stub_call_0(&mut self, stub: extern "C" fn(*mut super::Vm) -> u32) {
        self.emit_mv(reg::A0, reg::S1);
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_error_check();
    }

    pub(crate) fn emit_stub_call_1<T>(
        &mut self,
        stub: extern "C" fn(*mut super::Vm, T) -> u32,
        operand: u32,
    ) {
        self.emit_mv(reg::A0, reg::S1);
        self.emit_li32(reg::A1, operand as i32);
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_error_check();
    }

    pub(crate) fn emit_stub_call_2i(
        &mut self,
        stub: extern "C" fn(*mut super::Vm, i32, i32) -> u32,
        op1: i32,
        op2: i32,
    ) {
        self.emit_mv(reg::A0, reg::S1);
        self.emit_li32(reg::A1, op1);
        self.emit_li32(reg::A2, op2);
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_error_check();
    }

    pub(crate) fn emit_stub_call_0_return(&mut self, stub: extern "C" fn(*mut super::Vm) -> u32) {
        self.emit_mv(reg::A0, reg::S1);
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_addi(reg::T1, reg::ZERO, 2);
        let frame_done_patch = self.emit_beq_placeholder(reg::A0, reg::T1);
        self.jump_patches
            .push((frame_done_patch, FRAME_DONE_SENTINEL));
        self.emit_error_check();
    }

    fn emit_error_check(&mut self) {
        let patch_offset = self.emit_bne_placeholder(reg::A0, reg::ZERO);
        self.jump_patches.push((patch_offset, ERROR_EXIT_SENTINEL));
    }
}
