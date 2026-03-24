use super::JitCompiler;

impl JitCompiler {
    pub(crate) fn emit_u8(&mut self, byte: u8) {
        self.code.push(byte);
    }

    fn emit_bytes(&mut self, bytes: &[u8]) {
        self.code.extend_from_slice(bytes);
    }

    fn emit_u32(&mut self, value: u32) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }

    fn emit_u64(&mut self, value: u64) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }

    pub(crate) fn emit_push_rbp(&mut self) {
        self.emit_u8(0x55);
    }

    pub(crate) fn emit_push_rbx(&mut self) {
        self.emit_u8(0x53);
    }

    pub(crate) fn emit_pop_rbx(&mut self) {
        self.emit_u8(0x5B);
    }

    pub(crate) fn emit_pop_rbp(&mut self) {
        self.emit_u8(0x5D);
    }

    pub(crate) fn emit_mov_rbp_rsp(&mut self) {
        self.emit_bytes(&[0x48, 0x89, 0xE5]);
    }

    pub(crate) fn emit_mov_rbx_rdi(&mut self) {
        self.emit_bytes(&[0x48, 0x89, 0xFB]);
    }

    pub(crate) fn emit_mov_rdi_rbx(&mut self) {
        self.emit_bytes(&[0x48, 0x89, 0xDF]);
    }

    pub(crate) fn emit_sub_rsp_imm8(&mut self, value: u8) {
        self.emit_bytes(&[0x48, 0x83, 0xEC, value]);
    }

    pub(crate) fn emit_add_rsp_imm8(&mut self, value: u8) {
        self.emit_bytes(&[0x48, 0x83, 0xC4, value]);
    }

    pub(crate) fn emit_mov_eax_imm32(&mut self, value: u32) {
        self.emit_u8(0xB8);
        self.emit_u32(value);
    }

    pub(crate) fn emit_mov_esi_imm32(&mut self, value: u32) {
        self.emit_u8(0xBE);
        self.emit_u32(value);
    }

    pub(crate) fn emit_mov_edx_imm32(&mut self, value: u32) {
        self.emit_u8(0xBA);
        self.emit_u32(value);
    }

    pub(crate) fn emit_load_and_call(&mut self, address: usize) {
        self.emit_bytes(&[0x48, 0xB8]);
        self.emit_u64(address as u64);
        self.emit_bytes(&[0xFF, 0xD0]);
    }

    pub(crate) fn emit_ret(&mut self) {
        self.emit_u8(0xC3);
    }

    pub(crate) fn emit_test_eax_eax(&mut self) {
        self.emit_bytes(&[0x85, 0xC0]);
    }

    pub(crate) fn emit_test_eax_imm32(&mut self, value: u32) {
        self.emit_u8(0xA9);
        self.emit_u32(value);
    }

    pub(crate) fn emit_cmp_eax_imm8(&mut self, value: u8) {
        self.emit_bytes(&[0x83, 0xF8, value]);
    }

    pub(crate) fn emit_jmp_placeholder(&mut self) -> usize {
        self.emit_u8(0xE9);
        let offset = self.code.len();
        self.emit_u32(0);
        offset
    }

    pub(crate) fn emit_je_placeholder(&mut self) -> usize {
        self.emit_bytes(&[0x0F, 0x84]);
        let offset = self.code.len();
        self.emit_u32(0);
        offset
    }

    pub(crate) fn emit_jne_placeholder(&mut self) -> usize {
        self.emit_bytes(&[0x0F, 0x85]);
        let offset = self.code.len();
        self.emit_u32(0);
        offset
    }

    pub(crate) fn emit_js_placeholder(&mut self) -> usize {
        self.emit_bytes(&[0x0F, 0x88]);
        let offset = self.code.len();
        self.emit_u32(0);
        offset
    }
}
