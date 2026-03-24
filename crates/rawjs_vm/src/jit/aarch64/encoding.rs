use super::{reg, JitCompiler};

impl JitCompiler {
    #[inline]
    pub(crate) fn emit_u32(&mut self, instruction: u32) {
        self.code.extend_from_slice(&instruction.to_le_bytes());
    }

    pub(crate) fn emit_stp_pre(&mut self, rt1: u8, rt2: u8, rn: u8, imm: i16) {
        let imm7 = ((imm / 8) as u32) & 0x7F;
        let instruction = 0xA9800000
            | (imm7 << 15)
            | ((rt2 as u32 & 0x1F) << 10)
            | ((rn as u32 & 0x1F) << 5)
            | (rt1 as u32 & 0x1F);
        self.emit_u32(instruction);
    }

    pub(crate) fn emit_ldp_post(&mut self, rt1: u8, rt2: u8, rn: u8, imm: i16) {
        let imm7 = ((imm / 8) as u32) & 0x7F;
        let instruction = 0xA8C00000
            | (imm7 << 15)
            | ((rt2 as u32 & 0x1F) << 10)
            | ((rn as u32 & 0x1F) << 5)
            | (rt1 as u32 & 0x1F);
        self.emit_u32(instruction);
    }

    pub(crate) fn emit_mov_reg(&mut self, rd: u8, rn: u8) {
        let instruction =
            0xAA000000 | ((rn as u32 & 0x1F) << 16) | (0x1F << 5) | (rd as u32 & 0x1F);
        self.emit_u32(instruction);
    }

    pub(crate) fn emit_ret(&mut self) {
        self.emit_u32(0xD65F03C0);
    }

    pub(crate) fn emit_movz_w(&mut self, register: u8, imm16: u16) {
        self.emit_u32(0x52800000 | ((imm16 as u32) << 5) | (register as u32 & 0x1F));
    }

    fn emit_movz(&mut self, register: u8, imm16: u16, hw: u8) {
        self.emit_u32(
            0xD2800000
                | ((hw as u32 & 0x3) << 21)
                | ((imm16 as u32) << 5)
                | (register as u32 & 0x1F),
        );
    }

    fn emit_movk(&mut self, register: u8, imm16: u16, hw: u8) {
        self.emit_u32(
            0xF2800000
                | ((hw as u32 & 0x3) << 21)
                | ((imm16 as u32) << 5)
                | (register as u32 & 0x1F),
        );
    }

    pub(crate) fn emit_mov_imm32(&mut self, register: u8, value: u32) {
        self.emit_movz_w(register, (value & 0xFFFF) as u16);
        let hi = ((value >> 16) & 0xFFFF) as u16;
        if hi != 0 {
            self.emit_u32(0x72A00000 | ((hi as u32) << 5) | (register as u32 & 0x1F));
        }
    }

    pub(crate) fn emit_load_i32(&mut self, register: u8, value: i32) {
        if value >= 0 {
            self.emit_mov_imm32(register, value as u32);
            return;
        }

        let inverted = (!value) as u32;
        let lo = (inverted & 0xFFFF) as u16;
        self.emit_u32(0x12800000 | ((lo as u32) << 5) | (register as u32 & 0x1F));
        let hi = ((value as u32) >> 16) & 0xFFFF;
        if hi != 0 {
            self.emit_u32(0x72A00000 | (hi << 5) | (register as u32 & 0x1F));
        }
    }

    pub(crate) fn emit_load_and_call(&mut self, address: usize) {
        self.emit_mov_imm64(reg::X9, address as u64);
        self.emit_u32(0xD63F0000 | ((reg::X9 as u32 & 0x1F) << 5));
    }

    pub(crate) fn emit_cmp_imm(&mut self, register: u8, imm12: u32) {
        self.emit_u32(0x7100001F | ((imm12 & 0xFFF) << 10) | ((register as u32 & 0x1F) << 5));
    }

    pub(crate) fn emit_tst_imm(&mut self, register: u8, imm: u32) {
        if imm == 0x80000000 {
            self.emit_u32(0x7200001F | (1 << 16) | ((register as u32 & 0x1F) << 5));
        }
    }

    fn emit_mov_imm64(&mut self, register: u8, value: u64) {
        let halfwords = [
            (value & 0xFFFF) as u16,
            ((value >> 16) & 0xFFFF) as u16,
            ((value >> 32) & 0xFFFF) as u16,
            ((value >> 48) & 0xFFFF) as u16,
        ];

        if let Some(first) = halfwords.iter().position(|halfword| *halfword != 0) {
            self.emit_movz(register, halfwords[first], first as u8);
            for (index, halfword) in halfwords.iter().enumerate().skip(first + 1) {
                if *halfword != 0 {
                    self.emit_movk(register, *halfword, index as u8);
                }
            }
        } else {
            self.emit_movz(register, 0, 0);
        }
    }
}
