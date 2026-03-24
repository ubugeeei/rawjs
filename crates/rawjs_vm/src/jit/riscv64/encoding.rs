use super::{reg, JitCompiler};

impl JitCompiler {
    pub(crate) fn emit_u32(&mut self, instruction: u32) {
        self.code.extend_from_slice(&instruction.to_le_bytes());
    }

    pub(crate) fn emit_addi(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.emit_u32(encode_i(0x13, 0b000, rd, rs1, imm));
    }

    pub(crate) fn emit_sd(&mut self, rs2: u8, imm: i32, rs1: u8) {
        self.emit_u32(encode_s(0x23, 0b011, rs1, rs2, imm));
    }

    pub(crate) fn emit_ld(&mut self, rd: u8, imm: i32, rs1: u8) {
        self.emit_u32(encode_i(0x03, 0b011, rd, rs1, imm));
    }

    pub(crate) fn emit_mv(&mut self, rd: u8, rs: u8) {
        self.emit_addi(rd, rs, 0);
    }

    pub(crate) fn emit_lui(&mut self, rd: u8, imm20: i32) {
        self.emit_u32(encode_u(0x37, rd, imm20));
    }

    pub(crate) fn emit_li32(&mut self, rd: u8, value: i32) {
        if (-2048..=2047).contains(&value) {
            self.emit_addi(rd, reg::ZERO, value);
            return;
        }
        let upper = ((value as i64 + 0x800) >> 12) as i32;
        let lower = value - (upper << 12);
        self.emit_lui(rd, upper);
        self.emit_addi(rd, rd, lower);
    }

    pub(crate) fn emit_and(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.emit_u32(encode_r(0x33, 0b111, 0, rd, rs1, rs2));
    }

    pub(crate) fn emit_jalr(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.emit_u32(encode_i(0x67, 0b000, rd, rs1, imm));
    }

    pub(crate) fn emit_ret(&mut self) {
        self.emit_jalr(reg::ZERO, reg::RA, 0);
    }

    pub(crate) fn emit_jal_placeholder(&mut self, rd: u8) -> usize {
        let offset = self.code.len();
        self.emit_u32(encode_j(0x6f, rd, 0));
        offset
    }

    pub(crate) fn emit_beq_placeholder(&mut self, rs1: u8, rs2: u8) -> usize {
        let offset = self.code.len();
        self.emit_u32(encode_b(0x63, 0b000, rs1, rs2, 0));
        offset
    }

    pub(crate) fn emit_bne_placeholder(&mut self, rs1: u8, rs2: u8) -> usize {
        let offset = self.code.len();
        self.emit_u32(encode_b(0x63, 0b001, rs1, rs2, 0));
        offset
    }

    pub(crate) fn emit_load_and_call(&mut self, address: usize) {
        let literal_index = self.literal_pool.len();
        self.literal_pool.push(address as u64);
        let auipc_offset = self.code.len();
        self.emit_u32(encode_u(0x17, reg::T0, 0));
        self.emit_u32(encode_i(0x03, 0b011, reg::T0, reg::T0, 0));
        self.call_patches.push((auipc_offset, literal_index));
        self.emit_jalr(reg::RA, reg::T0, 0);
    }
}

pub(super) fn encode_i(opcode: u32, funct3: u32, rd: u8, rs1: u8, imm: i32) -> u32 {
    (((imm as u32) & 0x0fff) << 20)
        | ((rs1 as u32) << 15)
        | (funct3 << 12)
        | ((rd as u32) << 7)
        | opcode
}

pub(super) fn encode_s(opcode: u32, funct3: u32, rs1: u8, rs2: u8, imm: i32) -> u32 {
    let imm = imm as u32;
    (((imm >> 5) & 0x7f) << 25)
        | ((rs2 as u32) << 20)
        | ((rs1 as u32) << 15)
        | (funct3 << 12)
        | ((imm & 0x1f) << 7)
        | opcode
}

pub(super) fn encode_r(opcode: u32, funct3: u32, funct7: u32, rd: u8, rs1: u8, rs2: u8) -> u32 {
    (funct7 << 25)
        | ((rs2 as u32) << 20)
        | ((rs1 as u32) << 15)
        | (funct3 << 12)
        | ((rd as u32) << 7)
        | opcode
}

pub(super) fn encode_b(opcode: u32, funct3: u32, rs1: u8, rs2: u8, offset: i32) -> u32 {
    let offset = offset as u32;
    (((offset >> 12) & 0x1) << 31)
        | (((offset >> 5) & 0x3f) << 25)
        | ((rs2 as u32) << 20)
        | ((rs1 as u32) << 15)
        | (funct3 << 12)
        | (((offset >> 1) & 0x0f) << 8)
        | (((offset >> 11) & 0x1) << 7)
        | opcode
}

pub(super) fn encode_u(opcode: u32, rd: u8, imm20: i32) -> u32 {
    (((imm20 as u32) & 0x000f_ffff) << 12) | ((rd as u32) << 7) | opcode
}

pub(super) fn encode_j(opcode: u32, rd: u8, offset: i32) -> u32 {
    let offset = offset as u32;
    (((offset >> 20) & 0x1) << 31)
        | (((offset >> 1) & 0x03ff) << 21)
        | (((offset >> 11) & 0x1) << 20)
        | (((offset >> 12) & 0x0ff) << 12)
        | ((rd as u32) << 7)
        | opcode
}
