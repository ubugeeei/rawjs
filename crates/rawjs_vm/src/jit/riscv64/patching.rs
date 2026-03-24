use super::encoding::{encode_b, encode_i, encode_j, encode_u};
use super::{JitCompiler, ERROR_EXIT_SENTINEL, FRAME_DONE_SENTINEL};

impl JitCompiler {
    pub(crate) fn patch_jumps(&mut self) {
        let error_exit_offset = self.emit_error_epilogue();
        let frame_done_offset = self.emit_frame_done_epilogue();

        for &(patch_offset, target_bc) in &self.jump_patches {
            let target_code_offset = match target_bc {
                ERROR_EXIT_SENTINEL => error_exit_offset,
                FRAME_DONE_SENTINEL => frame_done_offset,
                target if target < self.pc_map.len() => self.pc_map[target],
                _ => self.code.len(),
            };
            let delta = target_code_offset as i64 - patch_offset as i64;
            let delta = i32::try_from(delta).expect("riscv64 branch out of range");
            let existing = u32::from_le_bytes(
                self.code[patch_offset..patch_offset + 4]
                    .try_into()
                    .unwrap(),
            );
            let patched = patch_jump(existing, delta);
            self.code[patch_offset..patch_offset + 4].copy_from_slice(&patched.to_le_bytes());
        }
    }

    pub(crate) fn emit_literal_pool(&mut self) -> Vec<usize> {
        let mut offsets = Vec::with_capacity(self.literal_pool.len());
        for literal in &self.literal_pool {
            offsets.push(self.code.len());
            self.code.extend_from_slice(&literal.to_le_bytes());
        }
        offsets
    }

    pub(crate) fn patch_call_sites(&mut self, literal_offsets: &[usize]) {
        for &(auipc_offset, literal_index) in &self.call_patches {
            let literal_offset = literal_offsets[literal_index];
            let delta = literal_offset as i64 - auipc_offset as i64;
            let upper = ((delta + 0x800) >> 12) as i32;
            let lower = delta - ((upper as i64) << 12);
            assert!(
                (-2048..=2047).contains(&lower),
                "riscv64 literal load out of range",
            );

            let auipc = u32::from_le_bytes(
                self.code[auipc_offset..auipc_offset + 4]
                    .try_into()
                    .unwrap(),
            );
            let rd = ((auipc >> 7) & 0x1f) as u8;
            let patched_auipc = encode_u(0x17, rd, upper);
            self.code[auipc_offset..auipc_offset + 4].copy_from_slice(&patched_auipc.to_le_bytes());

            let ld_offset = auipc_offset + 4;
            let load = u32::from_le_bytes(self.code[ld_offset..ld_offset + 4].try_into().unwrap());
            let rd = ((load >> 7) & 0x1f) as u8;
            let rs1 = ((load >> 15) & 0x1f) as u8;
            let funct3 = (load >> 12) & 0x7;
            let patched_load = encode_i(0x03, funct3, rd, rs1, lower as i32);
            self.code[ld_offset..ld_offset + 4].copy_from_slice(&patched_load.to_le_bytes());
        }
    }
}

fn patch_jump(existing: u32, delta: i32) -> u32 {
    match existing & 0x7f {
        0x6f => {
            let rd = ((existing >> 7) & 0x1f) as u8;
            encode_j(0x6f, rd, delta)
        }
        0x63 => {
            let funct3 = (existing >> 12) & 0x7;
            let rs1 = ((existing >> 15) & 0x1f) as u8;
            let rs2 = ((existing >> 20) & 0x1f) as u8;
            encode_b(0x63, funct3, rs1, rs2, delta)
        }
        _ => existing,
    }
}
