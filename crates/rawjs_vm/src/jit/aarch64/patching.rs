use super::{JitCompiler, ERROR_EXIT_SENTINEL, FRAME_DONE_SENTINEL};

impl JitCompiler {
    pub(crate) fn patch_jumps(&mut self) {
        let error_exit_offset = self.emit_error_epilogue();
        let frame_done_offset = self.emit_frame_done_epilogue();

        for &(code_offset, target_bc) in &self.jump_patches {
            let target_code_offset = match target_bc {
                ERROR_EXIT_SENTINEL => error_exit_offset,
                FRAME_DONE_SENTINEL => frame_done_offset,
                target if target < self.pc_map.len() => self.pc_map[target],
                _ => self.code.len(),
            };

            let delta = (target_code_offset as i64 - code_offset as i64) / 4;
            let existing =
                u32::from_le_bytes(self.code[code_offset..code_offset + 4].try_into().unwrap());
            let patched = patch_branch(existing, delta as u32);
            self.code[code_offset..code_offset + 4].copy_from_slice(&patched.to_le_bytes());
        }
    }
}

fn patch_branch(existing: u32, delta: u32) -> u32 {
    if existing & 0xFC000000 == 0x14000000 {
        0x14000000 | (delta & 0x03FFFFFF)
    } else if existing & 0xFF000010 == 0x54000000 {
        0x54000000 | ((delta & 0x7FFFF) << 5) | (existing & 0xF)
    } else if existing & 0xFF000000 == 0x34000000 {
        0x34000000 | ((delta & 0x7FFFF) << 5) | (existing & 0x1F)
    } else if existing & 0xFF000000 == 0x35000000 {
        0x35000000 | ((delta & 0x7FFFF) << 5) | (existing & 0x1F)
    } else {
        existing
    }
}
