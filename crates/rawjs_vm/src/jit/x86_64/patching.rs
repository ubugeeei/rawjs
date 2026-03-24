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
            let relative = target_code_offset as i64 - (patch_offset as i64 + 4);
            let relative = i32::try_from(relative).expect("x86_64 branch out of range");
            self.code[patch_offset..patch_offset + 4].copy_from_slice(&relative.to_le_bytes());
        }
    }
}
