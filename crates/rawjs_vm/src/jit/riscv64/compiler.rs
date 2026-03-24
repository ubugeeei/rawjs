use super::{reg, Chunk, JitCompiler, JitFunction};

impl JitCompiler {
    fn new() -> Self {
        Self {
            code: Vec::with_capacity(4096),
            pc_map: Vec::new(),
            jump_patches: Vec::new(),
            literal_pool: Vec::new(),
            call_patches: Vec::new(),
        }
    }

    pub fn compile(chunk: &Chunk) -> Option<JitFunction> {
        if !Self::is_jittable(chunk) {
            return None;
        }

        let mut compiler = Self::new();
        compiler.emit_prologue();
        for (index, instruction) in chunk.instructions.iter().copied().enumerate() {
            compiler.pc_map.push(compiler.code.len());
            compiler.emit_instruction(instruction, index);
        }
        compiler.pc_map.push(compiler.code.len());
        compiler.patch_jumps();
        compiler.emit_success_epilogue();
        let literal_offsets = compiler.emit_literal_pool();
        compiler.patch_call_sites(&literal_offsets);
        Some(compiler.finalize())
    }

    fn is_jittable(_chunk: &Chunk) -> bool {
        true
    }

    fn emit_prologue(&mut self) {
        self.emit_addi(reg::SP, reg::SP, -32);
        self.emit_sd(reg::RA, 24, reg::SP);
        self.emit_sd(reg::S0, 16, reg::SP);
        self.emit_sd(reg::S1, 8, reg::SP);
        self.emit_mv(reg::S0, reg::SP);
        self.emit_mv(reg::S1, reg::A0);
    }

    fn emit_success_epilogue(&mut self) {
        self.emit_li32(reg::A0, 0);
        self.emit_epilogue_body();
    }

    pub(crate) fn emit_error_epilogue(&mut self) -> usize {
        let offset = self.code.len();
        self.emit_li32(reg::A0, 1);
        self.emit_epilogue_body();
        offset
    }

    pub(crate) fn emit_frame_done_epilogue(&mut self) -> usize {
        let offset = self.code.len();
        self.emit_li32(reg::A0, 0);
        self.emit_epilogue_body();
        offset
    }

    fn emit_epilogue_body(&mut self) {
        self.emit_ld(reg::S1, 8, reg::SP);
        self.emit_ld(reg::S0, 16, reg::SP);
        self.emit_ld(reg::RA, 24, reg::SP);
        self.emit_addi(reg::SP, reg::SP, 32);
        self.emit_ret();
    }
}
