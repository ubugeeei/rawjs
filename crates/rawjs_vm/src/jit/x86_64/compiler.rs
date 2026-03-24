use super::{Chunk, JitCompiler, JitFunction};

impl JitCompiler {
    fn new() -> Self {
        Self {
            code: Vec::with_capacity(4096),
            pc_map: Vec::new(),
            jump_patches: Vec::new(),
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
        Some(compiler.finalize())
    }

    fn is_jittable(_chunk: &Chunk) -> bool {
        true
    }

    fn emit_prologue(&mut self) {
        self.emit_push_rbp();
        self.emit_mov_rbp_rsp();
        self.emit_push_rbx();
        self.emit_sub_rsp_imm8(8);
        self.emit_mov_rbx_rdi();
    }

    fn emit_success_epilogue(&mut self) {
        self.emit_mov_eax_imm32(0);
        self.emit_epilogue_body();
    }

    pub(crate) fn emit_error_epilogue(&mut self) -> usize {
        let offset = self.code.len();
        self.emit_mov_eax_imm32(1);
        self.emit_epilogue_body();
        offset
    }

    pub(crate) fn emit_frame_done_epilogue(&mut self) -> usize {
        let offset = self.code.len();
        self.emit_mov_eax_imm32(0);
        self.emit_epilogue_body();
        offset
    }

    fn emit_epilogue_body(&mut self) {
        self.emit_add_rsp_imm8(8);
        self.emit_pop_rbx();
        self.emit_pop_rbp();
        self.emit_ret();
    }
}
