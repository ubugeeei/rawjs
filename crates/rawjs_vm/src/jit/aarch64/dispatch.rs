use super::{Instruction, JitCompiler};

impl JitCompiler {
    pub(crate) fn emit_instruction(&mut self, instruction: Instruction, bc_index: usize) {
        if self.emit_value_instruction(instruction) {
            return;
        }

        self.emit_flow_instruction(instruction, bc_index);
    }
}
