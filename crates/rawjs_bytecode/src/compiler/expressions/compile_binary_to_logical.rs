impl Compiler {
    pub(super) fn compile_binary(&mut self, bin: &BinaryExpression) -> Result<()> {
        self.compile_expression(&bin.left)?;
        self.compile_expression(&bin.right)?;
        let instr = match bin.operator {
            BinaryOp::Add => Instruction::Add,
            BinaryOp::Sub => Instruction::Sub,
            BinaryOp::Mul => Instruction::Mul,
            BinaryOp::Div => Instruction::Div,
            BinaryOp::Mod => Instruction::Mod,
            BinaryOp::Exp => Instruction::Exp,
            BinaryOp::BitAnd => Instruction::BitAnd,
            BinaryOp::BitOr => Instruction::BitOr,
            BinaryOp::BitXor => Instruction::BitXor,
            BinaryOp::Shl => Instruction::Shl,
            BinaryOp::Shr => Instruction::Shr,
            BinaryOp::UShr => Instruction::UShr,
            BinaryOp::Eq => Instruction::Eq,
            BinaryOp::StrictEq => Instruction::StrictEq,
            BinaryOp::Ne => Instruction::Ne,
            BinaryOp::StrictNe => Instruction::StrictNe,
            BinaryOp::Lt => Instruction::Lt,
            BinaryOp::Le => Instruction::Le,
            BinaryOp::Gt => Instruction::Gt,
            BinaryOp::Ge => Instruction::Ge,
            BinaryOp::In => Instruction::In,
            BinaryOp::Instanceof => Instruction::Instanceof,
        };
        self.emit(instr);
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_logical(&mut self, log: &LogicalExpression) -> Result<()> {
        self.compile_expression(&log.left)?;
        match log.operator {
            LogicalOp::And => {
                self.emit(Instruction::Dup);
                let jump_idx = self.emit_jump(Instruction::JumpIfFalse);
                self.emit(Instruction::Pop);
                self.compile_expression(&log.right)?;
                self.patch_jump_to_here(jump_idx);
            }
            LogicalOp::Or => {
                self.emit(Instruction::Dup);
                let jump_idx = self.emit_jump(Instruction::JumpIfTrue);
                self.emit(Instruction::Pop);
                self.compile_expression(&log.right)?;
                self.patch_jump_to_here(jump_idx);
            }
            LogicalOp::NullishCoalescing => {
                self.emit(Instruction::Dup);
                self.emit(Instruction::Null);
                self.emit(Instruction::StrictEq);
                let null_check = self.emit_jump(Instruction::JumpIfTrue);
                self.emit(Instruction::Dup);
                self.emit(Instruction::Undefined);
                self.emit(Instruction::StrictEq);
                let undef_check = self.emit_jump(Instruction::JumpIfTrue);
                let end_jump = self.emit_jump(Instruction::Jump);
                self.patch_jump_to_here(null_check);
                self.patch_jump_to_here(undef_check);
                self.emit(Instruction::Pop);
                self.compile_expression(&log.right)?;
                self.patch_jump_to_here(end_jump);
            }
        }
        Ok(())
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
