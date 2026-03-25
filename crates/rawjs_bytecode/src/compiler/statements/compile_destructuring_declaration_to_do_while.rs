impl Compiler {
    #[doc = " Compile a destructuring pattern for a variable declaration."]
    #[doc = " Expects the value to destructure on TOS.  Leaves locals on the stack."]
    pub(crate) fn compile_destructuring_declaration(&mut self, pattern: &Pattern) -> Result<()> {
        match pattern {
            Pattern::Identifier(id) => {
                self.declare_local(&id.name)?;
                Ok(())
            }
            Pattern::Array(arr) => {
                for (i, elem) in arr.elements.iter().enumerate() {
                    if let Some(pat) = elem {
                        self.emit(Instruction::Dup);
                        let idx = self.add_number_constant(i as f64)?;
                        self.emit(Instruction::LoadConst(idx));
                        self.emit(Instruction::GetComputed);
                        self.compile_destructuring_declaration(pat)?;
                    }
                }
                self.emit(Instruction::Pop);
                Ok(())
            }
            Pattern::Object(obj) => {
                for prop in &obj.properties {
                    self.emit(Instruction::Dup);
                    if prop.computed {
                        self.compile_expression(&prop.key)?;
                        self.emit(Instruction::GetComputed);
                    } else {
                        let key_name = expression_to_property_name(&prop.key)?;
                        let idx = self.add_string_constant(&key_name)?;
                        self.emit(Instruction::GetProperty(idx));
                    }
                    self.compile_destructuring_declaration(&prop.value)?;
                }
                self.emit(Instruction::Pop);
                Ok(())
            }
            Pattern::Assignment(asn) => {
                self.emit(Instruction::Dup);
                self.emit(Instruction::Undefined);
                self.emit(Instruction::StrictEq);
                let jump_idx = self.emit_jump(Instruction::JumpIfFalse);
                self.emit(Instruction::Pop);
                self.compile_expression(&asn.right)?;
                let end_idx = self.emit_jump(Instruction::Jump);
                self.patch_jump_to_here(jump_idx);
                self.patch_jump_to_here(end_idx);
                self.compile_destructuring_declaration(&asn.left)?;
                Ok(())
            }
            Pattern::Rest(rest) => {
                self.compile_destructuring_declaration(&rest.argument)?;
                Ok(())
            }
        }
    }
}

impl Compiler {
    pub(super) fn compile_return(&mut self, ret: &ReturnStatement) -> Result<()> {
        if let Some(ref arg) = ret.argument {
            self.compile_expression(arg)?;
        } else {
            self.emit(Instruction::Undefined);
        }
        self.emit(Instruction::Return);
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_if(&mut self, if_stmt: &IfStatement) -> Result<()> {
        self.compile_expression(&if_stmt.test)?;
        let else_jump = self.emit_jump(Instruction::JumpIfFalse);
        self.compile_statement(&if_stmt.consequent)?;
        if let Some(ref alt) = if_stmt.alternate {
            let end_jump = self.emit_jump(Instruction::Jump);
            self.patch_jump_to_here(else_jump);
            self.compile_statement(alt)?;
            self.patch_jump_to_here(end_jump);
        } else {
            self.patch_jump_to_here(else_jump);
        }
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_while(&mut self, while_stmt: &WhileStatement) -> Result<()> {
        let loop_start = self.chunk.current_offset();
        self.push_loop_context(loop_start, None);
        self.compile_expression(&while_stmt.test)?;
        let exit_jump = self.emit_jump(Instruction::JumpIfFalse);
        self.compile_statement(&while_stmt.body)?;
        let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::Jump(back_offset));
        self.patch_jump_to_here(exit_jump);
        self.pop_loop_context_and_patch_breaks();
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_do_while(&mut self, do_while: &DoWhileStatement) -> Result<()> {
        let loop_start = self.chunk.current_offset();
        self.push_loop_context(loop_start, None);
        self.compile_statement(&do_while.body)?;
        self.compile_expression(&do_while.test)?;
        let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::JumpIfTrue(back_offset));
        self.pop_loop_context_and_patch_breaks();
        Ok(())
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
