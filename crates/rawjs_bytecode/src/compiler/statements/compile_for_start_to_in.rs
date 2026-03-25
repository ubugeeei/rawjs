impl Compiler {
    pub(super) fn compile_for(&mut self, for_stmt: &ForStatement) -> Result<()> {
        self.begin_scope();
        if let Some(ref init) = for_stmt.init {
            match init {
                ForInit::VariableDeclaration(vd) => self.compile_variable_declaration(vd)?,
                ForInit::Expression(expr) => {
                    self.compile_expression(expr)?;
                    self.emit(Instruction::Pop);
                }
            }
        }
        let loop_start = self.chunk.current_offset();
        self.push_loop_context(loop_start, None);
        let exit_jump = if let Some(ref test) = for_stmt.test {
            self.compile_expression(test)?;
            Some(self.emit_jump(Instruction::JumpIfFalse))
        } else {
            None
        };
        self.compile_statement(&for_stmt.body)?;
        let continue_target = self.chunk.current_offset();
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.continue_target = continue_target;
        }
        if let Some(ref update) = for_stmt.update {
            self.compile_expression(update)?;
            self.emit(Instruction::Pop);
        }
        let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::Jump(back_offset));
        if let Some(exit) = exit_jump {
            self.patch_jump_to_here(exit);
        }
        self.pop_loop_context_and_patch_breaks();
        self.end_scope();
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_for_in(&mut self, for_in: &ForInStatement) -> Result<()> {
        self.begin_scope();
        self.compile_expression(&for_in.right)?;
        self.emit(Instruction::ForInInit);
        let var_slot = match &for_in.left {
            ForInOfLeft::VariableDeclaration(vd) => {
                if let Some(decl) = vd.declarations.first() {
                    match &decl.id {
                        Pattern::Identifier(id) => self.declare_local(&id.name)?,
                        _ => {
                            return Err(RawJsError::syntax_error(
                                "Destructuring in for-in is not supported",
                                None,
                            ))
                        }
                    }
                } else {
                    return Err(RawJsError::syntax_error(
                        "for-in requires a variable declaration",
                        None,
                    ));
                }
            }
            ForInOfLeft::Pattern(pat) => match pat {
                Pattern::Identifier(id) => {
                    if let Some(slot) = self.resolve_local(&id.name) {
                        slot
                    } else {
                        self.declare_local(&id.name)?
                    }
                }
                _ => {
                    return Err(RawJsError::syntax_error(
                        "Destructuring in for-in is not supported",
                        None,
                    ))
                }
            },
        };
        let loop_start = self.chunk.current_offset();
        self.push_loop_context(loop_start, None);
        let for_in_next_idx = self.emit(Instruction::ForInNext(0));
        self.emit(Instruction::StoreLocal(var_slot));
        self.compile_statement(&for_in.body)?;
        let continue_target = self.chunk.current_offset();
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.continue_target = continue_target;
        }
        let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::Jump(back_offset));
        let exit_offset = self.chunk.current_offset() as i32 - for_in_next_idx as i32 - 1;
        self.chunk.instructions[for_in_next_idx] = Instruction::ForInNext(exit_offset);
        self.pop_loop_context_and_patch_breaks();
        self.emit(Instruction::Pop);
        self.emit(Instruction::Pop);
        self.end_scope();
        Ok(())
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
