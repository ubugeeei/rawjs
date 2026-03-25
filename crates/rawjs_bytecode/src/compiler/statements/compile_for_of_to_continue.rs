impl Compiler {
    pub(super) fn compile_for_of(&mut self, for_of: &ForOfStatement) -> Result<()> {
        self.begin_scope();
        self.compile_expression(&for_of.right)?;
        self.emit(Instruction::GetIterator);
        let var_slot = match &for_of.left {
            ForInOfLeft::VariableDeclaration(vd) => {
                if let Some(decl) = vd.declarations.first() {
                    match &decl.id {
                        Pattern::Identifier(id) => self.declare_local(&id.name)?,
                        _ => {
                            return Err(RawJsError::syntax_error(
                                "Destructuring in for-of is not yet supported",
                                None,
                            ))
                        }
                    }
                } else {
                    return Err(RawJsError::syntax_error(
                        "for-of requires a variable declaration",
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
                        "Destructuring in for-of is not yet supported",
                        None,
                    ))
                }
            },
        };
        let loop_start = self.chunk.current_offset();
        self.push_loop_context(loop_start, None);
        self.emit(Instruction::IteratorNext);
        let iter_done_idx = self.emit(Instruction::IteratorDone(0));
        self.emit(Instruction::StoreLocal(var_slot));
        self.compile_statement(&for_of.body)?;
        let continue_target = self.chunk.current_offset();
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.continue_target = continue_target;
        }
        let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::Jump(back_offset));
        let exit_offset = self.chunk.current_offset() as i32 - iter_done_idx as i32 - 1;
        self.chunk.instructions[iter_done_idx] = Instruction::IteratorDone(exit_offset);
        self.pop_loop_context_and_patch_breaks();
        self.emit(Instruction::Pop);
        self.end_scope();
        Ok(())
    }
}

impl Compiler {
    pub(crate) fn compile_block(&mut self, block: &BlockStatement) -> Result<()> {
        self.begin_scope();
        for stmt in &block.body {
            if let Statement::FunctionDeclaration(func) = stmt {
                self.hoist_function_declaration(func)?;
            }
        }
        for stmt in &block.body {
            if matches!(stmt, Statement::FunctionDeclaration(_)) {
                continue;
            }
            self.compile_statement(stmt)?;
        }
        self.end_scope();
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_break(&mut self, brk: &BreakStatement) -> Result<()> {
        let ctx_idx = if let Some(ref label) = brk.label {
            self.loop_stack
                .iter()
                .rposition(|c| c.label.as_deref() == Some(label))
                .ok_or_else(|| {
                    RawJsError::syntax_error(
                        format!("Undefined label '{}'", label),
                        Some(brk.location),
                    )
                })?
        } else {
            self.loop_stack.len().checked_sub(1).ok_or_else(|| {
                RawJsError::syntax_error("'break' outside of loop or switch", Some(brk.location))
            })?
        };
        let loop_depth = self.loop_stack[ctx_idx].scope_depth;
        let locals_to_pop = self
            .locals
            .iter()
            .rev()
            .take_while(|l| l.depth > loop_depth)
            .count();
        for _ in 0..locals_to_pop {
            self.emit(Instruction::Pop);
        }
        let jump_idx = self.emit_jump(Instruction::Jump);
        self.loop_stack[ctx_idx].break_jumps.push(jump_idx);
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_continue(&mut self, cont: &ContinueStatement) -> Result<()> {
        let ctx_idx = if let Some(ref label) = cont.label {
            self.loop_stack
                .iter()
                .rposition(|c| c.label.as_deref() == Some(label))
                .ok_or_else(|| {
                    RawJsError::syntax_error(
                        format!("Undefined label '{}'", label),
                        Some(cont.location),
                    )
                })?
        } else {
            self.loop_stack.len().checked_sub(1).ok_or_else(|| {
                RawJsError::syntax_error("'continue' outside of loop", Some(cont.location))
            })?
        };
        let loop_depth = self.loop_stack[ctx_idx].scope_depth;
        let locals_to_pop = self
            .locals
            .iter()
            .rev()
            .take_while(|l| l.depth > loop_depth)
            .count();
        for _ in 0..locals_to_pop {
            self.emit(Instruction::Pop);
        }
        let continue_target = self.loop_stack[ctx_idx].continue_target;
        let offset = continue_target as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::Jump(offset));
        Ok(())
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
