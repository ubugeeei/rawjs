impl Compiler {
    pub(super) fn compile_labeled(&mut self, labeled: &LabeledStatement) -> Result<()> {
        match labeled.body.as_ref() {
            Statement::While(w) => {
                let loop_start = self.chunk.current_offset();
                self.push_loop_context(loop_start, Some(labeled.label.clone()));
                self.compile_expression(&w.test)?;
                let exit_jump = self.emit_jump(Instruction::JumpIfFalse);
                self.compile_statement(&w.body)?;
                let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
                self.emit(Instruction::Jump(back_offset));
                self.patch_jump_to_here(exit_jump);
                self.pop_loop_context_and_patch_breaks();
            }
            Statement::For(f) => {
                self.begin_scope();
                if let Some(ref init) = f.init {
                    match init {
                        ForInit::VariableDeclaration(vd) => {
                            self.compile_variable_declaration(vd)?
                        }
                        ForInit::Expression(expr) => {
                            self.compile_expression(expr)?;
                            self.emit(Instruction::Pop);
                        }
                    }
                }
                let loop_start = self.chunk.current_offset();
                self.push_loop_context(loop_start, Some(labeled.label.clone()));
                let exit_jump = if let Some(ref test) = f.test {
                    self.compile_expression(test)?;
                    Some(self.emit_jump(Instruction::JumpIfFalse))
                } else {
                    None
                };
                self.compile_statement(&f.body)?;
                let continue_target = self.chunk.current_offset();
                if let Some(ctx) = self.loop_stack.last_mut() {
                    ctx.continue_target = continue_target;
                }
                if let Some(ref update) = f.update {
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
            }
            _ => {
                let here = self.chunk.current_offset();
                self.push_loop_context(here, Some(labeled.label.clone()));
                self.compile_statement(&labeled.body)?;
                self.pop_loop_context_and_patch_breaks();
            }
        }
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_class_declaration(&mut self, cls: &ClassDeclaration) -> Result<()> {
        if let Some(ref name) = cls.id {
            if self.is_repl_top_level_scope() {
                let (slot, _) = self.declare_binding_with_storage(name, true)?;
                self.compile_class_body(cls)?;
                self.emit(Instruction::StoreLocal(slot));
                self.emit(Instruction::LoadLocal(slot));
                let idx = self.add_string_constant(name)?;
                self.emit(Instruction::InitGlobal(idx));
                return Ok(());
            }
            let slot = self.declare_local(name)?;
            self.compile_class_body(cls)?;
            self.emit(Instruction::StoreLocal(slot));
            let len = self.chunk.instructions.len();
            self.chunk.instructions.truncate(len - 1);
        } else {
            self.compile_class_body(cls)?;
            self.emit(Instruction::Pop);
        }
        Ok(())
    }
}

impl Compiler {
    pub(crate) fn compile_class_body(&mut self, cls: &ClassDeclaration) -> Result<()> {
        let mut found_constructor = false;
        for member in &cls.body {
            if member.kind == ClassMemberKind::Constructor {
                if let Some(ref value) = member.value {
                    self.compile_expression(value)?;
                    found_constructor = true;
                    break;
                }
            }
        }
        if !found_constructor {
            self.emit(Instruction::CreateObject);
        }
        for member in &cls.body {
            if member.kind == ClassMemberKind::Constructor {
                continue;
            }
            if let Some(ref value) = member.value {
                self.emit(Instruction::Dup);
                self.compile_expression(value)?;
                if member.computed {
                    self.compile_expression(&member.key)?;
                    self.emit(Instruction::SetComputed);
                } else {
                    let key_name = expression_to_property_name(&member.key)?;
                    let idx = self.add_string_constant(&key_name)?;
                    self.emit(Instruction::SetProperty(idx));
                }
                self.emit(Instruction::Pop);
            }
        }
        Ok(())
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
