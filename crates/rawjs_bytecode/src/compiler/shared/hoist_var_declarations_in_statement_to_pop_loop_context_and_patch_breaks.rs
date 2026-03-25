impl Compiler {
    pub(super) fn hoist_var_declarations_in_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::VariableDeclaration(vd) if vd.kind == VarKind::Var => {
                for decl in &vd.declarations {
                    self.hoist_var_pattern(&decl.id)?;
                }
            }
            Statement::Block(block) => self.hoist_var_declarations(&block.body)?,
            Statement::If(if_stmt) => {
                self.hoist_var_declarations_in_statement(&if_stmt.consequent)?;
                if let Some(alternate) = &if_stmt.alternate {
                    self.hoist_var_declarations_in_statement(alternate)?;
                }
            }
            Statement::While(while_stmt) => {
                self.hoist_var_declarations_in_statement(&while_stmt.body)?;
            }
            Statement::DoWhile(do_while) => {
                self.hoist_var_declarations_in_statement(&do_while.body)?;
            }
            Statement::For(for_stmt) => {
                if let Some(ForInit::VariableDeclaration(vd)) = &for_stmt.init {
                    if vd.kind == VarKind::Var {
                        for decl in &vd.declarations {
                            self.hoist_var_pattern(&decl.id)?;
                        }
                    }
                }
                self.hoist_var_declarations_in_statement(&for_stmt.body)?;
            }
            Statement::ForIn(for_in) => {
                if let ForInOfLeft::VariableDeclaration(vd) = &for_in.left {
                    if vd.kind == VarKind::Var {
                        for decl in &vd.declarations {
                            self.hoist_var_pattern(&decl.id)?;
                        }
                    }
                }
                self.hoist_var_declarations_in_statement(&for_in.body)?;
            }
            Statement::ForOf(for_of) => {
                if let ForInOfLeft::VariableDeclaration(vd) = &for_of.left {
                    if vd.kind == VarKind::Var {
                        for decl in &vd.declarations {
                            self.hoist_var_pattern(&decl.id)?;
                        }
                    }
                }
                self.hoist_var_declarations_in_statement(&for_of.body)?;
            }
            Statement::Switch(sw) => {
                for case in &sw.cases {
                    self.hoist_var_declarations(&case.consequent)?;
                }
            }
            Statement::Try(try_stmt) => {
                self.hoist_var_declarations(&try_stmt.block.body)?;
                if let Some(handler) = &try_stmt.handler {
                    self.hoist_var_declarations(&handler.body.body)?;
                }
                if let Some(finalizer) = &try_stmt.finalizer {
                    self.hoist_var_declarations(&finalizer.body)?;
                }
            }
            Statement::Labeled(labeled) => {
                self.hoist_var_declarations_in_statement(&labeled.body)?;
            }
            Statement::ExportDeclaration(export_decl) => {
                if let ExportKind::Declaration(inner) = &export_decl.kind {
                    self.hoist_var_declarations_in_statement(inner)?;
                }
            }
            _ => {}
        }
        Ok(())
    }
}

impl Compiler {
    pub(super) fn hoist_var_pattern(&mut self, pattern: &Pattern) -> Result<()> {
        match pattern {
            Pattern::Identifier(id) => {
                let slot = match self.resolve_local_storage(&id.name) {
                    Some((slot, _)) => slot,
                    None if !self.in_function && self.scope_depth == 0 => {
                        self.declare_global_alias_local(&id.name)?
                    }
                    None => self.declare_local(&id.name)?,
                };
                self.emit(Instruction::Undefined);
                self.emit(Instruction::StoreLocal(slot));
                if !self.in_function && self.scope_depth == 0 {
                    self.emit(Instruction::LoadLocal(slot));
                    let idx = self.add_string_constant(&id.name)?;
                    self.emit(Instruction::InitGlobal(idx));
                }
            }
            Pattern::Array(array) => {
                for pattern in array.elements.iter().flatten() {
                    self.hoist_var_pattern(pattern)?;
                }
            }
            Pattern::Object(object) => {
                for property in &object.properties {
                    self.hoist_var_pattern(&property.value)?;
                }
            }
            Pattern::Assignment(assignment) => self.hoist_var_pattern(&assignment.left)?,
            Pattern::Rest(rest) => self.hoist_var_pattern(&rest.argument)?,
        }
        Ok(())
    }
}

impl Compiler {
    pub(crate) fn push_loop_context(&mut self, continue_target: usize, label: Option<String>) {
        self.loop_stack.push(LoopContext {
            break_jumps: Vec::new(),
            continue_target,
            label,
            scope_depth: self.scope_depth,
        });
    }
}

impl Compiler {
    pub(crate) fn pop_loop_context_and_patch_breaks(&mut self) {
        if let Some(ctx) = self.loop_stack.pop() {
            for jump_idx in ctx.break_jumps {
                self.patch_jump_to_here(jump_idx);
            }
        }
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
