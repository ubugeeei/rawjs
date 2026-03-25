use rawjs_ast::*;

use rawjs_common::{RawJsError, Result};

use crate::opcode::Instruction;

use super::{Compiler, LocalStorage};

impl Compiler {
    pub(crate) fn compile_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Expression(es) => self.compile_expression_statement(es),
            Statement::VariableDeclaration(vd) => self.compile_variable_declaration(vd),
            Statement::FunctionDeclaration(_) => Ok(()),
            Statement::Return(ret) => self.compile_return(ret),
            Statement::If(if_stmt) => self.compile_if(if_stmt),
            Statement::While(while_stmt) => self.compile_while(while_stmt),
            Statement::DoWhile(do_while) => self.compile_do_while(do_while),
            Statement::For(for_stmt) => self.compile_for(for_stmt),
            Statement::ForIn(for_in) => self.compile_for_in(for_in),
            Statement::ForOf(for_of) => self.compile_for_of(for_of),
            Statement::Block(block) => self.compile_block(block),
            Statement::Break(brk) => self.compile_break(brk),
            Statement::Continue(cont) => self.compile_continue(cont),
            Statement::Switch(sw) => self.compile_switch(sw),
            Statement::Throw(th) => self.compile_throw(th),
            Statement::Try(try_stmt) => self.compile_try(try_stmt),
            Statement::Labeled(labeled) => self.compile_labeled(labeled),
            Statement::With(_) => Err(RawJsError::syntax_error(
                "'with' statements are not supported in strict mode",
                None,
            )),
            Statement::Empty | Statement::Debugger => Ok(()),
            Statement::ClassDeclaration(cls) => self.compile_class_declaration(cls),
            Statement::ImportDeclaration(import_decl) => self.compile_import(import_decl),
            Statement::ExportDeclaration(export_decl) => self.compile_export(export_decl),
        }
    }
}

impl Compiler {
    pub(super) fn compile_expression_statement(&mut self, es: &ExpressionStatement) -> Result<()> {
        self.compile_expression(&es.expression)?;
        self.emit(Instruction::Pop);
        Ok(())
    }
}

impl Compiler {
    pub(crate) fn compile_variable_declaration(&mut self, vd: &VariableDeclaration) -> Result<()> {
        let is_using = matches!(vd.kind, VarKind::Using | VarKind::AwaitUsing);
        let is_await_using = vd.kind == VarKind::AwaitUsing;
        let is_top_level_var =
            !self.in_function && self.scope_depth == 0 && vd.kind == VarKind::Var;
        if is_await_using && !self.is_async {
            return Err(RawJsError::syntax_error(
                "'await using' is only valid in async functions and modules",
                None,
            ));
        }
        for decl in &vd.declarations {
            match &decl.id {
                Pattern::Identifier(id) => {
                    let (slot, storage) = if vd.kind == VarKind::Var {
                        if let Some((slot, storage)) = self.resolve_local_storage(&id.name) {
                            (slot, storage)
                        } else if is_top_level_var {
                            (
                                self.declare_global_alias_local(&id.name)?,
                                LocalStorage::GlobalAlias,
                            )
                        } else {
                            (self.declare_local(&id.name)?, LocalStorage::Local)
                        }
                    } else if is_top_level_var {
                        (
                            self.declare_global_alias_local(&id.name)?,
                            LocalStorage::GlobalAlias,
                        )
                    } else {
                        (self.declare_local(&id.name)?, LocalStorage::Local)
                    };
                    if let Some(ref init) = decl.init {
                        self.compile_expression(init)?;
                    } else {
                        self.emit(Instruction::Undefined);
                    }
                    self.emit(Instruction::StoreLocal(slot));
                    if storage == LocalStorage::GlobalAlias {
                        self.emit(Instruction::LoadLocal(slot));
                        let idx = self.add_string_constant(&id.name)?;
                        self.emit(Instruction::InitGlobal(idx));
                    }
                    if is_using {
                        if let Some(scope) = self.dispose_scopes.last_mut() {
                            scope.push((slot, is_await_using));
                        }
                    }
                }
                _ => {
                    if let Some(ref init) = decl.init {
                        self.compile_expression(init)?;
                    } else {
                        self.emit(Instruction::Undefined);
                    }
                    self.compile_destructuring_declaration(&decl.id)?;
                }
            }
        }
        Ok(())
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
