impl Compiler {
    pub(super) fn compile_import(&mut self, import_decl: &ImportDeclaration) -> Result<()> {
        let source_idx = self.add_string_constant(&import_decl.source)?;
        self.emit(Instruction::ImportModule(source_idx));
        for spec in &import_decl.specifiers {
            match spec {
                ImportSpecifier::Default { local, .. } => {
                    self.emit(Instruction::Dup);
                    let binding_idx = self.add_string_constant("default")?;
                    self.emit(Instruction::ImportBinding(binding_idx));
                    let slot = self.declare_local(local)?;
                    self.emit(Instruction::StoreLocal(slot));
                }
                ImportSpecifier::Named {
                    imported, local, ..
                } => {
                    self.emit(Instruction::Dup);
                    let binding_idx = self.add_string_constant(imported)?;
                    self.emit(Instruction::ImportBinding(binding_idx));
                    let slot = self.declare_local(local)?;
                    self.emit(Instruction::StoreLocal(slot));
                }
                ImportSpecifier::Namespace { local, .. } => {
                    self.emit(Instruction::Dup);
                    let slot = self.declare_local(local)?;
                    self.emit(Instruction::StoreLocal(slot));
                }
            }
        }
        self.emit(Instruction::Pop);
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_export(&mut self, export_decl: &ExportDeclaration) -> Result<()> {
        match &export_decl.kind {
            ExportKind::Default(expr) => {
                self.compile_expression(expr)?;
                self.emit(Instruction::ExportDefault);
            }
            ExportKind::Named(specifiers) => {
                for spec in specifiers {
                    self.compile_expression(&Expression::Identifier(IdentifierExpression {
                        name: spec.local.clone(),
                        location: spec.location,
                    }))?;
                    let name_idx = self.add_string_constant(&spec.exported)?;
                    self.emit(Instruction::ExportBinding(name_idx));
                }
            }
            ExportKind::Declaration(stmt) => match stmt.as_ref() {
                Statement::FunctionDeclaration(func) => {
                    if let Some(ref name) = func.id {
                        self.compile_expression(&Expression::Identifier(IdentifierExpression {
                            name: name.clone(),
                            location: func.location,
                        }))?;
                        let name_idx = self.add_string_constant(name)?;
                        self.emit(Instruction::ExportBinding(name_idx));
                    }
                }
                Statement::VariableDeclaration(vd) => {
                    self.compile_statement(stmt)?;
                    for decl in &vd.declarations {
                        if let Pattern::Identifier(id) = &decl.id {
                            self.compile_expression(&Expression::Identifier(
                                IdentifierExpression {
                                    name: id.name.clone(),
                                    location: id.location,
                                },
                            ))?;
                            let name_idx = self.add_string_constant(&id.name)?;
                            self.emit(Instruction::ExportBinding(name_idx));
                        }
                    }
                }
                Statement::ClassDeclaration(cls) => {
                    self.compile_statement(stmt)?;
                    if let Some(ref name) = cls.id {
                        self.compile_expression(&Expression::Identifier(IdentifierExpression {
                            name: name.clone(),
                            location: cls.location,
                        }))?;
                        let name_idx = self.add_string_constant(name)?;
                        self.emit(Instruction::ExportBinding(name_idx));
                    }
                }
                _ => {}
            },
            ExportKind::AllFrom(source) => {
                let source_idx = self.add_string_constant(source)?;
                self.emit(Instruction::ImportModule(source_idx));
                self.emit(Instruction::Pop);
            }
            ExportKind::NamedFrom(specifiers, source) => {
                let source_idx = self.add_string_constant(source)?;
                self.emit(Instruction::ImportModule(source_idx));
                for spec in specifiers {
                    self.emit(Instruction::Dup);
                    let binding_idx = self.add_string_constant(&spec.local)?;
                    self.emit(Instruction::ImportBinding(binding_idx));
                    let name_idx = self.add_string_constant(&spec.exported)?;
                    self.emit(Instruction::ExportBinding(name_idx));
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
