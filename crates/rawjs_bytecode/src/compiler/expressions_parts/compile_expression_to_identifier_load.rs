use rawjs_ast::*;

use rawjs_common::{RawJsError, Result};

use crate::chunk::Constant;

use crate::opcode::Instruction;

use super::{
    expression_to_property_name, pattern_to_name, Compiler, LocalStorage, ParentLocal,
    ParentUpvalue,
};

impl Compiler {
    pub(crate) fn compile_expression(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::NumberLiteral(n) => {
                let idx = self.add_number_constant(n.value)?;
                self.emit(Instruction::LoadConst(idx));
                Ok(())
            }
            Expression::StringLiteral(s) => {
                let idx = self.add_string_constant(&s.value)?;
                self.emit(Instruction::LoadConst(idx));
                Ok(())
            }
            Expression::BooleanLiteral(b) => {
                self.emit(if b.value {
                    Instruction::True
                } else {
                    Instruction::False
                });
                Ok(())
            }
            Expression::NullLiteral(_) => {
                self.emit(Instruction::Null);
                Ok(())
            }
            Expression::TemplateLiteral(tl) => {
                if tl.expressions.is_empty() {
                    let idx = self.add_string_constant(&tl.quasis[0])?;
                    self.emit(Instruction::LoadConst(idx));
                } else {
                    let first_idx = self.add_string_constant(&tl.quasis[0])?;
                    self.emit(Instruction::LoadConst(first_idx));
                    for (i, expr) in tl.expressions.iter().enumerate() {
                        self.compile_expression(expr)?;
                        self.emit(Instruction::Add);
                        let quasi_idx = self.add_string_constant(&tl.quasis[i + 1])?;
                        self.emit(Instruction::LoadConst(quasi_idx));
                        self.emit(Instruction::Add);
                    }
                }
                Ok(())
            }
            Expression::RegExpLiteral(re) => {
                let pattern_str = format!("/{}/{}", re.pattern, re.flags);
                let idx = self.add_string_constant(&pattern_str)?;
                self.emit(Instruction::LoadConst(idx));
                Ok(())
            }
            Expression::Identifier(id) => self.compile_identifier_load(&id.name),
            Expression::This(_) => {
                self.emit(Instruction::This);
                Ok(())
            }
            Expression::Super(_) => {
                let idx = self.add_string_constant("super")?;
                self.emit(Instruction::LoadGlobal(idx));
                Ok(())
            }
            Expression::ArrayExpression(arr) => self.compile_array(arr),
            Expression::ObjectExpression(obj) => self.compile_object(obj),
            Expression::FunctionExpression(func) => self.compile_function_expression(func),
            Expression::ArrowFunctionExpression(arrow) => self.compile_arrow_function(arrow),
            Expression::ClassExpression(cls) => self.compile_class_body(cls),
            Expression::Unary(unary) => self.compile_unary(unary),
            Expression::Update(update) => self.compile_update(update),
            Expression::Binary(bin) => self.compile_binary(bin),
            Expression::Logical(log) => self.compile_logical(log),
            Expression::Assignment(assign) => self.compile_assignment(assign),
            Expression::Conditional(cond) => self.compile_conditional(cond),
            Expression::Call(call) => self.compile_call(call),
            Expression::Import(import_expr) => self.compile_import_expression(import_expr),
            Expression::New(new_expr) => self.compile_new(new_expr),
            Expression::Member(member) => self.compile_member_load(member),
            Expression::Sequence(seq) => self.compile_sequence(seq),
            Expression::Spread(spread) => self.compile_expression(&spread.argument),
            Expression::ImportMeta(_) => {
                self.emit(Instruction::ImportMeta);
                Ok(())
            }
            Expression::Yield(y) => self.compile_yield(y),
            Expression::Await(a) => self.compile_await(a),
        }
    }
}

impl Compiler {
    pub(crate) fn compile_identifier_load(&mut self, name: &str) -> Result<()> {
        if let Some((slot, storage)) = self.resolve_local_storage(name) {
            match storage {
                LocalStorage::Local => {
                    self.emit(Instruction::LoadLocal(slot));
                }
                LocalStorage::GlobalAlias => {
                    let idx = self.add_string_constant(name)?;
                    self.emit(Instruction::LoadGlobal(idx));
                }
            }
        } else if self.in_function && name == "arguments" {
            self.emit(Instruction::LoadArguments);
        } else if let Some(uv) = self.resolve_upvalue(name) {
            self.emit(Instruction::LoadUpvalue(uv));
        } else {
            let idx = self.add_string_constant(name)?;
            self.emit(Instruction::LoadGlobal(idx));
        }
        Ok(())
    }
}
