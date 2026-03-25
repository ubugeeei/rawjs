impl Compiler {
    pub(super) fn compile_unary(&mut self, unary: &UnaryExpression) -> Result<()> {
        if unary.operator == UnaryOp::Delete {
            match unary.argument.as_ref() {
                Expression::Member(member) => {
                    if member.optional {
                        self.compile_expression(&member.object)?;
                        let nullish_jump = self.emit_optional_jump();
                        if member.computed {
                            self.compile_expression(&member.property)?;
                            self.emit(Instruction::Delete);
                        } else {
                            let name = expression_to_property_name(&member.property)?;
                            let idx = self.add_string_constant(&name)?;
                            self.emit(Instruction::LoadConst(idx));
                            self.emit(Instruction::Delete);
                        }
                        let end_jump = self.emit_jump(Instruction::Jump);
                        self.patch_jump_to_here(nullish_jump);
                        self.emit(Instruction::Pop);
                        self.emit(Instruction::True);
                        self.patch_jump_to_here(end_jump);
                        return Ok(());
                    }
                    self.compile_expression(&member.object)?;
                    if member.computed {
                        self.compile_expression(&member.property)?;
                    } else {
                        let name = expression_to_property_name(&member.property)?;
                        let idx = self.add_string_constant(&name)?;
                        self.emit(Instruction::LoadConst(idx));
                    }
                    self.emit(Instruction::DeleteProperty);
                    return Ok(());
                }
                Expression::Identifier(id) => {
                    if self.resolve_local(&id.name).is_some()
                        || (self.in_function && id.name == "arguments")
                        || self.resolve_upvalue(&id.name).is_some()
                    {
                        self.emit(Instruction::False);
                    } else {
                        let idx = self.add_string_constant(&id.name)?;
                        self.emit(Instruction::DeleteName(idx));
                    }
                    return Ok(());
                }
                _ => {
                    self.compile_expression(&unary.argument)?;
                    self.emit(Instruction::Pop);
                    self.emit(Instruction::True);
                    return Ok(());
                }
            }
        }
        if unary.operator == UnaryOp::Typeof {
            if let Expression::Identifier(id) = unary.argument.as_ref() {
                if self.resolve_local(&id.name).is_some()
                    || (self.in_function && id.name == "arguments")
                    || self.resolve_upvalue(&id.name).is_some()
                {
                    self.compile_identifier_load(&id.name)?;
                } else {
                    let idx = self.add_string_constant(&id.name)?;
                    self.emit(Instruction::LoadGlobalOrUndefined(idx));
                }
                self.emit(Instruction::TypeOf);
                return Ok(());
            }
        }
        self.compile_expression(&unary.argument)?;
        let instr = match unary.operator {
            UnaryOp::Minus => Instruction::Neg,
            UnaryOp::Plus => Instruction::Pos,
            UnaryOp::Not => Instruction::Not,
            UnaryOp::BitNot => Instruction::BitNot,
            UnaryOp::Typeof => Instruction::TypeOf,
            UnaryOp::Void => Instruction::Void,
            UnaryOp::Delete => unreachable!(),
        };
        self.emit(instr);
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_update(&mut self, update: &UpdateExpression) -> Result<()> {
        match update.argument.as_ref() {
            Expression::Identifier(id) => {
                if self.is_strict && matches!(id.name.as_str(), "arguments" | "eval") {
                    return Err(RawJsError::syntax_error(
                        "Invalid update target in strict mode",
                        None,
                    ));
                }
                if update.prefix {
                    self.compile_identifier_load(&id.name)?;
                    self.emit(Instruction::Pos);
                    let one = self.add_number_constant(1.0)?;
                    self.emit(Instruction::LoadConst(one));
                    self.emit(match update.operator {
                        UpdateOp::Increment => Instruction::Add,
                        UpdateOp::Decrement => Instruction::Sub,
                    });
                    self.emit(Instruction::Dup);
                    self.compile_identifier_store(&id.name)?;
                } else {
                    self.compile_identifier_load(&id.name)?;
                    self.emit(match update.operator {
                        UpdateOp::Increment => Instruction::PostfixIncrement,
                        UpdateOp::Decrement => Instruction::PostfixDecrement,
                    });
                    self.compile_identifier_store(&id.name)?;
                }
            }
            Expression::Member(member) => {
                if member.optional {
                    return Err(RawJsError::syntax_error(
                        "Invalid left-hand side expression in postfix operation",
                        None,
                    ));
                }
                let obj_slot = self.declare_temp_local("update_obj")?;
                let new_slot = self.declare_temp_local("update_new")?;
                let old_slot = if update.prefix {
                    None
                } else {
                    Some(self.declare_temp_local("update_old")?)
                };
                self.compile_expression(&member.object)?;
                self.emit(Instruction::StoreLocal(obj_slot));
                let one = self.add_number_constant(1.0)?;
                let op_instr = match update.operator {
                    UpdateOp::Increment => Instruction::Add,
                    UpdateOp::Decrement => Instruction::Sub,
                };
                if member.computed {
                    let key_slot = self.declare_temp_local("update_key")?;
                    self.compile_expression(&member.property)?;
                    self.emit(Instruction::StoreLocal(key_slot));
                    self.emit(Instruction::LoadLocal(obj_slot));
                    self.emit(Instruction::LoadLocal(key_slot));
                    self.emit(Instruction::GetComputed);
                    if let Some(old_slot) = old_slot {
                        self.emit(match update.operator {
                            UpdateOp::Increment => Instruction::PostfixIncrement,
                            UpdateOp::Decrement => Instruction::PostfixDecrement,
                        });
                        self.emit(Instruction::StoreLocal(new_slot));
                        self.emit(Instruction::StoreLocal(old_slot));
                    } else {
                        self.emit(Instruction::Pos);
                        self.emit(Instruction::LoadConst(one));
                        self.emit(op_instr);
                        self.emit(Instruction::StoreLocal(new_slot));
                    }
                    self.emit(Instruction::LoadLocal(obj_slot));
                    self.emit(Instruction::LoadLocal(key_slot));
                    self.emit(Instruction::LoadLocal(new_slot));
                    self.emit(Instruction::SetComputed);
                } else {
                    let name = expression_to_property_name(&member.property)?;
                    let idx = self.add_string_constant(&name)?;
                    self.emit(Instruction::LoadLocal(obj_slot));
                    self.emit(Instruction::GetProperty(idx));
                    if let Some(old_slot) = old_slot {
                        self.emit(match update.operator {
                            UpdateOp::Increment => Instruction::PostfixIncrement,
                            UpdateOp::Decrement => Instruction::PostfixDecrement,
                        });
                        self.emit(Instruction::StoreLocal(new_slot));
                        self.emit(Instruction::StoreLocal(old_slot));
                    } else {
                        self.emit(Instruction::Pos);
                        self.emit(Instruction::LoadConst(one));
                        self.emit(op_instr);
                        self.emit(Instruction::StoreLocal(new_slot));
                    }
                    self.emit(Instruction::LoadLocal(obj_slot));
                    self.emit(Instruction::LoadLocal(new_slot));
                    self.emit(Instruction::SetProperty(idx));
                }
                if let Some(old_slot) = old_slot {
                    self.emit(Instruction::Pop);
                    self.emit(Instruction::LoadLocal(old_slot));
                }
            }
            _ => {
                return Err(RawJsError::syntax_error(
                    "Invalid left-hand side in update expression",
                    None,
                ));
            }
        }
        Ok(())
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
