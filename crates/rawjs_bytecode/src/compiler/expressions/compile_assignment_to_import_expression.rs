impl Compiler {
    pub(super) fn compile_assignment(&mut self, assign: &AssignmentExpression) -> Result<()> {
        match assign.left.as_ref() {
            Expression::Identifier(id) => {
                if self.is_strict && matches!(id.name.as_str(), "arguments" | "eval") {
                    return Err(RawJsError::syntax_error(
                        "Invalid assignment target in strict mode",
                        None,
                    ));
                }
                if Self::is_logical_assignment(assign.operator) {
                    return self.compile_logical_identifier_assignment(
                        &id.name,
                        assign.operator,
                        &assign.right,
                    );
                }
                if assign.operator == AssignmentOp::Assign {
                    self.compile_expression(&assign.right)?;
                } else {
                    self.compile_identifier_load(&id.name)?;
                    self.compile_expression(&assign.right)?;
                    self.emit_compound_assign_op(assign.operator)?;
                }
                self.emit(Instruction::Dup);
                self.compile_identifier_store(&id.name)?;
            }
            Expression::Member(member) => {
                if member.optional {
                    return Err(RawJsError::syntax_error(
                        "Invalid left-hand side in assignment",
                        None,
                    ));
                }
                if Self::is_logical_assignment(assign.operator) {
                    return self.compile_logical_member_assignment(
                        member,
                        assign.operator,
                        &assign.right,
                    );
                }
                self.compile_expression(&member.object)?;
                if member.computed {
                    self.compile_expression(&member.property)?;
                    if assign.operator == AssignmentOp::Assign {
                        self.compile_expression(&assign.right)?;
                        self.emit(Instruction::SetComputed);
                    } else {
                        self.emit(Instruction::Dup);
                        self.emit(Instruction::GetComputed);
                        self.compile_expression(&assign.right)?;
                        self.emit_compound_assign_op(assign.operator)?;
                        self.emit(Instruction::SetComputed);
                    }
                } else {
                    let name = expression_to_property_name(&member.property)?;
                    let idx = self.add_string_constant(&name)?;
                    if assign.operator == AssignmentOp::Assign {
                        self.compile_expression(&assign.right)?;
                        self.emit(Instruction::SetProperty(idx));
                    } else {
                        self.emit(Instruction::Dup);
                        self.emit(Instruction::GetProperty(idx));
                        self.compile_expression(&assign.right)?;
                        self.emit_compound_assign_op(assign.operator)?;
                        self.emit(Instruction::SetProperty(idx));
                    }
                }
            }
            _ => {
                return Err(RawJsError::syntax_error(
                    "Invalid left-hand side in assignment",
                    None,
                ));
            }
        }
        Ok(())
    }
}

impl Compiler {
    pub(super) fn emit_compound_assign_op(&mut self, op: AssignmentOp) -> Result<()> {
        let instr = match op {
            AssignmentOp::AddAssign => Instruction::Add,
            AssignmentOp::SubAssign => Instruction::Sub,
            AssignmentOp::MulAssign => Instruction::Mul,
            AssignmentOp::DivAssign => Instruction::Div,
            AssignmentOp::ModAssign => Instruction::Mod,
            AssignmentOp::ExpAssign => Instruction::Exp,
            AssignmentOp::BitAndAssign => Instruction::BitAnd,
            AssignmentOp::BitOrAssign => Instruction::BitOr,
            AssignmentOp::BitXorAssign => Instruction::BitXor,
            AssignmentOp::ShlAssign => Instruction::Shl,
            AssignmentOp::ShrAssign => Instruction::Shr,
            AssignmentOp::UShrAssign => Instruction::UShr,
            AssignmentOp::AndAssign
            | AssignmentOp::OrAssign
            | AssignmentOp::NullishCoalescingAssign => {
                return Err(RawJsError::internal_error(
                    "Logical assignment operators require special handling",
                ));
            }
            AssignmentOp::Assign => unreachable!(),
        };
        self.emit(instr);
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_conditional(&mut self, cond: &ConditionalExpression) -> Result<()> {
        self.compile_expression(&cond.test)?;
        let else_jump = self.emit_jump(Instruction::JumpIfFalse);
        self.compile_expression(&cond.consequent)?;
        let end_jump = self.emit_jump(Instruction::Jump);
        self.patch_jump_to_here(else_jump);
        self.compile_expression(&cond.alternate)?;
        self.patch_jump_to_here(end_jump);
        Ok(())
    }
}

impl Compiler {
    pub(super) fn compile_import_expression(
        &mut self,
        import_expr: &ImportExpression,
    ) -> Result<()> {
        self.compile_expression(&import_expr.source)?;
        self.emit(Instruction::ImportModuleDynamic);
        Ok(())
    }
}

use super::*;
