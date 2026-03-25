impl Compiler {
    fn compile_call(&mut self, call: &CallExpression) -> Result<()> {
        let is_method_call = matches!(&*call.callee, Expression::Member(_));
        if is_method_call {
            if let Expression::Member(member) = &*call.callee {
                return self.compile_method_call(call, member);
            }
        }
        self.compile_expression(&call.callee)?;
        let argc = call.arguments.len();
        if argc > u16::MAX as usize {
            return Err(RawJsError::internal_error("Too many function arguments"));
        }
        let nullish_jump = if call.optional {
            Some(self.emit_optional_jump())
        } else {
            None
        };
        for arg in &call.arguments {
            self.compile_expression(arg)?;
        }
        self.emit(Instruction::Call(argc as u16));
        if let Some(nullish_jump) = nullish_jump {
            let end_jump = self.emit_jump(Instruction::Jump);
            self.patch_jump_to_here(nullish_jump);
            self.emit_optional_undefined_result();
            self.patch_jump_to_here(end_jump);
        }
        Ok(())
    }
}

impl Compiler {
    fn compile_method_call(
        &mut self,
        call: &CallExpression,
        member: &MemberExpression,
    ) -> Result<()> {
        self.compile_expression(&member.object)?;
        let receiver_nullish_jump = if member.optional {
            Some(self.emit_optional_jump())
        } else {
            None
        };
        self.emit(Instruction::Dup);
        self.compile_member_access(member)?;
        let method_nullish_jump = if call.optional {
            Some(self.emit_optional_jump())
        } else {
            None
        };
        let argc = call.arguments.len();
        if argc > u16::MAX as usize {
            return Err(RawJsError::internal_error("Too many function arguments"));
        }
        for arg in &call.arguments {
            self.compile_expression(arg)?;
        }
        self.emit(Instruction::CallMethod(argc as u16));
        let end_jump = if receiver_nullish_jump.is_some() || method_nullish_jump.is_some() {
            Some(self.emit_jump(Instruction::Jump))
        } else {
            None
        };
        let receiver_end_jump = if let Some(receiver_nullish_jump) = receiver_nullish_jump {
            self.patch_jump_to_here(receiver_nullish_jump);
            self.emit_optional_undefined_result();
            if method_nullish_jump.is_some() {
                Some(self.emit_jump(Instruction::Jump))
            } else {
                None
            }
        } else {
            None
        };
        if let Some(method_nullish_jump) = method_nullish_jump {
            self.patch_jump_to_here(method_nullish_jump);
            self.emit_optional_method_undefined_result();
        }
        if let Some(end_jump) = end_jump {
            self.patch_jump_to_here(end_jump);
        }
        if let Some(receiver_end_jump) = receiver_end_jump {
            self.patch_jump_to_here(receiver_end_jump);
        }
        Ok(())
    }
}

impl Compiler {
    fn compile_new(&mut self, new_expr: &NewExpression) -> Result<()> {
        self.compile_expression(&new_expr.callee)?;
        let argc = new_expr.arguments.len();
        if argc > u16::MAX as usize {
            return Err(RawJsError::internal_error("Too many constructor arguments"));
        }
        for arg in &new_expr.arguments {
            self.compile_expression(arg)?;
        }
        self.emit(Instruction::New(argc as u16));
        Ok(())
    }
}

impl Compiler {
    fn compile_member_load(&mut self, member: &MemberExpression) -> Result<()> {
        self.compile_expression(&member.object)?;
        if member.optional {
            let nullish_jump = self.emit_optional_jump();
            self.compile_member_access(member)?;
            let end_jump = self.emit_jump(Instruction::Jump);
            self.patch_jump_to_here(nullish_jump);
            self.emit_optional_undefined_result();
            self.patch_jump_to_here(end_jump);
            return Ok(());
        }
        self.compile_member_access(member)
    }
}

impl Compiler {
    fn compile_sequence(&mut self, seq: &SequenceExpression) -> Result<()> {
        let len = seq.expressions.len();
        for (i, expr) in seq.expressions.iter().enumerate() {
            self.compile_expression(expr)?;
            if i < len - 1 {
                self.emit(Instruction::Pop);
            }
        }
        Ok(())
    }
}
