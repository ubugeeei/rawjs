impl Compiler {
    fn compile_switch(&mut self, sw: &SwitchStatement) -> Result<()> {
        self.compile_expression(&sw.discriminant)?;
        let switch_start = self.chunk.current_offset();
        self.push_loop_context(switch_start, None);
        let mut case_jump_targets: Vec<usize> = Vec::new();
        let mut default_jump: Option<usize> = None;
        for case in &sw.cases {
            if let Some(ref test) = case.test {
                self.emit(Instruction::Dup);
                self.compile_expression(test)?;
                self.emit(Instruction::StrictEq);
                let jump_idx = self.emit_jump(Instruction::JumpIfTrue);
                case_jump_targets.push(jump_idx);
            } else {
                let jump_idx = self.emit_jump(Instruction::Jump);
                default_jump = Some(jump_idx);
                case_jump_targets.push(0);
            }
        }
        let end_no_match = self.emit_jump(Instruction::Jump);
        for (i, case) in sw.cases.iter().enumerate() {
            if case.test.is_some() {
                self.patch_jump_to_here(case_jump_targets[i]);
            } else if let Some(dj) = default_jump {
                self.patch_jump_to_here(dj);
            }
            for stmt in &case.consequent {
                self.compile_statement(stmt)?;
            }
        }
        self.patch_jump_to_here(end_no_match);
        self.emit(Instruction::Pop);
        self.pop_loop_context_and_patch_breaks();
        Ok(())
    }
}

impl Compiler {
    fn compile_throw(&mut self, th: &ThrowStatement) -> Result<()> {
        self.compile_expression(&th.argument)?;
        self.emit(Instruction::Throw);
        Ok(())
    }
}

impl Compiler {
    fn compile_try(&mut self, try_stmt: &TryStatement) -> Result<()> {
        let enter_try_idx = self.emit(Instruction::EnterTry(0, 0));
        self.begin_scope();
        for stmt in &try_stmt.block.body {
            self.compile_statement(stmt)?;
        }
        self.end_scope();
        self.emit(Instruction::LeaveTry);
        let jump_over_catch = self.emit_jump(Instruction::Jump);
        if let Some(ref handler) = try_stmt.handler {
            let catch_offset = self.chunk.current_offset() as i32 - enter_try_idx as i32 - 1;
            self.chunk.patch_jump(enter_try_idx, catch_offset);
            self.begin_scope();
            if let Some(ref param) = handler.param {
                match param {
                    Pattern::Identifier(id) => {
                        let slot = self.declare_local(&id.name)?;
                        self.emit(Instruction::StoreLocal(slot));
                    }
                    _ => {
                        self.compile_destructuring_declaration(param)?;
                    }
                }
            } else {
                self.emit(Instruction::Pop);
            }
            for stmt in &handler.body.body {
                self.compile_statement(stmt)?;
            }
            self.end_scope();
        }
        self.patch_jump_to_here(jump_over_catch);
        if let Some(ref finalizer) = try_stmt.finalizer {
            let finally_offset = self.chunk.current_offset() as i32 - enter_try_idx as i32 - 1;
            self.chunk.patch_finally(enter_try_idx, finally_offset);
            self.begin_scope();
            for stmt in &finalizer.body {
                self.compile_statement(stmt)?;
            }
            self.end_scope();
        }
        Ok(())
    }
}
