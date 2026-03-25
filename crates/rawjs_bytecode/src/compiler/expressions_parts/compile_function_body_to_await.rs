impl Compiler {
    pub(crate) fn compile_function_body(&mut self, func: &FunctionDeclaration) -> Result<()> {
        let name = func.id.as_deref().unwrap_or("<anonymous>");
        let mut child = Compiler::new(name);
        child.chunk.param_count = func.params.len() as u16;
        child.in_function = true;
        child.is_strict = self.is_strict || super::has_use_strict_directive(&func.body.body);
        child.chunk.is_strict = child.is_strict;
        child.is_generator = func.is_generator;
        child.is_async = func.is_async;
        child.chunk.is_generator = func.is_generator;
        child.chunk.is_async = func.is_async;
        child.parent_locals = self
            .locals
            .iter()
            .enumerate()
            .filter(|(_, local)| local.storage == LocalStorage::Local)
            .map(|(i, l)| ParentLocal {
                name: l.name.clone(),
                index: i as u16,
            })
            .collect();
        child.parent_upvalues = self
            .upvalue_names
            .iter()
            .enumerate()
            .map(|(i, name)| ParentUpvalue {
                name: name.clone(),
                index: i as u16,
            })
            .collect();
        for param in &func.params {
            let param_name = pattern_to_name(param);
            child.declare_local(&param_name)?;
        }
        child.hoist_var_declarations(&func.body.body)?;
        for stmt in &func.body.body {
            if let Statement::FunctionDeclaration(inner_func) = stmt {
                child.hoist_function_declaration(inner_func)?;
            }
        }
        for stmt in &func.body.body {
            if matches!(stmt, Statement::FunctionDeclaration(_)) {
                continue;
            }
            child.compile_statement(stmt)?;
        }
        child.emit(Instruction::Undefined);
        child.emit(Instruction::Return);
        child.chunk.local_count = child.max_locals as u16;
        child.chunk.upvalue_count = child.upvalues.len() as u16;
        child.chunk.upvalue_descriptors = child.upvalues.clone();
        let func_const_idx = self
            .chunk
            .add_constant(Constant::Function(Box::new(child.chunk)))
            .ok_or_else(|| RawJsError::internal_error("Constant pool overflow"))?;
        self.emit(Instruction::CreateClosure(func_const_idx));
        Ok(())
    }
}

impl Compiler {
    fn compile_yield(&mut self, yield_expr: &YieldExpression) -> Result<()> {
        if !self.is_generator {
            return Err(RawJsError::syntax_error(
                "yield is only valid in generator functions",
                None,
            ));
        }
        if let Some(ref arg) = yield_expr.argument {
            self.compile_expression(arg)?;
        } else {
            self.emit(Instruction::Undefined);
        }
        self.emit(Instruction::Yield);
        Ok(())
    }
}

impl Compiler {
    fn compile_await(&mut self, await_expr: &AwaitExpression) -> Result<()> {
        if !self.is_async {
            return Err(RawJsError::syntax_error(
                "await is only valid in async functions",
                None,
            ));
        }
        self.compile_expression(&await_expr.argument)?;
        self.emit(Instruction::Await);
        Ok(())
    }
}
