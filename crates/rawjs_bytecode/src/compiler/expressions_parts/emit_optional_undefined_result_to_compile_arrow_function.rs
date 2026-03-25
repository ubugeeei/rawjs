impl Compiler {
    fn emit_optional_undefined_result(&mut self) {
        self.emit(Instruction::Pop);
        self.emit(Instruction::Undefined);
    }
}

impl Compiler {
    fn emit_optional_method_undefined_result(&mut self) {
        self.emit(Instruction::Pop);
        self.emit(Instruction::Pop);
        self.emit(Instruction::Undefined);
    }
}

impl Compiler {
    fn compile_member_access(&mut self, member: &MemberExpression) -> Result<()> {
        if member.computed {
            self.compile_expression(&member.property)?;
            self.emit(Instruction::GetComputed);
            return Ok(());
        }
        let name = expression_to_property_name(&member.property)?;
        let idx = self.add_string_constant(&name)?;
        self.emit(Instruction::GetProperty(idx));
        Ok(())
    }
}

impl Compiler {
    fn compile_array(&mut self, arr: &ArrayExpression) -> Result<()> {
        let count = arr.elements.len();
        for elem in &arr.elements {
            match elem {
                Some(e) => self.compile_expression(e)?,
                None => {
                    self.emit(Instruction::Undefined);
                }
            }
        }
        if count > u16::MAX as usize {
            return Err(RawJsError::internal_error("Array literal too large"));
        }
        self.emit(Instruction::CreateArray(count as u16));
        Ok(())
    }
}

impl Compiler {
    fn compile_object(&mut self, obj: &ObjectExpression) -> Result<()> {
        self.emit(Instruction::CreateObject);
        for prop in &obj.properties {
            self.emit(Instruction::Dup);
            if prop.computed {
                self.compile_expression(&prop.key)?;
                self.compile_expression(&prop.value)?;
                self.emit(Instruction::SetComputed);
                self.emit(Instruction::Pop);
            } else {
                self.compile_expression(&prop.value)?;
                let key_name = expression_to_property_name(&prop.key)?;
                let idx = self.add_string_constant(&key_name)?;
                self.emit(Instruction::SetProperty(idx));
                self.emit(Instruction::Pop);
            }
        }
        Ok(())
    }
}

impl Compiler {
    fn compile_function_expression(&mut self, func: &FunctionDeclaration) -> Result<()> {
        self.compile_function_body(func)
    }
}

impl Compiler {
    fn compile_arrow_function(&mut self, arrow: &ArrowFunctionExpression) -> Result<()> {
        let name = "<arrow>";
        let mut child = Compiler::new(name);
        child.chunk.param_count = arrow.params.len() as u16;
        child.in_function = true;
        child.is_strict = self.is_strict;
        child.chunk.is_strict = child.is_strict;
        child.is_async = arrow.is_async;
        child.chunk.is_async = arrow.is_async;
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
        for param in &arrow.params {
            let param_name = pattern_to_name(param);
            child.declare_local(&param_name)?;
        }
        match &arrow.body {
            ArrowFunctionBody::Expression(expr) => {
                child.compile_expression(expr)?;
                child.emit(Instruction::Return);
            }
            ArrowFunctionBody::Block(block) => {
                for stmt in &block.body {
                    child.compile_statement(stmt)?;
                }
                child.emit(Instruction::Undefined);
                child.emit(Instruction::Return);
            }
        }
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
