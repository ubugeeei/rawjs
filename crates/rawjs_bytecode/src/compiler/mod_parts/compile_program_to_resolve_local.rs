impl Compiler {
    #[doc = " Compile a parsed program into a top-level bytecode chunk."]
    pub fn compile_program(program: &Program) -> Result<Chunk> {
        let mut compiler = Compiler::new("<script>");
        compiler.is_async = true;
        compiler.chunk.is_async = true;
        compiler.is_strict = has_use_strict_directive(&program.body);
        compiler.chunk.is_strict = compiler.is_strict;
        compiler.hoist_var_declarations(&program.body)?;
        for stmt in &program.body {
            if let Statement::ImportDeclaration(import_decl) = stmt {
                compiler.compile_statement(&Statement::ImportDeclaration(import_decl.clone()))?;
            }
        }
        for stmt in &program.body {
            match stmt {
                Statement::FunctionDeclaration(func) => {
                    compiler.hoist_function_declaration(func)?;
                }
                Statement::ExportDeclaration(export_decl) => {
                    if let ExportKind::Declaration(inner) = &export_decl.kind {
                        if let Statement::FunctionDeclaration(func) = inner.as_ref() {
                            compiler.hoist_function_declaration(func)?;
                        }
                    }
                }
                _ => {}
            }
        }
        for stmt in &program.body {
            if matches!(
                stmt,
                Statement::FunctionDeclaration(_) | Statement::ImportDeclaration(_)
            ) {
                continue;
            }
            compiler.compile_statement(stmt)?;
        }
        compiler.chunk.emit(Instruction::Undefined);
        compiler.chunk.emit(Instruction::Return);
        compiler.chunk.local_count = compiler.max_locals as u16;
        compiler.chunk.upvalue_count = compiler.upvalues.len() as u16;
        Ok(compiler.chunk)
    }
}

impl Compiler {
    pub(crate) fn new(name: &str) -> Self {
        Compiler {
            chunk: Chunk::new(name),
            locals: Vec::new(),
            upvalues: Vec::new(),
            upvalue_names: Vec::new(),
            scope_depth: 0,
            loop_stack: Vec::new(),
            max_locals: 0,
            parent_locals: Vec::new(),
            parent_upvalues: Vec::new(),
            is_generator: false,
            is_async: false,
            is_strict: false,
            in_function: false,
            dispose_scopes: Vec::new(),
        }
    }
}

impl Compiler {
    pub(crate) fn begin_scope(&mut self) {
        self.scope_depth += 1;
        self.dispose_scopes.push(Vec::new());
    }
}

impl Compiler {
    pub(crate) fn end_scope(&mut self) {
        if let Some(dispose_slots) = self.dispose_scopes.pop() {
            for &(slot, is_await) in dispose_slots.iter().rev() {
                if is_await {
                    self.chunk.emit(Instruction::AsyncDisposeResource(slot));
                    continue;
                }
                self.chunk.emit(Instruction::DisposeResource(slot));
            }
        }
        self.pop_locals_to_depth(self.scope_depth);
        self.scope_depth -= 1;
    }
}

impl Compiler {
    #[doc = " Remove locals whose depth >= `min_depth` from the compiler's tracking."]
    #[doc = " Since the VM uses a separate locals vector (not the operand stack),"]
    #[doc = " we do NOT emit Pop instructions here."]
    pub(crate) fn pop_locals_to_depth(&mut self, min_depth: u32) {
        while let Some(local) = self.locals.last() {
            if local.depth < min_depth {
                break;
            }
            self.locals.pop();
        }
    }
}

impl Compiler {
    #[doc = " Declare a local in the current scope, push `Undefined` as placeholder,"]
    #[doc = " and return its slot index."]
    pub(crate) fn declare_local(&mut self, name: &str) -> Result<u16> {
        self.declare_local_with_storage(name, LocalStorage::Local)
    }
}

impl Compiler {
    pub(crate) fn declare_global_alias_local(&mut self, name: &str) -> Result<u16> {
        self.declare_local_with_storage(name, LocalStorage::GlobalAlias)
    }
}

impl Compiler {
    pub(crate) fn declare_temp_local(&mut self, prefix: &str) -> Result<u16> {
        let name = format!("#{}{}", prefix, self.locals.len());
        self.declare_local(&name)
    }
}

impl Compiler {
    fn declare_local_with_storage(&mut self, name: &str, storage: LocalStorage) -> Result<u16> {
        let index = self.locals.len();
        if index > u16::MAX as usize {
            return Err(RawJsError::internal_error("Too many local variables"));
        }
        self.locals.push(Local {
            name: name.to_string(),
            depth: self.scope_depth,
            storage,
            is_captured: false,
        });
        if self.locals.len() > self.max_locals {
            self.max_locals = self.locals.len();
        }
        Ok(index as u16)
    }
}

impl Compiler {
    #[doc = " Resolve a name to a local slot index."]
    pub(crate) fn resolve_local(&self, name: &str) -> Option<u16> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                return Some(i as u16);
            }
        }
        None
    }
}
