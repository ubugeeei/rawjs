impl Vm {
    /// Execute a module file and return its namespace (exports) object.
    ///
    /// Resolves the path relative to `current_file_dir`, reads the file,
    /// parses, compiles, and executes it.  Results are cached so each
    /// module is only executed once.
    pub fn execute_module(&mut self, specifier: &str) -> Result<GcPtr<JsObject>> {
        let resolved = self.resolve_module_path(specifier)?;
        if let Some(cached) = self.module_cache.get(&resolved) {
            return Ok(cached.clone());
        }
        let source = std::fs::read_to_string(&resolved).map_err(|e| {
            RawJsError::type_error(format!("Cannot find module '{}': {}", specifier, e))
        })?;
        let program = rawjs_parser::parse(&source).map_err(|e| {
            RawJsError::syntax_error(format!("Error parsing module '{}': {}", specifier, e), None)
        })?;
        let chunk = rawjs_bytecode::compile(&program).map_err(|e| {
            RawJsError::internal_error(format!("Error compiling module '{}': {}", specifier, e))
        })?;
        let exports = self.heap.alloc(JsObject::ordinary());
        let prev_file_dir = self.current_file_dir.take();
        let prev_file_path = self.current_file_path.take();
        let prev_exports = self.module_exports.take();
        let module_dir = Path::new(&resolved)
            .parent()
            .map(|p| p.to_string_lossy().to_string());
        self.current_file_dir = module_dir;
        self.current_file_path = Some(resolved.clone());
        self.module_exports = Some(exports.clone());
        let chunk_index = self.add_chunk(chunk);
        let execution_result = if self.chunks[chunk_index].is_async {
            interpreter::execute_async_top_level(self, chunk_index).map(|_| ())
        } else {
            let local_count = self.chunks[chunk_index].local_count as usize;
            let frame = CallFrame {
                chunk_index,
                ip: 0,
                base: self.value_stack.len(),
                locals: vec![JsValue::Undefined; local_count],
                arguments: Vec::new(),
                arguments_object: None,
                callee: None,
                is_strict: self.chunks[chunk_index].is_strict,
                upvalues: Vec::new(),
                this_value: JsValue::Undefined,
            };
            self.call_stack.push(frame);
            interpreter::run_module_frame(self)
        };
        let final_exports = self.module_exports.take().unwrap_or(exports);
        self.current_file_dir = prev_file_dir;
        self.current_file_path = prev_file_path;
        self.module_exports = prev_exports;
        execution_result?;
        self.module_cache.insert(resolved, final_exports.clone());
        Ok(final_exports)
    }
}

impl Vm {
    /// Resolve a module specifier to an absolute file path.
    pub(super) fn resolve_module_path(&self, specifier: &str) -> Result<String> {
        if !specifier.starts_with("./") && !specifier.starts_with("../") {
            return Err(RawJsError::type_error(format!(
                "Bare module specifier '{}' is not supported. Use relative paths (./...)",
                specifier
            )));
        }
        let base_dir = self.current_file_dir.as_deref().unwrap_or(".");
        let mut path = PathBuf::from(base_dir);
        path.push(specifier);
        if path.extension().is_none() {
            path.set_extension("js");
        }
        let resolved = path.canonicalize().unwrap_or_else(|_| path.clone());
        Ok(resolved.to_string_lossy().to_string())
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

use super::*;
