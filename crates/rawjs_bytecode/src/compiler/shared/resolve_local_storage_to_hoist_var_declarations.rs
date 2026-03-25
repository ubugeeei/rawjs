impl Compiler {
    pub(crate) fn resolve_local_storage(&self, name: &str) -> Option<(u16, LocalStorage)> {
        self.resolve_local(name)
            .map(|slot| (slot, self.locals[slot as usize].storage))
    }
}

impl Compiler {
    #[doc = " Resolve a name to an upvalue index by searching the parallel"]
    #[doc = " `upvalue_names` vector.  This only finds upvalues that were"]
    #[doc = " previously registered via `add_upvalue`."]
    pub(crate) fn resolve_upvalue(&mut self, name: &str) -> Option<u16> {
        for (i, uv_name) in self.upvalue_names.iter().enumerate() {
            if uv_name == name {
                return Some(i as u16);
            }
        }
        for pl in &self.parent_locals {
            if pl.name == name {
                let desc = UpvalueDesc {
                    index: pl.index,
                    is_local: true,
                };
                return Some(self.add_upvalue(name, desc));
            }
        }
        for pu in &self.parent_upvalues {
            if pu.name == name {
                let desc = UpvalueDesc {
                    index: pu.index,
                    is_local: false,
                };
                return Some(self.add_upvalue(name, desc));
            }
        }
        None
    }
}

impl Compiler {
    #[doc = " Register a new upvalue (or return the index of an existing identical one)."]
    #[doc = " Used when compiling closures that capture variables from enclosing scopes."]
    pub(crate) fn add_upvalue(&mut self, name: &str, desc: UpvalueDesc) -> u16 {
        for (i, existing) in self.upvalues.iter().enumerate() {
            if existing.index == desc.index && existing.is_local == desc.is_local {
                return i as u16;
            }
        }
        let idx = self.upvalues.len() as u16;
        self.upvalues.push(desc);
        self.upvalue_names.push(name.to_string());
        idx
    }
}

impl Compiler {
    pub(crate) fn add_number_constant(&mut self, value: f64) -> Result<u16> {
        self.chunk
            .add_constant(Constant::Number(value))
            .ok_or_else(|| RawJsError::internal_error("Constant pool overflow"))
    }
}

impl Compiler {
    pub(crate) fn add_string_constant(&mut self, value: &str) -> Result<u16> {
        self.chunk
            .add_constant(Constant::String(value.to_string()))
            .ok_or_else(|| RawJsError::internal_error("Constant pool overflow"))
    }
}

impl Compiler {
    pub(crate) fn emit(&mut self, instr: Instruction) -> usize {
        self.chunk.emit(instr)
    }
}

impl Compiler {
    pub(crate) fn emit_jump(&mut self, instr_fn: fn(i32) -> Instruction) -> usize {
        self.chunk.emit(instr_fn(0))
    }
}

impl Compiler {
    pub(crate) fn patch_jump_to_here(&mut self, index: usize) {
        let here = self.chunk.current_offset() as i32;
        let target = here - index as i32 - 1;
        self.chunk.patch_jump(index, target);
    }
}

impl Compiler {
    pub(crate) fn hoist_function_declaration(&mut self, func: &FunctionDeclaration) -> Result<()> {
        if let Some(ref name) = func.id {
            let slot = if !self.in_function && self.scope_depth == 0 {
                self.declare_global_alias_local(name)?
            } else {
                self.declare_local(name)?
            };
            self.emit(Instruction::Undefined);
            self.compile_function_body(func)?;
            self.emit(Instruction::StoreLocal(slot));
            if !self.in_function && self.scope_depth == 0 {
                self.emit(Instruction::LoadLocal(slot));
                let idx = self.add_string_constant(name)?;
                self.emit(Instruction::InitGlobal(idx));
            }
            self.emit(Instruction::Pop);
        }
        Ok(())
    }
}

impl Compiler {
    pub(crate) fn hoist_var_declarations(&mut self, statements: &[Statement]) -> Result<()> {
        for stmt in statements {
            self.hoist_var_declarations_in_statement(stmt)?;
        }
        Ok(())
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
