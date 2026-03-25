impl Compiler {
    pub(crate) fn compile_identifier_store(&mut self, name: &str) -> Result<()> {
        if let Some((slot, storage)) = self.resolve_local_storage(name) {
            match storage {
                LocalStorage::Local => {
                    self.emit(Instruction::StoreLocal(slot));
                }
                LocalStorage::GlobalAlias => {
                    let idx = self.add_string_constant(name)?;
                    self.emit(Instruction::StoreGlobal(idx));
                }
            }
        } else if let Some(uv) = self.resolve_upvalue(name) {
            self.emit(Instruction::StoreUpvalue(uv));
        } else {
            let idx = self.add_string_constant(name)?;
            self.emit(Instruction::StoreGlobal(idx));
        }
        Ok(())
    }
}

impl Compiler {
    pub(super) fn is_logical_assignment(op: AssignmentOp) -> bool {
        matches!(
            op,
            AssignmentOp::AndAssign
                | AssignmentOp::OrAssign
                | AssignmentOp::NullishCoalescingAssign
        )
    }
}

impl Compiler {
    pub(super) fn compile_logical_assignment_with<F>(
        &mut self,
        op: AssignmentOp,
        mut compile_assign: F,
    ) -> Result<()>
    where
        F: FnMut(&mut Self) -> Result<()>,
    {
        self.emit(Instruction::Dup);
        match op {
            AssignmentOp::AndAssign => {
                let skip_jump = self.emit_jump(Instruction::JumpIfFalse);
                self.emit(Instruction::Pop);
                compile_assign(self)?;
                self.patch_jump_to_here(skip_jump);
            }
            AssignmentOp::OrAssign => {
                let skip_jump = self.emit_jump(Instruction::JumpIfTrue);
                self.emit(Instruction::Pop);
                compile_assign(self)?;
                self.patch_jump_to_here(skip_jump);
            }
            AssignmentOp::NullishCoalescingAssign => {
                let assign_jump = self.emit_jump(Instruction::JumpIfNullish);
                let end_jump = self.emit_jump(Instruction::Jump);
                self.patch_jump_to_here(assign_jump);
                self.emit(Instruction::Pop);
                compile_assign(self)?;
                self.patch_jump_to_here(end_jump);
            }
            _ => unreachable!(),
        }
        Ok(())
    }
}

impl Compiler {
    pub(super) fn load_member_from_temp(
        &mut self,
        object_slot: u16,
        key_slot: Option<u16>,
        property_idx: Option<u16>,
    ) {
        self.emit(Instruction::LoadLocal(object_slot));
        if let Some(key_slot) = key_slot {
            self.emit(Instruction::LoadLocal(key_slot));
            self.emit(Instruction::GetComputed);
            return;
        }
        self.emit(Instruction::GetProperty(property_idx.unwrap()));
    }
}

impl Compiler {
    pub(super) fn store_member_from_temp(
        &mut self,
        object_slot: u16,
        key_slot: Option<u16>,
        property_idx: Option<u16>,
        value_slot: u16,
    ) {
        self.emit(Instruction::LoadLocal(object_slot));
        if let Some(key_slot) = key_slot {
            self.emit(Instruction::LoadLocal(key_slot));
        }
        self.emit(Instruction::LoadLocal(value_slot));
        if key_slot.is_some() {
            self.emit(Instruction::SetComputed);
            return;
        }
        self.emit(Instruction::SetProperty(property_idx.unwrap()));
    }
}

impl Compiler {
    pub(super) fn compile_logical_identifier_assignment(
        &mut self,
        name: &str,
        op: AssignmentOp,
        right: &Expression,
    ) -> Result<()> {
        self.compile_identifier_load(name)?;
        self.compile_logical_assignment_with(op, |compiler| {
            compiler.compile_expression(right)?;
            compiler.emit(Instruction::Dup);
            compiler.compile_identifier_store(name)
        })
    }
}

impl Compiler {
    pub(super) fn compile_logical_member_assignment(
        &mut self,
        member: &MemberExpression,
        op: AssignmentOp,
        right: &Expression,
    ) -> Result<()> {
        let property_idx = if member.computed {
            None
        } else {
            let name = expression_to_property_name(&member.property)?;
            Some(self.add_string_constant(&name)?)
        };
        self.begin_scope();
        let object_slot = self.declare_temp_local("logical_obj")?;
        let key_slot = if member.computed {
            Some(self.declare_temp_local("logical_key")?)
        } else {
            None
        };
        let value_slot = self.declare_temp_local("logical_value")?;
        self.compile_expression(&member.object)?;
        self.emit(Instruction::StoreLocal(object_slot));
        if let Some(key_slot) = key_slot {
            self.compile_expression(&member.property)?;
            self.emit(Instruction::StoreLocal(key_slot));
        }
        self.load_member_from_temp(object_slot, key_slot, property_idx);
        self.compile_logical_assignment_with(op, |compiler| {
            compiler.compile_expression(right)?;
            compiler.emit(Instruction::StoreLocal(value_slot));
            compiler.store_member_from_temp(object_slot, key_slot, property_idx, value_slot);
            Ok(())
        })?;
        self.end_scope();
        Ok(())
    }
}

impl Compiler {
    pub(super) fn emit_optional_jump(&mut self) -> usize {
        self.emit(Instruction::Dup);
        self.emit_jump(Instruction::JumpIfNullish)
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
