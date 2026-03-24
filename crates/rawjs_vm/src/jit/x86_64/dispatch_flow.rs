use super::super::stubs;
use super::{Instruction, JitCompiler};

impl JitCompiler {
    pub(crate) fn emit_flow_instruction(&mut self, instruction: Instruction, bc_index: usize) {
        match instruction {
            Instruction::Jump(offset) => self.emit_jump(bc_index, offset),
            Instruction::JumpIfFalse(offset) => {
                self.emit_truthy_branch(stubs::stub_pop_and_test_truthy, bc_index, offset, false)
            }
            Instruction::JumpIfTrue(offset) => {
                self.emit_truthy_branch(stubs::stub_pop_and_test_truthy, bc_index, offset, true)
            }
            Instruction::JumpIfNullish(offset) => {
                self.emit_truthy_branch(stubs::stub_pop_and_test_nullish, bc_index, offset, true)
            }
            Instruction::GetIterator => self.emit_stub_call_0(stubs::stub_get_iterator),
            Instruction::IteratorNext => self.emit_stub_call_0(stubs::stub_iterator_next),
            Instruction::IteratorDone(offset) => self.emit_iterator_done(bc_index, offset),
            Instruction::ForInInit => self.emit_stub_call_0(stubs::stub_for_in_init),
            Instruction::ForInNext(offset) => self.emit_for_in_next(bc_index, offset),
            Instruction::ImportModule(idx) => {
                self.emit_stub_call_1(stubs::stub_import_module, idx as u32)
            }
            Instruction::ImportModuleDynamic => {
                self.emit_stub_call_0(stubs::stub_import_module_dynamic)
            }
            Instruction::ImportMeta => self.emit_stub_call_0(stubs::stub_import_meta),
            Instruction::ImportBinding(idx) => {
                self.emit_stub_call_1(stubs::stub_import_binding, idx as u32)
            }
            Instruction::ExportBinding(idx) => {
                self.emit_stub_call_1(stubs::stub_export_binding, idx as u32)
            }
            Instruction::ExportDefault => self.emit_stub_call_0(stubs::stub_export_default),
            Instruction::Throw => self.emit_stub_call_0(stubs::stub_throw),
            Instruction::EnterTry(catch_offset, finally_offset) => {
                self.emit_stub_call_2i(stubs::stub_enter_try, catch_offset, finally_offset)
            }
            Instruction::LeaveTry => self.emit_stub_call_0(stubs::stub_leave_try),
            Instruction::CreateGenerator => self.emit_stub_call_0(stubs::stub_create_generator),
            Instruction::Yield => self.emit_stub_call_0_return(stubs::stub_yield),
            Instruction::Await => self.emit_stub_call_0_return(stubs::stub_await),
            Instruction::DisposeResource(slot) => {
                self.emit_stub_call_1(stubs::stub_dispose_resource, slot as u32)
            }
            Instruction::AsyncDisposeResource(slot) => {
                self.emit_stub_call_1(stubs::stub_async_dispose_resource, slot as u32)
            }
            _ => unreachable!("unexpected instruction in flow emitter"),
        }
    }

    fn emit_jump(&mut self, bc_index: usize, offset: i32) {
        let patch_offset = self.emit_jmp_placeholder();
        self.jump_patches
            .push((patch_offset, branch_target(bc_index, offset)));
    }

    fn emit_truthy_branch(
        &mut self,
        stub: extern "C" fn(*mut super::Vm) -> u32,
        bc_index: usize,
        offset: i32,
        jump_on_nonzero: bool,
    ) {
        self.emit_mov_rdi_rbx();
        self.emit_load_and_call(stub as *const () as usize);
        self.emit_test_eax_eax();
        let patch_offset = if jump_on_nonzero {
            self.emit_jne_placeholder()
        } else {
            self.emit_je_placeholder()
        };
        self.jump_patches
            .push((patch_offset, branch_target(bc_index, offset)));
    }

    fn emit_iterator_done(&mut self, bc_index: usize, offset: i32) {
        self.emit_mov_rdi_rbx();
        self.emit_mov_esi_imm32(offset as u32);
        self.emit_load_and_call(stubs::stub_iterator_done_check as *const () as usize);
        self.emit_test_eax_imm32(0x8000_0000);
        let error_patch = self.emit_js_placeholder();
        self.jump_patches
            .push((error_patch, super::ERROR_EXIT_SENTINEL));
        self.emit_cmp_eax_imm8(1);
        let patch_offset = self.emit_je_placeholder();
        self.jump_patches
            .push((patch_offset, branch_target(bc_index, offset)));
    }

    fn emit_for_in_next(&mut self, bc_index: usize, offset: i32) {
        self.emit_mov_rdi_rbx();
        self.emit_mov_esi_imm32(offset as u32);
        self.emit_load_and_call(stubs::stub_for_in_next as *const () as usize);
        self.emit_test_eax_imm32(0x8000_0000);
        let error_patch = self.emit_js_placeholder();
        self.jump_patches
            .push((error_patch, super::ERROR_EXIT_SENTINEL));
        self.emit_cmp_eax_imm8(1);
        let patch_offset = self.emit_je_placeholder();
        self.jump_patches
            .push((patch_offset, branch_target(bc_index, offset)));
    }
}

fn branch_target(bc_index: usize, offset: i32) -> usize {
    ((bc_index as i64) + 1 + offset as i64) as usize
}
