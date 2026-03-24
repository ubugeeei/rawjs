use super::super::stubs;
use super::{Instruction, JitCompiler};

impl JitCompiler {
    pub(crate) fn emit_value_instruction(&mut self, instruction: Instruction) -> bool {
        match instruction {
            Instruction::LoadConst(idx) => {
                self.emit_stub_call_1(stubs::stub_load_const, idx as u32)
            }
            Instruction::LoadLocal(idx) => {
                self.emit_stub_call_1(stubs::stub_load_local, idx as u32)
            }
            Instruction::StoreLocal(idx) => {
                self.emit_stub_call_1(stubs::stub_store_local, idx as u32)
            }
            Instruction::LoadGlobal(idx) => {
                self.emit_stub_call_1(stubs::stub_load_global, idx as u32)
            }
            Instruction::StoreGlobal(idx) => {
                self.emit_stub_call_1(stubs::stub_store_global, idx as u32)
            }
            Instruction::LoadUpvalue(idx) => {
                self.emit_stub_call_1(stubs::stub_load_upvalue, idx as u32)
            }
            Instruction::StoreUpvalue(idx) => {
                self.emit_stub_call_1(stubs::stub_store_upvalue, idx as u32)
            }
            Instruction::Pop => self.emit_stub_call_0(stubs::stub_pop),
            Instruction::Dup => self.emit_stub_call_0(stubs::stub_dup),
            Instruction::Undefined => self.emit_stub_call_0(stubs::stub_push_undefined),
            Instruction::Null => self.emit_stub_call_0(stubs::stub_push_null),
            Instruction::True => self.emit_stub_call_0(stubs::stub_push_true),
            Instruction::False => self.emit_stub_call_0(stubs::stub_push_false),
            Instruction::This => self.emit_stub_call_0(stubs::stub_push_this),
            Instruction::Add => self.emit_stub_call_0(stubs::stub_add),
            Instruction::Sub => self.emit_stub_call_0(stubs::stub_sub),
            Instruction::Mul => self.emit_stub_call_0(stubs::stub_mul),
            Instruction::Div => self.emit_stub_call_0(stubs::stub_div),
            Instruction::Mod => self.emit_stub_call_0(stubs::stub_modulo),
            Instruction::Exp => self.emit_stub_call_0(stubs::stub_exp),
            Instruction::Neg => self.emit_stub_call_0(stubs::stub_neg),
            Instruction::Pos => self.emit_stub_call_0(stubs::stub_pos),
            Instruction::Not => self.emit_stub_call_0(stubs::stub_not),
            Instruction::BitNot => self.emit_stub_call_0(stubs::stub_bit_not),
            Instruction::TypeOf => self.emit_stub_call_0(stubs::stub_typeof),
            Instruction::Void => self.emit_stub_call_0(stubs::stub_void),
            Instruction::Delete => self.emit_stub_call_0(stubs::stub_delete),
            Instruction::Eq => self.emit_stub_call_0(stubs::stub_eq),
            Instruction::StrictEq => self.emit_stub_call_0(stubs::stub_strict_eq),
            Instruction::Ne => self.emit_stub_call_0(stubs::stub_ne),
            Instruction::StrictNe => self.emit_stub_call_0(stubs::stub_strict_ne),
            Instruction::Lt => self.emit_stub_call_0(stubs::stub_lt),
            Instruction::Le => self.emit_stub_call_0(stubs::stub_le),
            Instruction::Gt => self.emit_stub_call_0(stubs::stub_gt),
            Instruction::Ge => self.emit_stub_call_0(stubs::stub_ge),
            Instruction::BitAnd => self.emit_stub_call_0(stubs::stub_bit_and),
            Instruction::BitOr => self.emit_stub_call_0(stubs::stub_bit_or),
            Instruction::BitXor => self.emit_stub_call_0(stubs::stub_bit_xor),
            Instruction::Shl => self.emit_stub_call_0(stubs::stub_shl),
            Instruction::Shr => self.emit_stub_call_0(stubs::stub_shr),
            Instruction::UShr => self.emit_stub_call_0(stubs::stub_ushr),
            Instruction::In => self.emit_stub_call_0(stubs::stub_in),
            Instruction::Instanceof => self.emit_stub_call_0(stubs::stub_instanceof),
            Instruction::PostfixIncrement => self.emit_stub_call_0(stubs::stub_postfix_inc),
            Instruction::PostfixDecrement => self.emit_stub_call_0(stubs::stub_postfix_dec),
            Instruction::CreateObject => self.emit_stub_call_0(stubs::stub_create_object),
            Instruction::CreateArray(count) => {
                self.emit_stub_call_1(stubs::stub_create_array, count as u32)
            }
            Instruction::GetProperty(idx) => {
                self.emit_stub_call_1(stubs::stub_get_property, idx as u32)
            }
            Instruction::SetProperty(idx) => {
                self.emit_stub_call_1(stubs::stub_set_property, idx as u32)
            }
            Instruction::GetIndex => self.emit_stub_call_0(stubs::stub_get_index),
            Instruction::SetIndex => self.emit_stub_call_0(stubs::stub_set_index),
            Instruction::GetComputed => self.emit_stub_call_0(stubs::stub_get_computed),
            Instruction::SetComputed => self.emit_stub_call_0(stubs::stub_set_computed),
            Instruction::CreateClosure(idx) => {
                self.emit_stub_call_1(stubs::stub_create_closure, idx as u32)
            }
            Instruction::Call(argc) => self.emit_stub_call_1(stubs::stub_call, argc as u32),
            Instruction::CallMethod(argc) => {
                self.emit_stub_call_1(stubs::stub_call_method, argc as u32)
            }
            Instruction::Return => self.emit_stub_call_0_return(stubs::stub_return),
            _ => return false,
        }

        true
    }
}
