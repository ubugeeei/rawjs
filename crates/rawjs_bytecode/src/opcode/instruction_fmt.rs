impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::LoadConst(idx) => write!(f, "LOAD_CONST {}", idx),
            Instruction::LoadLocal(idx) => write!(f, "LOAD_LOCAL {}", idx),
            Instruction::StoreLocal(idx) => write!(f, "STORE_LOCAL {}", idx),
            Instruction::LoadGlobal(idx) => write!(f, "LOAD_GLOBAL {}", idx),
            Instruction::LoadGlobalOrUndefined(idx) => {
                write!(f, "LOAD_GLOBAL_OR_UNDEFINED {}", idx)
            }
            Instruction::StoreGlobal(idx) => write!(f, "STORE_GLOBAL {}", idx),
            Instruction::InitGlobal(idx) => write!(f, "INIT_GLOBAL {}", idx),
            Instruction::DeleteName(idx) => write!(f, "DELETE_NAME {}", idx),
            Instruction::LoadUpvalue(idx) => write!(f, "LOAD_UPVALUE {}", idx),
            Instruction::StoreUpvalue(idx) => write!(f, "STORE_UPVALUE {}", idx),
            Instruction::Call(argc) => write!(f, "CALL {}", argc),
            Instruction::New(argc) => write!(f, "NEW {}", argc),
            Instruction::CallMethod(argc) => write!(f, "CALL_METHOD {}", argc),
            Instruction::CreateClosure(idx) => write!(f, "CREATE_CLOSURE {}", idx),
            Instruction::CreateArray(cnt) => write!(f, "CREATE_ARRAY {}", cnt),
            Instruction::GetProperty(idx) => write!(f, "GET_PROPERTY {}", idx),
            Instruction::SetProperty(idx) => write!(f, "SET_PROPERTY {}", idx),
            Instruction::Jump(off) => write!(f, "JUMP {}", off),
            Instruction::JumpIfFalse(off) => write!(f, "JUMP_IF_FALSE {}", off),
            Instruction::JumpIfTrue(off) => write!(f, "JUMP_IF_TRUE {}", off),
            Instruction::JumpIfNullish(off) => write!(f, "JUMP_IF_NULLISH {}", off),
            Instruction::EnterTry(catch_off, finally_off) => {
                write!(f, "ENTER_TRY catch={} finally={}", catch_off, finally_off)
            }
            Instruction::IteratorDone(off) => write!(f, "ITERATOR_DONE {}", off),
            Instruction::ForInNext(off) => write!(f, "FOR_IN_NEXT {}", off),
            Instruction::ImportModule(idx) => write!(f, "IMPORT_MODULE {}", idx),
            Instruction::ImportModuleDynamic => write!(f, "IMPORT_MODULE_DYNAMIC"),
            Instruction::ImportMeta => write!(f, "IMPORT_META"),
            Instruction::ImportBinding(idx) => write!(f, "IMPORT_BINDING {}", idx),
            Instruction::ExportBinding(idx) => write!(f, "EXPORT_BINDING {}", idx),
            Instruction::DisposeResource(slot) => write!(f, "DISPOSE_RESOURCE {}", slot),
            Instruction::AsyncDisposeResource(slot) => {
                write!(f, "ASYNC_DISPOSE_RESOURCE {}", slot)
            }
            other => write!(f, "{}", other.name()),
        }
    }
}

#[allow(unused_imports)]
use super::*;
