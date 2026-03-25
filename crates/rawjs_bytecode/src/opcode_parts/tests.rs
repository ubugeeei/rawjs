#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn instruction_display() {
        assert_eq!(format!("{}", Instruction::LoadConst(42)), "LOAD_CONST 42");
        assert_eq!(format!("{}", Instruction::Add), "ADD");
        assert_eq!(format!("{}", Instruction::Jump(-5)), "JUMP -5");
        assert_eq!(
            format!("{}", Instruction::JumpIfNullish(3)),
            "JUMP_IF_NULLISH 3"
        );
        assert_eq!(
            format!("{}", Instruction::EnterTry(3, 7)),
            "ENTER_TRY catch=3 finally=7"
        );
    }
    #[test]
    fn instruction_name() {
        assert_eq!(Instruction::Pop.name(), "POP");
        assert_eq!(Instruction::StoreLocal(0).name(), "STORE_LOCAL");
        assert_eq!(Instruction::CreateClosure(1).name(), "CREATE_CLOSURE");
        assert_eq!(Instruction::JumpIfNullish(0).name(), "JUMP_IF_NULLISH");
    }
    #[test]
    fn instruction_clone_and_eq() {
        let a = Instruction::LoadConst(10);
        let b = a;
        assert_eq!(a, b);
    }
}
