impl Chunk {
    #[doc = " Pretty-print the chunk for debugging / disassembly."]
    pub fn disassemble(&self) -> String {
        let mut out = String::new();
        let gen_tag = if self.is_generator { " generator" } else { "" };
        let async_tag = if self.is_async { " async" } else { "" };
        out.push_str(&format!(
            "=== {}{}{} (params={}, locals={}, upvalues={}) ===\n",
            self.name, async_tag, gen_tag, self.param_count, self.local_count, self.upvalue_count
        ));
        if !self.constants.is_empty() {
            out.push_str("Constants:\n");
            for (i, c) in self.constants.iter().enumerate() {
                out.push_str(&format!("  {:>4}: {}\n", i, c));
            }
        }
        out.push_str("Instructions:\n");
        for (i, instr) in self.instructions.iter().enumerate() {
            out.push_str(&format!("  {:>4}: {}\n", i, instr));
        }
        for c in &self.constants {
            if let Constant::Function(ref func_chunk) = c {
                out.push('\n');
                out.push_str(&func_chunk.disassemble());
            }
        }
        out
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.disassemble())
    }
}

impl Constant {
    #[doc = " Value equality check used to de-duplicate constants."]
    #[doc = " Function constants are never de-duplicated."]
    fn eq_value(&self, other: &Constant) -> bool {
        match (self, other) {
            (Constant::Number(a), Constant::Number(b)) => a.to_bits() == b.to_bits(),
            (Constant::String(a), Constant::String(b)) => a == b,
            (Constant::Boolean(a), Constant::Boolean(b)) => a == b,
            (Constant::Null, Constant::Null) => true,
            (Constant::Undefined, Constant::Undefined) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Number(n) => write!(f, "{}", n),
            Constant::String(s) => write!(f, "\"{}\"", s),
            Constant::Boolean(b) => write!(f, "{}", b),
            Constant::Null => write!(f, "null"),
            Constant::Undefined => write!(f, "undefined"),
            Constant::Function(chunk) => write!(f, "<function {}>", chunk.name),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn new_chunk_is_empty() {
        let c = Chunk::new("<test>");
        assert!(c.instructions.is_empty());
        assert!(c.constants.is_empty());
        assert_eq!(c.name, "<test>");
        assert_eq!(c.param_count, 0);
        assert_eq!(c.local_count, 0);
        assert_eq!(c.upvalue_count, 0);
    }
    #[test]
    fn emit_returns_index() {
        let mut c = Chunk::new("test");
        let i0 = c.emit(Instruction::Null);
        let i1 = c.emit(Instruction::Pop);
        assert_eq!(i0, 0);
        assert_eq!(i1, 1);
        assert_eq!(c.instructions.len(), 2);
    }
    #[test]
    fn add_constant_deduplicates_numbers() {
        let mut c = Chunk::new("test");
        let a = c.add_constant(Constant::Number(2.5)).unwrap();
        let b = c.add_constant(Constant::Number(2.5)).unwrap();
        assert_eq!(a, b);
        assert_eq!(c.constants.len(), 1);
    }
    #[test]
    fn add_constant_deduplicates_strings() {
        let mut c = Chunk::new("test");
        let a = c.add_constant(Constant::String("hello".into())).unwrap();
        let b = c.add_constant(Constant::String("hello".into())).unwrap();
        assert_eq!(a, b);
        assert_eq!(c.constants.len(), 1);
    }
    #[test]
    fn add_constant_does_not_dedup_functions() {
        let mut c = Chunk::new("test");
        let f1 = Chunk::new("f1");
        let f2 = Chunk::new("f2");
        let a = c.add_constant(Constant::Function(Box::new(f1))).unwrap();
        let b = c.add_constant(Constant::Function(Box::new(f2))).unwrap();
        assert_ne!(a, b);
        assert_eq!(c.constants.len(), 2);
    }
    #[test]
    fn patch_jump_works() {
        let mut c = Chunk::new("test");
        let idx = c.emit(Instruction::JumpIfFalse(0));
        c.emit(Instruction::Pop);
        c.emit(Instruction::Pop);
        let target = c.current_offset() as i32 - idx as i32 - 1;
        c.patch_jump(idx, target);
        assert_eq!(c.instructions[idx], Instruction::JumpIfFalse(target));
    }
    #[test]
    fn patch_finally_works() {
        let mut c = Chunk::new("test");
        let idx = c.emit(Instruction::EnterTry(0, 0));
        c.emit(Instruction::Pop);
        c.patch_finally(idx, 42);
        assert_eq!(c.instructions[idx], Instruction::EnterTry(0, 42));
    }
    #[test]
    fn disassemble_output() {
        let mut c = Chunk::new("<script>");
        c.add_constant(Constant::Number(42.0));
        c.emit(Instruction::LoadConst(0));
        c.emit(Instruction::Return);
        let output = c.disassemble();
        assert!(output.contains("<script>"));
        assert!(output.contains("42"));
        assert!(output.contains("LOAD_CONST"));
        assert!(output.contains("RETURN"));
    }
    #[test]
    fn constant_display() {
        assert_eq!(format!("{}", Constant::Number(1.5)), "1.5");
        assert_eq!(format!("{}", Constant::String("hi".into())), "\"hi\"");
        assert_eq!(format!("{}", Constant::Boolean(true)), "true");
        assert_eq!(format!("{}", Constant::Null), "null");
        assert_eq!(format!("{}", Constant::Undefined), "undefined");
    }
}
