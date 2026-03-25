use crate::compiler::UpvalueDesc;

use crate::opcode::Instruction;

/// A compiled bytecode chunk representing a single compilation unit
/// (top-level script, function body, etc.).
#[derive(Debug, Clone)]
pub struct Chunk {
    /// The bytecode instruction stream.
    pub instructions: Vec<Instruction>,
    /// The constant pool referenced by `LoadConst` indices.
    pub constants: Vec<Constant>,
    /// A human-readable name for debugging (e.g. function name or "<script>").
    pub name: String,
    /// Number of declared parameters (for function chunks).
    pub param_count: u16,
    /// Total number of local variable slots needed at runtime.
    pub local_count: u16,
    /// Total number of upvalue slots needed at runtime.
    pub upvalue_count: u16,
    /// Descriptors for each upvalue this chunk captures.
    pub upvalue_descriptors: Vec<UpvalueDesc>,
    /// Whether this chunk is a generator function body.
    pub is_generator: bool,
    /// Whether this chunk is an async function body.
    pub is_async: bool,
    /// Whether this chunk is in strict mode.
    pub is_strict: bool,
}

/// A compile-time constant value stored in a chunk's constant pool.
#[derive(Debug, Clone)]
pub enum Constant {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
    /// A nested function body compiled into its own chunk.
    Function(Box<Chunk>),
}

impl Chunk {
    #[doc = " Create a new empty chunk with the given name."]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
            name: name.into(),
            param_count: 0,
            local_count: 0,
            upvalue_count: 0,
            upvalue_descriptors: Vec::new(),
            is_generator: false,
            is_async: false,
            is_strict: false,
        }
    }
}

impl Chunk {
    #[doc = " Append an instruction and return its index in the instruction vector."]
    pub fn emit(&mut self, instruction: Instruction) -> usize {
        let idx = self.instructions.len();
        self.instructions.push(instruction);
        idx
    }
}

impl Chunk {
    #[doc = " Add a constant to the pool and return its index."]
    #[doc = " Returns `None` if the pool would exceed u16::MAX entries."]
    pub fn add_constant(&mut self, constant: Constant) -> Option<u16> {
        for (i, existing) in self.constants.iter().enumerate() {
            if constant.eq_value(existing) {
                return Some(i as u16);
            }
        }
        let idx = self.constants.len();
        if idx > u16::MAX as usize {
            return None;
        }
        self.constants.push(constant);
        Some(idx as u16)
    }
}

impl Chunk {
    #[doc = " Return the current instruction count (useful for computing jump offsets)."]
    pub fn current_offset(&self) -> usize {
        self.instructions.len()
    }
}

impl Chunk {
    #[doc = " Patch a jump instruction at `index` with the given signed `offset`."]
    pub fn patch_jump(&mut self, index: usize, offset: i32) {
        match &mut self.instructions[index] {
            Instruction::Jump(ref mut o) => *o = offset,
            Instruction::JumpIfFalse(ref mut o) => *o = offset,
            Instruction::JumpIfTrue(ref mut o) => *o = offset,
            Instruction::JumpIfNullish(ref mut o) => *o = offset,
            Instruction::EnterTry(ref mut catch_o, _) => *catch_o = offset,
            _ => panic!(
                "patch_jump called on non-jump instruction at index {}: {:?}",
                index, self.instructions[index]
            ),
        }
    }
}

impl Chunk {
    #[doc = " Patch the *finally* offset of an `EnterTry` instruction at `index`."]
    pub fn patch_finally(&mut self, index: usize, offset: i32) {
        match &mut self.instructions[index] {
            Instruction::EnterTry(_, ref mut finally_o) => *finally_o = offset,
            _ => panic!(
                "patch_finally called on non-EnterTry instruction at index {}: {:?}",
                index, self.instructions[index]
            ),
        }
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
