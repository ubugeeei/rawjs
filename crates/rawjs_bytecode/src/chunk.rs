#[allow(unused_imports)]
use crate::compiler::UpvalueDesc;
#[allow(unused_imports)]
use crate::opcode::Instruction;
#[allow(unused_imports)]
use std::fmt;

#[path = "chunk/chunk_to_patch_finally.rs"]
mod chunk_to_patch_finally;
#[allow(unused_imports)]
use self::chunk_to_patch_finally::*;
#[allow(unused_imports)]
pub use self::chunk_to_patch_finally::{Chunk, Constant};
#[path = "chunk/disassemble_to_tests.rs"]
mod disassemble_to_tests;
#[allow(unused_imports)]
use self::disassemble_to_tests::*;
