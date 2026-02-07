pub mod chunk;
pub mod compiler;
pub mod opcode;

pub use chunk::{Chunk, Constant};
pub use compiler::{Compiler, UpvalueDesc};
pub use opcode::Instruction;

/// Convenience function to compile a program AST into a bytecode chunk.
pub fn compile(program: &rawjs_ast::Program) -> rawjs_common::Result<Chunk> {
    Compiler::compile_program(program)
}
