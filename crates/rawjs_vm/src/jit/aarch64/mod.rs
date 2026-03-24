use crate::Vm;
use rawjs_bytecode::{Chunk, Instruction};

mod compiler;
mod dispatch;
mod dispatch_flow;
mod dispatch_value;
mod encoding;
mod function;
mod memory;
mod patching;
mod reg;
mod stub_calls;

pub(crate) const ERROR_EXIT_SENTINEL: usize = usize::MAX;
pub(crate) const FRAME_DONE_SENTINEL: usize = usize::MAX - 1;

pub struct JitCompiler {
    pub(crate) code: Vec<u8>,
    pub(crate) pc_map: Vec<usize>,
    pub(crate) jump_patches: Vec<(usize, usize)>,
}

pub use function::JitFunction;
