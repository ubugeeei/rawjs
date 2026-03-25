use crate::chunk::Chunk;

/// A local variable tracked by the compiler.
#[derive(Debug, Clone)]
pub(crate) struct Local {
    pub(crate) name: String,
    pub(crate) depth: u32,
    pub(crate) storage: LocalStorage,
    /// Set to `true` when a nested function captures this local as an upvalue.
    /// Used by the VM to keep the value alive after the scope exits.
    #[allow(dead_code)]
    pub(crate) is_captured: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LocalStorage {
    Local,
    GlobalAlias,
}

/// Describes how an upvalue is captured.
#[derive(Debug, Clone, Copy)]
pub struct UpvalueDesc {
    /// Index in the *enclosing* scope's locals (if `is_local` is true)
    /// or in the enclosing scope's upvalue list (if `is_local` is false).
    pub index: u16,
    /// `true` if the upvalue captures a local in the immediately enclosing scope.
    pub is_local: bool,
}

/// Context saved for break/continue inside loops.
#[derive(Debug, Clone)]
pub(crate) struct LoopContext {
    /// Instruction indices of `Jump(0)` placeholders that need patching to the
    /// first instruction *after* the loop (for `break`).
    pub(crate) break_jumps: Vec<usize>,
    /// Instruction index of the loop header (target for `continue`).
    pub(crate) continue_target: usize,
    /// Optional label for labeled loops.
    pub(crate) label: Option<String>,
    /// Scope depth at the start of the loop body (so we can pop locals on break/continue).
    pub(crate) scope_depth: u32,
}

/// AST-to-bytecode compiler.
///
/// Each `Compiler` instance compiles exactly one lexical scope (top-level
/// script or function body).  Nested functions are compiled recursively by
/// creating a child `Compiler`.
/// Snapshot of a parent compiler's locals for upvalue resolution.
#[derive(Debug, Clone)]
pub(crate) struct ParentLocal {
    pub(crate) name: String,
    pub(crate) index: u16,
}

/// Snapshot of a parent compiler's upvalues for upvalue resolution.
#[derive(Debug, Clone)]
pub(crate) struct ParentUpvalue {
    pub(crate) name: String,
    pub(crate) index: u16,
}

pub struct Compiler {
    pub(crate) chunk: Chunk,
    pub(crate) locals: Vec<Local>,
    pub(crate) upvalues: Vec<UpvalueDesc>,
    /// Parallel to `upvalues` -- the variable name each upvalue captures.
    pub(crate) upvalue_names: Vec<String>,
    pub(crate) scope_depth: u32,
    pub(crate) loop_stack: Vec<LoopContext>,
    /// Track the maximum number of locals ever live at once.
    pub(crate) max_locals: usize,
    /// Parent compiler's locals snapshot (for upvalue resolution in nested functions).
    pub(crate) parent_locals: Vec<ParentLocal>,
    /// Parent compiler's upvalues snapshot (for chained upvalue resolution).
    pub(crate) parent_upvalues: Vec<ParentUpvalue>,
    /// Whether the current function is a generator.
    pub(crate) is_generator: bool,
    /// Whether the current function is async.
    pub(crate) is_async: bool,
    /// Whether the current scope is in strict mode.
    pub(crate) is_strict: bool,
    /// Whether the current scope is a function body.
    pub(crate) in_function: bool,
    /// Stack of dispose-tracking scopes. Each entry is a list of local slots
    /// that hold `using` resources and need to be disposed when the scope exits.
    /// Outer Vec = scope stack, inner Vec = (slot, is_await) pairs.
    pub(crate) dispose_scopes: Vec<Vec<(u16, bool)>>,
    /// When true, top-level REPL declarations become persistent bindings so
    /// later inputs can resolve them.
    pub(crate) persistent_top_level_bindings: bool,
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
