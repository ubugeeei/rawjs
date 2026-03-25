use std::collections::{HashMap, HashSet};

use rawjs_bytecode::Chunk;

use rawjs_common::{RawJsError, Result};

use rawjs_runtime::{GcPtr, Heap, JsObject, JsValue, ObjectInternal, Property, Upvalue};

use std::path::{Path, PathBuf};

use crate::jit::JitFunction;

/// Number of times a function must be called before the JIT compiler attempts
/// to compile it to native code.
const JIT_THRESHOLD: u32 = 10;

/// A single activation record on the call stack.
#[derive(Debug)]
pub(crate) struct CallFrame {
    /// Index of the chunk being executed in `Vm::chunks`.
    pub chunk_index: usize,
    /// Instruction pointer -- index of the *next* instruction to execute.
    pub ip: usize,
    /// Base offset into `Vm::value_stack` for this frame.
    pub base: usize,
    /// Local variable slots (including parameters).
    pub locals: Vec<JsValue>,
    /// The full argument list for this call.
    pub arguments: Vec<JsValue>,
    /// Lazily-created `arguments` object for this frame.
    pub arguments_object: Option<GcPtr<JsObject>>,
    /// The function object being executed for this frame, when available.
    pub callee: Option<GcPtr<JsObject>>,
    /// Whether this frame executes strict-mode code.
    pub is_strict: bool,
    /// Captured upvalues inherited from the closure.
    pub upvalues: Vec<Upvalue>,
    /// The `this` binding for this call frame.
    pub this_value: JsValue,
}

/// Bookkeeping for a single try/catch/finally scope.
#[derive(Debug, Clone)]
pub(crate) struct TryContext {
    /// Instruction index of the catch handler, or `None`.
    pub catch_ip: Option<usize>,
    /// Instruction index of the finally handler, or `None`.
    pub finally_ip: Option<usize>,
    /// Value stack depth at the point the try was entered.
    pub stack_depth: usize,
    /// Call stack depth at the point the try was entered.
    pub call_depth: usize,
}

/// The rawjs virtual machine.
///
/// Owns the heap, all compiled chunks, and execution state (value stack,
/// call stack, try stack).  The VM can execute bytecode through its
/// interpreter and will automatically JIT-compile hot functions on
/// supported `aarch64`, `x86_64`, and `riscv64` targets.
pub struct Vm {
    /// The garbage-collected heap for object allocations.
    pub heap: Heap,
    /// Global variable bindings (e.g. `console`, `Math`, `undefined`, ...).
    globals: HashMap<String, JsValue>,
    /// The global object used for top-level `this`.
    pub(crate) global_object: Option<GcPtr<JsObject>>,
    /// All registered chunks (top-level scripts and function bodies).
    chunks: Vec<Chunk>,
    /// The call (activation) stack.
    pub(crate) call_stack: Vec<CallFrame>,
    /// The operand stack.
    pub(crate) value_stack: Vec<JsValue>,
    /// The exception-handling stack.
    pub(crate) try_stack: Vec<TryContext>,
    /// Constructor call targets keyed by call-stack depth.
    pub(crate) construct_frames: Vec<(usize, JsValue)>,
    /// Cache of JIT-compiled native functions, keyed by chunk index.
    jit_cache: HashMap<usize, JitFunction>,
    /// Per-chunk invocation counts used to decide when to JIT.
    execution_counts: HashMap<usize, u32>,
    /// The last thrown JS value (for catch handlers).
    pub(crate) thrown_value: Option<JsValue>,
    /// Error detail from the most recent JIT stub failure.
    pub(crate) jit_error: Option<RawJsError>,
    /// Built-in prototypes for automatic prototype chain setup.
    pub(crate) array_prototype: Option<GcPtr<JsObject>>,
    pub(crate) function_prototype: Option<GcPtr<JsObject>>,
    pub(crate) string_prototype: Option<GcPtr<JsObject>>,
    pub(crate) number_prototype: Option<GcPtr<JsObject>>,
    pub(crate) boolean_prototype: Option<GcPtr<JsObject>>,
    pub(crate) object_prototype: Option<GcPtr<JsObject>>,
    pub(crate) symbol_prototype: Option<GcPtr<JsObject>>,
    pub(crate) map_prototype: Option<GcPtr<JsObject>>,
    pub(crate) set_prototype: Option<GcPtr<JsObject>>,
    pub(crate) promise_prototype: Option<GcPtr<JsObject>>,
    pub(crate) generator_prototype: Option<GcPtr<JsObject>>,
    /// Cache of already-executed modules, keyed by canonical file path.
    module_cache: HashMap<String, GcPtr<JsObject>>,
    /// Directory of the currently executing file (for relative path resolution).
    pub current_file_dir: Option<String>,
    /// Canonical path of the currently executing module.
    pub current_file_path: Option<String>,
    /// The exports object for the currently executing module (if any).
    pub(crate) module_exports: Option<GcPtr<JsObject>>,
}
