/// Internal state for a Promise object.
#[derive(Debug, Clone)]
pub struct PromiseState {
    pub status: PromiseStatus,
    pub value: JsValue,
    pub fulfill_reactions: Vec<PromiseReaction>,
    pub reject_reactions: Vec<PromiseReaction>,
}

impl PromiseState {
    pub fn new() -> Self {
        PromiseState {
            status: PromiseStatus::Pending,
            value: JsValue::Undefined,
            fulfill_reactions: Vec::new(),
            reject_reactions: Vec::new(),
        }
    }
}

impl Default for PromiseState {
    fn default() -> Self {
        Self::new()
    }
}

/// State for a built-in iterator (used by for-of on arrays/strings).
#[derive(Debug, Clone)]
pub struct IteratorState {
    /// The values to iterate over.
    pub values: Vec<JsValue>,
    /// Current index into `values`.
    pub index: usize,
}

impl IteratorState {
    pub fn new(values: Vec<JsValue>) -> Self {
        IteratorState { values, index: 0 }
    }
}

impl IteratorState {
    #[doc = " Advance the iterator and return `(value, done)`."]
    pub fn advance(&mut self) -> (JsValue, bool) {
        if self.index >= self.values.len() {
            (JsValue::Undefined, true)
        } else {
            let val = self.values[self.index].clone();
            self.index += 1;
            (val, false)
        }
    }
}

/// The internal kind of a JavaScript object.
#[derive(Debug, Clone)]
pub enum ObjectInternal {
    /// A plain object with no special semantics.
    Ordinary,
    /// An arguments object with optional parameter-to-index mappings.
    ArgumentsObject(Vec<Option<u16>>),
    /// A boxed Boolean object created by `new Boolean(...)`.
    BooleanObject(bool),
    /// A function object. Holds the chunk index and captured upvalues.
    Function(FunctionObject),
    /// A boxed String object created by `new String(...)`.
    StringObject(String),
    /// An array object. Elements stored separately from named properties.
    Array(Vec<JsValue>),
    /// An error object with a message string.
    Error(String),
    /// An iterator object with a list of values and current index.
    Iterator(IteratorState),
    /// A Map object. Entries stored as `(key, value)` pairs in insertion order.
    Map(Vec<(JsValue, JsValue)>),
    /// A Set object. Values stored in insertion order.
    Set(Vec<JsValue>),
    /// A Promise object.
    Promise(PromiseState),
    /// A Generator object.
    Generator(GeneratorState),
}

/// The status of a Generator object.
#[derive(Debug, Clone, PartialEq)]
pub enum GeneratorStatus {
    /// Created but .next() has not been called yet.
    SuspendedStart,
    /// Suspended at a yield point.
    SuspendedYield,
    /// Currently executing (re-entrant guard).
    Executing,
    /// Completed (returned or threw).
    Completed,
}

/// Internal state for a Generator object.
///
/// Also used internally by async functions (`result_promise` is `Some`).
#[derive(Debug, Clone)]
pub struct GeneratorState {
    pub status: GeneratorStatus,
    pub chunk_index: usize,
    pub ip: usize,
    pub locals: Vec<JsValue>,
    pub arguments: Vec<JsValue>,
    pub upvalues: Vec<Upvalue>,
    pub saved_stack: Vec<JsValue>,
    pub this_value: JsValue,
    /// For async functions: the outer Promise that the caller awaits.
    pub result_promise: Option<GcPtr<JsObject>>,
    /// Saved try-stack entries (catch_ip, finally_ip, stack_depth, call_depth)
    /// when the generator/async function suspends at a yield/await point.
    pub saved_try_stack: Vec<(Option<usize>, Option<usize>, usize, usize)>,
}

/// Representation of a JavaScript function.
#[derive(Debug, Clone)]
pub struct FunctionObject {
    /// The kind of function (bytecode or native).
    pub kind: FunctionKind,
    /// The function's name (for display/stack traces).
    pub name: String,
    /// Captured upvalues (closed-over variables).
    pub upvalues: Vec<Upvalue>,
}

/// Whether a function is backed by compiled bytecode or a native Rust fn.
#[derive(Clone)]
pub enum FunctionKind {
    /// Bytecode function -- `chunk_index` points into the VM's chunk table.
    Bytecode { chunk_index: usize },
    /// A native (host) function.
    Native(NativeFn),
}

/// Signature for native functions.
///
/// Receives:
/// - `&mut dyn GcHeap` -- the heap, so the function can allocate objects
/// - `&JsValue` -- the `this` value
/// - `&[JsValue]` -- the arguments
///
/// Returns a `Result<JsValue>`.
pub type NativeFn = fn(&mut Heap, &JsValue, &[JsValue]) -> Result<JsValue>;

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
