//! The rawjs virtual machine.
//!
//! This crate contains:
//! - A bytecode **interpreter** that executes `Chunk` programs from `rawjs_bytecode`.
//! - An aarch64 **JIT compiler** (macOS Apple Silicon) that compiles hot functions
//!   to native machine code.

pub mod interpreter;
pub mod jit;

use std::collections::HashMap;

use rawjs_bytecode::Chunk;
use rawjs_common::{RawJsError, Result};
use rawjs_runtime::{GcPtr, Heap, JsObject, JsValue, Upvalue};

use std::path::{Path, PathBuf};

use crate::jit::JitFunction;

// ---------------------------------------------------------------------------
// JIT hot-call threshold
// ---------------------------------------------------------------------------

/// Number of times a function must be called before the JIT compiler attempts
/// to compile it to native code.
const JIT_THRESHOLD: u32 = 10;

// ---------------------------------------------------------------------------
// Call frame
// ---------------------------------------------------------------------------

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
    /// Captured upvalues inherited from the closure.
    pub upvalues: Vec<Upvalue>,
    /// The `this` binding for this call frame.
    pub this_value: JsValue,
}

// ---------------------------------------------------------------------------
// Try context
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// VM
// ---------------------------------------------------------------------------

/// The rawjs virtual machine.
///
/// Owns the heap, all compiled chunks, and execution state (value stack,
/// call stack, try stack).  The VM can execute bytecode through its
/// interpreter and will automatically JIT-compile hot functions on
/// aarch64-apple-darwin.
pub struct Vm {
    /// The garbage-collected heap for object allocations.
    pub heap: Heap,

    /// Global variable bindings (e.g. `console`, `Math`, `undefined`, ...).
    globals: HashMap<String, JsValue>,

    /// All registered chunks (top-level scripts and function bodies).
    chunks: Vec<Chunk>,

    /// The call (activation) stack.
    pub(crate) call_stack: Vec<CallFrame>,

    /// The operand stack.
    pub(crate) value_stack: Vec<JsValue>,

    /// The exception-handling stack.
    pub(crate) try_stack: Vec<TryContext>,

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
    pub(crate) string_prototype: Option<GcPtr<JsObject>>,
    pub(crate) number_prototype: Option<GcPtr<JsObject>>,
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
    /// The exports object for the currently executing module (if any).
    pub(crate) module_exports: Option<GcPtr<JsObject>>,
}

impl Vm {
    // -----------------------------------------------------------------
    // Construction
    // -----------------------------------------------------------------

    /// Create a new VM with built-in globals pre-populated.
    pub fn new() -> Self {
        let mut vm = Vm {
            heap: Heap::new(),
            globals: HashMap::new(),
            chunks: Vec::new(),
            call_stack: Vec::new(),
            value_stack: Vec::new(),
            try_stack: Vec::new(),
            jit_cache: HashMap::new(),
            execution_counts: HashMap::new(),
            thrown_value: None,
            jit_error: None,
            array_prototype: None,
            string_prototype: None,
            number_prototype: None,
            object_prototype: None,
            symbol_prototype: None,
            map_prototype: None,
            set_prototype: None,
            promise_prototype: None,
            generator_prototype: None,
            module_cache: HashMap::new(),
            current_file_dir: None,
            module_exports: None,
        };
        vm.init_globals();
        vm
    }

    /// Register built-in global bindings.
    fn init_globals(&mut self) {
        use rawjs_runtime::builtins;

        // Primitive globals
        self.globals
            .insert("undefined".to_string(), JsValue::Undefined);
        self.globals
            .insert("NaN".to_string(), JsValue::Number(f64::NAN));
        self.globals
            .insert("Infinity".to_string(), JsValue::Number(f64::INFINITY));

        // console object
        let console_ptr = builtins::create_console_object(&mut self.heap);
        self.globals
            .insert("console".to_string(), JsValue::Object(console_ptr));

        // Math object
        let math_ptr = builtins::create_math_object(&mut self.heap);
        self.globals
            .insert("Math".to_string(), JsValue::Object(math_ptr));

        // Object.prototype and Object constructor
        let obj_proto = builtins::create_object_prototype(&mut self.heap);
        self.object_prototype = Some(obj_proto.clone());
        let object_ctor = builtins::create_object_constructor(&mut self.heap);
        self.globals
            .insert("Object".to_string(), JsValue::Object(object_ctor));

        // Array constructor + prototype
        let array_proto = builtins::create_array_prototype(&mut self.heap);
        self.array_prototype = Some(array_proto);
        let array_ctor = self.heap.alloc(JsObject::ordinary());
        self.globals
            .insert("Array".to_string(), JsValue::Object(array_ctor));

        // String.prototype
        let string_proto = builtins::create_string_prototype(&mut self.heap);
        self.string_prototype = Some(string_proto);

        // Number.prototype
        let number_proto = builtins::create_number_prototype(&mut self.heap);
        self.number_prototype = Some(number_proto);

        // Symbol constructor (a callable function with static properties)
        let mut symbol_fn = JsObject::native_function("Symbol", builtins::symbol_call);
        // Add static methods and well-known symbols
        {
            let symbol_obj = builtins::create_symbol_constructor(&mut self.heap);
            let borrowed = symbol_obj.borrow();
            for (key, prop) in borrowed.properties.iter() {
                symbol_fn.define_property(key.clone(), prop.clone());
            }
        }
        let symbol_ptr = self.heap.alloc(symbol_fn);
        self.globals
            .insert("Symbol".to_string(), JsValue::Object(symbol_ptr));

        // Symbol.prototype
        let symbol_proto = builtins::create_symbol_prototype(&mut self.heap);
        self.symbol_prototype = Some(symbol_proto);

        // Map constructor + prototype
        let map_proto = builtins::create_map_prototype(&mut self.heap);
        self.map_prototype = Some(map_proto);
        let map_ctor = self
            .heap
            .alloc(JsObject::native_function("Map", builtins::map_constructor));
        self.globals
            .insert("Map".to_string(), JsValue::Object(map_ctor));

        // Set constructor + prototype
        let set_proto = builtins::create_set_prototype(&mut self.heap);
        self.set_prototype = Some(set_proto);
        let set_ctor = self
            .heap
            .alloc(JsObject::native_function("Set", builtins::set_constructor));
        self.globals
            .insert("Set".to_string(), JsValue::Object(set_ctor));

        // Promise constructor + prototype
        let promise_proto = builtins::create_promise_prototype(&mut self.heap);
        self.promise_prototype = Some(promise_proto);
        let mut promise_ctor =
            JsObject::native_function("Promise", builtins::promise_constructor_placeholder);
        // Static methods: Promise.resolve, Promise.reject
        {
            let resolve_fn = JsObject::native_function("resolve", builtins::promise_resolve_static);
            promise_ctor.define_property(
                "resolve".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(self.heap.alloc(resolve_fn))),
            );
            let reject_fn = JsObject::native_function("reject", builtins::promise_reject_static);
            promise_ctor.define_property(
                "reject".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(self.heap.alloc(reject_fn))),
            );
        }
        let promise_ctor_ptr = self.heap.alloc(promise_ctor);
        self.globals
            .insert("Promise".to_string(), JsValue::Object(promise_ctor_ptr));

        // Generator.prototype with next/return/throw
        let gen_proto =
            rawjs_runtime::builtins::generator::create_generator_prototype(&mut self.heap);
        self.generator_prototype = Some(gen_proto);

        // JSON object
        let json_ptr = builtins::create_json_object(&mut self.heap);
        self.globals
            .insert("JSON".to_string(), JsValue::Object(json_ptr));

        // Error constructors
        for name in &[
            "Error",
            "TypeError",
            "ReferenceError",
            "SyntaxError",
            "RangeError",
        ] {
            let ctor = builtins::create_error_constructor(&mut self.heap, name);
            self.globals.insert(name.to_string(), JsValue::Object(ctor));
        }

        // Global functions (parseInt, parseFloat, isNaN, isFinite, encodeURIComponent, etc.)
        let global_fns = builtins::create_global_functions(&mut self.heap);
        for (name, func_ptr) in global_fns {
            self.globals
                .insert(name.to_string(), JsValue::Object(func_ptr));
        }

        // Keep Object.prototype ref for prototype chain lookups
        self.globals.insert(
            "__object_prototype__".to_string(),
            JsValue::Object(obj_proto),
        );
    }

    // -----------------------------------------------------------------
    // Chunk management
    // -----------------------------------------------------------------

    /// Register a chunk in the VM and return its index.
    pub fn add_chunk(&mut self, chunk: Chunk) -> usize {
        let idx = self.chunks.len();
        self.chunks.push(chunk);
        idx
    }

    /// Borrow a chunk by index.
    #[allow(dead_code)]
    pub(crate) fn chunk(&self, index: usize) -> &Chunk {
        &self.chunks[index]
    }

    // -----------------------------------------------------------------
    // Execution entry point
    // -----------------------------------------------------------------

    /// Execute a top-level chunk.
    ///
    /// Creates an initial call frame, runs the interpreter loop, and returns
    /// the final value left on the stack (or `undefined`).
    pub fn execute(&mut self, chunk: Chunk) -> Result<JsValue> {
        let chunk_index = self.add_chunk(chunk);

        let local_count = self.chunks[chunk_index].local_count as usize;

        let frame = CallFrame {
            chunk_index,
            ip: 0,
            base: self.value_stack.len(),
            locals: vec![JsValue::Undefined; local_count],
            upvalues: Vec::new(),
            this_value: JsValue::Undefined,
        };
        self.call_stack.push(frame);

        self.run()
    }

    /// Main execution loop -- delegates to the interpreter.
    pub fn run(&mut self) -> Result<JsValue> {
        interpreter::run(self)
    }

    // -----------------------------------------------------------------
    // Global access helpers
    // -----------------------------------------------------------------

    /// Get a global variable by name.
    pub fn get_global(&self, name: &str) -> Option<&JsValue> {
        self.globals.get(name)
    }

    /// Set (or create) a global variable.
    pub fn set_global(&mut self, name: String, value: JsValue) {
        self.globals.insert(name, value);
    }

    // -----------------------------------------------------------------
    // JIT helpers
    // -----------------------------------------------------------------

    /// Increment the execution count for a chunk and return the new count.
    pub(crate) fn bump_execution_count(&mut self, chunk_index: usize) -> u32 {
        let count = self.execution_counts.entry(chunk_index).or_insert(0);
        *count += 1;
        *count
    }

    /// Check if a JIT-compiled version exists for a chunk.
    pub(crate) fn has_jit(&self, chunk_index: usize) -> bool {
        self.jit_cache.contains_key(&chunk_index)
    }

    /// Attempt to JIT-compile a chunk.  Returns `true` on success.
    pub(crate) fn try_jit_compile(&mut self, chunk_index: usize) -> bool {
        if self.jit_cache.contains_key(&chunk_index) {
            return true;
        }
        let chunk = &self.chunks[chunk_index];
        match jit::JitCompiler::compile(chunk) {
            Some(func) => {
                self.jit_cache.insert(chunk_index, func);
                true
            }
            None => false,
        }
    }

    /// Execute a JIT-compiled function for the current top-of-stack call frame.
    ///
    /// The caller must have already pushed a `CallFrame` onto `call_stack`.
    /// The JIT function will execute the frame via stub calls and pop it
    /// on return.
    ///
    /// Returns `Ok(())` on success (result is on `value_stack`).
    /// Returns `Err` on failure (details in `jit_error`).
    ///
    /// # Safety
    ///
    /// The JIT function pointer must have been produced by `JitCompiler` and
    /// must still be mapped and valid.
    pub(crate) unsafe fn call_jit_new(&mut self, chunk_index: usize) -> u32 {
        if let Some(func) = self.jit_cache.get(&chunk_index) {
            let func_ptr = func.code;
            let func: extern "C" fn(*mut Vm) -> u32 = std::mem::transmute(func_ptr);
            func(self as *mut Vm)
        } else {
            1
        }
    }

    // -----------------------------------------------------------------
    // Stack helpers
    // -----------------------------------------------------------------

    #[inline]
    pub(crate) fn push(&mut self, value: JsValue) {
        self.value_stack.push(value);
    }

    #[inline]
    pub(crate) fn pop(&mut self) -> Result<JsValue> {
        self.value_stack
            .pop()
            .ok_or_else(|| RawJsError::internal_error("stack underflow"))
    }

    #[inline]
    pub(crate) fn peek(&self) -> Result<&JsValue> {
        self.value_stack
            .last()
            .ok_or_else(|| RawJsError::internal_error("stack underflow"))
    }

    // -----------------------------------------------------------------
    // Module system (ESM)
    // -----------------------------------------------------------------

    /// Execute a module file and return its namespace (exports) object.
    ///
    /// Resolves the path relative to `current_file_dir`, reads the file,
    /// parses, compiles, and executes it.  Results are cached so each
    /// module is only executed once.
    pub fn execute_module(&mut self, specifier: &str) -> Result<GcPtr<JsObject>> {
        // --- Resolve path ---
        let resolved = self.resolve_module_path(specifier)?;

        // --- Cache check ---
        if let Some(cached) = self.module_cache.get(&resolved) {
            return Ok(cached.clone());
        }

        // --- Read source ---
        let source = std::fs::read_to_string(&resolved).map_err(|e| {
            RawJsError::type_error(format!("Cannot find module '{}': {}", specifier, e))
        })?;

        // --- Parse ---
        let program = rawjs_parser::parse(&source).map_err(|e| {
            RawJsError::syntax_error(format!("Error parsing module '{}': {}", specifier, e), None)
        })?;

        // --- Compile ---
        let chunk = rawjs_bytecode::compile(&program).map_err(|e| {
            RawJsError::internal_error(format!("Error compiling module '{}': {}", specifier, e))
        })?;

        // --- Prepare exports object ---
        let exports = self.heap.alloc(JsObject::ordinary());

        // Save parent context
        let prev_file_dir = self.current_file_dir.take();
        let prev_exports = self.module_exports.take();

        // Set module context
        let module_dir = Path::new(&resolved)
            .parent()
            .map(|p| p.to_string_lossy().to_string());
        self.current_file_dir = module_dir;
        self.module_exports = Some(exports.clone());

        // --- Execute ---
        let chunk_index = self.add_chunk(chunk);
        let local_count = self.chunks[chunk_index].local_count as usize;

        let frame = CallFrame {
            chunk_index,
            ip: 0,
            base: self.value_stack.len(),
            locals: vec![JsValue::Undefined; local_count],
            upvalues: Vec::new(),
            this_value: JsValue::Undefined,
        };
        self.call_stack.push(frame);

        // Run only the module frame.  We record the call-stack depth so
        // the interpreter stops when the module's frame returns instead
        // of continuing into the caller's frame.
        interpreter::run_module_frame(self)?;

        // --- Restore parent context ---
        let final_exports = self.module_exports.take().unwrap_or(exports);
        self.current_file_dir = prev_file_dir;
        self.module_exports = prev_exports;

        // --- Cache result ---
        self.module_cache.insert(resolved, final_exports.clone());

        Ok(final_exports)
    }

    /// Resolve a module specifier to an absolute file path.
    fn resolve_module_path(&self, specifier: &str) -> Result<String> {
        // Only relative paths are supported
        if !specifier.starts_with("./") && !specifier.starts_with("../") {
            return Err(RawJsError::type_error(format!(
                "Bare module specifier '{}' is not supported. Use relative paths (./...)",
                specifier
            )));
        }

        let base_dir = self.current_file_dir.as_deref().unwrap_or(".");

        let mut path = PathBuf::from(base_dir);
        path.push(specifier);

        // Auto-append .js extension if missing
        if path.extension().is_none() {
            path.set_extension("js");
        }

        // Canonicalize to resolve ../ etc.  If the file doesn't exist yet,
        // fall back to the cleaned-up path string.
        let resolved = path.canonicalize().unwrap_or_else(|_| path.clone());

        Ok(resolved.to_string_lossy().to_string())
    }

    // -----------------------------------------------------------------
    // Heap access (GcHeap impl)
    // -----------------------------------------------------------------
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use rawjs_bytecode::{Chunk, Constant, Instruction};

    /// Helper: build a chunk that pushes a number constant and returns it.
    fn make_return_number(n: f64) -> Chunk {
        let mut chunk = Chunk::new("<test>");
        let idx = chunk.add_constant(Constant::Number(n)).unwrap();
        chunk.emit(Instruction::LoadConst(idx));
        chunk.emit(Instruction::Return);
        chunk
    }

    #[test]
    fn test_vm_new_has_globals() {
        let vm = Vm::new();
        assert!(vm.get_global("undefined").is_some());
        assert!(vm.get_global("NaN").is_some());
        assert!(vm.get_global("Infinity").is_some());
        assert!(vm.get_global("console").is_some());
        assert!(vm.get_global("Math").is_some());
        assert!(vm.get_global("Object").is_some());
        assert!(vm.get_global("Array").is_some());
    }

    #[test]
    fn test_execute_return_number() {
        let mut vm = Vm::new();
        let result = vm.execute(make_return_number(42.0)).unwrap();
        assert_eq!(result, JsValue::Number(42.0));
    }

    #[test]
    fn test_execute_simple_add() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let a = chunk.add_constant(Constant::Number(10.0)).unwrap();
        let b = chunk.add_constant(Constant::Number(32.0)).unwrap();
        chunk.emit(Instruction::LoadConst(a));
        chunk.emit(Instruction::LoadConst(b));
        chunk.emit(Instruction::Add);
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Number(42.0));
    }

    #[test]
    fn test_execute_string_concat() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let a = chunk
            .add_constant(Constant::String("hello ".to_string()))
            .unwrap();
        let b = chunk
            .add_constant(Constant::String("world".to_string()))
            .unwrap();
        chunk.emit(Instruction::LoadConst(a));
        chunk.emit(Instruction::LoadConst(b));
        chunk.emit(Instruction::Add);
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::string("hello world"));
    }

    #[test]
    fn test_execute_comparison() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let a = chunk.add_constant(Constant::Number(5.0)).unwrap();
        let b = chunk.add_constant(Constant::Number(3.0)).unwrap();
        chunk.emit(Instruction::LoadConst(a));
        chunk.emit(Instruction::LoadConst(b));
        chunk.emit(Instruction::Gt);
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Boolean(true));
    }

    #[test]
    fn test_execute_local_vars() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        chunk.local_count = 1;
        let val = chunk.add_constant(Constant::Number(99.0)).unwrap();
        chunk.emit(Instruction::LoadConst(val));
        chunk.emit(Instruction::StoreLocal(0));
        chunk.emit(Instruction::LoadLocal(0));
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Number(99.0));
    }

    #[test]
    fn test_execute_jump_if_false() {
        // if (false) { return 1; } return 2;
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let one = chunk.add_constant(Constant::Number(1.0)).unwrap();
        let two = chunk.add_constant(Constant::Number(2.0)).unwrap();

        chunk.emit(Instruction::False); // 0
        let jmp = chunk.emit(Instruction::JumpIfFalse(0)); // 1 -- placeholder
        chunk.emit(Instruction::LoadConst(one)); // 2
        chunk.emit(Instruction::Return); // 3
        chunk.emit(Instruction::LoadConst(two)); // 4
        chunk.emit(Instruction::Return); // 5

        // Patch: jump from instruction 1 to instruction 4 -- offset = 4 - (1+1) = 2
        chunk.patch_jump(jmp, 2);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Number(2.0));
    }

    #[test]
    fn test_execute_create_object() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        chunk.emit(Instruction::CreateObject);
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert!(result.is_object());
    }

    #[test]
    fn test_execute_create_array() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let a = chunk.add_constant(Constant::Number(1.0)).unwrap();
        let b = chunk.add_constant(Constant::Number(2.0)).unwrap();
        let c = chunk.add_constant(Constant::Number(3.0)).unwrap();
        chunk.emit(Instruction::LoadConst(a));
        chunk.emit(Instruction::LoadConst(b));
        chunk.emit(Instruction::LoadConst(c));
        chunk.emit(Instruction::CreateArray(3));
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert!(result.is_object());
        if let JsValue::Object(obj) = &result {
            assert!(obj.borrow().is_array());
            assert_eq!(obj.borrow().as_array().unwrap().len(), 3);
        }
    }

    #[test]
    fn test_execute_unary_neg() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let val = chunk.add_constant(Constant::Number(7.0)).unwrap();
        chunk.emit(Instruction::LoadConst(val));
        chunk.emit(Instruction::Neg);
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Number(-7.0));
    }

    #[test]
    fn test_execute_typeof() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let val = chunk.add_constant(Constant::Number(1.0)).unwrap();
        chunk.emit(Instruction::LoadConst(val));
        chunk.emit(Instruction::TypeOf);
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::string("number"));
    }

    #[test]
    fn test_execute_dup_and_pop() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let val = chunk.add_constant(Constant::Number(5.0)).unwrap();
        chunk.emit(Instruction::LoadConst(val));
        chunk.emit(Instruction::Dup); // stack: 5, 5
        chunk.emit(Instruction::Pop); // stack: 5
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Number(5.0));
    }

    #[test]
    fn test_execute_global_store_load() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let name_idx = chunk
            .add_constant(Constant::String("myGlobal".to_string()))
            .unwrap();
        let val_idx = chunk.add_constant(Constant::Number(123.0)).unwrap();
        chunk.emit(Instruction::LoadConst(val_idx));
        chunk.emit(Instruction::StoreGlobal(name_idx));
        chunk.emit(Instruction::LoadGlobal(name_idx));
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Number(123.0));
    }

    #[test]
    fn test_execute_bitwise() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let a = chunk.add_constant(Constant::Number(0b1100 as f64)).unwrap();
        let b = chunk.add_constant(Constant::Number(0b1010 as f64)).unwrap();
        chunk.emit(Instruction::LoadConst(a));
        chunk.emit(Instruction::LoadConst(b));
        chunk.emit(Instruction::BitAnd);
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Number(0b1000 as f64));
    }

    #[test]
    fn test_execute_function_call() {
        let mut vm = Vm::new();

        // Inner function: takes one param, returns param + 1
        let mut func_chunk = Chunk::new("addOne");
        func_chunk.param_count = 1;
        func_chunk.local_count = 1;
        let one = func_chunk.add_constant(Constant::Number(1.0)).unwrap();
        func_chunk.emit(Instruction::LoadLocal(0));
        func_chunk.emit(Instruction::LoadConst(one));
        func_chunk.emit(Instruction::Add);
        func_chunk.emit(Instruction::Return);

        // Outer chunk: create closure, call with arg 41
        let mut chunk = Chunk::new("<test>");
        let func_const = chunk
            .add_constant(Constant::Function(Box::new(func_chunk)))
            .unwrap();
        let arg = chunk.add_constant(Constant::Number(41.0)).unwrap();

        chunk.emit(Instruction::CreateClosure(func_const));
        chunk.emit(Instruction::LoadConst(arg));
        chunk.emit(Instruction::Call(1));
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Number(42.0));
    }

    #[test]
    fn test_execute_try_catch() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        chunk.local_count = 1; // for the caught error

        let msg_idx = chunk
            .add_constant(Constant::String("oops".to_string()))
            .unwrap();
        let result_idx = chunk.add_constant(Constant::Number(999.0)).unwrap();

        // 0: EnterTry(catch_offset=2, finally=0)
        //    IP advances to 1 after EnterTry, so catch_ip = 1 + 2 = 3
        chunk.emit(Instruction::EnterTry(2, 0));
        // 1: Push "oops"
        chunk.emit(Instruction::LoadConst(msg_idx));
        // 2: Throw
        chunk.emit(Instruction::Throw);
        // 3: catch block -- the thrown value is on the stack
        chunk.emit(Instruction::StoreLocal(0));
        // 4: LeaveTry
        chunk.emit(Instruction::LeaveTry);
        // 5: Push 999
        chunk.emit(Instruction::LoadConst(result_idx));
        // 6: Return
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Number(999.0));
    }

    #[test]
    fn test_execute_set_get_property() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        chunk.local_count = 1;

        let name_idx = chunk
            .add_constant(Constant::String("x".to_string()))
            .unwrap();
        let val_idx = chunk.add_constant(Constant::Number(42.0)).unwrap();

        // Create object and store in local
        chunk.emit(Instruction::CreateObject); // 0
        chunk.emit(Instruction::Dup); // 1 -- keep ref on stack
        chunk.emit(Instruction::StoreLocal(0)); // 2

        // Set property: stack = [obj], push value
        chunk.emit(Instruction::LoadConst(val_idx)); // 3
        chunk.emit(Instruction::SetProperty(name_idx)); // 4 -- pops obj, val; pushes val
        chunk.emit(Instruction::Pop); // 5 -- discard returned val

        // Get property
        chunk.emit(Instruction::LoadLocal(0)); // 6
        chunk.emit(Instruction::GetProperty(name_idx)); // 7
        chunk.emit(Instruction::Return); // 8

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Number(42.0));
    }

    #[test]
    fn test_execute_void() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let val = chunk.add_constant(Constant::Number(42.0)).unwrap();
        chunk.emit(Instruction::LoadConst(val));
        chunk.emit(Instruction::Void);
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert!(result.is_undefined());
    }

    #[test]
    fn test_execute_not() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        chunk.emit(Instruction::True);
        chunk.emit(Instruction::Not);
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Boolean(false));
    }

    #[test]
    fn test_execute_strict_eq() {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("<test>");
        let a = chunk.add_constant(Constant::Number(1.0)).unwrap();
        chunk.emit(Instruction::LoadConst(a));
        chunk.emit(Instruction::LoadConst(a));
        chunk.emit(Instruction::StrictEq);
        chunk.emit(Instruction::Return);

        let result = vm.execute(chunk).unwrap();
        assert_eq!(result, JsValue::Boolean(true));
    }

    #[test]
    fn test_add_chunk_returns_index() {
        let mut vm = Vm::new();
        let a = vm.add_chunk(Chunk::new("a"));
        let b = vm.add_chunk(Chunk::new("b"));
        assert_eq!(a, 0);
        assert_eq!(b, 1);
    }
}
