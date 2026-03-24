//! The rawjs virtual machine.
//!
//! This crate contains:
//! - A bytecode **interpreter** that executes `Chunk` programs from `rawjs_bytecode`.
//! - An aarch64 **JIT compiler** (macOS Apple Silicon) that compiles hot functions
//!   to native machine code.

pub mod interpreter;
pub mod jit;

use std::collections::{HashMap, HashSet};

use rawjs_bytecode::Chunk;
use rawjs_common::{RawJsError, Result};
use rawjs_runtime::{GcPtr, Heap, JsObject, JsValue, ObjectInternal, Property, Upvalue};

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
            global_object: None,
            chunks: Vec::new(),
            call_stack: Vec::new(),
            value_stack: Vec::new(),
            try_stack: Vec::new(),
            construct_frames: Vec::new(),
            jit_cache: HashMap::new(),
            execution_counts: HashMap::new(),
            thrown_value: None,
            jit_error: None,
            array_prototype: None,
            function_prototype: None,
            string_prototype: None,
            number_prototype: None,
            boolean_prototype: None,
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

        // Object.prototype and Object constructor
        let obj_proto = builtins::create_object_prototype(&mut self.heap);
        self.object_prototype = Some(obj_proto.clone());

        // Function constructor + prototype
        let function_proto = builtins::create_function_prototype(&mut self.heap);
        function_proto.borrow_mut().prototype = Some(obj_proto.clone());
        self.function_prototype = Some(function_proto.clone());
        let function_ctor = self.heap.alloc(JsObject::native_function(
            "Function",
            builtins::function_constructor_placeholder,
        ));
        {
            let mut ctor = function_ctor.borrow_mut();
            ctor.prototype = Some(obj_proto.clone());
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(function_proto.clone())),
            );
        }
        function_proto.borrow_mut().define_property(
            "constructor".to_string(),
            rawjs_runtime::Property::builtin(JsValue::Object(function_ctor.clone())),
        );
        self.globals
            .insert("Function".to_string(), JsValue::Object(function_ctor));

        let object_ctor = builtins::create_object_constructor(&mut self.heap);
        {
            let mut ctor = object_ctor.borrow_mut();
            ctor.prototype = Some(function_proto.clone());
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(obj_proto.clone())),
            );
        }
        obj_proto.borrow_mut().define_property(
            "constructor".to_string(),
            rawjs_runtime::Property::builtin(JsValue::Object(object_ctor.clone())),
        );
        self.globals
            .insert("Object".to_string(), JsValue::Object(object_ctor));

        // console object
        let console_ptr = builtins::create_console_object(&mut self.heap);
        console_ptr.borrow_mut().prototype = Some(obj_proto.clone());
        self.globals
            .insert("console".to_string(), JsValue::Object(console_ptr));

        // Math object
        let math_ptr = builtins::create_math_object(&mut self.heap);
        math_ptr.borrow_mut().prototype = Some(obj_proto.clone());
        self.globals
            .insert("Math".to_string(), JsValue::Object(math_ptr));

        // Array constructor + prototype
        let array_proto = builtins::create_array_prototype(&mut self.heap);
        array_proto.borrow_mut().prototype = Some(obj_proto.clone());
        self.array_prototype = Some(array_proto);
        let array_ctor = self.heap.alloc(JsObject::native_function(
            "Array",
            builtins::array_constructor,
        ));
        {
            let mut ctor = array_ctor.borrow_mut();
            ctor.prototype = Some(function_proto.clone());
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(
                    self.array_prototype.as_ref().unwrap().clone(),
                )),
            );
            ctor.define_property(
                "isArray".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(self.heap.alloc(
                    JsObject::native_function("isArray", builtins::array_is_array),
                ))),
            );
        }
        self.globals
            .insert("Array".to_string(), JsValue::Object(array_ctor));

        // String.prototype
        let string_proto = builtins::create_string_prototype(&mut self.heap);
        string_proto.borrow_mut().prototype = Some(obj_proto.clone());
        self.string_prototype = Some(string_proto);
        let string_ctor = self.heap.alloc(JsObject::native_function(
            "String",
            builtins::string_constructor,
        ));
        {
            let mut ctor = string_ctor.borrow_mut();
            ctor.prototype = Some(function_proto.clone());
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(
                    self.string_prototype.as_ref().unwrap().clone(),
                )),
            );
        }
        self.string_prototype
            .as_ref()
            .unwrap()
            .borrow_mut()
            .define_property(
                "constructor".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(string_ctor.clone())),
            );
        self.globals
            .insert("String".to_string(), JsValue::Object(string_ctor));

        // Boolean.prototype
        let boolean_proto = builtins::create_boolean_prototype(&mut self.heap);
        boolean_proto.borrow_mut().prototype = Some(obj_proto.clone());
        self.boolean_prototype = Some(boolean_proto);
        let boolean_ctor = self.heap.alloc(JsObject::native_function(
            "Boolean",
            builtins::boolean_constructor,
        ));
        {
            let mut ctor = boolean_ctor.borrow_mut();
            ctor.prototype = Some(function_proto.clone());
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(
                    self.boolean_prototype.as_ref().unwrap().clone(),
                )),
            );
        }
        self.boolean_prototype
            .as_ref()
            .unwrap()
            .borrow_mut()
            .define_property(
                "constructor".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(boolean_ctor.clone())),
            );
        self.globals
            .insert("Boolean".to_string(), JsValue::Object(boolean_ctor));

        // Number.prototype
        let number_proto = builtins::create_number_prototype(&mut self.heap);
        number_proto.borrow_mut().prototype = Some(obj_proto.clone());
        self.number_prototype = Some(number_proto);
        let number_ctor = self.heap.alloc(JsObject::native_function(
            "Number",
            builtins::number_constructor,
        ));
        {
            let mut ctor = number_ctor.borrow_mut();
            ctor.prototype = Some(function_proto.clone());
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(
                    self.number_prototype.as_ref().unwrap().clone(),
                )),
            );
            ctor.define_property(
                "NaN".to_string(),
                rawjs_runtime::Property::readonly_builtin(JsValue::Number(f64::NAN)),
            );
            ctor.define_property(
                "POSITIVE_INFINITY".to_string(),
                rawjs_runtime::Property::readonly_builtin(JsValue::Number(f64::INFINITY)),
            );
            ctor.define_property(
                "NEGATIVE_INFINITY".to_string(),
                rawjs_runtime::Property::readonly_builtin(JsValue::Number(f64::NEG_INFINITY)),
            );
            ctor.define_property(
                "MAX_VALUE".to_string(),
                rawjs_runtime::Property::readonly_builtin(JsValue::Number(f64::MAX)),
            );
            ctor.define_property(
                "MIN_VALUE".to_string(),
                rawjs_runtime::Property::readonly_builtin(JsValue::Number(f64::MIN_POSITIVE)),
            );
        }
        self.number_prototype
            .as_ref()
            .unwrap()
            .borrow_mut()
            .define_property(
                "constructor".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(number_ctor.clone())),
            );
        self.globals
            .insert("Number".to_string(), JsValue::Object(number_ctor));

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
        symbol_ptr.borrow_mut().prototype = Some(function_proto.clone());
        self.globals
            .insert("Symbol".to_string(), JsValue::Object(symbol_ptr));

        // Symbol.prototype
        let symbol_proto = builtins::create_symbol_prototype(&mut self.heap);
        symbol_proto.borrow_mut().prototype = Some(obj_proto.clone());
        if let Some(JsValue::Object(symbol_ctor)) = self.globals.get("Symbol") {
            symbol_ctor.borrow_mut().define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(symbol_proto.clone())),
            );
        }
        self.symbol_prototype = Some(symbol_proto);

        // Map constructor + prototype
        let map_proto = builtins::create_map_prototype(&mut self.heap);
        map_proto.borrow_mut().prototype = Some(obj_proto.clone());
        self.map_prototype = Some(map_proto);
        let map_ctor = self
            .heap
            .alloc(JsObject::native_function("Map", builtins::map_constructor));
        {
            let mut ctor = map_ctor.borrow_mut();
            ctor.prototype = Some(function_proto.clone());
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(
                    self.map_prototype.as_ref().unwrap().clone(),
                )),
            );
        }
        self.globals
            .insert("Map".to_string(), JsValue::Object(map_ctor));

        // Set constructor + prototype
        let set_proto = builtins::create_set_prototype(&mut self.heap);
        set_proto.borrow_mut().prototype = Some(obj_proto.clone());
        self.set_prototype = Some(set_proto);
        let set_ctor = self
            .heap
            .alloc(JsObject::native_function("Set", builtins::set_constructor));
        {
            let mut ctor = set_ctor.borrow_mut();
            ctor.prototype = Some(function_proto.clone());
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(
                    self.set_prototype.as_ref().unwrap().clone(),
                )),
            );
        }
        self.globals
            .insert("Set".to_string(), JsValue::Object(set_ctor));

        // Promise constructor + prototype
        let promise_proto = builtins::create_promise_prototype(&mut self.heap);
        promise_proto.borrow_mut().prototype = Some(obj_proto.clone());
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
        {
            let mut ctor = promise_ctor_ptr.borrow_mut();
            ctor.prototype = Some(function_proto.clone());
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(
                    self.promise_prototype.as_ref().unwrap().clone(),
                )),
            );
        }
        self.globals
            .insert("Promise".to_string(), JsValue::Object(promise_ctor_ptr));

        // Generator.prototype with next/return/throw
        let gen_proto =
            rawjs_runtime::builtins::generator::create_generator_prototype(&mut self.heap);
        gen_proto.borrow_mut().prototype = Some(obj_proto.clone());
        self.generator_prototype = Some(gen_proto);

        // JSON object
        let json_ptr = builtins::create_json_object(&mut self.heap);
        json_ptr.borrow_mut().prototype = Some(obj_proto.clone());
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
            ctor.borrow_mut().prototype = Some(function_proto.clone());
            self.globals.insert(name.to_string(), JsValue::Object(ctor));
        }

        // Global functions (parseInt, parseFloat, isNaN, isFinite, encodeURIComponent, etc.)
        let global_fns = builtins::create_global_functions(&mut self.heap);
        for (name, func_ptr) in global_fns {
            func_ptr.borrow_mut().prototype = Some(function_proto.clone());
            self.globals
                .insert(name.to_string(), JsValue::Object(func_ptr));
        }

        // Keep Object.prototype ref for prototype chain lookups
        self.globals.insert(
            "__object_prototype__".to_string(),
            JsValue::Object(obj_proto),
        );

        let global_obj = self.heap.alloc(JsObject::ordinary());
        if let Some(ref proto) = self.object_prototype {
            global_obj.borrow_mut().prototype = Some(proto.clone());
        }
        for (name, value) in self.globals.clone() {
            global_obj.borrow_mut().set_property(name, value);
        }
        global_obj.borrow_mut().set_property(
            "globalThis".to_string(),
            JsValue::Object(global_obj.clone()),
        );
        self.global_object = Some(global_obj);
        self.repair_builtin_function_prototypes();
    }

    fn repair_builtin_function_prototypes(&mut self) {
        let Some(function_proto) = self.function_prototype.clone() else {
            return;
        };
        let Some(object_proto) = self.object_prototype.clone() else {
            return;
        };

        let mut visited = HashSet::new();
        let mut roots: Vec<JsValue> = self.globals.values().cloned().collect();

        for ptr in [
            self.global_object.clone(),
            self.array_prototype.clone(),
            self.function_prototype.clone(),
            self.string_prototype.clone(),
            self.number_prototype.clone(),
            self.boolean_prototype.clone(),
            self.object_prototype.clone(),
            self.symbol_prototype.clone(),
            self.map_prototype.clone(),
            self.set_prototype.clone(),
            self.promise_prototype.clone(),
            self.generator_prototype.clone(),
        ]
        .into_iter()
        .flatten()
        {
            roots.push(JsValue::Object(ptr));
        }

        for root in roots {
            Self::repair_builtin_function_prototypes_in_value(
                &root,
                &function_proto,
                &object_proto,
                &mut visited,
            );
        }
    }

    fn repair_builtin_function_prototypes_in_value(
        value: &JsValue,
        function_proto: &GcPtr<JsObject>,
        object_proto: &GcPtr<JsObject>,
        visited: &mut HashSet<usize>,
    ) {
        if let JsValue::Object(ptr) = value {
            Self::repair_builtin_function_prototypes_in_object(
                ptr.clone(),
                function_proto,
                object_proto,
                visited,
            );
        }
    }

    fn repair_builtin_function_prototypes_in_object(
        ptr: GcPtr<JsObject>,
        function_proto: &GcPtr<JsObject>,
        object_proto: &GcPtr<JsObject>,
        visited: &mut HashSet<usize>,
    ) {
        if !visited.insert(ptr.addr()) {
            return;
        }

        let mut nested_values = Vec::new();
        let mut constructor_target = None;

        {
            let mut obj = ptr.borrow_mut();

            if matches!(&obj.internal, ObjectInternal::Function(_)) {
                if obj.prototype.is_none() {
                    obj.prototype = Some(function_proto.clone());
                }

                if let Some(prop) = obj.properties.get("prototype") {
                    if let JsValue::Object(proto_obj) = &prop.value {
                        constructor_target = Some(proto_obj.clone());
                    }
                }
            }

            if let Some(proto) = obj.prototype.clone() {
                nested_values.push(JsValue::Object(proto));
            }

            for prop in obj.properties.values() {
                nested_values.push(prop.value.clone());
                if let Some(getter) = &prop.get {
                    nested_values.push(getter.clone());
                }
                if let Some(setter) = &prop.set {
                    nested_values.push(setter.clone());
                }
            }

            for prop in obj.symbol_properties.values() {
                nested_values.push(prop.value.clone());
                if let Some(getter) = &prop.get {
                    nested_values.push(getter.clone());
                }
                if let Some(setter) = &prop.set {
                    nested_values.push(setter.clone());
                }
            }
        }

        if let Some(proto_obj) = constructor_target {
            let needs_proto = proto_obj.borrow().prototype.is_none();
            if needs_proto && !proto_obj.ptr_eq(object_proto) {
                proto_obj.borrow_mut().prototype = Some(object_proto.clone());
            }

            let has_constructor = proto_obj.borrow().has_own_property("constructor");
            if !has_constructor {
                proto_obj.borrow_mut().define_property(
                    "constructor".to_string(),
                    Property::builtin(JsValue::Object(ptr.clone())),
                );
            }
        }

        for nested in nested_values {
            Self::repair_builtin_function_prototypes_in_value(
                &nested,
                function_proto,
                object_proto,
                visited,
            );
        }
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
            arguments: Vec::new(),
            arguments_object: None,
            callee: None,
            is_strict: self.chunks[chunk_index].is_strict,
            upvalues: Vec::new(),
            this_value: self.global_this_value(),
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
        self.globals.insert(name.clone(), value.clone());
        if let Some(ref global_obj) = self.global_object {
            global_obj.borrow_mut().set_property(name, value);
        }
    }

    /// Delete a global variable. Returns `true` if it existed.
    pub(crate) fn delete_global(&mut self, name: &str) -> bool {
        let existed = self.globals.remove(name).is_some();
        if let Some(ref global_obj) = self.global_object {
            global_obj.borrow_mut().delete_property(name);
        }
        existed
    }

    /// Get the current global `this` value.
    pub(crate) fn global_this_value(&self) -> JsValue {
        self.global_object
            .as_ref()
            .map(|ptr| JsValue::Object(ptr.clone()))
            .unwrap_or(JsValue::Undefined)
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
            arguments: Vec::new(),
            arguments_object: None,
            callee: None,
            is_strict: self.chunks[chunk_index].is_strict,
            upvalues: Vec::new(),
            this_value: self.global_this_value(),
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

    fn execute_source(source: &str) -> Vm {
        let program = rawjs_parser::parse(source).unwrap();
        let chunk = rawjs_bytecode::compile(&program).unwrap();
        let mut vm = Vm::new();
        vm.execute(chunk).unwrap();
        vm
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
    fn test_execute_member_postfix_increment_uses_numeric_coercion() {
        let vm = execute_source("var __map = { foo: 'bar' }; __map.foo++;");
        let map = vm.get_global("__map").unwrap().clone();
        let JsValue::Object(obj) = map else {
            panic!("expected object");
        };
        let value = obj.borrow().get_property("foo");
        match value {
            JsValue::Number(n) => assert!(n.is_nan()),
            other => panic!("expected NaN number, got {other}"),
        }
    }

    #[test]
    fn test_execute_accessor_property_setter() {
        let vm = execute_source(
            "var o = {}; var v = 1; Object.defineProperty(o, 'b', { get: function () { return v; }, set: function (value) { v = value; } }); o.b = 11;",
        );
        assert_eq!(vm.get_global("v"), Some(&JsValue::Number(11.0)));
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

    #[test]
    fn test_execute_new_function_constructor_uses_prototype() {
        let vm = execute_source(
            r#"
            function Test262Error(message) {
              this.message = message;
            }
            Test262Error.prototype.answer = 42;
            let err = new Test262Error("boom");
            result = err.answer;
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Number(42.0));
    }

    #[test]
    fn test_execute_number_static_constants_are_read_only() {
        let vm = execute_source(
            r#"
            Number.NaN = 1;
            result = Number.NaN === 1;
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Boolean(false));
    }

    #[test]
    fn test_execute_new_array_length() {
        let vm = execute_source(
            r#"
            let arr = new Array(5);
            result = arr.length;
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Number(5.0));
    }

    #[test]
    fn test_execute_new_object_missing_property_is_undefined() {
        let vm = execute_source(
            r#"
            result = (new Object()).newProperty === undefined;
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Boolean(true));
    }

    #[test]
    fn test_execute_boolean_primitive_uses_boolean_prototype() {
        let vm = execute_source(
            r#"
            Boolean.prototype.answer = 42;
            result = true.answer;
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Number(42.0));
    }

    #[test]
    fn test_execute_new_boolean_value_of() {
        let vm = execute_source(
            r#"
            let boxed = new Boolean(false);
            result = boxed.valueOf();
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Boolean(false));
    }

    #[test]
    fn test_execute_primitive_assignment_uses_boolean_setter() {
        let vm = execute_source(
            r#"
            var count = 0;
            Object.defineProperty(Boolean.prototype, "test262", {
              set: function () { count += 1; }
            });
            true.test262 = null;
            result = count;
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Number(1.0));
    }

    #[test]
    fn test_execute_var_is_hoisted_before_initializer() {
        let vm = execute_source(
            r#"
            result = x === undefined;
            var x = true;
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Boolean(true));
    }

    #[test]
    fn test_execute_block_var_hoists_to_function_scope() {
        let vm = execute_source(
            r#"
            function f() {
              if (true) {
                var answer = 42;
              }
              return answer;
            }
            result = f();
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Number(42.0));
    }

    #[test]
    fn test_execute_strict_eval_rejects_arguments_assignment() {
        let vm = execute_source(
            r#"
            "use strict";
            var result = false;
            try {
              (function fun() {
                eval("arguments = 10");
              })(30);
            } catch (e) {
              result = e.name === "SyntaxError";
            }
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Boolean(true));
    }

    #[test]
    fn test_execute_arguments_object_persists_index_assignment() {
        let vm = execute_source(
            r#"
            function f() {
              arguments[7] = 12;
              return arguments[7];
            }
            result = f(30);
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Number(12.0));
    }

    #[test]
    fn test_execute_native_methods_inherit_function_prototype_call() {
        let vm = execute_source(
            r#"
            Function.prototype.call = function(thisArg) {
              var receiver = thisArg;
              if (receiver === null || receiver === undefined) {
                receiver = globalThis;
              }
              var key = "__rawjs_call__";
              while (receiver[key] !== undefined) {
                key += "_";
              }
              receiver[key] = this;
              var result = receiver[key](arguments[1]);
              delete receiver[key];
              return result;
            };

            var obj = { x: 1 };
            result = Object.prototype.hasOwnProperty.call(obj, "x");
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Boolean(true));
    }

    #[test]
    fn test_execute_descriptor_shim_can_call_native_object_methods() {
        let vm = execute_source(
            r#"
            Function.prototype.call = function(thisArg) {
              var receiver = thisArg;
              if (receiver === null || receiver === undefined) {
                receiver = globalThis;
              }
              var key = "__rawjs_call__";
              while (receiver[key] !== undefined) {
                key += "_";
              }
              receiver[key] = this;
              var result = receiver[key](arguments[1], arguments[2]);
              delete receiver[key];
              return result;
            };

            Object.getOwnPropertyDescriptor = function(obj, name) {
              if (!Object.prototype.hasOwnProperty.call(obj, name)) {
                return undefined;
              }
              return {
                value: obj[name],
                writable: true,
                enumerable: Object.prototype.propertyIsEnumerable.call(obj, name),
                configurable: true
              };
            };

            var argObj = (function () { return arguments; })(1);
            var desc = Object.getOwnPropertyDescriptor(argObj, "0");
            result = desc.value === 1 && desc.enumerable === true;
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Boolean(true));
    }

    #[test]
    fn test_execute_arguments_callee_descriptor() {
        let vm = execute_source(
            r#"
            function testcase() {
              var desc = Object.getOwnPropertyDescriptor(arguments, "callee");
              result =
                arguments.callee === testcase &&
                desc.configurable === true &&
                desc.enumerable === false &&
                desc.writable === true &&
                desc.hasOwnProperty("get") === false &&
                desc.hasOwnProperty("set") === false;
            }
            testcase();
            "#,
        );
        let result = vm.get_global("result").cloned().unwrap();
        assert_eq!(result, JsValue::Boolean(true));
    }
}
