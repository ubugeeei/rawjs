use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::gc::{GcPtr, Heap};
use crate::value::JsValue;
use rawjs_common::Result;

// ---------------------------------------------------------------------------
// Property descriptor
// ---------------------------------------------------------------------------

/// A property key -- either a string name, numeric index, or symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PropertyKey {
    String(Rc<str>),
    Index(u32),
    Symbol(u64),
}

impl PropertyKey {
    /// Create a string property key.
    pub fn from_string(s: &str) -> Self {
        // Try to parse as a u32 array index first.
        if let Ok(idx) = s.parse::<u32>() {
            // Verify round-trip (e.g. "01" should remain a string).
            if idx.to_string() == s {
                return PropertyKey::Index(idx);
            }
        }
        PropertyKey::String(Rc::from(s))
    }

    /// Produce the string representation of this key.
    pub fn as_str(&self) -> String {
        match self {
            PropertyKey::String(s) => s.to_string(),
            PropertyKey::Index(i) => i.to_string(),
            PropertyKey::Symbol(id) => format!("Symbol({})", id),
        }
    }

    /// Check if this key is a symbol.
    pub fn is_symbol(&self) -> bool {
        matches!(self, PropertyKey::Symbol(_))
    }
}

impl From<&str> for PropertyKey {
    fn from(s: &str) -> Self {
        PropertyKey::from_string(s)
    }
}

impl From<String> for PropertyKey {
    fn from(s: String) -> Self {
        PropertyKey::from_string(&s)
    }
}

impl From<u32> for PropertyKey {
    fn from(idx: u32) -> Self {
        PropertyKey::Index(idx)
    }
}

/// A full property descriptor (data property).
#[derive(Debug, Clone)]
pub struct Property {
    pub value: JsValue,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}

impl Property {
    /// Create a default data property (writable, enumerable, configurable).
    pub fn data(value: JsValue) -> Self {
        Property {
            value,
            writable: true,
            enumerable: true,
            configurable: true,
        }
    }

    /// Create a non-enumerable data property (used for built-in methods).
    pub fn builtin(value: JsValue) -> Self {
        Property {
            value,
            writable: true,
            enumerable: false,
            configurable: true,
        }
    }

    /// Create a read-only property.
    pub fn readonly(value: JsValue) -> Self {
        Property {
            value,
            writable: false,
            enumerable: true,
            configurable: false,
        }
    }
}

// ---------------------------------------------------------------------------
// PromiseState
// ---------------------------------------------------------------------------

/// The status of a Promise.
#[derive(Debug, Clone, PartialEq)]
pub enum PromiseStatus {
    Pending,
    Fulfilled,
    Rejected,
}

/// The kind of a promise reaction (then vs catch handler).
#[derive(Debug, Clone, PartialEq)]
pub enum PromiseReactionKind {
    Fulfill,
    Reject,
}

/// A reaction attached to a promise (a then/catch handler).
#[derive(Debug, Clone)]
pub struct PromiseReaction {
    pub handler: Option<JsValue>,
    pub kind: PromiseReactionKind,
    pub result_promise: Option<GcPtr<JsObject>>,
}

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

// ---------------------------------------------------------------------------
// IteratorState
// ---------------------------------------------------------------------------

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

    /// Advance the iterator and return `(value, done)`.
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

// ---------------------------------------------------------------------------
// ObjectInternal -- exotic internal behaviour
// ---------------------------------------------------------------------------

/// The internal kind of a JavaScript object.
#[derive(Debug, Clone)]
pub enum ObjectInternal {
    /// A plain object with no special semantics.
    Ordinary,
    /// A function object. Holds the chunk index and captured upvalues.
    Function(FunctionObject),
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

// ---------------------------------------------------------------------------
// GeneratorState
// ---------------------------------------------------------------------------

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
    pub upvalues: Vec<Upvalue>,
    pub saved_stack: Vec<JsValue>,
    pub this_value: JsValue,
    /// For async functions: the outer Promise that the caller awaits.
    pub result_promise: Option<GcPtr<JsObject>>,
    /// Saved try-stack entries (catch_ip, finally_ip, stack_depth, call_depth)
    /// when the generator/async function suspends at a yield/await point.
    pub saved_try_stack: Vec<(Option<usize>, Option<usize>, usize, usize)>,
}

// ---------------------------------------------------------------------------
// FunctionObject
// ---------------------------------------------------------------------------

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

impl fmt::Debug for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionKind::Bytecode { chunk_index } => {
                write!(f, "Bytecode(chunk={})", chunk_index)
            }
            FunctionKind::Native(_) => write!(f, "Native(<fn>)"),
        }
    }
}

// ---------------------------------------------------------------------------
// Upvalue
// ---------------------------------------------------------------------------

/// A shared upvalue cell.  Cloning an `Upvalue` creates another handle
/// to the **same** underlying `RefCell`, so mutations are visible through
/// every handle (parent frame, child closures, etc.).
#[derive(Debug, Clone)]
pub struct Upvalue(pub Rc<RefCell<JsValue>>);

impl Upvalue {
    /// Create a new closed upvalue wrapping the given value.
    pub fn new(value: JsValue) -> Self {
        Upvalue(Rc::new(RefCell::new(value)))
    }

    /// Read the current value.
    pub fn get(&self) -> JsValue {
        self.0.borrow().clone()
    }

    /// Overwrite the value.
    pub fn set(&self, value: JsValue) {
        *self.0.borrow_mut() = value;
    }
}

// ---------------------------------------------------------------------------
// JsObject
// ---------------------------------------------------------------------------

/// A JavaScript object: a bag of named properties plus optional internal state.
#[derive(Debug, Clone)]
pub struct JsObject {
    /// Named properties.
    pub properties: HashMap<String, Property>,
    /// Symbol-keyed properties (keyed by symbol ID).
    pub symbol_properties: HashMap<u64, Property>,
    /// Prototype chain link (null = end of chain).
    pub prototype: Option<GcPtr<JsObject>>,
    /// Internal / exotic object behaviour.
    pub internal: ObjectInternal,
}

impl JsObject {
    // -----------------------------------------------------------------
    // Constructors
    // -----------------------------------------------------------------

    /// Create a new ordinary (plain) object.
    pub fn ordinary() -> Self {
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            prototype: None,
            internal: ObjectInternal::Ordinary,
        }
    }

    /// Create a new ordinary object with a prototype.
    pub fn with_prototype(proto: GcPtr<JsObject>) -> Self {
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            prototype: Some(proto),
            internal: ObjectInternal::Ordinary,
        }
    }

    /// Create a new function object backed by bytecode.
    pub fn function(chunk_index: usize, upvalues: Vec<Upvalue>, name: String) -> Self {
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            prototype: None,
            internal: ObjectInternal::Function(FunctionObject {
                kind: FunctionKind::Bytecode { chunk_index },
                upvalues,
                name,
            }),
        }
    }

    /// Create a new native function object.
    pub fn native_function(name: impl Into<String>, func: NativeFn) -> Self {
        let name = name.into();
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            prototype: None,
            internal: ObjectInternal::Function(FunctionObject {
                kind: FunctionKind::Native(func),
                upvalues: Vec::new(),
                name,
            }),
        }
    }

    /// Create a new array object from a list of elements.
    pub fn array(elements: Vec<JsValue>) -> Self {
        let mut properties = HashMap::new();
        properties.insert(
            "length".to_string(),
            Property::data(JsValue::Number(elements.len() as f64)),
        );
        JsObject {
            properties,
            symbol_properties: HashMap::new(),
            prototype: None,
            internal: ObjectInternal::Array(elements),
        }
    }

    /// Create a new error object.
    pub fn error(message: impl Into<String>) -> Self {
        let msg: String = message.into();
        let mut properties = HashMap::new();
        properties.insert(
            "message".to_string(),
            Property::data(JsValue::string(msg.as_str())),
        );
        properties.insert("name".to_string(), Property::data(JsValue::string("Error")));
        JsObject {
            properties,
            symbol_properties: HashMap::new(),
            prototype: None,
            internal: ObjectInternal::Error(msg),
        }
    }

    /// Create a new iterator object from a vector of values.
    pub fn iterator(values: Vec<JsValue>) -> Self {
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            prototype: None,
            internal: ObjectInternal::Iterator(IteratorState::new(values)),
        }
    }

    /// Create a new Map object.
    pub fn map() -> Self {
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            prototype: None,
            internal: ObjectInternal::Map(Vec::new()),
        }
    }

    /// Create a new Promise object.
    pub fn promise() -> Self {
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            prototype: None,
            internal: ObjectInternal::Promise(PromiseState::new()),
        }
    }

    /// Create a new Set object.
    pub fn set() -> Self {
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            prototype: None,
            internal: ObjectInternal::Set(Vec::new()),
        }
    }

    /// Create a typed error object (TypeError, RangeError, etc.).
    pub fn typed_error(error_name: &str, message: impl Into<String>) -> Self {
        let msg: String = message.into();
        let mut properties = HashMap::new();
        properties.insert(
            "message".to_string(),
            Property::data(JsValue::string(msg.as_str())),
        );
        properties.insert(
            "name".to_string(),
            Property::data(JsValue::string(error_name)),
        );
        let display = format!("{}: {}", error_name, msg);
        JsObject {
            properties,
            symbol_properties: HashMap::new(),
            prototype: None,
            internal: ObjectInternal::Error(display),
        }
    }

    // -----------------------------------------------------------------
    // Type checks
    // -----------------------------------------------------------------

    pub fn is_function(&self) -> bool {
        matches!(self.internal, ObjectInternal::Function(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self.internal, ObjectInternal::Array(_))
    }

    pub fn is_error(&self) -> bool {
        matches!(self.internal, ObjectInternal::Error(_))
    }

    pub fn as_function(&self) -> Option<&FunctionObject> {
        match &self.internal {
            ObjectInternal::Function(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_function_mut(&mut self) -> Option<&mut FunctionObject> {
        match &mut self.internal {
            ObjectInternal::Function(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&Vec<JsValue>> {
        match &self.internal {
            ObjectInternal::Array(elems) => Some(elems),
            _ => None,
        }
    }

    pub fn as_array_mut(&mut self) -> Option<&mut Vec<JsValue>> {
        match &mut self.internal {
            ObjectInternal::Array(elems) => Some(elems),
            _ => None,
        }
    }

    // -----------------------------------------------------------------
    // Property access
    // -----------------------------------------------------------------

    /// Get a named property, walking the prototype chain.
    pub fn get_property(&self, name: &str) -> JsValue {
        // Own property first
        if let Some(prop) = self.properties.get(name) {
            return prop.value.clone();
        }
        // Array index access
        if let ObjectInternal::Array(ref elements) = self.internal {
            if let Ok(idx) = name.parse::<usize>() {
                if idx < elements.len() {
                    return elements[idx].clone();
                }
                return JsValue::Undefined;
            }
        }
        // Prototype chain
        if let Some(ref proto) = self.prototype {
            return proto.borrow().get_property(name);
        }
        JsValue::Undefined
    }

    /// Set a named property.
    pub fn set_property(&mut self, name: String, value: JsValue) {
        // Array index assignment
        if let ObjectInternal::Array(ref mut elements) = self.internal {
            if let Ok(idx) = name.parse::<usize>() {
                if idx >= elements.len() {
                    elements.resize(idx + 1, JsValue::Undefined);
                }
                elements[idx] = value.clone();
                self.properties.insert(
                    "length".to_string(),
                    Property::data(JsValue::Number(elements.len() as f64)),
                );
                return;
            }
        }
        self.properties.insert(name, Property::data(value));
    }

    /// Get a symbol-keyed property, walking the prototype chain.
    pub fn get_symbol_property(&self, symbol_id: u64) -> JsValue {
        if let Some(prop) = self.symbol_properties.get(&symbol_id) {
            return prop.value.clone();
        }
        if let Some(ref proto) = self.prototype {
            return proto.borrow().get_symbol_property(symbol_id);
        }
        JsValue::Undefined
    }

    /// Set a symbol-keyed property.
    pub fn set_symbol_property(&mut self, symbol_id: u64, value: JsValue) {
        self.symbol_properties
            .insert(symbol_id, Property::data(value));
    }

    /// Set a property with full descriptor control.
    pub fn define_property(&mut self, name: String, prop: Property) {
        self.properties.insert(name, prop);
    }

    /// Get an array element by index.
    pub fn get_index(&self, idx: u32) -> JsValue {
        if let ObjectInternal::Array(ref elements) = self.internal {
            if (idx as usize) < elements.len() {
                return elements[idx as usize].clone();
            }
        }
        // Fall through to named property
        self.get_property(&idx.to_string())
    }

    /// Set an array element by index.
    pub fn set_index(&mut self, idx: u32, value: JsValue) {
        if let ObjectInternal::Array(ref mut elements) = self.internal {
            let i = idx as usize;
            if i >= elements.len() {
                elements.resize(i + 1, JsValue::Undefined);
            }
            elements[i] = value;
            self.properties.insert(
                "length".to_string(),
                Property::data(JsValue::Number(elements.len() as f64)),
            );
            return;
        }
        // For non-arrays, store as named property.
        self.set_property(idx.to_string(), value);
    }

    /// Check if the object has a property (own or inherited).
    pub fn has_property(&self, name: &str) -> bool {
        if self.properties.contains_key(name) {
            return true;
        }
        // Array index
        if let ObjectInternal::Array(ref elements) = self.internal {
            if let Ok(idx) = name.parse::<usize>() {
                if idx < elements.len() {
                    return true;
                }
            }
        }
        // Prototype chain
        if let Some(ref proto) = self.prototype {
            return proto.borrow().has_property(name);
        }
        false
    }

    /// Check if the object has an own property (not inherited).
    pub fn has_own_property(&self, name: &str) -> bool {
        if self.properties.contains_key(name) {
            return true;
        }
        // Array index
        if let ObjectInternal::Array(ref elements) = self.internal {
            if let Ok(idx) = name.parse::<usize>() {
                return idx < elements.len();
            }
        }
        false
    }

    /// Delete a named property. Returns true if the property existed.
    pub fn delete_property(&mut self, name: &str) -> bool {
        if let Some(prop) = self.properties.get(name) {
            if !prop.configurable {
                return false;
            }
        }
        self.properties.remove(name).is_some()
    }

    /// Get all enumerable own property keys (strings).
    pub fn own_enumerable_keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = Vec::new();

        // Array indices first (in numeric order).
        if let ObjectInternal::Array(ref elements) = self.internal {
            for i in 0..elements.len() {
                keys.push(i.to_string());
            }
        }

        // Then named properties.
        let mut named: Vec<&String> = self
            .properties
            .iter()
            .filter(|(k, v)| v.enumerable && !(self.is_array() && k.as_str() == "length"))
            .map(|(k, _)| k)
            .collect();
        named.sort();
        for k in named {
            // Avoid duplicating array indices.
            if self.is_array() {
                if let Ok(_idx) = k.parse::<usize>() {
                    continue;
                }
            }
            keys.push(k.clone());
        }

        keys
    }

    /// Get all own property keys.
    pub fn own_keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = Vec::new();

        if let ObjectInternal::Array(ref elements) = self.internal {
            for i in 0..elements.len() {
                keys.push(i.to_string());
            }
        }

        let mut named: Vec<&String> = self.properties.keys().collect();
        named.sort();
        for k in named {
            if self.is_array() && k.parse::<usize>().is_ok() {
                continue;
            }
            keys.push(k.clone());
        }

        keys
    }

    /// Return the number of array elements, or 0 for non-arrays.
    pub fn array_length(&self) -> usize {
        match &self.internal {
            ObjectInternal::Array(elements) => elements.len(),
            _ => 0,
        }
    }

    /// Get an array element by index, or `Undefined` if out of bounds / not an array.
    pub fn get_element(&self, index: usize) -> JsValue {
        match &self.internal {
            ObjectInternal::Array(elements) => {
                elements.get(index).cloned().unwrap_or(JsValue::Undefined)
            }
            _ => JsValue::Undefined,
        }
    }
}

impl fmt::Display for JsObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.internal {
            ObjectInternal::Function(func) => {
                write!(f, "[Function: {}]", func.name)
            }
            ObjectInternal::Array(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            ObjectInternal::Error(msg) => {
                write!(f, "{}", msg)
            }
            ObjectInternal::Iterator(_) => {
                write!(f, "[object Iterator]")
            }
            ObjectInternal::Map(entries) => {
                write!(f, "Map({}) {{", entries.len())?;
                for (i, (k, v)) in entries.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, " {} => {}", k, v)?;
                }
                if !entries.is_empty() {
                    write!(f, " ")?;
                }
                write!(f, "}}")
            }
            ObjectInternal::Set(values) => {
                write!(f, "Set({}) {{", values.len())?;
                for (i, v) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, " {}", v)?;
                }
                if !values.is_empty() {
                    write!(f, " ")?;
                }
                write!(f, "}}")
            }
            ObjectInternal::Promise(state) => match state.status {
                PromiseStatus::Pending => write!(f, "Promise {{ <pending> }}"),
                PromiseStatus::Fulfilled => write!(f, "Promise {{ {} }}", state.value),
                PromiseStatus::Rejected => write!(f, "Promise {{ <rejected> {} }}", state.value),
            },
            ObjectInternal::Generator(state) => match state.status {
                GeneratorStatus::Completed => write!(f, "[object Generator] (completed)"),
                _ => write!(f, "[object Generator]"),
            },
            ObjectInternal::Ordinary => {
                write!(f, "[object Object]")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ordinary_object() {
        let mut obj = JsObject::ordinary();
        assert!(!obj.is_function());
        assert!(!obj.is_array());

        obj.set_property("x".to_string(), JsValue::Number(10.0));
        assert_eq!(obj.get_property("x"), JsValue::Number(10.0));
        assert!(obj.get_property("y").is_undefined());
    }

    #[test]
    fn test_array_object() {
        let arr = JsObject::array(vec![JsValue::Number(1.0), JsValue::Number(2.0)]);
        assert!(arr.is_array());
        assert_eq!(arr.get_property("0"), JsValue::Number(1.0));
        assert_eq!(arr.get_property("1"), JsValue::Number(2.0));
        assert_eq!(arr.get_property("length"), JsValue::Number(2.0));
        assert_eq!(arr.array_length(), 2);
    }

    #[test]
    fn test_array_index_access() {
        let arr = JsObject::array(vec![
            JsValue::string("a"),
            JsValue::string("b"),
            JsValue::string("c"),
        ]);
        assert_eq!(arr.get_index(0), JsValue::string("a"));
        assert_eq!(arr.get_index(1), JsValue::string("b"));
        assert_eq!(arr.get_index(2), JsValue::string("c"));
        assert!(arr.get_index(3).is_undefined());
    }

    #[test]
    fn test_array_set_index() {
        let mut arr = JsObject::array(vec![JsValue::Number(1.0)]);
        arr.set_index(2, JsValue::Number(3.0));
        assert_eq!(arr.array_length(), 3);
        assert!(arr.get_index(1).is_undefined());
        assert_eq!(arr.get_index(2), JsValue::Number(3.0));
    }

    #[test]
    fn test_function_object() {
        let func = JsObject::function(0, vec![], "test".to_string());
        assert!(func.is_function());
        assert_eq!(func.as_function().unwrap().name, "test");
    }

    #[test]
    fn test_native_function() {
        fn my_fn(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
            Ok(JsValue::Number(42.0))
        }
        let func = JsObject::native_function("myFn", my_fn);
        assert!(func.is_function());
        let fo = func.as_function().unwrap();
        assert_eq!(fo.name, "myFn");
        assert!(matches!(fo.kind, FunctionKind::Native(_)));
    }

    #[test]
    fn test_delete_property() {
        let mut obj = JsObject::ordinary();
        obj.set_property("x".to_string(), JsValue::Number(1.0));
        assert!(obj.delete_property("x"));
        assert!(!obj.delete_property("x"));
    }

    #[test]
    fn test_has_property() {
        let mut obj = JsObject::ordinary();
        obj.set_property("x".to_string(), JsValue::Number(1.0));
        assert!(obj.has_property("x"));
        assert!(!obj.has_property("y"));
    }

    #[test]
    fn test_has_own_property() {
        let mut obj = JsObject::ordinary();
        obj.set_property("x".to_string(), JsValue::Number(1.0));
        assert!(obj.has_own_property("x"));
        assert!(!obj.has_own_property("y"));
    }

    #[test]
    fn test_prototype_chain() {
        use crate::gc::GcPtr;

        let mut proto = JsObject::ordinary();
        proto.set_property("inherited".to_string(), JsValue::Number(99.0));
        let proto_ptr = GcPtr::new(proto);

        let child = JsObject::with_prototype(proto_ptr);
        assert_eq!(child.get_property("inherited"), JsValue::Number(99.0));
        assert!(child.has_property("inherited"));
        assert!(!child.has_own_property("inherited"));
    }

    #[test]
    fn test_error_object() {
        let err = JsObject::error("something went wrong");
        assert!(err.is_error());
        assert_eq!(
            err.get_property("message"),
            JsValue::string("something went wrong")
        );
    }

    #[test]
    fn test_typed_error() {
        let err = JsObject::typed_error("TypeError", "not a function");
        assert!(err.is_error());
        assert_eq!(err.get_property("name"), JsValue::string("TypeError"));
        assert_eq!(
            err.get_property("message"),
            JsValue::string("not a function")
        );
        assert_eq!(format!("{}", err), "TypeError: not a function");
    }

    #[test]
    fn test_property_key_from_str() {
        assert_eq!(PropertyKey::from_string("0"), PropertyKey::Index(0));
        assert_eq!(PropertyKey::from_string("42"), PropertyKey::Index(42));
        assert_eq!(
            PropertyKey::from_string("hello"),
            PropertyKey::String(Rc::from("hello"))
        );
        // "01" should remain a string (no round-trip)
        assert_eq!(
            PropertyKey::from_string("01"),
            PropertyKey::String(Rc::from("01"))
        );
    }

    #[test]
    fn test_own_enumerable_keys() {
        let mut obj = JsObject::ordinary();
        obj.set_property("b".to_string(), JsValue::Number(2.0));
        obj.set_property("a".to_string(), JsValue::Number(1.0));
        let keys = obj.own_enumerable_keys();
        assert_eq!(keys, vec!["a".to_string(), "b".to_string()]);
    }

    #[test]
    fn test_array_own_keys() {
        let arr = JsObject::array(vec![JsValue::Number(1.0), JsValue::Number(2.0)]);
        let keys = arr.own_enumerable_keys();
        assert_eq!(keys, vec!["0".to_string(), "1".to_string()]);
    }

    #[test]
    fn test_define_property() {
        let mut obj = JsObject::ordinary();
        obj.define_property("x".to_string(), Property::readonly(JsValue::Number(42.0)));
        assert_eq!(obj.get_property("x"), JsValue::Number(42.0));
        let prop = obj.properties.get("x").unwrap();
        assert!(!prop.writable);
        assert!(!prop.configurable);
    }

    #[test]
    fn test_display_array() {
        let arr = JsObject::array(vec![JsValue::Number(1.0), JsValue::Number(2.0)]);
        assert_eq!(format!("{}", arr), "[1, 2]");
    }

    #[test]
    fn test_display_function() {
        let func = JsObject::function(0, vec![], "myFunc".to_string());
        assert_eq!(format!("{}", func), "[Function: myFunc]");
    }

    #[test]
    fn test_display_ordinary() {
        let obj = JsObject::ordinary();
        assert_eq!(format!("{}", obj), "[object Object]");
    }
}
