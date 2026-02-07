mod conversions;
mod ops;

use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::gc::GcPtr;
use crate::object::JsObject;

// ---------------------------------------------------------------------------
// Symbol
// ---------------------------------------------------------------------------

/// A unique Symbol identifier.
static NEXT_SYMBOL_ID: AtomicU64 = AtomicU64::new(100); // IDs 0-99 reserved for well-known symbols

/// Well-known symbol IDs.
pub const SYMBOL_ITERATOR: u64 = 1;
pub const SYMBOL_TO_PRIMITIVE: u64 = 2;
pub const SYMBOL_HAS_INSTANCE: u64 = 3;
pub const SYMBOL_TO_STRING_TAG: u64 = 4;
pub const SYMBOL_SPECIES: u64 = 5;
pub const SYMBOL_DISPOSE: u64 = 6;
pub const SYMBOL_ASYNC_DISPOSE: u64 = 7;

/// A JavaScript Symbol value.
#[derive(Debug, Clone)]
pub struct JsSymbol {
    pub id: u64,
    pub description: Option<Rc<str>>,
}

impl JsSymbol {
    /// Create a new unique symbol with an optional description.
    pub fn new(description: Option<&str>) -> Self {
        let id = NEXT_SYMBOL_ID.fetch_add(1, Ordering::Relaxed);
        JsSymbol {
            id,
            description: description.map(Rc::from),
        }
    }

    /// Create a well-known symbol with a fixed ID.
    pub fn well_known(id: u64, description: &str) -> Self {
        JsSymbol {
            id,
            description: Some(Rc::from(description)),
        }
    }
}

impl PartialEq for JsSymbol {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for JsSymbol {}

impl std::hash::Hash for JsSymbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

/// A JavaScript value.
///
/// This represents any value that can exist on the VM's value stack or be
/// stored in variables.  Primitive types are stored inline; heap-allocated
/// objects are behind a `GcPtr`.
#[derive(Debug, Clone)]
pub enum JsValue {
    Undefined,
    Null,
    Boolean(bool),
    Number(f64),
    String(Rc<str>),
    Symbol(JsSymbol),
    Object(GcPtr<JsObject>),
}

// ---------------------------------------------------------------------------
// Construction helpers
// ---------------------------------------------------------------------------

impl JsValue {
    /// Shorthand for creating a heap-allocated JS string value.
    pub fn string(s: impl Into<Rc<str>>) -> Self {
        JsValue::String(s.into())
    }

    /// Shorthand for creating a JS number value.
    pub fn number(n: f64) -> Self {
        JsValue::Number(n)
    }

    /// Shorthand for creating a JS boolean value.
    pub fn boolean(b: bool) -> Self {
        JsValue::Boolean(b)
    }

    /// Shorthand for creating a Symbol value.
    pub fn symbol(description: Option<&str>) -> Self {
        JsValue::Symbol(JsSymbol::new(description))
    }

    /// Shorthand for wrapping a `GcPtr<JsObject>` in a `JsValue`.
    pub fn object(obj: GcPtr<JsObject>) -> Self {
        JsValue::Object(obj)
    }

    /// The JS value `NaN`.
    pub fn nan() -> Self {
        JsValue::Number(f64::NAN)
    }

    /// The JS value `+Infinity`.
    pub fn infinity() -> Self {
        JsValue::Number(f64::INFINITY)
    }

    /// The JS value `-Infinity`.
    pub fn neg_infinity() -> Self {
        JsValue::Number(f64::NEG_INFINITY)
    }

    /// The JS value `0`.
    pub fn zero() -> Self {
        JsValue::Number(0.0)
    }
}

// ---------------------------------------------------------------------------
// Type checks
// ---------------------------------------------------------------------------

impl JsValue {
    pub fn is_undefined(&self) -> bool {
        matches!(self, JsValue::Undefined)
    }

    pub fn is_null(&self) -> bool {
        matches!(self, JsValue::Null)
    }

    pub fn is_nullish(&self) -> bool {
        matches!(self, JsValue::Undefined | JsValue::Null)
    }

    pub fn is_number(&self) -> bool {
        matches!(self, JsValue::Number(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, JsValue::String(_))
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, JsValue::Boolean(_))
    }

    pub fn is_symbol(&self) -> bool {
        matches!(self, JsValue::Symbol(_))
    }

    pub fn is_object(&self) -> bool {
        matches!(self, JsValue::Object(_))
    }

    pub fn is_function(&self) -> bool {
        match self {
            JsValue::Object(ptr) => ptr.borrow().is_function(),
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            JsValue::Object(ptr) => ptr.borrow().is_array(),
            _ => false,
        }
    }

    pub fn is_nan(&self) -> bool {
        matches!(self, JsValue::Number(n) if n.is_nan())
    }
}

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

impl fmt::Display for JsValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsValue::Undefined => write!(f, "undefined"),
            JsValue::Null => write!(f, "null"),
            JsValue::Boolean(b) => write!(f, "{}", b),
            JsValue::Number(n) => write!(f, "{}", number_to_string(*n)),
            JsValue::String(s) => write!(f, "{}", s),
            JsValue::Symbol(sym) => {
                if let Some(ref desc) = sym.description {
                    write!(f, "Symbol({})", desc)
                } else {
                    write!(f, "Symbol()")
                }
            }
            JsValue::Object(ptr) => {
                let obj = ptr.borrow();
                write!(f, "{}", *obj)
            }
        }
    }
}

// ---------------------------------------------------------------------------
// PartialEq -- uses strict equality semantics (===).
// ---------------------------------------------------------------------------

impl PartialEq for JsValue {
    fn eq(&self, other: &Self) -> bool {
        self.strict_eq(other)
    }
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

/// Convert a JS string to a number following the ES specification rules.
pub(crate) fn string_to_number(s: &str) -> f64 {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return 0.0;
    }

    // Hex literal
    if trimmed.starts_with("0x") || trimmed.starts_with("0X") {
        return match i64::from_str_radix(&trimmed[2..], 16) {
            Ok(n) => n as f64,
            Err(_) => f64::NAN,
        };
    }

    // Octal literal
    if trimmed.starts_with("0o") || trimmed.starts_with("0O") {
        return match i64::from_str_radix(&trimmed[2..], 8) {
            Ok(n) => n as f64,
            Err(_) => f64::NAN,
        };
    }

    // Binary literal
    if trimmed.starts_with("0b") || trimmed.starts_with("0B") {
        return match i64::from_str_radix(&trimmed[2..], 2) {
            Ok(n) => n as f64,
            Err(_) => f64::NAN,
        };
    }

    // Infinity
    if trimmed == "Infinity" || trimmed == "+Infinity" {
        return f64::INFINITY;
    }
    if trimmed == "-Infinity" {
        return f64::NEG_INFINITY;
    }

    // Standard decimal
    trimmed.parse::<f64>().unwrap_or(f64::NAN)
}

/// Convert a number to its JS string representation.
pub fn number_to_string(n: f64) -> String {
    if n.is_nan() {
        return "NaN".to_string();
    }
    if n.is_infinite() {
        return if n.is_sign_positive() {
            "Infinity".to_string()
        } else {
            "-Infinity".to_string()
        };
    }
    if n == 0.0 {
        // Both +0 and -0 produce "0".
        return "0".to_string();
    }
    // If the number is an integer that fits in i64, print without decimal.
    if n.fract() == 0.0 && n.abs() < (i64::MAX as f64) {
        return format!("{}", n as i64);
    }
    format!("{}", n)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests;
