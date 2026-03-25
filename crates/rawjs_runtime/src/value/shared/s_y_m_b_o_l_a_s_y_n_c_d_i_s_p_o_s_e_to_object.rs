pub const SYMBOL_ASYNC_DISPOSE: u64 = 7;

/// A JavaScript Symbol value.
#[derive(Debug, Clone)]
pub struct JsSymbol {
    pub id: u64,
    pub description: Option<Rc<str>>,
}

impl JsSymbol {
    #[doc = " Create a new unique symbol with an optional description."]
    pub fn new(description: Option<&str>) -> Self {
        let id = NEXT_SYMBOL_ID.fetch_add(1, Ordering::Relaxed);
        JsSymbol {
            id,
            description: description.map(Rc::from),
        }
    }
}

impl JsSymbol {
    #[doc = " Create a well-known symbol with a fixed ID."]
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
#[repr(C, u8)]
pub enum JsValue {
    Undefined = 0,
    Null = 1,
    Boolean(bool) = 2,
    Number(f64) = 3,
    String(Rc<str>) = 4,
    Symbol(JsSymbol) = 5,
    Object(GcPtr<JsObject>) = 6,
}

impl JsValue {
    #[doc = " Shorthand for creating a heap-allocated JS string value."]
    pub fn string(s: impl Into<Rc<str>>) -> Self {
        JsValue::String(s.into())
    }
}

impl JsValue {
    #[doc = " Shorthand for creating a JS number value."]
    pub fn number(n: f64) -> Self {
        JsValue::Number(n)
    }
}

impl JsValue {
    #[doc = " Shorthand for creating a JS boolean value."]
    pub fn boolean(b: bool) -> Self {
        JsValue::Boolean(b)
    }
}

impl JsValue {
    #[doc = " Shorthand for creating a Symbol value."]
    pub fn symbol(description: Option<&str>) -> Self {
        JsValue::Symbol(JsSymbol::new(description))
    }
}

impl JsValue {
    #[doc = " Shorthand for wrapping a `GcPtr<JsObject>` in a `JsValue`."]
    pub fn object(obj: GcPtr<JsObject>) -> Self {
        JsValue::Object(obj)
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
