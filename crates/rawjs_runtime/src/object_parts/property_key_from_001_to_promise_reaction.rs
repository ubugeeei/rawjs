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
    pub get: Option<JsValue>,
    pub set: Option<JsValue>,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}

impl Property {
    #[doc = " Create a default data property (writable, enumerable, configurable)."]
    pub fn data(value: JsValue) -> Self {
        Property {
            value,
            get: None,
            set: None,
            writable: true,
            enumerable: true,
            configurable: true,
        }
    }
}

impl Property {
    #[doc = " Create a non-enumerable data property (used for built-in methods)."]
    pub fn builtin(value: JsValue) -> Self {
        Property {
            value,
            get: None,
            set: None,
            writable: true,
            enumerable: false,
            configurable: true,
        }
    }
}

impl Property {
    #[doc = " Create a read-only property."]
    pub fn readonly(value: JsValue) -> Self {
        Property {
            value,
            get: None,
            set: None,
            writable: false,
            enumerable: true,
            configurable: false,
        }
    }
}

impl Property {
    #[doc = " Create a read-only, non-enumerable property."]
    pub fn readonly_builtin(value: JsValue) -> Self {
        Property {
            value,
            get: None,
            set: None,
            writable: false,
            enumerable: false,
            configurable: false,
        }
    }
}

impl Property {
    #[doc = " Create an accessor property."]
    pub fn accessor(
        get: Option<JsValue>,
        set: Option<JsValue>,
        enumerable: bool,
        configurable: bool,
    ) -> Self {
        Property {
            value: JsValue::Undefined,
            get,
            set,
            writable: false,
            enumerable,
            configurable,
        }
    }
}

impl Property {
    pub fn is_accessor(&self) -> bool {
        self.get.is_some() || self.set.is_some()
    }
}

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
