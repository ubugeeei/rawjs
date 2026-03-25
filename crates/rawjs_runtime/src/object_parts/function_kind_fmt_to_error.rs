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

/// A shared upvalue cell.  Cloning an `Upvalue` creates another handle
/// to the **same** underlying `RefCell`, so mutations are visible through
/// every handle (parent frame, child closures, etc.).
#[derive(Debug, Clone)]
pub struct Upvalue(pub Rc<RefCell<JsValue>>);

impl Upvalue {
    #[doc = " Create a new closed upvalue wrapping the given value."]
    pub fn new(value: JsValue) -> Self {
        Upvalue(Rc::new(RefCell::new(value)))
    }
}

impl Upvalue {
    #[doc = " Read the current value."]
    pub fn get(&self) -> JsValue {
        self.0.borrow().clone()
    }
}

impl Upvalue {
    #[doc = " Overwrite the value."]
    pub fn set(&self, value: JsValue) {
        *self.0.borrow_mut() = value;
    }
}

/// A JavaScript object: a bag of named properties plus optional internal state.
#[derive(Debug, Clone)]
pub struct JsObject {
    /// Named properties.
    pub properties: HashMap<String, Property>,
    /// Symbol-keyed properties (keyed by symbol ID).
    pub symbol_properties: HashMap<u64, Property>,
    /// Canonicalized runtime shape ID for property-layout sharing.
    pub shape_id: u64,
    /// Whether new own properties can be added.
    pub extensible: bool,
    /// Prototype chain link (null = end of chain).
    pub prototype: Option<GcPtr<JsObject>>,
    /// Internal / exotic object behaviour.
    pub internal: ObjectInternal,
}

impl JsObject {
    #[doc = " Create a new ordinary (plain) object."]
    pub fn ordinary() -> Self {
        let mut object = JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            shape_id: 0,
            extensible: true,
            prototype: None,
            internal: ObjectInternal::Ordinary,
        };
        object.refresh_shape();
        object
    }
}

impl JsObject {
    #[doc = " Create a new ordinary object with a prototype."]
    pub fn with_prototype(proto: GcPtr<JsObject>) -> Self {
        let mut object = JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            shape_id: 0,
            extensible: true,
            prototype: Some(proto),
            internal: ObjectInternal::Ordinary,
        };
        object.refresh_shape();
        object
    }
}

impl JsObject {
    #[doc = " Create a new function object backed by bytecode."]
    pub fn function(chunk_index: usize, upvalues: Vec<Upvalue>, name: String) -> Self {
        let mut properties = HashMap::new();
        properties.insert(
            "prototype".to_string(),
            Property::builtin(JsValue::Object(GcPtr::new(JsObject::ordinary()))),
        );
        let mut object = JsObject {
            properties,
            symbol_properties: HashMap::new(),
            shape_id: 0,
            extensible: true,
            prototype: None,
            internal: ObjectInternal::Function(FunctionObject {
                kind: FunctionKind::Bytecode { chunk_index },
                upvalues,
                name,
            }),
        };
        object.refresh_shape();
        object
    }
}

impl JsObject {
    #[doc = " Create a new native function object."]
    pub fn native_function(name: impl Into<String>, func: NativeFn) -> Self {
        let name = name.into();
        let mut properties = HashMap::new();
        properties.insert(
            "prototype".to_string(),
            Property::builtin(JsValue::Object(GcPtr::new(JsObject::ordinary()))),
        );
        let mut object = JsObject {
            properties,
            symbol_properties: HashMap::new(),
            shape_id: 0,
            extensible: true,
            prototype: None,
            internal: ObjectInternal::Function(FunctionObject {
                kind: FunctionKind::Native(func),
                upvalues: Vec::new(),
                name,
            }),
        };
        object.refresh_shape();
        object
    }
}

impl JsObject {
    #[doc = " Create a new array object from a list of elements."]
    pub fn array(elements: Vec<JsValue>) -> Self {
        let mut properties = HashMap::new();
        properties.insert(
            "length".to_string(),
            Property::data(JsValue::Number(elements.len() as f64)),
        );
        let mut object = JsObject {
            properties,
            symbol_properties: HashMap::new(),
            shape_id: 0,
            extensible: true,
            prototype: None,
            internal: ObjectInternal::Array(elements),
        };
        object.refresh_shape();
        object
    }
}

impl JsObject {
    #[doc = " Create a new error object."]
    pub fn error(message: impl Into<String>) -> Self {
        let msg: String = message.into();
        let mut properties = HashMap::new();
        properties.insert(
            "message".to_string(),
            Property::data(JsValue::string(msg.as_str())),
        );
        properties.insert("name".to_string(), Property::data(JsValue::string("Error")));
        let mut object = JsObject {
            properties,
            symbol_properties: HashMap::new(),
            shape_id: 0,
            extensible: true,
            prototype: None,
            internal: ObjectInternal::Error(msg),
        };
        object.refresh_shape();
        object
    }
}
