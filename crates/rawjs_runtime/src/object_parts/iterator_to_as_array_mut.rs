impl JsObject {
    #[doc = " Create a new iterator object from a vector of values."]
    pub fn iterator(values: Vec<JsValue>) -> Self {
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            extensible: true,
            prototype: None,
            internal: ObjectInternal::Iterator(IteratorState::new(values)),
        }
    }
}

impl JsObject {
    #[doc = " Create a new Map object."]
    pub fn map() -> Self {
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            extensible: true,
            prototype: None,
            internal: ObjectInternal::Map(Vec::new()),
        }
    }
}

impl JsObject {
    #[doc = " Create a new Promise object."]
    pub fn promise() -> Self {
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            extensible: true,
            prototype: None,
            internal: ObjectInternal::Promise(PromiseState::new()),
        }
    }
}

impl JsObject {
    #[doc = " Create a new Set object."]
    pub fn set() -> Self {
        JsObject {
            properties: HashMap::new(),
            symbol_properties: HashMap::new(),
            extensible: true,
            prototype: None,
            internal: ObjectInternal::Set(Vec::new()),
        }
    }
}

impl JsObject {
    #[doc = " Create a typed error object (TypeError, RangeError, etc.)."]
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
            extensible: true,
            prototype: None,
            internal: ObjectInternal::Error(display),
        }
    }
}

impl JsObject {
    pub fn is_function(&self) -> bool {
        matches!(self.internal, ObjectInternal::Function(_))
    }
}

impl JsObject {
    pub fn is_array(&self) -> bool {
        matches!(self.internal, ObjectInternal::Array(_))
    }
}

impl JsObject {
    pub fn is_error(&self) -> bool {
        matches!(self.internal, ObjectInternal::Error(_))
    }
}

impl JsObject {
    pub fn as_function(&self) -> Option<&FunctionObject> {
        match &self.internal {
            ObjectInternal::Function(f) => Some(f),
            _ => None,
        }
    }
}

impl JsObject {
    pub fn as_function_mut(&mut self) -> Option<&mut FunctionObject> {
        match &mut self.internal {
            ObjectInternal::Function(f) => Some(f),
            _ => None,
        }
    }
}

impl JsObject {
    pub fn as_array(&self) -> Option<&Vec<JsValue>> {
        match &self.internal {
            ObjectInternal::Array(elems) => Some(elems),
            _ => None,
        }
    }
}

impl JsObject {
    pub fn as_array_mut(&mut self) -> Option<&mut Vec<JsValue>> {
        match &mut self.internal {
            ObjectInternal::Array(elems) => Some(elems),
            _ => None,
        }
    }
}
