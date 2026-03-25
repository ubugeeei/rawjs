impl JsValue {
    #[doc = " `>`"]
    pub fn gt(&self, other: &JsValue) -> JsValue {
        match other.abstract_relational(self) {
            Some(true) => JsValue::Boolean(true),
            _ => JsValue::Boolean(false),
        }
    }
}

impl JsValue {
    #[doc = " `>=`"]
    pub fn ge(&self, other: &JsValue) -> JsValue {
        match self.abstract_relational(other) {
            Some(true) => JsValue::Boolean(false),
            None => JsValue::Boolean(false),
            Some(false) => JsValue::Boolean(true),
        }
    }
}

impl JsValue {
    #[doc = " `!` (logical NOT) -- always returns a boolean."]
    pub fn logical_not(&self) -> JsValue {
        JsValue::Boolean(!self.to_boolean())
    }
}

impl JsValue {
    #[doc = " Check if this value is an instance of the given constructor object."]
    #[doc = " Simplified: walks the prototype chain of `self` looking for"]
    #[doc = " `constructor.prototype`."]
    pub fn instance_of(&self, constructor: &JsValue) -> bool {
        let obj_ptr = match self {
            JsValue::Object(ptr) => ptr.clone(),
            _ => return false,
        };
        let ctor_ptr = match constructor {
            JsValue::Object(ptr) => ptr.clone(),
            _ => return false,
        };
        let ctor_proto_val = ctor_ptr.borrow().get_property("prototype");
        let ctor_proto = match &ctor_proto_val {
            JsValue::Object(ptr) => ptr.clone(),
            _ => return false,
        };
        let mut current = obj_ptr.borrow().prototype.clone();
        while let Some(proto) = current {
            if proto.ptr_eq(&ctor_proto) {
                return true;
            }
            current = proto.borrow().prototype.clone();
        }
        false
    }
}
