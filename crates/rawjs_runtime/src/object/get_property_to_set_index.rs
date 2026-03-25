impl JsObject {
    #[doc = " Get a named property, walking the prototype chain."]
    pub fn get_property(&self, name: &str) -> JsValue {
        if let Some(prop) = self.properties.get(name) {
            return prop.value.clone();
        }
        if let ObjectInternal::StringObject(ref value) = self.internal {
            match name {
                "length" => return JsValue::Number(value.chars().count() as f64),
                _ => {
                    if let Ok(idx) = name.parse::<usize>() {
                        if let Some(ch) = value.chars().nth(idx) {
                            return JsValue::string(ch.to_string());
                        }
                    }
                }
            }
        }
        if let ObjectInternal::Array(ref elements) = self.internal {
            if let Ok(idx) = name.parse::<usize>() {
                if idx < elements.len() {
                    return elements[idx].clone();
                }
                return JsValue::Undefined;
            }
        }
        if let Some(ref proto) = self.prototype {
            return proto.borrow().get_property(name);
        }
        JsValue::Undefined
    }
}

impl JsObject {
    #[doc = " Set a named property."]
    pub fn set_property(&mut self, name: String, value: JsValue) {
        let _ = self.try_set_property(name, value);
    }
}

impl JsObject {
    #[doc = " Attempt to set a named property. Returns `false` if the write is rejected."]
    pub fn try_set_property(&mut self, name: String, value: JsValue) -> bool {
        if let Some(existing) = self.properties.get(&name) {
            if !existing.writable {
                return false;
            }
        }
        if let ObjectInternal::Array(ref mut elements) = self.internal {
            if let Ok(idx) = name.parse::<usize>() {
                if idx >= elements.len() && !self.extensible {
                    return false;
                }
                if idx >= elements.len() {
                    elements.resize(idx + 1, JsValue::Undefined);
                }
                elements[idx] = value.clone();
                self.properties.insert(
                    "length".to_string(),
                    Property::data(JsValue::Number(elements.len() as f64)),
                );
                return true;
            }
        }
        if !self.extensible && !self.properties.contains_key(&name) {
            return false;
        }
        let is_new = !self.properties.contains_key(&name);
        self.properties.insert(name, Property::data(value));
        if is_new {
            self.refresh_shape();
        }
        true
    }
}

impl JsObject {
    #[doc = " Get a symbol-keyed property, walking the prototype chain."]
    pub fn get_symbol_property(&self, symbol_id: u64) -> JsValue {
        if let Some(prop) = self.symbol_properties.get(&symbol_id) {
            return prop.value.clone();
        }
        if let Some(ref proto) = self.prototype {
            return proto.borrow().get_symbol_property(symbol_id);
        }
        JsValue::Undefined
    }
}

impl JsObject {
    #[doc = " Set a symbol-keyed property."]
    pub fn set_symbol_property(&mut self, symbol_id: u64, value: JsValue) {
        let is_new = !self.symbol_properties.contains_key(&symbol_id);
        self.symbol_properties
            .insert(symbol_id, Property::data(value));
        if is_new {
            self.refresh_shape();
        }
    }
}

impl JsObject {
    #[doc = " Set a property with full descriptor control."]
    pub fn define_property(&mut self, name: String, prop: Property) {
        let is_new = !self.properties.contains_key(&name);
        self.properties.insert(name, prop);
        if is_new {
            self.refresh_shape();
        }
    }
}

impl JsObject {
    #[doc = " Get an own property descriptor, if any."]
    pub fn get_own_property_descriptor(&self, name: &str) -> Option<Property> {
        if let Some(prop) = self.properties.get(name) {
            return Some(prop.clone());
        }
        if let ObjectInternal::Array(ref elements) = self.internal {
            if let Ok(idx) = name.parse::<usize>() {
                if idx < elements.len() {
                    return Some(Property::data(elements[idx].clone()));
                }
            }
        }
        None
    }
}

impl JsObject {
    #[doc = " Get a property descriptor, walking the prototype chain."]
    pub fn get_property_descriptor(&self, name: &str) -> Option<Property> {
        if let Some(prop) = self.properties.get(name) {
            return Some(prop.clone());
        }
        if let Some(ref proto) = self.prototype {
            return proto.borrow().get_property_descriptor(name);
        }
        None
    }
}

impl JsObject {
    #[doc = " Get an array element by index."]
    pub fn get_index(&self, idx: u32) -> JsValue {
        if let ObjectInternal::Array(ref elements) = self.internal {
            if (idx as usize) < elements.len() {
                return elements[idx as usize].clone();
            }
        }
        self.get_property(&idx.to_string())
    }
}

impl JsObject {
    #[doc = " Set an array element by index."]
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
        self.set_property(idx.to_string(), value);
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
