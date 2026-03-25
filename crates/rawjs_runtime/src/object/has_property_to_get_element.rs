impl JsObject {
    #[doc = " Check if the object has a property (own or inherited)."]
    pub fn has_property(&self, name: &str) -> bool {
        if self.properties.contains_key(name) {
            return true;
        }
        if let ObjectInternal::Array(ref elements) = self.internal {
            if let Ok(idx) = name.parse::<usize>() {
                if idx < elements.len() {
                    return true;
                }
            }
        }
        if let Some(ref proto) = self.prototype {
            return proto.borrow().has_property(name);
        }
        false
    }
}

impl JsObject {
    #[doc = " Check if the object has an own property (not inherited)."]
    pub fn has_own_property(&self, name: &str) -> bool {
        if self.properties.contains_key(name) {
            return true;
        }
        if let ObjectInternal::Array(ref elements) = self.internal {
            if let Ok(idx) = name.parse::<usize>() {
                return idx < elements.len();
            }
        }
        false
    }
}

impl JsObject {
    #[doc = " Delete a named property. Returns true if the property existed."]
    pub fn delete_property(&mut self, name: &str) -> bool {
        if let Some(prop) = self.properties.get(name) {
            if !prop.configurable {
                return false;
            }
        }
        let deleted = self.properties.remove(name).is_some();
        if deleted {
            self.refresh_shape();
        }
        deleted
    }
}

impl JsObject {
    #[doc = " Get all enumerable own property keys (strings)."]
    pub fn own_enumerable_keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = Vec::new();
        if let ObjectInternal::Array(ref elements) = self.internal {
            for i in 0..elements.len() {
                keys.push(i.to_string());
            }
        }
        let mut named: Vec<&String> = self
            .properties
            .iter()
            .filter(|(k, v)| v.enumerable && !(self.is_array() && k.as_str() == "length"))
            .map(|(k, _)| k)
            .collect();
        named.sort();
        for k in named {
            if self.is_array() {
                if let Ok(_idx) = k.parse::<usize>() {
                    continue;
                }
            }
            keys.push(k.clone());
        }
        keys
    }
}

impl JsObject {
    #[doc = " Get all own property keys."]
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
}

impl JsObject {
    #[doc = " Return the number of array elements, or 0 for non-arrays."]
    pub fn array_length(&self) -> usize {
        match &self.internal {
            ObjectInternal::Array(elements) => elements.len(),
            _ => 0,
        }
    }
}

impl JsObject {
    #[doc = " Get an array element by index, or `Undefined` if out of bounds / not an array."]
    pub fn get_element(&self, index: usize) -> JsValue {
        match &self.internal {
            ObjectInternal::Array(elements) => {
                elements.get(index).cloned().unwrap_or(JsValue::Undefined)
            }
            _ => JsValue::Undefined,
        }
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
