use std::sync::{Mutex, OnceLock};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ShapeKey {
    prototype_addr: usize,
    property_names: Vec<String>,
    symbol_ids: Vec<u64>,
}

#[derive(Debug, Default)]
struct ShapeRegistry {
    next_id: u64,
    shapes: HashMap<ShapeKey, u64>,
}

impl ShapeRegistry {
    fn intern(&mut self, key: ShapeKey) -> u64 {
        if let Some(id) = self.shapes.get(&key) {
            return *id;
        }
        self.next_id += 1;
        let id = self.next_id;
        self.shapes.insert(key, id);
        id
    }
}

fn shape_registry() -> &'static Mutex<ShapeRegistry> {
    static REGISTRY: OnceLock<Mutex<ShapeRegistry>> = OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(ShapeRegistry::default()))
}

impl JsObject {
    fn current_shape_key(&self) -> ShapeKey {
        let mut property_names: Vec<String> = self.properties.keys().cloned().collect();
        property_names.sort();
        let mut symbol_ids: Vec<u64> = self.symbol_properties.keys().copied().collect();
        symbol_ids.sort();
        ShapeKey {
            prototype_addr: self.prototype.as_ref().map_or(0, GcPtr::addr),
            property_names,
            symbol_ids,
        }
    }
}

impl JsObject {
    pub(crate) fn refresh_shape(&mut self) {
        let key = self.current_shape_key();
        let mut registry = shape_registry().lock().unwrap();
        self.shape_id = registry.intern(key);
    }
}

impl JsObject {
    pub fn shape_id(&self) -> u64 {
        self.shape_id
    }
}

impl JsObject {
    pub fn shares_shape_with(&self, other: &JsObject) -> bool {
        self.shape_id == other.shape_id
    }
}

impl JsObject {
    pub fn set_prototype(&mut self, prototype: Option<GcPtr<JsObject>>) {
        self.prototype = prototype;
        self.refresh_shape();
    }
}

#[cfg(test)]
mod shape_tests {
    use super::*;

    #[test]
    fn test_same_layout_objects_share_shape() {
        let mut left = JsObject::ordinary();
        left.set_property("x".to_string(), JsValue::Number(1.0));
        left.set_property("y".to_string(), JsValue::Number(2.0));

        let mut right = JsObject::ordinary();
        right.set_property("x".to_string(), JsValue::Number(3.0));
        right.set_property("y".to_string(), JsValue::Number(4.0));

        assert!(left.shares_shape_with(&right));
    }

    #[test]
    fn test_property_add_changes_shape() {
        let mut object = JsObject::ordinary();
        let initial_shape = object.shape_id();
        object.set_property("x".to_string(), JsValue::Number(1.0));
        assert_ne!(object.shape_id(), initial_shape);
    }

    #[test]
    fn test_delete_restores_canonical_shape() {
        let mut object = JsObject::ordinary();
        let initial_shape = object.shape_id();
        object.set_property("x".to_string(), JsValue::Number(1.0));
        assert!(object.delete_property("x"));
        assert_eq!(object.shape_id(), initial_shape);
    }

    #[test]
    fn test_prototype_participates_in_shape() {
        let proto = GcPtr::new(JsObject::ordinary());

        let left = JsObject::ordinary();
        let mut right = JsObject::ordinary();
        right.set_prototype(Some(proto));

        assert_ne!(left.shape_id(), right.shape_id());
    }
}

use super::*;
