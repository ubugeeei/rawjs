impl fmt::Display for JsObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.internal {
            ObjectInternal::ArgumentsObject(_) => {
                write!(f, "[object Arguments]")
            }
            ObjectInternal::BooleanObject(value) => {
                write!(f, "{}", value)
            }
            ObjectInternal::Function(func) => {
                write!(f, "[Function: {}]", func.name)
            }
            ObjectInternal::StringObject(value) => {
                write!(f, "{}", value)
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
    fn test_non_extensible_object_rejects_new_property() {
        let mut obj = JsObject::ordinary();
        obj.extensible = false;
        assert!(!obj.try_set_property("x".to_string(), JsValue::Number(1.0)));
        assert!(obj.get_property("x").is_undefined());
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
