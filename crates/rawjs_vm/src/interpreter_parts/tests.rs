#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_js_add_numbers() {
        let result = js_add(&JsValue::Number(1.0), &JsValue::Number(2.0));
        assert_eq!(result, JsValue::Number(3.0));
    }
    #[test]
    fn test_js_add_strings() {
        let result = js_add(&JsValue::string("foo"), &JsValue::string("bar"));
        assert_eq!(result, JsValue::string("foobar"));
    }
    #[test]
    fn test_js_add_string_number() {
        let result = js_add(&JsValue::string("x"), &JsValue::Number(1.0));
        assert_eq!(result, JsValue::string("x1"));
    }
    #[test]
    fn test_js_add_number_string() {
        let result = js_add(&JsValue::Number(1.0), &JsValue::string("x"));
        assert_eq!(result, JsValue::string("1x"));
    }
    #[test]
    fn test_constant_to_value() {
        assert_eq!(
            constant_to_value(&Constant::Number(2.5)),
            JsValue::Number(2.5)
        );
        assert_eq!(
            constant_to_value(&Constant::String("hi".into())),
            JsValue::string("hi")
        );
        assert_eq!(
            constant_to_value(&Constant::Boolean(true)),
            JsValue::Boolean(true)
        );
        assert_eq!(constant_to_value(&Constant::Null), JsValue::Null);
        assert_eq!(constant_to_value(&Constant::Undefined), JsValue::Undefined);
    }
    #[test]
    fn test_get_constant_string_ok() {
        let mut chunk = Chunk::new("test");
        chunk.add_constant(Constant::String("hello".into()));
        assert_eq!(get_constant_string(&chunk, 0).unwrap(), "hello");
    }
    #[test]
    fn test_get_constant_string_err() {
        let mut chunk = Chunk::new("test");
        chunk.add_constant(Constant::Number(1.0));
        assert!(get_constant_string(&chunk, 0).is_err());
    }
}
