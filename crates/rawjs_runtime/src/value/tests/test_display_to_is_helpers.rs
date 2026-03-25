#[test]
fn test_display() {
    assert_eq!(format!("{}", JsValue::Undefined), "undefined");
    assert_eq!(format!("{}", JsValue::Null), "null");
    assert_eq!(format!("{}", JsValue::Boolean(true)), "true");
    assert_eq!(format!("{}", JsValue::Number(42.0)), "42");
    assert_eq!(format!("{}", JsValue::string("hi")), "hi");
}

#[test]
fn test_is_helpers() {
    assert!(JsValue::Undefined.is_undefined());
    assert!(JsValue::Null.is_null());
    assert!(JsValue::Null.is_nullish());
    assert!(JsValue::Undefined.is_nullish());
    assert!(!JsValue::Number(0.0).is_nullish());
    assert!(JsValue::Number(1.0).is_number());
    assert!(JsValue::string("x").is_string());
    assert!(JsValue::Boolean(true).is_boolean());
    assert!(JsValue::Number(f64::NAN).is_nan());
    assert!(!JsValue::Number(1.0).is_nan());
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
