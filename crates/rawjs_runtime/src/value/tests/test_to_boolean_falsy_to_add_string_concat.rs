use super::*;

#[test]
fn test_to_boolean_falsy() {
    assert!(!JsValue::Undefined.to_boolean());
    assert!(!JsValue::Null.to_boolean());
    assert!(!JsValue::Boolean(false).to_boolean());
    assert!(!JsValue::Number(0.0).to_boolean());
    assert!(!JsValue::Number(f64::NAN).to_boolean());
    assert!(!JsValue::string("").to_boolean());
}

#[test]
fn test_to_boolean_truthy() {
    assert!(JsValue::Boolean(true).to_boolean());
    assert!(JsValue::Number(1.0).to_boolean());
    assert!(JsValue::Number(-1.0).to_boolean());
    assert!(JsValue::string("hello").to_boolean());
}

#[test]
fn test_to_number() {
    assert!(JsValue::Undefined.to_number().is_nan());
    assert_eq!(JsValue::Null.to_number(), 0.0);
    assert_eq!(JsValue::Boolean(true).to_number(), 1.0);
    assert_eq!(JsValue::Boolean(false).to_number(), 0.0);
    assert_eq!(JsValue::Number(42.0).to_number(), 42.0);
    assert_eq!(JsValue::string("2.5").to_number(), 2.5);
    assert_eq!(JsValue::string("  42  ").to_number(), 42.0);
    assert_eq!(JsValue::string("").to_number(), 0.0);
    assert!(JsValue::string("hello").to_number().is_nan());
    assert_eq!(JsValue::string("0xff").to_number(), 255.0);
    assert_eq!(JsValue::string("0b1010").to_number(), 10.0);
    assert_eq!(JsValue::string("0o17").to_number(), 15.0);
    assert_eq!(JsValue::string("Infinity").to_number(), f64::INFINITY);
    assert_eq!(JsValue::string("-Infinity").to_number(), f64::NEG_INFINITY);
}

#[test]
fn test_to_string_value() {
    assert_eq!(JsValue::Undefined.to_string_value(), "undefined");
    assert_eq!(JsValue::Null.to_string_value(), "null");
    assert_eq!(JsValue::Boolean(true).to_string_value(), "true");
    assert_eq!(JsValue::Boolean(false).to_string_value(), "false");
    assert_eq!(JsValue::Number(42.0).to_string_value(), "42");
    assert_eq!(JsValue::Number(0.0).to_string_value(), "0");
    assert_eq!(JsValue::Number(f64::NAN).to_string_value(), "NaN");
    assert_eq!(JsValue::Number(f64::INFINITY).to_string_value(), "Infinity");
    assert_eq!(JsValue::string("hello").to_string_value(), "hello");
}

#[test]
fn test_number_to_string_integer() {
    assert_eq!(number_to_string(1.0), "1");
    assert_eq!(number_to_string(-5.0), "-5");
    assert_eq!(number_to_string(100.0), "100");
}

#[test]
fn test_number_to_string_float() {
    assert_eq!(number_to_string(2.5), "2.5");
}

#[test]
fn test_type_of() {
    assert_eq!(JsValue::Undefined.type_of(), "undefined");
    assert_eq!(JsValue::Null.type_of(), "object");
    assert_eq!(JsValue::Boolean(true).type_of(), "boolean");
    assert_eq!(JsValue::Number(1.0).type_of(), "number");
    assert_eq!(JsValue::string("hi").type_of(), "string");
}

#[test]
fn test_strict_eq() {
    assert!(JsValue::Undefined.strict_eq(&JsValue::Undefined));
    assert!(JsValue::Null.strict_eq(&JsValue::Null));
    assert!(!JsValue::Undefined.strict_eq(&JsValue::Null));
    assert!(JsValue::Number(1.0).strict_eq(&JsValue::Number(1.0)));
    assert!(!JsValue::Number(1.0).strict_eq(&JsValue::Number(2.0)));
    assert!(!JsValue::Number(f64::NAN).strict_eq(&JsValue::Number(f64::NAN)));
    assert!(JsValue::string("a").strict_eq(&JsValue::string("a")));
    assert!(!JsValue::string("a").strict_eq(&JsValue::string("b")));
}

#[test]
fn test_abstract_eq() {
    assert!(JsValue::Undefined.abstract_eq(&JsValue::Null));
    assert!(JsValue::Null.abstract_eq(&JsValue::Undefined));
    assert!(JsValue::Number(1.0).abstract_eq(&JsValue::string("1")));
    assert!(JsValue::Boolean(true).abstract_eq(&JsValue::Number(1.0)));
    assert!(JsValue::Boolean(false).abstract_eq(&JsValue::Number(0.0)));
    assert!(JsValue::Number(42.0).abstract_eq(&JsValue::Number(42.0)));
}

#[test]
fn test_add_numbers() {
    let a = JsValue::Number(2.0);
    let b = JsValue::Number(3.0);
    assert_eq!(a.add(&b).to_number(), 5.0);
}

#[test]
fn test_add_string_concat() {
    let a = JsValue::string("hello");
    let b = JsValue::string(" world");
    assert_eq!(a.add(&b).to_string_value(), "hello world");
}
