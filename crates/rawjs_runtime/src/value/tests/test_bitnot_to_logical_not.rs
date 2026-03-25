#[test]
fn test_bitnot() {
    assert_eq!(JsValue::Number(0.0).bitnot().to_number(), -1.0);
    assert_eq!(JsValue::Number(-1.0).bitnot().to_number(), 0.0);
}

#[test]
fn test_shl() {
    assert_eq!(
        JsValue::Number(1.0).shl(&JsValue::Number(4.0)).to_number(),
        16.0
    );
}

#[test]
fn test_shr() {
    assert_eq!(
        JsValue::Number(16.0).shr(&JsValue::Number(2.0)).to_number(),
        4.0
    );
}

#[test]
fn test_ushr() {
    assert_eq!(
        JsValue::Number(-1.0)
            .ushr(&JsValue::Number(0.0))
            .to_number(),
        4294967295.0
    );
}

#[test]
fn test_lt() {
    assert!(JsValue::Number(1.0).lt(&JsValue::Number(2.0)).to_boolean());
    assert!(!JsValue::Number(2.0).lt(&JsValue::Number(1.0)).to_boolean());
    assert!(!JsValue::Number(1.0).lt(&JsValue::Number(1.0)).to_boolean());
}

#[test]
fn test_le() {
    assert!(JsValue::Number(1.0).le(&JsValue::Number(2.0)).to_boolean());
    assert!(JsValue::Number(1.0).le(&JsValue::Number(1.0)).to_boolean());
    assert!(!JsValue::Number(2.0).le(&JsValue::Number(1.0)).to_boolean());
}

#[test]
fn test_gt() {
    assert!(JsValue::Number(2.0).gt(&JsValue::Number(1.0)).to_boolean());
    assert!(!JsValue::Number(1.0).gt(&JsValue::Number(2.0)).to_boolean());
}

#[test]
fn test_ge() {
    assert!(JsValue::Number(2.0).ge(&JsValue::Number(1.0)).to_boolean());
    assert!(JsValue::Number(1.0).ge(&JsValue::Number(1.0)).to_boolean());
    assert!(!JsValue::Number(1.0).ge(&JsValue::Number(2.0)).to_boolean());
}

#[test]
fn test_nan_comparisons() {
    let nan = JsValue::nan();
    let one = JsValue::Number(1.0);
    assert!(!nan.lt(&one).to_boolean());
    assert!(!nan.le(&one).to_boolean());
    assert!(!nan.gt(&one).to_boolean());
    assert!(!nan.ge(&one).to_boolean());
}

#[test]
fn test_string_comparison() {
    let a = JsValue::string("abc");
    let b = JsValue::string("abd");
    assert!(a.lt(&b).to_boolean());
    assert!(!b.lt(&a).to_boolean());
}

#[test]
fn test_to_int32() {
    assert_eq!(JsValue::Number(1.5).to_int32(), 1);
    assert_eq!(JsValue::Number(-1.5).to_int32(), -1);
    assert_eq!(JsValue::Number(f64::NAN).to_int32(), 0);
    assert_eq!(JsValue::Number(f64::INFINITY).to_int32(), 0);
    assert_eq!(JsValue::Number(4294967296.0).to_int32(), 0);
}

#[test]
fn test_logical_not() {
    assert!(!JsValue::Boolean(true).logical_not().to_boolean());
    assert!(JsValue::Boolean(false).logical_not().to_boolean());
    assert!(JsValue::Number(0.0).logical_not().to_boolean());
    assert!(JsValue::string("").logical_not().to_boolean());
    assert!(!JsValue::string("a").logical_not().to_boolean());
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
