#[test]
fn test_add_number_string_concat() {
    let a = JsValue::Number(1.0);
    let b = JsValue::string("2");
    assert_eq!(a.add(&b).to_string_value(), "12");
}

#[test]
fn test_sub() {
    assert_eq!(
        JsValue::Number(5.0).sub(&JsValue::Number(3.0)).to_number(),
        2.0
    );
    assert_eq!(
        JsValue::string("10").sub(&JsValue::Number(3.0)).to_number(),
        7.0
    );
}

#[test]
fn test_mul() {
    assert_eq!(
        JsValue::Number(4.0).mul(&JsValue::Number(3.0)).to_number(),
        12.0
    );
}

#[test]
fn test_div() {
    assert_eq!(
        JsValue::Number(10.0).div(&JsValue::Number(2.0)).to_number(),
        5.0
    );
    assert!(JsValue::Number(1.0)
        .div(&JsValue::Number(0.0))
        .to_number()
        .is_infinite());
}

#[test]
fn test_rem() {
    assert_eq!(
        JsValue::Number(7.0).rem(&JsValue::Number(3.0)).to_number(),
        1.0
    );
    assert!(JsValue::Number(7.0)
        .rem(&JsValue::Number(0.0))
        .to_number()
        .is_nan());
}

#[test]
fn test_exp() {
    assert_eq!(
        JsValue::Number(2.0).exp(&JsValue::Number(10.0)).to_number(),
        1024.0
    );
}

#[test]
fn test_neg() {
    assert_eq!(JsValue::Number(5.0).neg().to_number(), -5.0);
    assert_eq!(JsValue::Number(-3.0).neg().to_number(), 3.0);
}

#[test]
fn test_pos() {
    assert_eq!(JsValue::string("42").pos().to_number(), 42.0);
}

#[test]
fn test_increment_decrement() {
    assert_eq!(JsValue::Number(5.0).increment().to_number(), 6.0);
    assert_eq!(JsValue::Number(5.0).decrement().to_number(), 4.0);
}

#[test]
fn test_bitand() {
    assert_eq!(
        JsValue::Number(0b1100 as f64)
            .bitand(&JsValue::Number(0b1010 as f64))
            .to_number(),
        0b1000 as f64
    );
}

#[test]
fn test_bitor() {
    assert_eq!(
        JsValue::Number(0b1100 as f64)
            .bitor(&JsValue::Number(0b1010 as f64))
            .to_number(),
        0b1110 as f64
    );
}

#[test]
fn test_bitxor() {
    assert_eq!(
        JsValue::Number(0b1100 as f64)
            .bitxor(&JsValue::Number(0b1010 as f64))
            .to_number(),
        0b0110 as f64
    );
}

use super::*;
