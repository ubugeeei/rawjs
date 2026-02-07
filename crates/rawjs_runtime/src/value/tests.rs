use super::*;

// -- to_boolean ---------------------------------------------------------

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

// -- to_number ----------------------------------------------------------

#[test]
fn test_to_number() {
    assert!(JsValue::Undefined.to_number().is_nan());
    assert_eq!(JsValue::Null.to_number(), 0.0);
    assert_eq!(JsValue::Boolean(true).to_number(), 1.0);
    assert_eq!(JsValue::Boolean(false).to_number(), 0.0);
    assert_eq!(JsValue::Number(42.0).to_number(), 42.0);
    assert_eq!(JsValue::string("3.14").to_number(), 3.14);
    assert_eq!(JsValue::string("  42  ").to_number(), 42.0);
    assert_eq!(JsValue::string("").to_number(), 0.0);
    assert!(JsValue::string("hello").to_number().is_nan());
    assert_eq!(JsValue::string("0xff").to_number(), 255.0);
    assert_eq!(JsValue::string("0b1010").to_number(), 10.0);
    assert_eq!(JsValue::string("0o17").to_number(), 15.0);
    assert_eq!(JsValue::string("Infinity").to_number(), f64::INFINITY);
    assert_eq!(JsValue::string("-Infinity").to_number(), f64::NEG_INFINITY);
}

// -- to_string_value ----------------------------------------------------

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
    assert_eq!(number_to_string(3.14), "3.14");
}

// -- type_of ------------------------------------------------------------

#[test]
fn test_type_of() {
    assert_eq!(JsValue::Undefined.type_of(), "undefined");
    assert_eq!(JsValue::Null.type_of(), "object");
    assert_eq!(JsValue::Boolean(true).type_of(), "boolean");
    assert_eq!(JsValue::Number(1.0).type_of(), "number");
    assert_eq!(JsValue::string("hi").type_of(), "string");
}

// -- strict_eq ----------------------------------------------------------

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

// -- abstract_eq --------------------------------------------------------

#[test]
fn test_abstract_eq() {
    assert!(JsValue::Undefined.abstract_eq(&JsValue::Null));
    assert!(JsValue::Null.abstract_eq(&JsValue::Undefined));
    assert!(JsValue::Number(1.0).abstract_eq(&JsValue::string("1")));
    assert!(JsValue::Boolean(true).abstract_eq(&JsValue::Number(1.0)));
    assert!(JsValue::Boolean(false).abstract_eq(&JsValue::Number(0.0)));
    assert!(JsValue::Number(42.0).abstract_eq(&JsValue::Number(42.0)));
}

// -- arithmetic ---------------------------------------------------------

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
    // String coercion
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

// -- bitwise ------------------------------------------------------------

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

// -- comparison ---------------------------------------------------------

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

// -- to_int32 -----------------------------------------------------------

#[test]
fn test_to_int32() {
    assert_eq!(JsValue::Number(1.5).to_int32(), 1);
    assert_eq!(JsValue::Number(-1.5).to_int32(), -1);
    assert_eq!(JsValue::Number(f64::NAN).to_int32(), 0);
    assert_eq!(JsValue::Number(f64::INFINITY).to_int32(), 0);
    assert_eq!(JsValue::Number(4294967296.0).to_int32(), 0);
}

// -- logical_not --------------------------------------------------------

#[test]
fn test_logical_not() {
    assert!(!JsValue::Boolean(true).logical_not().to_boolean());
    assert!(JsValue::Boolean(false).logical_not().to_boolean());
    assert!(JsValue::Number(0.0).logical_not().to_boolean());
    assert!(JsValue::string("").logical_not().to_boolean());
    assert!(!JsValue::string("a").logical_not().to_boolean());
}

// -- display ------------------------------------------------------------

#[test]
fn test_display() {
    assert_eq!(format!("{}", JsValue::Undefined), "undefined");
    assert_eq!(format!("{}", JsValue::Null), "null");
    assert_eq!(format!("{}", JsValue::Boolean(true)), "true");
    assert_eq!(format!("{}", JsValue::Number(42.0)), "42");
    assert_eq!(format!("{}", JsValue::string("hi")), "hi");
}

// -- is_ helpers --------------------------------------------------------

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
