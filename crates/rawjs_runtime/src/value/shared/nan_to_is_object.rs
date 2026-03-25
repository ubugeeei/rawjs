impl JsValue {
    #[doc = " The JS value `NaN`."]
    pub fn nan() -> Self {
        JsValue::Number(f64::NAN)
    }
}

impl JsValue {
    #[doc = " The JS value `+Infinity`."]
    pub fn infinity() -> Self {
        JsValue::Number(f64::INFINITY)
    }
}

impl JsValue {
    #[doc = " The JS value `-Infinity`."]
    pub fn neg_infinity() -> Self {
        JsValue::Number(f64::NEG_INFINITY)
    }
}

impl JsValue {
    #[doc = " The JS value `0`."]
    pub fn zero() -> Self {
        JsValue::Number(0.0)
    }
}

impl JsValue {
    pub fn is_undefined(&self) -> bool {
        matches!(self, JsValue::Undefined)
    }
}

impl JsValue {
    pub fn is_null(&self) -> bool {
        matches!(self, JsValue::Null)
    }
}

impl JsValue {
    pub fn is_nullish(&self) -> bool {
        matches!(self, JsValue::Undefined | JsValue::Null)
    }
}

impl JsValue {
    pub fn is_number(&self) -> bool {
        matches!(self, JsValue::Number(_))
    }
}

impl JsValue {
    pub fn is_string(&self) -> bool {
        matches!(self, JsValue::String(_))
    }
}

impl JsValue {
    pub fn is_boolean(&self) -> bool {
        matches!(self, JsValue::Boolean(_))
    }
}

impl JsValue {
    pub fn is_symbol(&self) -> bool {
        matches!(self, JsValue::Symbol(_))
    }
}

impl JsValue {
    pub fn is_object(&self) -> bool {
        matches!(self, JsValue::Object(_))
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
