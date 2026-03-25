impl JsValue {
    #[doc = " Pre/post increment helper: returns the new value."]
    pub fn increment(&self) -> JsValue {
        JsValue::Number(self.to_number() + 1.0)
    }
}

impl JsValue {
    #[doc = " Pre/post decrement helper: returns the new value."]
    pub fn decrement(&self) -> JsValue {
        JsValue::Number(self.to_number() - 1.0)
    }
}

impl JsValue {
    #[doc = " `&` (bitwise AND)."]
    pub fn bitand(&self, other: &JsValue) -> JsValue {
        JsValue::Number((self.to_int32() & other.to_int32()) as f64)
    }
}

impl JsValue {
    #[doc = " `|` (bitwise OR)."]
    pub fn bitor(&self, other: &JsValue) -> JsValue {
        JsValue::Number((self.to_int32() | other.to_int32()) as f64)
    }
}

impl JsValue {
    #[doc = " `^` (bitwise XOR)."]
    pub fn bitxor(&self, other: &JsValue) -> JsValue {
        JsValue::Number((self.to_int32() ^ other.to_int32()) as f64)
    }
}

impl JsValue {
    #[doc = " `~` (bitwise NOT)."]
    pub fn bitnot(&self) -> JsValue {
        JsValue::Number((!self.to_int32()) as f64)
    }
}

impl JsValue {
    #[doc = " `<<` (left shift)."]
    pub fn shl(&self, other: &JsValue) -> JsValue {
        let lhs = self.to_int32();
        let rhs = other.to_uint32() & 0x1f;
        JsValue::Number((lhs << rhs) as f64)
    }
}

impl JsValue {
    #[doc = " `>>` (signed right shift)."]
    pub fn shr(&self, other: &JsValue) -> JsValue {
        let lhs = self.to_int32();
        let rhs = other.to_uint32() & 0x1f;
        JsValue::Number((lhs >> rhs) as f64)
    }
}

impl JsValue {
    #[doc = " `>>>` (unsigned right shift)."]
    pub fn ushr(&self, other: &JsValue) -> JsValue {
        let lhs = self.to_uint32();
        let rhs = other.to_uint32() & 0x1f;
        JsValue::Number((lhs >> rhs) as f64)
    }
}

impl JsValue {
    #[doc = " ES Abstract Relational Comparison."]
    #[doc = " Returns `None` if result is **undefined** (i.e. NaN involved)."]
    pub(super) fn abstract_relational(&self, other: &JsValue) -> Option<bool> {
        if let (JsValue::String(a), JsValue::String(b)) = (self, other) {
            return Some(a.as_ref() < b.as_ref());
        }
        let a = self.to_number();
        let b = other.to_number();
        if a.is_nan() || b.is_nan() {
            return None;
        }
        Some(a < b)
    }
}

impl JsValue {
    #[doc = " `<`"]
    pub fn lt(&self, other: &JsValue) -> JsValue {
        match self.abstract_relational(other) {
            Some(true) => JsValue::Boolean(true),
            _ => JsValue::Boolean(false),
        }
    }
}

impl JsValue {
    #[doc = " `<=`"]
    pub fn le(&self, other: &JsValue) -> JsValue {
        match other.abstract_relational(self) {
            Some(true) => JsValue::Boolean(false),
            None => JsValue::Boolean(false),
            Some(false) => JsValue::Boolean(true),
        }
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
