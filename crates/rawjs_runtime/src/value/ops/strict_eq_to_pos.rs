use std::rc::Rc;

use super::JsValue;

impl JsValue {
    #[doc = " Strict equality (`===`)."]
    pub fn strict_eq(&self, other: &JsValue) -> bool {
        match (self, other) {
            (JsValue::Undefined, JsValue::Undefined) => true,
            (JsValue::Null, JsValue::Null) => true,
            (JsValue::Boolean(a), JsValue::Boolean(b)) => a == b,
            (JsValue::Number(a), JsValue::Number(b)) => {
                if a.is_nan() || b.is_nan() {
                    return false;
                }
                a == b
            }
            (JsValue::String(a), JsValue::String(b)) => a == b,
            (JsValue::Symbol(a), JsValue::Symbol(b)) => a.id == b.id,
            (JsValue::Object(a), JsValue::Object(b)) => a.ptr_eq(b),
            _ => false,
        }
    }
}

impl JsValue {
    #[doc = " Abstract equality (`==`)."]
    pub fn abstract_eq(&self, other: &JsValue) -> bool {
        match (self, other) {
            (JsValue::Undefined, JsValue::Undefined) => true,
            (JsValue::Null, JsValue::Null) => true,
            (JsValue::Undefined, JsValue::Null) | (JsValue::Null, JsValue::Undefined) => true,
            (JsValue::Number(a), JsValue::Number(b)) => a == b,
            (JsValue::String(a), JsValue::String(b)) => a == b,
            (JsValue::Boolean(a), JsValue::Boolean(b)) => a == b,
            (JsValue::Symbol(a), JsValue::Symbol(b)) => a.id == b.id,
            (JsValue::Object(a), JsValue::Object(b)) => a.ptr_eq(b),
            (JsValue::Number(_), JsValue::String(_)) => {
                self.strict_eq(&JsValue::Number(other.to_number()))
            }
            (JsValue::String(_), JsValue::Number(_)) => {
                JsValue::Number(self.to_number()).strict_eq(other)
            }
            (JsValue::Boolean(_), _) => JsValue::Number(self.to_number()).abstract_eq(other),
            (_, JsValue::Boolean(_)) => self.abstract_eq(&JsValue::Number(other.to_number())),
            (JsValue::String(_), JsValue::Object(_)) | (JsValue::Number(_), JsValue::Object(_)) => {
                let prim = JsValue::String(Rc::from(other.to_string_value().as_str()));
                self.abstract_eq(&prim)
            }
            (JsValue::Object(_), JsValue::String(_)) | (JsValue::Object(_), JsValue::Number(_)) => {
                let prim = JsValue::String(Rc::from(self.to_string_value().as_str()));
                prim.abstract_eq(other)
            }
            _ => false,
        }
    }
}

impl JsValue {
    #[doc = " The `+` operator."]
    #[doc = " If either operand is a string, concatenation is performed."]
    pub fn add(&self, other: &JsValue) -> JsValue {
        if self.is_string() || other.is_string() {
            let ls = self.to_string_value();
            let rs = other.to_string_value();
            return JsValue::String(Rc::from(format!("{}{}", ls, rs).as_str()));
        }
        if self.is_object() || other.is_object() {
            let ls = self.to_string_value();
            let rs = other.to_string_value();
            return JsValue::String(Rc::from(format!("{}{}", ls, rs).as_str()));
        }
        JsValue::Number(self.to_number() + other.to_number())
    }
}

impl JsValue {
    #[doc = " The `-` operator."]
    pub fn sub(&self, other: &JsValue) -> JsValue {
        JsValue::Number(self.to_number() - other.to_number())
    }
}

impl JsValue {
    #[doc = " The `*` operator."]
    pub fn mul(&self, other: &JsValue) -> JsValue {
        JsValue::Number(self.to_number() * other.to_number())
    }
}

impl JsValue {
    #[doc = " The `/` operator."]
    pub fn div(&self, other: &JsValue) -> JsValue {
        JsValue::Number(self.to_number() / other.to_number())
    }
}

impl JsValue {
    #[doc = " The `%` operator."]
    pub fn rem(&self, other: &JsValue) -> JsValue {
        let a = self.to_number();
        let b = other.to_number();
        if b == 0.0 {
            return JsValue::Number(f64::NAN);
        }
        JsValue::Number(a % b)
    }
}

impl JsValue {
    #[doc = " The `**` operator (exponentiation)."]
    pub fn exp(&self, other: &JsValue) -> JsValue {
        JsValue::Number(self.to_number().powf(other.to_number()))
    }
}

impl JsValue {
    #[doc = " Unary `-`."]
    pub fn neg(&self) -> JsValue {
        JsValue::Number(-self.to_number())
    }
}

impl JsValue {
    #[doc = " Unary `+`."]
    pub fn pos(&self) -> JsValue {
        JsValue::Number(self.to_number())
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
