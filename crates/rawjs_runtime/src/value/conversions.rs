use std::rc::Rc;

use crate::object::ObjectInternal;

use super::{number_to_string, string_to_number, JsValue};

// ---------------------------------------------------------------------------
// Abstract operations (ES spec conversions)
// ---------------------------------------------------------------------------

impl JsValue {
    /// Abstract `ToBoolean()` -- returns the truthiness of a value.
    pub fn to_boolean(&self) -> bool {
        match self {
            JsValue::Undefined | JsValue::Null => false,
            JsValue::Boolean(b) => *b,
            JsValue::Number(n) => *n != 0.0 && !n.is_nan(),
            JsValue::String(s) => !s.is_empty(),
            JsValue::Symbol(_) => true,
            JsValue::Object(_) => true,
        }
    }

    /// Abstract `ToNumber()`.
    pub fn to_number(&self) -> f64 {
        match self {
            JsValue::Undefined => f64::NAN,
            JsValue::Null => 0.0,
            JsValue::Boolean(b) => {
                if *b {
                    1.0
                } else {
                    0.0
                }
            }
            JsValue::Number(n) => *n,
            JsValue::String(s) => string_to_number(s),
            JsValue::Symbol(_) => f64::NAN,
            JsValue::Object(_) => {
                // Simplified ToPrimitive: convert via string representation.
                let s = self.to_string_value();
                string_to_number(&s)
            }
        }
    }

    /// Abstract `ToString()` -- returns an owned `String`.
    pub fn to_string_value(&self) -> String {
        match self {
            JsValue::Undefined => "undefined".to_string(),
            JsValue::Null => "null".to_string(),
            JsValue::Boolean(true) => "true".to_string(),
            JsValue::Boolean(false) => "false".to_string(),
            JsValue::Number(n) => number_to_string(*n),
            JsValue::String(s) => s.to_string(),
            JsValue::Symbol(sym) => {
                if let Some(ref desc) = sym.description {
                    format!("Symbol({})", desc)
                } else {
                    "Symbol()".to_string()
                }
            }
            JsValue::Object(ptr) => {
                let obj = ptr.borrow();
                match &obj.internal {
                    ObjectInternal::Array(elements) => {
                        let parts: Vec<String> = elements
                            .iter()
                            .map(|v| {
                                if v.is_nullish() {
                                    String::new()
                                } else {
                                    v.to_string_value()
                                }
                            })
                            .collect();
                        parts.join(",")
                    }
                    ObjectInternal::Function(f) => {
                        format!("function {}() {{ [native code] }}", f.name)
                    }
                    ObjectInternal::Error(msg) => msg.clone(),
                    ObjectInternal::Iterator(_) => "[object Iterator]".to_string(),
                    ObjectInternal::Map(_) => "[object Map]".to_string(),
                    ObjectInternal::Set(_) => "[object Set]".to_string(),
                    ObjectInternal::Promise(_) => "[object Promise]".to_string(),
                    ObjectInternal::Generator(_) => "[object Generator]".to_string(),
                    ObjectInternal::Ordinary => "[object Object]".to_string(),
                }
            }
        }
    }

    /// Abstract `ToString()` -- returns an `Rc<str>`.
    pub fn to_js_string(&self) -> Rc<str> {
        Rc::from(self.to_string_value().as_str())
    }

    /// Returns the `typeof` string for this value.
    pub fn type_of(&self) -> &'static str {
        match self {
            JsValue::Undefined => "undefined",
            JsValue::Null => "object", // historical quirk
            JsValue::Boolean(_) => "boolean",
            JsValue::Number(_) => "number",
            JsValue::String(_) => "string",
            JsValue::Symbol(_) => "symbol",
            JsValue::Object(obj) => {
                if obj.borrow().is_function() {
                    "function"
                } else {
                    "object"
                }
            }
        }
    }

    /// Alias for `type_of()`.
    pub fn typeof_str(&self) -> &'static str {
        self.type_of()
    }

    /// `ToInt32()` -- used for bitwise operations.
    pub fn to_int32(&self) -> i32 {
        let n = self.to_number();
        if n.is_nan() || n.is_infinite() || n == 0.0 {
            return 0;
        }
        let int = n.trunc() as i64;
        let int32 = int % (1i64 << 32);
        if int32 >= (1i64 << 31) {
            (int32 - (1i64 << 32)) as i32
        } else if int32 < -(1i64 << 31) {
            (int32 + (1i64 << 32)) as i32
        } else {
            int32 as i32
        }
    }

    /// `ToUint32()` -- used for unsigned shift.
    pub fn to_uint32(&self) -> u32 {
        self.to_int32() as u32
    }
}
