impl JsValue {
    pub fn is_function(&self) -> bool {
        match self {
            JsValue::Object(ptr) => ptr.borrow().is_function(),
            _ => false,
        }
    }
}

impl JsValue {
    pub fn is_array(&self) -> bool {
        match self {
            JsValue::Object(ptr) => ptr.borrow().is_array(),
            _ => false,
        }
    }
}

impl JsValue {
    pub fn is_nan(&self) -> bool {
        matches ! (self , JsValue :: Number (n) if n . is_nan ())
    }
}

impl fmt::Display for JsValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsValue::Undefined => write!(f, "undefined"),
            JsValue::Null => write!(f, "null"),
            JsValue::Boolean(b) => write!(f, "{}", b),
            JsValue::Number(n) => write!(f, "{}", number_to_string(*n)),
            JsValue::String(s) => write!(f, "{}", s),
            JsValue::Symbol(sym) => {
                if let Some(ref desc) = sym.description {
                    write!(f, "Symbol({})", desc)
                } else {
                    write!(f, "Symbol()")
                }
            }
            JsValue::Object(ptr) => {
                let obj = ptr.borrow();
                write!(f, "{}", *obj)
            }
        }
    }
}

impl PartialEq for JsValue {
    fn eq(&self, other: &Self) -> bool {
        self.strict_eq(other)
    }
}

/// Convert a JS string to a number following the ES specification rules.
pub(crate) fn string_to_number(s: &str) -> f64 {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return 0.0;
    }
    if trimmed.starts_with("0x") || trimmed.starts_with("0X") {
        return match i64::from_str_radix(&trimmed[2..], 16) {
            Ok(n) => n as f64,
            Err(_) => f64::NAN,
        };
    }
    if trimmed.starts_with("0o") || trimmed.starts_with("0O") {
        return match i64::from_str_radix(&trimmed[2..], 8) {
            Ok(n) => n as f64,
            Err(_) => f64::NAN,
        };
    }
    if trimmed.starts_with("0b") || trimmed.starts_with("0B") {
        return match i64::from_str_radix(&trimmed[2..], 2) {
            Ok(n) => n as f64,
            Err(_) => f64::NAN,
        };
    }
    if trimmed == "Infinity" || trimmed == "+Infinity" {
        return f64::INFINITY;
    }
    if trimmed == "-Infinity" {
        return f64::NEG_INFINITY;
    }
    trimmed.parse::<f64>().unwrap_or(f64::NAN)
}

/// Convert a number to its JS string representation.
pub fn number_to_string(n: f64) -> String {
    if n.is_nan() {
        return "NaN".to_string();
    }
    if n.is_infinite() {
        return if n.is_sign_positive() {
            "Infinity".to_string()
        } else {
            "-Infinity".to_string()
        };
    }
    if n == 0.0 {
        return "0".to_string();
    }
    if n.fract() == 0.0 && n.abs() < (i64::MAX as f64) {
        return format!("{}", n as i64);
    }
    format!("{}", n)
}

use super::*;
