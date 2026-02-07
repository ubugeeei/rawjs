use std::rc::Rc;

use super::JsValue;

// ---------------------------------------------------------------------------
// Equality
// ---------------------------------------------------------------------------

impl JsValue {
    /// Strict equality (`===`).
    pub fn strict_eq(&self, other: &JsValue) -> bool {
        match (self, other) {
            (JsValue::Undefined, JsValue::Undefined) => true,
            (JsValue::Null, JsValue::Null) => true,
            (JsValue::Boolean(a), JsValue::Boolean(b)) => a == b,
            (JsValue::Number(a), JsValue::Number(b)) => {
                // NaN !== NaN
                if a.is_nan() || b.is_nan() {
                    return false;
                }
                // +0 === -0
                a == b
            }
            (JsValue::String(a), JsValue::String(b)) => a == b,
            (JsValue::Symbol(a), JsValue::Symbol(b)) => a.id == b.id,
            (JsValue::Object(a), JsValue::Object(b)) => a.ptr_eq(b),
            _ => false,
        }
    }

    /// Abstract equality (`==`).
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
            // Mixed types: coerce
            (JsValue::Number(_), JsValue::String(_)) => {
                self.strict_eq(&JsValue::Number(other.to_number()))
            }
            (JsValue::String(_), JsValue::Number(_)) => {
                JsValue::Number(self.to_number()).strict_eq(other)
            }
            (JsValue::Boolean(_), _) => JsValue::Number(self.to_number()).abstract_eq(other),
            (_, JsValue::Boolean(_)) => self.abstract_eq(&JsValue::Number(other.to_number())),
            // String/Number == Object -> compare with ToPrimitive
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

// ---------------------------------------------------------------------------
// Arithmetic operations
// ---------------------------------------------------------------------------

impl JsValue {
    /// The `+` operator.
    /// If either operand is a string, concatenation is performed.
    pub fn add(&self, other: &JsValue) -> JsValue {
        // If either side is a string, concatenate.
        if self.is_string() || other.is_string() {
            let ls = self.to_string_value();
            let rs = other.to_string_value();
            return JsValue::String(Rc::from(format!("{}{}", ls, rs).as_str()));
        }
        // If either is an object, try ToPrimitive (simplified: toString).
        if self.is_object() || other.is_object() {
            let ls = self.to_string_value();
            let rs = other.to_string_value();
            return JsValue::String(Rc::from(format!("{}{}", ls, rs).as_str()));
        }
        JsValue::Number(self.to_number() + other.to_number())
    }

    /// The `-` operator.
    pub fn sub(&self, other: &JsValue) -> JsValue {
        JsValue::Number(self.to_number() - other.to_number())
    }

    /// The `*` operator.
    pub fn mul(&self, other: &JsValue) -> JsValue {
        JsValue::Number(self.to_number() * other.to_number())
    }

    /// The `/` operator.
    pub fn div(&self, other: &JsValue) -> JsValue {
        JsValue::Number(self.to_number() / other.to_number())
    }

    /// The `%` operator.
    pub fn rem(&self, other: &JsValue) -> JsValue {
        let a = self.to_number();
        let b = other.to_number();
        if b == 0.0 {
            return JsValue::Number(f64::NAN);
        }
        JsValue::Number(a % b)
    }

    /// The `**` operator (exponentiation).
    pub fn exp(&self, other: &JsValue) -> JsValue {
        JsValue::Number(self.to_number().powf(other.to_number()))
    }

    /// Unary `-`.
    pub fn neg(&self) -> JsValue {
        JsValue::Number(-self.to_number())
    }

    /// Unary `+`.
    pub fn pos(&self) -> JsValue {
        JsValue::Number(self.to_number())
    }

    /// Pre/post increment helper: returns the new value.
    pub fn increment(&self) -> JsValue {
        JsValue::Number(self.to_number() + 1.0)
    }

    /// Pre/post decrement helper: returns the new value.
    pub fn decrement(&self) -> JsValue {
        JsValue::Number(self.to_number() - 1.0)
    }
}

// ---------------------------------------------------------------------------
// Bitwise operations
// ---------------------------------------------------------------------------

impl JsValue {
    /// `&` (bitwise AND).
    pub fn bitand(&self, other: &JsValue) -> JsValue {
        JsValue::Number((self.to_int32() & other.to_int32()) as f64)
    }

    /// `|` (bitwise OR).
    pub fn bitor(&self, other: &JsValue) -> JsValue {
        JsValue::Number((self.to_int32() | other.to_int32()) as f64)
    }

    /// `^` (bitwise XOR).
    pub fn bitxor(&self, other: &JsValue) -> JsValue {
        JsValue::Number((self.to_int32() ^ other.to_int32()) as f64)
    }

    /// `~` (bitwise NOT).
    pub fn bitnot(&self) -> JsValue {
        JsValue::Number((!self.to_int32()) as f64)
    }

    /// `<<` (left shift).
    pub fn shl(&self, other: &JsValue) -> JsValue {
        let lhs = self.to_int32();
        let rhs = other.to_uint32() & 0x1f;
        JsValue::Number((lhs << rhs) as f64)
    }

    /// `>>` (signed right shift).
    pub fn shr(&self, other: &JsValue) -> JsValue {
        let lhs = self.to_int32();
        let rhs = other.to_uint32() & 0x1f;
        JsValue::Number((lhs >> rhs) as f64)
    }

    /// `>>>` (unsigned right shift).
    pub fn ushr(&self, other: &JsValue) -> JsValue {
        let lhs = self.to_uint32();
        let rhs = other.to_uint32() & 0x1f;
        JsValue::Number((lhs >> rhs) as f64)
    }
}

// ---------------------------------------------------------------------------
// Comparison operations
// ---------------------------------------------------------------------------

impl JsValue {
    /// ES Abstract Relational Comparison.
    /// Returns `None` if result is **undefined** (i.e. NaN involved).
    fn abstract_relational(&self, other: &JsValue) -> Option<bool> {
        // If both are strings, compare lexicographically.
        if let (JsValue::String(a), JsValue::String(b)) = (self, other) {
            return Some(a.as_ref() < b.as_ref());
        }
        let a = self.to_number();
        let b = other.to_number();
        if a.is_nan() || b.is_nan() {
            return None; // undefined
        }
        Some(a < b)
    }

    /// `<`
    pub fn lt(&self, other: &JsValue) -> JsValue {
        match self.abstract_relational(other) {
            Some(true) => JsValue::Boolean(true),
            _ => JsValue::Boolean(false),
        }
    }

    /// `<=`
    pub fn le(&self, other: &JsValue) -> JsValue {
        // a <= b  is  !(b < a)
        match other.abstract_relational(self) {
            Some(true) => JsValue::Boolean(false),
            None => JsValue::Boolean(false),
            Some(false) => JsValue::Boolean(true),
        }
    }

    /// `>`
    pub fn gt(&self, other: &JsValue) -> JsValue {
        match other.abstract_relational(self) {
            Some(true) => JsValue::Boolean(true),
            _ => JsValue::Boolean(false),
        }
    }

    /// `>=`
    pub fn ge(&self, other: &JsValue) -> JsValue {
        // a >= b  is  !(a < b)
        match self.abstract_relational(other) {
            Some(true) => JsValue::Boolean(false),
            None => JsValue::Boolean(false),
            Some(false) => JsValue::Boolean(true),
        }
    }
}

// ---------------------------------------------------------------------------
// Logical operators
// ---------------------------------------------------------------------------

impl JsValue {
    /// `!` (logical NOT) -- always returns a boolean.
    pub fn logical_not(&self) -> JsValue {
        JsValue::Boolean(!self.to_boolean())
    }
}

// ---------------------------------------------------------------------------
// instanceof helper
// ---------------------------------------------------------------------------

impl JsValue {
    /// Check if this value is an instance of the given constructor object.
    /// Simplified: walks the prototype chain of `self` looking for
    /// `constructor.prototype`.
    pub fn instance_of(&self, constructor: &JsValue) -> bool {
        let obj_ptr = match self {
            JsValue::Object(ptr) => ptr.clone(),
            _ => return false,
        };
        let ctor_ptr = match constructor {
            JsValue::Object(ptr) => ptr.clone(),
            _ => return false,
        };

        // Get constructor.prototype
        let ctor_proto_val = ctor_ptr.borrow().get_property("prototype");
        let ctor_proto = match &ctor_proto_val {
            JsValue::Object(ptr) => ptr.clone(),
            _ => return false,
        };

        // Walk the prototype chain of `obj_ptr`.
        let mut current = obj_ptr.borrow().prototype.clone();
        while let Some(proto) = current {
            if proto.ptr_eq(&ctor_proto) {
                return true;
            }
            current = proto.borrow().prototype.clone();
        }
        false
    }
}
