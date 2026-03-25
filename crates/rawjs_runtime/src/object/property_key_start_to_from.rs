use std::rc::Rc;

/// A property key -- either a string name, numeric index, or symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PropertyKey {
    String(Rc<str>),
    Index(u32),
    Symbol(u64),
}

impl PropertyKey {
    /// Create a string property key.
    pub fn from_string(s: &str) -> Self {
        if let Ok(idx) = s.parse::<u32>() {
            if idx.to_string() == s {
                return PropertyKey::Index(idx);
            }
        }
        PropertyKey::String(Rc::from(s))
    }
}

impl PropertyKey {
    /// Produce the string representation of this key.
    pub fn as_str(&self) -> String {
        match self {
            PropertyKey::String(s) => s.to_string(),
            PropertyKey::Index(i) => i.to_string(),
            PropertyKey::Symbol(id) => format!("Symbol({})", id),
        }
    }
}

impl PropertyKey {
    /// Check if this key is a symbol.
    pub fn is_symbol(&self) -> bool {
        matches!(self, PropertyKey::Symbol(_))
    }
}

impl From<&str> for PropertyKey {
    fn from(s: &str) -> Self {
        PropertyKey::from_string(s)
    }
}

use super::*;
