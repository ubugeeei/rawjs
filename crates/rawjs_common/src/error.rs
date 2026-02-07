use crate::source::SourceLocation;
use std::fmt;

pub type Result<T> = std::result::Result<T, RawJsError>;

#[derive(Debug, Clone)]
pub struct RawJsError {
    pub kind: ErrorKind,
    pub message: String,
    pub location: Option<SourceLocation>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    SyntaxError,
    TypeError,
    ReferenceError,
    RangeError,
    InternalError,
}

impl RawJsError {
    pub fn syntax_error(message: impl Into<String>, location: Option<SourceLocation>) -> Self {
        Self {
            kind: ErrorKind::SyntaxError,
            message: message.into(),
            location,
        }
    }

    pub fn type_error(message: impl Into<String>) -> Self {
        Self {
            kind: ErrorKind::TypeError,
            message: message.into(),
            location: None,
        }
    }

    pub fn reference_error(message: impl Into<String>) -> Self {
        Self {
            kind: ErrorKind::ReferenceError,
            message: message.into(),
            location: None,
        }
    }

    pub fn range_error(message: impl Into<String>) -> Self {
        Self {
            kind: ErrorKind::RangeError,
            message: message.into(),
            location: None,
        }
    }

    pub fn internal_error(message: impl Into<String>) -> Self {
        Self {
            kind: ErrorKind::InternalError,
            message: message.into(),
            location: None,
        }
    }
}

impl fmt::Display for RawJsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind_str = match self.kind {
            ErrorKind::SyntaxError => "SyntaxError",
            ErrorKind::TypeError => "TypeError",
            ErrorKind::ReferenceError => "ReferenceError",
            ErrorKind::RangeError => "RangeError",
            ErrorKind::InternalError => "InternalError",
        };
        if let Some(ref loc) = self.location {
            write!(
                f,
                "{}: {} at {}:{}",
                kind_str, self.message, loc.line, loc.column
            )
        } else {
            write!(f, "{}: {}", kind_str, self.message)
        }
    }
}

impl std::error::Error for RawJsError {}
