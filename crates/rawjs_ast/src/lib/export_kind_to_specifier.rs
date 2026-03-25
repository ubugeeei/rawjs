#[derive(Debug, Clone)]
pub enum ExportKind {
    Default(Expression),
    Named(Vec<ExportSpecifier>),
    Declaration(Box<Statement>),
    AllFrom(String),
    NamedFrom(Vec<ExportSpecifier>, String),
}

#[derive(Debug, Clone)]
pub struct ExportSpecifier {
    pub local: String,
    pub exported: String,
    pub location: SourceLocation,
}

#[allow(unused_imports)]
use super::*;
