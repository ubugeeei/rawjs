#[derive(Debug, Clone)]
pub struct YieldExpression {
    pub argument: Option<Box<Expression>>,
    pub delegate: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct AwaitExpression {
    pub argument: Box<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(IdentifierPattern),
    Array(ArrayPattern),
    Object(ObjectPattern),
    Assignment(AssignmentPattern),
    Rest(RestPattern),
}

#[derive(Debug, Clone)]
pub struct IdentifierPattern {
    pub name: String,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ArrayPattern {
    pub elements: Vec<Option<Pattern>>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ObjectPattern {
    pub properties: Vec<ObjectPatternProperty>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ObjectPatternProperty {
    pub key: Expression,
    pub value: Pattern,
    pub computed: bool,
    pub shorthand: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct AssignmentPattern {
    pub left: Box<Pattern>,
    pub right: Box<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct RestPattern {
    pub argument: Box<Pattern>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ImportDeclaration {
    pub specifiers: Vec<ImportSpecifier>,
    pub source: String,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum ImportSpecifier {
    Default {
        local: String,
        location: SourceLocation,
    },
    Named {
        imported: String,
        local: String,
        location: SourceLocation,
    },
    Namespace {
        local: String,
        location: SourceLocation,
    },
}

#[derive(Debug, Clone)]
pub struct ExportDeclaration {
    pub kind: ExportKind,
    pub location: SourceLocation,
}

use super::*;
