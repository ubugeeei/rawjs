#[derive(Debug, Clone)]
pub struct ForStatement {
    pub init: Option<ForInit>,
    pub test: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Box<Statement>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum ForInit {
    VariableDeclaration(VariableDeclaration),
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct ForInStatement {
    pub left: ForInOfLeft,
    pub right: Expression,
    pub body: Box<Statement>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ForOfStatement {
    pub left: ForInOfLeft,
    pub right: Expression,
    pub body: Box<Statement>,
    pub is_await: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum ForInOfLeft {
    VariableDeclaration(VariableDeclaration),
    Pattern(Pattern),
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct BreakStatement {
    pub label: Option<String>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ContinueStatement {
    pub label: Option<String>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct SwitchStatement {
    pub discriminant: Expression,
    pub cases: Vec<SwitchCase>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub test: Option<Expression>,
    pub consequent: Vec<Statement>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ThrowStatement {
    pub argument: Expression,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct TryStatement {
    pub block: BlockStatement,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStatement>,
    pub location: SourceLocation,
}

use super::*;
