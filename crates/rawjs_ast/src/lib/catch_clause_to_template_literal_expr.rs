#[derive(Debug, Clone)]
pub struct CatchClause {
    pub param: Option<Pattern>,
    pub body: BlockStatement,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct LabeledStatement {
    pub label: String,
    pub body: Box<Statement>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct WithStatement {
    pub object: Expression,
    pub body: Box<Statement>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ClassDeclaration {
    pub id: Option<String>,
    pub super_class: Option<Box<Expression>>,
    pub body: Vec<ClassMember>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ClassMember {
    pub key: Expression,
    pub value: Option<Expression>,
    pub kind: ClassMemberKind,
    pub is_static: bool,
    pub computed: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassMemberKind {
    Method,
    Get,
    Set,
    Constructor,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(IdentifierExpression),
    NumberLiteral(NumberLiteral),
    StringLiteral(StringLiteral),
    BooleanLiteral(BooleanLiteral),
    NullLiteral(SourceLocation),
    TemplateLiteral(TemplateLiteralExpr),
    RegExpLiteral(RegExpLiteral),
    ArrayExpression(ArrayExpression),
    ObjectExpression(ObjectExpression),
    FunctionExpression(FunctionDeclaration),
    ArrowFunctionExpression(ArrowFunctionExpression),
    ClassExpression(ClassDeclaration),
    Unary(UnaryExpression),
    Update(UpdateExpression),
    Binary(BinaryExpression),
    Logical(LogicalExpression),
    Assignment(AssignmentExpression),
    Conditional(ConditionalExpression),
    Call(CallExpression),
    Import(ImportExpression),
    New(NewExpression),
    Member(MemberExpression),
    Sequence(SequenceExpression),
    Spread(SpreadExpression),
    This(SourceLocation),
    Super(SourceLocation),
    ImportMeta(SourceLocation),
    Yield(YieldExpression),
    Await(AwaitExpression),
}

#[derive(Debug, Clone)]
pub struct IdentifierExpression {
    pub name: String,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct NumberLiteral {
    pub value: f64,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: String,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub value: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct TemplateLiteralExpr {
    pub quasis: Vec<String>,
    pub expressions: Vec<Expression>,
    pub location: SourceLocation,
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
