#[derive(Debug, Clone)]
pub struct RegExpLiteral {
    pub pattern: String,
    pub flags: String,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ArrayExpression {
    pub elements: Vec<Option<Expression>>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ObjectExpression {
    pub properties: Vec<Property>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct Property {
    pub key: Expression,
    pub value: Expression,
    pub kind: PropertyKind,
    pub computed: bool,
    pub shorthand: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

#[derive(Debug, Clone)]
pub struct ArrowFunctionExpression {
    pub params: Vec<Pattern>,
    pub body: ArrowFunctionBody,
    pub is_async: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum ArrowFunctionBody {
    Expression(Box<Expression>),
    Block(BlockStatement),
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: UnaryOp,
    pub argument: Box<Expression>,
    pub prefix: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
    Plus,
    Not,
    BitNot,
    Typeof,
    Void,
    Delete,
}

#[derive(Debug, Clone)]
pub struct UpdateExpression {
    pub operator: UpdateOp,
    pub argument: Box<Expression>,
    pub prefix: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateOp {
    Increment,
    Decrement,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub operator: BinaryOp,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub location: SourceLocation,
}
