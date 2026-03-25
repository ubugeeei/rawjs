#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    UShr,
    Eq,
    StrictEq,
    Ne,
    StrictNe,
    Lt,
    Le,
    Gt,
    Ge,
    In,
    Instanceof,
}

#[derive(Debug, Clone)]
pub struct LogicalExpression {
    pub operator: LogicalOp,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
    NullishCoalescing,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpression {
    pub operator: AssignmentOp,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    ExpAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
    UShrAssign,
    AndAssign,
    OrAssign,
    NullishCoalescingAssign,
}

#[derive(Debug, Clone)]
pub struct ConditionalExpression {
    pub test: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternate: Box<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub optional: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ImportExpression {
    pub source: Box<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct NewExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct MemberExpression {
    pub object: Box<Expression>,
    pub property: Box<Expression>,
    pub computed: bool,
    pub optional: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct SequenceExpression {
    pub expressions: Vec<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct SpreadExpression {
    pub argument: Box<Expression>,
    pub location: SourceLocation,
}
