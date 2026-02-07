use rawjs_common::SourceLocation;

#[derive(Debug, Clone)]
pub struct Program {
    pub body: Vec<Statement>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ExpressionStatement),
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    Return(ReturnStatement),
    If(IfStatement),
    While(WhileStatement),
    DoWhile(DoWhileStatement),
    For(ForStatement),
    ForIn(ForInStatement),
    ForOf(ForOfStatement),
    Block(BlockStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Switch(SwitchStatement),
    Throw(ThrowStatement),
    Try(TryStatement),
    Labeled(LabeledStatement),
    With(WithStatement),
    Empty,
    Debugger,
    ClassDeclaration(ClassDeclaration),
    ImportDeclaration(ImportDeclaration),
    ExportDeclaration(ExportDeclaration),
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub kind: VarKind,
    pub declarations: Vec<VariableDeclarator>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarKind {
    Var,
    Let,
    Const,
    Using,
    AwaitUsing,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarator {
    pub id: Pattern,
    pub init: Option<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub id: Option<String>,
    pub params: Vec<Pattern>,
    pub body: Box<BlockStatement>,
    pub is_async: bool,
    pub is_generator: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub argument: Option<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub test: Expression,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub test: Expression,
    pub body: Box<Statement>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct DoWhileStatement {
    pub test: Expression,
    pub body: Box<Statement>,
    pub location: SourceLocation,
}

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
    New(NewExpression),
    Member(MemberExpression),
    Sequence(SequenceExpression),
    Spread(SpreadExpression),
    This(SourceLocation),
    Super(SourceLocation),
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

// ---------------------------------------------------------------------------
// ESM: import / export
// ---------------------------------------------------------------------------

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
