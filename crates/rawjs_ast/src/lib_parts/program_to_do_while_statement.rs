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
