use rawjs_common::SourceLocation;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub location: SourceLocation,
    pub had_line_break_before: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    Number(f64),
    String(String),
    Identifier(String),
    Boolean(bool),
    Null,
    TemplateLiteral(String),
    TemplateHead(String),
    TemplateMiddle(String),
    TemplateTail(String),
    RegExp { pattern: String, flags: String },

    // Keywords
    Var,
    Let,
    Const,
    Function,
    Return,
    If,
    Else,
    While,
    For,
    Do,
    Break,
    Continue,
    Switch,
    Case,
    Default,
    Throw,
    Try,
    Catch,
    Finally,
    New,
    Delete,
    Typeof,
    Void,
    In,
    Instanceof,
    This,
    Class,
    Extends,
    Super,
    Import,
    Export,
    Of,
    With,
    Debugger,
    Yield,
    Async,
    Await,

    // Punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Comma,
    Dot,
    Ellipsis,
    Colon,
    QuestionMark,
    OptionalChain,
    Arrow,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    StarStar,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    ShiftLeft,
    ShiftRight,
    UnsignedShiftRight,
    And,
    Or,
    Not,
    NullishCoalescing,

    // Comparison
    Equal,
    StrictEqual,
    NotEqual,
    StrictNotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,

    // Assignment
    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,
    StarStarAssign,
    AmpersandAssign,
    PipeAssign,
    CaretAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    UnsignedShiftRightAssign,
    AndAssign,
    OrAssign,
    NullishCoalescingAssign,

    // Increment/Decrement
    PlusPlus,
    MinusMinus,

    // Special
    Eof,
}

impl TokenKind {
    pub fn is_assignment_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Assign
                | TokenKind::PlusAssign
                | TokenKind::MinusAssign
                | TokenKind::StarAssign
                | TokenKind::SlashAssign
                | TokenKind::PercentAssign
                | TokenKind::StarStarAssign
                | TokenKind::AmpersandAssign
                | TokenKind::PipeAssign
                | TokenKind::CaretAssign
                | TokenKind::ShiftLeftAssign
                | TokenKind::ShiftRightAssign
                | TokenKind::UnsignedShiftRightAssign
                | TokenKind::AndAssign
                | TokenKind::OrAssign
                | TokenKind::NullishCoalescingAssign
        )
    }
}
