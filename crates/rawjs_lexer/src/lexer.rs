use rawjs_common::{RawJsError, Result, SourceLocation};

use crate::token::{Token, TokenKind};

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    line: u32,
    column: u32,
    had_line_break: bool,
    /// Stack tracking brace depth within template expression contexts.
    /// Each entry represents a nested template literal; the value is the
    /// brace depth inside the current template expression. When a `{` is
    /// encountered while inside a template expression, the depth is
    /// incremented; when `}` is encountered and the depth is 0, we resume
    /// scanning the template literal instead of emitting RBrace.
    template_brace_stack: Vec<u32>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            pos: 0,
            line: 1,
            column: 0,
            had_line_break: false,
            template_brace_stack: Vec::new(),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }

    fn peek_char(&self) -> Option<char> {
        self.source.get(self.pos).copied()
    }

    fn peek_char_at(&self, offset: usize) -> Option<char> {
        self.source.get(self.pos + offset).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.source.get(self.pos).copied()?;
        self.pos += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 0;
            self.had_line_break = true;
        } else {
            self.column += 1;
        }
        Some(ch)
    }

    fn location(&self) -> SourceLocation {
        SourceLocation::new(self.line, self.column, self.pos as u32)
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek_char() {
                Some(' ' | '\t' | '\r') => {
                    self.advance();
                }
                Some('\n') => {
                    self.advance();
                }
                Some('/') => {
                    if self.peek_char_at(1) == Some('/') {
                        // line comment
                        self.advance();
                        self.advance();
                        while let Some(ch) = self.peek_char() {
                            if ch == '\n' {
                                break;
                            }
                            self.advance();
                        }
                    } else if self.peek_char_at(1) == Some('*') {
                        // block comment
                        self.advance();
                        self.advance();
                        loop {
                            match self.advance() {
                                Some('*') if self.peek_char() == Some('/') => {
                                    self.advance();
                                    break;
                                }
                                None => break,
                                _ => {}
                            }
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.had_line_break = false;
        self.skip_whitespace_and_comments();

        let loc = self.location();
        let had_line_break = self.had_line_break;

        let ch = match self.peek_char() {
            Some(ch) => ch,
            None => {
                return Ok(Token {
                    kind: TokenKind::Eof,
                    location: loc,
                    had_line_break_before: had_line_break,
                });
            }
        };

        let kind = match ch {
            '0'..='9' => self.read_number()?,
            '"' | '\'' => self.read_string()?,
            '`' => self.read_template_literal()?,
            'a'..='z' | 'A'..='Z' | '_' | '$' => self.read_identifier_or_keyword()?,
            ch if is_id_start(ch) => self.read_identifier_or_keyword()?,
            '(' => {
                self.advance();
                TokenKind::LParen
            }
            ')' => {
                self.advance();
                TokenKind::RParen
            }
            '{' => {
                self.advance();
                if let Some(depth) = self.template_brace_stack.last_mut() {
                    *depth += 1;
                }
                TokenKind::LBrace
            }
            '}' => {
                if let Some(depth) = self.template_brace_stack.last_mut() {
                    if *depth == 0 {
                        self.template_brace_stack.pop();
                        self.advance(); // consume }
                        let kind = self.read_template_middle_or_tail()?;
                        if matches!(kind, TokenKind::TemplateMiddle(_)) {
                            self.template_brace_stack.push(0);
                        }
                        kind
                    } else {
                        *depth -= 1;
                        self.advance();
                        TokenKind::RBrace
                    }
                } else {
                    self.advance();
                    TokenKind::RBrace
                }
            }
            '[' => {
                self.advance();
                TokenKind::LBracket
            }
            ']' => {
                self.advance();
                TokenKind::RBracket
            }
            ';' => {
                self.advance();
                TokenKind::Semicolon
            }
            ',' => {
                self.advance();
                TokenKind::Comma
            }
            ':' => {
                self.advance();
                TokenKind::Colon
            }
            '~' => {
                self.advance();
                TokenKind::Tilde
            }
            '.' => {
                if self.peek_char_at(1) == Some('.') && self.peek_char_at(2) == Some('.') {
                    self.advance();
                    self.advance();
                    self.advance();
                    TokenKind::Ellipsis
                } else if matches!(self.peek_char_at(1), Some('0'..='9')) {
                    self.read_number()?
                } else {
                    self.advance();
                    TokenKind::Dot
                }
            }
            '?' => {
                self.advance();
                if self.peek_char() == Some('?') {
                    self.advance();
                    if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::NullishCoalescingAssign
                    } else {
                        TokenKind::NullishCoalescing
                    }
                } else if self.peek_char() == Some('.') {
                    self.advance();
                    TokenKind::OptionalChain
                } else {
                    TokenKind::QuestionMark
                }
            }
            '+' => {
                self.advance();
                match self.peek_char() {
                    Some('+') => {
                        self.advance();
                        TokenKind::PlusPlus
                    }
                    Some('=') => {
                        self.advance();
                        TokenKind::PlusAssign
                    }
                    _ => TokenKind::Plus,
                }
            }
            '-' => {
                self.advance();
                match self.peek_char() {
                    Some('-') => {
                        self.advance();
                        TokenKind::MinusMinus
                    }
                    Some('=') => {
                        self.advance();
                        TokenKind::MinusAssign
                    }
                    _ => TokenKind::Minus,
                }
            }
            '*' => {
                self.advance();
                if self.peek_char() == Some('*') {
                    self.advance();
                    if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::StarStarAssign
                    } else {
                        TokenKind::StarStar
                    }
                } else if self.peek_char() == Some('=') {
                    self.advance();
                    TokenKind::StarAssign
                } else {
                    TokenKind::Star
                }
            }
            '/' => {
                self.advance();
                if self.peek_char() == Some('=') {
                    self.advance();
                    TokenKind::SlashAssign
                } else {
                    TokenKind::Slash
                }
            }
            '%' => {
                self.advance();
                if self.peek_char() == Some('=') {
                    self.advance();
                    TokenKind::PercentAssign
                } else {
                    TokenKind::Percent
                }
            }
            '&' => {
                self.advance();
                match self.peek_char() {
                    Some('&') => {
                        self.advance();
                        if self.peek_char() == Some('=') {
                            self.advance();
                            TokenKind::AndAssign
                        } else {
                            TokenKind::And
                        }
                    }
                    Some('=') => {
                        self.advance();
                        TokenKind::AmpersandAssign
                    }
                    _ => TokenKind::Ampersand,
                }
            }
            '|' => {
                self.advance();
                match self.peek_char() {
                    Some('|') => {
                        self.advance();
                        if self.peek_char() == Some('=') {
                            self.advance();
                            TokenKind::OrAssign
                        } else {
                            TokenKind::Or
                        }
                    }
                    Some('=') => {
                        self.advance();
                        TokenKind::PipeAssign
                    }
                    _ => TokenKind::Pipe,
                }
            }
            '^' => {
                self.advance();
                if self.peek_char() == Some('=') {
                    self.advance();
                    TokenKind::CaretAssign
                } else {
                    TokenKind::Caret
                }
            }
            '!' => {
                self.advance();
                if self.peek_char() == Some('=') {
                    self.advance();
                    if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::StrictNotEqual
                    } else {
                        TokenKind::NotEqual
                    }
                } else {
                    TokenKind::Not
                }
            }
            '=' => {
                self.advance();
                match self.peek_char() {
                    Some('=') => {
                        self.advance();
                        if self.peek_char() == Some('=') {
                            self.advance();
                            TokenKind::StrictEqual
                        } else {
                            TokenKind::Equal
                        }
                    }
                    Some('>') => {
                        self.advance();
                        TokenKind::Arrow
                    }
                    _ => TokenKind::Assign,
                }
            }
            '<' => {
                self.advance();
                match self.peek_char() {
                    Some('<') => {
                        self.advance();
                        if self.peek_char() == Some('=') {
                            self.advance();
                            TokenKind::ShiftLeftAssign
                        } else {
                            TokenKind::ShiftLeft
                        }
                    }
                    Some('=') => {
                        self.advance();
                        TokenKind::LessEqual
                    }
                    _ => TokenKind::LessThan,
                }
            }
            '>' => {
                self.advance();
                match self.peek_char() {
                    Some('>') => {
                        self.advance();
                        if self.peek_char() == Some('>') {
                            self.advance();
                            if self.peek_char() == Some('=') {
                                self.advance();
                                TokenKind::UnsignedShiftRightAssign
                            } else {
                                TokenKind::UnsignedShiftRight
                            }
                        } else if self.peek_char() == Some('=') {
                            self.advance();
                            TokenKind::ShiftRightAssign
                        } else {
                            TokenKind::ShiftRight
                        }
                    }
                    Some('=') => {
                        self.advance();
                        TokenKind::GreaterEqual
                    }
                    _ => TokenKind::GreaterThan,
                }
            }
            _ => {
                return Err(RawJsError::syntax_error(
                    format!("Unexpected character: '{}'", ch),
                    Some(loc),
                ));
            }
        };

        Ok(Token {
            kind,
            location: loc,
            had_line_break_before: had_line_break,
        })
    }

    fn read_number(&mut self) -> Result<TokenKind> {
        let mut s = String::new();

        if self.peek_char() == Some('0') {
            s.push('0');
            self.advance();
            match self.peek_char() {
                Some('x' | 'X') => {
                    s.push(self.advance().unwrap());
                    while let Some(ch) = self.peek_char() {
                        if ch.is_ascii_hexdigit() || ch == '_' {
                            if ch != '_' {
                                s.push(ch);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let val = i64::from_str_radix(&s[2..], 16).map_err(|_| {
                        RawJsError::syntax_error("Invalid hex literal", Some(self.location()))
                    })?;
                    return Ok(TokenKind::Number(val as f64));
                }
                Some('o' | 'O') => {
                    s.push(self.advance().unwrap());
                    while let Some(ch) = self.peek_char() {
                        if ('0'..='7').contains(&ch) || ch == '_' {
                            if ch != '_' {
                                s.push(ch);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let val = i64::from_str_radix(&s[2..], 8).map_err(|_| {
                        RawJsError::syntax_error("Invalid octal literal", Some(self.location()))
                    })?;
                    return Ok(TokenKind::Number(val as f64));
                }
                Some('b' | 'B') => {
                    s.push(self.advance().unwrap());
                    while let Some(ch) = self.peek_char() {
                        if ch == '0' || ch == '1' || ch == '_' {
                            if ch != '_' {
                                s.push(ch);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let val = i64::from_str_radix(&s[2..], 2).map_err(|_| {
                        RawJsError::syntax_error("Invalid binary literal", Some(self.location()))
                    })?;
                    return Ok(TokenKind::Number(val as f64));
                }
                _ => {}
            }
        }

        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() || ch == '_' {
                if ch != '_' {
                    s.push(ch);
                }
                self.advance();
            } else {
                break;
            }
        }

        if self.peek_char() == Some('.') {
            s.push('.');
            self.advance();
            while let Some(ch) = self.peek_char() {
                if ch.is_ascii_digit() || ch == '_' {
                    if ch != '_' {
                        s.push(ch);
                    }
                    self.advance();
                } else {
                    break;
                }
            }
        }

        if matches!(self.peek_char(), Some('e' | 'E')) {
            s.push(self.advance().unwrap());
            if matches!(self.peek_char(), Some('+' | '-')) {
                s.push(self.advance().unwrap());
            }
            while let Some(ch) = self.peek_char() {
                if ch.is_ascii_digit() {
                    s.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let val: f64 = s.parse().map_err(|_| {
            RawJsError::syntax_error(format!("Invalid number: {}", s), Some(self.location()))
        })?;
        Ok(TokenKind::Number(val))
    }

    fn read_string(&mut self) -> Result<TokenKind> {
        let quote = self.advance().unwrap();
        let mut s = String::new();
        loop {
            match self.advance() {
                Some(ch) if ch == quote => break,
                Some('\\') => {
                    let esc = self.read_escape_sequence()?;
                    s.push(esc);
                }
                Some(ch) => s.push(ch),
                None => {
                    return Err(RawJsError::syntax_error(
                        "Unterminated string literal",
                        Some(self.location()),
                    ));
                }
            }
        }
        Ok(TokenKind::String(s))
    }

    fn read_escape_sequence(&mut self) -> Result<char> {
        match self.advance() {
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('\\') => Ok('\\'),
            Some('\'') => Ok('\''),
            Some('"') => Ok('"'),
            Some('`') => Ok('`'),
            Some('0') => Ok('\0'),
            Some('b') => Ok('\u{0008}'),
            Some('f') => Ok('\u{000C}'),
            Some('v') => Ok('\u{000B}'),
            Some('u') => self.read_unicode_escape(),
            Some('x') => self.read_hex_escape(),
            Some('\n') => Ok('\n'),
            Some(ch) => Ok(ch),
            None => Err(RawJsError::syntax_error(
                "Unexpected end of input in escape sequence",
                Some(self.location()),
            )),
        }
    }

    fn read_unicode_escape(&mut self) -> Result<char> {
        if self.peek_char() == Some('{') {
            self.advance();
            let mut hex = String::new();
            while let Some(ch) = self.peek_char() {
                if ch == '}' {
                    self.advance();
                    break;
                }
                hex.push(ch);
                self.advance();
            }
            let cp = u32::from_str_radix(&hex, 16).map_err(|_| {
                RawJsError::syntax_error("Invalid unicode escape", Some(self.location()))
            })?;
            char::from_u32(cp).ok_or_else(|| {
                RawJsError::syntax_error("Invalid unicode code point", Some(self.location()))
            })
        } else {
            let mut hex = String::new();
            for _ in 0..4 {
                match self.advance() {
                    Some(ch) if ch.is_ascii_hexdigit() => hex.push(ch),
                    _ => {
                        return Err(RawJsError::syntax_error(
                            "Invalid unicode escape sequence",
                            Some(self.location()),
                        ));
                    }
                }
            }
            let cp = u32::from_str_radix(&hex, 16).unwrap();
            char::from_u32(cp).ok_or_else(|| {
                RawJsError::syntax_error("Invalid unicode code point", Some(self.location()))
            })
        }
    }

    fn read_hex_escape(&mut self) -> Result<char> {
        let mut hex = String::new();
        for _ in 0..2 {
            match self.advance() {
                Some(ch) if ch.is_ascii_hexdigit() => hex.push(ch),
                _ => {
                    return Err(RawJsError::syntax_error(
                        "Invalid hex escape sequence",
                        Some(self.location()),
                    ));
                }
            }
        }
        let val = u8::from_str_radix(&hex, 16).unwrap();
        Ok(val as char)
    }

    fn read_template_literal(&mut self) -> Result<TokenKind> {
        self.advance(); // consume `
        let mut s = String::new();
        loop {
            match self.peek_char() {
                Some('`') => {
                    self.advance();
                    return Ok(TokenKind::TemplateLiteral(s));
                }
                Some('$') if self.peek_char_at(1) == Some('{') => {
                    self.advance(); // consume $
                    self.advance(); // consume {
                    self.template_brace_stack.push(0);
                    return Ok(TokenKind::TemplateHead(s));
                }
                Some('\\') => {
                    self.advance();
                    let esc = self.read_escape_sequence()?;
                    s.push(esc);
                }
                Some(_) => {
                    let ch = self.advance().unwrap();
                    s.push(ch);
                }
                None => {
                    return Err(RawJsError::syntax_error(
                        "Unterminated template literal",
                        Some(self.location()),
                    ));
                }
            }
        }
    }

    fn read_template_middle_or_tail(&mut self) -> Result<TokenKind> {
        let mut s = String::new();
        loop {
            match self.peek_char() {
                Some('`') => {
                    self.advance();
                    return Ok(TokenKind::TemplateTail(s));
                }
                Some('$') if self.peek_char_at(1) == Some('{') => {
                    self.advance(); // consume $
                    self.advance(); // consume {
                    return Ok(TokenKind::TemplateMiddle(s));
                }
                Some('\\') => {
                    self.advance();
                    let esc = self.read_escape_sequence()?;
                    s.push(esc);
                }
                Some(_) => {
                    let ch = self.advance().unwrap();
                    s.push(ch);
                }
                None => {
                    return Err(RawJsError::syntax_error(
                        "Unterminated template literal",
                        Some(self.location()),
                    ));
                }
            }
        }
    }

    fn read_identifier_or_keyword(&mut self) -> Result<TokenKind> {
        let mut name = String::new();
        while let Some(ch) = self.peek_char() {
            if is_id_continue(ch) {
                name.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let kind = match name.as_str() {
            "var" => TokenKind::Var,
            "let" => TokenKind::Let,
            "const" => TokenKind::Const,
            "function" => TokenKind::Function,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "do" => TokenKind::Do,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "switch" => TokenKind::Switch,
            "case" => TokenKind::Case,
            "default" => TokenKind::Default,
            "throw" => TokenKind::Throw,
            "try" => TokenKind::Try,
            "catch" => TokenKind::Catch,
            "finally" => TokenKind::Finally,
            "new" => TokenKind::New,
            "delete" => TokenKind::Delete,
            "typeof" => TokenKind::Typeof,
            "void" => TokenKind::Void,
            "in" => TokenKind::In,
            "instanceof" => TokenKind::Instanceof,
            "this" => TokenKind::This,
            "true" => TokenKind::Boolean(true),
            "false" => TokenKind::Boolean(false),
            "null" => TokenKind::Null,
            "class" => TokenKind::Class,
            "extends" => TokenKind::Extends,
            "super" => TokenKind::Super,
            "import" => TokenKind::Import,
            "export" => TokenKind::Export,
            "of" => TokenKind::Of,
            "with" => TokenKind::With,
            "debugger" => TokenKind::Debugger,
            "yield" => TokenKind::Yield,
            "async" => TokenKind::Async,
            "await" => TokenKind::Await,
            _ => TokenKind::Identifier(name),
        };

        Ok(kind)
    }
}

fn is_id_start(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_' || ch == '$'
}

fn is_id_continue(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_' || ch == '$'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let mut lexer = Lexer::new("let x = 42;");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Identifier("x".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::Assign);
        assert_eq!(tokens[3].kind, TokenKind::Number(42.0));
        assert_eq!(tokens[4].kind, TokenKind::Semicolon);
        assert_eq!(tokens[5].kind, TokenKind::Eof);
    }

    #[test]
    fn test_string_literal() {
        let mut lexer = Lexer::new(r#""hello world""#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::String("hello world".to_string()));
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ - * / === !== <= >=");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Plus);
        assert_eq!(tokens[1].kind, TokenKind::Minus);
        assert_eq!(tokens[2].kind, TokenKind::Star);
        assert_eq!(tokens[3].kind, TokenKind::Slash);
        assert_eq!(tokens[4].kind, TokenKind::StrictEqual);
        assert_eq!(tokens[5].kind, TokenKind::StrictNotEqual);
        assert_eq!(tokens[6].kind, TokenKind::LessEqual);
        assert_eq!(tokens[7].kind, TokenKind::GreaterEqual);
    }

    #[test]
    fn test_line_breaks() {
        let mut lexer = Lexer::new("a\nb");
        let tokens = lexer.tokenize().unwrap();
        assert!(!tokens[0].had_line_break_before);
        assert!(tokens[1].had_line_break_before);
    }
}
