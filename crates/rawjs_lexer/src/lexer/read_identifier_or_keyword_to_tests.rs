impl Lexer {
    pub(super) fn read_identifier_or_keyword(&mut self) -> Result<TokenKind> {
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

pub(super) fn is_id_start(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_' || ch == '$'
}

pub(super) fn is_id_continue(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_' || ch == '$'
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    pub(super) fn test_basic_tokens() {
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
    pub(super) fn test_string_literal() {
        let mut lexer = Lexer::new(r#""hello world""#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::String("hello world".to_string()));
    }
    #[test]
    pub(super) fn test_operators() {
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
    pub(super) fn test_line_breaks() {
        let mut lexer = Lexer::new("a\nb");
        let tokens = lexer.tokenize().unwrap();
        assert!(!tokens[0].had_line_break_before);
        assert!(tokens[1].had_line_break_before);
    }
}

use super::*;
