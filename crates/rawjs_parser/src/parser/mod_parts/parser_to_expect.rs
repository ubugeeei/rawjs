use rawjs_ast::*;

use rawjs_common::{RawJsError, Result, SourceLocation};

use rawjs_lexer::{Lexer, Token, TokenKind};

pub struct Parser {
    pub(crate) tokens: Vec<Token>,
    pub(crate) pos: usize,
    pub(crate) allow_in: bool,
}

impl Parser {
    pub fn new(source: &str) -> Result<Self> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize()?;
        Ok(Self {
            tokens,
            pos: 0,
            allow_in: true,
        })
    }
}

impl Parser {
    pub(crate) fn peek(&self) -> &TokenKind {
        &self.tokens[self.pos].kind
    }
}

impl Parser {
    pub(crate) fn current_token(&self) -> &Token {
        &self.tokens[self.pos]
    }
}

impl Parser {
    pub(crate) fn location(&self) -> SourceLocation {
        self.tokens[self.pos].location
    }
}

impl Parser {
    pub(crate) fn advance(&mut self) -> Token {
        let token = self.tokens[self.pos].clone();
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
        token
    }
}

impl Parser {
    pub(crate) fn at(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(kind)
    }
}

impl Parser {
    pub(crate) fn eat(&mut self, kind: &TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }
}

impl Parser {
    pub(crate) fn expect(&mut self, kind: &TokenKind) -> Result<Token> {
        if self.at(kind) {
            Ok(self.advance())
        } else {
            Err(RawJsError::syntax_error(
                format!("Expected {:?}, got {:?}", kind, self.peek()),
                Some(self.location()),
            ))
        }
    }
}
