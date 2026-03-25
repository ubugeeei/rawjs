impl Parser {
    pub(crate) fn expect_semicolon(&mut self) -> Result<()> {
        if self.eat(&TokenKind::Semicolon) {
            return Ok(());
        }
        if self.at(&TokenKind::RBrace) || self.at(&TokenKind::Eof) {
            return Ok(());
        }
        if self.current_token().had_line_break_before {
            return Ok(());
        }
        Err(RawJsError::syntax_error(
            format!("Expected semicolon, got {:?}", self.peek()),
            Some(self.location()),
        ))
    }
}

impl Parser {
    pub(crate) fn expect_identifier(&mut self) -> Result<String> {
        match self.peek().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(name)
            }
            _ => Err(RawJsError::syntax_error(
                format!("Expected identifier, got {:?}", self.peek()),
                Some(self.location()),
            )),
        }
    }
}

impl Parser {
    pub(crate) fn expect_identifier_or_keyword(&mut self) -> Result<String> {
        match self.peek().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(name)
            }
            TokenKind::In => {
                self.advance();
                Ok("in".to_string())
            }
            TokenKind::Instanceof => {
                self.advance();
                Ok("instanceof".to_string())
            }
            TokenKind::Typeof => {
                self.advance();
                Ok("typeof".to_string())
            }
            TokenKind::Void => {
                self.advance();
                Ok("void".to_string())
            }
            TokenKind::Delete => {
                self.advance();
                Ok("delete".to_string())
            }
            TokenKind::Return => {
                self.advance();
                Ok("return".to_string())
            }
            TokenKind::If => {
                self.advance();
                Ok("if".to_string())
            }
            TokenKind::Else => {
                self.advance();
                Ok("else".to_string())
            }
            TokenKind::While => {
                self.advance();
                Ok("while".to_string())
            }
            TokenKind::For => {
                self.advance();
                Ok("for".to_string())
            }
            TokenKind::Do => {
                self.advance();
                Ok("do".to_string())
            }
            TokenKind::Switch => {
                self.advance();
                Ok("switch".to_string())
            }
            TokenKind::Case => {
                self.advance();
                Ok("case".to_string())
            }
            TokenKind::Default => {
                self.advance();
                Ok("default".to_string())
            }
            TokenKind::Break => {
                self.advance();
                Ok("break".to_string())
            }
            TokenKind::Continue => {
                self.advance();
                Ok("continue".to_string())
            }
            TokenKind::Try => {
                self.advance();
                Ok("try".to_string())
            }
            TokenKind::Catch => {
                self.advance();
                Ok("catch".to_string())
            }
            TokenKind::Finally => {
                self.advance();
                Ok("finally".to_string())
            }
            TokenKind::Throw => {
                self.advance();
                Ok("throw".to_string())
            }
            TokenKind::New => {
                self.advance();
                Ok("new".to_string())
            }
            TokenKind::This => {
                self.advance();
                Ok("this".to_string())
            }
            TokenKind::Class => {
                self.advance();
                Ok("class".to_string())
            }
            TokenKind::Extends => {
                self.advance();
                Ok("extends".to_string())
            }
            TokenKind::Super => {
                self.advance();
                Ok("super".to_string())
            }
            TokenKind::Import => {
                self.advance();
                Ok("import".to_string())
            }
            TokenKind::Export => {
                self.advance();
                Ok("export".to_string())
            }
            TokenKind::Var => {
                self.advance();
                Ok("var".to_string())
            }
            TokenKind::Let => {
                self.advance();
                Ok("let".to_string())
            }
            TokenKind::Const => {
                self.advance();
                Ok("const".to_string())
            }
            TokenKind::Function => {
                self.advance();
                Ok("function".to_string())
            }
            TokenKind::With => {
                self.advance();
                Ok("with".to_string())
            }
            TokenKind::Debugger => {
                self.advance();
                Ok("debugger".to_string())
            }
            TokenKind::Yield => {
                self.advance();
                Ok("yield".to_string())
            }
            TokenKind::Async => {
                self.advance();
                Ok("async".to_string())
            }
            TokenKind::Await => {
                self.advance();
                Ok("await".to_string())
            }
            TokenKind::Of => {
                self.advance();
                Ok("of".to_string())
            }
            TokenKind::Boolean(true) => {
                self.advance();
                Ok("true".to_string())
            }
            TokenKind::Boolean(false) => {
                self.advance();
                Ok("false".to_string())
            }
            TokenKind::Null => {
                self.advance();
                Ok("null".to_string())
            }
            _ => Err(RawJsError::syntax_error(
                format!("Expected identifier, got {:?}", self.peek()),
                Some(self.location()),
            )),
        }
    }
}

impl Parser {
    pub fn parse_program(&mut self) -> Result<Program> {
        let loc = self.location();
        let mut body = Vec::new();
        while !self.at(&TokenKind::Eof) {
            body.push(self.parse_statement()?);
        }
        Ok(Program {
            body,
            location: loc,
        })
    }
}
