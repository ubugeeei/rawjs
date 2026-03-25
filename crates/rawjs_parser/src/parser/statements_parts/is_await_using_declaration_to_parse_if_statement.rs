impl Parser {
    #[doc = " Check if current `await` token starts an `await using` declaration."]
    fn is_await_using_declaration(&self) -> bool {
        if self.pos + 2 >= self.tokens.len() {
            return false;
        }
        matches ! (& self . tokens [self . pos + 1] . kind , TokenKind :: Identifier (name) if name == "using")
            && !self.tokens[self.pos + 1].had_line_break_before
            && matches!(self.tokens[self.pos + 2].kind, TokenKind::Identifier(_))
            && !self.tokens[self.pos + 2].had_line_break_before
    }
}

impl Parser {
    #[doc = " Parse a `using x = expr;` or `await using x = expr;` declaration."]
    fn parse_using_declaration(&mut self, is_await: bool) -> Result<Statement> {
        let loc = self.location();
        if is_await {
            self.advance();
        }
        self.advance();
        let kind = if is_await {
            VarKind::AwaitUsing
        } else {
            VarKind::Using
        };
        let mut declarations = Vec::new();
        loop {
            let decl_loc = self.location();
            let id = self.parse_binding_pattern()?;
            if !self.eat(&TokenKind::Assign) {
                return Err(RawJsError::syntax_error(
                    "Missing initializer in 'using' declaration",
                    Some(decl_loc),
                ));
            }
            let init = Some(self.parse_assignment_expression()?);
            declarations.push(VariableDeclarator {
                id,
                init,
                location: decl_loc,
            });
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        self.expect_semicolon()?;
        Ok(Statement::VariableDeclaration(VariableDeclaration {
            kind,
            declarations,
            location: loc,
        }))
    }
}

impl Parser {
    fn parse_variable_declaration_statement(&mut self) -> Result<Statement> {
        let decl = self.parse_variable_declaration()?;
        self.expect_semicolon()?;
        Ok(Statement::VariableDeclaration(decl))
    }
}

impl Parser {
    pub(crate) fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration> {
        let loc = self.location();
        let kind = match self.advance().kind {
            TokenKind::Var => VarKind::Var,
            TokenKind::Let => VarKind::Let,
            TokenKind::Const => VarKind::Const,
            _ => unreachable!(),
        };
        let mut declarations = Vec::new();
        loop {
            let decl_loc = self.location();
            let id = self.parse_binding_pattern()?;
            let init = if self.eat(&TokenKind::Assign) {
                Some(self.parse_assignment_expression()?)
            } else {
                None
            };
            declarations.push(VariableDeclarator {
                id,
                init,
                location: decl_loc,
            });
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(VariableDeclaration {
            kind,
            declarations,
            location: loc,
        })
    }
}

impl Parser {
    pub(crate) fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration> {
        let loc = self.location();
        self.expect(&TokenKind::Function)?;
        let is_generator = self.eat(&TokenKind::Star);
        let id = if let TokenKind::Identifier(_) = self.peek() {
            Some(self.expect_identifier()?)
        } else {
            None
        };
        self.expect(&TokenKind::LParen)?;
        let params = self.parse_formal_parameters()?;
        self.expect(&TokenKind::RParen)?;
        let body = self.parse_block_statement()?;
        Ok(FunctionDeclaration {
            id,
            params,
            body: Box::new(body),
            is_async: false,
            is_generator,
            location: loc,
        })
    }
}

impl Parser {
    fn parse_async_function_declaration(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.advance();
        self.expect(&TokenKind::Function)?;
        let is_generator = self.eat(&TokenKind::Star);
        let id = if let TokenKind::Identifier(_) = self.peek() {
            Some(self.expect_identifier()?)
        } else {
            None
        };
        self.expect(&TokenKind::LParen)?;
        let params = self.parse_formal_parameters()?;
        self.expect(&TokenKind::RParen)?;
        let body = self.parse_block_statement()?;
        Ok(Statement::FunctionDeclaration(FunctionDeclaration {
            id,
            params,
            body: Box::new(body),
            is_async: true,
            is_generator,
            location: loc,
        }))
    }
}

impl Parser {
    fn parse_if_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::If)?;
        self.expect(&TokenKind::LParen)?;
        let test = self.parse_expression()?;
        self.expect(&TokenKind::RParen)?;
        let consequent = Box::new(self.parse_statement()?);
        let alternate = if self.eat(&TokenKind::Else) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };
        Ok(Statement::If(IfStatement {
            test,
            consequent,
            alternate,
            location: loc,
        }))
    }
}
