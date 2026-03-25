impl Parser {
    fn parse_while_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::While)?;
        self.expect(&TokenKind::LParen)?;
        let test = self.parse_expression()?;
        self.expect(&TokenKind::RParen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Statement::While(WhileStatement {
            test,
            body,
            location: loc,
        }))
    }
}

impl Parser {
    fn parse_do_while_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Do)?;
        let body = Box::new(self.parse_statement()?);
        self.expect(&TokenKind::While)?;
        self.expect(&TokenKind::LParen)?;
        let test = self.parse_expression()?;
        self.expect(&TokenKind::RParen)?;
        self.expect_semicolon()?;
        Ok(Statement::DoWhile(DoWhileStatement {
            test,
            body,
            location: loc,
        }))
    }
}

impl Parser {
    fn parse_for_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::For)?;
        self.expect(&TokenKind::LParen)?;
        if self.at(&TokenKind::Semicolon) {
            self.advance();
            let test = if self.at(&TokenKind::Semicolon) {
                None
            } else {
                Some(self.parse_expression()?)
            };
            self.expect(&TokenKind::Semicolon)?;
            let update = if self.at(&TokenKind::RParen) {
                None
            } else {
                Some(self.parse_expression()?)
            };
            self.expect(&TokenKind::RParen)?;
            let body = Box::new(self.parse_statement()?);
            return Ok(Statement::For(ForStatement {
                init: None,
                test,
                update,
                body,
                location: loc,
            }));
        }
        if self.at(&TokenKind::Var) || self.at(&TokenKind::Let) || self.at(&TokenKind::Const) {
            let decl = self.parse_variable_declaration()?;
            if self.at(&TokenKind::In) {
                self.advance();
                let right = self.parse_expression()?;
                self.expect(&TokenKind::RParen)?;
                let body = Box::new(self.parse_statement()?);
                return Ok(Statement::ForIn(ForInStatement {
                    left: ForInOfLeft::VariableDeclaration(decl),
                    right,
                    body,
                    location: loc,
                }));
            }
            if self.at_of() {
                self.advance();
                let right = self.parse_assignment_expression()?;
                self.expect(&TokenKind::RParen)?;
                let body = Box::new(self.parse_statement()?);
                return Ok(Statement::ForOf(ForOfStatement {
                    left: ForInOfLeft::VariableDeclaration(decl),
                    right,
                    body,
                    is_await: false,
                    location: loc,
                }));
            }
            self.expect(&TokenKind::Semicolon)?;
            let test = if self.at(&TokenKind::Semicolon) {
                None
            } else {
                Some(self.parse_expression()?)
            };
            self.expect(&TokenKind::Semicolon)?;
            let update = if self.at(&TokenKind::RParen) {
                None
            } else {
                Some(self.parse_expression()?)
            };
            self.expect(&TokenKind::RParen)?;
            let body = Box::new(self.parse_statement()?);
            return Ok(Statement::For(ForStatement {
                init: Some(ForInit::VariableDeclaration(decl)),
                test,
                update,
                body,
                location: loc,
            }));
        }
        let expr = self.parse_expression_no_in()?;
        if self.at(&TokenKind::In) {
            self.advance();
            let right = self.parse_expression()?;
            self.expect(&TokenKind::RParen)?;
            let body = Box::new(self.parse_statement()?);
            let left = ForInOfLeft::Pattern(self.expression_to_pattern(expr)?);
            return Ok(Statement::ForIn(ForInStatement {
                left,
                right,
                body,
                location: loc,
            }));
        }
        if self.at_of() {
            self.advance();
            let right = self.parse_assignment_expression()?;
            self.expect(&TokenKind::RParen)?;
            let body = Box::new(self.parse_statement()?);
            let left = ForInOfLeft::Pattern(self.expression_to_pattern(expr)?);
            return Ok(Statement::ForOf(ForOfStatement {
                left,
                right,
                body,
                is_await: false,
                location: loc,
            }));
        }
        self.expect(&TokenKind::Semicolon)?;
        let test = if self.at(&TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.expect(&TokenKind::Semicolon)?;
        let update = if self.at(&TokenKind::RParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.expect(&TokenKind::RParen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Statement::For(ForStatement {
            init: Some(ForInit::Expression(expr)),
            test,
            update,
            body,
            location: loc,
        }))
    }
}
