impl Parser {
    pub(super) fn parse_expression_no_in(&mut self) -> Result<Expression> {
        let allow_in = self.allow_in;
        self.allow_in = false;
        let result = self.parse_assignment_expression();
        self.allow_in = allow_in;
        result
    }
}

impl Parser {
    pub(super) fn parse_return_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Return)?;
        let argument = if self.at(&TokenKind::Semicolon)
            || self.at(&TokenKind::RBrace)
            || self.at(&TokenKind::Eof)
            || self.current_token().had_line_break_before
        {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.expect_semicolon()?;
        Ok(Statement::Return(ReturnStatement {
            argument,
            location: loc,
        }))
    }
}

impl Parser {
    pub(super) fn parse_break_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Break)?;
        let label = if !self.at(&TokenKind::Semicolon)
            && !self.at(&TokenKind::RBrace)
            && !self.at(&TokenKind::Eof)
            && !self.current_token().had_line_break_before
        {
            if let TokenKind::Identifier(_) = self.peek() {
                Some(self.expect_identifier()?)
            } else {
                None
            }
        } else {
            None
        };
        self.expect_semicolon()?;
        Ok(Statement::Break(BreakStatement {
            label,
            location: loc,
        }))
    }
}

impl Parser {
    pub(super) fn parse_continue_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Continue)?;
        let label = if !self.at(&TokenKind::Semicolon)
            && !self.at(&TokenKind::RBrace)
            && !self.at(&TokenKind::Eof)
            && !self.current_token().had_line_break_before
        {
            if let TokenKind::Identifier(_) = self.peek() {
                Some(self.expect_identifier()?)
            } else {
                None
            }
        } else {
            None
        };
        self.expect_semicolon()?;
        Ok(Statement::Continue(ContinueStatement {
            label,
            location: loc,
        }))
    }
}

impl Parser {
    pub(super) fn parse_switch_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Switch)?;
        self.expect(&TokenKind::LParen)?;
        let discriminant = self.parse_expression()?;
        self.expect(&TokenKind::RParen)?;
        self.expect(&TokenKind::LBrace)?;
        let mut cases = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            let case_loc = self.location();
            let test = if self.eat(&TokenKind::Case) {
                Some(self.parse_expression()?)
            } else {
                self.expect(&TokenKind::Default)?;
                None
            };
            self.expect(&TokenKind::Colon)?;
            let mut consequent = Vec::new();
            while !self.at(&TokenKind::Case)
                && !self.at(&TokenKind::Default)
                && !self.at(&TokenKind::RBrace)
                && !self.at(&TokenKind::Eof)
            {
                consequent.push(self.parse_statement()?);
            }
            cases.push(SwitchCase {
                test,
                consequent,
                location: case_loc,
            });
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(Statement::Switch(SwitchStatement {
            discriminant,
            cases,
            location: loc,
        }))
    }
}

impl Parser {
    pub(super) fn parse_throw_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Throw)?;
        if self.current_token().had_line_break_before {
            return Err(RawJsError::syntax_error(
                "Illegal newline after throw",
                Some(loc),
            ));
        }
        let argument = self.parse_expression()?;
        self.expect_semicolon()?;
        Ok(Statement::Throw(ThrowStatement {
            argument,
            location: loc,
        }))
    }
}

impl Parser {
    pub(super) fn parse_try_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Try)?;
        let block = self.parse_block_statement()?;
        let handler = if self.eat(&TokenKind::Catch) {
            let catch_loc = self.location();
            let param = if self.eat(&TokenKind::LParen) {
                let p = self.parse_binding_pattern()?;
                self.expect(&TokenKind::RParen)?;
                Some(p)
            } else {
                None
            };
            let body = self.parse_block_statement()?;
            Some(CatchClause {
                param,
                body,
                location: catch_loc,
            })
        } else {
            None
        };
        let finalizer = if self.eat(&TokenKind::Finally) {
            Some(self.parse_block_statement()?)
        } else {
            None
        };
        Ok(Statement::Try(TryStatement {
            block,
            handler,
            finalizer,
            location: loc,
        }))
    }
}

use super::*;
