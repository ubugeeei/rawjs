use rawjs_ast::*;

use rawjs_common::Result;

use rawjs_lexer::TokenKind;

use super::Parser;

impl Parser {
    pub(crate) fn parse_expression(&mut self) -> Result<Expression> {
        let expr = self.parse_assignment_expression()?;
        if self.at(&TokenKind::Comma) {
            let loc = self.location();
            let mut expressions = vec![expr];
            while self.eat(&TokenKind::Comma) {
                expressions.push(self.parse_assignment_expression()?);
            }
            Ok(Expression::Sequence(SequenceExpression {
                expressions,
                location: loc,
            }))
        } else {
            Ok(expr)
        }
    }
}

impl Parser {
    pub(crate) fn parse_assignment_expression(&mut self) -> Result<Expression> {
        if self.is_arrow_function() {
            return self.parse_arrow_function(false);
        }
        if self.is_async_arrow_function() {
            return self.parse_async_arrow_function();
        }
        let loc = self.location();
        let left = self.parse_conditional_expression()?;
        if self.peek().is_assignment_operator() {
            let op = self.parse_assignment_operator()?;
            let right = self.parse_assignment_expression()?;
            Ok(Expression::Assignment(AssignmentExpression {
                operator: op,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            }))
        } else {
            Ok(left)
        }
    }
}

impl Parser {
    pub(super) fn is_arrow_function(&self) -> bool {
        if let TokenKind::Identifier(_) = self.peek() {
            if self.pos + 1 < self.tokens.len()
                && self.tokens[self.pos + 1].kind == TokenKind::Arrow
            {
                return true;
            }
        }
        if self.at(&TokenKind::LParen) {
            let mut depth = 0;
            let mut i = self.pos;
            while i < self.tokens.len() {
                match &self.tokens[i].kind {
                    TokenKind::LParen => depth += 1,
                    TokenKind::RParen => {
                        depth -= 1;
                        if depth == 0 {
                            return i + 1 < self.tokens.len()
                                && self.tokens[i + 1].kind == TokenKind::Arrow;
                        }
                    }
                    _ => {}
                }
                i += 1;
            }
        }
        false
    }
}

impl Parser {
    pub(super) fn is_async_arrow_function(&self) -> bool {
        let is_async_kw = matches!(self.peek(), TokenKind::Async)
            || matches ! (self . peek () , TokenKind :: Identifier (ref name) if name == "async");
        if !is_async_kw || self.pos + 1 >= self.tokens.len() {
            return false;
        }
        if let TokenKind::Identifier(_) = &self.tokens[self.pos + 1].kind {
            if self.pos + 2 < self.tokens.len()
                && self.tokens[self.pos + 2].kind == TokenKind::Arrow
                && !self.tokens[self.pos + 1].had_line_break_before
            {
                return true;
            }
        }
        if self.tokens[self.pos + 1].kind == TokenKind::LParen
            && !self.tokens[self.pos + 1].had_line_break_before
        {
            let mut depth = 0;
            let mut i = self.pos + 1;
            while i < self.tokens.len() {
                match &self.tokens[i].kind {
                    TokenKind::LParen => depth += 1,
                    TokenKind::RParen => {
                        depth -= 1;
                        if depth == 0 {
                            return i + 1 < self.tokens.len()
                                && self.tokens[i + 1].kind == TokenKind::Arrow;
                        }
                    }
                    _ => {}
                }
                i += 1;
            }
        }
        false
    }
}

impl Parser {
    pub(super) fn parse_arrow_function(&mut self, is_async: bool) -> Result<Expression> {
        let loc = self.location();
        let params = if self.at(&TokenKind::LParen) {
            self.advance();
            let p = self.parse_formal_parameters()?;
            self.expect(&TokenKind::RParen)?;
            p
        } else {
            let name = self.expect_identifier()?;
            vec![Pattern::Identifier(IdentifierPattern {
                name,
                location: loc,
            })]
        };
        self.expect(&TokenKind::Arrow)?;
        let body = if self.at(&TokenKind::LBrace) {
            ArrowFunctionBody::Block(self.parse_block_statement()?)
        } else {
            ArrowFunctionBody::Expression(Box::new(self.parse_assignment_expression()?))
        };
        Ok(Expression::ArrowFunctionExpression(
            ArrowFunctionExpression {
                params,
                body,
                is_async,
                location: loc,
            },
        ))
    }
}

impl Parser {
    pub(super) fn parse_async_arrow_function(&mut self) -> Result<Expression> {
        self.advance();
        self.parse_arrow_function(true)
    }
}

use super::*;
