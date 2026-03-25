impl Parser {
    pub(crate) fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let loc = self.location();
        self.expect(&TokenKind::LBrace)?;
        let mut body = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            body.push(self.parse_statement()?);
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(BlockStatement {
            body,
            location: loc,
        })
    }
}

impl Parser {
    pub(super) fn parse_labeled_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        let label = self.expect_identifier()?;
        self.expect(&TokenKind::Colon)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Statement::Labeled(LabeledStatement {
            label,
            body,
            location: loc,
        }))
    }
}

impl Parser {
    pub(super) fn parse_with_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::With)?;
        self.expect(&TokenKind::LParen)?;
        let object = self.parse_expression()?;
        self.expect(&TokenKind::RParen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Statement::With(WithStatement {
            object,
            body,
            location: loc,
        }))
    }
}

impl Parser {
    pub(crate) fn parse_class_declaration(&mut self) -> Result<ClassDeclaration> {
        let loc = self.location();
        self.expect(&TokenKind::Class)?;
        let id = if let TokenKind::Identifier(_) = self.peek() {
            Some(self.expect_identifier()?)
        } else {
            None
        };
        let super_class = if self.eat(&TokenKind::Extends) {
            Some(Box::new(self.parse_left_hand_side_expression()?))
        } else {
            None
        };
        self.expect(&TokenKind::LBrace)?;
        let mut body = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            if self.eat(&TokenKind::Semicolon) {
                continue;
            }
            body.push(self.parse_class_member()?);
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(ClassDeclaration {
            id,
            super_class,
            body,
            location: loc,
        })
    }
}

impl Parser {
    pub(super) fn parse_class_member(&mut self) -> Result<ClassMember> {
        let loc = self.location();
        let is_static = if let TokenKind::Identifier(name) = self.peek() {
            if name == "static" {
                self.advance();
                true
            } else {
                false
            }
        } else {
            false
        };
        let mut kind = ClassMemberKind::Method;
        let is_async_method = self.is_async_method_start();
        if is_async_method {
            self.advance();
        }
        let is_generator = self.eat(&TokenKind::Star);
        if !is_async_method && !is_generator {
            if let TokenKind::Identifier(name) = self.peek() {
                if name == "get" && !self.is_next_lparen() {
                    self.advance();
                    kind = ClassMemberKind::Get;
                } else if name == "set" && !self.is_next_lparen() {
                    self.advance();
                    kind = ClassMemberKind::Set;
                }
            }
        }
        let mut computed = self.at(&TokenKind::LBracket);
        let key = if is_async_method && !is_generator && self.at(&TokenKind::LParen) {
            computed = false;
            Expression::Identifier(IdentifierExpression {
                name: "async".to_string(),
                location: loc,
            })
        } else if computed {
            self.advance();
            let k = self.parse_assignment_expression()?;
            self.expect(&TokenKind::RBracket)?;
            k
        } else {
            self.parse_property_name()?
        };
        if let Expression::Identifier(ref id) = key {
            if id.name == "constructor"
                && kind == ClassMemberKind::Method
                && !is_async_method
                && !is_generator
            {
                kind = ClassMemberKind::Constructor;
            }
        }
        let value = self.parse_method_expression(loc, is_async_method, is_generator)?;
        Ok(ClassMember {
            key,
            value: Some(value),
            kind,
            is_static,
            computed,
            location: loc,
        })
    }
}

impl Parser {
    pub(crate) fn parse_expression_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        let expression = self.parse_expression()?;
        self.expect_semicolon()?;
        Ok(Statement::Expression(ExpressionStatement {
            expression,
            location: loc,
        }))
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
