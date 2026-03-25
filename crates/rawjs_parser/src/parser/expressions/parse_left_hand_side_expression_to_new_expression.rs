impl Parser {
    pub(crate) fn parse_left_hand_side_expression(&mut self) -> Result<Expression> {
        let mut expr = if self.at(&TokenKind::New) {
            self.parse_new_expression()?
        } else {
            self.parse_primary_expression()?
        };
        loop {
            match self.peek() {
                TokenKind::Dot => {
                    let loc = self.location();
                    self.advance();
                    let prop_loc = self.location();
                    let property = self.expect_identifier_or_keyword()?;
                    expr = Expression::Member(MemberExpression {
                        object: Box::new(expr),
                        property: Box::new(Expression::Identifier(IdentifierExpression {
                            name: property,
                            location: prop_loc,
                        })),
                        computed: false,
                        optional: false,
                        location: loc,
                    });
                }
                TokenKind::LBracket => {
                    let loc = self.location();
                    self.advance();
                    let property = self.parse_expression()?;
                    self.expect(&TokenKind::RBracket)?;
                    expr = Expression::Member(MemberExpression {
                        object: Box::new(expr),
                        property: Box::new(property),
                        computed: true,
                        optional: false,
                        location: loc,
                    });
                }
                TokenKind::LParen => {
                    let loc = self.location();
                    self.advance();
                    let arguments = self.parse_arguments()?;
                    self.expect(&TokenKind::RParen)?;
                    expr = Expression::Call(CallExpression {
                        callee: Box::new(expr),
                        arguments,
                        optional: false,
                        location: loc,
                    });
                }
                TokenKind::OptionalChain => {
                    let loc = self.location();
                    self.advance();
                    if self.at(&TokenKind::LParen) {
                        self.advance();
                        let arguments = self.parse_arguments()?;
                        self.expect(&TokenKind::RParen)?;
                        expr = Expression::Call(CallExpression {
                            callee: Box::new(expr),
                            arguments,
                            optional: true,
                            location: loc,
                        });
                    } else if self.at(&TokenKind::LBracket) {
                        self.advance();
                        let property = self.parse_expression()?;
                        self.expect(&TokenKind::RBracket)?;
                        expr = Expression::Member(MemberExpression {
                            object: Box::new(expr),
                            property: Box::new(property),
                            computed: true,
                            optional: true,
                            location: loc,
                        });
                    } else {
                        let prop_loc = self.location();
                        let property = self.expect_identifier()?;
                        expr = Expression::Member(MemberExpression {
                            object: Box::new(expr),
                            property: Box::new(Expression::Identifier(IdentifierExpression {
                                name: property,
                                location: prop_loc,
                            })),
                            computed: false,
                            optional: true,
                            location: loc,
                        });
                    }
                }
                TokenKind::TemplateLiteral(_) | TokenKind::TemplateHead(_) => {
                    break;
                }
                _ => break,
            }
        }
        Ok(expr)
    }
}

impl Parser {
    pub(super) fn parse_new_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        self.expect(&TokenKind::New)?;
        if self.at(&TokenKind::New) {
            let callee = self.parse_new_expression()?;
            return Ok(Expression::New(NewExpression {
                callee: Box::new(callee),
                arguments: Vec::new(),
                location: loc,
            }));
        }
        let callee = self.parse_primary_expression()?;
        let mut expr = callee;
        loop {
            match self.peek() {
                TokenKind::Dot => {
                    let mloc = self.location();
                    self.advance();
                    let prop_loc = self.location();
                    let property = self.expect_identifier_or_keyword()?;
                    expr = Expression::Member(MemberExpression {
                        object: Box::new(expr),
                        property: Box::new(Expression::Identifier(IdentifierExpression {
                            name: property,
                            location: prop_loc,
                        })),
                        computed: false,
                        optional: false,
                        location: mloc,
                    });
                }
                TokenKind::LBracket => {
                    let mloc = self.location();
                    self.advance();
                    let property = self.parse_expression()?;
                    self.expect(&TokenKind::RBracket)?;
                    expr = Expression::Member(MemberExpression {
                        object: Box::new(expr),
                        property: Box::new(property),
                        computed: true,
                        optional: false,
                        location: mloc,
                    });
                }
                _ => break,
            }
        }
        let arguments = if self.at(&TokenKind::LParen) {
            self.advance();
            let args = self.parse_arguments()?;
            self.expect(&TokenKind::RParen)?;
            args
        } else {
            Vec::new()
        };
        Ok(Expression::New(NewExpression {
            callee: Box::new(expr),
            arguments,
            location: loc,
        }))
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
