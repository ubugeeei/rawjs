impl Parser {
    fn parse_arguments(&mut self) -> Result<Vec<Expression>> {
        let mut args = Vec::new();
        while !self.at(&TokenKind::RParen) && !self.at(&TokenKind::Eof) {
            if self.at(&TokenKind::Ellipsis) {
                let loc = self.location();
                self.advance();
                let arg = self.parse_assignment_expression()?;
                args.push(Expression::Spread(SpreadExpression {
                    argument: Box::new(arg),
                    location: loc,
                }));
            } else {
                args.push(self.parse_assignment_expression()?);
            }
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(args)
    }
}

impl Parser {
    fn parse_primary_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        match self.peek().clone() {
            TokenKind::Number(n) => {
                self.advance();
                Ok(Expression::NumberLiteral(NumberLiteral {
                    value: n,
                    location: loc,
                }))
            }
            TokenKind::String(s) => {
                self.advance();
                Ok(Expression::StringLiteral(StringLiteral {
                    value: s,
                    location: loc,
                }))
            }
            TokenKind::Boolean(b) => {
                self.advance();
                Ok(Expression::BooleanLiteral(BooleanLiteral {
                    value: b,
                    location: loc,
                }))
            }
            TokenKind::Null => {
                self.advance();
                Ok(Expression::NullLiteral(loc))
            }
            TokenKind::TemplateLiteral(s) => {
                self.advance();
                Ok(Expression::TemplateLiteral(TemplateLiteralExpr {
                    quasis: vec![s],
                    expressions: vec![],
                    location: loc,
                }))
            }
            TokenKind::TemplateHead(s) => {
                self.advance();
                let mut quasis = vec![s];
                let mut expressions = Vec::new();
                loop {
                    expressions.push(self.parse_expression()?);
                    match self.peek().clone() {
                        TokenKind::TemplateTail(s) => {
                            self.advance();
                            quasis.push(s);
                            break;
                        }
                        TokenKind::TemplateMiddle(s) => {
                            self.advance();
                            quasis.push(s);
                        }
                        _ => {
                            return Err(RawJsError::syntax_error(
                                format!("Expected template continuation, got {:?}", self.peek()),
                                Some(self.location()),
                            ));
                        }
                    }
                }
                Ok(Expression::TemplateLiteral(TemplateLiteralExpr {
                    quasis,
                    expressions,
                    location: loc,
                }))
            }
            TokenKind::This => {
                self.advance();
                Ok(Expression::This(loc))
            }
            TokenKind::Super => {
                self.advance();
                Ok(Expression::Super(loc))
            }
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(Expression::Identifier(IdentifierExpression {
                    name,
                    location: loc,
                }))
            }
            TokenKind::Import => self.parse_import_primary_expression(),
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(&TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::LBracket => self.parse_array_expression(),
            TokenKind::LBrace => self.parse_object_expression(),
            TokenKind::Function => {
                let func = self.parse_function_declaration()?;
                Ok(Expression::FunctionExpression(func))
            }
            TokenKind::Class => {
                let cls = self.parse_class_declaration()?;
                Ok(Expression::ClassExpression(cls))
            }
            TokenKind::Async => {
                if self.pos + 1 < self.tokens.len()
                    && self.tokens[self.pos + 1].kind == TokenKind::Function
                    && !self.tokens[self.pos + 1].had_line_break_before
                {
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
                    Ok(Expression::FunctionExpression(
                        rawjs_ast::FunctionDeclaration {
                            id,
                            params,
                            body: Box::new(body),
                            is_async: true,
                            is_generator,
                            location: loc,
                        },
                    ))
                } else {
                    self.advance();
                    Ok(Expression::Identifier(IdentifierExpression {
                        name: "async".to_string(),
                        location: loc,
                    }))
                }
            }
            TokenKind::Yield => {
                self.advance();
                let delegate = self.eat(&TokenKind::Star);
                let argument = if !self.at(&TokenKind::Semicolon)
                    && !self.at(&TokenKind::RBrace)
                    && !self.at(&TokenKind::RParen)
                    && !self.at(&TokenKind::RBracket)
                    && !self.at(&TokenKind::Comma)
                    && !self.at(&TokenKind::Colon)
                    && !self.at(&TokenKind::Eof)
                    && !self.current_token().had_line_break_before
                {
                    Some(Box::new(self.parse_assignment_expression()?))
                } else {
                    None
                };
                Ok(Expression::Yield(YieldExpression {
                    argument,
                    delegate,
                    location: loc,
                }))
            }
            _ => Err(RawJsError::syntax_error(
                format!("Unexpected token: {:?}", self.peek()),
                Some(loc),
            )),
        }
    }
}
