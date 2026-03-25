impl Parser {
    fn parse_import_primary_expression(&mut self) -> Result<Expression> {
        if self.pos + 1 < self.tokens.len() && self.tokens[self.pos + 1].kind == TokenKind::Dot {
            return self.parse_import_meta_expression();
        }
        self.parse_import_expression()
    }
}

impl Parser {
    fn parse_import_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        self.expect(&TokenKind::Import)?;
        self.expect(&TokenKind::LParen)?;
        let arguments = self.parse_arguments()?;
        self.expect(&TokenKind::RParen)?;
        if arguments.len() != 1 {
            return Err(RawJsError::syntax_error(
                "import() expects exactly one argument",
                Some(loc),
            ));
        }
        let source = match arguments.into_iter().next().unwrap() {
            Expression::Spread(_) => {
                return Err(RawJsError::syntax_error(
                    "import() does not allow spread arguments",
                    Some(loc),
                ))
            }
            source => source,
        };
        Ok(Expression::Import(ImportExpression {
            source: Box::new(source),
            location: loc,
        }))
    }
}

impl Parser {
    fn parse_import_meta_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        self.expect(&TokenKind::Import)?;
        self.expect(&TokenKind::Dot)?;
        match self.expect_identifier()? {
            name if name == "meta" => Ok(Expression::ImportMeta(loc)),
            _ => Err(RawJsError::syntax_error(
                "Expected 'meta' after 'import.'",
                Some(loc),
            )),
        }
    }
}

impl Parser {
    fn parse_array_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        self.expect(&TokenKind::LBracket)?;
        let mut elements = Vec::new();
        while !self.at(&TokenKind::RBracket) && !self.at(&TokenKind::Eof) {
            if self.at(&TokenKind::Comma) {
                self.advance();
                elements.push(None);
                continue;
            }
            if self.at(&TokenKind::Ellipsis) {
                let spread_loc = self.location();
                self.advance();
                let arg = self.parse_assignment_expression()?;
                elements.push(Some(Expression::Spread(SpreadExpression {
                    argument: Box::new(arg),
                    location: spread_loc,
                })));
            } else {
                elements.push(Some(self.parse_assignment_expression()?));
            }
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        self.expect(&TokenKind::RBracket)?;
        Ok(Expression::ArrayExpression(ArrayExpression {
            elements,
            location: loc,
        }))
    }
}
