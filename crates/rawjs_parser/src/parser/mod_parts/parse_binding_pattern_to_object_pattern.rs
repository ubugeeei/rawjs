impl Parser {
    pub(crate) fn parse_binding_pattern(&mut self) -> Result<Pattern> {
        match self.peek() {
            TokenKind::LBracket => self.parse_array_pattern(),
            TokenKind::LBrace => self.parse_object_pattern(),
            _ => {
                let loc = self.location();
                let name = self.expect_identifier()?;
                Ok(Pattern::Identifier(IdentifierPattern {
                    name,
                    location: loc,
                }))
            }
        }
    }
}

impl Parser {
    pub(crate) fn parse_array_pattern(&mut self) -> Result<Pattern> {
        let loc = self.location();
        self.expect(&TokenKind::LBracket)?;
        let mut elements = Vec::new();
        while !self.at(&TokenKind::RBracket) && !self.at(&TokenKind::Eof) {
            if self.eat(&TokenKind::Comma) {
                elements.push(None);
                continue;
            }
            if self.at(&TokenKind::Ellipsis) {
                let rest_loc = self.location();
                self.advance();
                let argument = self.parse_binding_pattern()?;
                elements.push(Some(Pattern::Rest(RestPattern {
                    argument: Box::new(argument),
                    location: rest_loc,
                })));
                break;
            }
            let mut pat = self.parse_binding_pattern()?;
            if self.eat(&TokenKind::Assign) {
                let right = self.parse_assignment_expression()?;
                let pat_loc = loc;
                pat = Pattern::Assignment(AssignmentPattern {
                    left: Box::new(pat),
                    right: Box::new(right),
                    location: pat_loc,
                });
            }
            elements.push(Some(pat));
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        self.expect(&TokenKind::RBracket)?;
        Ok(Pattern::Array(ArrayPattern {
            elements,
            location: loc,
        }))
    }
}

impl Parser {
    pub(crate) fn parse_object_pattern(&mut self) -> Result<Pattern> {
        let loc = self.location();
        self.expect(&TokenKind::LBrace)?;
        let mut properties = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            if self.at(&TokenKind::Ellipsis) {
                let rest_loc = self.location();
                self.advance();
                let argument = self.parse_binding_pattern()?;
                properties.push(ObjectPatternProperty {
                    key: Expression::Identifier(IdentifierExpression {
                        name: String::new(),
                        location: rest_loc,
                    }),
                    value: Pattern::Rest(RestPattern {
                        argument: Box::new(argument),
                        location: rest_loc,
                    }),
                    computed: false,
                    shorthand: false,
                    location: rest_loc,
                });
                break;
            }
            let prop_loc = self.location();
            let computed = self.at(&TokenKind::LBracket);
            let key = if computed {
                self.advance();
                let k = self.parse_assignment_expression()?;
                self.expect(&TokenKind::RBracket)?;
                k
            } else {
                self.parse_property_name()?
            };
            if self.eat(&TokenKind::Colon) {
                let mut value = self.parse_binding_pattern()?;
                if self.eat(&TokenKind::Assign) {
                    let right = self.parse_assignment_expression()?;
                    value = Pattern::Assignment(AssignmentPattern {
                        left: Box::new(value),
                        right: Box::new(right),
                        location: prop_loc,
                    });
                }
                properties.push(ObjectPatternProperty {
                    key,
                    value,
                    computed,
                    shorthand: false,
                    location: prop_loc,
                });
            } else {
                let name = match &key {
                    Expression::Identifier(id) => id.name.clone(),
                    _ => {
                        return Err(RawJsError::syntax_error(
                            "Invalid shorthand property pattern",
                            Some(prop_loc),
                        ))
                    }
                };
                let mut value = Pattern::Identifier(IdentifierPattern {
                    name,
                    location: prop_loc,
                });
                if self.eat(&TokenKind::Assign) {
                    let right = self.parse_assignment_expression()?;
                    value = Pattern::Assignment(AssignmentPattern {
                        left: Box::new(value),
                        right: Box::new(right),
                        location: prop_loc,
                    });
                }
                properties.push(ObjectPatternProperty {
                    key,
                    value,
                    computed: false,
                    shorthand: true,
                    location: prop_loc,
                });
            }
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(Pattern::Object(ObjectPattern {
            properties,
            location: loc,
        }))
    }
}
