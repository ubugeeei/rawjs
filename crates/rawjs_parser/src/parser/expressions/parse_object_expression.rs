impl Parser {
    pub(super) fn parse_object_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        self.expect(&TokenKind::LBrace)?;
        let mut properties = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            if self.at(&TokenKind::Ellipsis) {
                let spread_loc = self.location();
                self.advance();
                let arg = self.parse_assignment_expression()?;
                properties.push(Property {
                    key: Expression::Identifier(IdentifierExpression {
                        name: String::new(),
                        location: spread_loc,
                    }),
                    value: Expression::Spread(SpreadExpression {
                        argument: Box::new(arg),
                        location: spread_loc,
                    }),
                    kind: PropertyKind::Init,
                    computed: false,
                    shorthand: false,
                    location: spread_loc,
                });
                if !self.eat(&TokenKind::Comma) {
                    break;
                }
                continue;
            }
            let prop_loc = self.location();
            let mut kind = PropertyKind::Init;
            let is_async_method = self.is_async_method_start();
            if is_async_method {
                self.advance();
            }
            let is_generator = self.eat(&TokenKind::Star);
            if !is_async_method && !is_generator {
                if let TokenKind::Identifier(name) = self.peek() {
                    if (name == "get" || name == "set") && !self.is_next_colon_or_lparen_or_comma()
                    {
                        let getter_setter = name.clone();
                        self.advance();
                        if self.at(&TokenKind::LParen)
                            || self.at(&TokenKind::Colon)
                            || self.at(&TokenKind::Comma)
                            || self.at(&TokenKind::RBrace)
                            || self.at(&TokenKind::Assign)
                        {
                            let name_str = getter_setter;
                            if self.eat(&TokenKind::Colon) {
                                let value = self.parse_assignment_expression()?;
                                properties.push(Property {
                                    key: Expression::Identifier(IdentifierExpression {
                                        name: name_str.clone(),
                                        location: prop_loc,
                                    }),
                                    value,
                                    kind: PropertyKind::Init,
                                    computed: false,
                                    shorthand: false,
                                    location: prop_loc,
                                });
                            } else if self.at(&TokenKind::LParen) {
                                let value = self.parse_method_expression(prop_loc, false, false)?;
                                properties.push(Property {
                                    key: Expression::Identifier(IdentifierExpression {
                                        name: name_str.clone(),
                                        location: prop_loc,
                                    }),
                                    value,
                                    kind: PropertyKind::Init,
                                    computed: false,
                                    shorthand: false,
                                    location: prop_loc,
                                });
                            } else {
                                let value = Expression::Identifier(IdentifierExpression {
                                    name: name_str.clone(),
                                    location: prop_loc,
                                });
                                properties.push(Property {
                                    key: Expression::Identifier(IdentifierExpression {
                                        name: name_str,
                                        location: prop_loc,
                                    }),
                                    value,
                                    kind: PropertyKind::Init,
                                    computed: false,
                                    shorthand: true,
                                    location: prop_loc,
                                });
                            }
                            if !self.eat(&TokenKind::Comma) {
                                break;
                            }
                            continue;
                        }
                        kind = if getter_setter == "get" {
                            PropertyKind::Get
                        } else {
                            PropertyKind::Set
                        };
                    }
                }
            }
            let mut computed = self.at(&TokenKind::LBracket);
            let key = if is_async_method && !is_generator && self.at(&TokenKind::LParen) {
                computed = false;
                Expression::Identifier(IdentifierExpression {
                    name: "async".to_string(),
                    location: prop_loc,
                })
            } else if computed {
                self.advance();
                let k = self.parse_assignment_expression()?;
                self.expect(&TokenKind::RBracket)?;
                k
            } else {
                self.parse_property_name()?
            };
            if kind != PropertyKind::Init {
                let value = self.parse_method_expression(prop_loc, false, false)?;
                properties.push(Property {
                    key,
                    value,
                    kind,
                    computed,
                    shorthand: false,
                    location: prop_loc,
                });
            } else if is_async_method || is_generator || self.at(&TokenKind::LParen) {
                let value =
                    self.parse_method_expression(prop_loc, is_async_method, is_generator)?;
                properties.push(Property {
                    key,
                    value,
                    kind: PropertyKind::Init,
                    computed,
                    shorthand: false,
                    location: prop_loc,
                });
            } else if self.eat(&TokenKind::Colon) {
                let value = self.parse_assignment_expression()?;
                properties.push(Property {
                    key,
                    value,
                    kind: PropertyKind::Init,
                    computed,
                    shorthand: false,
                    location: prop_loc,
                });
            } else {
                let name = match &key {
                    Expression::Identifier(id) => id.name.clone(),
                    _ => {
                        return Err(RawJsError::syntax_error(
                            "Invalid shorthand property",
                            Some(prop_loc),
                        ))
                    }
                };
                let value = Expression::Identifier(IdentifierExpression {
                    name: name.clone(),
                    location: prop_loc,
                });
                properties.push(Property {
                    key,
                    value,
                    kind: PropertyKind::Init,
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
        Ok(Expression::ObjectExpression(ObjectExpression {
            properties,
            location: loc,
        }))
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
