impl Parser {
    pub(crate) fn parse_formal_parameters(&mut self) -> Result<Vec<Pattern>> {
        let mut params = Vec::new();
        while !self.at(&TokenKind::RParen) && !self.at(&TokenKind::Eof) {
            if self.at(&TokenKind::Ellipsis) {
                let rest_loc = self.location();
                self.advance();
                let arg = self.parse_binding_pattern()?;
                params.push(Pattern::Rest(RestPattern {
                    argument: Box::new(arg),
                    location: rest_loc,
                }));
                break;
            }
            let mut pat = self.parse_binding_pattern()?;
            if self.eat(&TokenKind::Assign) {
                let right = self.parse_assignment_expression()?;
                let pat_loc = self.location();
                pat = Pattern::Assignment(AssignmentPattern {
                    left: Box::new(pat),
                    right: Box::new(right),
                    location: pat_loc,
                });
            }
            params.push(pat);
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(params)
    }
}

impl Parser {
    pub(crate) fn expression_to_pattern(&self, expr: Expression) -> Result<Pattern> {
        match expr {
            Expression::Identifier(id) => Ok(Pattern::Identifier(IdentifierPattern {
                name: id.name,
                location: id.location,
            })),
            Expression::ArrayExpression(arr) => {
                let elements = arr
                    .elements
                    .into_iter()
                    .map(|e| e.map(|e| self.expression_to_pattern(e)).transpose())
                    .collect::<Result<Vec<_>>>()?;
                Ok(Pattern::Array(ArrayPattern {
                    elements,
                    location: arr.location,
                }))
            }
            Expression::ObjectExpression(obj) => {
                let properties = obj
                    .properties
                    .into_iter()
                    .map(|p| {
                        let value = self.expression_to_pattern(p.value)?;
                        Ok(ObjectPatternProperty {
                            key: p.key,
                            value,
                            computed: p.computed,
                            shorthand: p.shorthand,
                            location: p.location,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(Pattern::Object(ObjectPattern {
                    properties,
                    location: obj.location,
                }))
            }
            Expression::Assignment(a) => {
                let left = self.expression_to_pattern(*a.left)?;
                Ok(Pattern::Assignment(AssignmentPattern {
                    left: Box::new(left),
                    right: a.right,
                    location: a.location,
                }))
            }
            Expression::Spread(s) => {
                let argument = self.expression_to_pattern(*s.argument)?;
                Ok(Pattern::Rest(RestPattern {
                    argument: Box::new(argument),
                    location: s.location,
                }))
            }
            _ => Err(RawJsError::syntax_error(
                "Invalid destructuring target",
                Some(SourceLocation::default()),
            )),
        }
    }
}

impl Parser {
    pub(crate) fn at_of(&self) -> bool {
        matches!(self.peek(), TokenKind::Of)
    }
}

impl Parser {
    pub(crate) fn is_next_lparen(&self) -> bool {
        self.pos + 1 < self.tokens.len() && self.tokens[self.pos + 1].kind == TokenKind::LParen
    }
}

impl Parser {
    pub(crate) fn is_next_colon_or_lparen_or_comma(&self) -> bool {
        if self.pos + 1 < self.tokens.len() {
            matches!(
                self.tokens[self.pos + 1].kind,
                TokenKind::Colon
                    | TokenKind::LParen
                    | TokenKind::Comma
                    | TokenKind::RBrace
                    | TokenKind::Assign
            )
        } else {
            false
        }
    }
}

impl Parser {
    pub(super) fn is_property_name_token(kind: &TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Identifier(_)
                | TokenKind::String(_)
                | TokenKind::Number(_)
                | TokenKind::Boolean(_)
                | TokenKind::Null
                | TokenKind::Var
                | TokenKind::Let
                | TokenKind::Const
                | TokenKind::Function
                | TokenKind::Return
                | TokenKind::If
                | TokenKind::Else
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Do
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Switch
                | TokenKind::Case
                | TokenKind::Default
                | TokenKind::Throw
                | TokenKind::Try
                | TokenKind::Catch
                | TokenKind::Finally
                | TokenKind::New
                | TokenKind::Delete
                | TokenKind::Typeof
                | TokenKind::Void
                | TokenKind::In
                | TokenKind::Instanceof
                | TokenKind::This
                | TokenKind::Class
                | TokenKind::Extends
                | TokenKind::Super
                | TokenKind::Import
                | TokenKind::Export
                | TokenKind::Of
                | TokenKind::With
                | TokenKind::Debugger
                | TokenKind::Yield
                | TokenKind::Async
                | TokenKind::Await
        )
    }
}

impl Parser {
    pub(super) fn computed_property_end(&self, start: usize) -> Option<usize> {
        let mut depth = 0;
        let mut index = start;
        while index < self.tokens.len() {
            match self.tokens[index].kind {
                TokenKind::LBracket => depth += 1,
                TokenKind::RBracket => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(index + 1);
                    }
                }
                _ => {}
            }
            index += 1;
        }
        None
    }
}

use super::*;
