impl Parser {
    pub(crate) fn is_async_method_start(&self) -> bool {
        let async_like = matches!(self.peek(), TokenKind::Async)
            || matches ! (self . peek () , TokenKind :: Identifier (ref name) if name == "async");
        if !async_like || self.pos + 1 >= self.tokens.len() {
            return false;
        }
        let next = &self.tokens[self.pos + 1];
        if next.had_line_break_before {
            return false;
        }
        match &next.kind {
            TokenKind::Star | TokenKind::LParen => true,
            TokenKind::LBracket => self
                .computed_property_end(self.pos + 1)
                .and_then(|end| self.tokens.get(end))
                .is_some_and(|token| token.kind == TokenKind::LParen),
            kind if Self::is_property_name_token(kind) => self
                .tokens
                .get(self.pos + 2)
                .is_some_and(|token| token.kind == TokenKind::LParen),
            _ => false,
        }
    }
}

impl Parser {
    pub(crate) fn parse_method_expression(
        &mut self,
        location: SourceLocation,
        is_async: bool,
        is_generator: bool,
    ) -> Result<Expression> {
        self.expect(&TokenKind::LParen)?;
        let params = self.parse_formal_parameters()?;
        self.expect(&TokenKind::RParen)?;
        let body = self.parse_block_statement()?;
        Ok(Expression::FunctionExpression(FunctionDeclaration {
            id: None,
            params,
            body: Box::new(body),
            is_async,
            is_generator,
            location,
        }))
    }
}

impl Parser {
    pub(crate) fn parse_property_name(&mut self) -> Result<Expression> {
        let loc = self.location();
        match self.peek().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(Expression::Identifier(IdentifierExpression {
                    name,
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
            TokenKind::Number(n) => {
                self.advance();
                Ok(Expression::NumberLiteral(NumberLiteral {
                    value: n,
                    location: loc,
                }))
            }
            _ => {
                let name = self.expect_identifier_or_keyword()?;
                Ok(Expression::Identifier(IdentifierExpression {
                    name,
                    location: loc,
                }))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse;
    use rawjs_ast::{AssignmentOp, Expression, Statement};
    #[test]
    pub(super) fn test_variable_declaration() {
        let program = parse("let x = 42;").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_function_declaration() {
        let program = parse("function foo(a, b) { return a + b; }").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_if_statement() {
        let program = parse("if (x > 0) { y = 1; } else { y = 2; }").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_for_loop() {
        let program = parse("for (let i = 0; i < 10; i++) { x = i; }").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_arrow_function() {
        let program = parse("const f = (a, b) => a + b;").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_object_literal() {
        let program = parse("let o = { a: 1, b: 2, c: 3 };").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_array_literal() {
        let program = parse("let a = [1, 2, 3];").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_try_catch() {
        let program = parse("try { x(); } catch (e) { y(); } finally { z(); }").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_class() {
        let program = parse("class Foo extends Bar { constructor() {} method() {} }").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_async_generator_object_method() {
        let program = parse("({ async *method() {} });").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_async_generator_class_method() {
        let program = parse("class Foo { async *method() {} }").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_member_expression() {
        let program = parse("console.log(42);").unwrap();
        assert_eq!(program.body.len(), 1);
    }
    #[test]
    pub(super) fn test_optional_chain() {
        let program = parse("obj?.prop?.();").unwrap();
        let Statement::Expression(stmt) = &program.body[0] else {
            panic!("expected expression statement");
        };
        let Expression::Call(call) = &stmt.expression else {
            panic!("expected call expression");
        };
        assert!(call.optional);
        let Expression::Member(member) = call.callee.as_ref() else {
            panic!("expected member expression");
        };
        assert!(member.optional);
    }
    #[test]
    pub(super) fn test_logical_assignment() {
        let program = parse("value ||= other;").unwrap();
        let Statement::Expression(stmt) = &program.body[0] else {
            panic!("expected expression statement");
        };
        let Expression::Assignment(assign) = &stmt.expression else {
            panic!("expected assignment expression");
        };
        assert_eq!(assign.operator, AssignmentOp::OrAssign);
    }
    #[test]
    pub(super) fn test_dynamic_import_expression() {
        let program = parse("import('./dep.js');").unwrap();
        let Statement::Expression(stmt) = &program.body[0] else {
            panic!("expected expression statement");
        };
        let Expression::Import(import_expr) = &stmt.expression else {
            panic!("expected import expression");
        };
        let Expression::StringLiteral(source) = import_expr.source.as_ref() else {
            panic!("expected string literal source");
        };
        assert_eq!(source.value, "./dep.js");
    }
    #[test]
    pub(super) fn test_import_meta_expression() {
        let program = parse("import.meta.url;").unwrap();
        let Statement::Expression(stmt) = &program.body[0] else {
            panic!("expected expression statement");
        };
        let Expression::Member(member) = &stmt.expression else {
            panic!("expected member expression");
        };
        assert!(matches!(member.object.as_ref(), Expression::ImportMeta(_)));
    }
    #[test]
    pub(super) fn test_asi() {
        let program = parse("let x = 1\nlet y = 2\n").unwrap();
        assert_eq!(program.body.len(), 2);
    }
    #[test]
    pub(super) fn test_switch() {
        let program = parse("switch(x) { case 1: break; default: break; }").unwrap();
        assert_eq!(program.body.len(), 1);
    }
}

use super::*;
