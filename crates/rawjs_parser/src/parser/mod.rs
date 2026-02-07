mod expressions;
mod statements;

use rawjs_ast::*;
use rawjs_common::{RawJsError, Result, SourceLocation};
use rawjs_lexer::{Lexer, Token, TokenKind};

pub struct Parser {
    pub(crate) tokens: Vec<Token>,
    pub(crate) pos: usize,
}

impl Parser {
    pub fn new(source: &str) -> Result<Self> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize()?;
        Ok(Self { tokens, pos: 0 })
    }

    pub(crate) fn peek(&self) -> &TokenKind {
        &self.tokens[self.pos].kind
    }

    pub(crate) fn current_token(&self) -> &Token {
        &self.tokens[self.pos]
    }

    pub(crate) fn location(&self) -> SourceLocation {
        self.tokens[self.pos].location
    }

    pub(crate) fn advance(&mut self) -> Token {
        let token = self.tokens[self.pos].clone();
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
        token
    }

    pub(crate) fn at(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(kind)
    }

    pub(crate) fn eat(&mut self, kind: &TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub(crate) fn expect(&mut self, kind: &TokenKind) -> Result<Token> {
        if self.at(kind) {
            Ok(self.advance())
        } else {
            Err(RawJsError::syntax_error(
                format!("Expected {:?}, got {:?}", kind, self.peek()),
                Some(self.location()),
            ))
        }
    }

    pub(crate) fn expect_semicolon(&mut self) -> Result<()> {
        if self.eat(&TokenKind::Semicolon) {
            return Ok(());
        }
        if self.at(&TokenKind::RBrace) || self.at(&TokenKind::Eof) {
            return Ok(());
        }
        if self.current_token().had_line_break_before {
            return Ok(());
        }
        Err(RawJsError::syntax_error(
            format!("Expected semicolon, got {:?}", self.peek()),
            Some(self.location()),
        ))
    }

    pub(crate) fn expect_identifier(&mut self) -> Result<String> {
        match self.peek().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(name)
            }
            _ => Err(RawJsError::syntax_error(
                format!("Expected identifier, got {:?}", self.peek()),
                Some(self.location()),
            )),
        }
    }

    pub(crate) fn expect_identifier_or_keyword(&mut self) -> Result<String> {
        match self.peek().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(name)
            }
            // Allow keywords as property names
            TokenKind::In => {
                self.advance();
                Ok("in".to_string())
            }
            TokenKind::Instanceof => {
                self.advance();
                Ok("instanceof".to_string())
            }
            TokenKind::Typeof => {
                self.advance();
                Ok("typeof".to_string())
            }
            TokenKind::Void => {
                self.advance();
                Ok("void".to_string())
            }
            TokenKind::Delete => {
                self.advance();
                Ok("delete".to_string())
            }
            TokenKind::Return => {
                self.advance();
                Ok("return".to_string())
            }
            TokenKind::If => {
                self.advance();
                Ok("if".to_string())
            }
            TokenKind::Else => {
                self.advance();
                Ok("else".to_string())
            }
            TokenKind::While => {
                self.advance();
                Ok("while".to_string())
            }
            TokenKind::For => {
                self.advance();
                Ok("for".to_string())
            }
            TokenKind::Do => {
                self.advance();
                Ok("do".to_string())
            }
            TokenKind::Switch => {
                self.advance();
                Ok("switch".to_string())
            }
            TokenKind::Case => {
                self.advance();
                Ok("case".to_string())
            }
            TokenKind::Default => {
                self.advance();
                Ok("default".to_string())
            }
            TokenKind::Break => {
                self.advance();
                Ok("break".to_string())
            }
            TokenKind::Continue => {
                self.advance();
                Ok("continue".to_string())
            }
            TokenKind::Try => {
                self.advance();
                Ok("try".to_string())
            }
            TokenKind::Catch => {
                self.advance();
                Ok("catch".to_string())
            }
            TokenKind::Finally => {
                self.advance();
                Ok("finally".to_string())
            }
            TokenKind::Throw => {
                self.advance();
                Ok("throw".to_string())
            }
            TokenKind::New => {
                self.advance();
                Ok("new".to_string())
            }
            TokenKind::This => {
                self.advance();
                Ok("this".to_string())
            }
            TokenKind::Class => {
                self.advance();
                Ok("class".to_string())
            }
            TokenKind::Extends => {
                self.advance();
                Ok("extends".to_string())
            }
            TokenKind::Super => {
                self.advance();
                Ok("super".to_string())
            }
            TokenKind::Import => {
                self.advance();
                Ok("import".to_string())
            }
            TokenKind::Export => {
                self.advance();
                Ok("export".to_string())
            }
            TokenKind::Var => {
                self.advance();
                Ok("var".to_string())
            }
            TokenKind::Let => {
                self.advance();
                Ok("let".to_string())
            }
            TokenKind::Const => {
                self.advance();
                Ok("const".to_string())
            }
            TokenKind::Function => {
                self.advance();
                Ok("function".to_string())
            }
            TokenKind::With => {
                self.advance();
                Ok("with".to_string())
            }
            TokenKind::Debugger => {
                self.advance();
                Ok("debugger".to_string())
            }
            TokenKind::Yield => {
                self.advance();
                Ok("yield".to_string())
            }
            TokenKind::Async => {
                self.advance();
                Ok("async".to_string())
            }
            TokenKind::Await => {
                self.advance();
                Ok("await".to_string())
            }
            TokenKind::Of => {
                self.advance();
                Ok("of".to_string())
            }
            TokenKind::Boolean(true) => {
                self.advance();
                Ok("true".to_string())
            }
            TokenKind::Boolean(false) => {
                self.advance();
                Ok("false".to_string())
            }
            TokenKind::Null => {
                self.advance();
                Ok("null".to_string())
            }
            _ => Err(RawJsError::syntax_error(
                format!("Expected identifier, got {:?}", self.peek()),
                Some(self.location()),
            )),
        }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let loc = self.location();
        let mut body = Vec::new();
        while !self.at(&TokenKind::Eof) {
            body.push(self.parse_statement()?);
        }
        Ok(Program {
            body,
            location: loc,
        })
    }

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

    pub(crate) fn at_of(&self) -> bool {
        matches!(self.peek(), TokenKind::Of)
    }

    pub(crate) fn is_next_lparen(&self) -> bool {
        self.pos + 1 < self.tokens.len() && self.tokens[self.pos + 1].kind == TokenKind::LParen
    }

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
                // try keyword as property name
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

    #[test]
    fn test_variable_declaration() {
        let program = parse("let x = 42;").unwrap();
        assert_eq!(program.body.len(), 1);
    }

    #[test]
    fn test_function_declaration() {
        let program = parse("function foo(a, b) { return a + b; }").unwrap();
        assert_eq!(program.body.len(), 1);
    }

    #[test]
    fn test_if_statement() {
        let program = parse("if (x > 0) { y = 1; } else { y = 2; }").unwrap();
        assert_eq!(program.body.len(), 1);
    }

    #[test]
    fn test_for_loop() {
        let program = parse("for (let i = 0; i < 10; i++) { x = i; }").unwrap();
        assert_eq!(program.body.len(), 1);
    }

    #[test]
    fn test_arrow_function() {
        let program = parse("const f = (a, b) => a + b;").unwrap();
        assert_eq!(program.body.len(), 1);
    }

    #[test]
    fn test_object_literal() {
        let program = parse("let o = { a: 1, b: 2, c: 3 };").unwrap();
        assert_eq!(program.body.len(), 1);
    }

    #[test]
    fn test_array_literal() {
        let program = parse("let a = [1, 2, 3];").unwrap();
        assert_eq!(program.body.len(), 1);
    }

    #[test]
    fn test_try_catch() {
        let program = parse("try { x(); } catch (e) { y(); } finally { z(); }").unwrap();
        assert_eq!(program.body.len(), 1);
    }

    #[test]
    fn test_class() {
        let program = parse("class Foo extends Bar { constructor() {} method() {} }").unwrap();
        assert_eq!(program.body.len(), 1);
    }

    #[test]
    fn test_member_expression() {
        let program = parse("console.log(42);").unwrap();
        assert_eq!(program.body.len(), 1);
    }

    #[test]
    fn test_asi() {
        let program = parse("let x = 1\nlet y = 2\n").unwrap();
        assert_eq!(program.body.len(), 2);
    }

    #[test]
    fn test_switch() {
        let program = parse("switch(x) { case 1: break; default: break; }").unwrap();
        assert_eq!(program.body.len(), 1);
    }
}
