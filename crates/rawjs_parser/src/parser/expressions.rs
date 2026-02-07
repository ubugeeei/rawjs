use rawjs_ast::*;
use rawjs_common::{RawJsError, Result};
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

    fn is_arrow_function(&self) -> bool {
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

    fn is_async_arrow_function(&self) -> bool {
        let is_async_kw = matches!(self.peek(), TokenKind::Async)
            || matches!(self.peek(), TokenKind::Identifier(ref name) if name == "async");
        if !is_async_kw || self.pos + 1 >= self.tokens.len() {
            return false;
        }
        // async ident =>
        if let TokenKind::Identifier(_) = &self.tokens[self.pos + 1].kind {
            if self.pos + 2 < self.tokens.len()
                && self.tokens[self.pos + 2].kind == TokenKind::Arrow
                && !self.tokens[self.pos + 1].had_line_break_before
            {
                return true;
            }
        }
        // async (...) =>
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

    fn parse_arrow_function(&mut self, is_async: bool) -> Result<Expression> {
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

    fn parse_async_arrow_function(&mut self) -> Result<Expression> {
        self.advance(); // async
        self.parse_arrow_function(true)
    }

    fn parse_assignment_operator(&mut self) -> Result<AssignmentOp> {
        let op = match self.advance().kind {
            TokenKind::Assign => AssignmentOp::Assign,
            TokenKind::PlusAssign => AssignmentOp::AddAssign,
            TokenKind::MinusAssign => AssignmentOp::SubAssign,
            TokenKind::StarAssign => AssignmentOp::MulAssign,
            TokenKind::SlashAssign => AssignmentOp::DivAssign,
            TokenKind::PercentAssign => AssignmentOp::ModAssign,
            TokenKind::StarStarAssign => AssignmentOp::ExpAssign,
            TokenKind::AmpersandAssign => AssignmentOp::BitAndAssign,
            TokenKind::PipeAssign => AssignmentOp::BitOrAssign,
            TokenKind::CaretAssign => AssignmentOp::BitXorAssign,
            TokenKind::ShiftLeftAssign => AssignmentOp::ShlAssign,
            TokenKind::ShiftRightAssign => AssignmentOp::ShrAssign,
            TokenKind::UnsignedShiftRightAssign => AssignmentOp::UShrAssign,
            TokenKind::AndAssign => AssignmentOp::AndAssign,
            TokenKind::OrAssign => AssignmentOp::OrAssign,
            TokenKind::NullishCoalescingAssign => AssignmentOp::NullishCoalescingAssign,
            _ => unreachable!(),
        };
        Ok(op)
    }

    fn parse_conditional_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let expr = self.parse_nullish_coalescing_expression()?;
        if self.eat(&TokenKind::QuestionMark) {
            let consequent = self.parse_assignment_expression()?;
            self.expect(&TokenKind::Colon)?;
            let alternate = self.parse_assignment_expression()?;
            Ok(Expression::Conditional(ConditionalExpression {
                test: Box::new(expr),
                consequent: Box::new(consequent),
                alternate: Box::new(alternate),
                location: loc,
            }))
        } else {
            Ok(expr)
        }
    }

    fn parse_nullish_coalescing_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_logical_or_expression()?;
        while self.eat(&TokenKind::NullishCoalescing) {
            let right = self.parse_logical_or_expression()?;
            left = Expression::Logical(LogicalExpression {
                operator: LogicalOp::NullishCoalescing,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            });
        }
        Ok(left)
    }

    fn parse_logical_or_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_logical_and_expression()?;
        while self.eat(&TokenKind::Or) {
            let right = self.parse_logical_and_expression()?;
            left = Expression::Logical(LogicalExpression {
                operator: LogicalOp::Or,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            });
        }
        Ok(left)
    }

    fn parse_logical_and_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_bitwise_or_expression()?;
        while self.eat(&TokenKind::And) {
            let right = self.parse_bitwise_or_expression()?;
            left = Expression::Logical(LogicalExpression {
                operator: LogicalOp::And,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            });
        }
        Ok(left)
    }

    fn parse_bitwise_or_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_bitwise_xor_expression()?;
        while self.eat(&TokenKind::Pipe) {
            let right = self.parse_bitwise_xor_expression()?;
            left = Expression::Binary(BinaryExpression {
                operator: BinaryOp::BitOr,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            });
        }
        Ok(left)
    }

    fn parse_bitwise_xor_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_bitwise_and_expression()?;
        while self.eat(&TokenKind::Caret) {
            let right = self.parse_bitwise_and_expression()?;
            left = Expression::Binary(BinaryExpression {
                operator: BinaryOp::BitXor,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            });
        }
        Ok(left)
    }

    fn parse_bitwise_and_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_equality_expression()?;
        while self.eat(&TokenKind::Ampersand) {
            let right = self.parse_equality_expression()?;
            left = Expression::Binary(BinaryExpression {
                operator: BinaryOp::BitAnd,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            });
        }
        Ok(left)
    }

    fn parse_equality_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_relational_expression()?;
        loop {
            let op = match self.peek() {
                TokenKind::Equal => BinaryOp::Eq,
                TokenKind::StrictEqual => BinaryOp::StrictEq,
                TokenKind::NotEqual => BinaryOp::Ne,
                TokenKind::StrictNotEqual => BinaryOp::StrictNe,
                _ => break,
            };
            self.advance();
            let right = self.parse_relational_expression()?;
            left = Expression::Binary(BinaryExpression {
                operator: op,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            });
        }
        Ok(left)
    }

    fn parse_relational_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_shift_expression()?;
        loop {
            let op = match self.peek() {
                TokenKind::LessThan => BinaryOp::Lt,
                TokenKind::LessEqual => BinaryOp::Le,
                TokenKind::GreaterThan => BinaryOp::Gt,
                TokenKind::GreaterEqual => BinaryOp::Ge,
                TokenKind::Instanceof => BinaryOp::Instanceof,
                TokenKind::In => BinaryOp::In,
                _ => break,
            };
            self.advance();
            let right = self.parse_shift_expression()?;
            left = Expression::Binary(BinaryExpression {
                operator: op,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            });
        }
        Ok(left)
    }

    fn parse_shift_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_additive_expression()?;
        loop {
            let op = match self.peek() {
                TokenKind::ShiftLeft => BinaryOp::Shl,
                TokenKind::ShiftRight => BinaryOp::Shr,
                TokenKind::UnsignedShiftRight => BinaryOp::UShr,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive_expression()?;
            left = Expression::Binary(BinaryExpression {
                operator: op,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            });
        }
        Ok(left)
    }

    fn parse_additive_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_multiplicative_expression()?;
        loop {
            let op = match self.peek() {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative_expression()?;
            left = Expression::Binary(BinaryExpression {
                operator: op,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            });
        }
        Ok(left)
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_exponentiation_expression()?;
        loop {
            let op = match self.peek() {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                TokenKind::Percent => BinaryOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_exponentiation_expression()?;
            left = Expression::Binary(BinaryExpression {
                operator: op,
                left: Box::new(left),
                right: Box::new(right),
                location: loc,
            });
        }
        Ok(left)
    }

    fn parse_exponentiation_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let base = self.parse_unary_expression()?;
        if self.eat(&TokenKind::StarStar) {
            let exponent = self.parse_exponentiation_expression()?;
            Ok(Expression::Binary(BinaryExpression {
                operator: BinaryOp::Exp,
                left: Box::new(base),
                right: Box::new(exponent),
                location: loc,
            }))
        } else {
            Ok(base)
        }
    }

    fn parse_unary_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        match self.peek() {
            TokenKind::Not => {
                self.advance();
                let arg = self.parse_unary_expression()?;
                Ok(Expression::Unary(UnaryExpression {
                    operator: UnaryOp::Not,
                    argument: Box::new(arg),
                    prefix: true,
                    location: loc,
                }))
            }
            TokenKind::Tilde => {
                self.advance();
                let arg = self.parse_unary_expression()?;
                Ok(Expression::Unary(UnaryExpression {
                    operator: UnaryOp::BitNot,
                    argument: Box::new(arg),
                    prefix: true,
                    location: loc,
                }))
            }
            TokenKind::Minus => {
                self.advance();
                let arg = self.parse_unary_expression()?;
                Ok(Expression::Unary(UnaryExpression {
                    operator: UnaryOp::Minus,
                    argument: Box::new(arg),
                    prefix: true,
                    location: loc,
                }))
            }
            TokenKind::Plus => {
                self.advance();
                let arg = self.parse_unary_expression()?;
                Ok(Expression::Unary(UnaryExpression {
                    operator: UnaryOp::Plus,
                    argument: Box::new(arg),
                    prefix: true,
                    location: loc,
                }))
            }
            TokenKind::Typeof => {
                self.advance();
                let arg = self.parse_unary_expression()?;
                Ok(Expression::Unary(UnaryExpression {
                    operator: UnaryOp::Typeof,
                    argument: Box::new(arg),
                    prefix: true,
                    location: loc,
                }))
            }
            TokenKind::Void => {
                self.advance();
                let arg = self.parse_unary_expression()?;
                Ok(Expression::Unary(UnaryExpression {
                    operator: UnaryOp::Void,
                    argument: Box::new(arg),
                    prefix: true,
                    location: loc,
                }))
            }
            TokenKind::Delete => {
                self.advance();
                let arg = self.parse_unary_expression()?;
                Ok(Expression::Unary(UnaryExpression {
                    operator: UnaryOp::Delete,
                    argument: Box::new(arg),
                    prefix: true,
                    location: loc,
                }))
            }
            TokenKind::PlusPlus => {
                self.advance();
                let arg = self.parse_unary_expression()?;
                Ok(Expression::Update(UpdateExpression {
                    operator: UpdateOp::Increment,
                    argument: Box::new(arg),
                    prefix: true,
                    location: loc,
                }))
            }
            TokenKind::MinusMinus => {
                self.advance();
                let arg = self.parse_unary_expression()?;
                Ok(Expression::Update(UpdateExpression {
                    operator: UpdateOp::Decrement,
                    argument: Box::new(arg),
                    prefix: true,
                    location: loc,
                }))
            }
            TokenKind::Await => {
                self.advance();
                let arg = self.parse_unary_expression()?;
                Ok(Expression::Await(AwaitExpression {
                    argument: Box::new(arg),
                    location: loc,
                }))
            }
            _ => self.parse_postfix_expression(),
        }
    }

    fn parse_postfix_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut expr = self.parse_left_hand_side_expression()?;

        if !self.current_token().had_line_break_before {
            if self.at(&TokenKind::PlusPlus) {
                self.advance();
                expr = Expression::Update(UpdateExpression {
                    operator: UpdateOp::Increment,
                    argument: Box::new(expr),
                    prefix: false,
                    location: loc,
                });
            } else if self.at(&TokenKind::MinusMinus) {
                self.advance();
                expr = Expression::Update(UpdateExpression {
                    operator: UpdateOp::Decrement,
                    argument: Box::new(expr),
                    prefix: false,
                    location: loc,
                });
            }
        }
        Ok(expr)
    }

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

    fn parse_new_expression(&mut self) -> Result<Expression> {
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
                // `async function` expression or `async` as identifier
                if self.pos + 1 < self.tokens.len()
                    && self.tokens[self.pos + 1].kind == TokenKind::Function
                    && !self.tokens[self.pos + 1].had_line_break_before
                {
                    self.advance(); // async
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
                    // Treat `async` as a plain identifier.
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

    fn parse_object_expression(&mut self) -> Result<Expression> {
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

            if let TokenKind::Identifier(name) = self.peek() {
                if (name == "get" || name == "set") && !self.is_next_colon_or_lparen_or_comma() {
                    let getter_setter = name.clone();
                    self.advance();
                    if self.at(&TokenKind::LParen)
                        || self.at(&TokenKind::Colon)
                        || self.at(&TokenKind::Comma)
                        || self.at(&TokenKind::RBrace)
                        || self.at(&TokenKind::Assign)
                    {
                        // Shorthand property named "get" or "set"
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
                            // Method shorthand: get() { ... }
                            self.advance();
                            let params = self.parse_formal_parameters()?;
                            self.expect(&TokenKind::RParen)?;
                            let body = self.parse_block_statement()?;
                            let value = Expression::FunctionExpression(FunctionDeclaration {
                                id: None,
                                params,
                                body: Box::new(body),
                                is_async: false,
                                is_generator: false,
                                location: prop_loc,
                            });
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

            let computed = self.at(&TokenKind::LBracket);
            let key = if computed {
                self.advance();
                let k = self.parse_assignment_expression()?;
                self.expect(&TokenKind::RBracket)?;
                k
            } else {
                self.parse_property_name()?
            };

            if kind != PropertyKind::Init {
                // getter/setter
                self.expect(&TokenKind::LParen)?;
                let params = self.parse_formal_parameters()?;
                self.expect(&TokenKind::RParen)?;
                let body = self.parse_block_statement()?;
                let value = Expression::FunctionExpression(FunctionDeclaration {
                    id: None,
                    params,
                    body: Box::new(body),
                    is_async: false,
                    is_generator: false,
                    location: prop_loc,
                });
                properties.push(Property {
                    key,
                    value,
                    kind,
                    computed,
                    shorthand: false,
                    location: prop_loc,
                });
            } else if self.at(&TokenKind::LParen) {
                // method shorthand
                self.advance();
                let params = self.parse_formal_parameters()?;
                self.expect(&TokenKind::RParen)?;
                let body = self.parse_block_statement()?;
                let value = Expression::FunctionExpression(FunctionDeclaration {
                    id: None,
                    params,
                    body: Box::new(body),
                    is_async: false,
                    is_generator: false,
                    location: prop_loc,
                });
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
                // shorthand property
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
