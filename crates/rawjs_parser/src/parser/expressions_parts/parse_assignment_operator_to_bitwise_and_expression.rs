impl Parser {
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
}

impl Parser {
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
}

impl Parser {
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
}

impl Parser {
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
}

impl Parser {
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
}

impl Parser {
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
}

impl Parser {
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
}

impl Parser {
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
}
