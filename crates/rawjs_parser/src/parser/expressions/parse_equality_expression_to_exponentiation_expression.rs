impl Parser {
    pub(super) fn parse_equality_expression(&mut self) -> Result<Expression> {
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
}

impl Parser {
    pub(super) fn parse_relational_expression(&mut self) -> Result<Expression> {
        let loc = self.location();
        let mut left = self.parse_shift_expression()?;
        loop {
            let op = match self.peek() {
                TokenKind::LessThan => BinaryOp::Lt,
                TokenKind::LessEqual => BinaryOp::Le,
                TokenKind::GreaterThan => BinaryOp::Gt,
                TokenKind::GreaterEqual => BinaryOp::Ge,
                TokenKind::Instanceof => BinaryOp::Instanceof,
                TokenKind::In if self.allow_in => BinaryOp::In,
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
}

impl Parser {
    pub(super) fn parse_shift_expression(&mut self) -> Result<Expression> {
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
}

impl Parser {
    pub(super) fn parse_additive_expression(&mut self) -> Result<Expression> {
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
}

impl Parser {
    pub(super) fn parse_multiplicative_expression(&mut self) -> Result<Expression> {
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
}

impl Parser {
    pub(super) fn parse_exponentiation_expression(&mut self) -> Result<Expression> {
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
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
