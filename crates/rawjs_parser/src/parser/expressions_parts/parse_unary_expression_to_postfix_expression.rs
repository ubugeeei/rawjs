impl Parser {
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
}

impl Parser {
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
}
