impl Lexer {
    fn read_simple_punctuation(&mut self, ch: char) -> Option<TokenKind> {
        let kind = match ch {
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            '~' => TokenKind::Tilde,
            _ => return None,
        };
        self.advance();
        Some(kind)
    }
}

impl Lexer {
    fn consume_if(&mut self, expected: char) -> bool {
        if self.peek_char() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }
}

impl Lexer {
    fn read_lbrace_token(&mut self) -> TokenKind {
        self.advance();
        if let Some(depth) = self.template_brace_stack.last_mut() {
            *depth += 1;
        }
        TokenKind::LBrace
    }
}

impl Lexer {
    fn read_rbrace_or_template_token(&mut self) -> Result<TokenKind> {
        if let Some(depth) = self.template_brace_stack.last_mut() {
            if *depth == 0 {
                self.template_brace_stack.pop();
                self.advance();
                let kind = self.read_template_middle_or_tail()?;
                if matches!(kind, TokenKind::TemplateMiddle(_)) {
                    self.template_brace_stack.push(0);
                }
                Ok(kind)
            } else {
                *depth -= 1;
                self.advance();
                Ok(TokenKind::RBrace)
            }
        } else {
            self.advance();
            Ok(TokenKind::RBrace)
        }
    }
}

impl Lexer {
    fn read_dot_token(&mut self) -> Result<TokenKind> {
        if self.peek_char_at(1) == Some('.') && self.peek_char_at(2) == Some('.') {
            self.advance();
            self.advance();
            self.advance();
            Ok(TokenKind::Ellipsis)
        } else if matches!(self.peek_char_at(1), Some('0'..='9')) {
            self.read_number()
        } else {
            self.advance();
            Ok(TokenKind::Dot)
        }
    }
}

impl Lexer {
    fn read_question_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('?') {
            if self.consume_if('=') {
                TokenKind::NullishCoalescingAssign
            } else {
                TokenKind::NullishCoalescing
            }
        } else if self.consume_if('.') {
            TokenKind::OptionalChain
        } else {
            TokenKind::QuestionMark
        }
    }
}

impl Lexer {
    fn read_plus_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('+') {
            TokenKind::PlusPlus
        } else if self.consume_if('=') {
            TokenKind::PlusAssign
        } else {
            TokenKind::Plus
        }
    }
}

impl Lexer {
    fn read_minus_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('-') {
            TokenKind::MinusMinus
        } else if self.consume_if('=') {
            TokenKind::MinusAssign
        } else {
            TokenKind::Minus
        }
    }
}

impl Lexer {
    fn read_star_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('*') {
            if self.consume_if('=') {
                TokenKind::StarStarAssign
            } else {
                TokenKind::StarStar
            }
        } else if self.consume_if('=') {
            TokenKind::StarAssign
        } else {
            TokenKind::Star
        }
    }
}

impl Lexer {
    fn read_slash_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('=') {
            TokenKind::SlashAssign
        } else {
            TokenKind::Slash
        }
    }
}

impl Lexer {
    fn read_percent_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('=') {
            TokenKind::PercentAssign
        } else {
            TokenKind::Percent
        }
    }
}

impl Lexer {
    fn read_ampersand_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('&') {
            if self.consume_if('=') {
                TokenKind::AndAssign
            } else {
                TokenKind::And
            }
        } else if self.consume_if('=') {
            TokenKind::AmpersandAssign
        } else {
            TokenKind::Ampersand
        }
    }
}
