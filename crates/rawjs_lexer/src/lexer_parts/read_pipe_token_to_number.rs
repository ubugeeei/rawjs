impl Lexer {
    fn read_pipe_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('|') {
            if self.consume_if('=') {
                TokenKind::OrAssign
            } else {
                TokenKind::Or
            }
        } else if self.consume_if('=') {
            TokenKind::PipeAssign
        } else {
            TokenKind::Pipe
        }
    }
}

impl Lexer {
    fn read_caret_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('=') {
            TokenKind::CaretAssign
        } else {
            TokenKind::Caret
        }
    }
}

impl Lexer {
    fn read_bang_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('=') {
            if self.consume_if('=') {
                TokenKind::StrictNotEqual
            } else {
                TokenKind::NotEqual
            }
        } else {
            TokenKind::Not
        }
    }
}

impl Lexer {
    fn read_equal_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('=') {
            if self.consume_if('=') {
                TokenKind::StrictEqual
            } else {
                TokenKind::Equal
            }
        } else if self.consume_if('>') {
            TokenKind::Arrow
        } else {
            TokenKind::Assign
        }
    }
}

impl Lexer {
    fn read_less_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('<') {
            if self.consume_if('=') {
                TokenKind::ShiftLeftAssign
            } else {
                TokenKind::ShiftLeft
            }
        } else if self.consume_if('=') {
            TokenKind::LessEqual
        } else {
            TokenKind::LessThan
        }
    }
}

impl Lexer {
    fn read_greater_token(&mut self) -> TokenKind {
        self.advance();
        if self.consume_if('>') {
            if self.consume_if('>') {
                if self.consume_if('=') {
                    TokenKind::UnsignedShiftRightAssign
                } else {
                    TokenKind::UnsignedShiftRight
                }
            } else if self.consume_if('=') {
                TokenKind::ShiftRightAssign
            } else {
                TokenKind::ShiftRight
            }
        } else if self.consume_if('=') {
            TokenKind::GreaterEqual
        } else {
            TokenKind::GreaterThan
        }
    }
}

impl Lexer {
    fn read_number(&mut self) -> Result<TokenKind> {
        let mut s = String::new();
        if self.peek_char() == Some('0') {
            s.push('0');
            self.advance();
            match self.peek_char() {
                Some('x' | 'X') => {
                    s.push(self.advance().unwrap());
                    while let Some(ch) = self.peek_char() {
                        if ch.is_ascii_hexdigit() || ch == '_' {
                            if ch != '_' {
                                s.push(ch);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let val = i64::from_str_radix(&s[2..], 16).map_err(|_| {
                        RawJsError::syntax_error("Invalid hex literal", Some(self.location()))
                    })?;
                    return Ok(TokenKind::Number(val as f64));
                }
                Some('o' | 'O') => {
                    s.push(self.advance().unwrap());
                    while let Some(ch) = self.peek_char() {
                        if ('0'..='7').contains(&ch) || ch == '_' {
                            if ch != '_' {
                                s.push(ch);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let val = i64::from_str_radix(&s[2..], 8).map_err(|_| {
                        RawJsError::syntax_error("Invalid octal literal", Some(self.location()))
                    })?;
                    return Ok(TokenKind::Number(val as f64));
                }
                Some('b' | 'B') => {
                    s.push(self.advance().unwrap());
                    while let Some(ch) = self.peek_char() {
                        if ch == '0' || ch == '1' || ch == '_' {
                            if ch != '_' {
                                s.push(ch);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let val = i64::from_str_radix(&s[2..], 2).map_err(|_| {
                        RawJsError::syntax_error("Invalid binary literal", Some(self.location()))
                    })?;
                    return Ok(TokenKind::Number(val as f64));
                }
                _ => {}
            }
        }
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() || ch == '_' {
                if ch != '_' {
                    s.push(ch);
                }
                self.advance();
            } else {
                break;
            }
        }
        if self.peek_char() == Some('.') {
            s.push('.');
            self.advance();
            while let Some(ch) = self.peek_char() {
                if ch.is_ascii_digit() || ch == '_' {
                    if ch != '_' {
                        s.push(ch);
                    }
                    self.advance();
                } else {
                    break;
                }
            }
        }
        if matches!(self.peek_char(), Some('e' | 'E')) {
            s.push(self.advance().unwrap());
            if matches!(self.peek_char(), Some('+' | '-')) {
                s.push(self.advance().unwrap());
            }
            while let Some(ch) = self.peek_char() {
                if ch.is_ascii_digit() {
                    s.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }
        let val: f64 = s.parse().map_err(|_| {
            RawJsError::syntax_error(format!("Invalid number: {}", s), Some(self.location()))
        })?;
        Ok(TokenKind::Number(val))
    }
}
