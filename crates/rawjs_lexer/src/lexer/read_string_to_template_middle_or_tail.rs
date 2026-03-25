impl Lexer {
    pub(super) fn read_string(&mut self) -> Result<TokenKind> {
        let quote = self.advance().unwrap();
        let mut s = String::new();
        loop {
            match self.advance() {
                Some(ch) if ch == quote => break,
                Some('\\') => {
                    let esc = self.read_escape_sequence()?;
                    s.push(esc);
                }
                Some(ch) => s.push(ch),
                None => {
                    return Err(RawJsError::syntax_error(
                        "Unterminated string literal",
                        Some(self.location()),
                    ));
                }
            }
        }
        Ok(TokenKind::String(s))
    }
}

impl Lexer {
    pub(super) fn read_escape_sequence(&mut self) -> Result<char> {
        match self.advance() {
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('\\') => Ok('\\'),
            Some('\'') => Ok('\''),
            Some('"') => Ok('"'),
            Some('`') => Ok('`'),
            Some('0') => Ok('\0'),
            Some('b') => Ok('\u{0008}'),
            Some('f') => Ok('\u{000C}'),
            Some('v') => Ok('\u{000B}'),
            Some('u') => self.read_unicode_escape(),
            Some('x') => self.read_hex_escape(),
            Some('\n') => Ok('\n'),
            Some(ch) => Ok(ch),
            None => Err(RawJsError::syntax_error(
                "Unexpected end of input in escape sequence",
                Some(self.location()),
            )),
        }
    }
}

impl Lexer {
    pub(super) fn read_unicode_escape(&mut self) -> Result<char> {
        if self.peek_char() == Some('{') {
            self.advance();
            let mut hex = String::new();
            while let Some(ch) = self.peek_char() {
                if ch == '}' {
                    self.advance();
                    break;
                }
                hex.push(ch);
                self.advance();
            }
            let cp = u32::from_str_radix(&hex, 16).map_err(|_| {
                RawJsError::syntax_error("Invalid unicode escape", Some(self.location()))
            })?;
            char::from_u32(cp).ok_or_else(|| {
                RawJsError::syntax_error("Invalid unicode code point", Some(self.location()))
            })
        } else {
            let mut hex = String::new();
            for _ in 0..4 {
                match self.advance() {
                    Some(ch) if ch.is_ascii_hexdigit() => hex.push(ch),
                    _ => {
                        return Err(RawJsError::syntax_error(
                            "Invalid unicode escape sequence",
                            Some(self.location()),
                        ));
                    }
                }
            }
            let cp = u32::from_str_radix(&hex, 16).unwrap();
            char::from_u32(cp).ok_or_else(|| {
                RawJsError::syntax_error("Invalid unicode code point", Some(self.location()))
            })
        }
    }
}

impl Lexer {
    pub(super) fn read_hex_escape(&mut self) -> Result<char> {
        let mut hex = String::new();
        for _ in 0..2 {
            match self.advance() {
                Some(ch) if ch.is_ascii_hexdigit() => hex.push(ch),
                _ => {
                    return Err(RawJsError::syntax_error(
                        "Invalid hex escape sequence",
                        Some(self.location()),
                    ));
                }
            }
        }
        let val = u8::from_str_radix(&hex, 16).unwrap();
        Ok(val as char)
    }
}

impl Lexer {
    pub(super) fn read_template_literal(&mut self) -> Result<TokenKind> {
        self.advance();
        let mut s = String::new();
        loop {
            match self.peek_char() {
                Some('`') => {
                    self.advance();
                    return Ok(TokenKind::TemplateLiteral(s));
                }
                Some('$') if self.peek_char_at(1) == Some('{') => {
                    self.advance();
                    self.advance();
                    self.template_brace_stack.push(0);
                    return Ok(TokenKind::TemplateHead(s));
                }
                Some('\\') => {
                    self.advance();
                    let esc = self.read_escape_sequence()?;
                    s.push(esc);
                }
                Some(_) => {
                    let ch = self.advance().unwrap();
                    s.push(ch);
                }
                None => {
                    return Err(RawJsError::syntax_error(
                        "Unterminated template literal",
                        Some(self.location()),
                    ));
                }
            }
        }
    }
}

impl Lexer {
    pub(super) fn read_template_middle_or_tail(&mut self) -> Result<TokenKind> {
        let mut s = String::new();
        loop {
            match self.peek_char() {
                Some('`') => {
                    self.advance();
                    return Ok(TokenKind::TemplateTail(s));
                }
                Some('$') if self.peek_char_at(1) == Some('{') => {
                    self.advance();
                    self.advance();
                    return Ok(TokenKind::TemplateMiddle(s));
                }
                Some('\\') => {
                    self.advance();
                    let esc = self.read_escape_sequence()?;
                    s.push(esc);
                }
                Some(_) => {
                    let ch = self.advance().unwrap();
                    s.push(ch);
                }
                None => {
                    return Err(RawJsError::syntax_error(
                        "Unterminated template literal",
                        Some(self.location()),
                    ));
                }
            }
        }
    }
}

use super::*;
