use rawjs_common::{RawJsError, Result, SourceLocation};

use crate::token::{Token, TokenKind};

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    line: u32,
    column: u32,
    had_line_break: bool,
    /// Stack tracking brace depth within template expression contexts.
    /// Each entry represents a nested template literal; the value is the
    /// brace depth inside the current template expression. When a `{` is
    /// encountered while inside a template expression, the depth is
    /// incremented; when `}` is encountered and the depth is 0, we resume
    /// scanning the template literal instead of emitting RBrace.
    pub(super) template_brace_stack: Vec<u32>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            pos: 0,
            line: 1,
            column: 0,
            had_line_break: false,
            template_brace_stack: Vec::new(),
        }
    }
}

impl Lexer {
    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }
}

impl Lexer {
    pub(super) fn peek_char(&self) -> Option<char> {
        self.source.get(self.pos).copied()
    }
}

impl Lexer {
    pub(super) fn peek_char_at(&self, offset: usize) -> Option<char> {
        self.source.get(self.pos + offset).copied()
    }
}

impl Lexer {
    pub(super) fn advance(&mut self) -> Option<char> {
        let ch = self.source.get(self.pos).copied()?;
        self.pos += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 0;
            self.had_line_break = true;
        } else {
            self.column += 1;
        }
        Some(ch)
    }
}

impl Lexer {
    pub(super) fn location(&self) -> SourceLocation {
        SourceLocation::new(self.line, self.column, self.pos as u32)
    }
}

impl Lexer {
    pub(super) fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek_char() {
                Some(' ' | '\t' | '\r') => {
                    self.advance();
                }
                Some('\n') => {
                    self.advance();
                }
                Some('/') => {
                    if self.peek_char_at(1) == Some('/') {
                        self.advance();
                        self.advance();
                        while let Some(ch) = self.peek_char() {
                            if ch == '\n' {
                                break;
                            }
                            self.advance();
                        }
                    } else if self.peek_char_at(1) == Some('*') {
                        self.advance();
                        self.advance();
                        loop {
                            match self.advance() {
                                Some('*') if self.peek_char() == Some('/') => {
                                    self.advance();
                                    break;
                                }
                                None => break,
                                _ => {}
                            }
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }
}

impl Lexer {
    pub fn next_token(&mut self) -> Result<Token> {
        self.had_line_break = false;
        self.skip_whitespace_and_comments();
        let loc = self.location();
        let had_line_break = self.had_line_break;
        let kind = match self.peek_char() {
            Some(ch) => self.read_token_kind(ch, loc)?,
            None => TokenKind::Eof,
        };
        Ok(Token {
            kind,
            location: loc,
            had_line_break_before: had_line_break,
        })
    }
}

impl Lexer {
    pub(super) fn read_token_kind(&mut self, ch: char, loc: SourceLocation) -> Result<TokenKind> {
        match ch {
            '0'..='9' => self.read_number(),
            '"' | '\'' => self.read_string(),
            '`' => self.read_template_literal(),
            'a'..='z' | 'A'..='Z' | '_' | '$' => self.read_identifier_or_keyword(),
            ch if is_id_start(ch) => self.read_identifier_or_keyword(),
            '{' => Ok(self.read_lbrace_token()),
            '}' => self.read_rbrace_or_template_token(),
            '.' => self.read_dot_token(),
            '?' => Ok(self.read_question_token()),
            '+' => Ok(self.read_plus_token()),
            '-' => Ok(self.read_minus_token()),
            '*' => Ok(self.read_star_token()),
            '/' => Ok(self.read_slash_token()),
            '%' => Ok(self.read_percent_token()),
            '&' => Ok(self.read_ampersand_token()),
            '|' => Ok(self.read_pipe_token()),
            '^' => Ok(self.read_caret_token()),
            '!' => Ok(self.read_bang_token()),
            '=' => Ok(self.read_equal_token()),
            '<' => Ok(self.read_less_token()),
            '>' => Ok(self.read_greater_token()),
            _ => self.read_simple_punctuation(ch).ok_or_else(|| {
                RawJsError::syntax_error(format!("Unexpected character: '{}'", ch), Some(loc))
            }),
        }
    }
}

use super::*;
