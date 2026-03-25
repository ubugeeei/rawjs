impl Parser {
    /// Helper: expect a string literal token and return its value.
    pub(super) fn expect_string_literal(&mut self) -> Result<String> {
        match self.peek().clone() {
            TokenKind::String(s) => {
                self.advance();
                Ok(s)
            }
            _ => Err(RawJsError::syntax_error(
                format!("Expected string literal, got {:?}", self.peek()),
                Some(self.location()),
            )),
        }
    }
}

impl Parser {
    /// Helper: check if current token is an identifier with a specific name.
    pub(super) fn is_identifier_named(&self, name: &str) -> bool {
        matches ! (self . peek () , TokenKind :: Identifier (n) if n == name)
    }
}

impl Parser {
    /// Helper: expect an identifier with a specific name (e.g., "from", "as").
    pub(super) fn expect_identifier_matching(&mut self, name: &str) -> Result<()> {
        if self.is_identifier_named(name) {
            self.advance();
            Ok(())
        } else {
            Err(RawJsError::syntax_error(
                format!("Expected '{}', got {:?}", name, self.peek()),
                Some(self.location()),
            ))
        }
    }
}

use super::*;
