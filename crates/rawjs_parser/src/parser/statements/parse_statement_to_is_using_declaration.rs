use rawjs_ast::*;

use rawjs_common::Result;

use rawjs_lexer::TokenKind;

use super::Parser;

impl Parser {
    pub(crate) fn parse_statement(&mut self) -> Result<Statement> {
        match self.peek() {
            TokenKind::Var | TokenKind::Let | TokenKind::Const => {
                self.parse_variable_declaration_statement()
            }
            TokenKind::Function => {
                if self.is_function_declaration() {
                    self.parse_function_declaration()
                        .map(Statement::FunctionDeclaration)
                } else {
                    self.parse_expression_statement()
                }
            }
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_statement(),
            TokenKind::Do => self.parse_do_while_statement(),
            TokenKind::For => self.parse_for_statement(),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Break => self.parse_break_statement(),
            TokenKind::Continue => self.parse_continue_statement(),
            TokenKind::Switch => self.parse_switch_statement(),
            TokenKind::Throw => self.parse_throw_statement(),
            TokenKind::Try => self.parse_try_statement(),
            TokenKind::LBrace => self.parse_block_statement().map(Statement::Block),
            TokenKind::Semicolon => {
                self.advance();
                Ok(Statement::Empty)
            }
            TokenKind::Debugger => {
                self.advance();
                self.expect_semicolon()?;
                Ok(Statement::Debugger)
            }
            TokenKind::Class => self
                .parse_class_declaration()
                .map(Statement::ClassDeclaration),
            TokenKind::With => self.parse_with_statement(),
            TokenKind::Import => {
                if self.is_import_expression() {
                    self.parse_expression_statement()
                } else {
                    self.parse_import_declaration()
                }
            }
            TokenKind::Export => self.parse_export_declaration(),
            TokenKind::Async => {
                if self.pos + 1 < self.tokens.len()
                    && self.tokens[self.pos + 1].kind == TokenKind::Function
                    && !self.tokens[self.pos + 1].had_line_break_before
                {
                    self.parse_async_function_declaration()
                } else {
                    self.parse_expression_statement()
                }
            }
            TokenKind::Await => {
                if self.is_await_using_declaration() {
                    self.parse_using_declaration(true)
                } else {
                    self.parse_expression_statement()
                }
            }
            TokenKind::Identifier(ref name) if name == "using" => {
                if self.is_using_declaration() {
                    self.parse_using_declaration(false)
                } else {
                    self.parse_expression_statement()
                }
            }
            TokenKind::Identifier(_) => {
                if self.is_labeled_statement() {
                    self.parse_labeled_statement()
                } else if self.is_async_function() {
                    self.parse_async_function_declaration()
                } else {
                    self.parse_expression_statement()
                }
            }
            _ => self.parse_expression_statement(),
        }
    }
}

impl Parser {
    pub(super) fn is_function_declaration(&self) -> bool {
        if !self.at(&TokenKind::Function) {
            return false;
        }
        if self.pos + 1 < self.tokens.len() {
            matches!(
                self.tokens[self.pos + 1].kind,
                TokenKind::Identifier(_) | TokenKind::Star | TokenKind::LParen
            )
        } else {
            false
        }
    }
}

impl Parser {
    pub(super) fn is_labeled_statement(&self) -> bool {
        if let TokenKind::Identifier(_) = self.peek() {
            self.pos + 1 < self.tokens.len() && self.tokens[self.pos + 1].kind == TokenKind::Colon
        } else {
            false
        }
    }
}

impl Parser {
    pub(super) fn is_async_function(&self) -> bool {
        if let TokenKind::Identifier(name) = self.peek() {
            if name == "async"
                && self.pos + 1 < self.tokens.len()
                && self.tokens[self.pos + 1].kind == TokenKind::Function
                && !self.tokens[self.pos + 1].had_line_break_before
            {
                return true;
            }
        }
        false
    }
}

impl Parser {
    pub(super) fn is_import_expression(&self) -> bool {
        if !self.at(&TokenKind::Import) || self.pos + 1 >= self.tokens.len() {
            return false;
        }
        matches!(
            self.tokens[self.pos + 1].kind,
            TokenKind::LParen | TokenKind::Dot
        )
    }
}

impl Parser {
    #[doc = " Check if current `using` identifier starts a `using` declaration."]
    #[doc = " `using` is a contextual keyword, so we need to check that the next token"]
    #[doc = " is an identifier (the binding name), not an operator or semicolon."]
    pub(super) fn is_using_declaration(&self) -> bool {
        if self.pos + 1 >= self.tokens.len() {
            return false;
        }
        matches!(self.tokens[self.pos + 1].kind, TokenKind::Identifier(_))
            && !self.tokens[self.pos + 1].had_line_break_before
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
