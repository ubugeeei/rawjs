use rawjs_ast::*;
use rawjs_common::{RawJsError, Result};
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
            TokenKind::Import => self.parse_import_declaration(),
            TokenKind::Export => self.parse_export_declaration(),
            TokenKind::Async => {
                // `async function ...` declaration
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
                // `await using` declaration
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

    fn is_function_declaration(&self) -> bool {
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

    fn is_labeled_statement(&self) -> bool {
        if let TokenKind::Identifier(_) = self.peek() {
            self.pos + 1 < self.tokens.len() && self.tokens[self.pos + 1].kind == TokenKind::Colon
        } else {
            false
        }
    }

    fn is_async_function(&self) -> bool {
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

    /// Check if current `using` identifier starts a `using` declaration.
    /// `using` is a contextual keyword, so we need to check that the next token
    /// is an identifier (the binding name), not an operator or semicolon.
    fn is_using_declaration(&self) -> bool {
        if self.pos + 1 >= self.tokens.len() {
            return false;
        }
        // `using x = ...` — next token must be an identifier
        matches!(self.tokens[self.pos + 1].kind, TokenKind::Identifier(_))
            && !self.tokens[self.pos + 1].had_line_break_before
    }

    /// Check if current `await` token starts an `await using` declaration.
    fn is_await_using_declaration(&self) -> bool {
        if self.pos + 2 >= self.tokens.len() {
            return false;
        }
        matches!(
            &self.tokens[self.pos + 1].kind,
            TokenKind::Identifier(name) if name == "using"
        ) && !self.tokens[self.pos + 1].had_line_break_before
            && matches!(self.tokens[self.pos + 2].kind, TokenKind::Identifier(_))
            && !self.tokens[self.pos + 2].had_line_break_before
    }

    /// Parse a `using x = expr;` or `await using x = expr;` declaration.
    fn parse_using_declaration(&mut self, is_await: bool) -> Result<Statement> {
        let loc = self.location();

        if is_await {
            self.advance(); // consume `await`
        }
        self.advance(); // consume `using`

        let kind = if is_await {
            VarKind::AwaitUsing
        } else {
            VarKind::Using
        };

        let mut declarations = Vec::new();
        loop {
            let decl_loc = self.location();
            let id = self.parse_binding_pattern()?;

            // `using` declarations require an initializer.
            if !self.eat(&TokenKind::Assign) {
                return Err(RawJsError::syntax_error(
                    "Missing initializer in 'using' declaration",
                    Some(decl_loc),
                ));
            }
            let init = Some(self.parse_assignment_expression()?);

            declarations.push(VariableDeclarator {
                id,
                init,
                location: decl_loc,
            });
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }

        self.expect_semicolon()?;

        Ok(Statement::VariableDeclaration(VariableDeclaration {
            kind,
            declarations,
            location: loc,
        }))
    }

    fn parse_variable_declaration_statement(&mut self) -> Result<Statement> {
        let decl = self.parse_variable_declaration()?;
        self.expect_semicolon()?;
        Ok(Statement::VariableDeclaration(decl))
    }

    pub(crate) fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration> {
        let loc = self.location();
        let kind = match self.advance().kind {
            TokenKind::Var => VarKind::Var,
            TokenKind::Let => VarKind::Let,
            TokenKind::Const => VarKind::Const,
            _ => unreachable!(),
        };

        let mut declarations = Vec::new();
        loop {
            let decl_loc = self.location();
            let id = self.parse_binding_pattern()?;
            let init = if self.eat(&TokenKind::Assign) {
                Some(self.parse_assignment_expression()?)
            } else {
                None
            };
            declarations.push(VariableDeclarator {
                id,
                init,
                location: decl_loc,
            });
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }

        Ok(VariableDeclaration {
            kind,
            declarations,
            location: loc,
        })
    }

    pub(crate) fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration> {
        let loc = self.location();
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
        Ok(FunctionDeclaration {
            id,
            params,
            body: Box::new(body),
            is_async: false,
            is_generator,
            location: loc,
        })
    }

    fn parse_async_function_declaration(&mut self) -> Result<Statement> {
        let loc = self.location();
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
        Ok(Statement::FunctionDeclaration(FunctionDeclaration {
            id,
            params,
            body: Box::new(body),
            is_async: true,
            is_generator,
            location: loc,
        }))
    }

    fn parse_if_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::If)?;
        self.expect(&TokenKind::LParen)?;
        let test = self.parse_expression()?;
        self.expect(&TokenKind::RParen)?;
        let consequent = Box::new(self.parse_statement()?);
        let alternate = if self.eat(&TokenKind::Else) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };
        Ok(Statement::If(IfStatement {
            test,
            consequent,
            alternate,
            location: loc,
        }))
    }

    fn parse_while_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::While)?;
        self.expect(&TokenKind::LParen)?;
        let test = self.parse_expression()?;
        self.expect(&TokenKind::RParen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Statement::While(WhileStatement {
            test,
            body,
            location: loc,
        }))
    }

    fn parse_do_while_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Do)?;
        let body = Box::new(self.parse_statement()?);
        self.expect(&TokenKind::While)?;
        self.expect(&TokenKind::LParen)?;
        let test = self.parse_expression()?;
        self.expect(&TokenKind::RParen)?;
        self.expect_semicolon()?;
        Ok(Statement::DoWhile(DoWhileStatement {
            test,
            body,
            location: loc,
        }))
    }

    fn parse_for_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::For)?;
        self.expect(&TokenKind::LParen)?;

        if self.at(&TokenKind::Semicolon) {
            self.advance();
            let test = if self.at(&TokenKind::Semicolon) {
                None
            } else {
                Some(self.parse_expression()?)
            };
            self.expect(&TokenKind::Semicolon)?;
            let update = if self.at(&TokenKind::RParen) {
                None
            } else {
                Some(self.parse_expression()?)
            };
            self.expect(&TokenKind::RParen)?;
            let body = Box::new(self.parse_statement()?);
            return Ok(Statement::For(ForStatement {
                init: None,
                test,
                update,
                body,
                location: loc,
            }));
        }

        if self.at(&TokenKind::Var) || self.at(&TokenKind::Let) || self.at(&TokenKind::Const) {
            let decl = self.parse_variable_declaration()?;
            if self.at(&TokenKind::In) {
                self.advance();
                let right = self.parse_expression()?;
                self.expect(&TokenKind::RParen)?;
                let body = Box::new(self.parse_statement()?);
                return Ok(Statement::ForIn(ForInStatement {
                    left: ForInOfLeft::VariableDeclaration(decl),
                    right,
                    body,
                    location: loc,
                }));
            }
            if self.at_of() {
                self.advance();
                let right = self.parse_assignment_expression()?;
                self.expect(&TokenKind::RParen)?;
                let body = Box::new(self.parse_statement()?);
                return Ok(Statement::ForOf(ForOfStatement {
                    left: ForInOfLeft::VariableDeclaration(decl),
                    right,
                    body,
                    is_await: false,
                    location: loc,
                }));
            }
            self.expect(&TokenKind::Semicolon)?;
            let test = if self.at(&TokenKind::Semicolon) {
                None
            } else {
                Some(self.parse_expression()?)
            };
            self.expect(&TokenKind::Semicolon)?;
            let update = if self.at(&TokenKind::RParen) {
                None
            } else {
                Some(self.parse_expression()?)
            };
            self.expect(&TokenKind::RParen)?;
            let body = Box::new(self.parse_statement()?);
            return Ok(Statement::For(ForStatement {
                init: Some(ForInit::VariableDeclaration(decl)),
                test,
                update,
                body,
                location: loc,
            }));
        }

        let expr = self.parse_expression_no_in()?;
        if self.at(&TokenKind::In) {
            self.advance();
            let right = self.parse_expression()?;
            self.expect(&TokenKind::RParen)?;
            let body = Box::new(self.parse_statement()?);
            let left = ForInOfLeft::Pattern(self.expression_to_pattern(expr)?);
            return Ok(Statement::ForIn(ForInStatement {
                left,
                right,
                body,
                location: loc,
            }));
        }
        if self.at_of() {
            self.advance();
            let right = self.parse_assignment_expression()?;
            self.expect(&TokenKind::RParen)?;
            let body = Box::new(self.parse_statement()?);
            let left = ForInOfLeft::Pattern(self.expression_to_pattern(expr)?);
            return Ok(Statement::ForOf(ForOfStatement {
                left,
                right,
                body,
                is_await: false,
                location: loc,
            }));
        }

        self.expect(&TokenKind::Semicolon)?;
        let test = if self.at(&TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.expect(&TokenKind::Semicolon)?;
        let update = if self.at(&TokenKind::RParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.expect(&TokenKind::RParen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Statement::For(ForStatement {
            init: Some(ForInit::Expression(expr)),
            test,
            update,
            body,
            location: loc,
        }))
    }

    fn parse_expression_no_in(&mut self) -> Result<Expression> {
        self.parse_assignment_expression()
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Return)?;
        let argument = if self.at(&TokenKind::Semicolon)
            || self.at(&TokenKind::RBrace)
            || self.at(&TokenKind::Eof)
            || self.current_token().had_line_break_before
        {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.expect_semicolon()?;
        Ok(Statement::Return(ReturnStatement {
            argument,
            location: loc,
        }))
    }

    fn parse_break_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Break)?;
        let label = if !self.at(&TokenKind::Semicolon)
            && !self.at(&TokenKind::RBrace)
            && !self.at(&TokenKind::Eof)
            && !self.current_token().had_line_break_before
        {
            if let TokenKind::Identifier(_) = self.peek() {
                Some(self.expect_identifier()?)
            } else {
                None
            }
        } else {
            None
        };
        self.expect_semicolon()?;
        Ok(Statement::Break(BreakStatement {
            label,
            location: loc,
        }))
    }

    fn parse_continue_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Continue)?;
        let label = if !self.at(&TokenKind::Semicolon)
            && !self.at(&TokenKind::RBrace)
            && !self.at(&TokenKind::Eof)
            && !self.current_token().had_line_break_before
        {
            if let TokenKind::Identifier(_) = self.peek() {
                Some(self.expect_identifier()?)
            } else {
                None
            }
        } else {
            None
        };
        self.expect_semicolon()?;
        Ok(Statement::Continue(ContinueStatement {
            label,
            location: loc,
        }))
    }

    fn parse_switch_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Switch)?;
        self.expect(&TokenKind::LParen)?;
        let discriminant = self.parse_expression()?;
        self.expect(&TokenKind::RParen)?;
        self.expect(&TokenKind::LBrace)?;
        let mut cases = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            let case_loc = self.location();
            let test = if self.eat(&TokenKind::Case) {
                Some(self.parse_expression()?)
            } else {
                self.expect(&TokenKind::Default)?;
                None
            };
            self.expect(&TokenKind::Colon)?;
            let mut consequent = Vec::new();
            while !self.at(&TokenKind::Case)
                && !self.at(&TokenKind::Default)
                && !self.at(&TokenKind::RBrace)
                && !self.at(&TokenKind::Eof)
            {
                consequent.push(self.parse_statement()?);
            }
            cases.push(SwitchCase {
                test,
                consequent,
                location: case_loc,
            });
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(Statement::Switch(SwitchStatement {
            discriminant,
            cases,
            location: loc,
        }))
    }

    fn parse_throw_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Throw)?;
        if self.current_token().had_line_break_before {
            return Err(RawJsError::syntax_error(
                "Illegal newline after throw",
                Some(loc),
            ));
        }
        let argument = self.parse_expression()?;
        self.expect_semicolon()?;
        Ok(Statement::Throw(ThrowStatement {
            argument,
            location: loc,
        }))
    }

    fn parse_try_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Try)?;
        let block = self.parse_block_statement()?;
        let handler = if self.eat(&TokenKind::Catch) {
            let catch_loc = self.location();
            let param = if self.eat(&TokenKind::LParen) {
                let p = self.parse_binding_pattern()?;
                self.expect(&TokenKind::RParen)?;
                Some(p)
            } else {
                None
            };
            let body = self.parse_block_statement()?;
            Some(CatchClause {
                param,
                body,
                location: catch_loc,
            })
        } else {
            None
        };
        let finalizer = if self.eat(&TokenKind::Finally) {
            Some(self.parse_block_statement()?)
        } else {
            None
        };
        Ok(Statement::Try(TryStatement {
            block,
            handler,
            finalizer,
            location: loc,
        }))
    }

    pub(crate) fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let loc = self.location();
        self.expect(&TokenKind::LBrace)?;
        let mut body = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            body.push(self.parse_statement()?);
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(BlockStatement {
            body,
            location: loc,
        })
    }

    fn parse_labeled_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        let label = self.expect_identifier()?;
        self.expect(&TokenKind::Colon)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Statement::Labeled(LabeledStatement {
            label,
            body,
            location: loc,
        }))
    }

    fn parse_with_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::With)?;
        self.expect(&TokenKind::LParen)?;
        let object = self.parse_expression()?;
        self.expect(&TokenKind::RParen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Statement::With(WithStatement {
            object,
            body,
            location: loc,
        }))
    }

    pub(crate) fn parse_class_declaration(&mut self) -> Result<ClassDeclaration> {
        let loc = self.location();
        self.expect(&TokenKind::Class)?;
        let id = if let TokenKind::Identifier(_) = self.peek() {
            Some(self.expect_identifier()?)
        } else {
            None
        };
        let super_class = if self.eat(&TokenKind::Extends) {
            Some(Box::new(self.parse_left_hand_side_expression()?))
        } else {
            None
        };
        self.expect(&TokenKind::LBrace)?;
        let mut body = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            if self.eat(&TokenKind::Semicolon) {
                continue;
            }
            body.push(self.parse_class_member()?);
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(ClassDeclaration {
            id,
            super_class,
            body,
            location: loc,
        })
    }

    fn parse_class_member(&mut self) -> Result<ClassMember> {
        let loc = self.location();
        let is_static = if let TokenKind::Identifier(name) = self.peek() {
            if name == "static" {
                self.advance();
                true
            } else {
                false
            }
        } else {
            false
        };

        let mut kind = ClassMemberKind::Method;
        if let TokenKind::Identifier(name) = self.peek() {
            if name == "get" && !self.is_next_lparen() {
                self.advance();
                kind = ClassMemberKind::Get;
            } else if name == "set" && !self.is_next_lparen() {
                self.advance();
                kind = ClassMemberKind::Set;
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

        if let Expression::Identifier(ref id) = key {
            if id.name == "constructor" {
                kind = ClassMemberKind::Constructor;
            }
        }

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
            location: loc,
        });

        Ok(ClassMember {
            key,
            value: Some(value),
            kind,
            is_static,
            computed,
            location: loc,
        })
    }

    pub(crate) fn parse_expression_statement(&mut self) -> Result<Statement> {
        let loc = self.location();
        let expression = self.parse_expression()?;
        self.expect_semicolon()?;
        Ok(Statement::Expression(ExpressionStatement {
            expression,
            location: loc,
        }))
    }

    // -----------------------------------------------------------------
    // ESM: import / export
    // -----------------------------------------------------------------

    fn parse_import_declaration(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Import)?; // consume `import`

        let mut specifiers = Vec::new();

        match self.peek().clone() {
            // import "module"  (side-effect import)
            TokenKind::String(_) => {
                let source = self.expect_string_literal()?;
                self.expect_semicolon()?;
                return Ok(Statement::ImportDeclaration(ImportDeclaration {
                    specifiers,
                    source,
                    location: loc,
                }));
            }
            // import * as ns from "module"
            TokenKind::Star => {
                self.advance(); // consume *
                self.expect_identifier_matching("as")?;
                let local = self.expect_identifier()?;
                specifiers.push(ImportSpecifier::Namespace {
                    local,
                    location: self.location(),
                });
            }
            // import { a, b as c } from "module"
            TokenKind::LBrace => {
                self.advance(); // consume {
                while !self.at(&TokenKind::RBrace) {
                    let imported = self.expect_identifier_or_keyword()?;
                    let local = if self.is_identifier_named("as") {
                        self.advance(); // consume `as`
                        self.expect_identifier()?
                    } else {
                        imported.clone()
                    };
                    specifiers.push(ImportSpecifier::Named {
                        imported,
                        local,
                        location: self.location(),
                    });
                    if !self.at(&TokenKind::RBrace) {
                        self.expect(&TokenKind::Comma)?;
                    }
                }
                self.expect(&TokenKind::RBrace)?;
            }
            // import defaultExport from "module"
            // import defaultExport, { named } from "module"
            _ => {
                let local = self.expect_identifier()?;
                specifiers.push(ImportSpecifier::Default {
                    local,
                    location: self.location(),
                });
                // Check for `, { ... }` or `, * as ns`
                if self.at(&TokenKind::Comma) {
                    self.advance(); // consume ,
                    match self.peek().clone() {
                        TokenKind::LBrace => {
                            self.advance(); // consume {
                            while !self.at(&TokenKind::RBrace) {
                                let imported = self.expect_identifier_or_keyword()?;
                                let local = if self.is_identifier_named("as") {
                                    self.advance();
                                    self.expect_identifier()?
                                } else {
                                    imported.clone()
                                };
                                specifiers.push(ImportSpecifier::Named {
                                    imported,
                                    local,
                                    location: self.location(),
                                });
                                if !self.at(&TokenKind::RBrace) {
                                    self.expect(&TokenKind::Comma)?;
                                }
                            }
                            self.expect(&TokenKind::RBrace)?;
                        }
                        TokenKind::Star => {
                            self.advance(); // consume *
                            self.expect_identifier_matching("as")?;
                            let local = self.expect_identifier()?;
                            specifiers.push(ImportSpecifier::Namespace {
                                local,
                                location: self.location(),
                            });
                        }
                        _ => {
                            return Err(RawJsError::syntax_error(
                                "Expected '{' or '*' after ',' in import",
                                Some(self.location()),
                            ));
                        }
                    }
                }
            }
        }

        // expect `from "source"`
        self.expect_identifier_matching("from")?;
        let source = self.expect_string_literal()?;
        self.expect_semicolon()?;

        Ok(Statement::ImportDeclaration(ImportDeclaration {
            specifiers,
            source,
            location: loc,
        }))
    }

    fn parse_export_declaration(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Export)?; // consume `export`

        // export default ...
        if self.at(&TokenKind::Default) {
            self.advance(); // consume `default`
            let expr = self.parse_expression()?;
            self.expect_semicolon()?;
            return Ok(Statement::ExportDeclaration(ExportDeclaration {
                kind: ExportKind::Default(expr),
                location: loc,
            }));
        }

        // export * from "module"
        if self.at(&TokenKind::Star) {
            self.advance(); // consume *
            self.expect_identifier_matching("from")?;
            let source = self.expect_string_literal()?;
            self.expect_semicolon()?;
            return Ok(Statement::ExportDeclaration(ExportDeclaration {
                kind: ExportKind::AllFrom(source),
                location: loc,
            }));
        }

        // export { a, b as c } or export { a, b } from "module"
        if self.at(&TokenKind::LBrace) {
            self.advance(); // consume {
            let mut specifiers = Vec::new();
            while !self.at(&TokenKind::RBrace) {
                let local = self.expect_identifier_or_keyword()?;
                let exported = if self.is_identifier_named("as") {
                    self.advance();
                    self.expect_identifier_or_keyword()?
                } else {
                    local.clone()
                };
                specifiers.push(ExportSpecifier {
                    local,
                    exported,
                    location: self.location(),
                });
                if !self.at(&TokenKind::RBrace) {
                    self.expect(&TokenKind::Comma)?;
                }
            }
            self.expect(&TokenKind::RBrace)?;

            // Check for `from "module"`
            if self.is_identifier_named("from") {
                self.advance();
                let source = self.expect_string_literal()?;
                self.expect_semicolon()?;
                return Ok(Statement::ExportDeclaration(ExportDeclaration {
                    kind: ExportKind::NamedFrom(specifiers, source),
                    location: loc,
                }));
            }

            self.expect_semicolon()?;
            return Ok(Statement::ExportDeclaration(ExportDeclaration {
                kind: ExportKind::Named(specifiers),
                location: loc,
            }));
        }

        // export function / const / let / var / class
        let decl = self.parse_statement()?;
        Ok(Statement::ExportDeclaration(ExportDeclaration {
            kind: ExportKind::Declaration(Box::new(decl)),
            location: loc,
        }))
    }

    /// Helper: expect a string literal token and return its value.
    fn expect_string_literal(&mut self) -> Result<String> {
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

    /// Helper: check if current token is an identifier with a specific name.
    fn is_identifier_named(&self, name: &str) -> bool {
        matches!(self.peek(), TokenKind::Identifier(n) if n == name)
    }

    /// Helper: expect an identifier with a specific name (e.g., "from", "as").
    fn expect_identifier_matching(&mut self, name: &str) -> Result<()> {
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
