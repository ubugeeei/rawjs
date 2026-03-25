impl Parser {
    fn parse_import_declaration(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Import)?;
        let mut specifiers = Vec::new();
        match self.peek().clone() {
            TokenKind::String(_) => {
                let source = self.expect_string_literal()?;
                self.expect_semicolon()?;
                return Ok(Statement::ImportDeclaration(ImportDeclaration {
                    specifiers,
                    source,
                    location: loc,
                }));
            }
            TokenKind::Star => {
                self.advance();
                self.expect_identifier_matching("as")?;
                let local = self.expect_identifier()?;
                specifiers.push(ImportSpecifier::Namespace {
                    local,
                    location: self.location(),
                });
            }
            TokenKind::LBrace => {
                self.advance();
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
            _ => {
                let local = self.expect_identifier()?;
                specifiers.push(ImportSpecifier::Default {
                    local,
                    location: self.location(),
                });
                if self.at(&TokenKind::Comma) {
                    self.advance();
                    match self.peek().clone() {
                        TokenKind::LBrace => {
                            self.advance();
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
                            self.advance();
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
        self.expect_identifier_matching("from")?;
        let source = self.expect_string_literal()?;
        self.expect_semicolon()?;
        Ok(Statement::ImportDeclaration(ImportDeclaration {
            specifiers,
            source,
            location: loc,
        }))
    }
}

impl Parser {
    fn parse_export_declaration(&mut self) -> Result<Statement> {
        let loc = self.location();
        self.expect(&TokenKind::Export)?;
        if self.at(&TokenKind::Default) {
            self.advance();
            let expr = self.parse_expression()?;
            self.expect_semicolon()?;
            return Ok(Statement::ExportDeclaration(ExportDeclaration {
                kind: ExportKind::Default(expr),
                location: loc,
            }));
        }
        if self.at(&TokenKind::Star) {
            self.advance();
            self.expect_identifier_matching("from")?;
            let source = self.expect_string_literal()?;
            self.expect_semicolon()?;
            return Ok(Statement::ExportDeclaration(ExportDeclaration {
                kind: ExportKind::AllFrom(source),
                location: loc,
            }));
        }
        if self.at(&TokenKind::LBrace) {
            self.advance();
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
        let decl = self.parse_statement()?;
        Ok(Statement::ExportDeclaration(ExportDeclaration {
            kind: ExportKind::Declaration(Box::new(decl)),
            location: loc,
        }))
    }
}
