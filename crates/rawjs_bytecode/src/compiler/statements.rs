use rawjs_ast::*;
use rawjs_common::{RawJsError, Result};

use crate::opcode::Instruction;

use super::{expression_to_property_name, Compiler};

impl Compiler {
    // ------------------------------------------------------------------
    //  Statements
    // ------------------------------------------------------------------

    pub(crate) fn compile_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Expression(es) => self.compile_expression_statement(es),
            Statement::VariableDeclaration(vd) => self.compile_variable_declaration(vd),
            Statement::FunctionDeclaration(_) => {
                // Already hoisted at the top of compile_program / block.
                Ok(())
            }
            Statement::Return(ret) => self.compile_return(ret),
            Statement::If(if_stmt) => self.compile_if(if_stmt),
            Statement::While(while_stmt) => self.compile_while(while_stmt),
            Statement::DoWhile(do_while) => self.compile_do_while(do_while),
            Statement::For(for_stmt) => self.compile_for(for_stmt),
            Statement::ForIn(for_in) => self.compile_for_in(for_in),
            Statement::ForOf(for_of) => self.compile_for_of(for_of),
            Statement::Block(block) => self.compile_block(block),
            Statement::Break(brk) => self.compile_break(brk),
            Statement::Continue(cont) => self.compile_continue(cont),
            Statement::Switch(sw) => self.compile_switch(sw),
            Statement::Throw(th) => self.compile_throw(th),
            Statement::Try(try_stmt) => self.compile_try(try_stmt),
            Statement::Labeled(labeled) => self.compile_labeled(labeled),
            Statement::With(_) => Err(RawJsError::syntax_error(
                "'with' statements are not supported in strict mode",
                None,
            )),
            Statement::Empty | Statement::Debugger => Ok(()),
            Statement::ClassDeclaration(cls) => self.compile_class_declaration(cls),
            Statement::ImportDeclaration(import_decl) => self.compile_import(import_decl),
            Statement::ExportDeclaration(export_decl) => self.compile_export(export_decl),
        }
    }

    fn compile_expression_statement(&mut self, es: &ExpressionStatement) -> Result<()> {
        self.compile_expression(&es.expression)?;
        self.emit(Instruction::Pop);
        Ok(())
    }

    pub(crate) fn compile_variable_declaration(&mut self, vd: &VariableDeclaration) -> Result<()> {
        let is_using = matches!(vd.kind, VarKind::Using | VarKind::AwaitUsing);
        let is_await_using = vd.kind == VarKind::AwaitUsing;

        for decl in &vd.declarations {
            match &decl.id {
                Pattern::Identifier(id) => {
                    let slot = self.declare_local(&id.name)?;
                    if let Some(ref init) = decl.init {
                        self.compile_expression(init)?;
                    } else {
                        self.emit(Instruction::Undefined);
                    }
                    self.emit(Instruction::StoreLocal(slot));

                    // Track `using` bindings for dispose on scope exit.
                    if is_using {
                        if let Some(scope) = self.dispose_scopes.last_mut() {
                            scope.push((slot, is_await_using));
                        }
                    }
                }
                _ => {
                    // Destructuring patterns -- compile init, then destructure.
                    if let Some(ref init) = decl.init {
                        self.compile_expression(init)?;
                    } else {
                        self.emit(Instruction::Undefined);
                    }
                    self.compile_destructuring_declaration(&decl.id)?;
                }
            }
        }
        Ok(())
    }

    /// Compile a destructuring pattern for a variable declaration.
    /// Expects the value to destructure on TOS.  Leaves locals on the stack.
    pub(crate) fn compile_destructuring_declaration(&mut self, pattern: &Pattern) -> Result<()> {
        match pattern {
            Pattern::Identifier(id) => {
                // Value is on TOS.  Declare a local -- TOS becomes the local.
                self.declare_local(&id.name)?;
                Ok(())
            }
            Pattern::Array(arr) => {
                // TOS is the array value.
                for (i, elem) in arr.elements.iter().enumerate() {
                    if let Some(pat) = elem {
                        self.emit(Instruction::Dup);
                        let idx = self.add_number_constant(i as f64)?;
                        self.emit(Instruction::LoadConst(idx));
                        self.emit(Instruction::GetComputed);
                        self.compile_destructuring_declaration(pat)?;
                    }
                }
                // Pop the original array value.
                self.emit(Instruction::Pop);
                Ok(())
            }
            Pattern::Object(obj) => {
                for prop in &obj.properties {
                    self.emit(Instruction::Dup);
                    if prop.computed {
                        self.compile_expression(&prop.key)?;
                        self.emit(Instruction::GetComputed);
                    } else {
                        let key_name = expression_to_property_name(&prop.key)?;
                        let idx = self.add_string_constant(&key_name)?;
                        self.emit(Instruction::GetProperty(idx));
                    }
                    self.compile_destructuring_declaration(&prop.value)?;
                }
                self.emit(Instruction::Pop);
                Ok(())
            }
            Pattern::Assignment(asn) => {
                // Default value: if TOS is undefined, use the default.
                self.emit(Instruction::Dup);
                self.emit(Instruction::Undefined);
                self.emit(Instruction::StrictEq);
                let jump_idx = self.emit_jump(Instruction::JumpIfFalse);
                self.emit(Instruction::Pop); // pop the original undefined
                self.compile_expression(&asn.right)?;
                let end_idx = self.emit_jump(Instruction::Jump);
                self.patch_jump_to_here(jump_idx);
                // If not undefined, value is still on TOS, nothing to do.
                self.patch_jump_to_here(end_idx);
                self.compile_destructuring_declaration(&asn.left)?;
                Ok(())
            }
            Pattern::Rest(rest) => {
                // Simplified: treat rest as just taking the current value.
                self.compile_destructuring_declaration(&rest.argument)?;
                Ok(())
            }
        }
    }

    fn compile_return(&mut self, ret: &ReturnStatement) -> Result<()> {
        if let Some(ref arg) = ret.argument {
            self.compile_expression(arg)?;
        } else {
            self.emit(Instruction::Undefined);
        }
        self.emit(Instruction::Return);
        Ok(())
    }

    fn compile_if(&mut self, if_stmt: &IfStatement) -> Result<()> {
        self.compile_expression(&if_stmt.test)?;
        let else_jump = self.emit_jump(Instruction::JumpIfFalse);

        self.compile_statement(&if_stmt.consequent)?;

        if let Some(ref alt) = if_stmt.alternate {
            let end_jump = self.emit_jump(Instruction::Jump);
            self.patch_jump_to_here(else_jump);
            self.compile_statement(alt)?;
            self.patch_jump_to_here(end_jump);
        } else {
            self.patch_jump_to_here(else_jump);
        }
        Ok(())
    }

    fn compile_while(&mut self, while_stmt: &WhileStatement) -> Result<()> {
        let loop_start = self.chunk.current_offset();

        self.push_loop_context(loop_start, None);

        self.compile_expression(&while_stmt.test)?;
        let exit_jump = self.emit_jump(Instruction::JumpIfFalse);

        self.compile_statement(&while_stmt.body)?;

        // Jump back to loop_start.
        let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::Jump(back_offset));

        self.patch_jump_to_here(exit_jump);
        self.pop_loop_context_and_patch_breaks();
        Ok(())
    }

    fn compile_do_while(&mut self, do_while: &DoWhileStatement) -> Result<()> {
        let loop_start = self.chunk.current_offset();

        self.push_loop_context(loop_start, None);

        self.compile_statement(&do_while.body)?;

        self.compile_expression(&do_while.test)?;
        // Jump back if truthy.
        let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::JumpIfTrue(back_offset));

        self.pop_loop_context_and_patch_breaks();
        Ok(())
    }

    fn compile_for(&mut self, for_stmt: &ForStatement) -> Result<()> {
        self.begin_scope();

        // Init
        if let Some(ref init) = for_stmt.init {
            match init {
                ForInit::VariableDeclaration(vd) => self.compile_variable_declaration(vd)?,
                ForInit::Expression(expr) => {
                    self.compile_expression(expr)?;
                    self.emit(Instruction::Pop);
                }
            }
        }

        let loop_start = self.chunk.current_offset();
        self.push_loop_context(loop_start, None);

        // Test
        let exit_jump = if let Some(ref test) = for_stmt.test {
            self.compile_expression(test)?;
            Some(self.emit_jump(Instruction::JumpIfFalse))
        } else {
            None
        };

        // Body
        self.compile_statement(&for_stmt.body)?;

        // Update -- this is also the continue target.
        let continue_target = self.chunk.current_offset();
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.continue_target = continue_target;
        }

        if let Some(ref update) = for_stmt.update {
            self.compile_expression(update)?;
            self.emit(Instruction::Pop);
        }

        // Jump back
        let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::Jump(back_offset));

        if let Some(exit) = exit_jump {
            self.patch_jump_to_here(exit);
        }

        self.pop_loop_context_and_patch_breaks();
        self.end_scope();
        Ok(())
    }

    fn compile_for_in(&mut self, for_in: &ForInStatement) -> Result<()> {
        self.begin_scope();

        // Evaluate the object expression and initialize the for-in iterator.
        // Stack after ForInInit: [..., keys_array, 0]
        self.compile_expression(&for_in.right)?;
        self.emit(Instruction::ForInInit);

        // Declare the iteration variable.
        let var_slot = match &for_in.left {
            ForInOfLeft::VariableDeclaration(vd) => {
                if let Some(decl) = vd.declarations.first() {
                    match &decl.id {
                        Pattern::Identifier(id) => self.declare_local(&id.name)?,
                        _ => {
                            return Err(RawJsError::syntax_error(
                                "Destructuring in for-in is not supported",
                                None,
                            ))
                        }
                    }
                } else {
                    return Err(RawJsError::syntax_error(
                        "for-in requires a variable declaration",
                        None,
                    ));
                }
            }
            ForInOfLeft::Pattern(pat) => match pat {
                Pattern::Identifier(id) => {
                    // Resolve existing local or declare a new one.
                    if let Some(slot) = self.resolve_local(&id.name) {
                        slot
                    } else {
                        self.declare_local(&id.name)?
                    }
                }
                _ => {
                    return Err(RawJsError::syntax_error(
                        "Destructuring in for-in is not supported",
                        None,
                    ))
                }
            },
        };

        // Loop header: ForInNext checks if iteration is done.
        let loop_start = self.chunk.current_offset();
        self.push_loop_context(loop_start, None);

        // ForInNext: if done, jump to exit. Otherwise pushes the next key.
        // Stack before: [..., keys_array, index]
        // Stack after (not done): [..., keys_array, index+1, key]
        let for_in_next_idx = self.emit(Instruction::ForInNext(0)); // placeholder

        // Store the key into the iteration variable.
        self.emit(Instruction::StoreLocal(var_slot));

        // Compile the loop body.
        self.compile_statement(&for_in.body)?;

        // Continue target: jump back to loop header.
        let continue_target = self.chunk.current_offset();
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.continue_target = continue_target;
        }

        let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::Jump(back_offset));

        // Patch the ForInNext exit jump to here.
        let exit_offset = self.chunk.current_offset() as i32 - for_in_next_idx as i32 - 1;
        self.chunk.instructions[for_in_next_idx] = Instruction::ForInNext(exit_offset);

        self.pop_loop_context_and_patch_breaks();

        // Clean up: pop the index and keys_array from the stack.
        self.emit(Instruction::Pop); // index
        self.emit(Instruction::Pop); // keys_array

        self.end_scope();
        Ok(())
    }

    fn compile_for_of(&mut self, for_of: &ForOfStatement) -> Result<()> {
        self.begin_scope();

        // Evaluate the iterable expression and get its iterator.
        // Stack after GetIterator: [..., iterator]
        self.compile_expression(&for_of.right)?;
        self.emit(Instruction::GetIterator);

        // Declare the iteration variable.
        let var_slot = match &for_of.left {
            ForInOfLeft::VariableDeclaration(vd) => {
                if let Some(decl) = vd.declarations.first() {
                    match &decl.id {
                        Pattern::Identifier(id) => self.declare_local(&id.name)?,
                        _ => {
                            return Err(RawJsError::syntax_error(
                                "Destructuring in for-of is not yet supported",
                                None,
                            ))
                        }
                    }
                } else {
                    return Err(RawJsError::syntax_error(
                        "for-of requires a variable declaration",
                        None,
                    ));
                }
            }
            ForInOfLeft::Pattern(pat) => match pat {
                Pattern::Identifier(id) => {
                    if let Some(slot) = self.resolve_local(&id.name) {
                        slot
                    } else {
                        self.declare_local(&id.name)?
                    }
                }
                _ => {
                    return Err(RawJsError::syntax_error(
                        "Destructuring in for-of is not yet supported",
                        None,
                    ))
                }
            },
        };

        // Loop header.
        let loop_start = self.chunk.current_offset();
        self.push_loop_context(loop_start, None);

        // IteratorNext: pushes value and done onto stack.
        // Stack: [..., iterator] -> [..., iterator, value, done]
        self.emit(Instruction::IteratorNext);

        // IteratorDone: if done, jump to exit. If not done, value stays on stack.
        // Stack after (not done): [..., iterator, value]
        let iter_done_idx = self.emit(Instruction::IteratorDone(0)); // placeholder

        // Store the value into the iteration variable.
        self.emit(Instruction::StoreLocal(var_slot));

        // Compile the loop body.
        self.compile_statement(&for_of.body)?;

        // Continue target: jump back to loop header.
        let continue_target = self.chunk.current_offset();
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.continue_target = continue_target;
        }

        let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::Jump(back_offset));

        // Patch the IteratorDone exit jump to here.
        let exit_offset = self.chunk.current_offset() as i32 - iter_done_idx as i32 - 1;
        self.chunk.instructions[iter_done_idx] = Instruction::IteratorDone(exit_offset);

        self.pop_loop_context_and_patch_breaks();

        // Clean up: pop the iterator from the stack.
        self.emit(Instruction::Pop); // iterator

        self.end_scope();
        Ok(())
    }

    pub(crate) fn compile_block(&mut self, block: &BlockStatement) -> Result<()> {
        self.begin_scope();

        // Hoist function declarations inside the block.
        for stmt in &block.body {
            if let Statement::FunctionDeclaration(func) = stmt {
                self.hoist_function_declaration(func)?;
            }
        }

        for stmt in &block.body {
            if matches!(stmt, Statement::FunctionDeclaration(_)) {
                continue;
            }
            self.compile_statement(stmt)?;
        }

        self.end_scope();
        Ok(())
    }

    fn compile_break(&mut self, brk: &BreakStatement) -> Result<()> {
        let ctx_idx = if let Some(ref label) = brk.label {
            self.loop_stack
                .iter()
                .rposition(|c| c.label.as_deref() == Some(label))
                .ok_or_else(|| {
                    RawJsError::syntax_error(
                        format!("Undefined label '{}'", label),
                        Some(brk.location),
                    )
                })?
        } else {
            self.loop_stack.len().checked_sub(1).ok_or_else(|| {
                RawJsError::syntax_error("'break' outside of loop or switch", Some(brk.location))
            })?
        };

        // Pop locals between current scope and the loop scope.
        let loop_depth = self.loop_stack[ctx_idx].scope_depth;
        let locals_to_pop = self
            .locals
            .iter()
            .rev()
            .take_while(|l| l.depth > loop_depth)
            .count();
        for _ in 0..locals_to_pop {
            self.emit(Instruction::Pop);
        }

        let jump_idx = self.emit_jump(Instruction::Jump);
        self.loop_stack[ctx_idx].break_jumps.push(jump_idx);
        Ok(())
    }

    fn compile_continue(&mut self, cont: &ContinueStatement) -> Result<()> {
        let ctx_idx = if let Some(ref label) = cont.label {
            self.loop_stack
                .iter()
                .rposition(|c| c.label.as_deref() == Some(label))
                .ok_or_else(|| {
                    RawJsError::syntax_error(
                        format!("Undefined label '{}'", label),
                        Some(cont.location),
                    )
                })?
        } else {
            self.loop_stack.len().checked_sub(1).ok_or_else(|| {
                RawJsError::syntax_error("'continue' outside of loop", Some(cont.location))
            })?
        };

        let loop_depth = self.loop_stack[ctx_idx].scope_depth;
        let locals_to_pop = self
            .locals
            .iter()
            .rev()
            .take_while(|l| l.depth > loop_depth)
            .count();
        for _ in 0..locals_to_pop {
            self.emit(Instruction::Pop);
        }

        let continue_target = self.loop_stack[ctx_idx].continue_target;
        let offset = continue_target as i32 - self.chunk.current_offset() as i32 - 1;
        self.emit(Instruction::Jump(offset));
        Ok(())
    }

    fn compile_switch(&mut self, sw: &SwitchStatement) -> Result<()> {
        self.compile_expression(&sw.discriminant)?;

        // We treat switch as a pseudo-loop (so `break` works).
        let switch_start = self.chunk.current_offset();
        self.push_loop_context(switch_start, None);

        // Phase 1: emit comparisons and conditional jumps for each case.
        let mut case_jump_targets: Vec<usize> = Vec::new();
        let mut default_jump: Option<usize> = None;

        for case in &sw.cases {
            if let Some(ref test) = case.test {
                self.emit(Instruction::Dup); // duplicate discriminant
                self.compile_expression(test)?;
                self.emit(Instruction::StrictEq);
                let jump_idx = self.emit_jump(Instruction::JumpIfTrue);
                case_jump_targets.push(jump_idx);
            } else {
                // default case
                let jump_idx = self.emit_jump(Instruction::Jump);
                default_jump = Some(jump_idx);
                case_jump_targets.push(0); // placeholder, not used
            }
        }

        // If no case matched and no default, jump past the switch.
        let end_no_match = self.emit_jump(Instruction::Jump);

        // Phase 2: emit bodies and patch case jumps.
        for (i, case) in sw.cases.iter().enumerate() {
            if case.test.is_some() {
                self.patch_jump_to_here(case_jump_targets[i]);
            } else if let Some(dj) = default_jump {
                self.patch_jump_to_here(dj);
            }
            for stmt in &case.consequent {
                self.compile_statement(stmt)?;
            }
        }

        self.patch_jump_to_here(end_no_match);
        // Pop the discriminant.
        self.emit(Instruction::Pop);

        self.pop_loop_context_and_patch_breaks();
        Ok(())
    }

    fn compile_throw(&mut self, th: &ThrowStatement) -> Result<()> {
        self.compile_expression(&th.argument)?;
        self.emit(Instruction::Throw);
        Ok(())
    }

    fn compile_try(&mut self, try_stmt: &TryStatement) -> Result<()> {
        let enter_try_idx = self.emit(Instruction::EnterTry(0, 0));

        // --- try block ---
        self.begin_scope();
        for stmt in &try_stmt.block.body {
            self.compile_statement(stmt)?;
        }
        self.end_scope();
        self.emit(Instruction::LeaveTry);

        // Jump over catch/finally on normal completion.
        let jump_over_catch = self.emit_jump(Instruction::Jump);

        // --- catch block ---
        if let Some(ref handler) = try_stmt.handler {
            // Patch the catch offset.
            let catch_offset = self.chunk.current_offset() as i32 - enter_try_idx as i32 - 1;
            self.chunk.patch_jump(enter_try_idx, catch_offset);

            self.begin_scope();

            // The VM pushes the thrown value onto the stack before jumping here.
            if let Some(ref param) = handler.param {
                match param {
                    Pattern::Identifier(id) => {
                        let slot = self.declare_local(&id.name)?;
                        // Store the thrown value (on TOS) into the catch variable's local slot.
                        self.emit(Instruction::StoreLocal(slot));
                    }
                    _ => {
                        self.compile_destructuring_declaration(param)?;
                    }
                }
            } else {
                // No param -- discard the thrown value.
                self.emit(Instruction::Pop);
            }

            for stmt in &handler.body.body {
                self.compile_statement(stmt)?;
            }
            self.end_scope();
        } else {
            // No catch block -- patch catch offset to 0 (already 0).
        }

        self.patch_jump_to_here(jump_over_catch);

        // --- finally block ---
        if let Some(ref finalizer) = try_stmt.finalizer {
            let finally_offset = self.chunk.current_offset() as i32 - enter_try_idx as i32 - 1;
            self.chunk.patch_finally(enter_try_idx, finally_offset);

            self.begin_scope();
            for stmt in &finalizer.body {
                self.compile_statement(stmt)?;
            }
            self.end_scope();
        }

        Ok(())
    }

    fn compile_labeled(&mut self, labeled: &LabeledStatement) -> Result<()> {
        // If the body is a loop, attach the label to the loop context.
        match labeled.body.as_ref() {
            Statement::While(w) => {
                let loop_start = self.chunk.current_offset();
                self.push_loop_context(loop_start, Some(labeled.label.clone()));
                self.compile_expression(&w.test)?;
                let exit_jump = self.emit_jump(Instruction::JumpIfFalse);
                self.compile_statement(&w.body)?;
                let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
                self.emit(Instruction::Jump(back_offset));
                self.patch_jump_to_here(exit_jump);
                self.pop_loop_context_and_patch_breaks();
            }
            Statement::For(f) => {
                self.begin_scope();
                if let Some(ref init) = f.init {
                    match init {
                        ForInit::VariableDeclaration(vd) => {
                            self.compile_variable_declaration(vd)?
                        }
                        ForInit::Expression(expr) => {
                            self.compile_expression(expr)?;
                            self.emit(Instruction::Pop);
                        }
                    }
                }
                let loop_start = self.chunk.current_offset();
                self.push_loop_context(loop_start, Some(labeled.label.clone()));
                let exit_jump = if let Some(ref test) = f.test {
                    self.compile_expression(test)?;
                    Some(self.emit_jump(Instruction::JumpIfFalse))
                } else {
                    None
                };
                self.compile_statement(&f.body)?;
                let continue_target = self.chunk.current_offset();
                if let Some(ctx) = self.loop_stack.last_mut() {
                    ctx.continue_target = continue_target;
                }
                if let Some(ref update) = f.update {
                    self.compile_expression(update)?;
                    self.emit(Instruction::Pop);
                }
                let back_offset = loop_start as i32 - self.chunk.current_offset() as i32 - 1;
                self.emit(Instruction::Jump(back_offset));
                if let Some(exit) = exit_jump {
                    self.patch_jump_to_here(exit);
                }
                self.pop_loop_context_and_patch_breaks();
                self.end_scope();
            }
            _ => {
                // Labels on non-loop statements: just compile the body.
                // break with this label is handled like a named break.
                let here = self.chunk.current_offset();
                self.push_loop_context(here, Some(labeled.label.clone()));
                self.compile_statement(&labeled.body)?;
                self.pop_loop_context_and_patch_breaks();
            }
        }
        Ok(())
    }

    fn compile_class_declaration(&mut self, cls: &ClassDeclaration) -> Result<()> {
        // Simplified class compilation: compile as an object with a constructor.
        if let Some(ref name) = cls.id {
            let slot = self.declare_local(name)?;
            self.compile_class_body(cls)?;
            self.emit(Instruction::StoreLocal(slot));
            let len = self.chunk.instructions.len();
            self.chunk.instructions.truncate(len - 1);
        } else {
            self.compile_class_body(cls)?;
            self.emit(Instruction::Pop);
        }
        Ok(())
    }

    pub(crate) fn compile_class_body(&mut self, cls: &ClassDeclaration) -> Result<()> {
        // Push constructor function or empty object as placeholder.
        let mut found_constructor = false;
        for member in &cls.body {
            if member.kind == ClassMemberKind::Constructor {
                if let Some(ref value) = member.value {
                    self.compile_expression(value)?;
                    found_constructor = true;
                    break;
                }
            }
        }
        if !found_constructor {
            // Create an empty function as constructor placeholder.
            self.emit(Instruction::CreateObject);
        }

        // Add methods as properties.
        for member in &cls.body {
            if member.kind == ClassMemberKind::Constructor {
                continue;
            }
            if let Some(ref value) = member.value {
                self.emit(Instruction::Dup); // dup the class object
                self.compile_expression(value)?;
                if member.computed {
                    self.compile_expression(&member.key)?;
                    self.emit(Instruction::SetComputed);
                } else {
                    let key_name = expression_to_property_name(&member.key)?;
                    let idx = self.add_string_constant(&key_name)?;
                    self.emit(Instruction::SetProperty(idx));
                }
                self.emit(Instruction::Pop);
            }
        }
        Ok(())
    }

    // ------------------------------------------------------------------
    //  ESM: import / export
    // ------------------------------------------------------------------

    fn compile_import(&mut self, import_decl: &ImportDeclaration) -> Result<()> {
        // Emit ImportModule to load the module namespace object
        let source_idx = self.add_string_constant(&import_decl.source)?;
        self.emit(Instruction::ImportModule(source_idx));

        // For each specifier, extract the binding from the namespace
        for spec in &import_decl.specifiers {
            match spec {
                ImportSpecifier::Default { local, .. } => {
                    // import x from "mod" -> get "default" from namespace
                    self.emit(Instruction::Dup); // keep namespace on stack
                    let binding_idx = self.add_string_constant("default")?;
                    self.emit(Instruction::ImportBinding(binding_idx));
                    let slot = self.declare_local(local)?;
                    self.emit(Instruction::StoreLocal(slot));
                }
                ImportSpecifier::Named {
                    imported, local, ..
                } => {
                    self.emit(Instruction::Dup);
                    let binding_idx = self.add_string_constant(imported)?;
                    self.emit(Instruction::ImportBinding(binding_idx));
                    let slot = self.declare_local(local)?;
                    self.emit(Instruction::StoreLocal(slot));
                }
                ImportSpecifier::Namespace { local, .. } => {
                    // import * as ns -> the namespace IS the value
                    self.emit(Instruction::Dup);
                    let slot = self.declare_local(local)?;
                    self.emit(Instruction::StoreLocal(slot));
                }
            }
        }

        // Pop the namespace object
        self.emit(Instruction::Pop);
        Ok(())
    }

    fn compile_export(&mut self, export_decl: &ExportDeclaration) -> Result<()> {
        match &export_decl.kind {
            ExportKind::Default(expr) => {
                self.compile_expression(expr)?;
                self.emit(Instruction::ExportDefault);
            }
            ExportKind::Named(specifiers) => {
                for spec in specifiers {
                    // Load the local variable and export it
                    self.compile_expression(&Expression::Identifier(IdentifierExpression {
                        name: spec.local.clone(),
                        location: spec.location,
                    }))?;
                    let name_idx = self.add_string_constant(&spec.exported)?;
                    self.emit(Instruction::ExportBinding(name_idx));
                }
            }
            ExportKind::Declaration(stmt) => {
                // Compile the declaration, then export the declared bindings.
                // Function declarations are hoisted by compile_program, so
                // we only need to load the local and export it.
                match stmt.as_ref() {
                    Statement::FunctionDeclaration(func) => {
                        // Already hoisted — just load local and export
                        if let Some(ref name) = func.id {
                            self.compile_expression(&Expression::Identifier(
                                IdentifierExpression {
                                    name: name.clone(),
                                    location: func.location,
                                },
                            ))?;
                            let name_idx = self.add_string_constant(name)?;
                            self.emit(Instruction::ExportBinding(name_idx));
                        }
                    }
                    Statement::VariableDeclaration(vd) => {
                        self.compile_statement(stmt)?;
                        for decl in &vd.declarations {
                            if let Pattern::Identifier(id) = &decl.id {
                                self.compile_expression(&Expression::Identifier(
                                    IdentifierExpression {
                                        name: id.name.clone(),
                                        location: id.location,
                                    },
                                ))?;
                                let name_idx = self.add_string_constant(&id.name)?;
                                self.emit(Instruction::ExportBinding(name_idx));
                            }
                        }
                    }
                    Statement::ClassDeclaration(cls) => {
                        self.compile_statement(stmt)?;
                        if let Some(ref name) = cls.id {
                            self.compile_expression(&Expression::Identifier(
                                IdentifierExpression {
                                    name: name.clone(),
                                    location: cls.location,
                                },
                            ))?;
                            let name_idx = self.add_string_constant(name)?;
                            self.emit(Instruction::ExportBinding(name_idx));
                        }
                    }
                    _ => {}
                }
            }
            ExportKind::AllFrom(source) => {
                let source_idx = self.add_string_constant(source)?;
                self.emit(Instruction::ImportModule(source_idx));
                // Re-export all: for now, just push module as-is
                // The VM will handle copying all exports
                self.emit(Instruction::Pop);
            }
            ExportKind::NamedFrom(specifiers, source) => {
                let source_idx = self.add_string_constant(source)?;
                self.emit(Instruction::ImportModule(source_idx));
                for spec in specifiers {
                    self.emit(Instruction::Dup);
                    let binding_idx = self.add_string_constant(&spec.local)?;
                    self.emit(Instruction::ImportBinding(binding_idx));
                    let name_idx = self.add_string_constant(&spec.exported)?;
                    self.emit(Instruction::ExportBinding(name_idx));
                }
                self.emit(Instruction::Pop);
            }
        }
        Ok(())
    }
}
