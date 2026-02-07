use rawjs_ast::*;
use rawjs_common::{RawJsError, Result};

use crate::chunk::Constant;
use crate::opcode::Instruction;

use super::{expression_to_property_name, pattern_to_name, Compiler, ParentLocal, ParentUpvalue};

impl Compiler {
    // ------------------------------------------------------------------
    //  Expressions
    // ------------------------------------------------------------------

    pub(crate) fn compile_expression(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::NumberLiteral(n) => {
                let idx = self.add_number_constant(n.value)?;
                self.emit(Instruction::LoadConst(idx));
                Ok(())
            }
            Expression::StringLiteral(s) => {
                let idx = self.add_string_constant(&s.value)?;
                self.emit(Instruction::LoadConst(idx));
                Ok(())
            }
            Expression::BooleanLiteral(b) => {
                self.emit(if b.value {
                    Instruction::True
                } else {
                    Instruction::False
                });
                Ok(())
            }
            Expression::NullLiteral(_) => {
                self.emit(Instruction::Null);
                Ok(())
            }
            Expression::TemplateLiteral(tl) => {
                if tl.expressions.is_empty() {
                    // No interpolation: treat as a plain string constant.
                    let idx = self.add_string_constant(&tl.quasis[0])?;
                    self.emit(Instruction::LoadConst(idx));
                } else {
                    // Build the concatenated string from quasis and expressions.
                    // Pattern: quasis[0] + expr[0] + quasis[1] + expr[1] + ... + quasis[n]
                    let first_idx = self.add_string_constant(&tl.quasis[0])?;
                    self.emit(Instruction::LoadConst(first_idx));

                    for (i, expr) in tl.expressions.iter().enumerate() {
                        self.compile_expression(expr)?;
                        self.emit(Instruction::Add);
                        let quasi_idx = self.add_string_constant(&tl.quasis[i + 1])?;
                        self.emit(Instruction::LoadConst(quasi_idx));
                        self.emit(Instruction::Add);
                    }
                }
                Ok(())
            }
            Expression::RegExpLiteral(re) => {
                // Store the pattern as a string constant.
                let pattern_str = format!("/{}/{}", re.pattern, re.flags);
                let idx = self.add_string_constant(&pattern_str)?;
                self.emit(Instruction::LoadConst(idx));
                Ok(())
            }
            Expression::Identifier(id) => self.compile_identifier_load(&id.name),
            Expression::This(_) => {
                self.emit(Instruction::This);
                Ok(())
            }
            Expression::Super(_) => {
                // 'super' is context-dependent; for now push as a global lookup.
                let idx = self.add_string_constant("super")?;
                self.emit(Instruction::LoadGlobal(idx));
                Ok(())
            }
            Expression::ArrayExpression(arr) => self.compile_array(arr),
            Expression::ObjectExpression(obj) => self.compile_object(obj),
            Expression::FunctionExpression(func) => self.compile_function_expression(func),
            Expression::ArrowFunctionExpression(arrow) => self.compile_arrow_function(arrow),
            Expression::ClassExpression(cls) => self.compile_class_body(cls),
            Expression::Unary(unary) => self.compile_unary(unary),
            Expression::Update(update) => self.compile_update(update),
            Expression::Binary(bin) => self.compile_binary(bin),
            Expression::Logical(log) => self.compile_logical(log),
            Expression::Assignment(assign) => self.compile_assignment(assign),
            Expression::Conditional(cond) => self.compile_conditional(cond),
            Expression::Call(call) => self.compile_call(call),
            Expression::New(new_expr) => self.compile_new(new_expr),
            Expression::Member(member) => self.compile_member_load(member),
            Expression::Sequence(seq) => self.compile_sequence(seq),
            Expression::Spread(spread) => {
                // Spread in array literals, etc. -- just compile the argument.
                self.compile_expression(&spread.argument)
            }
            Expression::Yield(y) => self.compile_yield(y),
            Expression::Await(a) => self.compile_await(a),
        }
    }

    pub(crate) fn compile_identifier_load(&mut self, name: &str) -> Result<()> {
        if let Some(slot) = self.resolve_local(name) {
            self.emit(Instruction::LoadLocal(slot));
        } else if let Some(uv) = self.resolve_upvalue(name) {
            self.emit(Instruction::LoadUpvalue(uv));
        } else {
            let idx = self.add_string_constant(name)?;
            self.emit(Instruction::LoadGlobal(idx));
        }
        Ok(())
    }

    pub(crate) fn compile_identifier_store(&mut self, name: &str) -> Result<()> {
        if let Some(slot) = self.resolve_local(name) {
            self.emit(Instruction::StoreLocal(slot));
        } else if let Some(uv) = self.resolve_upvalue(name) {
            self.emit(Instruction::StoreUpvalue(uv));
        } else {
            let idx = self.add_string_constant(name)?;
            self.emit(Instruction::StoreGlobal(idx));
        }
        Ok(())
    }

    fn compile_array(&mut self, arr: &ArrayExpression) -> Result<()> {
        let count = arr.elements.len();
        for elem in &arr.elements {
            match elem {
                Some(e) => self.compile_expression(e)?,
                None => {
                    self.emit(Instruction::Undefined);
                }
            }
        }
        if count > u16::MAX as usize {
            return Err(RawJsError::internal_error("Array literal too large"));
        }
        self.emit(Instruction::CreateArray(count as u16));
        Ok(())
    }

    fn compile_object(&mut self, obj: &ObjectExpression) -> Result<()> {
        self.emit(Instruction::CreateObject);
        for prop in &obj.properties {
            self.emit(Instruction::Dup); // keep object reference on stack

            if prop.computed {
                // Stack after Dup: [..., object, object]
                // SetComputed expects: [object, key, value] -> [value]
                self.compile_expression(&prop.key)?;
                self.compile_expression(&prop.value)?;
                self.emit(Instruction::SetComputed);
                self.emit(Instruction::Pop); // discard the value left by SetComputed
            } else {
                // Stack after Dup: [..., object, object]
                // SetProperty expects: [object, value] -> [value]
                self.compile_expression(&prop.value)?;
                let key_name = expression_to_property_name(&prop.key)?;
                let idx = self.add_string_constant(&key_name)?;
                self.emit(Instruction::SetProperty(idx));
                self.emit(Instruction::Pop); // discard the value left by SetProperty
            }
        }
        Ok(())
    }

    fn compile_function_expression(&mut self, func: &FunctionDeclaration) -> Result<()> {
        self.compile_function_body(func)
    }

    fn compile_arrow_function(&mut self, arrow: &ArrowFunctionExpression) -> Result<()> {
        let name = "<arrow>";
        let mut child = Compiler::new(name);
        child.chunk.param_count = arrow.params.len() as u16;

        // Propagate async flag for arrow functions.
        child.is_async = arrow.is_async;
        child.chunk.is_async = arrow.is_async;

        // Pass parent scope info for upvalue resolution.
        child.parent_locals = self
            .locals
            .iter()
            .enumerate()
            .map(|(i, l)| ParentLocal {
                name: l.name.clone(),
                index: i as u16,
            })
            .collect();
        child.parent_upvalues = self
            .upvalue_names
            .iter()
            .enumerate()
            .map(|(i, name)| ParentUpvalue {
                name: name.clone(),
                index: i as u16,
            })
            .collect();

        // Declare parameters as locals.
        for param in &arrow.params {
            let param_name = pattern_to_name(param);
            child.declare_local(&param_name)?;
        }

        match &arrow.body {
            ArrowFunctionBody::Expression(expr) => {
                child.compile_expression(expr)?;
                child.emit(Instruction::Return);
            }
            ArrowFunctionBody::Block(block) => {
                for stmt in &block.body {
                    child.compile_statement(stmt)?;
                }
                // Implicit undefined return.
                child.emit(Instruction::Undefined);
                child.emit(Instruction::Return);
            }
        }

        child.chunk.local_count = child.max_locals as u16;
        child.chunk.upvalue_count = child.upvalues.len() as u16;
        child.chunk.upvalue_descriptors = child.upvalues.clone();

        let func_const_idx = self
            .chunk
            .add_constant(Constant::Function(Box::new(child.chunk)))
            .ok_or_else(|| RawJsError::internal_error("Constant pool overflow"))?;

        self.emit(Instruction::CreateClosure(func_const_idx));
        Ok(())
    }

    pub(crate) fn compile_function_body(&mut self, func: &FunctionDeclaration) -> Result<()> {
        let name = func.id.as_deref().unwrap_or("<anonymous>");
        let mut child = Compiler::new(name);
        child.chunk.param_count = func.params.len() as u16;

        // Propagate generator/async flags.
        child.is_generator = func.is_generator;
        child.is_async = func.is_async;
        child.chunk.is_generator = func.is_generator;
        child.chunk.is_async = func.is_async;

        // Pass parent scope info for upvalue resolution.
        child.parent_locals = self
            .locals
            .iter()
            .enumerate()
            .map(|(i, l)| ParentLocal {
                name: l.name.clone(),
                index: i as u16,
            })
            .collect();
        child.parent_upvalues = self
            .upvalue_names
            .iter()
            .enumerate()
            .map(|(i, name)| ParentUpvalue {
                name: name.clone(),
                index: i as u16,
            })
            .collect();

        // Declare parameters as locals.
        for param in &func.params {
            let param_name = pattern_to_name(param);
            child.declare_local(&param_name)?;
        }

        // Hoist function declarations inside the body.
        for stmt in &func.body.body {
            if let Statement::FunctionDeclaration(inner_func) = stmt {
                child.hoist_function_declaration(inner_func)?;
            }
        }

        for stmt in &func.body.body {
            if matches!(stmt, Statement::FunctionDeclaration(_)) {
                continue;
            }
            child.compile_statement(stmt)?;
        }

        // Implicit undefined return.
        child.emit(Instruction::Undefined);
        child.emit(Instruction::Return);

        child.chunk.local_count = child.max_locals as u16;
        child.chunk.upvalue_count = child.upvalues.len() as u16;
        child.chunk.upvalue_descriptors = child.upvalues.clone();

        let func_const_idx = self
            .chunk
            .add_constant(Constant::Function(Box::new(child.chunk)))
            .ok_or_else(|| RawJsError::internal_error("Constant pool overflow"))?;

        self.emit(Instruction::CreateClosure(func_const_idx));
        Ok(())
    }

    fn compile_yield(&mut self, yield_expr: &YieldExpression) -> Result<()> {
        if !self.is_generator {
            return Err(RawJsError::syntax_error(
                "yield is only valid in generator functions",
                None,
            ));
        }
        if let Some(ref arg) = yield_expr.argument {
            self.compile_expression(arg)?;
        } else {
            self.emit(Instruction::Undefined);
        }
        self.emit(Instruction::Yield);
        Ok(())
    }

    fn compile_await(&mut self, await_expr: &AwaitExpression) -> Result<()> {
        if !self.is_async {
            return Err(RawJsError::syntax_error(
                "await is only valid in async functions",
                None,
            ));
        }
        self.compile_expression(&await_expr.argument)?;
        self.emit(Instruction::Await);
        Ok(())
    }

    fn compile_unary(&mut self, unary: &UnaryExpression) -> Result<()> {
        // Special handling for `delete` on member expressions.
        if unary.operator == UnaryOp::Delete {
            match unary.argument.as_ref() {
                Expression::Member(member) => {
                    self.compile_expression(&member.object)?;
                    if member.computed {
                        self.compile_expression(&member.property)?;
                    } else {
                        let name = expression_to_property_name(&member.property)?;
                        let idx = self.add_string_constant(&name)?;
                        self.emit(Instruction::LoadConst(idx));
                    }
                    self.emit(Instruction::Delete);
                    return Ok(());
                }
                _ => {
                    // `delete` on non-member expressions always returns true.
                    self.compile_expression(&unary.argument)?;
                    self.emit(Instruction::Pop);
                    self.emit(Instruction::True);
                    return Ok(());
                }
            }
        }

        // Special handling for `typeof` on identifiers (should not throw ReferenceError).
        if unary.operator == UnaryOp::Typeof {
            if let Expression::Identifier(id) = unary.argument.as_ref() {
                // Use LoadGlobal which will push undefined for undeclared vars
                // at runtime. The VM should handle this gracefully.
                self.compile_identifier_load(&id.name)?;
                self.emit(Instruction::TypeOf);
                return Ok(());
            }
        }

        self.compile_expression(&unary.argument)?;
        let instr = match unary.operator {
            UnaryOp::Minus => Instruction::Neg,
            UnaryOp::Plus => Instruction::Pos,
            UnaryOp::Not => Instruction::Not,
            UnaryOp::BitNot => Instruction::BitNot,
            UnaryOp::Typeof => Instruction::TypeOf,
            UnaryOp::Void => Instruction::Void,
            UnaryOp::Delete => unreachable!(), // handled above
        };
        self.emit(instr);
        Ok(())
    }

    fn compile_update(&mut self, update: &UpdateExpression) -> Result<()> {
        // ++x / --x (prefix) or x++ / x-- (postfix)
        match update.argument.as_ref() {
            Expression::Identifier(id) => {
                if update.prefix {
                    // Load, increment/decrement, store, result is new value.
                    self.compile_identifier_load(&id.name)?;
                    let one = self.add_number_constant(1.0)?;
                    self.emit(Instruction::LoadConst(one));
                    self.emit(match update.operator {
                        UpdateOp::Increment => Instruction::Add,
                        UpdateOp::Decrement => Instruction::Sub,
                    });
                    self.emit(Instruction::Dup);
                    // StoreLocal/StoreGlobal pops the duplicate.
                    // The new value remains on the stack as the expression result.
                    self.compile_identifier_store(&id.name)?;
                } else {
                    // Postfix: result is old value.
                    self.compile_identifier_load(&id.name)?;
                    self.emit(Instruction::Dup); // save old value
                    let one = self.add_number_constant(1.0)?;
                    self.emit(Instruction::LoadConst(one));
                    self.emit(match update.operator {
                        UpdateOp::Increment => Instruction::Add,
                        UpdateOp::Decrement => Instruction::Sub,
                    });
                    // StoreLocal/StoreGlobal pops the new value from stack.
                    // The old value remains on the stack as the expression result.
                    self.compile_identifier_store(&id.name)?;
                }
            }
            Expression::Member(member) => {
                // obj.prop++ or obj[expr]++
                self.compile_expression(&member.object)?;
                if member.computed {
                    self.compile_expression(&member.property)?;
                    self.emit(Instruction::Dup); // keep key
                                                 // Stack: [obj, key, key]
                                                 // We need: GetComputed(obj, key), then Inc, then SetComputed(obj, key, new_val)
                                                 // This is complex without more stack ops. Simplified approach:
                    self.emit(Instruction::GetComputed);
                    if update.prefix {
                        let one = self.add_number_constant(1.0)?;
                        self.emit(Instruction::LoadConst(one));
                        self.emit(match update.operator {
                            UpdateOp::Increment => Instruction::Add,
                            UpdateOp::Decrement => Instruction::Sub,
                        });
                    } else {
                        // Need postfix: keep old value, compute new, store
                        self.emit(match update.operator {
                            UpdateOp::Increment => Instruction::PostfixIncrement,
                            UpdateOp::Decrement => Instruction::PostfixDecrement,
                        });
                    }
                } else {
                    let name = expression_to_property_name(&member.property)?;
                    let idx = self.add_string_constant(&name)?;
                    self.emit(Instruction::Dup); // keep obj
                    self.emit(Instruction::GetProperty(idx));
                    if update.prefix {
                        let one = self.add_number_constant(1.0)?;
                        self.emit(Instruction::LoadConst(one));
                        self.emit(match update.operator {
                            UpdateOp::Increment => Instruction::Add,
                            UpdateOp::Decrement => Instruction::Sub,
                        });
                        self.emit(Instruction::SetProperty(idx));
                    } else {
                        self.emit(match update.operator {
                            UpdateOp::Increment => Instruction::PostfixIncrement,
                            UpdateOp::Decrement => Instruction::PostfixDecrement,
                        });
                        self.emit(Instruction::SetProperty(idx));
                    }
                }
            }
            _ => {
                return Err(RawJsError::syntax_error(
                    "Invalid left-hand side in update expression",
                    None,
                ));
            }
        }
        Ok(())
    }

    fn compile_binary(&mut self, bin: &BinaryExpression) -> Result<()> {
        self.compile_expression(&bin.left)?;
        self.compile_expression(&bin.right)?;
        let instr = match bin.operator {
            BinaryOp::Add => Instruction::Add,
            BinaryOp::Sub => Instruction::Sub,
            BinaryOp::Mul => Instruction::Mul,
            BinaryOp::Div => Instruction::Div,
            BinaryOp::Mod => Instruction::Mod,
            BinaryOp::Exp => Instruction::Exp,
            BinaryOp::BitAnd => Instruction::BitAnd,
            BinaryOp::BitOr => Instruction::BitOr,
            BinaryOp::BitXor => Instruction::BitXor,
            BinaryOp::Shl => Instruction::Shl,
            BinaryOp::Shr => Instruction::Shr,
            BinaryOp::UShr => Instruction::UShr,
            BinaryOp::Eq => Instruction::Eq,
            BinaryOp::StrictEq => Instruction::StrictEq,
            BinaryOp::Ne => Instruction::Ne,
            BinaryOp::StrictNe => Instruction::StrictNe,
            BinaryOp::Lt => Instruction::Lt,
            BinaryOp::Le => Instruction::Le,
            BinaryOp::Gt => Instruction::Gt,
            BinaryOp::Ge => Instruction::Ge,
            BinaryOp::In => Instruction::In,
            BinaryOp::Instanceof => Instruction::Instanceof,
        };
        self.emit(instr);
        Ok(())
    }

    fn compile_logical(&mut self, log: &LogicalExpression) -> Result<()> {
        self.compile_expression(&log.left)?;

        match log.operator {
            LogicalOp::And => {
                // Short-circuit: if left is falsy, skip right.
                self.emit(Instruction::Dup);
                let jump_idx = self.emit_jump(Instruction::JumpIfFalse);
                self.emit(Instruction::Pop); // discard left (truthy case)
                self.compile_expression(&log.right)?;
                self.patch_jump_to_here(jump_idx);
            }
            LogicalOp::Or => {
                // Short-circuit: if left is truthy, skip right.
                self.emit(Instruction::Dup);
                let jump_idx = self.emit_jump(Instruction::JumpIfTrue);
                self.emit(Instruction::Pop); // discard left (falsy case)
                self.compile_expression(&log.right)?;
                self.patch_jump_to_here(jump_idx);
            }
            LogicalOp::NullishCoalescing => {
                // left ?? right: if left is null/undefined, use right.
                self.emit(Instruction::Dup);
                self.emit(Instruction::Null);
                self.emit(Instruction::StrictEq);
                let null_check = self.emit_jump(Instruction::JumpIfTrue);

                self.emit(Instruction::Dup);
                self.emit(Instruction::Undefined);
                self.emit(Instruction::StrictEq);
                let undef_check = self.emit_jump(Instruction::JumpIfTrue);

                // Not nullish -- jump to end (keep left).
                let end_jump = self.emit_jump(Instruction::Jump);

                // Nullish -- pop left, compute right.
                self.patch_jump_to_here(null_check);
                self.patch_jump_to_here(undef_check);
                self.emit(Instruction::Pop);
                self.compile_expression(&log.right)?;

                self.patch_jump_to_here(end_jump);
            }
        }
        Ok(())
    }

    fn compile_assignment(&mut self, assign: &AssignmentExpression) -> Result<()> {
        match assign.left.as_ref() {
            Expression::Identifier(id) => {
                if assign.operator == AssignmentOp::Assign {
                    self.compile_expression(&assign.right)?;
                } else {
                    self.compile_identifier_load(&id.name)?;
                    self.compile_expression(&assign.right)?;
                    self.emit_compound_assign_op(assign.operator)?;
                }
                self.emit(Instruction::Dup); // result stays on stack
                self.compile_identifier_store(&id.name)?;
            }
            Expression::Member(member) => {
                self.compile_expression(&member.object)?;

                if member.computed {
                    self.compile_expression(&member.property)?;

                    if assign.operator == AssignmentOp::Assign {
                        self.compile_expression(&assign.right)?;
                        self.emit(Instruction::SetComputed);
                    } else {
                        // obj, key on stack. We need to get old, apply op, set.
                        self.emit(Instruction::Dup); // obj, key, key
                        self.emit(Instruction::GetComputed); // obj, key, old_val
                        self.compile_expression(&assign.right)?;
                        self.emit_compound_assign_op(assign.operator)?;
                        self.emit(Instruction::SetComputed);
                    }
                } else {
                    let name = expression_to_property_name(&member.property)?;
                    let idx = self.add_string_constant(&name)?;

                    if assign.operator == AssignmentOp::Assign {
                        self.compile_expression(&assign.right)?;
                        self.emit(Instruction::SetProperty(idx));
                    } else {
                        self.emit(Instruction::Dup);
                        self.emit(Instruction::GetProperty(idx));
                        self.compile_expression(&assign.right)?;
                        self.emit_compound_assign_op(assign.operator)?;
                        self.emit(Instruction::SetProperty(idx));
                    }
                }
            }
            _ => {
                return Err(RawJsError::syntax_error(
                    "Invalid left-hand side in assignment",
                    None,
                ));
            }
        }
        Ok(())
    }

    fn emit_compound_assign_op(&mut self, op: AssignmentOp) -> Result<()> {
        let instr = match op {
            AssignmentOp::AddAssign => Instruction::Add,
            AssignmentOp::SubAssign => Instruction::Sub,
            AssignmentOp::MulAssign => Instruction::Mul,
            AssignmentOp::DivAssign => Instruction::Div,
            AssignmentOp::ModAssign => Instruction::Mod,
            AssignmentOp::ExpAssign => Instruction::Exp,
            AssignmentOp::BitAndAssign => Instruction::BitAnd,
            AssignmentOp::BitOrAssign => Instruction::BitOr,
            AssignmentOp::BitXorAssign => Instruction::BitXor,
            AssignmentOp::ShlAssign => Instruction::Shl,
            AssignmentOp::ShrAssign => Instruction::Shr,
            AssignmentOp::UShrAssign => Instruction::UShr,
            AssignmentOp::AndAssign
            | AssignmentOp::OrAssign
            | AssignmentOp::NullishCoalescingAssign => {
                // These are logical assignment operators and should use
                // short-circuit semantics.  For simplicity, we treat them as
                // regular operators here.  A fully correct implementation
                // would need short-circuit handling similar to compile_logical.
                return Err(RawJsError::internal_error(
                    "Logical assignment operators require special handling",
                ));
            }
            AssignmentOp::Assign => unreachable!(),
        };
        self.emit(instr);
        Ok(())
    }

    fn compile_conditional(&mut self, cond: &ConditionalExpression) -> Result<()> {
        self.compile_expression(&cond.test)?;
        let else_jump = self.emit_jump(Instruction::JumpIfFalse);
        self.compile_expression(&cond.consequent)?;
        let end_jump = self.emit_jump(Instruction::Jump);
        self.patch_jump_to_here(else_jump);
        self.compile_expression(&cond.alternate)?;
        self.patch_jump_to_here(end_jump);
        Ok(())
    }

    fn compile_call(&mut self, call: &CallExpression) -> Result<()> {
        // Detect method calls: obj.method(...) or obj[method](...)
        let is_method_call = matches!(&*call.callee, Expression::Member(_));

        if is_method_call {
            // For method calls, push the receiver first, then the method value.
            if let Expression::Member(member) = &*call.callee {
                // Push receiver (the object).
                self.compile_expression(&member.object)?;
                // Duplicate the receiver so it stays on the stack below the method.
                self.emit(Instruction::Dup);
                // Load the method from the receiver.
                if member.computed {
                    self.compile_expression(&member.property)?;
                    self.emit(Instruction::GetComputed);
                } else {
                    let name = expression_to_property_name(&member.property)?;
                    let idx = self.add_string_constant(&name)?;
                    self.emit(Instruction::GetProperty(idx));
                }
            }
        } else {
            // Regular call: push the callee.
            self.compile_expression(&call.callee)?;
        }

        // Push arguments.
        let argc = call.arguments.len();
        if argc > u16::MAX as usize {
            return Err(RawJsError::internal_error("Too many function arguments"));
        }
        for arg in &call.arguments {
            self.compile_expression(arg)?;
        }

        if is_method_call {
            self.emit(Instruction::CallMethod(argc as u16));
        } else {
            self.emit(Instruction::Call(argc as u16));
        }
        Ok(())
    }

    fn compile_new(&mut self, new_expr: &NewExpression) -> Result<()> {
        // new Constructor(args)
        // For a complete implementation, `new` would use a different opcode.
        // Here we simulate it by calling the constructor.
        self.compile_expression(&new_expr.callee)?;

        let argc = new_expr.arguments.len();
        if argc > u16::MAX as usize {
            return Err(RawJsError::internal_error("Too many constructor arguments"));
        }
        for arg in &new_expr.arguments {
            self.compile_expression(arg)?;
        }

        // Use Call for now; a proper VM would distinguish `new` calls.
        self.emit(Instruction::Call(argc as u16));
        Ok(())
    }

    fn compile_member_load(&mut self, member: &MemberExpression) -> Result<()> {
        self.compile_expression(&member.object)?;
        if member.computed {
            self.compile_expression(&member.property)?;
            self.emit(Instruction::GetComputed);
        } else {
            let name = expression_to_property_name(&member.property)?;
            let idx = self.add_string_constant(&name)?;
            self.emit(Instruction::GetProperty(idx));
        }
        Ok(())
    }

    fn compile_sequence(&mut self, seq: &SequenceExpression) -> Result<()> {
        let len = seq.expressions.len();
        for (i, expr) in seq.expressions.iter().enumerate() {
            self.compile_expression(expr)?;
            if i < len - 1 {
                self.emit(Instruction::Pop);
            }
        }
        Ok(())
    }
}
