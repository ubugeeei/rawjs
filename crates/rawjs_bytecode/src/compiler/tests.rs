use super::*;
use rawjs_common::SourceLocation;

fn loc() -> SourceLocation {
    SourceLocation::new(1, 0, 0)
}

/// Helper: build a minimal Program from a list of statements.
fn program(body: Vec<Statement>) -> Program {
    Program {
        body,
        location: loc(),
    }
}

/// Helper: build an expression statement.
fn expr_stmt(expression: Expression) -> Statement {
    Statement::Expression(ExpressionStatement {
        expression,
        location: loc(),
    })
}

fn num_lit(value: f64) -> Expression {
    Expression::NumberLiteral(NumberLiteral {
        value,
        location: loc(),
    })
}

fn str_lit(value: &str) -> Expression {
    Expression::StringLiteral(StringLiteral {
        value: value.to_string(),
        location: loc(),
    })
}

fn ident_expr(name: &str) -> Expression {
    Expression::Identifier(IdentifierExpression {
        name: name.to_string(),
        location: loc(),
    })
}

fn ident_pat(name: &str) -> Pattern {
    Pattern::Identifier(IdentifierPattern {
        name: name.to_string(),
        location: loc(),
    })
}

fn binary(op: BinaryOp, left: Expression, right: Expression) -> Expression {
    Expression::Binary(BinaryExpression {
        operator: op,
        left: Box::new(left),
        right: Box::new(right),
        location: loc(),
    })
}

// ---- Tests ----------------------------------------------------------

#[test]
fn compile_empty_program() {
    let prog = program(vec![]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Should have Undefined + Return.
    assert_eq!(chunk.instructions.len(), 2);
    assert_eq!(chunk.instructions[0], Instruction::Undefined);
    assert_eq!(chunk.instructions[1], Instruction::Return);
}

#[test]
fn compile_number_literal() {
    let prog = program(vec![expr_stmt(num_lit(42.0))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // LoadConst(0), Pop, Undefined, Return
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::Pop);
    assert_eq!(chunk.constants.len(), 1);
    match &chunk.constants[0] {
        Constant::Number(n) => assert_eq!(*n, 42.0),
        _ => panic!("Expected number constant"),
    }
}

#[test]
fn compile_string_literal() {
    let prog = program(vec![expr_stmt(str_lit("hello"))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    match &chunk.constants[0] {
        Constant::String(s) => assert_eq!(s, "hello"),
        _ => panic!("Expected string constant"),
    }
}

#[test]
fn compile_boolean_literals() {
    let prog = program(vec![
        expr_stmt(Expression::BooleanLiteral(BooleanLiteral {
            value: true,
            location: loc(),
        })),
        expr_stmt(Expression::BooleanLiteral(BooleanLiteral {
            value: false,
            location: loc(),
        })),
    ]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::True);
    assert_eq!(chunk.instructions[1], Instruction::Pop);
    assert_eq!(chunk.instructions[2], Instruction::False);
    assert_eq!(chunk.instructions[3], Instruction::Pop);
}

#[test]
fn compile_null_literal() {
    let prog = program(vec![expr_stmt(Expression::NullLiteral(loc()))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::Null);
}

#[test]
fn compile_this() {
    let prog = program(vec![expr_stmt(Expression::This(loc()))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::This);
}

#[test]
fn compile_binary_add() {
    // 1 + 2
    let prog = program(vec![expr_stmt(binary(
        BinaryOp::Add,
        num_lit(1.0),
        num_lit(2.0),
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0)); // 1
    assert_eq!(chunk.instructions[1], Instruction::LoadConst(1)); // 2
    assert_eq!(chunk.instructions[2], Instruction::Add);
    assert_eq!(chunk.instructions[3], Instruction::Pop);
}

#[test]
fn compile_all_binary_ops() {
    let ops = vec![
        (BinaryOp::Sub, Instruction::Sub),
        (BinaryOp::Mul, Instruction::Mul),
        (BinaryOp::Div, Instruction::Div),
        (BinaryOp::Mod, Instruction::Mod),
        (BinaryOp::Exp, Instruction::Exp),
        (BinaryOp::BitAnd, Instruction::BitAnd),
        (BinaryOp::BitOr, Instruction::BitOr),
        (BinaryOp::BitXor, Instruction::BitXor),
        (BinaryOp::Shl, Instruction::Shl),
        (BinaryOp::Shr, Instruction::Shr),
        (BinaryOp::UShr, Instruction::UShr),
        (BinaryOp::Eq, Instruction::Eq),
        (BinaryOp::StrictEq, Instruction::StrictEq),
        (BinaryOp::Ne, Instruction::Ne),
        (BinaryOp::StrictNe, Instruction::StrictNe),
        (BinaryOp::Lt, Instruction::Lt),
        (BinaryOp::Le, Instruction::Le),
        (BinaryOp::Gt, Instruction::Gt),
        (BinaryOp::Ge, Instruction::Ge),
        (BinaryOp::In, Instruction::In),
        (BinaryOp::Instanceof, Instruction::Instanceof),
    ];
    for (ast_op, expected_instr) in ops {
        let prog = program(vec![expr_stmt(binary(ast_op, num_lit(1.0), num_lit(2.0)))]);
        let chunk = Compiler::compile_program(&prog).unwrap();
        assert_eq!(
            chunk.instructions[2], expected_instr,
            "Failed for {:?}",
            ast_op
        );
    }
}

#[test]
fn compile_unary_neg() {
    let prog = program(vec![expr_stmt(Expression::Unary(UnaryExpression {
        operator: UnaryOp::Minus,
        argument: Box::new(num_lit(5.0)),
        prefix: true,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::Neg);
}

#[test]
fn compile_unary_not() {
    let prog = program(vec![expr_stmt(Expression::Unary(UnaryExpression {
        operator: UnaryOp::Not,
        argument: Box::new(Expression::BooleanLiteral(BooleanLiteral {
            value: true,
            location: loc(),
        })),
        prefix: true,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::True);
    assert_eq!(chunk.instructions[1], Instruction::Not);
}

#[test]
fn compile_unary_typeof() {
    let prog = program(vec![expr_stmt(Expression::Unary(UnaryExpression {
        operator: UnaryOp::Typeof,
        argument: Box::new(ident_expr("x")),
        prefix: true,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // LoadGlobal("x"), TypeOf, Pop, Undefined, Return
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert_eq!(chunk.instructions[1], Instruction::TypeOf);
}

#[test]
fn compile_unary_void() {
    let prog = program(vec![expr_stmt(Expression::Unary(UnaryExpression {
        operator: UnaryOp::Void,
        argument: Box::new(num_lit(0.0)),
        prefix: true,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::Void);
}

#[test]
fn compile_variable_declaration() {
    // let x = 10;
    let prog = program(vec![Statement::VariableDeclaration(VariableDeclaration {
        kind: VarKind::Let,
        declarations: vec![VariableDeclarator {
            id: ident_pat("x"),
            init: Some(num_lit(10.0)),
            location: loc(),
        }],
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // The value 10 is pushed, becoming the local.
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.local_count, 1);
}

#[test]
fn compile_variable_no_init() {
    // let x;
    let prog = program(vec![Statement::VariableDeclaration(VariableDeclaration {
        kind: VarKind::Let,
        declarations: vec![VariableDeclarator {
            id: ident_pat("x"),
            init: None,
            location: loc(),
        }],
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::Undefined);
}

#[test]
fn compile_variable_read() {
    // let x = 5; x;
    let prog = program(vec![
        Statement::VariableDeclaration(VariableDeclaration {
            kind: VarKind::Let,
            declarations: vec![VariableDeclarator {
                id: ident_pat("x"),
                init: Some(num_lit(5.0)),
                location: loc(),
            }],
            location: loc(),
        }),
        expr_stmt(ident_expr("x")),
    ]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // LoadConst(0)=5, StoreLocal(0), LoadLocal(0), Pop, Undefined, Return
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::StoreLocal(0));
    assert_eq!(chunk.instructions[2], Instruction::LoadLocal(0));
    assert_eq!(chunk.instructions[3], Instruction::Pop);
}

#[test]
fn compile_global_read() {
    // console (undeclared identifier -> global)
    let prog = program(vec![expr_stmt(ident_expr("console"))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
}

#[test]
fn compile_if_statement() {
    // if (true) { 1; }
    let prog = program(vec![Statement::If(IfStatement {
        test: Expression::BooleanLiteral(BooleanLiteral {
            value: true,
            location: loc(),
        }),
        consequent: Box::new(expr_stmt(num_lit(1.0))),
        alternate: None,
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::True);
    assert!(matches!(chunk.instructions[1], Instruction::JumpIfFalse(_)));
    assert_eq!(chunk.instructions[2], Instruction::LoadConst(0)); // 1.0
    assert_eq!(chunk.instructions[3], Instruction::Pop);
}

#[test]
fn compile_if_else() {
    // if (true) { 1; } else { 2; }
    let prog = program(vec![Statement::If(IfStatement {
        test: Expression::BooleanLiteral(BooleanLiteral {
            value: true,
            location: loc(),
        }),
        consequent: Box::new(expr_stmt(num_lit(1.0))),
        alternate: Some(Box::new(expr_stmt(num_lit(2.0)))),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // True, JumpIfFalse(->else), LoadConst(1.0), Pop, Jump(->end), LoadConst(2.0), Pop, ...
    assert_eq!(chunk.instructions[0], Instruction::True);
    assert!(matches!(chunk.instructions[1], Instruction::JumpIfFalse(_)));
    assert!(matches!(chunk.instructions[4], Instruction::Jump(_)));
}

#[test]
fn compile_while_loop() {
    // while (true) { 1; }
    let prog = program(vec![Statement::While(WhileStatement {
        test: Expression::BooleanLiteral(BooleanLiteral {
            value: true,
            location: loc(),
        }),
        body: Box::new(expr_stmt(num_lit(1.0))),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // True, JumpIfFalse, LoadConst, Pop, Jump(back), ...Undefined, Return
    assert_eq!(chunk.instructions[0], Instruction::True);
    assert!(matches!(chunk.instructions[1], Instruction::JumpIfFalse(_)));
    // Last instruction before Undefined/Return is Jump(back).
    let last_loop_instr = chunk.instructions.len() - 3; // Jump, Undefined, Return
    assert!(matches!(
        chunk.instructions[last_loop_instr],
        Instruction::Jump(_)
    ));
}

#[test]
fn compile_do_while_loop() {
    // do { 1; } while (false);
    let prog = program(vec![Statement::DoWhile(DoWhileStatement {
        body: Box::new(expr_stmt(num_lit(1.0))),
        test: Expression::BooleanLiteral(BooleanLiteral {
            value: false,
            location: loc(),
        }),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // LoadConst(1), Pop, False, JumpIfTrue(back), Undefined, Return
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::Pop);
    assert_eq!(chunk.instructions[2], Instruction::False);
    assert!(matches!(chunk.instructions[3], Instruction::JumpIfTrue(_)));
}

#[test]
fn compile_for_loop() {
    // for (let i = 0; i < 10; i = i + 1) { i; }
    let prog = program(vec![Statement::For(ForStatement {
        init: Some(ForInit::VariableDeclaration(VariableDeclaration {
            kind: VarKind::Let,
            declarations: vec![VariableDeclarator {
                id: ident_pat("i"),
                init: Some(num_lit(0.0)),
                location: loc(),
            }],
            location: loc(),
        })),
        test: Some(binary(BinaryOp::Lt, ident_expr("i"), num_lit(10.0))),
        update: Some(Expression::Assignment(AssignmentExpression {
            operator: AssignmentOp::Assign,
            left: Box::new(ident_expr("i")),
            right: Box::new(binary(BinaryOp::Add, ident_expr("i"), num_lit(1.0))),
            location: loc(),
        })),
        body: Box::new(expr_stmt(ident_expr("i"))),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Should compile without error and contain loops.
    assert!(chunk.instructions.len() > 5);
    // Verify there's a backward jump.
    let has_backward_jump = chunk.instructions.iter().any(|i| match i {
        Instruction::Jump(offset) => *offset < 0,
        _ => false,
    });
    assert!(has_backward_jump, "For loop should have a backward jump");
}

#[test]
fn compile_return_statement() {
    // return 42;
    let prog = program(vec![Statement::Return(ReturnStatement {
        argument: Some(num_lit(42.0)),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::Return);
}

#[test]
fn compile_return_void() {
    // return;
    let prog = program(vec![Statement::Return(ReturnStatement {
        argument: None,
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::Undefined);
    assert_eq!(chunk.instructions[1], Instruction::Return);
}

#[test]
fn compile_throw_statement() {
    // throw "error";
    let prog = program(vec![Statement::Throw(ThrowStatement {
        argument: str_lit("error"),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::Throw);
}

#[test]
fn compile_try_catch() {
    // try { 1; } catch (e) { 2; }
    let prog = program(vec![Statement::Try(TryStatement {
        block: BlockStatement {
            body: vec![expr_stmt(num_lit(1.0))],
            location: loc(),
        },
        handler: Some(CatchClause {
            param: Some(ident_pat("e")),
            body: BlockStatement {
                body: vec![expr_stmt(num_lit(2.0))],
                location: loc(),
            },
            location: loc(),
        }),
        finalizer: None,
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::EnterTry(_, _)));
    // Should have LeaveTry somewhere.
    let has_leave_try = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::LeaveTry));
    assert!(has_leave_try);
}

#[test]
fn compile_try_finally() {
    // try { 1; } finally { 2; }
    let prog = program(vec![Statement::Try(TryStatement {
        block: BlockStatement {
            body: vec![expr_stmt(num_lit(1.0))],
            location: loc(),
        },
        handler: None,
        finalizer: Some(BlockStatement {
            body: vec![expr_stmt(num_lit(2.0))],
            location: loc(),
        }),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::EnterTry(_, _)));
}

#[test]
fn compile_function_declaration() {
    // function add(a, b) { return a + b; }
    let prog = program(vec![Statement::FunctionDeclaration(FunctionDeclaration {
        id: Some("add".to_string()),
        params: vec![ident_pat("a"), ident_pat("b")],
        body: Box::new(BlockStatement {
            body: vec![Statement::Return(ReturnStatement {
                argument: Some(binary(BinaryOp::Add, ident_expr("a"), ident_expr("b"))),
                location: loc(),
            })],
            location: loc(),
        }),
        is_async: false,
        is_generator: false,
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.local_count, 1); // "add" local
                                      // Should have a Function constant.
    let has_func = chunk
        .constants
        .iter()
        .any(|c| matches!(c, Constant::Function(_)));
    assert!(has_func, "Should have a function constant");
}

#[test]
fn compile_function_body_correct() {
    // function add(a, b) { return a + b; }
    let prog = program(vec![Statement::FunctionDeclaration(FunctionDeclaration {
        id: Some("add".to_string()),
        params: vec![ident_pat("a"), ident_pat("b")],
        body: Box::new(BlockStatement {
            body: vec![Statement::Return(ReturnStatement {
                argument: Some(binary(BinaryOp::Add, ident_expr("a"), ident_expr("b"))),
                location: loc(),
            })],
            location: loc(),
        }),
        is_async: false,
        is_generator: false,
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Get the function chunk.
    let func_chunk = chunk.constants.iter().find_map(|c| match c {
        Constant::Function(f) => Some(f),
        _ => None,
    });
    let func_chunk = func_chunk.unwrap();
    assert_eq!(func_chunk.name, "add");
    assert_eq!(func_chunk.param_count, 2);
    assert_eq!(func_chunk.local_count, 2); // a, b
                                           // Instructions: LoadLocal(0), LoadLocal(1), Add, Return, Undefined, Return
    assert_eq!(func_chunk.instructions[0], Instruction::LoadLocal(0));
    assert_eq!(func_chunk.instructions[1], Instruction::LoadLocal(1));
    assert_eq!(func_chunk.instructions[2], Instruction::Add);
    assert_eq!(func_chunk.instructions[3], Instruction::Return);
}

#[test]
fn compile_arrow_function() {
    // (a, b) => a + b
    let prog = program(vec![expr_stmt(Expression::ArrowFunctionExpression(
        ArrowFunctionExpression {
            params: vec![ident_pat("a"), ident_pat("b")],
            body: ArrowFunctionBody::Expression(Box::new(binary(
                BinaryOp::Add,
                ident_expr("a"),
                ident_expr("b"),
            ))),
            is_async: false,
            location: loc(),
        },
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(
        chunk.instructions[0],
        Instruction::CreateClosure(_)
    ));
}

#[test]
fn compile_function_call() {
    // f(1, 2)
    let prog = program(vec![expr_stmt(Expression::Call(CallExpression {
        callee: Box::new(ident_expr("f")),
        arguments: vec![num_lit(1.0), num_lit(2.0)],
        optional: false,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // LoadGlobal("f"), LoadConst(1), LoadConst(2), Call(2), Pop
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert_eq!(chunk.instructions[1], Instruction::LoadConst(1)); // 1.0
    assert_eq!(chunk.instructions[2], Instruction::LoadConst(2)); // 2.0
    assert_eq!(chunk.instructions[3], Instruction::Call(2));
}

#[test]
fn compile_member_access() {
    // obj.prop
    let prog = program(vec![expr_stmt(Expression::Member(MemberExpression {
        object: Box::new(ident_expr("obj")),
        property: Box::new(ident_expr("prop")),
        computed: false,
        optional: false,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // LoadGlobal("obj"), GetProperty("prop")
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert!(matches!(chunk.instructions[1], Instruction::GetProperty(_)));
}

#[test]
fn compile_computed_member() {
    // obj[0]
    let prog = program(vec![expr_stmt(Expression::Member(MemberExpression {
        object: Box::new(ident_expr("obj")),
        property: Box::new(num_lit(0.0)),
        computed: true,
        optional: false,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert_eq!(chunk.instructions[1], Instruction::LoadConst(1)); // 0.0
    assert_eq!(chunk.instructions[2], Instruction::GetComputed);
}

#[test]
fn compile_array_literal() {
    // [1, 2, 3]
    let prog = program(vec![expr_stmt(Expression::ArrayExpression(
        ArrayExpression {
            elements: vec![Some(num_lit(1.0)), Some(num_lit(2.0)), Some(num_lit(3.0))],
            location: loc(),
        },
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::LoadConst(1));
    assert_eq!(chunk.instructions[2], Instruction::LoadConst(2));
    assert_eq!(chunk.instructions[3], Instruction::CreateArray(3));
}

#[test]
fn compile_object_literal() {
    // { a: 1 }
    let prog = program(vec![expr_stmt(Expression::ObjectExpression(
        ObjectExpression {
            properties: vec![Property {
                key: ident_expr("a"),
                value: num_lit(1.0),
                kind: PropertyKind::Init,
                computed: false,
                shorthand: false,
                location: loc(),
            }],
            location: loc(),
        },
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::CreateObject);
    assert_eq!(chunk.instructions[1], Instruction::Dup);
    assert_eq!(chunk.instructions[2], Instruction::LoadConst(0)); // 1.0
    assert!(matches!(chunk.instructions[3], Instruction::SetProperty(_)));
}

#[test]
fn compile_conditional_expression() {
    // true ? 1 : 2
    let prog = program(vec![expr_stmt(Expression::Conditional(
        ConditionalExpression {
            test: Box::new(Expression::BooleanLiteral(BooleanLiteral {
                value: true,
                location: loc(),
            })),
            consequent: Box::new(num_lit(1.0)),
            alternate: Box::new(num_lit(2.0)),
            location: loc(),
        },
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::True);
    assert!(matches!(chunk.instructions[1], Instruction::JumpIfFalse(_)));
}

#[test]
fn compile_logical_and() {
    // a && b
    let prog = program(vec![expr_stmt(Expression::Logical(LogicalExpression {
        operator: LogicalOp::And,
        left: Box::new(ident_expr("a")),
        right: Box::new(ident_expr("b")),
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // LoadGlobal("a"), Dup, JumpIfFalse, Pop, LoadGlobal("b")
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert_eq!(chunk.instructions[1], Instruction::Dup);
    assert!(matches!(chunk.instructions[2], Instruction::JumpIfFalse(_)));
    assert_eq!(chunk.instructions[3], Instruction::Pop);
}

#[test]
fn compile_logical_or() {
    // a || b
    let prog = program(vec![expr_stmt(Expression::Logical(LogicalExpression {
        operator: LogicalOp::Or,
        left: Box::new(ident_expr("a")),
        right: Box::new(ident_expr("b")),
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert_eq!(chunk.instructions[1], Instruction::Dup);
    assert!(matches!(chunk.instructions[2], Instruction::JumpIfTrue(_)));
    assert_eq!(chunk.instructions[3], Instruction::Pop);
}

#[test]
fn compile_assignment_simple() {
    // let x = 0; x = 5;
    let prog = program(vec![
        Statement::VariableDeclaration(VariableDeclaration {
            kind: VarKind::Let,
            declarations: vec![VariableDeclarator {
                id: ident_pat("x"),
                init: Some(num_lit(0.0)),
                location: loc(),
            }],
            location: loc(),
        }),
        expr_stmt(Expression::Assignment(AssignmentExpression {
            operator: AssignmentOp::Assign,
            left: Box::new(ident_expr("x")),
            right: Box::new(num_lit(5.0)),
            location: loc(),
        })),
    ]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // After var decl: the assignment should use StoreLocal.
    let has_store_local = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::StoreLocal(0)));
    assert!(has_store_local);
}

#[test]
fn compile_compound_assignment() {
    // let x = 10; x += 5;
    let prog = program(vec![
        Statement::VariableDeclaration(VariableDeclaration {
            kind: VarKind::Let,
            declarations: vec![VariableDeclarator {
                id: ident_pat("x"),
                init: Some(num_lit(10.0)),
                location: loc(),
            }],
            location: loc(),
        }),
        expr_stmt(Expression::Assignment(AssignmentExpression {
            operator: AssignmentOp::AddAssign,
            left: Box::new(ident_expr("x")),
            right: Box::new(num_lit(5.0)),
            location: loc(),
        })),
    ]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Should have LoadLocal, LoadConst, Add, Dup, StoreLocal
    let has_add = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::Add));
    assert!(has_add, "Compound assignment should emit Add");
}

#[test]
fn compile_prefix_increment() {
    // let x = 0; ++x;
    let prog = program(vec![
        Statement::VariableDeclaration(VariableDeclaration {
            kind: VarKind::Let,
            declarations: vec![VariableDeclarator {
                id: ident_pat("x"),
                init: Some(num_lit(0.0)),
                location: loc(),
            }],
            location: loc(),
        }),
        expr_stmt(Expression::Update(UpdateExpression {
            operator: UpdateOp::Increment,
            argument: Box::new(ident_expr("x")),
            prefix: true,
            location: loc(),
        })),
    ]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Should load x, add 1, dup, store back.
    let has_add = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::Add));
    assert!(has_add, "Prefix increment should use Add");
}

#[test]
fn compile_postfix_increment() {
    // let x = 0; x++;
    let prog = program(vec![
        Statement::VariableDeclaration(VariableDeclaration {
            kind: VarKind::Let,
            declarations: vec![VariableDeclarator {
                id: ident_pat("x"),
                init: Some(num_lit(0.0)),
                location: loc(),
            }],
            location: loc(),
        }),
        expr_stmt(Expression::Update(UpdateExpression {
            operator: UpdateOp::Increment,
            argument: Box::new(ident_expr("x")),
            prefix: false,
            location: loc(),
        })),
    ]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Should have Dup (to save old value), Add, StoreLocal.
    let has_dup = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::Dup));
    assert!(has_dup, "Postfix increment should Dup old value");
}

#[test]
fn compile_sequence() {
    // (1, 2, 3)
    let prog = program(vec![expr_stmt(Expression::Sequence(SequenceExpression {
        expressions: vec![num_lit(1.0), num_lit(2.0), num_lit(3.0)],
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // LoadConst(1), Pop, LoadConst(2), Pop, LoadConst(3), Pop(expr_stmt), ...
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::Pop);
    assert_eq!(chunk.instructions[2], Instruction::LoadConst(1));
    assert_eq!(chunk.instructions[3], Instruction::Pop);
    assert_eq!(chunk.instructions[4], Instruction::LoadConst(2));
    // Last value stays on stack for the expr stmt's Pop.
}

#[test]
fn compile_block_scoping() {
    // let x = 1; { let y = 2; } x;
    let prog = program(vec![
        Statement::VariableDeclaration(VariableDeclaration {
            kind: VarKind::Let,
            declarations: vec![VariableDeclarator {
                id: ident_pat("x"),
                init: Some(num_lit(1.0)),
                location: loc(),
            }],
            location: loc(),
        }),
        Statement::Block(BlockStatement {
            body: vec![Statement::VariableDeclaration(VariableDeclaration {
                kind: VarKind::Let,
                declarations: vec![VariableDeclarator {
                    id: ident_pat("y"),
                    init: Some(num_lit(2.0)),
                    location: loc(),
                }],
                location: loc(),
            })],
            location: loc(),
        }),
        expr_stmt(ident_expr("x")),
    ]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Since locals are stored in the VM's locals vec (not the stack),
    // end_scope does NOT emit Pop. But expression statements still emit Pop.
    // The block-scoped variable `y` should not be accessible after the block.
    // Verify that local_count includes both x and y (max locals = 2).
    assert_eq!(
        chunk.local_count, 2,
        "Block scoping should track max locals"
    );
}

#[test]
fn compile_switch_statement() {
    // switch (1) { case 1: 10; break; default: 20; }
    let prog = program(vec![Statement::Switch(SwitchStatement {
        discriminant: num_lit(1.0),
        cases: vec![
            SwitchCase {
                test: Some(num_lit(1.0)),
                consequent: vec![
                    expr_stmt(num_lit(10.0)),
                    Statement::Break(BreakStatement {
                        label: None,
                        location: loc(),
                    }),
                ],
                location: loc(),
            },
            SwitchCase {
                test: None,
                consequent: vec![expr_stmt(num_lit(20.0))],
                location: loc(),
            },
        ],
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Should compile without error.
    assert!(chunk.instructions.len() > 5);
    // Should have StrictEq for case comparison.
    let has_strict_eq = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::StrictEq));
    assert!(has_strict_eq, "Switch should use StrictEq");
}

#[test]
fn compile_break_in_while() {
    // while (true) { break; }
    let prog = program(vec![Statement::While(WhileStatement {
        test: Expression::BooleanLiteral(BooleanLiteral {
            value: true,
            location: loc(),
        }),
        body: Box::new(Statement::Block(BlockStatement {
            body: vec![Statement::Break(BreakStatement {
                label: None,
                location: loc(),
            })],
            location: loc(),
        })),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // The break should produce a forward Jump that lands past the loop.
    let jump_count = chunk
        .instructions
        .iter()
        .filter(|i| matches!(i, Instruction::Jump(_)))
        .count();
    // At least 2 jumps: one backward (loop) and one forward (break).
    assert!(jump_count >= 2, "Break should emit a forward jump");
}

#[test]
fn compile_continue_in_while() {
    // while (true) { continue; }
    let prog = program(vec![Statement::While(WhileStatement {
        test: Expression::BooleanLiteral(BooleanLiteral {
            value: true,
            location: loc(),
        }),
        body: Box::new(Statement::Block(BlockStatement {
            body: vec![Statement::Continue(ContinueStatement {
                label: None,
                location: loc(),
            })],
            location: loc(),
        })),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Continue should produce a backward jump.
    let backward_jumps: Vec<_> = chunk
        .instructions
        .iter()
        .filter(|i| matches!(i, Instruction::Jump(off) if *off < 0))
        .collect();
    assert!(
        backward_jumps.len() >= 1,
        "Continue should produce at least one backward jump"
    );
}

#[test]
fn compile_empty_and_debugger_statements() {
    let prog = program(vec![Statement::Empty, Statement::Debugger]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Only Undefined + Return.
    assert_eq!(chunk.instructions.len(), 2);
}

#[test]
fn compile_with_statement_errors() {
    let prog = program(vec![Statement::With(WithStatement {
        object: num_lit(1.0),
        body: Box::new(Statement::Empty),
        location: loc(),
    })]);
    let result = Compiler::compile_program(&prog);
    assert!(result.is_err());
}

#[test]
fn compile_disassemble_output() {
    let prog = program(vec![expr_stmt(binary(
        BinaryOp::Add,
        num_lit(1.0),
        num_lit(2.0),
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    let disasm = chunk.disassemble();
    assert!(disasm.contains("<script>"));
    assert!(disasm.contains("LOAD_CONST"));
    assert!(disasm.contains("ADD"));
}

#[test]
fn compile_nested_functions() {
    // function outer() { function inner() { return 1; } return inner(); }
    let prog = program(vec![Statement::FunctionDeclaration(FunctionDeclaration {
        id: Some("outer".to_string()),
        params: vec![],
        body: Box::new(BlockStatement {
            body: vec![
                Statement::FunctionDeclaration(FunctionDeclaration {
                    id: Some("inner".to_string()),
                    params: vec![],
                    body: Box::new(BlockStatement {
                        body: vec![Statement::Return(ReturnStatement {
                            argument: Some(num_lit(1.0)),
                            location: loc(),
                        })],
                        location: loc(),
                    }),
                    is_async: false,
                    is_generator: false,
                    location: loc(),
                }),
                Statement::Return(ReturnStatement {
                    argument: Some(Expression::Call(CallExpression {
                        callee: Box::new(ident_expr("inner")),
                        arguments: vec![],
                        optional: false,
                        location: loc(),
                    })),
                    location: loc(),
                }),
            ],
            location: loc(),
        }),
        is_async: false,
        is_generator: false,
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Should have a Function constant for "outer".
    let outer = chunk.constants.iter().find_map(|c| match c {
        Constant::Function(f) if f.name == "outer" => Some(f),
        _ => None,
    });
    assert!(outer.is_some(), "Should have outer function");
    let outer = outer.unwrap();
    // outer should have a Function constant for "inner".
    let inner = outer.constants.iter().find_map(|c| match c {
        Constant::Function(f) if f.name == "inner" => Some(f),
        _ => None,
    });
    assert!(inner.is_some(), "outer should contain inner function");
}

#[test]
fn compile_multiple_variables() {
    // let a = 1, b = 2;
    let prog = program(vec![Statement::VariableDeclaration(VariableDeclaration {
        kind: VarKind::Let,
        declarations: vec![
            VariableDeclarator {
                id: ident_pat("a"),
                init: Some(num_lit(1.0)),
                location: loc(),
            },
            VariableDeclarator {
                id: ident_pat("b"),
                init: Some(num_lit(2.0)),
                location: loc(),
            },
        ],
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.local_count, 2);
}

#[test]
fn compile_delete_member() {
    // delete obj.prop
    let prog = program(vec![expr_stmt(Expression::Unary(UnaryExpression {
        operator: UnaryOp::Delete,
        argument: Box::new(Expression::Member(MemberExpression {
            object: Box::new(ident_expr("obj")),
            property: Box::new(ident_expr("prop")),
            computed: false,
            optional: false,
            location: loc(),
        })),
        prefix: true,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    let has_delete = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::Delete));
    assert!(has_delete, "delete obj.prop should emit Delete");
}

#[test]
fn compile_delete_non_member() {
    // delete 1 -> always true
    let prog = program(vec![expr_stmt(Expression::Unary(UnaryExpression {
        operator: UnaryOp::Delete,
        argument: Box::new(num_lit(1.0)),
        prefix: true,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Should push the expression, pop it, then push True.
    let has_true = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::True));
    assert!(has_true, "delete non-member should produce True");
}

#[test]
fn compile_nullish_coalescing() {
    // a ?? b
    let prog = program(vec![expr_stmt(Expression::Logical(LogicalExpression {
        operator: LogicalOp::NullishCoalescing,
        left: Box::new(ident_expr("a")),
        right: Box::new(ident_expr("b")),
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    // Should compile without error.
    assert!(chunk.instructions.len() > 5);
}

#[test]
fn compile_labeled_while() {
    // outer: while (true) { break outer; }
    let prog = program(vec![Statement::Labeled(LabeledStatement {
        label: "outer".to_string(),
        body: Box::new(Statement::While(WhileStatement {
            test: Expression::BooleanLiteral(BooleanLiteral {
                value: true,
                location: loc(),
            }),
            body: Box::new(Statement::Block(BlockStatement {
                body: vec![Statement::Break(BreakStatement {
                    label: Some("outer".to_string()),
                    location: loc(),
                })],
                location: loc(),
            })),
            location: loc(),
        })),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(chunk.instructions.len() > 3);
}
