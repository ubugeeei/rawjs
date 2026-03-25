#[test]
fn compile_for_loop() {
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
    assert!(chunk.instructions.len() > 5);
    let has_backward_jump = chunk.instructions.iter().any(|i| match i {
        Instruction::Jump(offset) => *offset < 0,
        _ => false,
    });
    assert!(has_backward_jump, "For loop should have a backward jump");
}

#[test]
fn compile_return_statement() {
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
    let has_leave_try = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::LeaveTry));
    assert!(has_leave_try);
}

#[test]
fn compile_try_finally() {
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
    assert_eq!(chunk.local_count, 1);
    let has_func = chunk
        .constants
        .iter()
        .any(|c| matches!(c, Constant::Function(_)));
    assert!(has_func, "Should have a function constant");
}

use super::*;
