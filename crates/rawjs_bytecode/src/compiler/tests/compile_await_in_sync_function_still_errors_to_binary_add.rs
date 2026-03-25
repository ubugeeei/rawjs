#[test]
fn compile_await_in_sync_function_still_errors() {
    let prog = program(vec![Statement::FunctionDeclaration(FunctionDeclaration {
        id: Some("syncFn".to_string()),
        params: vec![],
        body: Box::new(BlockStatement {
            body: vec![expr_stmt(Expression::Await(AwaitExpression {
                argument: Box::new(ident_expr("value")),
                location: loc(),
            }))],
            location: loc(),
        }),
        is_async: false,
        is_generator: false,
        location: loc(),
    })]);
    let result = Compiler::compile_program(&prog);
    assert!(result.is_err());
}

#[test]
fn compile_await_using_emits_async_dispose() {
    let prog = program(vec![Statement::Block(BlockStatement {
        body: vec![Statement::VariableDeclaration(VariableDeclaration {
            kind: VarKind::AwaitUsing,
            declarations: vec![VariableDeclarator {
                id: ident_pat("resource"),
                init: Some(ident_expr("value")),
                location: loc(),
            }],
            location: loc(),
        })],
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(chunk
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instruction::AsyncDisposeResource(_))));
}

#[test]
fn compile_await_using_in_sync_function_errors() {
    let prog = program(vec![Statement::FunctionDeclaration(FunctionDeclaration {
        id: Some("syncFn".to_string()),
        params: vec![],
        body: Box::new(BlockStatement {
            body: vec![Statement::VariableDeclaration(VariableDeclaration {
                kind: VarKind::AwaitUsing,
                declarations: vec![VariableDeclarator {
                    id: ident_pat("resource"),
                    init: Some(ident_expr("value")),
                    location: loc(),
                }],
                location: loc(),
            })],
            location: loc(),
        }),
        is_async: false,
        is_generator: false,
        location: loc(),
    })]);
    let result = Compiler::compile_program(&prog);
    assert!(result.is_err());
}

#[test]
fn compile_number_literal() {
    let prog = program(vec![expr_stmt(num_lit(42.0))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
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
    let prog = program(vec![expr_stmt(binary(
        BinaryOp::Add,
        num_lit(1.0),
        num_lit(2.0),
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::LoadConst(1));
    assert_eq!(chunk.instructions[2], Instruction::Add);
    assert_eq!(chunk.instructions[3], Instruction::Pop);
}

use super::*;
