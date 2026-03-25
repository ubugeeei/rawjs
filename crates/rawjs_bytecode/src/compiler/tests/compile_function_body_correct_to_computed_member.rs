#[test]
fn compile_function_body_correct() {
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
    let func_chunk = chunk.constants.iter().find_map(|c| match c {
        Constant::Function(f) => Some(f),
        _ => None,
    });
    let func_chunk = func_chunk.unwrap();
    assert_eq!(func_chunk.name, "add");
    assert_eq!(func_chunk.param_count, 2);
    assert_eq!(func_chunk.local_count, 2);
    assert_eq!(func_chunk.instructions[0], Instruction::LoadLocal(0));
    assert_eq!(func_chunk.instructions[1], Instruction::LoadLocal(1));
    assert_eq!(func_chunk.instructions[2], Instruction::Add);
    assert_eq!(func_chunk.instructions[3], Instruction::Return);
}

#[test]
fn compile_arrow_function() {
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
    let prog = program(vec![expr_stmt(Expression::Call(CallExpression {
        callee: Box::new(ident_expr("f")),
        arguments: vec![num_lit(1.0), num_lit(2.0)],
        optional: false,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert_eq!(chunk.instructions[1], Instruction::LoadConst(1));
    assert_eq!(chunk.instructions[2], Instruction::LoadConst(2));
    assert_eq!(chunk.instructions[3], Instruction::Call(2));
}

#[test]
fn compile_member_access() {
    let prog = program(vec![expr_stmt(Expression::Member(MemberExpression {
        object: Box::new(ident_expr("obj")),
        property: Box::new(ident_expr("prop")),
        computed: false,
        optional: false,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert!(matches!(chunk.instructions[1], Instruction::GetProperty(_)));
}

#[test]
fn compile_optional_member_access() {
    let prog = program(vec![expr_stmt(Expression::Member(MemberExpression {
        object: Box::new(ident_expr("obj")),
        property: Box::new(ident_expr("prop")),
        computed: false,
        optional: true,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert_eq!(chunk.instructions[1], Instruction::Dup);
    assert!(matches!(
        chunk.instructions[2],
        Instruction::JumpIfNullish(_)
    ));
    assert!(matches!(chunk.instructions[3], Instruction::GetProperty(_)));
}

#[test]
fn compile_computed_member() {
    let prog = program(vec![expr_stmt(Expression::Member(MemberExpression {
        object: Box::new(ident_expr("obj")),
        property: Box::new(num_lit(0.0)),
        computed: true,
        optional: false,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert_eq!(chunk.instructions[1], Instruction::LoadConst(1));
    assert_eq!(chunk.instructions[2], Instruction::GetComputed);
}

use super::*;
