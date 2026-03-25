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
    assert!(matches!(
        chunk.instructions[0],
        Instruction::LoadGlobalOrUndefined(_)
    ));
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
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.local_count, 1);
}

#[test]
fn compile_variable_no_init() {
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

use super::*;
