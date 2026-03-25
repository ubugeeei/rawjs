#[test]
fn compile_variable_read() {
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
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::StoreLocal(0));
    assert_eq!(chunk.instructions[2], Instruction::LoadLocal(0));
    assert_eq!(chunk.instructions[3], Instruction::Pop);
}

#[test]
fn compile_global_read() {
    let prog = program(vec![expr_stmt(ident_expr("console"))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
}

#[test]
fn compile_if_statement() {
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
    assert_eq!(chunk.instructions[2], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[3], Instruction::Pop);
}

#[test]
fn compile_if_else() {
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
    assert_eq!(chunk.instructions[0], Instruction::True);
    assert!(matches!(chunk.instructions[1], Instruction::JumpIfFalse(_)));
    assert!(matches!(chunk.instructions[4], Instruction::Jump(_)));
}

#[test]
fn compile_while_loop() {
    let prog = program(vec![Statement::While(WhileStatement {
        test: Expression::BooleanLiteral(BooleanLiteral {
            value: true,
            location: loc(),
        }),
        body: Box::new(expr_stmt(num_lit(1.0))),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::True);
    assert!(matches!(chunk.instructions[1], Instruction::JumpIfFalse(_)));
    let last_loop_instr = chunk.instructions.len() - 3;
    assert!(matches!(
        chunk.instructions[last_loop_instr],
        Instruction::Jump(_)
    ));
}

#[test]
fn compile_do_while_loop() {
    let prog = program(vec![Statement::DoWhile(DoWhileStatement {
        body: Box::new(expr_stmt(num_lit(1.0))),
        test: Expression::BooleanLiteral(BooleanLiteral {
            value: false,
            location: loc(),
        }),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::Pop);
    assert_eq!(chunk.instructions[2], Instruction::False);
    assert!(matches!(chunk.instructions[3], Instruction::JumpIfTrue(_)));
}

use super::*;
