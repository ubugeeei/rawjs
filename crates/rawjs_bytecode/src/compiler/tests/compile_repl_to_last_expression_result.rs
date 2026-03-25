use super::*;

#[test]
fn compile_repl_returns_last_expression_value() {
    let program = program(vec![expr_stmt(binary(
        BinaryOp::Add,
        num_lit(1.0),
        num_lit(2.0),
    ))]);
    let chunk = Compiler::compile_repl_program(&program).unwrap();

    assert_eq!(chunk.instructions.last(), Some(&Instruction::Return));
    assert!(!chunk.instructions.contains(&Instruction::Pop));
    assert!(!chunk.instructions.contains(&Instruction::Undefined));
}

#[test]
fn compile_repl_keeps_undefined_for_declarations() {
    let program = program(vec![Statement::VariableDeclaration(VariableDeclaration {
        kind: VarKind::Let,
        declarations: vec![VariableDeclarator {
            id: ident_pat("value"),
            init: Some(num_lit(1.0)),
            location: loc(),
        }],
        location: loc(),
    })]);
    let chunk = Compiler::compile_repl_program(&program).unwrap();

    assert_eq!(chunk.instructions.last(), Some(&Instruction::Return));
    assert!(chunk.instructions.contains(&Instruction::Undefined));
    assert!(chunk
        .instructions
        .iter()
        .any(|instr| matches!(instr, Instruction::InitGlobal(_))));
}
