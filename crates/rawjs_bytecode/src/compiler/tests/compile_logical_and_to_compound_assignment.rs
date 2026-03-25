#[test]
fn compile_logical_and() {
    let prog = program(vec![expr_stmt(Expression::Logical(LogicalExpression {
        operator: LogicalOp::And,
        left: Box::new(ident_expr("a")),
        right: Box::new(ident_expr("b")),
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert_eq!(chunk.instructions[1], Instruction::Dup);
    assert!(matches!(chunk.instructions[2], Instruction::JumpIfFalse(_)));
    assert_eq!(chunk.instructions[3], Instruction::Pop);
}

#[test]
fn compile_logical_or() {
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
    let has_store_local = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::StoreLocal(0)));
    assert!(has_store_local);
}

#[test]
fn compile_top_level_var_accesses_use_global_binding() {
    let prog = program(vec![
        Statement::VariableDeclaration(VariableDeclaration {
            kind: VarKind::Var,
            declarations: vec![VariableDeclarator {
                id: ident_pat("count"),
                init: Some(num_lit(0.0)),
                location: loc(),
            }],
            location: loc(),
        }),
        expr_stmt(Expression::Assignment(AssignmentExpression {
            operator: AssignmentOp::Assign,
            left: Box::new(ident_expr("count")),
            right: Box::new(num_lit(1.0)),
            location: loc(),
        })),
        expr_stmt(ident_expr("count")),
    ]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(chunk
        .instructions
        .iter()
        .any(|instr| matches!(instr, Instruction::InitGlobal(_))));
    assert!(chunk
        .instructions
        .iter()
        .any(|instr| matches!(instr, Instruction::StoreGlobal(_))));
    assert!(chunk
        .instructions
        .iter()
        .any(|instr| matches!(instr, Instruction::LoadGlobal(_))));
}

#[test]
fn compile_compound_assignment() {
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
    let has_add = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::Add));
    assert!(has_add, "Compound assignment should emit Add");
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
