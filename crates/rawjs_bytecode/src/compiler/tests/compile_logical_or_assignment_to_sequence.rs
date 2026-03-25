#[test]
fn compile_logical_or_assignment() {
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
            operator: AssignmentOp::OrAssign,
            left: Box::new(ident_expr("x")),
            right: Box::new(num_lit(5.0)),
            location: loc(),
        })),
    ]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(chunk
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instruction::JumpIfTrue(_))));
    assert!(chunk
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instruction::StoreLocal(0))));
}

#[test]
fn compile_logical_nullish_assignment_to_computed_member() {
    let prog = program(vec![expr_stmt(Expression::Assignment(
        AssignmentExpression {
            operator: AssignmentOp::NullishCoalescingAssign,
            left: Box::new(Expression::Member(MemberExpression {
                object: Box::new(ident_expr("obj")),
                property: Box::new(ident_expr("key")),
                computed: true,
                optional: false,
                location: loc(),
            })),
            right: Box::new(num_lit(1.0)),
            location: loc(),
        },
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(chunk
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instruction::JumpIfNullish(_))));
    assert!(chunk
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instruction::GetComputed)));
    assert!(chunk
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instruction::SetComputed)));
}

#[test]
fn compile_prefix_increment() {
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
    let has_add = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::Add));
    assert!(has_add, "Prefix increment should use Add");
}

#[test]
fn compile_postfix_increment() {
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
    let has_postfix_increment = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::PostfixIncrement));
    assert!(
        has_postfix_increment,
        "Postfix increment should use the dedicated opcode"
    );
}

#[test]
fn compile_sequence() {
    let prog = program(vec![expr_stmt(Expression::Sequence(SequenceExpression {
        expressions: vec![num_lit(1.0), num_lit(2.0), num_lit(3.0)],
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::Pop);
    assert_eq!(chunk.instructions[2], Instruction::LoadConst(1));
    assert_eq!(chunk.instructions[3], Instruction::Pop);
    assert_eq!(chunk.instructions[4], Instruction::LoadConst(2));
}

use super::*;
