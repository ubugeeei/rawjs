#[test]
fn compile_delete_non_member() {
    let prog = program(vec![expr_stmt(Expression::Unary(UnaryExpression {
        operator: UnaryOp::Delete,
        argument: Box::new(num_lit(1.0)),
        prefix: true,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    let has_true = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::True));
    assert!(has_true, "delete non-member should produce True");
}

#[test]
fn compile_nullish_coalescing() {
    let prog = program(vec![expr_stmt(Expression::Logical(LogicalExpression {
        operator: LogicalOp::NullishCoalescing,
        left: Box::new(ident_expr("a")),
        right: Box::new(ident_expr("b")),
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(chunk.instructions.len() > 5);
}

#[test]
fn compile_labeled_while() {
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

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
