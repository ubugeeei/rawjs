use super::*;

use rawjs_common::SourceLocation;

pub(super) fn loc() -> SourceLocation {
    SourceLocation::new(1, 0, 0)
}

/// Helper: build a minimal Program from a list of statements.
pub(super) fn program(body: Vec<Statement>) -> Program {
    Program {
        body,
        location: loc(),
    }
}

/// Helper: build an expression statement.
pub(super) fn expr_stmt(expression: Expression) -> Statement {
    Statement::Expression(ExpressionStatement {
        expression,
        location: loc(),
    })
}

pub(super) fn num_lit(value: f64) -> Expression {
    Expression::NumberLiteral(NumberLiteral {
        value,
        location: loc(),
    })
}

pub(super) fn str_lit(value: &str) -> Expression {
    Expression::StringLiteral(StringLiteral {
        value: value.to_string(),
        location: loc(),
    })
}

pub(super) fn ident_expr(name: &str) -> Expression {
    Expression::Identifier(IdentifierExpression {
        name: name.to_string(),
        location: loc(),
    })
}

pub(super) fn ident_pat(name: &str) -> Pattern {
    Pattern::Identifier(IdentifierPattern {
        name: name.to_string(),
        location: loc(),
    })
}

pub(super) fn binary(op: BinaryOp, left: Expression, right: Expression) -> Expression {
    Expression::Binary(BinaryExpression {
        operator: op,
        left: Box::new(left),
        right: Box::new(right),
        location: loc(),
    })
}

#[test]
fn compile_empty_program() {
    let prog = program(vec![]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions.len(), 2);
    assert_eq!(chunk.instructions[0], Instruction::Undefined);
    assert_eq!(chunk.instructions[1], Instruction::Return);
}

#[test]
fn compile_top_level_await_marks_program_async() {
    let prog = program(vec![expr_stmt(Expression::Await(AwaitExpression {
        argument: Box::new(ident_expr("value")),
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(chunk.is_async);
    assert!(matches!(chunk.instructions[0], Instruction::LoadGlobal(_)));
    assert_eq!(chunk.instructions[1], Instruction::Await);
}
