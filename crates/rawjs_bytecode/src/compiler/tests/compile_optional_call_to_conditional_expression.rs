#[test]
fn compile_optional_call() {
    let prog = program(vec![expr_stmt(Expression::Call(CallExpression {
        callee: Box::new(ident_expr("fn")),
        arguments: vec![num_lit(1.0)],
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
    assert_eq!(chunk.instructions[4], Instruction::Call(1));
}

#[test]
fn compile_dynamic_import_expression() {
    let prog = program(vec![expr_stmt(Expression::Import(ImportExpression {
        source: Box::new(str_lit("./dep.js")),
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(matches!(chunk.instructions[0], Instruction::LoadConst(_)));
    assert_eq!(chunk.instructions[1], Instruction::ImportModuleDynamic);
}

#[test]
fn compile_import_meta_expression() {
    let prog = program(vec![expr_stmt(Expression::Member(MemberExpression {
        object: Box::new(Expression::ImportMeta(loc())),
        property: Box::new(ident_expr("url")),
        computed: false,
        optional: false,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::ImportMeta);
    assert!(matches!(chunk.instructions[1], Instruction::GetProperty(_)));
}

#[test]
fn compile_optional_assignment_errors() {
    let prog = program(vec![expr_stmt(Expression::Assignment(
        AssignmentExpression {
            operator: AssignmentOp::Assign,
            left: Box::new(Expression::Member(MemberExpression {
                object: Box::new(ident_expr("obj")),
                property: Box::new(ident_expr("prop")),
                computed: false,
                optional: true,
                location: loc(),
            })),
            right: Box::new(num_lit(1.0)),
            location: loc(),
        },
    ))]);
    let result = Compiler::compile_program(&prog);
    assert!(result.is_err());
}

#[test]
fn compile_array_literal() {
    let prog = program(vec![expr_stmt(Expression::ArrayExpression(
        ArrayExpression {
            elements: vec![Some(num_lit(1.0)), Some(num_lit(2.0)), Some(num_lit(3.0))],
            location: loc(),
        },
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::LoadConst(0));
    assert_eq!(chunk.instructions[1], Instruction::LoadConst(1));
    assert_eq!(chunk.instructions[2], Instruction::LoadConst(2));
    assert_eq!(chunk.instructions[3], Instruction::CreateArray(3));
}

#[test]
fn compile_object_literal() {
    let prog = program(vec![expr_stmt(Expression::ObjectExpression(
        ObjectExpression {
            properties: vec![Property {
                key: ident_expr("a"),
                value: num_lit(1.0),
                kind: PropertyKind::Init,
                computed: false,
                shorthand: false,
                location: loc(),
            }],
            location: loc(),
        },
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::CreateObject);
    assert_eq!(chunk.instructions[1], Instruction::Dup);
    assert_eq!(chunk.instructions[2], Instruction::LoadConst(0));
    assert!(matches!(chunk.instructions[3], Instruction::SetProperty(_)));
}

#[test]
fn compile_conditional_expression() {
    let prog = program(vec![expr_stmt(Expression::Conditional(
        ConditionalExpression {
            test: Box::new(Expression::BooleanLiteral(BooleanLiteral {
                value: true,
                location: loc(),
            })),
            consequent: Box::new(num_lit(1.0)),
            alternate: Box::new(num_lit(2.0)),
            location: loc(),
        },
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions[0], Instruction::True);
    assert!(matches!(chunk.instructions[1], Instruction::JumpIfFalse(_)));
}

use super::*;
