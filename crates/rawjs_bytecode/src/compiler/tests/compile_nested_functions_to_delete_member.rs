#[test]
fn compile_nested_functions() {
    let prog = program(vec![Statement::FunctionDeclaration(FunctionDeclaration {
        id: Some("outer".to_string()),
        params: vec![],
        body: Box::new(BlockStatement {
            body: vec![
                Statement::FunctionDeclaration(FunctionDeclaration {
                    id: Some("inner".to_string()),
                    params: vec![],
                    body: Box::new(BlockStatement {
                        body: vec![Statement::Return(ReturnStatement {
                            argument: Some(num_lit(1.0)),
                            location: loc(),
                        })],
                        location: loc(),
                    }),
                    is_async: false,
                    is_generator: false,
                    location: loc(),
                }),
                Statement::Return(ReturnStatement {
                    argument: Some(Expression::Call(CallExpression {
                        callee: Box::new(ident_expr("inner")),
                        arguments: vec![],
                        optional: false,
                        location: loc(),
                    })),
                    location: loc(),
                }),
            ],
            location: loc(),
        }),
        is_async: false,
        is_generator: false,
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    let outer = chunk.constants.iter().find_map(|c| match c {
        Constant::Function(f) if f.name == "outer" => Some(f),
        _ => None,
    });
    assert!(outer.is_some(), "Should have outer function");
    let outer = outer.unwrap();
    let inner = outer.constants.iter().find_map(|c| match c {
        Constant::Function(f) if f.name == "inner" => Some(f),
        _ => None,
    });
    assert!(inner.is_some(), "outer should contain inner function");
}

#[test]
fn compile_nested_function_uses_globals_for_top_level_var() {
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
        Statement::FunctionDeclaration(FunctionDeclaration {
            id: Some("inc".to_string()),
            params: vec![],
            body: Box::new(BlockStatement {
                body: vec![expr_stmt(Expression::Assignment(AssignmentExpression {
                    operator: AssignmentOp::Assign,
                    left: Box::new(ident_expr("count")),
                    right: Box::new(binary(BinaryOp::Add, ident_expr("count"), num_lit(1.0))),
                    location: loc(),
                }))],
                location: loc(),
            }),
            is_async: false,
            is_generator: false,
            location: loc(),
        }),
    ]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    let inc = chunk.constants.iter().find_map(|c| match c {
        Constant::Function(f) if f.name == "inc" => Some(f),
        _ => None,
    });
    let inc = inc.expect("Should have inc function");
    assert_eq!(inc.upvalue_count, 0);
    assert!(matches!(inc.instructions[0], Instruction::LoadGlobal(_)));
    assert!(matches!(inc.instructions[2], Instruction::Add));
    assert!(matches!(inc.instructions[4], Instruction::StoreGlobal(_)));
}

#[test]
fn compile_multiple_variables() {
    let prog = program(vec![Statement::VariableDeclaration(VariableDeclaration {
        kind: VarKind::Let,
        declarations: vec![
            VariableDeclarator {
                id: ident_pat("a"),
                init: Some(num_lit(1.0)),
                location: loc(),
            },
            VariableDeclarator {
                id: ident_pat("b"),
                init: Some(num_lit(2.0)),
                location: loc(),
            },
        ],
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.local_count, 2);
}

#[test]
fn compile_delete_member() {
    let prog = program(vec![expr_stmt(Expression::Unary(UnaryExpression {
        operator: UnaryOp::Delete,
        argument: Box::new(Expression::Member(MemberExpression {
            object: Box::new(ident_expr("obj")),
            property: Box::new(ident_expr("prop")),
            computed: false,
            optional: false,
            location: loc(),
        })),
        prefix: true,
        location: loc(),
    }))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    let has_delete = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::DeleteProperty));
    assert!(has_delete, "delete obj.prop should emit DeleteProperty");
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
