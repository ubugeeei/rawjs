#[test]
fn compile_block_scoping() {
    let prog = program(vec![
        Statement::VariableDeclaration(VariableDeclaration {
            kind: VarKind::Let,
            declarations: vec![VariableDeclarator {
                id: ident_pat("x"),
                init: Some(num_lit(1.0)),
                location: loc(),
            }],
            location: loc(),
        }),
        Statement::Block(BlockStatement {
            body: vec![Statement::VariableDeclaration(VariableDeclaration {
                kind: VarKind::Let,
                declarations: vec![VariableDeclarator {
                    id: ident_pat("y"),
                    init: Some(num_lit(2.0)),
                    location: loc(),
                }],
                location: loc(),
            })],
            location: loc(),
        }),
        expr_stmt(ident_expr("x")),
    ]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(
        chunk.local_count, 2,
        "Block scoping should track max locals"
    );
}

#[test]
fn compile_switch_statement() {
    let prog = program(vec![Statement::Switch(SwitchStatement {
        discriminant: num_lit(1.0),
        cases: vec![
            SwitchCase {
                test: Some(num_lit(1.0)),
                consequent: vec![
                    expr_stmt(num_lit(10.0)),
                    Statement::Break(BreakStatement {
                        label: None,
                        location: loc(),
                    }),
                ],
                location: loc(),
            },
            SwitchCase {
                test: None,
                consequent: vec![expr_stmt(num_lit(20.0))],
                location: loc(),
            },
        ],
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert!(chunk.instructions.len() > 5);
    let has_strict_eq = chunk
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::StrictEq));
    assert!(has_strict_eq, "Switch should use StrictEq");
}

#[test]
fn compile_break_in_while() {
    let prog = program(vec![Statement::While(WhileStatement {
        test: Expression::BooleanLiteral(BooleanLiteral {
            value: true,
            location: loc(),
        }),
        body: Box::new(Statement::Block(BlockStatement {
            body: vec![Statement::Break(BreakStatement {
                label: None,
                location: loc(),
            })],
            location: loc(),
        })),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    let jump_count = chunk
        .instructions
        .iter()
        .filter(|i| matches!(i, Instruction::Jump(_)))
        .count();
    assert!(jump_count >= 2, "Break should emit a forward jump");
}

#[test]
fn compile_continue_in_while() {
    let prog = program(vec![Statement::While(WhileStatement {
        test: Expression::BooleanLiteral(BooleanLiteral {
            value: true,
            location: loc(),
        }),
        body: Box::new(Statement::Block(BlockStatement {
            body: vec![Statement::Continue(ContinueStatement {
                label: None,
                location: loc(),
            })],
            location: loc(),
        })),
        location: loc(),
    })]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    let backward_jumps: Vec<_> = chunk
        .instructions
        .iter()
        .filter(|i| matches!(i, Instruction::Jump(off) if * off < 0))
        .collect();
    assert!(
        !backward_jumps.is_empty(),
        "Continue should produce at least one backward jump"
    );
}

#[test]
fn compile_empty_and_debugger_statements() {
    let prog = program(vec![Statement::Empty, Statement::Debugger]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    assert_eq!(chunk.instructions.len(), 2);
}

#[test]
fn compile_with_statement_errors() {
    let prog = program(vec![Statement::With(WithStatement {
        object: num_lit(1.0),
        body: Box::new(Statement::Empty),
        location: loc(),
    })]);
    let result = Compiler::compile_program(&prog);
    assert!(result.is_err());
}

#[test]
fn compile_disassemble_output() {
    let prog = program(vec![expr_stmt(binary(
        BinaryOp::Add,
        num_lit(1.0),
        num_lit(2.0),
    ))]);
    let chunk = Compiler::compile_program(&prog).unwrap();
    let disasm = chunk.disassemble();
    assert!(disasm.contains("<script>"));
    assert!(disasm.contains("LOAD_CONST"));
    assert!(disasm.contains("ADD"));
}
