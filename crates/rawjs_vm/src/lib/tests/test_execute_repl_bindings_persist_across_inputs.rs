use super::*;

fn execute_repl_input(vm: &mut Vm, source: &str) -> JsValue {
    let program = rawjs_parser::parse(source).unwrap();
    let chunk = rawjs_bytecode::compile_repl(&program).unwrap();
    vm.execute(chunk).unwrap()
}

#[test]
fn test_execute_repl_const_binding_persists_across_inputs() {
    let mut vm = Vm::new();

    assert_eq!(
        execute_repl_input(&mut vm, "const a = 1"),
        JsValue::Undefined
    );
    assert_eq!(execute_repl_input(&mut vm, "a"), JsValue::Number(1.0));
}

#[test]
fn test_execute_repl_let_binding_persists_across_inputs() {
    let mut vm = Vm::new();

    assert_eq!(
        execute_repl_input(&mut vm, "let value = 41"),
        JsValue::Undefined
    );
    assert_eq!(
        execute_repl_input(&mut vm, "value + 1"),
        JsValue::Number(42.0)
    );
}
