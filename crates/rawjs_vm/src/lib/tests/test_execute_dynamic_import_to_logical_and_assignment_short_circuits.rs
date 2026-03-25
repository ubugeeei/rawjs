#[test]
fn test_execute_dynamic_import() {
    let mut vm = Vm::new();
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let module_dir = std::env::temp_dir().join(format!("rawjs-dynamic-import-{}", unique));
    let module_path = module_dir.join("dep.js");
    let _ = fs::remove_dir_all(&module_dir);
    fs::create_dir_all(&module_dir).unwrap();
    fs::write(&module_path, "export default 42;\n").unwrap();
    vm.current_file_dir = Some(module_dir.to_string_lossy().to_string());
    let program =
        rawjs_parser::parse("name = 'dep'; out = (await import('./' + name + '.js')).default;")
            .unwrap();
    let chunk = rawjs_bytecode::compile(&program).unwrap();
    vm.execute(chunk).unwrap();
    assert_eq!(vm.get_global("out"), Some(&JsValue::Number(42.0)));
    let _ = fs::remove_dir_all(&module_dir);
}

#[test]
fn test_execute_dynamic_import_rejects_missing_module() {
    let mut vm = Vm::new();
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let module_dir = std::env::temp_dir().join(format!("rawjs-dynamic-import-missing-{}", unique));
    let _ = fs::remove_dir_all(&module_dir);
    fs::create_dir_all(&module_dir).unwrap();
    vm.current_file_dir = Some(module_dir.to_string_lossy().to_string());
    let program = rawjs_parser::parse(
        "out = ''; try { await import('./missing.js'); } catch (e) { out = e; }",
    )
    .unwrap();
    let chunk = rawjs_bytecode::compile(&program).unwrap();
    vm.execute(chunk).unwrap();
    let out = vm.get_global("out").cloned().unwrap_or(JsValue::Undefined);
    let out = out.to_string_value();
    assert!(out.contains("Cannot find module './missing.js'"));
    let _ = fs::remove_dir_all(&module_dir);
}

#[test]
fn test_execute_optional_member_access() {
    let vm = execute_source("let obj = { value: 42 }; out = obj?.value;");
    assert_eq!(vm.get_global("out"), Some(&JsValue::Number(42.0)));
}

#[test]
fn test_execute_optional_member_access_on_null() {
    let vm = execute_source("let obj = null; out = obj?.value;");
    assert_eq!(vm.get_global("out"), Some(&JsValue::Undefined));
}

#[test]
fn test_execute_optional_method_call_preserves_this() {
    let vm = execute_source(
        "let obj = { value: 42, getValue: function() { return this.value; } }; out = obj.getValue?.();",
    );
    assert_eq!(vm.get_global("out"), Some(&JsValue::Number(42.0)));
}

#[test]
fn test_execute_optional_method_call_on_null_receiver() {
    let vm = execute_source("let obj = null; out = obj?.getValue();");
    assert_eq!(vm.get_global("out"), Some(&JsValue::Undefined));
}

#[test]
fn test_execute_optional_delete_on_null_receiver() {
    let vm = execute_source("let obj = null; out = delete obj?.value;");
    assert_eq!(vm.get_global("out"), Some(&JsValue::Boolean(true)));
}

#[test]
fn test_execute_using_calls_bytecode_dispose() {
    let vm = execute_source(
        "disposed = 0; { using r = { [Symbol.dispose]: function() { disposed = 1; } }; } out = disposed;",
    );
    assert_eq!(vm.get_global("out"), Some(&JsValue::Number(1.0)));
}

#[test]
fn test_execute_await_using_awaits_async_dispose() {
    let vm = execute_source(
        "disposed = 0; { await using r = { [Symbol.asyncDispose]: async function() { await Promise.resolve(0); disposed = 1; } }; } out = disposed;",
    );
    assert_eq!(vm.get_global("out"), Some(&JsValue::Number(1.0)));
}

#[test]
fn test_execute_await_using_sync_fallback_is_not_awaited() {
    let vm = execute_source(
        "disposed = 0; { await using r = { [Symbol.dispose]: async function() { await Promise.resolve(0); disposed = 1; } }; } out = disposed;",
    );
    assert_eq!(vm.get_global("out"), Some(&JsValue::Number(0.0)));
}

#[test]
fn test_execute_logical_or_assignment() {
    let vm = execute_source("let value = 0; out = (value ||= 42); out2 = value;");
    assert_eq!(vm.get_global("out"), Some(&JsValue::Number(42.0)));
    assert_eq!(vm.get_global("out2"), Some(&JsValue::Number(42.0)));
}

#[test]
fn test_execute_logical_and_assignment_short_circuits() {
    let vm = execute_source("let value = 0; out = (value &&= 42); out2 = value;");
    assert_eq!(vm.get_global("out"), Some(&JsValue::Number(0.0)));
    assert_eq!(vm.get_global("out2"), Some(&JsValue::Number(0.0)));
}

use super::*;
