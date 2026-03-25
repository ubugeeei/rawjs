#[test]
fn test_execute_global_store_load() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let name_idx = chunk
        .add_constant(Constant::String("myGlobal".to_string()))
        .unwrap();
    let val_idx = chunk.add_constant(Constant::Number(123.0)).unwrap();
    chunk.emit(Instruction::LoadConst(val_idx));
    chunk.emit(Instruction::StoreGlobal(name_idx));
    chunk.emit(Instruction::LoadGlobal(name_idx));
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Number(123.0));
}

#[test]
fn test_execute_bitwise() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    let a = chunk.add_constant(Constant::Number(0b1100 as f64)).unwrap();
    let b = chunk.add_constant(Constant::Number(0b1010 as f64)).unwrap();
    chunk.emit(Instruction::LoadConst(a));
    chunk.emit(Instruction::LoadConst(b));
    chunk.emit(Instruction::BitAnd);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Number(0b1000 as f64));
}

#[test]
fn test_execute_function_call() {
    let mut vm = Vm::new();
    let mut func_chunk = Chunk::new("addOne");
    func_chunk.param_count = 1;
    func_chunk.local_count = 1;
    let one = func_chunk.add_constant(Constant::Number(1.0)).unwrap();
    func_chunk.emit(Instruction::LoadLocal(0));
    func_chunk.emit(Instruction::LoadConst(one));
    func_chunk.emit(Instruction::Add);
    func_chunk.emit(Instruction::Return);
    let mut chunk = Chunk::new("<test>");
    let func_const = chunk
        .add_constant(Constant::Function(Box::new(func_chunk)))
        .unwrap();
    let arg = chunk.add_constant(Constant::Number(41.0)).unwrap();
    chunk.emit(Instruction::CreateClosure(func_const));
    chunk.emit(Instruction::LoadConst(arg));
    chunk.emit(Instruction::Call(1));
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_execute_top_level_await() {
    let mut vm = Vm::new();
    let mut chunk = Chunk::new("<test>");
    chunk.is_async = true;
    let promise_name = chunk
        .add_constant(Constant::String("Promise".to_string()))
        .unwrap();
    let resolve_name = chunk
        .add_constant(Constant::String("resolve".to_string()))
        .unwrap();
    let value = chunk.add_constant(Constant::Number(42.0)).unwrap();
    chunk.emit(Instruction::LoadGlobal(promise_name));
    chunk.emit(Instruction::Dup);
    chunk.emit(Instruction::GetProperty(resolve_name));
    chunk.emit(Instruction::LoadConst(value));
    chunk.emit(Instruction::CallMethod(1));
    chunk.emit(Instruction::Await);
    chunk.emit(Instruction::Return);
    let result = vm.execute(chunk).unwrap();
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_execute_module_with_top_level_await() {
    let mut vm = Vm::new();
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let module_dir = std::env::temp_dir().join(format!("rawjs-top-level-await-{}", unique));
    let module_path = module_dir.join("dep.js");
    let _ = fs::remove_dir_all(&module_dir);
    fs::create_dir_all(&module_dir).unwrap();
    fs::write(&module_path, "export default await Promise.resolve(42);\n").unwrap();
    vm.current_file_dir = Some(module_dir.to_string_lossy().to_string());
    let exports = vm.execute_module("./dep.js").unwrap();
    assert_eq!(
        exports.borrow().get_property("default"),
        JsValue::Number(42.0)
    );
    let _ = fs::remove_dir_all(&module_dir);
}

#[test]
fn test_execute_import_meta_url() {
    let mut vm = Vm::new();
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let module_dir = std::env::temp_dir().join(format!("rawjs-import-meta-{}", unique));
    let module_path = module_dir.join("dep.js");
    let _ = fs::remove_dir_all(&module_dir);
    fs::create_dir_all(&module_dir).unwrap();
    fs::write(&module_path, "export default import.meta.url;\n").unwrap();
    vm.current_file_dir = Some(module_dir.to_string_lossy().to_string());
    let exports = vm.execute_module("./dep.js").unwrap();
    assert_eq!(
        exports.borrow().get_property("default"),
        JsValue::string(module_path.canonicalize().unwrap().to_string_lossy())
    );
    let _ = fs::remove_dir_all(&module_dir);
}

#[allow(unused_imports)]
use super::*;
