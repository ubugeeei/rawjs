#[test]
fn test_execute_new_array_length() {
    let vm = execute_source(
        r#"
            let arr = new Array(5);
            result = arr.length;
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Number(5.0));
}

#[test]
fn test_execute_new_object_missing_property_is_undefined() {
    let vm = execute_source(
        r#"
            result = (new Object()).newProperty === undefined;
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_execute_boolean_primitive_uses_boolean_prototype() {
    let vm = execute_source(
        r#"
            Boolean.prototype.answer = 42;
            result = true.answer;
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_execute_new_boolean_value_of() {
    let vm = execute_source(
        r#"
            let boxed = new Boolean(false);
            result = boxed.valueOf();
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_execute_primitive_assignment_uses_boolean_setter() {
    let vm = execute_source(
        r#"
            var count = 0;
            Object.defineProperty(Boolean.prototype, "test262", {
              set: function () { count += 1; }
            });
            true.test262 = null;
            result = count;
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Number(1.0));
}

#[test]
fn test_execute_var_is_hoisted_before_initializer() {
    let vm = execute_source(
        r#"
            result = x === undefined;
            var x = true;
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_execute_block_var_hoists_to_function_scope() {
    let vm = execute_source(
        r#"
            function f() {
              if (true) {
                var answer = 42;
              }
              return answer;
            }
            result = f();
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Number(42.0));
}

#[test]
fn test_execute_strict_eval_rejects_arguments_assignment() {
    let vm = execute_source(
        r#"
            "use strict";
            var result = false;
            try {
              (function fun() {
                eval("arguments = 10");
              })(30);
            } catch (e) {
              result = e.name === "SyntaxError";
            }
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_execute_arguments_object_persists_index_assignment() {
    let vm = execute_source(
        r#"
            function f() {
              arguments[7] = 12;
              return arguments[7];
            }
            result = f(30);
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Number(12.0));
}

#[test]
fn test_execute_native_methods_inherit_function_prototype_call() {
    let vm = execute_source(
        r#"
            Function.prototype.call = function(thisArg) {
              var receiver = thisArg;
              if (receiver === null || receiver === undefined) {
                receiver = globalThis;
              }
              var key = "__rawjs_call__";
              while (receiver[key] !== undefined) {
                key += "_";
              }
              receiver[key] = this;
              var result = receiver[key](arguments[1]);
              delete receiver[key];
              return result;
            };

            var obj = { x: 1 };
            result = Object.prototype.hasOwnProperty.call(obj, "x");
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[allow(unused_imports)]
use super::*;
