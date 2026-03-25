#[test]
fn test_execute_async_generator_method_next_resolves() {
    let vm = execute_source(
        r#"
            var obj = {
              async *method() {
                callCount = arguments.length;
              }
            };
            obj.method(42, "TC39").next().then(function (step) {
              done = step.done;
            });
            "#,
    );
    let call_count = vm.get_global("callCount").cloned().unwrap();
    let done = vm.get_global("done").cloned().unwrap();
    assert_eq!(call_count, JsValue::Number(2.0));
    assert_eq!(done, JsValue::Boolean(true));
}

#[test]
fn test_execute_async_generator_next_survives_nested_call() {
    let vm = execute_source(
        r#"
            var helper = function (a, b) { return a === b; };
            var obj = {
              async *method() {
                helper(arguments.length, 2);
              }
            };
            var promise = obj.method(42, "TC39").next();
            kind = typeof promise;
            thenKind = typeof promise.then;
            promise.then(function (step) {
              done = step.done;
            });
            "#,
    );
    let kind = vm.get_global("kind").cloned().unwrap();
    let then_kind = vm.get_global("thenKind").cloned().unwrap();
    let done = vm.get_global("done").cloned().unwrap();
    assert_eq!(kind, JsValue::string("object"));
    assert_eq!(then_kind, JsValue::string("function"));
    assert_eq!(done, JsValue::Boolean(true));
}

#[test]
fn test_execute_generator_next_survives_nested_call() {
    let vm = execute_source(
        r#"
            var helper = function (a, b) { return a === b; };
            function *gen() {
              helper(1, 1);
            }
            var step = gen().next();
            kind = typeof step;
            done = step.done;
            "#,
    );
    let kind = vm.get_global("kind").cloned().unwrap();
    let done = vm.get_global("done").cloned().unwrap();
    assert_eq!(kind, JsValue::string("object"));
    assert_eq!(done, JsValue::Boolean(true));
}

use super::*;
