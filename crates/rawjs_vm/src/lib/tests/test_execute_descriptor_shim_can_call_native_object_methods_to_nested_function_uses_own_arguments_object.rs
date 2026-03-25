#[test]
fn test_execute_descriptor_shim_can_call_native_object_methods() {
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
              var result = receiver[key](arguments[1], arguments[2]);
              delete receiver[key];
              return result;
            };

            Object.getOwnPropertyDescriptor = function(obj, name) {
              if (!Object.prototype.hasOwnProperty.call(obj, name)) {
                return undefined;
              }
              return {
                value: obj[name],
                writable: true,
                enumerable: Object.prototype.propertyIsEnumerable.call(obj, name),
                configurable: true
              };
            };

            var argObj = (function () { return arguments; })(1);
            var desc = Object.getOwnPropertyDescriptor(argObj, "0");
            result = desc.value === 1 && desc.enumerable === true;
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_execute_arguments_callee_descriptor() {
    let vm = execute_source(
        r#"
            function testcase() {
              var desc = Object.getOwnPropertyDescriptor(arguments, "callee");
              result =
                arguments.callee === testcase &&
                desc.configurable === true &&
                desc.enumerable === false &&
                desc.writable === true &&
                desc.hasOwnProperty("get") === false &&
                desc.hasOwnProperty("set") === false;
            }
            testcase();
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_execute_strict_arguments_callee_throws_type_error() {
    let vm = execute_source(
        r#"
            "use strict";
            var result = false;
            try {
              (function () {
                arguments.callee;
              })();
            } catch (e) {
              result = e.name === "TypeError";
            }
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_execute_strict_arguments_callee_descriptor_is_accessor() {
    let vm = execute_source(
        r#"
            "use strict";
            var result = false;
            function testcase() {
              var desc = Object.getOwnPropertyDescriptor(arguments, "callee");
              result =
                desc.configurable === false &&
                desc.enumerable === false &&
                desc.hasOwnProperty("value") === false &&
                desc.hasOwnProperty("writable") === false &&
                desc.hasOwnProperty("get") === true &&
                desc.hasOwnProperty("set") === true;
            }
            testcase();
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_execute_arguments_object_uses_object_prototype() {
    let vm = execute_source(
        r#"
            function testcase() {
              result = Object.getPrototypeOf(arguments) === Object.getPrototypeOf({});
            }
            testcase();
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_execute_nested_function_uses_own_arguments_object() {
    let vm = execute_source(
        r#"
            function testcase() {
              var arguments = undefined;
              (function () {
                result = arguments.length;
              })();
            }
            testcase();
            "#,
    );
    let result = vm.get_global("result").cloned().unwrap();
    assert_eq!(result, JsValue::Number(0.0));
}

#[allow(unused_imports)]
use super::*;
