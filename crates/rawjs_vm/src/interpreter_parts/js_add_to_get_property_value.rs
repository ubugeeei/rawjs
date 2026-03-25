/// JavaScript `+` operator: string concatenation if either operand is a
/// string, otherwise numeric addition.
pub(crate) fn js_add(lhs: &JsValue, rhs: &JsValue) -> JsValue {
    match (lhs, rhs) {
        (JsValue::String(a), JsValue::String(b)) => {
            let mut s = String::with_capacity(a.len() + b.len());
            s.push_str(a);
            s.push_str(b);
            JsValue::string(s.as_str())
        }
        (JsValue::String(a), _) => {
            let b_str = rhs.to_js_string();
            let mut s = String::with_capacity(a.len() + b_str.len());
            s.push_str(a);
            s.push_str(&b_str);
            JsValue::string(s.as_str())
        }
        (_, JsValue::String(b)) => {
            let a_str = lhs.to_js_string();
            let mut s = String::with_capacity(a_str.len() + b.len());
            s.push_str(&a_str);
            s.push_str(b);
            JsValue::string(s.as_str())
        }
        _ => JsValue::Number(lhs.to_number() + rhs.to_number()),
    }
}

fn invoke_function_immediately(
    vm: &mut Vm,
    callee: JsValue,
    this_value: JsValue,
    args: &[JsValue],
) -> Result<JsValue> {
    let saved_stack_len = vm.value_stack.len();
    let saved_call_depth = vm.call_stack.len();
    vm.push(callee);
    for arg in args {
        vm.push(arg.clone());
    }
    exec_call(vm, args.len(), this_value)?;
    if vm.call_stack.len() > saved_call_depth {
        run_executor_frame(vm, saved_call_depth)?;
    }
    let result = if vm.value_stack.len() > saved_stack_len {
        vm.pop()?
    } else {
        JsValue::Undefined
    };
    vm.value_stack.truncate(saved_stack_len);
    Ok(result)
}

/// Get a property from a JsValue (must be an object).
pub(crate) fn get_property_value(vm: &mut Vm, obj_val: &JsValue, name: &str) -> Result<JsValue> {
    match obj_val {
        JsValue::Object(ptr) => {
            if let Some(prop) = ptr.borrow().get_property_descriptor(name) {
                if prop.is_accessor() {
                    if let Some(getter) = prop.get {
                        return invoke_function_immediately(vm, getter, obj_val.clone(), &[]);
                    }
                    return Ok(JsValue::Undefined);
                }
                return Ok(prop.value);
            }
            let obj = ptr.borrow();
            let val = obj.get_property(name);
            if !val.is_undefined() {
                return Ok(val);
            }
            if obj.is_array() {
                if name == "length" {
                    return Ok(JsValue::Number(obj.array_length() as f64));
                }
                drop(obj);
                if let Some(ref proto) = vm.array_prototype {
                    let result = proto.borrow().get_property(name);
                    if !result.is_undefined() {
                        return Ok(result);
                    }
                }
                return Ok(JsValue::Undefined);
            }
            if matches!(obj.internal, ObjectInternal::Map(_)) {
                drop(obj);
                if let Some(ref proto) = vm.map_prototype {
                    let result = proto.borrow().get_property(name);
                    if !result.is_undefined() {
                        return Ok(result);
                    }
                }
                return Ok(val);
            }
            if matches!(obj.internal, ObjectInternal::Set(_)) {
                drop(obj);
                if let Some(ref proto) = vm.set_prototype {
                    let result = proto.borrow().get_property(name);
                    if !result.is_undefined() {
                        return Ok(result);
                    }
                }
                return Ok(val);
            }
            if matches!(obj.internal, ObjectInternal::Promise(_)) {
                drop(obj);
                if let Some(ref proto) = vm.promise_prototype {
                    let result = proto.borrow().get_property(name);
                    if !result.is_undefined() {
                        return Ok(result);
                    }
                }
                return Ok(val);
            }
            if matches!(obj.internal, ObjectInternal::Generator(_)) {
                drop(obj);
                if let Some(ref proto) = vm.generator_prototype {
                    let result = proto.borrow().get_property(name);
                    if !result.is_undefined() {
                        return Ok(result);
                    }
                }
                return Ok(val);
            }
            Ok(val)
        }
        JsValue::String(s) => match name {
            "length" => Ok(JsValue::Number(s.len() as f64)),
            _ => {
                if let Ok(idx) = name.parse::<usize>() {
                    if idx < s.len() {
                        let ch = s.chars().nth(idx).unwrap_or('\0');
                        return Ok(JsValue::string(ch.to_string().as_str()));
                    }
                }
                if let Some(ref proto) = vm.string_prototype {
                    let result = proto.borrow().get_property(name);
                    if !result.is_undefined() {
                        return Ok(result);
                    }
                }
                Ok(JsValue::Undefined)
            }
        },
        JsValue::Number(_) => {
            if let Some(ref proto) = vm.number_prototype {
                let result = proto.borrow().get_property(name);
                if !result.is_undefined() {
                    return Ok(result);
                }
            }
            Ok(JsValue::Undefined)
        }
        JsValue::Boolean(_) => {
            if let Some(ref proto) = vm.boolean_prototype {
                let result = proto.borrow().get_property(name);
                if !result.is_undefined() {
                    return Ok(result);
                }
            }
            Ok(JsValue::Undefined)
        }
        JsValue::Symbol(sym) => {
            if name == "description" {
                return Ok(match &sym.description {
                    Some(desc) => JsValue::string(desc.as_ref()),
                    None => JsValue::Undefined,
                });
            }
            if let Some(ref proto) = vm.symbol_prototype {
                let result = proto.borrow().get_property(name);
                if !result.is_undefined() {
                    return Ok(result);
                }
            }
            Ok(JsValue::Undefined)
        }
        _ => Ok(JsValue::Undefined),
    }
}
