/// Set a property on a JsValue (must be an object).
pub(crate) fn set_property_value(
    vm: &mut Vm,
    obj_val: &JsValue,
    name: &str,
    value: &JsValue,
    is_strict: bool,
) -> Result<()> {
    match obj_val {
        JsValue::Object(ptr) => {
            if let Some(prop) = ptr.borrow().get_property_descriptor(name) {
                if prop.is_accessor() {
                    if let Some(setter) = prop.set {
                        invoke_function_immediately(
                            vm,
                            setter,
                            obj_val.clone(),
                            std::slice::from_ref(value),
                        )?;
                        return Ok(());
                    }
                    if is_strict {
                        return Err(RawJsError::type_error(format!(
                            "Cannot assign to property '{}'",
                            name
                        )));
                    }
                    return Ok(());
                }
                if !prop.writable {
                    if is_strict {
                        return Err(RawJsError::type_error(format!(
                            "Cannot assign to property '{}'",
                            name
                        )));
                    }
                    return Ok(());
                }
            }
            let wrote = ptr
                .borrow_mut()
                .try_set_property(name.to_string(), value.clone());
            if wrote {
                sync_local_from_arguments_object(vm, ptr, name, value.clone());
            }
            if !wrote && is_strict {
                return Err(RawJsError::type_error(format!(
                    "Cannot assign to property '{}'",
                    name
                )));
            }
            Ok(())
        }
        JsValue::String(_) | JsValue::Number(_) | JsValue::Boolean(_) | JsValue::Symbol(_) => {
            let prototype = match obj_val {
                JsValue::String(_) => vm.string_prototype.clone(),
                JsValue::Number(_) => vm.number_prototype.clone(),
                JsValue::Boolean(_) => vm.boolean_prototype.clone(),
                JsValue::Symbol(_) => vm.symbol_prototype.clone(),
                _ => None,
            };
            if let Some(proto) = prototype {
                if let Some(prop) = proto.borrow().get_property_descriptor(name) {
                    if prop.is_accessor() {
                        if let Some(setter) = prop.set {
                            invoke_function_immediately(
                                vm,
                                setter,
                                obj_val.clone(),
                                std::slice::from_ref(value),
                            )?;
                            return Ok(());
                        }
                    }
                }
            }
            if is_strict {
                return Err(RawJsError::type_error(format!(
                    "cannot set property '{}' of {}",
                    name,
                    obj_val.type_of()
                )));
            }
            Ok(())
        }
        _ => Err(RawJsError::type_error(format!(
            "cannot set property '{}' of {}",
            name,
            obj_val.type_of()
        ))),
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
