use rawjs_common::Result;

use crate::gc::{GcPtr, Heap};

use crate::object::{JsObject, Property};

use crate::value::JsValue;

use super::helpers::set_native;

pub fn create_math_object(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();
    obj.define_property(
        "PI".to_string(),
        Property::readonly(JsValue::Number(std::f64::consts::PI)),
    );
    obj.define_property(
        "E".to_string(),
        Property::readonly(JsValue::Number(std::f64::consts::E)),
    );
    obj.define_property(
        "LN2".to_string(),
        Property::readonly(JsValue::Number(std::f64::consts::LN_2)),
    );
    obj.define_property(
        "LN10".to_string(),
        Property::readonly(JsValue::Number(std::f64::consts::LN_10)),
    );
    obj.define_property(
        "LOG2E".to_string(),
        Property::readonly(JsValue::Number(std::f64::consts::LOG2_E)),
    );
    obj.define_property(
        "LOG10E".to_string(),
        Property::readonly(JsValue::Number(std::f64::consts::LOG10_E)),
    );
    obj.define_property(
        "SQRT2".to_string(),
        Property::readonly(JsValue::Number(std::f64::consts::SQRT_2)),
    );
    obj.define_property(
        "SQRT1_2".to_string(),
        Property::readonly(JsValue::Number(std::f64::consts::FRAC_1_SQRT_2)),
    );
    set_native(&mut obj, "floor", math_floor);
    set_native(&mut obj, "ceil", math_ceil);
    set_native(&mut obj, "round", math_round);
    set_native(&mut obj, "abs", math_abs);
    set_native(&mut obj, "min", math_min);
    set_native(&mut obj, "max", math_max);
    set_native(&mut obj, "random", math_random);
    set_native(&mut obj, "sqrt", math_sqrt);
    set_native(&mut obj, "pow", math_pow);
    set_native(&mut obj, "log", math_log_fn);
    set_native(&mut obj, "sin", math_sin);
    set_native(&mut obj, "cos", math_cos);
    set_native(&mut obj, "tan", math_tan);
    set_native(&mut obj, "trunc", math_trunc);
    set_native(&mut obj, "sign", math_sign);
    heap.alloc(obj)
}

pub(super) fn math_floor(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.floor()))
}

pub(super) fn math_ceil(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.ceil()))
}

pub(super) fn math_round(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    if n.is_nan() || n.is_infinite() || n == 0.0 {
        return Ok(JsValue::Number(n));
    }
    Ok(JsValue::Number((n + 0.5).floor()))
}

pub(super) fn math_abs(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.abs()))
}

pub(super) fn math_min(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    if args.is_empty() {
        return Ok(JsValue::Number(f64::INFINITY));
    }
    let mut result = f64::INFINITY;
    for arg in args {
        let n = arg.to_number();
        if n.is_nan() {
            return Ok(JsValue::Number(f64::NAN));
        }
        if n < result || (n == 0.0 && result == 0.0 && n.is_sign_negative()) {
            result = n;
        }
    }
    Ok(JsValue::Number(result))
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
