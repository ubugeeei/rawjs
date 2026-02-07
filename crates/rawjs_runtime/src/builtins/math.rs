use std::sync::atomic::{AtomicU64, Ordering};

use rawjs_common::Result;

use crate::gc::{GcPtr, Heap};
use crate::object::{JsObject, Property};
use crate::value::JsValue;

use super::helpers::set_native;

pub fn create_math_object(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();

    // Constants
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

    // Methods
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

fn math_floor(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.floor()))
}

fn math_ceil(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.ceil()))
}

fn math_round(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    // JS Math.round rounds half-up (not half-even).
    if n.is_nan() || n.is_infinite() || n == 0.0 {
        return Ok(JsValue::Number(n));
    }
    Ok(JsValue::Number((n + 0.5).floor()))
}

fn math_abs(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.abs()))
}

fn math_min(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
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

fn math_max(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    if args.is_empty() {
        return Ok(JsValue::Number(f64::NEG_INFINITY));
    }
    let mut result = f64::NEG_INFINITY;
    for arg in args {
        let n = arg.to_number();
        if n.is_nan() {
            return Ok(JsValue::Number(f64::NAN));
        }
        if n > result || (n == 0.0 && result == 0.0 && n.is_sign_positive()) {
            result = n;
        }
    }
    Ok(JsValue::Number(result))
}

fn math_random(_heap: &mut Heap, _this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    static SEED: AtomicU64 = AtomicU64::new(0);
    let mut s = SEED.load(Ordering::Relaxed);
    if s == 0 {
        // Initialize seed from address of a stack variable (ASLR entropy)
        let stack_var: u64 = 0;
        s = (&stack_var as *const u64 as u64) ^ 0x517cc1b727220a95;
        if s == 0 {
            s = 1;
        }
    }
    // xorshift64
    s ^= s << 13;
    s ^= s >> 7;
    s ^= s << 17;
    SEED.store(s, Ordering::Relaxed);
    // Convert to [0, 1) range
    let val = (s >> 11) as f64 / (1u64 << 53) as f64;
    Ok(JsValue::Number(val))
}

fn math_sqrt(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.sqrt()))
}

fn math_pow(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let base = args.first().unwrap_or(&JsValue::Undefined).to_number();
    let exp = args.get(1).unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(base.powf(exp)))
}

fn math_log_fn(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.ln()))
}

fn math_sin(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.sin()))
}

fn math_cos(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.cos()))
}

fn math_tan(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.tan()))
}

fn math_trunc(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    Ok(JsValue::Number(n.trunc()))
}

fn math_sign(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = args.first().unwrap_or(&JsValue::Undefined).to_number();
    if n.is_nan() {
        Ok(JsValue::Number(f64::NAN))
    } else if n == 0.0 {
        Ok(JsValue::Number(n)) // preserve sign of zero
    } else if n > 0.0 {
        Ok(JsValue::Number(1.0))
    } else {
        Ok(JsValue::Number(-1.0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_math_constants() {
        let mut heap = Heap::new();
        let math = create_math_object(&mut heap);
        let obj = math.borrow();
        if let JsValue::Number(pi) = obj.get_property("PI") {
            assert!((pi - std::f64::consts::PI).abs() < 1e-15);
        } else {
            panic!("Math.PI should be a number");
        }
        if let JsValue::Number(e) = obj.get_property("E") {
            assert!((e - std::f64::consts::E).abs() < 1e-15);
        } else {
            panic!("Math.E should be a number");
        }
    }

    #[test]
    fn test_math_floor() {
        let mut heap = Heap::new();
        let result = math_floor(&mut heap, &JsValue::Undefined, &[JsValue::Number(4.7)]).unwrap();
        assert_eq!(result, JsValue::Number(4.0));
    }

    #[test]
    fn test_math_ceil() {
        let mut heap = Heap::new();
        let result = math_ceil(&mut heap, &JsValue::Undefined, &[JsValue::Number(4.2)]).unwrap();
        assert_eq!(result, JsValue::Number(5.0));
    }

    #[test]
    fn test_math_round() {
        let mut heap = Heap::new();
        assert_eq!(
            math_round(&mut heap, &JsValue::Undefined, &[JsValue::Number(4.5)]).unwrap(),
            JsValue::Number(5.0)
        );
        assert_eq!(
            math_round(&mut heap, &JsValue::Undefined, &[JsValue::Number(4.4)]).unwrap(),
            JsValue::Number(4.0)
        );
    }

    #[test]
    fn test_math_abs() {
        let mut heap = Heap::new();
        assert_eq!(
            math_abs(&mut heap, &JsValue::Undefined, &[JsValue::Number(-5.0)]).unwrap(),
            JsValue::Number(5.0)
        );
    }

    #[test]
    fn test_math_min_max() {
        let mut heap = Heap::new();
        assert_eq!(
            math_min(
                &mut heap,
                &JsValue::Undefined,
                &[
                    JsValue::Number(3.0),
                    JsValue::Number(1.0),
                    JsValue::Number(2.0)
                ]
            )
            .unwrap(),
            JsValue::Number(1.0)
        );
        assert_eq!(
            math_max(
                &mut heap,
                &JsValue::Undefined,
                &[
                    JsValue::Number(3.0),
                    JsValue::Number(1.0),
                    JsValue::Number(2.0)
                ]
            )
            .unwrap(),
            JsValue::Number(3.0)
        );
        assert_eq!(
            math_min(&mut heap, &JsValue::Undefined, &[]).unwrap(),
            JsValue::Number(f64::INFINITY)
        );
        assert_eq!(
            math_max(&mut heap, &JsValue::Undefined, &[]).unwrap(),
            JsValue::Number(f64::NEG_INFINITY)
        );
    }

    #[test]
    fn test_math_sqrt() {
        let mut heap = Heap::new();
        assert_eq!(
            math_sqrt(&mut heap, &JsValue::Undefined, &[JsValue::Number(9.0)]).unwrap(),
            JsValue::Number(3.0)
        );
    }

    #[test]
    fn test_math_pow() {
        let mut heap = Heap::new();
        assert_eq!(
            math_pow(
                &mut heap,
                &JsValue::Undefined,
                &[JsValue::Number(2.0), JsValue::Number(8.0)]
            )
            .unwrap(),
            JsValue::Number(256.0)
        );
    }

    #[test]
    fn test_math_random() {
        let mut heap = Heap::new();
        let result = math_random(&mut heap, &JsValue::Undefined, &[]).unwrap();
        if let JsValue::Number(n) = result {
            assert!((0.0..1.0).contains(&n));
        } else {
            panic!("Math.random should return a number");
        }
    }
}
