use rawjs_common::{RawJsError, Result};

use crate::gc::{GcPtr, Heap};
use crate::object::JsObject;
use crate::value::JsValue;

use super::helpers::set_native;

pub fn create_number_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();

    set_native(&mut obj, "toString", number_to_string_method);
    set_native(&mut obj, "toFixed", number_to_fixed);
    set_native(&mut obj, "valueOf", number_value_of);
    set_native(&mut obj, "toLocaleString", number_to_string_method);
    set_native(&mut obj, "toPrecision", number_to_precision);

    heap.alloc(obj)
}

fn number_to_string_method(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = this.to_number();
    let radix = args.first().map(|v| v.to_number() as u32).unwrap_or(10);
    if !(2..=36).contains(&radix) {
        return Err(RawJsError::range_error(
            "toString() radix must be between 2 and 36",
        ));
    }
    if radix == 10 {
        return Ok(JsValue::string(this.to_string_value()));
    }
    if n.is_nan() {
        return Ok(JsValue::string("NaN"));
    }
    if n.is_infinite() {
        return Ok(JsValue::string(if n > 0.0 {
            "Infinity"
        } else {
            "-Infinity"
        }));
    }
    let negative = n < 0.0;
    let abs = n.abs();
    let int_part = abs as u64;
    let mut digits = String::new();
    if int_part == 0 {
        digits.push('0');
    } else {
        let mut val = int_part;
        while val > 0 {
            let d = (val % radix as u64) as u32;
            let ch = char::from_digit(d, radix).unwrap_or('?');
            digits.push(ch);
            val /= radix as u64;
        }
    }
    let result: String = if negative {
        format!("-{}", digits.chars().rev().collect::<String>())
    } else {
        digits.chars().rev().collect()
    };
    Ok(JsValue::string(result))
}

fn number_to_fixed(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = this.to_number();
    let digits = args.first().map(|v| v.to_number() as i32).unwrap_or(0);
    if !(0..=100).contains(&digits) {
        return Err(RawJsError::range_error(
            "toFixed() digits argument must be between 0 and 100",
        ));
    }
    Ok(JsValue::string(format!(
        "{:.prec$}",
        n,
        prec = digits as usize
    )))
}

fn number_value_of(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    Ok(JsValue::Number(this.to_number()))
}

fn number_to_precision(_heap: &mut Heap, this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let n = this.to_number();
    match args.first() {
        None | Some(JsValue::Undefined) => Ok(JsValue::string(this.to_string_value())),
        Some(prec_val) => {
            let prec = prec_val.to_number() as usize;
            if !(1..=100).contains(&prec) {
                return Err(RawJsError::range_error(
                    "toPrecision() argument must be between 1 and 100",
                ));
            }
            Ok(JsValue::string(format!("{:.prec$e}", n, prec = prec - 1)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number_to_fixed_method() {
        let mut heap = Heap::new();
        assert_eq!(
            number_to_fixed(
                &mut heap,
                &JsValue::Number(3.14159),
                &[JsValue::Number(2.0)]
            )
            .unwrap(),
            JsValue::string("3.14")
        );
    }

    #[test]
    fn test_number_to_string_radix() {
        let mut heap = Heap::new();
        assert_eq!(
            number_to_string_method(&mut heap, &JsValue::Number(255.0), &[JsValue::Number(16.0)])
                .unwrap(),
            JsValue::string("ff")
        );
        assert_eq!(
            number_to_string_method(&mut heap, &JsValue::Number(10.0), &[JsValue::Number(2.0)])
                .unwrap(),
            JsValue::string("1010")
        );
    }
}
