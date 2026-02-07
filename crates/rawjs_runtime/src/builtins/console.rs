use rawjs_common::Result;

use crate::gc::{GcPtr, Heap};
use crate::object::JsObject;
use crate::value::JsValue;

use super::helpers::set_native;

pub fn create_console_object(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();

    set_native(&mut obj, "log", console_log);
    set_native(&mut obj, "warn", console_warn);
    set_native(&mut obj, "error", console_error);
    set_native(&mut obj, "info", console_log);
    set_native(&mut obj, "debug", console_log);

    heap.alloc(obj)
}

fn format_args(args: &[JsValue]) -> String {
    args.iter()
        .map(|v| v.to_string_value())
        .collect::<Vec<_>>()
        .join(" ")
}

fn console_log(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    println!("{}", format_args(args));
    Ok(JsValue::Undefined)
}

fn console_warn(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    eprintln!("warn: {}", format_args(args));
    Ok(JsValue::Undefined)
}

fn console_error(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    eprintln!("{}", format_args(args));
    Ok(JsValue::Undefined)
}
