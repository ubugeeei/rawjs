use rawjs_common::Result;

use crate::gc::{GcPtr, Heap};
use crate::object::JsObject;
use crate::value::{
    JsSymbol, JsValue, SYMBOL_ASYNC_DISPOSE, SYMBOL_DISPOSE, SYMBOL_HAS_INSTANCE, SYMBOL_ITERATOR,
    SYMBOL_SPECIES, SYMBOL_TO_PRIMITIVE, SYMBOL_TO_STRING_TAG,
};

use super::helpers::set_native;

// ============================================================================
// Symbol constructor (called as a function, NOT with `new`)
// ============================================================================

pub fn create_symbol_constructor(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();

    // Symbol() itself is a native function
    set_native(&mut obj, "for", symbol_for);
    set_native(&mut obj, "keyFor", symbol_key_for);

    // Well-known symbols as properties
    obj.set_property(
        "iterator".to_string(),
        JsValue::Symbol(JsSymbol::well_known(SYMBOL_ITERATOR, "Symbol.iterator")),
    );
    obj.set_property(
        "toPrimitive".to_string(),
        JsValue::Symbol(JsSymbol::well_known(
            SYMBOL_TO_PRIMITIVE,
            "Symbol.toPrimitive",
        )),
    );
    obj.set_property(
        "hasInstance".to_string(),
        JsValue::Symbol(JsSymbol::well_known(
            SYMBOL_HAS_INSTANCE,
            "Symbol.hasInstance",
        )),
    );
    obj.set_property(
        "toStringTag".to_string(),
        JsValue::Symbol(JsSymbol::well_known(
            SYMBOL_TO_STRING_TAG,
            "Symbol.toStringTag",
        )),
    );
    obj.set_property(
        "species".to_string(),
        JsValue::Symbol(JsSymbol::well_known(SYMBOL_SPECIES, "Symbol.species")),
    );
    obj.set_property(
        "dispose".to_string(),
        JsValue::Symbol(JsSymbol::well_known(SYMBOL_DISPOSE, "Symbol.dispose")),
    );
    obj.set_property(
        "asyncDispose".to_string(),
        JsValue::Symbol(JsSymbol::well_known(
            SYMBOL_ASYNC_DISPOSE,
            "Symbol.asyncDispose",
        )),
    );

    heap.alloc(obj)
}

/// `Symbol(description?)` — create a new unique symbol.
pub fn symbol_call(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let description = match args.first() {
        Some(JsValue::Undefined) | None => None,
        Some(val) => Some(val.to_string_value()),
    };
    Ok(JsValue::Symbol(JsSymbol::new(description.as_deref())))
}

/// `Symbol.for(key)` — look up or create a symbol in the global registry.
/// Note: The global registry is managed by the VM, so this is a simplified stub
/// that always creates a new symbol. Full implementation needs VM access.
fn symbol_for(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    let key = args
        .first()
        .unwrap_or(&JsValue::Undefined)
        .to_string_value();
    // Simplified: create a new symbol with the key as description.
    // A proper implementation would use a global registry in the VM.
    Ok(JsValue::Symbol(JsSymbol::new(Some(&key))))
}

/// `Symbol.keyFor(sym)` — look up the key for a symbol in the global registry.
fn symbol_key_for(_heap: &mut Heap, _this: &JsValue, args: &[JsValue]) -> Result<JsValue> {
    match args.first() {
        Some(JsValue::Symbol(sym)) => {
            // Simplified: return the description as the key.
            match &sym.description {
                Some(desc) => Ok(JsValue::string(desc.as_ref())),
                None => Ok(JsValue::Undefined),
            }
        }
        _ => Err(rawjs_common::RawJsError::type_error(
            "Symbol.keyFor requires a symbol argument",
        )),
    }
}

// ============================================================================
// Symbol.prototype methods
// ============================================================================

pub fn create_symbol_prototype(heap: &mut Heap) -> GcPtr<JsObject> {
    let mut obj = JsObject::ordinary();

    set_native(&mut obj, "toString", symbol_to_string);
    set_native(&mut obj, "valueOf", symbol_value_of);
    set_native(&mut obj, "description", symbol_description);

    heap.alloc(obj)
}

fn symbol_to_string(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Symbol(sym) => {
            if let Some(ref desc) = sym.description {
                Ok(JsValue::string(format!("Symbol({})", desc).as_str()))
            } else {
                Ok(JsValue::string("Symbol()"))
            }
        }
        _ => Err(rawjs_common::RawJsError::type_error(
            "Symbol.prototype.toString requires a symbol",
        )),
    }
}

fn symbol_value_of(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Symbol(_) => Ok(this.clone()),
        _ => Err(rawjs_common::RawJsError::type_error(
            "Symbol.prototype.valueOf requires a symbol",
        )),
    }
}

fn symbol_description(_heap: &mut Heap, this: &JsValue, _args: &[JsValue]) -> Result<JsValue> {
    match this {
        JsValue::Symbol(sym) => match &sym.description {
            Some(desc) => Ok(JsValue::string(desc.as_ref())),
            None => Ok(JsValue::Undefined),
        },
        _ => Ok(JsValue::Undefined),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_unique() {
        let s1 = JsSymbol::new(Some("test"));
        let s2 = JsSymbol::new(Some("test"));
        assert_ne!(s1.id, s2.id);
    }

    #[test]
    fn test_symbol_equality() {
        let s1 = JsSymbol::new(Some("foo"));
        let s2 = s1.clone();
        assert_eq!(s1, s2);
    }

    #[test]
    fn test_well_known_symbols() {
        let iter = JsSymbol::well_known(SYMBOL_ITERATOR, "Symbol.iterator");
        assert_eq!(iter.id, SYMBOL_ITERATOR);
        assert_eq!(iter.description.as_deref(), Some("Symbol.iterator"));
    }

    #[test]
    fn test_symbol_call() {
        let mut heap = Heap::new();
        let result =
            symbol_call(&mut heap, &JsValue::Undefined, &[JsValue::string("test")]).unwrap();
        assert!(matches!(result, JsValue::Symbol(_)));
        if let JsValue::Symbol(sym) = result {
            assert_eq!(sym.description.as_deref(), Some("test"));
        }
    }

    #[test]
    fn test_symbol_typeof() {
        let sym = JsValue::Symbol(JsSymbol::new(None));
        assert_eq!(sym.type_of(), "symbol");
    }

    #[test]
    fn test_symbol_to_boolean() {
        let sym = JsValue::Symbol(JsSymbol::new(None));
        assert!(sym.to_boolean());
    }
}
