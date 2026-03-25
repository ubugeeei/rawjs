impl Vm {
    fn init_number_global(
        &mut self,
        obj_proto: &GcPtr<JsObject>,
        function_proto: &GcPtr<JsObject>,
    ) {
        use rawjs_runtime::builtins;
        let number_proto = builtins::create_number_prototype(&mut self.heap);
        number_proto
            .borrow_mut()
            .set_prototype(Some(obj_proto.clone()));
        self.number_prototype = Some(number_proto);
        let number_ctor = self.heap.alloc(JsObject::native_function(
            "Number",
            builtins::number_constructor,
        ));
        {
            let mut ctor = number_ctor.borrow_mut();
            ctor.set_prototype(Some(function_proto.clone()));
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(
                    self.number_prototype.as_ref().unwrap().clone(),
                )),
            );
            ctor.define_property(
                "NaN".to_string(),
                rawjs_runtime::Property::readonly_builtin(JsValue::Number(f64::NAN)),
            );
            ctor.define_property(
                "POSITIVE_INFINITY".to_string(),
                rawjs_runtime::Property::readonly_builtin(JsValue::Number(f64::INFINITY)),
            );
            ctor.define_property(
                "NEGATIVE_INFINITY".to_string(),
                rawjs_runtime::Property::readonly_builtin(JsValue::Number(f64::NEG_INFINITY)),
            );
            ctor.define_property(
                "MAX_VALUE".to_string(),
                rawjs_runtime::Property::readonly_builtin(JsValue::Number(f64::MAX)),
            );
            ctor.define_property(
                "MIN_VALUE".to_string(),
                rawjs_runtime::Property::readonly_builtin(JsValue::Number(f64::MIN_POSITIVE)),
            );
        }
        self.number_prototype
            .as_ref()
            .unwrap()
            .borrow_mut()
            .define_property(
                "constructor".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(number_ctor.clone())),
            );
        self.globals
            .insert("Number".to_string(), JsValue::Object(number_ctor));
    }
}

impl Vm {
    fn init_collection_and_symbol_globals(
        &mut self,
        obj_proto: &GcPtr<JsObject>,
        function_proto: &GcPtr<JsObject>,
    ) {
        use rawjs_runtime::builtins;
        let mut symbol_fn = JsObject::native_function("Symbol", builtins::symbol_call);
        let symbol_obj = builtins::create_symbol_constructor(&mut self.heap);
        for (key, prop) in symbol_obj.borrow().properties.iter() {
            symbol_fn.define_property(key.clone(), prop.clone());
        }
        let symbol_ptr = self.heap.alloc(symbol_fn);
        symbol_ptr
            .borrow_mut()
            .set_prototype(Some(function_proto.clone()));
        self.globals
            .insert("Symbol".to_string(), JsValue::Object(symbol_ptr.clone()));
        let symbol_proto = builtins::create_symbol_prototype(&mut self.heap);
        symbol_proto
            .borrow_mut()
            .set_prototype(Some(obj_proto.clone()));
        symbol_ptr.borrow_mut().define_property(
            "prototype".to_string(),
            rawjs_runtime::Property::builtin(JsValue::Object(symbol_proto.clone())),
        );
        self.symbol_prototype = Some(symbol_proto);
        let map_proto = builtins::create_map_prototype(&mut self.heap);
        map_proto
            .borrow_mut()
            .set_prototype(Some(obj_proto.clone()));
        self.map_prototype = Some(map_proto);
        let map_ctor = self
            .heap
            .alloc(JsObject::native_function("Map", builtins::map_constructor));
        map_ctor
            .borrow_mut()
            .set_prototype(Some(function_proto.clone()));
        map_ctor.borrow_mut().define_property(
            "prototype".to_string(),
            rawjs_runtime::Property::builtin(JsValue::Object(
                self.map_prototype.as_ref().unwrap().clone(),
            )),
        );
        self.globals
            .insert("Map".to_string(), JsValue::Object(map_ctor));
        let set_proto = builtins::create_set_prototype(&mut self.heap);
        set_proto
            .borrow_mut()
            .set_prototype(Some(obj_proto.clone()));
        self.set_prototype = Some(set_proto);
        let set_ctor = self
            .heap
            .alloc(JsObject::native_function("Set", builtins::set_constructor));
        set_ctor
            .borrow_mut()
            .set_prototype(Some(function_proto.clone()));
        set_ctor.borrow_mut().define_property(
            "prototype".to_string(),
            rawjs_runtime::Property::builtin(JsValue::Object(
                self.set_prototype.as_ref().unwrap().clone(),
            )),
        );
        self.globals
            .insert("Set".to_string(), JsValue::Object(set_ctor));
    }
}
