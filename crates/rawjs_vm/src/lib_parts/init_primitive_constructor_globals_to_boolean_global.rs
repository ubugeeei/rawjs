impl Vm {
    fn init_primitive_constructor_globals(
        &mut self,
        obj_proto: &GcPtr<JsObject>,
        function_proto: &GcPtr<JsObject>,
    ) {
        use rawjs_runtime::builtins;
        let array_proto = builtins::create_array_prototype(&mut self.heap);
        array_proto
            .borrow_mut()
            .set_prototype(Some(obj_proto.clone()));
        self.array_prototype = Some(array_proto);
        let array_ctor = self.heap.alloc(JsObject::native_function(
            "Array",
            builtins::array_constructor,
        ));
        {
            let mut ctor = array_ctor.borrow_mut();
            ctor.set_prototype(Some(function_proto.clone()));
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(
                    self.array_prototype.as_ref().unwrap().clone(),
                )),
            );
            ctor.define_property(
                "isArray".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(self.heap.alloc(
                    JsObject::native_function("isArray", builtins::array_is_array),
                ))),
            );
        }
        self.globals
            .insert("Array".to_string(), JsValue::Object(array_ctor));
        self.init_string_global(obj_proto, function_proto);
        self.init_boolean_global(obj_proto, function_proto);
        self.init_number_global(obj_proto, function_proto);
    }
}

impl Vm {
    fn init_string_global(
        &mut self,
        obj_proto: &GcPtr<JsObject>,
        function_proto: &GcPtr<JsObject>,
    ) {
        use rawjs_runtime::builtins;
        let string_proto = builtins::create_string_prototype(&mut self.heap);
        string_proto
            .borrow_mut()
            .set_prototype(Some(obj_proto.clone()));
        self.string_prototype = Some(string_proto);
        let string_ctor = self.heap.alloc(JsObject::native_function(
            "String",
            builtins::string_constructor,
        ));
        {
            let mut ctor = string_ctor.borrow_mut();
            ctor.set_prototype(Some(function_proto.clone()));
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(
                    self.string_prototype.as_ref().unwrap().clone(),
                )),
            );
        }
        self.string_prototype
            .as_ref()
            .unwrap()
            .borrow_mut()
            .define_property(
                "constructor".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(string_ctor.clone())),
            );
        self.globals
            .insert("String".to_string(), JsValue::Object(string_ctor));
    }
}

impl Vm {
    fn init_boolean_global(
        &mut self,
        obj_proto: &GcPtr<JsObject>,
        function_proto: &GcPtr<JsObject>,
    ) {
        use rawjs_runtime::builtins;
        let boolean_proto = builtins::create_boolean_prototype(&mut self.heap);
        boolean_proto
            .borrow_mut()
            .set_prototype(Some(obj_proto.clone()));
        self.boolean_prototype = Some(boolean_proto);
        let boolean_ctor = self.heap.alloc(JsObject::native_function(
            "Boolean",
            builtins::boolean_constructor,
        ));
        {
            let mut ctor = boolean_ctor.borrow_mut();
            ctor.set_prototype(Some(function_proto.clone()));
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(
                    self.boolean_prototype.as_ref().unwrap().clone(),
                )),
            );
        }
        self.boolean_prototype
            .as_ref()
            .unwrap()
            .borrow_mut()
            .define_property(
                "constructor".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(boolean_ctor.clone())),
            );
        self.globals
            .insert("Boolean".to_string(), JsValue::Object(boolean_ctor));
    }
}
