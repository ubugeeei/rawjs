impl Vm {
    #[doc = " Create a new VM with built-in globals pre-populated."]
    pub fn new() -> Self {
        let mut vm = Vm {
            heap: Heap::new(),
            globals: HashMap::new(),
            global_object: None,
            chunks: Vec::new(),
            call_stack: Vec::new(),
            value_stack: Vec::new(),
            try_stack: Vec::new(),
            construct_frames: Vec::new(),
            jit_cache: HashMap::new(),
            execution_counts: HashMap::new(),
            thrown_value: None,
            jit_error: None,
            array_prototype: None,
            function_prototype: None,
            string_prototype: None,
            number_prototype: None,
            boolean_prototype: None,
            object_prototype: None,
            symbol_prototype: None,
            map_prototype: None,
            set_prototype: None,
            promise_prototype: None,
            generator_prototype: None,
            module_cache: HashMap::new(),
            current_file_dir: None,
            current_file_path: None,
            module_exports: None,
        };
        vm.init_globals();
        vm
    }
}

impl Vm {
    #[doc = " Register built-in global bindings."]
    fn init_globals(&mut self) {
        let obj_proto = self.init_core_globals();
        let function_proto = self.init_function_and_object_globals(&obj_proto);
        self.init_host_globals(&obj_proto);
        self.init_primitive_constructor_globals(&obj_proto, &function_proto);
        self.init_collection_and_symbol_globals(&obj_proto, &function_proto);
        self.init_misc_globals(&obj_proto, &function_proto);
        self.init_global_object(obj_proto);
        self.repair_builtin_function_prototypes();
    }
}

impl Vm {
    fn init_core_globals(&mut self) -> GcPtr<JsObject> {
        use rawjs_runtime::builtins;
        self.globals
            .insert("undefined".to_string(), JsValue::Undefined);
        self.globals
            .insert("NaN".to_string(), JsValue::Number(f64::NAN));
        self.globals
            .insert("Infinity".to_string(), JsValue::Number(f64::INFINITY));
        let obj_proto = builtins::create_object_prototype(&mut self.heap);
        self.object_prototype = Some(obj_proto.clone());
        obj_proto
    }
}

impl Vm {
    fn init_function_and_object_globals(&mut self, obj_proto: &GcPtr<JsObject>) -> GcPtr<JsObject> {
        use rawjs_runtime::builtins;
        let function_proto = builtins::create_function_prototype(&mut self.heap);
        function_proto.borrow_mut().prototype = Some(obj_proto.clone());
        self.function_prototype = Some(function_proto.clone());
        let function_ctor = self.heap.alloc(JsObject::native_function(
            "Function",
            builtins::function_constructor_placeholder,
        ));
        {
            let mut ctor = function_ctor.borrow_mut();
            ctor.prototype = Some(obj_proto.clone());
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(function_proto.clone())),
            );
        }
        function_proto.borrow_mut().define_property(
            "constructor".to_string(),
            rawjs_runtime::Property::builtin(JsValue::Object(function_ctor.clone())),
        );
        self.globals
            .insert("Function".to_string(), JsValue::Object(function_ctor));
        let object_ctor = builtins::create_object_constructor(&mut self.heap);
        {
            let mut ctor = object_ctor.borrow_mut();
            ctor.prototype = Some(function_proto.clone());
            ctor.define_property(
                "prototype".to_string(),
                rawjs_runtime::Property::builtin(JsValue::Object(obj_proto.clone())),
            );
        }
        obj_proto.borrow_mut().define_property(
            "constructor".to_string(),
            rawjs_runtime::Property::builtin(JsValue::Object(object_ctor.clone())),
        );
        self.globals
            .insert("Object".to_string(), JsValue::Object(object_ctor));
        function_proto
    }
}

impl Vm {
    fn init_host_globals(&mut self, obj_proto: &GcPtr<JsObject>) {
        use rawjs_runtime::builtins;
        let console_ptr = builtins::create_console_object(&mut self.heap);
        console_ptr.borrow_mut().prototype = Some(obj_proto.clone());
        self.globals
            .insert("console".to_string(), JsValue::Object(console_ptr));
        let math_ptr = builtins::create_math_object(&mut self.heap);
        math_ptr.borrow_mut().prototype = Some(obj_proto.clone());
        self.globals
            .insert("Math".to_string(), JsValue::Object(math_ptr));
        let json_ptr = builtins::create_json_object(&mut self.heap);
        json_ptr.borrow_mut().prototype = Some(obj_proto.clone());
        self.globals
            .insert("JSON".to_string(), JsValue::Object(json_ptr));
    }
}
