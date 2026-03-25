impl Vm {
    fn repair_builtin_function_prototypes_in_object(
        ptr: GcPtr<JsObject>,
        function_proto: &GcPtr<JsObject>,
        object_proto: &GcPtr<JsObject>,
        visited: &mut HashSet<usize>,
    ) {
        if !visited.insert(ptr.addr()) {
            return;
        }
        let mut nested_values = Vec::new();
        let mut constructor_target = None;
        {
            let mut obj = ptr.borrow_mut();
            if matches!(&obj.internal, ObjectInternal::Function(_)) {
                if obj.prototype.is_none() {
                    obj.prototype = Some(function_proto.clone());
                }
                if let Some(prop) = obj.properties.get("prototype") {
                    if let JsValue::Object(proto_obj) = &prop.value {
                        constructor_target = Some(proto_obj.clone());
                    }
                }
            }
            if let Some(proto) = obj.prototype.clone() {
                nested_values.push(JsValue::Object(proto));
            }
            for prop in obj.properties.values() {
                nested_values.push(prop.value.clone());
                if let Some(getter) = &prop.get {
                    nested_values.push(getter.clone());
                }
                if let Some(setter) = &prop.set {
                    nested_values.push(setter.clone());
                }
            }
            for prop in obj.symbol_properties.values() {
                nested_values.push(prop.value.clone());
                if let Some(getter) = &prop.get {
                    nested_values.push(getter.clone());
                }
                if let Some(setter) = &prop.set {
                    nested_values.push(setter.clone());
                }
            }
        }
        if let Some(proto_obj) = constructor_target {
            let needs_proto = proto_obj.borrow().prototype.is_none();
            if needs_proto && !proto_obj.ptr_eq(object_proto) {
                proto_obj.borrow_mut().prototype = Some(object_proto.clone());
            }
            let has_constructor = proto_obj.borrow().has_own_property("constructor");
            if !has_constructor {
                proto_obj.borrow_mut().define_property(
                    "constructor".to_string(),
                    Property::builtin(JsValue::Object(ptr.clone())),
                );
            }
        }
        for nested in nested_values {
            Self::repair_builtin_function_prototypes_in_value(
                &nested,
                function_proto,
                object_proto,
                visited,
            );
        }
    }
}

impl Vm {
    #[doc = " Register a chunk in the VM and return its index."]
    pub fn add_chunk(&mut self, chunk: Chunk) -> usize {
        let idx = self.chunks.len();
        self.chunks.push(chunk);
        idx
    }
}

impl Vm {
    #[doc = " Borrow a chunk by index."]
    #[allow(dead_code)]
    pub(crate) fn chunk(&self, index: usize) -> &Chunk {
        &self.chunks[index]
    }
}

impl Vm {
    #[doc = " Execute a top-level chunk."]
    #[doc = ""]
    #[doc = " Creates an initial call frame, runs the interpreter loop, and returns"]
    #[doc = " the final value left on the stack (or `undefined`)."]
    pub fn execute(&mut self, chunk: Chunk) -> Result<JsValue> {
        let chunk_index = self.add_chunk(chunk);
        if self.chunks[chunk_index].is_async {
            return interpreter::execute_async_top_level(self, chunk_index);
        }
        let local_count = self.chunks[chunk_index].local_count as usize;
        let frame = CallFrame {
            chunk_index,
            ip: 0,
            base: self.value_stack.len(),
            locals: vec![JsValue::Undefined; local_count],
            arguments: Vec::new(),
            arguments_object: None,
            callee: None,
            is_strict: self.chunks[chunk_index].is_strict,
            upvalues: Vec::new(),
            this_value: self.global_this_value(),
        };
        self.call_stack.push(frame);
        self.run()
    }
}

impl Vm {
    #[doc = " Main execution loop -- delegates to the interpreter."]
    pub fn run(&mut self) -> Result<JsValue> {
        interpreter::run(self)
    }
}

impl Vm {
    #[doc = " Get a global variable by name."]
    pub fn get_global(&self, name: &str) -> Option<&JsValue> {
        self.globals.get(name)
    }
}

impl Vm {
    #[doc = " Set (or create) a global variable."]
    pub fn set_global(&mut self, name: String, value: JsValue) {
        self.globals.insert(name.clone(), value.clone());
        if let Some(ref global_obj) = self.global_object {
            global_obj.borrow_mut().set_property(name, value);
        }
    }
}

impl Vm {
    #[doc = " Delete a global variable. Returns `true` if it existed."]
    pub(crate) fn delete_global(&mut self, name: &str) -> bool {
        let existed = self.globals.remove(name).is_some();
        if let Some(ref global_obj) = self.global_object {
            global_obj.borrow_mut().delete_property(name);
        }
        existed
    }
}

impl Vm {
    #[doc = " Get the current global `this` value."]
    pub(crate) fn global_this_value(&self) -> JsValue {
        self.global_object
            .as_ref()
            .map(|ptr| JsValue::Object(ptr.clone()))
            .unwrap_or(JsValue::Undefined)
    }
}
