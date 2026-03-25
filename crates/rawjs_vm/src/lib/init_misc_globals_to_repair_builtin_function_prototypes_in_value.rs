impl Vm {
    pub(super) fn init_misc_globals(
        &mut self,
        obj_proto: &GcPtr<JsObject>,
        function_proto: &GcPtr<JsObject>,
    ) {
        use rawjs_runtime::builtins;
        let promise_proto = builtins::create_promise_prototype(&mut self.heap);
        promise_proto
            .borrow_mut()
            .set_prototype(Some(obj_proto.clone()));
        self.promise_prototype = Some(promise_proto);
        let mut promise_ctor =
            JsObject::native_function("Promise", builtins::promise_constructor_placeholder);
        let resolve_fn = JsObject::native_function("resolve", builtins::promise_resolve_static);
        promise_ctor.define_property(
            "resolve".to_string(),
            rawjs_runtime::Property::builtin(JsValue::Object(self.heap.alloc(resolve_fn))),
        );
        let reject_fn = JsObject::native_function("reject", builtins::promise_reject_static);
        promise_ctor.define_property(
            "reject".to_string(),
            rawjs_runtime::Property::builtin(JsValue::Object(self.heap.alloc(reject_fn))),
        );
        let promise_ctor_ptr = self.heap.alloc(promise_ctor);
        promise_ctor_ptr
            .borrow_mut()
            .set_prototype(Some(function_proto.clone()));
        promise_ctor_ptr.borrow_mut().define_property(
            "prototype".to_string(),
            rawjs_runtime::Property::builtin(JsValue::Object(
                self.promise_prototype.as_ref().unwrap().clone(),
            )),
        );
        self.globals
            .insert("Promise".to_string(), JsValue::Object(promise_ctor_ptr));
        let gen_proto =
            rawjs_runtime::builtins::generator::create_generator_prototype(&mut self.heap);
        gen_proto
            .borrow_mut()
            .set_prototype(Some(obj_proto.clone()));
        self.generator_prototype = Some(gen_proto);
        for name in &[
            "Error",
            "TypeError",
            "ReferenceError",
            "SyntaxError",
            "RangeError",
        ] {
            let ctor = builtins::create_error_constructor(&mut self.heap, name);
            ctor.borrow_mut()
                .set_prototype(Some(function_proto.clone()));
            self.globals.insert(name.to_string(), JsValue::Object(ctor));
        }
        for (name, func_ptr) in builtins::create_global_functions(&mut self.heap) {
            func_ptr
                .borrow_mut()
                .set_prototype(Some(function_proto.clone()));
            self.globals
                .insert(name.to_string(), JsValue::Object(func_ptr));
        }
    }
}

impl Vm {
    pub(super) fn init_global_object(&mut self, obj_proto: GcPtr<JsObject>) {
        self.globals.insert(
            "__object_prototype__".to_string(),
            JsValue::Object(obj_proto),
        );
        let global_obj = self.heap.alloc(JsObject::ordinary());
        if let Some(ref proto) = self.object_prototype {
            global_obj.borrow_mut().set_prototype(Some(proto.clone()));
        }
        for (name, value) in self.globals.clone() {
            global_obj.borrow_mut().set_property(name, value);
        }
        global_obj.borrow_mut().set_property(
            "globalThis".to_string(),
            JsValue::Object(global_obj.clone()),
        );
        self.global_object = Some(global_obj);
    }
}

impl Vm {
    pub(super) fn repair_builtin_function_prototypes(&mut self) {
        let Some(function_proto) = self.function_prototype.clone() else {
            return;
        };
        let Some(object_proto) = self.object_prototype.clone() else {
            return;
        };
        let mut visited = HashSet::new();
        let mut roots: Vec<JsValue> = self.globals.values().cloned().collect();
        for ptr in [
            self.global_object.clone(),
            self.array_prototype.clone(),
            self.function_prototype.clone(),
            self.string_prototype.clone(),
            self.number_prototype.clone(),
            self.boolean_prototype.clone(),
            self.object_prototype.clone(),
            self.symbol_prototype.clone(),
            self.map_prototype.clone(),
            self.set_prototype.clone(),
            self.promise_prototype.clone(),
            self.generator_prototype.clone(),
        ]
        .into_iter()
        .flatten()
        {
            roots.push(JsValue::Object(ptr));
        }
        for root in roots {
            Self::repair_builtin_function_prototypes_in_value(
                &root,
                &function_proto,
                &object_proto,
                &mut visited,
            );
        }
    }
}

impl Vm {
    pub(super) fn repair_builtin_function_prototypes_in_value(
        value: &JsValue,
        function_proto: &GcPtr<JsObject>,
        object_proto: &GcPtr<JsObject>,
        visited: &mut HashSet<usize>,
    ) {
        if let JsValue::Object(ptr) = value {
            Self::repair_builtin_function_prototypes_in_object(
                ptr.clone(),
                function_proto,
                object_proto,
                visited,
            );
        }
    }
}

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
