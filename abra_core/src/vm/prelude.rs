use crate::vm::value::{Value, TypeValue};
use crate::typechecker::types::Type;
use crate::builtins::native_fns::native_fns;
use std::collections::HashMap;

#[derive(Clone)]
struct PreludeBinding {
    typ: Type,
    value: Value,
}

pub struct Prelude {
    bindings: HashMap<String, PreludeBinding>,
    typedefs: HashMap<String, Type>,
}

impl Prelude {
    pub fn new() -> Self {
        let mut bindings = HashMap::new();
        let mut typedefs = HashMap::new();

        for (native_fn_desc, native_fn) in native_fns() {
            let value = Value::NativeFn(native_fn);

            let name = native_fn_desc.name.to_string();
            let typ = native_fn_desc.get_fn_type();

            bindings.insert(name, PreludeBinding { typ, value });
        }

        // Insert None
        bindings.insert("None".to_string(), PreludeBinding { typ: Type::Option(Box::new(Type::Placeholder)), value: Value::Nil });

        let prelude_types = vec![
            ("Int", Type::Int),
            ("Float", Type::Float),
            ("Bool", Type::Bool),
            ("String", Type::String),
        ];
        for (type_name, typ) in prelude_types {
            let binding = PreludeBinding {
                typ: Type::Type(type_name.to_string(), Box::new(typ.clone())),
                value: Value::Type(TypeValue {
                    name: type_name.to_string(),
                    methods: vec![],
                    static_fields: vec![],
                }),
            };
            bindings.insert(type_name.to_string(), binding);

            typedefs.insert(type_name.to_string(), typ);
        }

        Self { bindings, typedefs }
    }

    pub fn get_binding_types(&self) -> HashMap<String, Type> {
        self.bindings.iter()
            .map(|(name, binding)| (name.clone(), binding.typ.clone()))
            .collect()
    }

    pub fn get_typedefs(&self) -> HashMap<String, Type> {
        self.typedefs.iter()
            .map(|(name, typ)| (name.clone(), typ.clone()))
            .collect()
    }

    pub fn resolve_ident<S: AsRef<str>>(&self, ident: S) -> Option<Value> {
        match self.bindings.get(ident.as_ref()) {
            None => None,
            Some(PreludeBinding { value, .. }) => Some(value.clone()),
        }
    }
}
