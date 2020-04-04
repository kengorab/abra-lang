use crate::vm::value::Value;
use crate::typechecker::types::Type;
use crate::builtins::native_fns::{NATIVE_FNS, NativeFn};
use std::collections::HashMap;
use std::slice::Iter;

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

        let native_fns: Iter<NativeFn> = NATIVE_FNS.iter();
        for native_fn in native_fns {
            let native_fn = native_fn.clone();
            let name = native_fn.name.clone();
            let value = Value::NativeFn(native_fn.clone());

            let req_args = native_fn.args.iter().enumerate()
                .map(|(idx, arg)| (format!("_{}", idx), arg.clone(), false));
            let opt_args = native_fn.opt_args.iter().enumerate()
                .map(|(idx, arg)| (format!("_{}", idx + native_fn.args.len()), arg.clone(), true));
            let args = req_args.chain(opt_args).collect();
            let typ = Type::Fn(args, Box::new(native_fn.return_type));

            bindings.insert(name, PreludeBinding { typ, value });
        }

        let prelude_types = vec![
            ("Int", Type::Int),
            ("Float", Type::Float),
            ("Bool", Type::Bool),
            ("String", Type::String),
        ];
        for (type_name, typ) in prelude_types {
            let binding = PreludeBinding {
                typ: Type::Type(type_name.to_string(), Box::new(typ.clone())),
                value: Value::Type(type_name.to_string()),
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
