use crate::typechecker::types::Type;
use crate::builtins::native_value_trait::NativeValue;
use crate::vm::value::{Value, TypeValue};
use crate::typechecker::typechecker::{TypedModule, ExportedValue};
use crate::vm::compiler::Module;
use crate::parser::ast::ModuleId;
use std::collections::HashMap;

#[derive(Debug)]
pub struct ModuleSpec {
    pub(crate) typed_module: TypedModule,
    pub(crate) compiled_module: Module,
    pub(crate) constant_names: Vec<String>,
}

#[derive(Default)]
pub struct ModuleSpecBuilder {
    name: String,
    exports: HashMap<String, ExportedValue>,
    referencable_types: HashMap<String, Type>,
    constants: Vec<Value>,
    constant_names: Vec<String>,
}

impl ModuleSpecBuilder {
    pub fn new(name: &str) -> Self {
        Self { name: name.to_string(), ..Self::default() }
    }

    pub fn build(self) -> ModuleSpec {
        let module_id = ModuleId::from_name(self.name.as_str());
        ModuleSpec {
            typed_module: TypedModule { module_id, exports: self.exports, referencable_types: self.referencable_types, ..TypedModule::default() },
            compiled_module: Module { name: self.name, is_native: true, num_globals: self.constants.len(), constants: self.constants, code: vec![] },
            constant_names: self.constant_names,
        }
    }

    pub fn add_binding(mut self, name: &str, typ: Type, value: Value) -> Self {
        self.exports.insert(name.to_string(), ExportedValue::Binding(typ));
        self.constant_names.push(name.to_string());
        self.constants.push(value);

        self
    }

    pub fn add_function(mut self, get_fn_spec: fn() -> (String, Type, Value)) -> Self {
        let (fn_name, fn_type, fn_value) = get_fn_spec();
        self.exports.insert(fn_name.clone(), ExportedValue::Binding(fn_type));
        self.constant_names.push(fn_name);
        self.constants.push(fn_value);

        self
    }

    pub fn add_type(mut self, type_spec: TypeSpec) -> Self {
        let TypeSpec { name, typ, reference_type, native_value } = type_spec;
        if reference_type.is_some() {
            self.referencable_types.insert(format!("{}/{}", &self.name, &name), typ.clone());
        }
        self.exports.insert(name.clone(), ExportedValue::Type { reference: reference_type, backing_type: typ, node: None });

        self.constant_names.push(name.clone());
        let type_value = match native_value {
            Some(type_value) => type_value,
            None => Value::Type(TypeValue { name, module_name: self.name.clone(), ..TypeValue::default() })
        };
        self.constants.push(type_value);

        self
    }

    pub fn add_type_impl<V: NativeValue>(mut self) -> Self {
        let (type_name, type_args, t) = if V::is_struct() {
            let t = V::get_struct_type();
            (t.name.clone(), t.type_args.clone(), Type::Struct(t))
        } else {
            let t = V::get_enum_type();
            (t.name.clone(), t.type_args.clone(), Type::Enum(t))
        };

        self.constant_names.push(type_name.clone());
        self.constants.push(V::get_type_value());

        self.referencable_types.insert(format!("{}/{}", &self.name, type_name), t.clone());
        let reference = Type::Reference(
            type_name.clone(),
            type_args.into_iter().map(|(_, t)| t).collect(),
        );
        self.exports.insert(
            type_name.clone(),
            ExportedValue::Type { reference: Some(reference), backing_type: t, node: None },
        );

        self
    }
}

pub struct TypeSpec {
    name: String,
    typ: Type,
    reference_type: Option<Type>,
    native_value: Option<Value>,
}

impl TypeSpec {
    pub fn builder(name: &str, typ: Type) -> Self {
        Self { name: name.to_string(), typ, reference_type: None, native_value: None }
    }

    pub fn with_typeref(mut self) -> Self {
        self.reference_type = Some(match &self.typ {
            Type::Array(t) => Type::Reference("Array".to_string(), vec![(**t).clone()]),
            Type::Map(k, v) => Type::Reference("Map".to_string(), vec![(**k).clone(), (**v).clone()]),
            Type::Set(t) => Type::Reference("Set".to_string(), vec![(**t).clone()]),
            _ => unimplemented!()
        });

        self
    }

    pub fn with_native_value<V: NativeValue>(mut self) -> Self {
        self.native_value = Some(V::get_type_value());
        self
    }
}
