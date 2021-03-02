use crate::builtins::native_value_trait::NativeValue;
use crate::builtins::native::{NativeArray, NativeMap, NativeSet, NativeString, NativeInt, NativeFloat, NativeDate};
use crate::builtins::native_fns::native_fns;
use crate::typechecker::types::Type;
use crate::vm::value::{Value, TypeValue};
use std::collections::HashMap;
use crate::typechecker::typechecker::{TypedModule, ExportedValue};
use crate::builtins::native_value_trait::NativeTyp;
use crate::parser::ast::ModuleId;
use crate::vm::compiler::Module;

#[derive(Debug, Clone)]
struct PreludeBinding {
    name: String,
    typ: Type,
    value: Value,
}

#[cfg(test)]
pub static PRELUDE_PRINTLN_INDEX: u8 = 0;
#[cfg(test)]
pub static PRELUDE_STRING_INDEX: u8 = 7;

pub struct Prelude;

impl Prelude {
    pub fn typed_module() -> TypedModule {
        let mut exports = HashMap::new();

        for (native_fn_desc, _) in native_fns() {
            let name = native_fn_desc.name.to_string();
            let typ = native_fn_desc.get_fn_type();

            exports.insert(name, ExportedValue::Binding(typ));
        }

        // Insert None
        exports.insert("None".to_string(), ExportedValue::Binding(Type::Option(Box::new(Type::Placeholder))));

        let prelude_types = vec![
            ("Int", Type::Int, None),
            ("Float", Type::Float, None),
            ("Bool", Type::Bool, None),
            ("String", Type::String, None),
            ("Unit", Type::Unit, None),
            ("Any", Type::Any, None),
            ("Array", Type::Array(Box::new(Type::Generic("T".to_string()))), Some(Type::Reference("Array".to_string(), vec![Type::Generic("T".to_string())]))),
            ("Map", Type::Map(Box::new(Type::Generic("K".to_string())), Box::new(Type::Generic("V".to_string()))), Some(Type::Reference("Map".to_string(), vec![Type::Generic("K".to_string()), Type::Generic("V".to_string())]))),
            ("Set", Type::Set(Box::new(Type::Generic("T".to_string()))), Some(Type::Reference("Set".to_string(), vec![Type::Generic("T".to_string())]))),
            ("Date", Type::Struct(NativeDate::get_type()), Some(Type::Reference("Date".to_string(), vec![]))),
        ];
        for (type_name, backing_type, reference) in prelude_types {
            exports.insert(type_name.to_string(), ExportedValue::Type { reference, backing_type, node: None });
        }

        TypedModule {
            module_id: ModuleId::from_name("prelude"),
            exports,
            ..TypedModule::default()
        }
    }

    pub fn compiled_module() -> (Module, HashMap<String, usize>) {
        let mut constants = Vec::new();
        let mut constant_names = Vec::new();

        for (native_fn_desc, native_fn) in native_fns() {
            let value = Value::NativeFn(native_fn);
            let name = native_fn_desc.name.to_string();

            constants.push(value);
            constant_names.push(name);
        }

        constants.push(Value::Nil);
        constant_names.push("None".to_string());

        let prelude_types = vec![
            ("Int", Some(NativeInt::get_type_value())),
            ("Float", Some(NativeFloat::get_type_value())),
            ("Bool", None),
            ("String", Some(NativeString::get_type_value())),
            ("Unit", None),
            ("Any", None),
            ("Array", Some(NativeArray::get_type_value())),
            ("Map", Some(NativeMap::get_type_value())),
            ("Set", Some(NativeSet::get_type_value())),
            ("Date", Some(NativeDate::get_type_value())),
        ];
        for (type_name, type_value) in prelude_types {
            let value = match type_value {
                Some(type_value) => Value::Type(type_value),
                None => Value::Type(TypeValue { name: type_name.to_string(), ..TypeValue::default() })
            };

            constants.push(value);
            constant_names.push(type_name.to_string());
        }

        let constant_indexes_by_ident = constant_names.into_iter()
            .enumerate()
            .map(|(idx, ident)| (ident, idx))
            .collect();

        let module = Module { name: "prelude".to_string(), constants, code: vec![] };
        (module, constant_indexes_by_ident)
    }
}
