use crate::vm::vm::VM;
use crate::vm::value::{Value, FnValue, ClosureValue, TypeValue, EnumValue, Obj, EnumVariantObj};
use crate::typechecker::types::Type;
use crate::builtins::native_fns::NativeFn;
use itertools::Itertools;

pub trait NativeType {
    fn get_field_or_method(name: &str) -> Option<(usize, Type)>;
    fn get_static_field_or_method(name: &str) -> Option<(usize, Type)>;
    fn get_field_value(obj: Box<Value>, field_idx: usize) -> Value;
    fn get_static_field_values() -> Vec<(String, Value)>;

    fn get_field_idx(field_name: &str) -> usize {
        match Self::get_field_or_method(field_name) {
            Some((idx, _)) => idx,
            None => unreachable!()
        }
    }
}

pub fn invoke_fn(vm: &mut VM, fn_obj: &Value, args: Vec<Value>) -> Value {
    let res = vm.invoke_fn(args, fn_obj.clone());
    match res {
        Ok(v) => v.unwrap_or(Value::Nil),
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
            std::process::exit(1);
        }
    }
}

pub fn to_string(value: &Value, vm: &mut VM) -> String {
    match value {
        Value::Int(val) => format!("{}", val),
        Value::Float(val) => format!("{}", val),
        Value::Bool(val) => format!("{}", val),
        Value::Str(val) => val.clone(),
        Value::Obj(obj) => {
            match &*(obj.borrow()) {
                Obj::StringObj(value) => value.clone(),
                Obj::ArrayObj(value) => {
                    let items = value.iter()
                        .map(|v| to_string(v, vm))
                        .join(", ");
                    format!("[{}]", items)
                }
                Obj::SetObj(value) => {
                    let items = value.iter()
                        .map(|v| to_string(v, vm))
                        .join(", ");
                    format!("#{{{}}}", items)
                }
                Obj::TupleObj(value) => {
                    let items = value.iter()
                        .map(|v| to_string(v, vm))
                        .join(", ");
                    format!("({})", items)
                }
                Obj::MapObj(map) => {
                    let fields = map.iter()
                        .map(|(k, v)| {
                            let k = to_string(k, vm);
                            let v = to_string(v, vm);
                            format!("{}: {}", k, v)
                        })
                        .join(", ");
                    format!("{{ {} }}", fields)
                }
                Obj::InstanceObj(o) => {
                    let (to_string_method, type_name, fields) = match &*o.typ {
                        Value::Type(t) => {
                            let tostring_method_idx = t.methods.iter()
                                .position(|(name, _)| name == "toString")
                                .map(|idx| idx + t.fields.len());
                            (tostring_method_idx, &t.name, &t.fields)
                        }
                        _ => unreachable!()
                    };

                    if let Some(method_idx) = to_string_method {
                        let ret = invoke_fn(vm, &o.fields[method_idx], vec![]);
                        if let Value::Obj(o) = ret {
                            if let Obj::StringObj(s) = &*(o.borrow()) {
                                s.clone()
                            } else { unreachable!() }
                        } else { unreachable!() }
                    } else {
                        let values = fields.iter().zip(&o.fields)
                            .map(|(field_name, field_value)| format!("{}: {}", field_name, to_string(field_value, vm)))
                            .join(", ");
                        format!("{}({})", type_name, values)
                    }
                }
                Obj::EnumVariantObj(EnumVariantObj { enum_name, name, values, .. }) => {
                    match values {
                        None => format!("{}.{}", enum_name, name),
                        Some(values) => {
                            let values = values.iter()
                                .map(|v| to_string(v, vm))
                                .join(", ");
                            format!("{}.{}({})", enum_name, name, values)
                        }
                    }
                }
            }
        }
        Value::Fn(FnValue { name, .. }) |
        Value::Closure(ClosureValue { name, .. }) => format!("<func {}>", name),
        Value::NativeFn(NativeFn { name, .. }) => format!("<func {}>", name),
        Value::Type(TypeValue { name, .. }) => format!("<type {}>", name),
        Value::Enum(EnumValue { name, .. }) => format!("<enum {}>", name),
        Value::Nil => format!("None"),
    }
}
