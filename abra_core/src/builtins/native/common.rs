use crate::vm::vm::VM;
use crate::vm::value::{Value, FnValue, ClosureValue, TypeValue, EnumValue, Obj, EnumVariantObj};
use crate::typechecker::types::Type;
use crate::builtins::native_fns::NativeFn;
use itertools::Itertools;

pub trait NativeType {
    fn get_field_type(name: &str) -> Option<(usize, Type)>;
    fn get_method_type(name: &str) -> Option<(usize, Type)>;
    fn get_static_field_or_method(name: &str) -> Option<(usize, Type)>;
    fn get_field_value(obj: Box<Value>, field_idx: usize) -> Value;
    fn get_method_value(obj: Box<Value>, method_idx: usize) -> Value;
    fn get_static_field_values() -> Vec<(String, Value)>;

    fn get_field_idx(field_name: &str) -> usize {
        match Self::get_field_type(field_name) {
            Some((idx, _)) => idx,
            None => unreachable!()
        }
    }

    fn get_method_idx(field_name: &str) -> usize {
        match Self::get_method_type(field_name) {
            Some((idx, _)) => idx,
            None => unreachable!()
        }
    }

    fn get_field_or_method_type(field_name: &str) -> Option<(usize, Type, bool)> {
        Self::get_field_type(field_name)
            .map(|(idx, typ)| (idx, typ, false))
            .or_else(|| {
                Self::get_method_type(field_name)
                    .map(|(idx, typ)| (idx, typ, true))
            })
    }

    fn get_field_or_method_value(is_method: bool, inst: Box<Value>, idx: usize) -> Value {
        if is_method {
            Self::get_method_value(inst, idx)
        } else {
            Self::get_field_value(inst, idx)
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

pub fn default_to_string_method(receiver: Option<Value>, _args: Vec<Value>, vm: &mut VM) -> Option<Value> {
    let str_val = if let Value::Obj(obj) = receiver.unwrap() {
        match &*(obj.borrow()) {
            Obj::InstanceObj(obj) => {
                let type_name = &obj.typ.name;
                let field_names = &obj.typ.fields;
                let values = field_names.iter().zip(&obj.fields)
                    .map(|(field_name, field_value)| format!("{}: {}", field_name, to_string(field_value, vm)))
                    .join(", ");
                format!("{}({})", type_name, values)
            }
            _ => unreachable!()
        }
    } else { unreachable!() };

    Some(Value::new_string_obj(str_val))
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
                    let tostring_method_idx = o.typ.methods.iter()
                        .position(|(name, _)| name == "toString")
                        .map(|idx| idx);
                    let idx = tostring_method_idx.expect("Every instance should have at least the default toString method");
                    if let Value::Obj(o) = invoke_fn(vm, &o.methods[idx], vec![]) {
                        if let Obj::StringObj(s) = &*(o.borrow()) {
                            s.clone()
                        } else { unreachable!() }
                    } else { unreachable!() }
                }
                Obj::NativeInstanceObj(i) => {
                    let v = i.inst.method_to_string(vm);
                    if let Value::Obj(o) = v {
                        if let Obj::StringObj(s) = &*(o.borrow()) {
                            s.clone()
                        } else { unreachable!() }
                    } else { unreachable!() }
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
