use crate::vm::vm::VM;
use crate::vm::value::{Value, FnValue, ClosureValue, NativeFn, TypeValue, EnumValue};
use itertools::Itertools;

pub fn invoke_fn(vm: &mut VM, fn_obj: &Value, args: Vec<Value>) -> Value {
    let res = vm.invoke_fn(args, fn_obj.clone());
    match res {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
            std::process::exit(1);
        }
    }
}

pub fn default_to_string_method(receiver: Option<Value>, _args: Vec<Value>, vm: &mut VM) -> Value {
    let rcv = receiver.unwrap();
    let str_val = match rcv {
        Value::InstanceObj(rcv) => {
            let obj = &*rcv.borrow();
            let type_value = vm.load_type(obj.type_id);
            let type_name = type_value.name.clone();
            let field_names = type_value.fields.clone();
            let values = field_names.iter().zip(&obj.fields)
                .map(|(field_name, field_value)| format!("{}: {}", field_name, to_string(field_value, vm)))
                .join(", ");
            format!("{}({})", type_name, values)
        }
        Value::EnumInstanceObj(rcv) => {
            let obj = &*rcv.borrow();
            let enum_value = vm.load_enum(obj.type_id);
            let enum_name = enum_value.name.clone();
            let variant_name = enum_value.variants[obj.idx].0.clone();

            match &obj.values {
                None => format!("{}.{}", enum_name, variant_name),
                Some(values) => {
                    let values = values.iter().map(|v| to_string(v, vm)).join(", ");
                    format!("{}.{}({})", enum_name, variant_name, values)
                }
            }
        }
        _ => unimplemented!("The default toString implementation should only ever be attached to instances of types/enums")
    };

    Value::new_string_obj(str_val)
}

pub fn to_string(value: &Value, vm: &mut VM) -> String {
    match value {
        Value::Int(val) => format!("{}", val),
        Value::Float(val) => format!("{}", val),
        Value::Bool(val) => format!("{}", val),
        Value::StringObj(o) => {
            let str = &*o.borrow()._inner;
            format!("{}", str)
        }
        Value::ArrayObj(o) => {
            let arr = &*o.borrow();
            let items = arr._inner.iter().map(|v| to_string(v, vm)).join(", ");
            format!("[{}]", items)
        }
        Value::TupleObj(o) => {
            let tup = &*o.borrow();
            let items = tup.iter().map(|v| to_string(v, vm)).join(", ");
            format!("({})", items)
        }
        Value::SetObj(o) => {
            let set = &*o.borrow();
            let items = set._inner.iter().map(|v| to_string(v, vm)).join(", ");
            format!("#{{{}}}", items)
        }
        Value::MapObj(o) => {
            let map = &*o.borrow();
            if map._inner.is_empty() {
                return format!("{{}}");
            }

            let fields = map._inner.iter()
                .map(|(k, v)| {
                    let k = to_string(k, vm);
                    let v = to_string(v, vm);
                    format!("{}: {}", k, v)
                })
                .join(", ");
            format!("{{ {} }}", fields)
        }
        Value::InstanceObj(o) => {
            let o = &*o.borrow();

            let type_value = vm.load_type(o.type_id);
            let mut tostring_method = type_value.methods.iter()
                .find(|(name, _)| name == "toString")
                .map(|(_, m)| m)
                .expect("Every instance should have at least the default toString method")
                .clone();
            tostring_method.bind_fn_value(value.clone());

            let ret = invoke_fn(vm, &tostring_method, vec![]);
            let ret = &*ret.as_string().borrow();
            ret._inner.clone()
        }
        Value::NativeInstanceObj(o) => {
            let i = &*o.borrow();
            let v = i.inst.method_to_string(vm);
            let v = &*v.as_string().borrow();
            v._inner.clone()
        }
        Value::EnumInstanceObj(o) => {
            let o = &*o.borrow();
            let enum_value = vm.load_enum(o.type_id);
            let mut tostring_method = enum_value.methods.iter()
                .find(|(name, _)| name == "toString")
                .map(|(_, m)| m)
                .expect("Every instance should have at least the default toString method")
                .clone();
            tostring_method.bind_fn_value(value.clone());
            let ret = invoke_fn(vm, &tostring_method, vec![]);
            let ret = &*ret.as_string().borrow();
            ret._inner.clone()
        }
        Value::NativeEnumInstanceObj(o) => {
            let i = &*o.borrow();
            let v = i.inst.method_to_string(vm);
            let v = &*v.as_string().borrow();
            v._inner.clone()
        }
        Value::Fn(FnValue { name, .. }) |
        Value::Closure(ClosureValue { name, .. }) => format!("<func {}>", name),
        Value::NativeFn(NativeFn { name, .. }) => format!("<func {}>", name),
        Value::Type(TypeValue { name, .. }) => format!("<type {}>", name),
        Value::Enum(EnumValue { name, .. }) => format!("<enum {}>", name),
        Value::Module(module_id) => format!("<module {}>", module_id.get_name()),
        Value::Nil => format!("None"),
    }
}
