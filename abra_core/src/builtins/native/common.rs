use crate::vm::vm::VM;
use crate::vm::value::Value;
use crate::typechecker::types::Type;

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
