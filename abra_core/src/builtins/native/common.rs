use crate::vm::vm::VM;
use crate::vm::value::Value;
use crate::typechecker::types::Type;

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
