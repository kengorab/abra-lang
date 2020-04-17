use crate::typechecker::types::Type;
use crate::vm::value::{Value, Obj};
use std::collections::HashMap;

pub trait NativeType {
    fn fields(typ: &Type) -> HashMap<&str, Type>;
    fn get_field_value(value: &Value, field_idx: usize) -> Value;
}

pub fn fields_for_type(typ: &Type) -> Option<HashMap<&str, Type>> {
    match &typ {
        Type::Array(_) => Some(NativeArray::fields(typ)),
        Type::String => Some(NativeString::fields(typ)),
        _ => None
    }
}

pub struct NativeArray {}

impl NativeType for NativeArray {
    fn fields(typ: &Type) -> HashMap<&str, Type> {
        if let Type::Array(_inner_type) = typ { // <- The inner type is to support any generics
            let mut fields = HashMap::new();
            fields.insert("length", Type::Int);
            fields
        } else {
            unreachable!()
        }
    }

    fn get_field_value(value: &Value, field_idx: usize) -> Value {
        if let Value::Obj(Obj::ArrayObj { value }) = value {
            match field_idx {
                0 => Value::Int(value.len() as i64),
                _ => unreachable!()
            }
        } else {
            unreachable!()
        }
    }
}

pub struct NativeString {}

impl NativeType for NativeString {
    fn fields(_typ: &Type) -> HashMap<&str, Type> {
        let mut fields = HashMap::new();
        fields.insert("length", Type::Int);
        fields
    }

    fn get_field_value(value: &Value, field_idx: usize) -> Value {
        if let Value::Obj(Obj::StringObj { value }) = value {
            match field_idx {
                0 => Value::Int(value.len() as i64),
                _ => unreachable!()
            }
        } else {
            unreachable!()
        }
    }
}
