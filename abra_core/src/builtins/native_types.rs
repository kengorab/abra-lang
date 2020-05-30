use crate::typechecker::types::Type;
use crate::vm::value::{Value, Obj, StringObj};
use std::sync::Arc;
use std::cell::RefCell;

pub trait NativeType {
    const FIELDS: &'static [(&'static str, Type)];

    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value;

    fn get_field_idx(field_name: &str) -> usize {
        match Self::get_field(field_name) {
            Some((idx, _)) => idx,
            None => unreachable!()
        }
    }

    fn get_field(field_name: &str) -> Option<(usize, &'static (&'static str, Type))> {
        Self::FIELDS.iter().enumerate().find(|(_, (name, _))| name == &field_name)
    }
}

pub fn field_for_type(typ: &Type, field_name: &str) -> Option<(usize, &'static (&'static str, Type))> {
    match &typ {
        Type::Array(_) => NativeArray::get_field(field_name),
        Type::String => NativeString::get_field(field_name),
        _ => None
    }
}

// TODO: This could stand to be cleaned up a bit, esp to handle generic field types
pub struct NativeArray {}

impl NativeType for NativeArray {
    const FIELDS: &'static [(&'static str, Type)] = &[
        ("length", Type::Int)
    ];

    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value {
        match &*obj.borrow() {
            Obj::ArrayObj { value } => {
                match field_idx {
                    0 => Value::Int(value.len() as i64),
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }
}

pub struct NativeString {}

impl NativeType for NativeString {
    const FIELDS: &'static [(&'static str, Type)] = &[
        ("length", Type::Int)
    ];

    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value {
        match &*obj.borrow() {
            Obj::StringObj(StringObj { value, .. }) => {
                match field_idx {
                    0 => Value::Int(value.len() as i64),
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }
}
