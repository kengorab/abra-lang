use crate::typechecker::types::Type;
use crate::vm::value::{Value, Obj};
use crate::builtins::native_fns::NativeFn;
use crate::vm::vm::VMContext;
use std::sync::Arc;
use std::cell::RefCell;

pub trait NativeType {
    fn fields(typ: &Type) -> Vec<(&'static str, Type)>;
    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value;

    fn get_field_idx(typ: &Type, field_name: &str) -> usize {
        match Self::get_field(typ, field_name) {
            Some((idx, _)) => idx,
            None => unreachable!()
        }
    }

    fn get_field(typ: &Type, field_name: &str) -> Option<(usize, (&'static str, Type))> {
        Self::fields(typ).into_iter().enumerate().find(|(_, (name, _))| name == &field_name)
    }
}

pub fn field_for_type(typ: &Type, field_name: &str) -> Option<(usize, (&'static str, Type))> {
    match &typ {
        Type::Array(_) => NativeArray::get_field(typ, field_name),
        Type::String => NativeString::get_field(typ, field_name),
        _ => None
    }
}

// TODO: This could stand to be cleaned up a bit, esp to handle generic field types
pub struct NativeArray;

impl NativeArray {
    fn push(_ctx: &VMContext, receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>) -> Option<Value> {
        let item = args.into_iter().next().expect("Array::push requires 1 argument");

        let mut receiver = receiver.as_ref().unwrap().borrow_mut();
        match *receiver {
            Obj::ArrayObj(ref mut value) => value.push(item),
            _ => unreachable!()
        };
        None
    }

    fn concat(_ctx: &VMContext, receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>) -> Option<Value> {
        let arg = args.into_iter().next().expect("Array::concat requires 1 argument");
        let mut other_arr = match arg {
            Value::Obj(obj) => match &*obj.borrow() {
                Obj::ArrayObj(other_arr) => other_arr.clone(),
                _ => unreachable!()
            },
            _ => unreachable!()
        };

        let result = match &*receiver.as_ref().unwrap().borrow() {
            Obj::ArrayObj(value) => {
                let mut new_arr = Vec::with_capacity(value.len() + other_arr.len());
                let mut old_arr = value.clone();
                new_arr.append(&mut old_arr);
                new_arr.append(&mut other_arr);
                Value::new_array_obj(new_arr)
            }
            _ => unreachable!()
        };

        Some(result)
    }
}

impl NativeType for NativeArray {
    fn fields(typ: &Type) -> Vec<(&'static str, Type)> {
        let inner_type = match typ {
            Type::Array(inner_type) => *inner_type.clone(),
            _ => unreachable!()
        };

        vec![
            ("length", Type::Int),
            ("push", Type::Fn(Some(Box::new(typ.clone())), vec![("item".to_string(), inner_type.clone(), false)], Box::new(Type::Unit))),
            ("concat", Type::Fn(Some(Box::new(typ.clone())), vec![("items".to_string(), Type::Array(Box::new(inner_type.clone())), false)], Box::new(typ.clone()))),
        ]
    }

    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value {
        match &*obj.borrow() {
            Obj::ArrayObj(value) => {
                match field_idx {
                    0 => Value::Int(value.len() as i64),
                    1 => Value::NativeFn(NativeFn {
                        name: "push".to_string(),
                        args: vec![Type::Any],
                        opt_args: vec![],
                        return_type: Type::Unit,
                        receiver: Some(obj.clone()),
                        native_fn: NativeArray::push,
                    }),
                    2 => Value::NativeFn(NativeFn {
                        name: "concat".to_string(),
                        args: vec![Type::Any],
                        opt_args: vec![],
                        return_type: Type::Any,
                        receiver: Some(obj.clone()),
                        native_fn: NativeArray::concat,
                    }),
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }
}

pub struct NativeString;

impl NativeString {
    fn to_lower(_ctx: &VMContext, receiver: &Option<Arc<RefCell<Obj>>>, _args: Vec<Value>) -> Option<Value> {
        let string = match &*(receiver.as_ref().unwrap().borrow()) {
            Obj::StringObj(value) => value.clone(),
            _ => unreachable!()
        };

        Some(Value::new_string_obj(string.to_lowercase()))
    }

    fn to_upper(_ctx: &VMContext, receiver: &Option<Arc<RefCell<Obj>>>, _args: Vec<Value>) -> Option<Value> {
        let string = match &*(receiver.as_ref().unwrap().borrow()) {
            Obj::StringObj(value) => value.clone(),
            _ => unreachable!()
        };

        Some(Value::new_string_obj(string.to_uppercase()))
    }
}

impl NativeType for NativeString {
    fn fields(_: &Type) -> Vec<(&'static str, Type)> {
        vec![
            ("length", Type::Int),
            ("toLower", Type::Fn(Some(Box::new(Type::String)), vec![], Box::new(Type::String))),
            ("toUpper", Type::Fn(Some(Box::new(Type::String)), vec![], Box::new(Type::String))),
        ]
    }

    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value {
        match &*obj.borrow() {
            Obj::StringObj(value) => {
                match field_idx {
                    0 => Value::Int(value.len() as i64),
                    1 => Value::NativeFn(NativeFn {
                        name: "toLower".to_string(),
                        args: vec![],
                        opt_args: vec![],
                        return_type: Type::String,
                        receiver: Some(obj.clone()),
                        native_fn: NativeString::to_lower,
                    }),
                    2 => Value::NativeFn(NativeFn {
                        name: "toUpper".to_string(),
                        args: vec![],
                        opt_args: vec![],
                        return_type: Type::String,
                        receiver: Some(obj.clone()),
                        native_fn: NativeString::to_upper,
                    }),
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }
}
