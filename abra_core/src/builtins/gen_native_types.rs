// Auto-generated file, do not modify

use crate::builtins::native_fns::NativeFn;
use crate::builtins::native_types::NativeType;
use crate::typechecker::types::{FnType, Type};
use crate::vm::value::{Obj, Value};
use crate::vm::vm::VM;
use std::cell::RefCell;
use std::sync::Arc;
pub struct NativeString;
pub trait NativeStringMethodsAndFields {
    fn field_length(obj: &Arc<RefCell<Obj>>) -> Value;
    fn method_to_lower(
        receiver: &Option<Arc<RefCell<Obj>>>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
    fn method_to_upper(
        receiver: &Option<Arc<RefCell<Obj>>>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
}
impl NativeType for NativeString {
    fn get_field_or_method(name: &str) -> Option<(usize, Type)> {
        match name {
            "length" => Some((0usize, Type::Int)),
            "toLower" => Some((
                1usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::String),
                }),
            )),
            "toUpper" => Some((
                2usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::String),
                }),
            )),
            _ => None,
        }
    }
    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value {
        match &*obj.borrow() {
            Obj::StringObj(_) => match field_idx {
                0usize => Self::field_length(obj),
                1usize => Value::NativeFn(NativeFn {
                    name: "toLower",
                    receiver: Some(obj.clone()),
                    native_fn: Self::method_to_lower,
                    has_return: true,
                }),
                2usize => Value::NativeFn(NativeFn {
                    name: "toUpper",
                    receiver: Some(obj.clone()),
                    native_fn: Self::method_to_upper,
                    has_return: true,
                }),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}
pub struct NativeArray;
pub trait NativeArrayMethodsAndFields {
    fn field_length(obj: &Arc<RefCell<Obj>>) -> Value;
    fn method_push(
        receiver: &Option<Arc<RefCell<Obj>>>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
    fn method_concat(
        receiver: &Option<Arc<RefCell<Obj>>>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
    fn method_map(
        receiver: &Option<Arc<RefCell<Obj>>>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
    fn method_filter(
        receiver: &Option<Arc<RefCell<Obj>>>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
    fn method_reduce(
        receiver: &Option<Arc<RefCell<Obj>>>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
}
impl NativeType for NativeArray {
    fn get_field_or_method(name: &str) -> Option<(usize, Type)> {
        match name {
            "length" => Some((0usize, Type::Int)),
            "push" => Some((
                1usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("item".to_string(), Type::Generic("T".to_string()), false)],
                    ret_type: Box::new(Type::Unit),
                }),
            )),
            "concat" => Some((
                2usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![(
                        "other".to_string(),
                        Type::Array(Box::new(Type::Generic("T".to_string()))),
                        false,
                    )],
                    ret_type: Box::new(Type::Array(Box::new(Type::Generic("T".to_string())))),
                }),
            )),
            "map" => Some((
                3usize,
                Type::Fn(FnType {
                    type_args: vec!["U".to_string()],
                    arg_types: vec![(
                        "fn".to_string(),
                        Type::Fn(FnType {
                            type_args: vec![],
                            arg_types: vec![(
                                "_".to_string(),
                                Type::Generic("T".to_string()),
                                false,
                            )],
                            ret_type: Box::new(Type::Generic("U".to_string())),
                        }),
                        false,
                    )],
                    ret_type: Box::new(Type::Array(Box::new(Type::Generic("U".to_string())))),
                }),
            )),
            "filter" => Some((
                4usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![(
                        "fn".to_string(),
                        Type::Fn(FnType {
                            type_args: vec![],
                            arg_types: vec![(
                                "_".to_string(),
                                Type::Generic("T".to_string()),
                                false,
                            )],
                            ret_type: Box::new(Type::Bool),
                        }),
                        false,
                    )],
                    ret_type: Box::new(Type::Array(Box::new(Type::Generic("T".to_string())))),
                }),
            )),
            "reduce" => Some((
                5usize,
                Type::Fn(FnType {
                    type_args: vec!["U".to_string()],
                    arg_types: vec![
                        (
                            "initialValue".to_string(),
                            Type::Generic("U".to_string()),
                            false,
                        ),
                        (
                            "fn".to_string(),
                            Type::Fn(FnType {
                                type_args: vec![],
                                arg_types: vec![
                                    ("_".to_string(), Type::Generic("U".to_string()), false),
                                    ("_".to_string(), Type::Generic("T".to_string()), false),
                                ],
                                ret_type: Box::new(Type::Generic("U".to_string())),
                            }),
                            false,
                        ),
                    ],
                    ret_type: Box::new(Type::Generic("U".to_string())),
                }),
            )),
            _ => None,
        }
    }
    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value {
        match &*obj.borrow() {
            Obj::ArrayObj(_) => match field_idx {
                0usize => Self::field_length(obj),
                1usize => Value::NativeFn(NativeFn {
                    name: "push",
                    receiver: Some(obj.clone()),
                    native_fn: Self::method_push,
                    has_return: false,
                }),
                2usize => Value::NativeFn(NativeFn {
                    name: "concat",
                    receiver: Some(obj.clone()),
                    native_fn: Self::method_concat,
                    has_return: true,
                }),
                3usize => Value::NativeFn(NativeFn {
                    name: "map",
                    receiver: Some(obj.clone()),
                    native_fn: Self::method_map,
                    has_return: true,
                }),
                4usize => Value::NativeFn(NativeFn {
                    name: "filter",
                    receiver: Some(obj.clone()),
                    native_fn: Self::method_filter,
                    has_return: true,
                }),
                5usize => Value::NativeFn(NativeFn {
                    name: "reduce",
                    receiver: Some(obj.clone()),
                    native_fn: Self::method_reduce,
                    has_return: true,
                }),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}
