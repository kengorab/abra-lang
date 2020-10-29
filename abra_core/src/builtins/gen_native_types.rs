// Auto-generated file, do not modify

use crate::builtins::native_fns::NativeFn;
use crate::builtins::native_types::NativeType;
use crate::typechecker::types::{FnType, Type};
use crate::vm::value::Value;
use crate::vm::vm::VM;
pub struct NativeFloat;
pub trait NativeFloatMethodsAndFields {
    fn method_floor(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_ceil(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_round(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_with_precision(
        receiver: Option<Value>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
    fn method_abs(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
}
impl NativeType for NativeFloat {
    fn get_field_or_method(name: &str) -> Option<(usize, Type)> {
        match name {
            "floor" => Some((
                0usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Int),
                }),
            )),
            "ceil" => Some((
                1usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Int),
                }),
            )),
            "round" => Some((
                2usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Int),
                }),
            )),
            "withPrecision" => Some((
                3usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("precision".to_string(), Type::Int, false)],
                    ret_type: Box::new(Type::Float),
                }),
            )),
            "abs" => Some((
                4usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Float),
                }),
            )),
            _ => None,
        }
    }
    fn get_field_value(obj: Box<Value>, field_idx: usize) -> Value {
        match field_idx {
            0usize => Value::NativeFn(NativeFn {
                name: "floor",
                receiver: Some(obj),
                native_fn: Self::method_floor,
                has_return: true,
            }),
            1usize => Value::NativeFn(NativeFn {
                name: "ceil",
                receiver: Some(obj),
                native_fn: Self::method_ceil,
                has_return: true,
            }),
            2usize => Value::NativeFn(NativeFn {
                name: "round",
                receiver: Some(obj),
                native_fn: Self::method_round,
                has_return: true,
            }),
            3usize => Value::NativeFn(NativeFn {
                name: "withPrecision",
                receiver: Some(obj),
                native_fn: Self::method_with_precision,
                has_return: true,
            }),
            4usize => Value::NativeFn(NativeFn {
                name: "abs",
                receiver: Some(obj),
                native_fn: Self::method_abs,
                has_return: true,
            }),
            _ => unreachable!(),
        }
    }
}
pub struct NativeInt;
pub trait NativeIntMethodsAndFields {
    fn method_abs(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_as_base(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
}
impl NativeType for NativeInt {
    fn get_field_or_method(name: &str) -> Option<(usize, Type)> {
        match name {
            "abs" => Some((
                0usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Int),
                }),
            )),
            "asBase" => Some((
                1usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("base".to_string(), Type::Int, false)],
                    ret_type: Box::new(Type::String),
                }),
            )),
            _ => None,
        }
    }
    fn get_field_value(obj: Box<Value>, field_idx: usize) -> Value {
        match field_idx {
            0usize => Value::NativeFn(NativeFn {
                name: "abs",
                receiver: Some(obj),
                native_fn: Self::method_abs,
                has_return: true,
            }),
            1usize => Value::NativeFn(NativeFn {
                name: "asBase",
                receiver: Some(obj),
                native_fn: Self::method_as_base,
                has_return: true,
            }),
            _ => unreachable!(),
        }
    }
}
pub struct NativeString;
pub trait NativeStringMethodsAndFields {
    fn field_length(obj: Box<Value>) -> Value;
    fn method_to_lower(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_to_upper(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_pad_left(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
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
            "padLeft" => Some((
                3usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![
                        ("totalSize".to_string(), Type::Int, false),
                        ("padding".to_string(), Type::String, true),
                    ],
                    ret_type: Box::new(Type::String),
                }),
            )),
            _ => None,
        }
    }
    fn get_field_value(obj: Box<Value>, field_idx: usize) -> Value {
        match field_idx {
            0usize => Self::field_length(obj),
            1usize => Value::NativeFn(NativeFn {
                name: "toLower",
                receiver: Some(obj),
                native_fn: Self::method_to_lower,
                has_return: true,
            }),
            2usize => Value::NativeFn(NativeFn {
                name: "toUpper",
                receiver: Some(obj),
                native_fn: Self::method_to_upper,
                has_return: true,
            }),
            3usize => Value::NativeFn(NativeFn {
                name: "padLeft",
                receiver: Some(obj),
                native_fn: Self::method_pad_left,
                has_return: true,
            }),
            _ => unreachable!(),
        }
    }
}
pub struct NativeArray;
pub trait NativeArrayMethodsAndFields {
    fn field_length(obj: Box<Value>) -> Value;
    fn method_push(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_concat(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_map(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_filter(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_reduce(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_join(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
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
            "join" => Some((
                6usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("joiner".to_string(), Type::String, true)],
                    ret_type: Box::new(Type::String),
                }),
            )),
            _ => None,
        }
    }
    fn get_field_value(obj: Box<Value>, field_idx: usize) -> Value {
        match field_idx {
            0usize => Self::field_length(obj),
            1usize => Value::NativeFn(NativeFn {
                name: "push",
                receiver: Some(obj),
                native_fn: Self::method_push,
                has_return: false,
            }),
            2usize => Value::NativeFn(NativeFn {
                name: "concat",
                receiver: Some(obj),
                native_fn: Self::method_concat,
                has_return: true,
            }),
            3usize => Value::NativeFn(NativeFn {
                name: "map",
                receiver: Some(obj),
                native_fn: Self::method_map,
                has_return: true,
            }),
            4usize => Value::NativeFn(NativeFn {
                name: "filter",
                receiver: Some(obj),
                native_fn: Self::method_filter,
                has_return: true,
            }),
            5usize => Value::NativeFn(NativeFn {
                name: "reduce",
                receiver: Some(obj),
                native_fn: Self::method_reduce,
                has_return: true,
            }),
            6usize => Value::NativeFn(NativeFn {
                name: "join",
                receiver: Some(obj),
                native_fn: Self::method_join,
                has_return: true,
            }),
            _ => unreachable!(),
        }
    }
}
