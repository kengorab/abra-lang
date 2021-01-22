// Auto-generated file, do not modify

use crate::builtins::native::NativeType;
use crate::builtins::native_fns::NativeFn;
use crate::typechecker::types::{FnType, Type};
use crate::vm::value::Value;
use crate::vm::vm::VM;
pub struct NativeFloat;
pub trait NativeFloatMethodsAndFields {
    fn method_to_string(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
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
    fn get_field_type(name: &str) -> Option<(usize, Type)> {
        match name {
            _ => None,
        }
    }
    fn get_method_type(name: &str) -> Option<(usize, Type)> {
        match name {
            "toString" => Some((
                0usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::String),
                    is_variadic: false,
                }),
            )),
            "floor" => Some((
                1usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Int),
                    is_variadic: false,
                }),
            )),
            "ceil" => Some((
                2usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Int),
                    is_variadic: false,
                }),
            )),
            "round" => Some((
                3usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Int),
                    is_variadic: false,
                }),
            )),
            "withPrecision" => Some((
                4usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("precision".to_string(), Type::Int, false)],
                    ret_type: Box::new(Type::Float),
                    is_variadic: false,
                }),
            )),
            "abs" => Some((
                5usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Float),
                    is_variadic: false,
                }),
            )),
            _ => None,
        }
    }
    fn get_static_field_or_method(_name: &str) -> Option<(usize, Type)> {
        None
    }
    fn get_field_value(_obj: Box<Value>, field_idx: usize) -> Value {
        match field_idx {
            _ => unreachable!(),
        }
    }
    fn get_method_value(obj: Box<Value>, method_idx: usize) -> Value {
        match method_idx {
            0usize => Value::NativeFn(NativeFn {
                name: "toString",
                receiver: Some(obj),
                native_fn: Self::method_to_string,
                has_return: true,
            }),
            1usize => Value::NativeFn(NativeFn {
                name: "floor",
                receiver: Some(obj),
                native_fn: Self::method_floor,
                has_return: true,
            }),
            2usize => Value::NativeFn(NativeFn {
                name: "ceil",
                receiver: Some(obj),
                native_fn: Self::method_ceil,
                has_return: true,
            }),
            3usize => Value::NativeFn(NativeFn {
                name: "round",
                receiver: Some(obj),
                native_fn: Self::method_round,
                has_return: true,
            }),
            4usize => Value::NativeFn(NativeFn {
                name: "withPrecision",
                receiver: Some(obj),
                native_fn: Self::method_with_precision,
                has_return: true,
            }),
            5usize => Value::NativeFn(NativeFn {
                name: "abs",
                receiver: Some(obj),
                native_fn: Self::method_abs,
                has_return: true,
            }),
            _ => unreachable!(),
        }
    }
    fn get_static_field_values() -> Vec<(String, Value)> {
        vec![]
    }
}
pub struct NativeInt;
pub trait NativeIntMethodsAndFields {
    fn method_to_string(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_abs(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_as_base(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_is_even(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_is_odd(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_is_between(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
}
impl NativeType for NativeInt {
    fn get_field_type(name: &str) -> Option<(usize, Type)> {
        match name {
            _ => None,
        }
    }
    fn get_method_type(name: &str) -> Option<(usize, Type)> {
        match name {
            "toString" => Some((
                0usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::String),
                    is_variadic: false,
                }),
            )),
            "abs" => Some((
                1usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Int),
                    is_variadic: false,
                }),
            )),
            "asBase" => Some((
                2usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("base".to_string(), Type::Int, false)],
                    ret_type: Box::new(Type::String),
                    is_variadic: false,
                }),
            )),
            "isEven" => Some((
                3usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Bool),
                    is_variadic: false,
                }),
            )),
            "isOdd" => Some((
                4usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Bool),
                    is_variadic: false,
                }),
            )),
            "isBetween" => Some((
                5usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![
                        ("lower".to_string(), Type::Int, false),
                        ("upper".to_string(), Type::Int, false),
                        ("inclusive".to_string(), Type::Bool, true),
                    ],
                    ret_type: Box::new(Type::Bool),
                    is_variadic: false,
                }),
            )),
            _ => None,
        }
    }
    fn get_static_field_or_method(_name: &str) -> Option<(usize, Type)> {
        None
    }
    fn get_field_value(_obj: Box<Value>, field_idx: usize) -> Value {
        match field_idx {
            _ => unreachable!(),
        }
    }
    fn get_method_value(obj: Box<Value>, method_idx: usize) -> Value {
        match method_idx {
            0usize => Value::NativeFn(NativeFn {
                name: "toString",
                receiver: Some(obj),
                native_fn: Self::method_to_string,
                has_return: true,
            }),
            1usize => Value::NativeFn(NativeFn {
                name: "abs",
                receiver: Some(obj),
                native_fn: Self::method_abs,
                has_return: true,
            }),
            2usize => Value::NativeFn(NativeFn {
                name: "asBase",
                receiver: Some(obj),
                native_fn: Self::method_as_base,
                has_return: true,
            }),
            3usize => Value::NativeFn(NativeFn {
                name: "isEven",
                receiver: Some(obj),
                native_fn: Self::method_is_even,
                has_return: true,
            }),
            4usize => Value::NativeFn(NativeFn {
                name: "isOdd",
                receiver: Some(obj),
                native_fn: Self::method_is_odd,
                has_return: true,
            }),
            5usize => Value::NativeFn(NativeFn {
                name: "isBetween",
                receiver: Some(obj),
                native_fn: Self::method_is_between,
                has_return: true,
            }),
            _ => unreachable!(),
        }
    }
    fn get_static_field_values() -> Vec<(String, Value)> {
        vec![]
    }
}
pub struct NativeString;
pub trait NativeStringMethodsAndFields {
    fn field_length(obj: Box<Value>) -> Value;
    fn method_to_string(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_to_lower(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_to_upper(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_pad_left(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_trim(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_trim_start(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_trim_end(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_split(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_split_at(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_lines(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_chars(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_parse_int(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_parse_float(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_concat(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
}
impl NativeType for NativeString {
    fn get_field_type(name: &str) -> Option<(usize, Type)> {
        match name {
            "length" => Some((0usize, Type::Int)),
            _ => None,
        }
    }
    fn get_method_type(name: &str) -> Option<(usize, Type)> {
        match name {
            "toString" => Some((
                0usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::String),
                    is_variadic: false,
                }),
            )),
            "toLower" => Some((
                1usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::String),
                    is_variadic: false,
                }),
            )),
            "toUpper" => Some((
                2usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::String),
                    is_variadic: false,
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
                    is_variadic: false,
                }),
            )),
            "trim" => Some((
                4usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::String),
                    is_variadic: false,
                }),
            )),
            "trimStart" => Some((
                5usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("pattern".to_string(), Type::String, true)],
                    ret_type: Box::new(Type::String),
                    is_variadic: false,
                }),
            )),
            "trimEnd" => Some((
                6usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("pattern".to_string(), Type::String, true)],
                    ret_type: Box::new(Type::String),
                    is_variadic: false,
                }),
            )),
            "split" => Some((
                7usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("splitter".to_string(), Type::String, false)],
                    ret_type: Box::new(Type::Array(Box::new(Type::String))),
                    is_variadic: false,
                }),
            )),
            "splitAt" => Some((
                8usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("index".to_string(), Type::Int, false)],
                    ret_type: Box::new(Type::Tuple(vec![Type::String, Type::String])),
                    is_variadic: false,
                }),
            )),
            "lines" => Some((
                9usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Array(Box::new(Type::String))),
                    is_variadic: false,
                }),
            )),
            "chars" => Some((
                10usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Array(Box::new(Type::String))),
                    is_variadic: false,
                }),
            )),
            "parseInt" => Some((
                11usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("radix".to_string(), Type::Int, true)],
                    ret_type: Box::new(Type::Option(Box::new(Type::Int))),
                    is_variadic: false,
                }),
            )),
            "parseFloat" => Some((
                12usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Option(Box::new(Type::Float))),
                    is_variadic: false,
                }),
            )),
            "concat" => Some((
                13usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![
                        ("str".to_string(), Type::Any, false),
                        ("others".to_string(), Type::Array(Box::new(Type::Any)), true),
                    ],
                    ret_type: Box::new(Type::String),
                    is_variadic: true,
                }),
            )),
            _ => None,
        }
    }
    fn get_static_field_or_method(_name: &str) -> Option<(usize, Type)> {
        None
    }
    fn get_field_value(obj: Box<Value>, field_idx: usize) -> Value {
        match field_idx {
            0usize => Self::field_length(obj),
            _ => unreachable!(),
        }
    }
    fn get_method_value(obj: Box<Value>, method_idx: usize) -> Value {
        match method_idx {
            0usize => Value::NativeFn(NativeFn {
                name: "toString",
                receiver: Some(obj),
                native_fn: Self::method_to_string,
                has_return: true,
            }),
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
            4usize => Value::NativeFn(NativeFn {
                name: "trim",
                receiver: Some(obj),
                native_fn: Self::method_trim,
                has_return: true,
            }),
            5usize => Value::NativeFn(NativeFn {
                name: "trimStart",
                receiver: Some(obj),
                native_fn: Self::method_trim_start,
                has_return: true,
            }),
            6usize => Value::NativeFn(NativeFn {
                name: "trimEnd",
                receiver: Some(obj),
                native_fn: Self::method_trim_end,
                has_return: true,
            }),
            7usize => Value::NativeFn(NativeFn {
                name: "split",
                receiver: Some(obj),
                native_fn: Self::method_split,
                has_return: true,
            }),
            8usize => Value::NativeFn(NativeFn {
                name: "splitAt",
                receiver: Some(obj),
                native_fn: Self::method_split_at,
                has_return: true,
            }),
            9usize => Value::NativeFn(NativeFn {
                name: "lines",
                receiver: Some(obj),
                native_fn: Self::method_lines,
                has_return: true,
            }),
            10usize => Value::NativeFn(NativeFn {
                name: "chars",
                receiver: Some(obj),
                native_fn: Self::method_chars,
                has_return: true,
            }),
            11usize => Value::NativeFn(NativeFn {
                name: "parseInt",
                receiver: Some(obj),
                native_fn: Self::method_parse_int,
                has_return: true,
            }),
            12usize => Value::NativeFn(NativeFn {
                name: "parseFloat",
                receiver: Some(obj),
                native_fn: Self::method_parse_float,
                has_return: true,
            }),
            13usize => Value::NativeFn(NativeFn {
                name: "concat",
                receiver: Some(obj),
                native_fn: Self::method_concat,
                has_return: true,
            }),
            _ => unreachable!(),
        }
    }
    fn get_static_field_values() -> Vec<(String, Value)> {
        vec![]
    }
}
