// Auto-generated file, do not modify

use crate::builtins::native::NativeType;
use crate::builtins::native_fns::NativeFn;
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
    fn get_static_field_or_method(_name: &str) -> Option<(usize, Type)> {
        None
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
    fn get_static_field_values() -> Vec<(String, Value)> {
        vec![]
    }
}
pub struct NativeInt;
pub trait NativeIntMethodsAndFields {
    fn method_abs(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_as_base(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_is_even(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_is_odd(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_is_between(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
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
            "isEven" => Some((
                2usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Bool),
                }),
            )),
            "isOdd" => Some((
                3usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Bool),
                }),
            )),
            "isBetween" => Some((
                4usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![
                        ("lower".to_string(), Type::Int, false),
                        ("upper".to_string(), Type::Int, false),
                        ("inclusive".to_string(), Type::Bool, true),
                    ],
                    ret_type: Box::new(Type::Bool),
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
            2usize => Value::NativeFn(NativeFn {
                name: "isEven",
                receiver: Some(obj),
                native_fn: Self::method_is_even,
                has_return: true,
            }),
            3usize => Value::NativeFn(NativeFn {
                name: "isOdd",
                receiver: Some(obj),
                native_fn: Self::method_is_odd,
                has_return: true,
            }),
            4usize => Value::NativeFn(NativeFn {
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
    fn method_to_lower(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_to_upper(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_pad_left(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_trim(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_trim_start(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_trim_end(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_split(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_lines(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_chars(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_parse_int(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_parse_float(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
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
            "trim" => Some((
                4usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::String),
                }),
            )),
            "trimStart" => Some((
                5usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("pattern".to_string(), Type::String, true)],
                    ret_type: Box::new(Type::String),
                }),
            )),
            "trimEnd" => Some((
                6usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("pattern".to_string(), Type::String, true)],
                    ret_type: Box::new(Type::String),
                }),
            )),
            "split" => Some((
                7usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("splitter".to_string(), Type::String, false)],
                    ret_type: Box::new(Type::Array(Box::new(Type::String))),
                }),
            )),
            "lines" => Some((
                8usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Array(Box::new(Type::String))),
                }),
            )),
            "chars" => Some((
                9usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Array(Box::new(Type::String))),
                }),
            )),
            "parseInt" => Some((
                10usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("radix".to_string(), Type::Int, true)],
                    ret_type: Box::new(Type::Option(Box::new(Type::Int))),
                }),
            )),
            "parseFloat" => Some((
                11usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Option(Box::new(Type::Float))),
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
                name: "lines",
                receiver: Some(obj),
                native_fn: Self::method_lines,
                has_return: true,
            }),
            9usize => Value::NativeFn(NativeFn {
                name: "chars",
                receiver: Some(obj),
                native_fn: Self::method_chars,
                has_return: true,
            }),
            10usize => Value::NativeFn(NativeFn {
                name: "parseInt",
                receiver: Some(obj),
                native_fn: Self::method_parse_int,
                has_return: true,
            }),
            11usize => Value::NativeFn(NativeFn {
                name: "parseFloat",
                receiver: Some(obj),
                native_fn: Self::method_parse_float,
                has_return: true,
            }),
            _ => unreachable!(),
        }
    }
    fn get_static_field_values() -> Vec<(String, Value)> {
        vec![]
    }
}
pub struct NativeArray;
pub trait NativeArrayMethodsAndFields {
    fn field_length(obj: Box<Value>) -> Value;
    fn static_method_fill(_receiver: Option<Value>, args: Vec<Value>, vm: &mut VM)
        -> Option<Value>;
    fn static_method_fill_by(
        _receiver: Option<Value>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
    fn method_is_empty(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_enumerate(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_push(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_concat(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_map(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_filter(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_reduce(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_join(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_contains(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_find(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_find_index(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_any(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_all(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_none(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_sort_by(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_dedupe(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_dedupe_by(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_partition(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_tally(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_tally_by(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_as_set(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_get_or_default(
        receiver: Option<Value>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
    fn method_get_or_else(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_update(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
}
impl NativeType for NativeArray {
    fn get_field_or_method(name: &str) -> Option<(usize, Type)> {
        match name {
            "length" => Some((0usize, Type::Int)),
            "isEmpty" => Some((
                1usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Bool),
                }),
            )),
            "enumerate" => Some((
                2usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Array(Box::new(Type::Tuple(vec![
                        Type::Generic("T".to_string()),
                        Type::Int,
                    ])))),
                }),
            )),
            "push" => Some((
                3usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("item".to_string(), Type::Generic("T".to_string()), false)],
                    ret_type: Box::new(Type::Unit),
                }),
            )),
            "concat" => Some((
                4usize,
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
                5usize,
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
                6usize,
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
                7usize,
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
                8usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("joiner".to_string(), Type::String, true)],
                    ret_type: Box::new(Type::String),
                }),
            )),
            "contains" => Some((
                9usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("item".to_string(), Type::Generic("T".to_string()), false)],
                    ret_type: Box::new(Type::Bool),
                }),
            )),
            "find" => Some((
                10usize,
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
                            ret_type: Box::new(Type::Union(vec![
                                Type::Bool,
                                Type::Option(Box::new(Type::Generic("U".to_string()))),
                            ])),
                        }),
                        false,
                    )],
                    ret_type: Box::new(Type::Option(Box::new(Type::Generic("T".to_string())))),
                }),
            )),
            "findIndex" => Some((
                11usize,
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
                            ret_type: Box::new(Type::Union(vec![
                                Type::Bool,
                                Type::Option(Box::new(Type::Generic("U".to_string()))),
                            ])),
                        }),
                        false,
                    )],
                    ret_type: Box::new(Type::Option(Box::new(Type::Tuple(vec![
                        Type::Generic("T".to_string()),
                        Type::Int,
                    ])))),
                }),
            )),
            "any" => Some((
                12usize,
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
                            ret_type: Box::new(Type::Union(vec![
                                Type::Bool,
                                Type::Option(Box::new(Type::Generic("U".to_string()))),
                            ])),
                        }),
                        false,
                    )],
                    ret_type: Box::new(Type::Bool),
                }),
            )),
            "all" => Some((
                13usize,
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
                            ret_type: Box::new(Type::Union(vec![
                                Type::Bool,
                                Type::Option(Box::new(Type::Generic("U".to_string()))),
                            ])),
                        }),
                        false,
                    )],
                    ret_type: Box::new(Type::Bool),
                }),
            )),
            "none" => Some((
                14usize,
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
                            ret_type: Box::new(Type::Union(vec![
                                Type::Bool,
                                Type::Option(Box::new(Type::Generic("U".to_string()))),
                            ])),
                        }),
                        false,
                    )],
                    ret_type: Box::new(Type::Bool),
                }),
            )),
            "sortBy" => Some((
                15usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![
                        (
                            "fn".to_string(),
                            Type::Fn(FnType {
                                type_args: vec![],
                                arg_types: vec![(
                                    "_".to_string(),
                                    Type::Generic("T".to_string()),
                                    false,
                                )],
                                ret_type: Box::new(Type::Int),
                            }),
                            false,
                        ),
                        ("reverse".to_string(), Type::Bool, true),
                    ],
                    ret_type: Box::new(Type::Array(Box::new(Type::Generic("T".to_string())))),
                }),
            )),
            "dedupe" => Some((
                16usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Array(Box::new(Type::Generic("T".to_string())))),
                }),
            )),
            "dedupeBy" => Some((
                17usize,
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
                    ret_type: Box::new(Type::Array(Box::new(Type::Generic("T".to_string())))),
                }),
            )),
            "partition" => Some((
                18usize,
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
                    ret_type: Box::new(Type::Map(
                        Box::new(Type::Generic("U".to_string())),
                        Box::new(Type::Array(Box::new(Type::Generic("T".to_string())))),
                    )),
                }),
            )),
            "tally" => Some((
                19usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Map(
                        Box::new(Type::Generic("T".to_string())),
                        Box::new(Type::Int),
                    )),
                }),
            )),
            "tallyBy" => Some((
                20usize,
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
                    ret_type: Box::new(Type::Map(
                        Box::new(Type::Generic("U".to_string())),
                        Box::new(Type::Int),
                    )),
                }),
            )),
            "asSet" => Some((
                21usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Set(Box::new(Type::Generic("T".to_string())))),
                }),
            )),
            "getOrDefault" => Some((
                22usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![
                        ("key".to_string(), Type::Int, false),
                        ("default".to_string(), Type::Generic("T".to_string()), false),
                    ],
                    ret_type: Box::new(Type::Generic("T".to_string())),
                }),
            )),
            "getOrElse" => Some((
                23usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![
                        ("key".to_string(), Type::Int, false),
                        (
                            "fn".to_string(),
                            Type::Fn(FnType {
                                type_args: vec![],
                                arg_types: vec![],
                                ret_type: Box::new(Type::Generic("T".to_string())),
                            }),
                            false,
                        ),
                    ],
                    ret_type: Box::new(Type::Generic("T".to_string())),
                }),
            )),
            "update" => Some((
                24usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![
                        ("key".to_string(), Type::Int, false),
                        (
                            "fn".to_string(),
                            Type::Fn(FnType {
                                type_args: vec![],
                                arg_types: vec![(
                                    "_".to_string(),
                                    Type::Generic("T".to_string()),
                                    false,
                                )],
                                ret_type: Box::new(Type::Generic("T".to_string())),
                            }),
                            false,
                        ),
                    ],
                    ret_type: Box::new(Type::Unit),
                }),
            )),
            _ => None,
        }
    }
    fn get_static_field_or_method(name: &str) -> Option<(usize, Type)> {
        match name {
            "fill" => Some((
                0usize,
                Type::Fn(FnType {
                    type_args: vec!["T1".to_string()],
                    arg_types: vec![
                        ("amount".to_string(), Type::Int, false),
                        ("value".to_string(), Type::Generic("T1".to_string()), false),
                    ],
                    ret_type: Box::new(Type::Array(Box::new(Type::Generic("T1".to_string())))),
                }),
            )),
            "fillBy" => Some((
                1usize,
                Type::Fn(FnType {
                    type_args: vec!["T1".to_string()],
                    arg_types: vec![
                        ("amount".to_string(), Type::Int, false),
                        (
                            "fn".to_string(),
                            Type::Fn(FnType {
                                type_args: vec![],
                                arg_types: vec![("_".to_string(), Type::Int, false)],
                                ret_type: Box::new(Type::Generic("T1".to_string())),
                            }),
                            false,
                        ),
                    ],
                    ret_type: Box::new(Type::Array(Box::new(Type::Generic("T1".to_string())))),
                }),
            )),
            _ => None,
        }
    }
    fn get_field_value(obj: Box<Value>, field_idx: usize) -> Value {
        match field_idx {
            0usize => Self::field_length(obj),
            1usize => Value::NativeFn(NativeFn {
                name: "isEmpty",
                receiver: Some(obj),
                native_fn: Self::method_is_empty,
                has_return: true,
            }),
            2usize => Value::NativeFn(NativeFn {
                name: "enumerate",
                receiver: Some(obj),
                native_fn: Self::method_enumerate,
                has_return: true,
            }),
            3usize => Value::NativeFn(NativeFn {
                name: "push",
                receiver: Some(obj),
                native_fn: Self::method_push,
                has_return: false,
            }),
            4usize => Value::NativeFn(NativeFn {
                name: "concat",
                receiver: Some(obj),
                native_fn: Self::method_concat,
                has_return: true,
            }),
            5usize => Value::NativeFn(NativeFn {
                name: "map",
                receiver: Some(obj),
                native_fn: Self::method_map,
                has_return: true,
            }),
            6usize => Value::NativeFn(NativeFn {
                name: "filter",
                receiver: Some(obj),
                native_fn: Self::method_filter,
                has_return: true,
            }),
            7usize => Value::NativeFn(NativeFn {
                name: "reduce",
                receiver: Some(obj),
                native_fn: Self::method_reduce,
                has_return: true,
            }),
            8usize => Value::NativeFn(NativeFn {
                name: "join",
                receiver: Some(obj),
                native_fn: Self::method_join,
                has_return: true,
            }),
            9usize => Value::NativeFn(NativeFn {
                name: "contains",
                receiver: Some(obj),
                native_fn: Self::method_contains,
                has_return: true,
            }),
            10usize => Value::NativeFn(NativeFn {
                name: "find",
                receiver: Some(obj),
                native_fn: Self::method_find,
                has_return: true,
            }),
            11usize => Value::NativeFn(NativeFn {
                name: "findIndex",
                receiver: Some(obj),
                native_fn: Self::method_find_index,
                has_return: true,
            }),
            12usize => Value::NativeFn(NativeFn {
                name: "any",
                receiver: Some(obj),
                native_fn: Self::method_any,
                has_return: true,
            }),
            13usize => Value::NativeFn(NativeFn {
                name: "all",
                receiver: Some(obj),
                native_fn: Self::method_all,
                has_return: true,
            }),
            14usize => Value::NativeFn(NativeFn {
                name: "none",
                receiver: Some(obj),
                native_fn: Self::method_none,
                has_return: true,
            }),
            15usize => Value::NativeFn(NativeFn {
                name: "sortBy",
                receiver: Some(obj),
                native_fn: Self::method_sort_by,
                has_return: true,
            }),
            16usize => Value::NativeFn(NativeFn {
                name: "dedupe",
                receiver: Some(obj),
                native_fn: Self::method_dedupe,
                has_return: true,
            }),
            17usize => Value::NativeFn(NativeFn {
                name: "dedupeBy",
                receiver: Some(obj),
                native_fn: Self::method_dedupe_by,
                has_return: true,
            }),
            18usize => Value::NativeFn(NativeFn {
                name: "partition",
                receiver: Some(obj),
                native_fn: Self::method_partition,
                has_return: true,
            }),
            19usize => Value::NativeFn(NativeFn {
                name: "tally",
                receiver: Some(obj),
                native_fn: Self::method_tally,
                has_return: true,
            }),
            20usize => Value::NativeFn(NativeFn {
                name: "tallyBy",
                receiver: Some(obj),
                native_fn: Self::method_tally_by,
                has_return: true,
            }),
            21usize => Value::NativeFn(NativeFn {
                name: "asSet",
                receiver: Some(obj),
                native_fn: Self::method_as_set,
                has_return: true,
            }),
            22usize => Value::NativeFn(NativeFn {
                name: "getOrDefault",
                receiver: Some(obj),
                native_fn: Self::method_get_or_default,
                has_return: true,
            }),
            23usize => Value::NativeFn(NativeFn {
                name: "getOrElse",
                receiver: Some(obj),
                native_fn: Self::method_get_or_else,
                has_return: true,
            }),
            24usize => Value::NativeFn(NativeFn {
                name: "update",
                receiver: Some(obj),
                native_fn: Self::method_update,
                has_return: false,
            }),
            _ => unreachable!(),
        }
    }
    fn get_static_field_values() -> Vec<(String, Value)> {
        vec![
            (
                "fill".to_string(),
                Value::NativeFn(NativeFn {
                    name: "fill",
                    receiver: None,
                    native_fn: Self::static_method_fill,
                    has_return: true,
                }),
            ),
            (
                "fillBy".to_string(),
                Value::NativeFn(NativeFn {
                    name: "fillBy",
                    receiver: None,
                    native_fn: Self::static_method_fill_by,
                    has_return: true,
                }),
            ),
        ]
    }
}
pub struct NativeMap;
pub trait NativeMapMethodsAndFields {
    fn field_size(obj: Box<Value>) -> Value;
    fn static_method_from_pairs(
        _receiver: Option<Value>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
    fn method_is_empty(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_enumerate(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_keys(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_values(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_entries(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_contains_key(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM)
        -> Option<Value>;
    fn method_map_values(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_get_or_default(
        receiver: Option<Value>,
        args: Vec<Value>,
        vm: &mut VM,
    ) -> Option<Value>;
    fn method_get_or_else(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_update(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
}
impl NativeType for NativeMap {
    fn get_field_or_method(name: &str) -> Option<(usize, Type)> {
        match name {
            "size" => Some((0usize, Type::Int)),
            "isEmpty" => Some((
                1usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Bool),
                }),
            )),
            "enumerate" => Some((
                2usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Array(Box::new(Type::Tuple(vec![
                        Type::Generic("K".to_string()),
                        Type::Generic("V".to_string()),
                    ])))),
                }),
            )),
            "keys" => Some((
                3usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Set(Box::new(Type::Generic("K".to_string())))),
                }),
            )),
            "values" => Some((
                4usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Set(Box::new(Type::Generic("V".to_string())))),
                }),
            )),
            "entries" => Some((
                5usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Set(Box::new(Type::Tuple(vec![
                        Type::Generic("K".to_string()),
                        Type::Generic("V".to_string()),
                    ])))),
                }),
            )),
            "containsKey" => Some((
                6usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("key".to_string(), Type::Generic("K".to_string()), false)],
                    ret_type: Box::new(Type::Bool),
                }),
            )),
            "mapValues" => Some((
                7usize,
                Type::Fn(FnType {
                    type_args: vec!["U".to_string()],
                    arg_types: vec![(
                        "fn".to_string(),
                        Type::Fn(FnType {
                            type_args: vec![],
                            arg_types: vec![
                                ("_".to_string(), Type::Generic("K".to_string()), false),
                                ("_".to_string(), Type::Generic("V".to_string()), false),
                            ],
                            ret_type: Box::new(Type::Generic("U".to_string())),
                        }),
                        false,
                    )],
                    ret_type: Box::new(Type::Map(
                        Box::new(Type::Generic("K".to_string())),
                        Box::new(Type::Generic("U".to_string())),
                    )),
                }),
            )),
            "getOrDefault" => Some((
                8usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![
                        ("key".to_string(), Type::Generic("K".to_string()), false),
                        ("default".to_string(), Type::Generic("V".to_string()), false),
                    ],
                    ret_type: Box::new(Type::Generic("V".to_string())),
                }),
            )),
            "getOrElse" => Some((
                9usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![
                        ("key".to_string(), Type::Generic("K".to_string()), false),
                        (
                            "fn".to_string(),
                            Type::Fn(FnType {
                                type_args: vec![],
                                arg_types: vec![],
                                ret_type: Box::new(Type::Generic("V".to_string())),
                            }),
                            false,
                        ),
                    ],
                    ret_type: Box::new(Type::Generic("V".to_string())),
                }),
            )),
            "update" => Some((
                10usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![
                        ("key".to_string(), Type::Generic("K".to_string()), false),
                        (
                            "fn".to_string(),
                            Type::Fn(FnType {
                                type_args: vec![],
                                arg_types: vec![(
                                    "_".to_string(),
                                    Type::Generic("V".to_string()),
                                    false,
                                )],
                                ret_type: Box::new(Type::Generic("V".to_string())),
                            }),
                            false,
                        ),
                    ],
                    ret_type: Box::new(Type::Unit),
                }),
            )),
            _ => None,
        }
    }
    fn get_static_field_or_method(name: &str) -> Option<(usize, Type)> {
        match name {
            "fromPairs" => Some((
                0usize,
                Type::Fn(FnType {
                    type_args: vec!["T1".to_string(), "T2".to_string()],
                    arg_types: vec![(
                        "pairs".to_string(),
                        Type::Array(Box::new(Type::Tuple(vec![
                            Type::Generic("T1".to_string()),
                            Type::Generic("T2".to_string()),
                        ]))),
                        false,
                    )],
                    ret_type: Box::new(Type::Map(
                        Box::new(Type::Generic("T1".to_string())),
                        Box::new(Type::Generic("T2".to_string())),
                    )),
                }),
            )),
            _ => None,
        }
    }
    fn get_field_value(obj: Box<Value>, field_idx: usize) -> Value {
        match field_idx {
            0usize => Self::field_size(obj),
            1usize => Value::NativeFn(NativeFn {
                name: "isEmpty",
                receiver: Some(obj),
                native_fn: Self::method_is_empty,
                has_return: true,
            }),
            2usize => Value::NativeFn(NativeFn {
                name: "enumerate",
                receiver: Some(obj),
                native_fn: Self::method_enumerate,
                has_return: true,
            }),
            3usize => Value::NativeFn(NativeFn {
                name: "keys",
                receiver: Some(obj),
                native_fn: Self::method_keys,
                has_return: true,
            }),
            4usize => Value::NativeFn(NativeFn {
                name: "values",
                receiver: Some(obj),
                native_fn: Self::method_values,
                has_return: true,
            }),
            5usize => Value::NativeFn(NativeFn {
                name: "entries",
                receiver: Some(obj),
                native_fn: Self::method_entries,
                has_return: true,
            }),
            6usize => Value::NativeFn(NativeFn {
                name: "containsKey",
                receiver: Some(obj),
                native_fn: Self::method_contains_key,
                has_return: true,
            }),
            7usize => Value::NativeFn(NativeFn {
                name: "mapValues",
                receiver: Some(obj),
                native_fn: Self::method_map_values,
                has_return: true,
            }),
            8usize => Value::NativeFn(NativeFn {
                name: "getOrDefault",
                receiver: Some(obj),
                native_fn: Self::method_get_or_default,
                has_return: true,
            }),
            9usize => Value::NativeFn(NativeFn {
                name: "getOrElse",
                receiver: Some(obj),
                native_fn: Self::method_get_or_else,
                has_return: true,
            }),
            10usize => Value::NativeFn(NativeFn {
                name: "update",
                receiver: Some(obj),
                native_fn: Self::method_update,
                has_return: false,
            }),
            _ => unreachable!(),
        }
    }
    fn get_static_field_values() -> Vec<(String, Value)> {
        vec![(
            "fromPairs".to_string(),
            Value::NativeFn(NativeFn {
                name: "fromPairs",
                receiver: None,
                native_fn: Self::static_method_from_pairs,
                has_return: true,
            }),
        )]
    }
}
pub struct NativeSet;
pub trait NativeSetMethodsAndFields {
    fn field_size(obj: Box<Value>) -> Value;
    fn method_is_empty(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_enumerate(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_contains(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_map(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_filter(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_reduce(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_as_array(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_union(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_difference(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
    fn method_intersection(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM)
        -> Option<Value>;
}
impl NativeType for NativeSet {
    fn get_field_or_method(name: &str) -> Option<(usize, Type)> {
        match name {
            "size" => Some((0usize, Type::Int)),
            "isEmpty" => Some((
                1usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Bool),
                }),
            )),
            "enumerate" => Some((
                2usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Array(Box::new(Type::Tuple(vec![
                        Type::Generic("T".to_string()),
                        Type::Int,
                    ])))),
                }),
            )),
            "contains" => Some((
                3usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![("value".to_string(), Type::Generic("T".to_string()), false)],
                    ret_type: Box::new(Type::Bool),
                }),
            )),
            "map" => Some((
                4usize,
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
                5usize,
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
                    ret_type: Box::new(Type::Set(Box::new(Type::Generic("T".to_string())))),
                }),
            )),
            "reduce" => Some((
                6usize,
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
            "asArray" => Some((
                7usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![],
                    ret_type: Box::new(Type::Array(Box::new(Type::Generic("T".to_string())))),
                }),
            )),
            "union" => Some((
                8usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![(
                        "other".to_string(),
                        Type::Set(Box::new(Type::Generic("T".to_string()))),
                        false,
                    )],
                    ret_type: Box::new(Type::Set(Box::new(Type::Generic("T".to_string())))),
                }),
            )),
            "difference" => Some((
                9usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![(
                        "other".to_string(),
                        Type::Set(Box::new(Type::Generic("T".to_string()))),
                        false,
                    )],
                    ret_type: Box::new(Type::Set(Box::new(Type::Generic("T".to_string())))),
                }),
            )),
            "intersection" => Some((
                10usize,
                Type::Fn(FnType {
                    type_args: vec![],
                    arg_types: vec![(
                        "other".to_string(),
                        Type::Set(Box::new(Type::Generic("T".to_string()))),
                        false,
                    )],
                    ret_type: Box::new(Type::Set(Box::new(Type::Generic("T".to_string())))),
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
            0usize => Self::field_size(obj),
            1usize => Value::NativeFn(NativeFn {
                name: "isEmpty",
                receiver: Some(obj),
                native_fn: Self::method_is_empty,
                has_return: true,
            }),
            2usize => Value::NativeFn(NativeFn {
                name: "enumerate",
                receiver: Some(obj),
                native_fn: Self::method_enumerate,
                has_return: true,
            }),
            3usize => Value::NativeFn(NativeFn {
                name: "contains",
                receiver: Some(obj),
                native_fn: Self::method_contains,
                has_return: true,
            }),
            4usize => Value::NativeFn(NativeFn {
                name: "map",
                receiver: Some(obj),
                native_fn: Self::method_map,
                has_return: true,
            }),
            5usize => Value::NativeFn(NativeFn {
                name: "filter",
                receiver: Some(obj),
                native_fn: Self::method_filter,
                has_return: true,
            }),
            6usize => Value::NativeFn(NativeFn {
                name: "reduce",
                receiver: Some(obj),
                native_fn: Self::method_reduce,
                has_return: true,
            }),
            7usize => Value::NativeFn(NativeFn {
                name: "asArray",
                receiver: Some(obj),
                native_fn: Self::method_as_array,
                has_return: true,
            }),
            8usize => Value::NativeFn(NativeFn {
                name: "union",
                receiver: Some(obj),
                native_fn: Self::method_union,
                has_return: true,
            }),
            9usize => Value::NativeFn(NativeFn {
                name: "difference",
                receiver: Some(obj),
                native_fn: Self::method_difference,
                has_return: true,
            }),
            10usize => Value::NativeFn(NativeFn {
                name: "intersection",
                receiver: Some(obj),
                native_fn: Self::method_intersection,
                has_return: true,
            }),
            _ => unreachable!(),
        }
    }
    fn get_static_field_values() -> Vec<(String, Value)> {
        vec![]
    }
}
