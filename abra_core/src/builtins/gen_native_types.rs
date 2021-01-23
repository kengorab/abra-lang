// Auto-generated file, do not modify

// use crate::builtins::native::NativeType;
// use crate::builtins::native_fns::NativeFn;
// use crate::typechecker::types::{FnType, Type};
// use crate::vm::value::Value;
// use crate::vm::vm::VM;
// pub struct NativeFloat;
// pub trait NativeFloatMethodsAndFields {
//     fn method_to_string(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
//     fn method_floor(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
//     fn method_ceil(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
//     fn method_round(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
//     fn method_with_precision(
//         receiver: Option<Value>,
//         args: Vec<Value>,
//         vm: &mut VM,
//     ) -> Option<Value>;
//     fn method_abs(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
// }
// impl NativeType for NativeFloat {
//     fn get_field_type(name: &str) -> Option<(usize, Type)> {
//         match name {
//             _ => None,
//         }
//     }
//     fn get_method_type(name: &str) -> Option<(usize, Type)> {
//         match name {
//             "toString" => Some((
//                 0usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![],
//                     ret_type: Box::new(Type::String),
//                     is_variadic: false,
//                 }),
//             )),
//             "floor" => Some((
//                 1usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![],
//                     ret_type: Box::new(Type::Int),
//                     is_variadic: false,
//                 }),
//             )),
//             "ceil" => Some((
//                 2usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![],
//                     ret_type: Box::new(Type::Int),
//                     is_variadic: false,
//                 }),
//             )),
//             "round" => Some((
//                 3usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![],
//                     ret_type: Box::new(Type::Int),
//                     is_variadic: false,
//                 }),
//             )),
//             "withPrecision" => Some((
//                 4usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![("precision".to_string(), Type::Int, false)],
//                     ret_type: Box::new(Type::Float),
//                     is_variadic: false,
//                 }),
//             )),
//             "abs" => Some((
//                 5usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![],
//                     ret_type: Box::new(Type::Float),
//                     is_variadic: false,
//                 }),
//             )),
//             _ => None,
//         }
//     }
//     fn get_static_field_or_method(_name: &str) -> Option<(usize, Type)> {
//         None
//     }
//     fn get_field_value(_obj: Box<Value>, field_idx: usize) -> Value {
//         match field_idx {
//             _ => unreachable!(),
//         }
//     }
//     fn get_method_value(obj: Box<Value>, method_idx: usize) -> Value {
//         match method_idx {
//             0usize => Value::NativeFn(NativeFn {
//                 name: "toString",
//                 receiver: Some(obj),
//                 native_fn: Self::method_to_string,
//                 has_return: true,
//             }),
//             1usize => Value::NativeFn(NativeFn {
//                 name: "floor",
//                 receiver: Some(obj),
//                 native_fn: Self::method_floor,
//                 has_return: true,
//             }),
//             2usize => Value::NativeFn(NativeFn {
//                 name: "ceil",
//                 receiver: Some(obj),
//                 native_fn: Self::method_ceil,
//                 has_return: true,
//             }),
//             3usize => Value::NativeFn(NativeFn {
//                 name: "round",
//                 receiver: Some(obj),
//                 native_fn: Self::method_round,
//                 has_return: true,
//             }),
//             4usize => Value::NativeFn(NativeFn {
//                 name: "withPrecision",
//                 receiver: Some(obj),
//                 native_fn: Self::method_with_precision,
//                 has_return: true,
//             }),
//             5usize => Value::NativeFn(NativeFn {
//                 name: "abs",
//                 receiver: Some(obj),
//                 native_fn: Self::method_abs,
//                 has_return: true,
//             }),
//             _ => unreachable!(),
//         }
//     }
//     fn get_static_field_values() -> Vec<(String, Value)> {
//         vec![]
//     }
// }
// pub struct NativeInt;
// pub trait NativeIntMethodsAndFields {
//     fn method_to_string(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
//     fn method_abs(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
//     fn method_as_base(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
//     fn method_is_even(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
//     fn method_is_odd(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
//     fn method_is_between(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
// }
// impl NativeType for NativeInt {
//     fn get_field_type(name: &str) -> Option<(usize, Type)> {
//         match name {
//             _ => None,
//         }
//     }
//     fn get_method_type(name: &str) -> Option<(usize, Type)> {
//         match name {
//             "toString" => Some((
//                 0usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![],
//                     ret_type: Box::new(Type::String),
//                     is_variadic: false,
//                 }),
//             )),
//             "abs" => Some((
//                 1usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![],
//                     ret_type: Box::new(Type::Int),
//                     is_variadic: false,
//                 }),
//             )),
//             "asBase" => Some((
//                 2usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![("base".to_string(), Type::Int, false)],
//                     ret_type: Box::new(Type::String),
//                     is_variadic: false,
//                 }),
//             )),
//             "isEven" => Some((
//                 3usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![],
//                     ret_type: Box::new(Type::Bool),
//                     is_variadic: false,
//                 }),
//             )),
//             "isOdd" => Some((
//                 4usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![],
//                     ret_type: Box::new(Type::Bool),
//                     is_variadic: false,
//                 }),
//             )),
//             "isBetween" => Some((
//                 5usize,
//                 Type::Fn(FnType {
//                     type_args: vec![],
//                     arg_types: vec![
//                         ("lower".to_string(), Type::Int, false),
//                         ("upper".to_string(), Type::Int, false),
//                         ("inclusive".to_string(), Type::Bool, true),
//                     ],
//                     ret_type: Box::new(Type::Bool),
//                     is_variadic: false,
//                 }),
//             )),
//             _ => None,
//         }
//     }
//     fn get_static_field_or_method(_name: &str) -> Option<(usize, Type)> {
//         None
//     }
//     fn get_field_value(_obj: Box<Value>, field_idx: usize) -> Value {
//         match field_idx {
//             _ => unreachable!(),
//         }
//     }
//     fn get_method_value(obj: Box<Value>, method_idx: usize) -> Value {
//         match method_idx {
//             0usize => Value::NativeFn(NativeFn {
//                 name: "toString",
//                 receiver: Some(obj),
//                 native_fn: Self::method_to_string,
//                 has_return: true,
//             }),
//             1usize => Value::NativeFn(NativeFn {
//                 name: "abs",
//                 receiver: Some(obj),
//                 native_fn: Self::method_abs,
//                 has_return: true,
//             }),
//             2usize => Value::NativeFn(NativeFn {
//                 name: "asBase",
//                 receiver: Some(obj),
//                 native_fn: Self::method_as_base,
//                 has_return: true,
//             }),
//             3usize => Value::NativeFn(NativeFn {
//                 name: "isEven",
//                 receiver: Some(obj),
//                 native_fn: Self::method_is_even,
//                 has_return: true,
//             }),
//             4usize => Value::NativeFn(NativeFn {
//                 name: "isOdd",
//                 receiver: Some(obj),
//                 native_fn: Self::method_is_odd,
//                 has_return: true,
//             }),
//             5usize => Value::NativeFn(NativeFn {
//                 name: "isBetween",
//                 receiver: Some(obj),
//                 native_fn: Self::method_is_between,
//                 has_return: true,
//             }),
//             _ => unreachable!(),
//         }
//     }
//     fn get_static_field_values() -> Vec<(String, Value)> {
//         vec![]
//     }
// }
