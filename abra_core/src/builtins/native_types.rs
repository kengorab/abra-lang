use crate::typechecker::types::{Type, FnType};
use crate::vm::value::{Value, Obj};
use crate::builtins::native_fns::{NativeFn, NativeFnDesc};
use crate::vm::vm::VM;
use std::sync::Arc;
use std::cell::RefCell;

pub trait NativeType {
    fn fields(typ: &Type) -> Vec<(&'static str, Type)>;
    fn methods(typ: &Type) -> Vec<NativeFnDesc>;
    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value;

    fn fields_and_methods(typ: &Type) -> Vec<(&'static str, Type)> {
        let mut fields = Self::fields(typ);

        let mut methods = Self::methods(typ).into_iter()
            .map(|m| (m.name, m.get_fn_type()))
            .collect::<Vec<(&str, Type)>>();
        fields.append(&mut methods);
        fields
    }

    fn get_field_idx(typ: &Type, field_name: &str) -> usize {
        match Self::get_field(typ, field_name) {
            Some((idx, _)) => idx,
            None => unreachable!()
        }
    }

    fn get_field(typ: &Type, field_name: &str) -> Option<(usize, (&'static str, Type))> {
        Self::fields_and_methods(typ).into_iter().enumerate().find(|(_, (name, _))| name == &field_name)
    }
}

pub fn field_for_type(typ: &Type, field_name: &str) -> Option<(usize, (&'static str, Type))> {
    match &typ {
        Type::Array(_) => NativeArray::get_field(typ, field_name),
        Type::String => NativeString::get_field(typ, field_name),
        _ => None
    }
}

// TODO: This could stand to be cleaned up a bit
pub struct NativeArray;

impl NativeArray {
    fn push(receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let item = args.into_iter().next().expect("Array::push requires 1 argument");

        let mut receiver = receiver.as_ref().unwrap().borrow_mut();
        match *receiver {
            Obj::ArrayObj(ref mut array) => array.push(item),
            _ => unreachable!()
        };
        None
    }

    fn concat(receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let arg = args.into_iter().next().expect("Array::concat requires 1 argument");
        let mut other_arr = match arg {
            Value::Obj(obj) => match &*obj.borrow() {
                Obj::ArrayObj(other_arr) => other_arr.clone(),
                _ => unreachable!()
            },
            _ => unreachable!()
        };

        let result = match &*receiver.as_ref().unwrap().borrow() {
            Obj::ArrayObj(array) => {
                let mut new_arr = Vec::with_capacity(array.len() + other_arr.len());
                let mut old_arr = array.clone();
                new_arr.append(&mut old_arr);
                new_arr.append(&mut other_arr);
                Value::new_array_obj(new_arr)
            }
            _ => unreachable!()
        };

        Some(result)
    }

    fn map(receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        match &*receiver.as_ref().unwrap().borrow() {
            Obj::ArrayObj(array) => {
                let callback = args.into_iter().next().expect("Array::map requires 1 argument");

                let mut new_array_items = Vec::new();

                for value in array {
                    let args = vec![value.clone()];
                    let new_value = vm.invoke_fn(args, callback.clone())
                        .unwrap_or(Some(Value::Nil))
                        .unwrap_or(Value::Nil);
                    new_array_items.push(new_value);
                }

                Some(Value::new_array_obj(new_array_items))
            }
            _ => unreachable!()
        }
    }

    fn filter(receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        match &*receiver.as_ref().unwrap().borrow() {
            Obj::ArrayObj(array) => {
                let callback = args.into_iter().next().expect("Array::filter requires 1 argument");

                let mut new_array_items = Vec::new();

                for value in array {
                    let args = vec![value.clone()];
                    let new_value = vm.invoke_fn(args, callback.clone())
                        .unwrap_or(Some(Value::Bool(false)))
                        .unwrap_or(Value::Bool(false));
                    if let Value::Bool(true) = new_value {
                        new_array_items.push(value.clone());
                    }
                }

                Some(Value::new_array_obj(new_array_items))
            }
            _ => unreachable!()
        }
    }

    fn reduce(receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        match &*receiver.as_ref().unwrap().borrow() {
            Obj::ArrayObj(array) => {
                let mut args = args.into_iter();
                let initial_value = args.next().expect("Array::reduce requires 2 arguments");
                let callback = args.next().expect("Array::reduce requires 2 arguments");

                let mut accumulator = initial_value;

                for value in array {
                    let args = vec![accumulator, value.clone()];
                    let new_value = vm.invoke_fn(args, callback.clone())
                        .unwrap_or(Some(Value::Nil))
                        .unwrap_or(Value::Nil);
                    accumulator = new_value;
                }

                Some(accumulator)
            }
            _ => unreachable!()
        }
    }
}

impl NativeType for NativeArray {
    fn fields(_: &Type) -> Vec<(&'static str, Type)> {
        vec![
            ("length", Type::Int)
        ]
    }

    fn methods(typ: &Type) -> Vec<NativeFnDesc> {
        let inner_type = match typ {
            Type::Array(inner_type) => &**inner_type,
            _ => unreachable!()
        };

        vec![
            NativeFnDesc {
                name: "push",
                type_args: vec![],
                args: vec![("item", inner_type.clone())],
                opt_args: vec![],
                return_type: Type::Unit,
            },
            NativeFnDesc {
                name: "concat",
                type_args: vec![],
                args: vec![("items", typ.clone())],
                opt_args: vec![],
                return_type: typ.clone(),
            },
            NativeFnDesc {
                name: "map",
                type_args: vec!["U"],
                args: vec![("fn", Type::Fn(FnType {
                    arg_types: vec![
                        ("item".to_string(), inner_type.clone(), false),
                    ],
                    type_args: vec!["U".to_string()],
                    ret_type: Box::new(Type::Generic("U".to_string())),
                }))],
                opt_args: vec![],
                return_type: Type::Array(Box::new(Type::Generic("U".to_string()))),
            },
            NativeFnDesc {
                name: "filter",
                type_args: vec![],
                args: vec![("fn", Type::Fn(FnType {
                    arg_types: vec![
                        ("item".to_string(), inner_type.clone(), false),
                    ],
                    type_args: vec![],
                    ret_type: Box::new(Type::Bool), // TODO: Allow returning of Optionals
                }))],
                opt_args: vec![],
                return_type: typ.clone(),
            },
            NativeFnDesc {
                name: "reduce",
                type_args: vec!["U"],
                args: vec![
                    ("initialValue", Type::Generic("U".to_string())),
                    ("fn", Type::Fn(FnType {
                        arg_types: vec![
                            ("accumulator".to_string(), Type::Generic("U".to_string()), false),
                            ("item".to_string(), inner_type.clone(), false),
                        ],
                        type_args: vec!["U".to_string()],
                        ret_type: Box::new(Type::Generic("U".to_string())),
                    })),
                ],
                opt_args: vec![],
                return_type: Type::Generic("U".to_string()),
            }
        ]
    }

    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value {
        match &*obj.borrow() {
            Obj::ArrayObj(value) => {
                match field_idx {
                    0 => Value::Int(value.len() as i64),
                    1 => Value::NativeFn(NativeFn {
                        name: "push",
                        receiver: Some(obj.clone()),
                        native_fn: NativeArray::push,
                        has_return: false,
                    }),
                    2 => Value::NativeFn(NativeFn {
                        name: "concat",
                        receiver: Some(obj.clone()),
                        native_fn: NativeArray::concat,
                        has_return: true,
                    }),
                    3 => Value::NativeFn(NativeFn {
                        name: "map",
                        receiver: Some(obj.clone()),
                        native_fn: NativeArray::map,
                        has_return: true,
                    }),
                    4 => Value::NativeFn(NativeFn {
                        name: "filter",
                        receiver: Some(obj.clone()),
                        native_fn: NativeArray::filter,
                        has_return: true,
                    }),
                    5 => Value::NativeFn(NativeFn {
                        name: "reduce",
                        receiver: Some(obj.clone()),
                        native_fn: NativeArray::reduce,
                        has_return: true,
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
    fn to_lower(receiver: &Option<Arc<RefCell<Obj>>>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let string = match &*(receiver.as_ref().unwrap().borrow()) {
            Obj::StringObj(value) => value.clone(),
            _ => unreachable!()
        };

        Some(Value::new_string_obj(string.to_lowercase()))
    }

    fn to_upper(receiver: &Option<Arc<RefCell<Obj>>>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
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
        ]
    }

    fn methods(_: &Type) -> Vec<NativeFnDesc> {
        vec![
            NativeFnDesc {
                name: "toLower",
                type_args: vec![],
                args: vec![],
                opt_args: vec![],
                return_type: Type::String,
            },
            NativeFnDesc {
                name: "toUpper",
                type_args: vec![],
                args: vec![],
                opt_args: vec![],
                return_type: Type::String,
            }
        ]
    }

    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value {
        match &*obj.borrow() {
            Obj::StringObj(value) => {
                match field_idx {
                    0 => Value::Int(value.len() as i64),
                    1 => Value::NativeFn(NativeFn {
                        name: "toLower",
                        receiver: Some(obj.clone()),
                        native_fn: NativeString::to_lower,
                        has_return: true,
                    }),
                    2 => Value::NativeFn(NativeFn {
                        name: "toUpper",
                        receiver: Some(obj.clone()),
                        native_fn: NativeString::to_upper,
                        has_return: true,
                    }),
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::typechecker::typechecker::typecheck;
    use crate::vm::compiler::compile;
    use crate::vm::value::Value;
    use crate::vm::vm::{VM, VMContext};

    fn new_string_obj(string: &str) -> Value {
        Value::new_string_obj(string.to_string())
    }

    macro_rules! array {
        ($($i:expr),*) => { Value::new_array_obj(vec![$($i),*]) };
    }

    macro_rules! int_array {
        ($($i:expr),*) => { Value::new_array_obj(vec![$($i),*].into_iter().map(Value::Int).collect()) };
    }

    macro_rules! string_array {
        ($($i:expr),*) => { Value::new_array_obj(vec![$($i),*].into_iter().map(new_string_obj).collect()) };
    }

    fn interpret(input: &str) -> Option<Value> {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let (_, typed_ast) = typecheck(ast).unwrap();
        let (module, _) = compile(typed_ast).unwrap();

        let mut vm = VM::new(module, VMContext::default());
        vm.run(false).unwrap()
    }

    #[test]
    fn test_string_length() {
        let result = interpret("\"asdf qwer\".length");
        let expected = Value::Int(9);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_to_upper() {
        let result = interpret("\"Asdf Qwer\".toUpper()");
        let expected = new_string_obj("ASDF QWER");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_to_lower() {
        let result = interpret("\"aSDF qWER\".toLower()");
        let expected = new_string_obj("asdf qwer");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_length() {
        let result = interpret("[1, 2, 3, 4, 5].length");
        let expected = Value::Int(5);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_push() {
        let result = interpret(r#"
          val arr = [1, 2, 3]
          arr.push(4)
          arr.push(5)
          arr
        "#);
        let expected = int_array!(1, 2, 3, 4, 5);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_concat() {
        let result = interpret(r#"
          val arr1 = [1, 2, 3]
          val arr2 = [4, 5, 6]
          arr1.concat(arr2)
        "#);
        let expected = int_array![1, 2, 3, 4, 5, 6];
        assert_eq!(Some(expected), result);

        // Verify that the original arrays aren't modified
        let result = interpret(r#"
          val arr1 = [1, 2, 3]
          val arr2 = [4, 5, 6]
          val arr3 = arr1.concat(arr2)
          [arr1, arr2]
        "#);
        let expected = array![
            int_array![1, 2, 3],
            int_array![4, 5, 6]
        ];
        assert_eq!(Some(expected), result);

        // Verify that the arrays' items are copied by reference
        let result = interpret(r#"
          type Counter {
            count: Int = 0
            func inc(self): Int { self.count += 1 }
          }

          val arr1 = [Counter(), Counter()]
          val arr2 = [Counter(), Counter()]
          val arr3 = arr1.concat(arr2)

          if arr1[0] |c| c.inc()
          if arr2[1] |c| c.inc()

          [
            arr1.map(c => c.count),
            arr2.map(c => c.count),
            arr3.map(c => c.count)
          ]
        "#);
        let expected = array![
            int_array![1, 0],
            int_array![0, 1],
            int_array![1, 0, 0, 1]
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_map() {
        let result = interpret(r#"
          val arr = [1, 2, 3, 4]
          arr.map(i => i * 3)
        "#);
        let expected = int_array![3, 6, 9, 12];
        assert_eq!(Some(expected), result);

        // Verify closures work
        let result = interpret(r#"
          var total = 0
          val arr = [1, 2, 3, 4]
          val arr2 = arr.map(i => {
            total += i
            i * 3
          })
          arr2.concat([total])
        "#);
        let expected = int_array![3, 6, 9, 12, 10];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_filter() {
        let result = interpret(r#"
          val arr = ["a", "bc", "def", "ghij", "klmno"]
          arr.filter(w => w.length < 4)
        "#);
        let expected = string_array!["a", "bc", "def"];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_reduce() {
        let result = interpret(r#"
          val arr = [1, 2, 3, 4, 5]
          arr.reduce(0, (acc, i) => acc + i)
        "#);
        let expected = Value::Int(15);
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr = [1, 2, 3, 4, 5]
          arr.reduce("", (acc, i) => acc + i)
        "#);
        let expected = new_string_obj("12345");
        assert_eq!(Some(expected), result);
    }
}
