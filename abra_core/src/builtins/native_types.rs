use crate::typechecker::types::Type;
use crate::vm::value::{Value, Obj};
use crate::builtins::gen_native_types::{NativeStringMethodsAndFields, NativeArrayMethodsAndFields};
use crate::vm::vm::VM;
use std::sync::Arc;
use std::cell::RefCell;

pub trait NativeType {
    fn get_field_or_method(name: &str) -> Option<(usize, Type)>;
    fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value;

    fn get_field_idx(field_name: &str) -> usize {
        match Self::get_field_or_method(field_name) {
            Some((idx, _)) => idx,
            None => unreachable!()
        }
    }
}

pub type NativeString = crate::builtins::gen_native_types::NativeString;

impl NativeStringMethodsAndFields for crate::builtins::gen_native_types::NativeString {
    fn field_length(obj: &Arc<RefCell<Obj>>) -> Value {
        match &*(obj.borrow()) {
            Obj::StringObj(value) => Value::Int(value.len() as i64),
            _ => unreachable!()
        }
    }

    fn method_to_lower(receiver: &Option<Arc<RefCell<Obj>>>, _: Vec<Value>, _: &mut VM) -> Option<Value> {
        let string = match &*(receiver.as_ref().unwrap().borrow()) {
            Obj::StringObj(value) => value.clone(),
            _ => unreachable!()
        };

        Some(Value::new_string_obj(string.to_lowercase()))
    }

    fn method_to_upper(receiver: &Option<Arc<RefCell<Obj>>>, _: Vec<Value>, _: &mut VM) -> Option<Value> {
        let string = match &*(receiver.as_ref().unwrap().borrow()) {
            Obj::StringObj(value) => value.clone(),
            _ => unreachable!()
        };

        Some(Value::new_string_obj(string.to_uppercase()))
    }
}

pub type NativeArray = crate::builtins::gen_native_types::NativeArray;

impl NativeArrayMethodsAndFields for crate::builtins::gen_native_types::NativeArray {
    fn field_length(obj: &Arc<RefCell<Obj>>) -> Value {
        match &*(obj.borrow()) {
            Obj::ArrayObj(values) => Value::Int(values.len() as i64),
            _ => unreachable!()
        }
    }

    fn method_push(receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>, _: &mut VM) -> Option<Value> {
        let item = args.into_iter().next().expect("Array::push requires 1 argument");

        let mut receiver = receiver.as_ref().unwrap().borrow_mut();
        match *receiver {
            Obj::ArrayObj(ref mut array) => array.push(item),
            _ => unreachable!()
        };
        None
    }

    fn method_concat(receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>, _: &mut VM) -> Option<Value> {
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

    fn method_map(receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
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

    fn method_filter(receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
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


    fn method_reduce(receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
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
        vm.run().unwrap()
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
        // TODO: See #172
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

        // Verify deep call stack initiated from native fn call
        let result = interpret(r#"
          func mult1(a: Int) = a * 1
          func sub1(a: Int) = mult1(a) - 1
          func sameNum(a: Int) = sub1(a) + 1
          [1, 2].map(i => sameNum(i))
        "#);
        let expected = int_array![1, 2];
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
