use crate::builtins::gen_native_types::NativeSetMethodsAndFields;
use crate::vm::value::{Value, Obj};
use crate::vm::vm::VM;
use crate::builtins::native::common::invoke_fn;
use std::collections::HashSet;

pub type NativeSet = crate::builtins::gen_native_types::NativeSet;

impl NativeSetMethodsAndFields for NativeSet {
    fn field_size(obj: Box<Value>) -> Value {
        if let Value::Obj(obj) = *obj {
            match &*(obj.borrow()) {
                Obj::SetObj(value) => Value::Int(value.len() as i64),
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_is_empty(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::SetObj(set) => Some(Value::Bool(set.is_empty())),
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_enumerate(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::SetObj(set) => {
                    let tuples = set.iter().enumerate()
                        .map(|(idx, value)| {
                            Value::new_tuple_obj(vec![value.clone(), Value::Int(idx as i64)])
                        }).collect();
                    Some(Value::new_array_obj(tuples))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_contains(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let item = args.into_iter().next().expect("Set::contains requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::SetObj(set) => Some(Value::Bool(set.contains(&item))),
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_map(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Set::map requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match *obj.borrow() {
                Obj::SetObj(ref set) => {
                    let mut new_items = Vec::new();

                    for value in set {
                        let args = vec![value.clone()];
                        let value = invoke_fn(vm, &callback, args);
                        new_items.push(value);
                    }

                    Some(Value::new_array_obj(new_items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_filter(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Set::filter requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::SetObj(set) => {
                    let mut new_items = HashSet::new();

                    for value in set {
                        let args = vec![value.clone()];
                        let ret_val = invoke_fn(vm, &callback, args);
                        if let Value::Bool(true) = ret_val {
                            new_items.insert(value.clone());
                        }
                    }

                    Some(Value::new_set_obj(new_items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_reduce(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let initial_value = args.next().expect("Set::reduce requires 2 arguments");
        let callback = args.next().expect("Set::reduce requires 2 arguments");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::SetObj(set) => {
                    let mut accumulator = initial_value;

                    for value in set {
                        let args = vec![accumulator, value.clone()];
                        accumulator = invoke_fn(vm, &callback, args);
                    }

                    Some(accumulator)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_as_array(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::SetObj(set) => {
                    Some(Value::new_array_obj(set.into_iter().map(|v| v.clone()).collect()))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_union(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let other = args.into_iter().next().expect("Set::union requires 1 argument");
        let other = if let Value::Obj(obj) = other {
            match &(*obj.borrow()) {
                Obj::SetObj(s) => s.clone(),
                _ => unreachable!()
            }
        } else { unreachable!() };

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::SetObj(set) => {
                    let new_set = set.union(&other).map(|v| v.clone()).collect();
                    Some(Value::new_set_obj(new_set))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_difference(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let other = args.into_iter().next().expect("Set::difference requires 1 argument");
        let other = if let Value::Obj(obj) = other {
            match &(*obj.borrow()) {
                Obj::SetObj(s) => s.clone(),
                _ => unreachable!()
            }
        } else { unreachable!() };

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::SetObj(set) => {
                    let new_set = set.difference(&other).map(|v| v.clone()).collect();
                    Some(Value::new_set_obj(new_set))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_intersection(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let other = args.into_iter().next().expect("Set::intersection requires 1 argument");
        let other = if let Value::Obj(obj) = other {
            match &(*obj.borrow()) {
                Obj::SetObj(s) => s.clone(),
                _ => unreachable!()
            }
        } else { unreachable!() };

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::SetObj(set) => {
                    let new_set = set.intersection(&other).map(|v| v.clone()).collect();
                    Some(Value::new_set_obj(new_set))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::native::test_utils::interpret;
    use crate::vm::value::Value;

    #[test]
    fn test_set_size() {
        let result = interpret("#{}.size");
        let expected = Value::Int(0);
        assert_eq!(Some(expected), result);

        let result = interpret("#{0, 1, 2, \"3\"}.size");
        let expected = Value::Int(4);
        assert_eq!(Some(expected), result);

        let result = interpret("#{0, 1, 2, 1, 0}.size");
        let expected = Value::Int(3);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_set_is_empty() {
        let result = interpret("#{}.isEmpty()");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("#{1, 2, \"3\"}.isEmpty()");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_set_contains() {
        let result = interpret("#{}.contains(\"a\")");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("#{\"a\", \"b\"}.contains(\"a\")");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("\
          type Person { name: String }\n\
          #{Person(name: \"Ken\"), Person(name: \"Ken\")}.contains(Person(name: \"Ken\"))\
        ");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_set_map() {
        let result = interpret("#{\"a\", \"b\"}.map(w => w.length)");
        let expected = array![Value::Int(1), Value::Int(1)];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_set_filter() {
        let result = interpret("#{1, 2, 3, 4, 5}.filter(n => n.isEven())");
        let expected = set![Value::Int(2), Value::Int(4)];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_set_reduce() {
        let result = interpret("#{1, 2, 3, 4, 5}.reduce(0, (acc, n) => acc + n)");
        let expected = Value::Int(15);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_set_as_array() {
        let result = interpret("#{3, 4, 5}.asArray()");
        let expecteds = vec![
            Some(array![Value::Int(3), Value::Int(4), Value::Int(5)]),
            Some(array![Value::Int(3), Value::Int(5), Value::Int(4)]),
            Some(array![Value::Int(4), Value::Int(3), Value::Int(5)]),
            Some(array![Value::Int(4), Value::Int(5), Value::Int(3)]),
            Some(array![Value::Int(5), Value::Int(4), Value::Int(3)]),
            Some(array![Value::Int(5), Value::Int(3), Value::Int(4)]),
        ];
        assert!(expecteds.contains(&result)); // Sets' order isn't guaranteed :(
    }

    #[test]
    fn test_set_union() {
        let result = interpret("#{}.union(#{})");
        let expected = set![];
        assert_eq!(Some(expected), result);

        let result = interpret("#{1}.union(#{1, 2})");
        let expected = set![Value::Int(1), Value::Int(2)];
        assert_eq!(Some(expected), result);

        let result = interpret("\
          val s1 = #{1, 3, 5}
          val s2 = #{2, 4,}
          s1.union(s2)
        ");
        let expected = set![Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(4), Value::Int(5)];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_set_difference() {
        let result = interpret("#{}.difference(#{})");
        let expected = set![];
        assert_eq!(Some(expected), result);

        let result = interpret("#{1}.difference(#{1, 2})");
        let expected = set![];
        assert_eq!(Some(expected), result);

        let result = interpret("#{1, 2}.difference(#{2})");
        let expected = set![Value::Int(1)];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_set_intersection() {
        let result = interpret("#{1, 2, 3}.intersection(#{})");
        let expected = set![];
        assert_eq!(Some(expected), result);

        let result = interpret("#{1, 2}.intersection(#{3, 4})");
        let expected = set![];
        assert_eq!(Some(expected), result);

        let result = interpret("#{1}.intersection(#{1, 2})");
        let expected = set![Value::Int(1)];
        assert_eq!(Some(expected), result);
    }
}
