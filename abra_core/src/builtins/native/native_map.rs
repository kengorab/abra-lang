use crate::builtins::gen_native_types::NativeMapMethodsAndFields;
use crate::vm::value::{Value, Obj};
use crate::vm::vm::VM;
use std::collections::HashMap;
use crate::builtins::native::common::invoke_fn;

pub type NativeMap = crate::builtins::gen_native_types::NativeMap;

impl NativeMapMethodsAndFields for NativeMap {
    fn field_size(obj: Box<Value>) -> Value {
        if let Value::Obj(obj) = *obj {
            match &*(obj.borrow()) {
                Obj::MapObj(value) => Value::Int(value.len() as i64),
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn static_method_from_pairs(_receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let pairs_array = args.into_iter().next().expect("Map::fromPairs requires 1 argument");
        if let Value::Obj(obj) = pairs_array {
            match &*(obj.borrow()) {
                Obj::ArrayObj(items) => {
                    let mut map = HashMap::new();

                    for item in items {
                        if let Value::Obj(obj) = item {
                            match &*(obj.borrow()) {
                                Obj::TupleObj(items) => {
                                    let key = items[0].clone();
                                    let val = items[1].clone();
                                    map.insert(key, val);
                                }
                                _ => unreachable!()
                            }
                        } else { unreachable!() }
                    }

                    Some(Value::new_map_obj(map))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_is_empty(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(value) => Some(Value::Bool(value.is_empty())),
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_enumerate(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    let tuples = map.iter()
                        .map(|(key, value)| {
                            Value::new_tuple_obj(vec![key.clone(), value.clone()])
                        }).collect();
                    Some(Value::new_array_obj(tuples))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_keys(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    let keys = map.keys()
                        .map(|k| k.clone())
                        .collect();
                    Some(Value::new_set_obj(keys))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_values(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    let values = map.values()
                        .map(|v| v.clone())
                        .collect();
                    Some(Value::new_set_obj(values))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_entries(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    let entries = map.iter().map(|(k, v)| Value::new_tuple_obj(vec![k.clone(), v.clone()])).collect();
                    Some(Value::new_set_obj(entries))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_contains_key(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let key = args.into_iter().next().expect("Map::containsKey requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    Some(Value::Bool(map.contains_key(&key)))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_map_values(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Map::mapValues requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    let mut new_map = HashMap::new();

                    for (k, v) in map {
                        let args = vec![k.clone(), v.clone()];
                        let value = invoke_fn(vm, &callback, args);
                        new_map.insert(k.clone(), value);
                    }

                    Some(Value::new_map_obj(new_map))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_get_or_default(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let key = args.next().expect("Map::getOrDefault requires 2 arguments");
        let default = args.next().expect("Map::getOrDefault requires 2 arguments");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    map.get(&key).map(|v| v.clone()).or(Some(default))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_get_or_else(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let key = args.next().expect("Map::getOrElse requires 2 arguments");
        let callback = args.next().expect("Map::getOrElse requires 2 arguments");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    map.get(&key)
                        .map(|v| v.clone())
                        .or_else(|| Some(invoke_fn(vm, &callback, vec![])))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_update(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let key = args.next().expect("Map::update requires 2 arguments");
        let callback = args.next().expect("Map::update requires 2 arguments");

        if let Value::Obj(obj) = receiver.unwrap() {
            match *(obj.borrow_mut()) {
                Obj::MapObj(ref mut map) => {
                    match map.get_mut(&key) {
                        None => {}
                        Some(item) => {
                            *item = invoke_fn(vm, &callback, vec![item.clone()]);
                        }
                    }
                    None
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::native::test_utils::{interpret, new_string_obj};
    use crate::vm::value::Value;
    use std::collections::HashMap;

    #[test]
    fn test_map_field_size() {
        let result = interpret("{}.size");
        let expected = Value::Int(0);
        assert_eq!(Some(expected), result);

        let result = interpret("{ a: 123, b: true }.size");
        let expected = Value::Int(2);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_map_static_from_pairs() {
        let result = interpret("Map.fromPairs([])");
        let expected = Value::new_map_obj(HashMap::new());
        assert_eq!(result, Some(expected));

        let result = interpret("Map.fromPairs([(\"a\", 123), (\"b\", 456)])");
        let expected = map! {
            new_string_obj("a") => Value::Int(123),
            new_string_obj("b") => Value::Int(456)
        };
        assert_eq!(result, Some(expected));
    }

    #[test]
    fn test_map_is_empty() {
        let result = interpret("{}.isEmpty()");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("{ a: 123, b: true }.isEmpty()");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_map_keys() {
        let result = interpret("{}.keys()");
        let expected = set![];
        assert_eq!(Some(expected), result);

        let result = interpret("{ a: 123, b: true }.keys()");
        let expected = set![new_string_obj("a"), new_string_obj("b")];
        assert_eq!(Some(expected), result);

        let result = interpret("\
          val m: Map<Int[], Int> = {}\
          m[[1, 2]] = 2\
          m[[1, 2, 3]] = 3\
          m.keys()
        ");
        let expected = set![
            array![Value::Int(1), Value::Int(2)],
            array![Value::Int(1), Value::Int(2), Value::Int(3)]
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("{ a: 123, b: true }.keys()");
        let expected = set![new_string_obj("a"), new_string_obj("b")];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_map_values() {
        let result = interpret("{}.values()");
        let expected = set![];
        assert_eq!(Some(expected), result);

        let result = interpret("{ a: 123, b: true }.values()");
        let expected = set![Value::Int(123), Value::Bool(true)];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_map_entries() {
        let result = interpret("{}.entries()");
        let expected = set![];
        assert_eq!(Some(expected), result);

        let result = interpret("{ a: 123, b: true }.entries()");
        let expected = set![
            tuple!(new_string_obj("a"), Value::Int(123)),
            tuple!(new_string_obj("b"), Value::Bool(true))
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_map_contains_key() {
        let result = interpret("{}.containsKey(\"asdf\")");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("{ a: 24 }.containsKey(\"a\")");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("\
          val m: Map<Int[], String> = {}\
          m[[1, 2, 3]] = \"hello\"\
          m.containsKey([1, 2, 3])\
        ");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_map_map_values() {
        let result = interpret(
            "{ a: 1, b: 2 }.mapValues((_, v) => v + 1)"
        );
        let expected = map! {
            new_string_obj("a") => Value::Int(2),
            new_string_obj("b") => Value::Int(3)
        };
        assert_eq!(Some(expected), result);

        let result = interpret(
            "{ a: 1, b: 2 }.mapValues((k, v) => k + v)"
        );
        let expected = map! {
            new_string_obj("a") => new_string_obj("a1"),
            new_string_obj("b") => new_string_obj("b2")
        };
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_map_get_or_default() {
        let result = interpret("{a:1, b:2}.getOrDefault(\"b\", 12)");
        let expected = Value::Int(2);
        assert_eq!(Some(expected), result);

        let result = interpret("{a:1, b:2}.getOrDefault(\"c\", 12)");
        let expected = Value::Int(12);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_map_get_or_else() {
        let result = interpret("{a:1, b:2}.getOrElse(\"b\", () => 12)");
        let expected = Value::Int(2);
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          var counter = 0
          {a:1, b:2}.getOrElse("b", () => {
            counter += 1
            12
          })
          counter
        "#);
        let expected = Value::Int(0);
        assert_eq!(Some(expected), result);

        let result = interpret("{a:1, b:2}.getOrElse(\"c\", () => 12)");
        let expected = Value::Int(12);
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          var counter = 0
          {a:1, b:2}.getOrElse("c", () => {
            counter += 1
            12
          })
          counter
        "#);
        let expected = Value::Int(1);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_map_update() {
        let result = interpret(r#"
          val map = {a:1, b:2}
          map.update("b", n => n + 100)
          map
        "#);
        let expected = map! {
          new_string_obj("a") => Value::Int(1),
          new_string_obj("b") => Value::Int(102)
        };
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val map = {a:1, b:2}
          map.update("c", n => n + 100)
          map
        "#);
        let expected = map! {
          new_string_obj("a") => Value::Int(1),
          new_string_obj("b") => Value::Int(2)
        };
        assert_eq!(Some(expected), result);
    }
}
