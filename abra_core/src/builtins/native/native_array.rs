use crate::builtins::gen_native_types::{NativeArrayMethodsAndFields};
use crate::vm::value::{Value, Obj};
use crate::vm::vm::VM;
use crate::builtins::native::common::{invoke_fn, to_string};
use std::collections::{HashSet, HashMap};
use itertools::Itertools;

pub type NativeArray = crate::builtins::gen_native_types::NativeArray;

impl NativeArrayMethodsAndFields for crate::builtins::gen_native_types::NativeArray {
    fn field_length(obj: Box<Value>) -> Value {
        if let Value::Obj(obj) = *obj {
            match &*(obj.borrow()) {
                Obj::ArrayObj(value) => Value::Int(value.len() as i64),
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn static_method_fill(_receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let amount = args.next().expect("Array::fill requires 2 arguments");
        let amount = if let Value::Int(i) = amount { i } else { unreachable!() };

        let filler = args.next().expect("Array::fill requires 2 arguments");

        let mut values = Vec::with_capacity(amount as usize);
        for _ in 0..(amount as usize) {
            values.push(filler.clone());
        }

        Some(Value::new_array_obj(values))
    }

    fn static_method_fill_by(_receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let amount = args.next().expect("Array::fillBy requires 2 arguments");
        let amount = if let Value::Int(i) = amount { i } else { unreachable!() };

        let filler_fn = args.next().expect("Array::fillBy requires 2 arguments");

        let mut values = Vec::with_capacity(amount as usize);
        for i in 0..(amount as usize) {
            let value = invoke_fn(vm, &filler_fn, vec![Value::Int(i as i64)]);
            values.push(value);
        }

        Some(Value::new_array_obj(values))
    }

    fn method_to_string(receiver: Option<Value>, _args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        if let Some(obj) = receiver {
            Some(Value::new_string_obj(to_string(&obj, vm)))
        } else { unreachable!() }
    }

    fn method_is_empty(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(value) => Some(Value::Bool(value.is_empty())),
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_enumerate(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let tuples = array.iter().enumerate()
                        .map(|(idx, value)| {
                            Value::new_tuple_obj(vec![value.clone(), Value::Int(idx as i64)])
                        }).collect();
                    Some(Value::new_array_obj(tuples))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_push(receiver: Option<Value>, args: Vec<Value>, _: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let item = args.next().expect("Array::push requires 2 arguments");

        let others = args.next().expect("Array::push requires 2 arguments");
        let others = if let Value::Obj(obj) = others {
            match &*(obj.borrow()) {
                Obj::ArrayObj(vals) => vals.clone(),
                _ => unreachable!()
            }
        } else { vec![] };

        if let Value::Obj(obj) = receiver.unwrap() {
            match *obj.borrow_mut() {
                Obj::ArrayObj(ref mut array) => {
                    array.push(item);
                    for item in others {
                        array.push(item);
                    }
                },
                _ => unreachable!()
            }
        } else { unreachable!() }

        None
    }

    fn method_pop(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match *obj.borrow_mut() {
                Obj::ArrayObj(ref mut array) => {
                    Some(array.pop().unwrap_or(Value::Nil))
                },
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_pop_front(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match *obj.borrow_mut() {
                Obj::ArrayObj(ref mut array) => {
                    if array.is_empty() {
                        Some(Value::Nil)
                    } else {
                        Some(array.remove(0))
                    }
                },
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_split_at(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let item = args.into_iter().next().expect("Array::splitAt requires 1 argument");
        let index = if let Value::Int(index) = item { index } else { unreachable!() };

        if let Value::Obj(obj) = receiver.unwrap() {
            match *obj.borrow_mut() {
                Obj::ArrayObj(ref mut array) => {
                    let tuple = if index >= array.len() as i64 {
                        Value::new_tuple_obj(vec![
                            Value::new_array_obj(array.clone()),
                            Value::new_array_obj(vec![]),
                        ])
                    } else if index < -(array.len() as i64) {
                        Value::new_tuple_obj(vec![
                            Value::new_array_obj(vec![]),
                            Value::new_array_obj(array.clone()),
                        ])
                    } else {
                        let split_idx = ((array.len() as i64 + index) % array.len() as i64) as usize;
                        let (h1, h2) = array.split_at(split_idx);
                        Value::new_tuple_obj(vec![
                            Value::new_array_obj(h1.to_vec().clone()),
                            Value::new_array_obj(h2.to_vec().clone()),
                        ])
                    };

                    Some(tuple)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_concat(receiver: Option<Value>, args: Vec<Value>, _: &mut VM) -> Option<Value> {
        let arg = args.into_iter().next().expect("Array::concat requires 1 argument");
        let mut other_arr = match arg {
            Value::Obj(obj) => match &*obj.borrow() {
                Obj::ArrayObj(other_arr) => other_arr.clone(),
                _ => unreachable!()
            },
            _ => unreachable!()
        };

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut new_arr = Vec::with_capacity(array.len() + other_arr.len());
                    let mut old_arr = array.clone();
                    new_arr.append(&mut old_arr);
                    new_arr.append(&mut other_arr);
                    Some(Value::new_array_obj(new_arr))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_map(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match *obj.borrow() {
                Obj::ArrayObj(ref array) => {
                    let callback = args.into_iter().next().expect("Array::map requires 1 argument");

                    let mut new_array_items = Vec::new();

                    for value in array {
                        let args = vec![value.clone()];
                        let value = invoke_fn(vm, &callback, args);
                        new_array_items.push(value);
                    }

                    Some(Value::new_array_obj(new_array_items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_filter(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::filter requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut new_array_items = Vec::new();

                    for value in array {
                        let args = vec![value.clone()];
                        let ret_val = invoke_fn(vm, &callback, args);
                        if let Value::Bool(true) = ret_val {
                            new_array_items.push(value.clone());
                        }
                    }

                    Some(Value::new_array_obj(new_array_items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_reduce(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let initial_value = args.next().expect("Array::reduce requires 2 arguments");
        let callback = args.next().expect("Array::reduce requires 2 arguments");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut accumulator = initial_value;

                    for value in array {
                        let args = vec![accumulator, value.clone()];
                        accumulator = invoke_fn(vm, &callback, args);
                    }

                    Some(accumulator)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_join(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let joiner = args.into_iter().next().expect("Array::join requires 1 argument");
        let joiner = if let Value::Obj(obj) = joiner {
            match &(*obj.borrow()) {
                Obj::StringObj(s) => s.clone(),
                _ => unreachable!()
            }
        } else if let Value::Nil = joiner {
            "".to_string()
        } else { unreachable!() };

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let joined = array.iter()
                        .map(|v| to_string(v, vm))
                        .join(joiner.as_str());
                    Some(Value::new_string_obj(joined))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_contains(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let item = args.into_iter().next().expect("Array::contains requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let v = array.contains(&item);
                    Some(Value::Bool(v))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_find(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::find requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut iter = array.iter();
                    let return_value = loop {
                        match iter.next() {
                            None => break Value::Nil,
                            Some(value) => {
                                let args = vec![value.clone()];
                                let ret_val = invoke_fn(vm, &callback, args);
                                match ret_val {
                                    Value::Bool(false) | Value::Nil => continue,
                                    _ => break value.clone()
                                }
                            }
                        }
                    };
                    Some(return_value)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_find_index(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::findIndex requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut iter = array.iter().enumerate();
                    let return_value = loop {
                        match iter.next() {
                            None => break Value::Nil,
                            Some((idx, value)) => {
                                let args = vec![value.clone()];
                                let ret_val = invoke_fn(vm, &callback, args);
                                match ret_val {
                                    Value::Bool(false) | Value::Nil => continue,
                                    _ => break Value::new_tuple_obj(vec![value.clone(), Value::Int(idx as i64)])
                                }
                            }
                        }
                    };
                    Some(return_value)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_any(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::any requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut iter = array.iter();
                    let return_value = loop {
                        match iter.next() {
                            None => break Value::Bool(false),
                            Some(value) => {
                                let args = vec![value.clone()];
                                let value = invoke_fn(vm, &callback, args);
                                match value {
                                    Value::Bool(false) | Value::Nil => continue,
                                    _ => break Value::Bool(true)
                                }
                            }
                        }
                    };
                    Some(return_value)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_all(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::all requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut iter = array.iter();
                    let return_value = loop {
                        match iter.next() {
                            None => break Value::Bool(true),
                            Some(value) => {
                                let args = vec![value.clone()];
                                let value = invoke_fn(vm, &callback, args);
                                match value {
                                    Value::Bool(false) | Value::Nil => break Value::Bool(false),
                                    _ => continue
                                }
                            }
                        }
                    };
                    Some(return_value)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_none(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::none requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut iter = array.iter();
                    let return_value = loop {
                        match iter.next() {
                            None => break Value::Bool(true),
                            Some(value) => {
                                let args = vec![value.clone()];
                                let value = invoke_fn(vm, &callback, args);
                                match value {
                                    Value::Bool(false) | Value::Nil => continue,
                                    _ => break Value::Bool(false)
                                }
                            }
                        }
                    };
                    Some(return_value)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_sort_by(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();

        let callback = args.next().expect("Array::sortBy requires 2 arguments");

        let reverse = args.next().expect("Array::sortBy requires 2 arguments");
        let reverse = if let Value::Bool(b) = reverse { b } else { false };

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow_mut()) {
                Obj::ArrayObj(array) => {
                    let mut sort_values = array.iter().enumerate().map(|(idx, item)| {
                        let args = vec![item.clone()];
                        let value = invoke_fn(vm, &callback, args);
                        (value, idx)
                    }).collect::<Vec<_>>();
                    sort_values.sort_by(|v1, v2| {
                        match (&v1.0, &v2.0) {
                            (Value::Int(i1), Value::Int(i2)) => {
                                if reverse {
                                    i2.cmp(&i1)
                                } else {
                                    i1.cmp(&i2)
                                }
                            }
                            _ => unreachable!()
                        }
                    });
                    let items = sort_values.iter()
                        .map(|(_, idx)| array[*idx].clone())
                        .collect();
                    Some(Value::new_array_obj(items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_dedupe(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut new_array_items = vec![];
                    let mut seen = HashSet::new();

                    for item in array {
                        if seen.contains(item) {
                            continue;
                        }
                        seen.insert(item);
                        new_array_items.push(item.clone())
                    }

                    Some(Value::new_array_obj(new_array_items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_dedupe_by(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::dedupeBy requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut new_array_items = vec![];
                    let mut seen = HashSet::new();

                    for item in array {
                        let args = vec![item.clone()];
                        let value = invoke_fn(vm, &callback, args);

                        if seen.contains(&value) {
                            continue;
                        }
                        seen.insert(value);
                        new_array_items.push(item.clone())
                    }

                    Some(Value::new_array_obj(new_array_items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_partition(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::partition requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut map = HashMap::new();

                    for item in array {
                        let args = vec![item.clone()];
                        let value = invoke_fn(vm, &callback, args);

                        map.entry(value).or_insert(vec![]).push(item.clone());
                    }

                    let map = map.into_iter()
                        .map(|(k, v)| (k, Value::new_array_obj(v)))
                        .collect();
                    Some(Value::new_map_obj(map))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_tally(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut map = HashMap::new();

                    for item in array {
                        *map.entry(item.clone()).or_insert(0) += 1;
                    }

                    let map = map.into_iter()
                        .map(|(k, v)| (k, Value::Int(v)))
                        .collect();
                    Some(Value::new_map_obj(map))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_tally_by(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::partition requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut map = HashMap::new();

                    for item in array {
                        let args = vec![item.clone()];
                        let value = invoke_fn(vm, &callback, args);

                        *map.entry(value).or_insert(0) += 1;
                    }

                    let map = map.into_iter()
                        .map(|(k, v)| (k, Value::Int(v)))
                        .collect();
                    Some(Value::new_map_obj(map))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_as_set(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let set = array.into_iter().map(|v| v.clone()).collect();
                    Some(Value::new_set_obj(set))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_get_or_default(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let key = args.next().expect("Array::getOrDefault requires 2 arguments");
        let key = if let Value::Int(i) = key { i as usize } else { unreachable!() };
        let default = args.next().expect("Array::getOrDefault requires 2 arguments");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    array.get(key).map(|v| v.clone()).or(Some(default))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_get_or_else(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let key = args.next().expect("Array::getOrElse requires 2 arguments");
        let key = if let Value::Int(i) = key { i as usize } else { unreachable!() };
        let callback = args.next().expect("Array::getOrElse requires 2 arguments");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    array.get(key)
                        .map(|v| v.clone())
                        .or_else(|| Some(invoke_fn(vm, &callback, vec![])))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_update(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let key = args.next().expect("Array::update requires 2 arguments");
        let key = if let Value::Int(i) = key { i as usize } else { unreachable!() };
        let callback = args.next().expect("Array::update requires 2 arguments");

        if let Value::Obj(obj) = receiver.unwrap() {
            match *(obj.borrow_mut()) {
                Obj::ArrayObj(ref mut array) => {
                    match array.get_mut(key) {
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

    #[test]
    fn test_array_field_length() {
        let result = interpret("[1, 2, 3, 4, 5].length");
        let expected = Value::Int(5);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_static_fill() {
        let result = interpret("Array.fill(0, 123)");
        let expected = int_array!();
        assert_eq!(Some(expected), result);

        let result = interpret("Array.fill(5, 12)");
        let expected = int_array!(12, 12, 12, 12, 12);
        assert_eq!(Some(expected), result);

        let result = interpret("Array.fill(6, \"24\")");
        let expected = string_array!("24", "24", "24", "24", "24", "24");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_static_fill_by() {
        let result = interpret("Array.fillBy(0, i => i + 1)");
        let expected = int_array!();
        assert_eq!(Some(expected), result);

        let result = interpret("Array.fillBy(5, i => i + 1)");
        let expected = int_array!(1, 2, 3, 4, 5);
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          func fib(n: Int): Int = if n <= 1 1 else fib(n - 1) + fib(n - 2)
          Array.fillBy(6, fib)
        "#);
        let expected = int_array!(1, 1, 2, 3, 5, 8);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_to_string() {
        let result = interpret("[1, 2, 3].toString()");
        let expected = new_string_obj("[1, 2, 3]");
        assert_eq!(Some(expected), result);

        let result = interpret("[[1, 2], [3, 4], [5, 6]].toString()");
        let expected = new_string_obj("[[1, 2], [3, 4], [5, 6]]");
        assert_eq!(Some(expected), result);

        let result = interpret("[{ a: 1 }, { b: 3 }].toString()");
        let expected = new_string_obj("[{ a: 1 }, { b: 3 }]");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_is_empty() {
        let result = interpret("[].isEmpty()");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("[1, 2, 3].isEmpty()");
        let expected = Value::Bool(false);
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

        let result = interpret(r#"
          val arr = [1, 2, 3]
          arr.push(4, 5, 6)
          arr
        "#);
        let expected = int_array!(1, 2, 3, 4, 5, 6);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_pop() {
        let result = interpret(r#"
          val arr = [1, 2, 3]
          val last = arr.pop() ?: 0
          arr.concat([last])
        "#);
        let expected = int_array!(1, 2, 3);
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr: Int[] = []
          arr.pop()
        "#);
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_pop_front() {
        let result = interpret(r#"
          val arr = [1, 2, 3]
          val first = arr.popFront() ?: 0
          [first].concat(arr)
        "#);
        let expected = int_array!(1, 2, 3);
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr: Int[] = []
          arr.popFront()
        "#);
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_split_at() {
        let result = interpret(r#"
          val arr = [1, 2, 3, 4, 5, 6, 7]
          arr.splitAt(0)
        "#);
        let expected = tuple!(
            int_array!(),
            int_array!(1, 2, 3, 4, 5, 6, 7)
        );
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr = [1, 2, 3, 4, 5, 6, 7]
          arr.splitAt(1)
        "#);
        let expected = tuple!(
            int_array!(1),
            int_array!(2, 3, 4, 5, 6, 7)
        );
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr = [1, 2, 3, 4, 5, 6, 7]
          arr.splitAt(-1)
        "#);
        let expected = tuple!(
            int_array!(1, 2, 3, 4, 5, 6),
            int_array!(7)
        );
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr = [1, 2, 3, 4, 5, 6, 7]
          arr.splitAt(-8)
        "#);
        let expected = tuple!(
            int_array!(),
            int_array!(1, 2, 3, 4, 5, 6, 7)
        );
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr = [1, 2, 3, 4, 5, 6, 7]
          arr.splitAt(10)
        "#);
        let expected = tuple!(
            int_array!(1, 2, 3, 4, 5, 6, 7),
            int_array!()
        );
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
          func mult1(a: Int): Int = a * 1
          func sub1(a: Int): Int = mult1(a) - 1
          func sameNum(a: Int): Int = sub1(a) + 1
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

    #[test]
    fn test_array_join() {
        let result = interpret("[1, 2, 3, 4, 5].join()");
        let expected = new_string_obj("12345");
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\", \"b\", \"c\"].join(\", \")");
        let expected = new_string_obj("a, b, c");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_contains() {
        let result = interpret("[].contains(5)");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("[1, 2, 3, 4, 5].contains(5)");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("[1, 2, 3, 4].contains(6)");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_find() {
        let result = interpret("[1, 2, 3].find(x => x >= 2)");
        let expected = Value::Int(2);
        assert_eq!(Some(expected), result);

        let result = interpret("[[1, 2], [3, 4]].find(p => p[0])");
        let expected = array![Value::Int(1), Value::Int(2)];
        assert_eq!(Some(expected), result);

        let result = interpret("[[1, 2], [3, 4]].find(p => if p[0] |f| f >= 2)");
        let expected = array![Value::Int(3), Value::Int(4)];
        assert_eq!(Some(expected), result);

        let result = interpret("[1, 2, 3].find(x => x >= 4)");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_find_index() {
        let result = interpret("[1, 2, 3].findIndex(x => x >= 2)");
        let expected = tuple![Value::Int(2), Value::Int(1)];
        assert_eq!(Some(expected), result);

        let result = interpret("[[1, 2], [3, 4]].findIndex(p => p[0])");
        let expected = tuple![
            array![Value::Int(1), Value::Int(2)],
            Value::Int(0)
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("[1, 2, 3].findIndex(x => x >= 4)");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_any() {
        let result = interpret("[1, 2, 3, 4, 5].any(x => x > 4)");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("[1, 2, 3, 4, 5].any(x => x < 0)");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("[[1, 2], [3, 4]].any(p => if p[0] |f| f >= 2)");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_all() {
        let result = interpret("[\"a\", \"bc\", \"def\"].all(w => w.length > 0)");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\", \"bc\", \"def\"].all(w => w.length < 3)");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"1\", \"2\", \"24\"].all(w => w.parseInt())");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\"].all(w => w.parseInt())");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_none() {
        let result = interpret("[\"a\", \"bc\", \"def\"].none(w => w.length > 0)");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\", \"bc\", \"def\"].none(w => w.length < 0)");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"1\", \"2\", \"24\"].none(w => w.parseInt())");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\", \"b\"].none(w => w.parseInt())");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_sort_by() {
        let result = interpret(r#"
          type Person { name: String }
          val people = [
            Person(name: "Ken"),
            Person(name: "Meghan"),
            Person(name: "Brian"),
            Person(name: "Kelsey"),
          ]
          people.sortBy(p => p.name.length).map(p => p.name)
        "#);
        let expected = array![
          new_string_obj("Ken"),
          new_string_obj("Brian"),
          new_string_obj("Meghan"),
          new_string_obj("Kelsey")
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("\
          [1, 8, 3, 6, 1, 11, 5839, 6].sortBy(fn: i => i, reverse: true)
        ");
        let expected = array![
            Value::Int(5839),
            Value::Int(11),
            Value::Int(8),
            Value::Int(6),
            Value::Int(6),
            Value::Int(3),
            Value::Int(1),
            Value::Int(1)
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_dedupe() {
        let result = interpret("[\"a\", \"bc\", \"def\"].dedupe()");
        let expected = array![
            new_string_obj("a"),
            new_string_obj("bc"),
            new_string_obj("def")
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("\
          type Person { name: String }\
          [Person(name: \"Ken\"), Person(name: \"Meg\"), Person(name: \"Ken\")]\
            .dedupe()\
            .map(p => p.name)\
        ");
        let expected = array![
            new_string_obj("Ken"),
            new_string_obj("Meg")
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_dedupe_by() {
        let result = interpret("[\"a\", \"bc\", \"def\"].dedupeBy(w => w.length)");
        let expected = array![
            new_string_obj("a"),
            new_string_obj("bc"),
            new_string_obj("def")
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\", \"bc\", \"def\", \"ghi\"].dedupeBy(w => w.length)");
        let expected = array![
            new_string_obj("a"),
            new_string_obj("bc"),
            new_string_obj("def")
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_partition() {
        let result = interpret("[1, 2, 3, 4, 5].partition(n => n.isEven())");
        let expected = map! {
            Value::Bool(true) => array![Value::Int(2), Value::Int(4)],
            Value::Bool(false) => array![Value::Int(1), Value::Int(3), Value::Int(5)]
        };
        assert_eq!(Some(expected), result);

        let result = interpret(
            "[[1, 1], [1, 2], [2, 1], [2, 2], [3, 1], [3, 2]].partition(p => p[0])"
        );
        let expected = map! {
             Value::Int(1) => array![
                array![Value::Int(1), Value::Int(1)],
                array![Value::Int(1), Value::Int(2)]
            ],
            Value::Int(2) => array![
                array![Value::Int(2), Value::Int(1)],
                array![Value::Int(2), Value::Int(2)]
            ],
            Value::Int(3) => array![
                array![Value::Int(3), Value::Int(1)],
                array![Value::Int(3), Value::Int(2)]
            ]
        };
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_tally() {
        let result = interpret(
            "[1, 2, 3, 4, 3, 2, 1, 2, 1].tally()"
        );
        let expected = map! {
            Value::Int(1) => Value::Int(3),
            Value::Int(2) => Value::Int(3),
            Value::Int(3) => Value::Int(2),
            Value::Int(4) => Value::Int(1)
        };
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_tally_by() {
        let result = interpret("\
          type Person { name: String }\
          [Person(name: \"Ken\"), Person(name: \"Meg\")].tallyBy(p => p.name.length)
        ");
        let expected = map! {
            Value::Int(3) => Value::Int(2)
        };
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_as_set() {
        let result = interpret("\
          [1, 2, 3, 4, 3, 2, 1, 2, 1].asSet()\n\
        ");
        let expected = set![Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(4)];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_get_or_default() {
        let result = interpret("[1, 2, 3].getOrDefault(1, 12)");
        let expected = Value::Int(2);
        assert_eq!(Some(expected), result);

        let result = interpret("[1, 2, 3].getOrDefault(10, 12)");
        let expected = Value::Int(12);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_get_or_else() {
        let result = interpret("[1, 2, 3].getOrElse(1, () => 12)");
        let expected = Value::Int(2);
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          var counter = 0
          [1, 2, 3].getOrElse(1, () => {
            counter += 1
            12
          })
          counter
        "#);
        let expected = Value::Int(0);
        assert_eq!(Some(expected), result);

        let result = interpret("[1, 2, 3].getOrElse(10, () => 12)");
        let expected = Value::Int(12);
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          var counter = 0
          [1, 2, 3].getOrElse(10, () => {
            counter += 1
            12
          })
          counter
        "#);
        let expected = Value::Int(1);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_update() {
        let result = interpret(r#"
          val arr = [1, 2, 3]
          arr.update(1, n => n + 100)
          arr
        "#);
        let expected = array![Value::Int(1), Value::Int(102), Value::Int(3)];
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr = [1, 2, 3]
          arr.update(7, n => n + 100)
          arr
        "#);
        let expected = array![Value::Int(1), Value::Int(2), Value::Int(3)];
        assert_eq!(Some(expected), result);
    }
}
