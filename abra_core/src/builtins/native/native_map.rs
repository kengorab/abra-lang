use abra_native::{AbraType, abra_methods};
use crate::vm::value::Value;
use std::collections::HashMap;
use crate::vm::vm::VM;
use std::hash::{Hash, Hasher};
use crate::builtins::arguments::Arguments;
use crate::builtins::native::common::invoke_fn;

#[derive(AbraType, Debug, Clone, Eq, PartialEq)]
#[abra_type(signature = "Map<K, V>", variant = "MapObj")]
pub struct NativeMap {
    pub _inner: HashMap<Value, Value>,

    #[abra_field(name = "size", field_type = "Int", readonly)]
    size: usize,
}

#[abra_methods]
impl NativeMap {
    #[abra_constructor]
    pub(crate) fn new(values: Vec<Value>) -> Self {
        let mut _inner = HashMap::new();

        let mut values = values.into_iter();
        while let Some(key) = values.next() {
            let value = values.next().expect("Expected an even number of values");
            _inner.insert(key, value);
        }

        Self { _inner, size: 0 }
    }

    #[abra_getter(field = "size")]
    fn get_size(&self) -> Value {
        Value::Int(self._inner.len() as i64)
    }

    #[abra_static_method(signature = "fromPairs<T1, T2>(pairs: (T1, T2)[]): Map<T1, T2>")]
    fn from_pairs(mut args: Arguments) -> Self {
        let pairs = args.next_array();

        let items = pairs.into_iter().flat_map(|p| {
            let tuple = &*p.as_tuple().borrow();
            tuple.clone()
        }).collect();
        Self::new(items)
    }

    #[abra_method(signature = "isEmpty(): Bool")]
    fn is_empty(&self) -> Value {
        Value::Bool(self._inner.is_empty())
    }

    #[abra_method(signature = "enumerate(): (K, V)[]")]
    fn enumerate(&self) -> Value {
        let tuples = self._inner.iter()
            .map(|(key, value)| {
                Value::new_tuple_obj(vec![key.clone(), value.clone()])
            }).collect();
        Value::new_array_obj(tuples)
    }

    #[abra_method(signature = "keys(): Set<K>")]
    fn keys(&self) -> Value {
        let keys = self._inner.keys()
            .map(|k| k.clone())
            .collect();
        Value::new_set_obj(keys)
    }

    #[abra_method(signature = "values(): Set<V>")]
    fn values(&self) -> Value {
        let keys = self._inner.values()
            .map(|v| v.clone())
            .collect();
        Value::new_set_obj(keys)
    }

    #[abra_method(signature = "entries(): Set<(K, V)>")]
    fn entries(&self) -> Value {
        let entries = self._inner.iter()
            .map(|(k, v)| Value::new_tuple_obj(vec![k.clone(), v.clone()]))
            .collect();
        Value::new_set_obj(entries)
    }

    #[abra_method(signature = "containsKey(key: K): Bool")]
    fn contains_key(&self, mut args: Arguments) -> Value {
        let key = args.next_value();

        Value::Bool(self._inner.contains_key(&key))
    }

    #[abra_method(signature = "mapValues<U>(fn: (K, V) => U): Map<K, U>")]
    fn map_values(&self, mut args: Arguments, vm: &mut VM) -> Self {
        let callback = args.next_value();

        let items = self._inner.iter()
            .flat_map(|(k, v)| {
                let args = vec![k.clone(), v.clone()];
                let value = invoke_fn(vm, &callback, args);
                vec![k.clone(), value]
            })
            .collect();

        Self::new(items)
    }

    #[abra_method(signature = "getOrDefault(key: K, default: V): V")]
    fn get_or_default(&self, mut args: Arguments) -> Value {
        let key = args.next_value();
        let default = args.next_value();

        match self._inner.get(&key) {
            Some(value) => value.clone(),
            None => default
        }
    }

    #[abra_method(signature = "getOrElse(key: K, fn: () => V): V")]
    fn get_or_else(&self, mut args: Arguments, vm: &mut VM) -> Value {
        let key = args.next_value();
        let callback = args.next_value();

        match self._inner.get(&key) {
            Some(value) => value.clone(),
            None => invoke_fn(vm, &callback, vec![])
        }
    }

    #[abra_method(signature = "update(key: K, fn: (V) => V)")]
    fn update(&mut self, mut args: Arguments, vm: &mut VM) {
        let key = args.next_value();
        let callback = args.next_value();

        if let Some(item) = self._inner.get_mut(&key) {
            *item = invoke_fn(vm, &callback, vec![item.clone()]);
        }
    }
}

impl Hash for NativeMap {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        for (k, v) in &self._inner {
            k.hash(hasher);
            v.hash(hasher);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::native::test_utils::{interpret, new_string_obj, interpret_get_result};
    use crate::vm::value::Value;

    #[test]
    fn test_map_field_size() {
        let result = interpret("{}.size");
        let expected = Value::Int(0);
        assert_eq!(Some(expected), result);

        let result = interpret("{ a: 123, b: true }.size");
        let expected = Value::Int(2);
        assert_eq!(Some(expected), result);

        // Setting size should produce an error
        let is_err = interpret_get_result("{ a: 4 }.size = 8").is_err();
        assert!(is_err);
    }

    #[test]
    fn test_map_static_from_pairs() {
        let result = interpret("Map.fromPairs([])");
        let expected = Value::new_map_obj(vec![]);
        assert_eq!(result, Some(expected));

        let result = interpret("Map.fromPairs([(\"a\", 123), (\"b\", 456)])");
        let expected = map! {
            new_string_obj("a") => Value::Int(123),
            new_string_obj("b") => Value::Int(456)
        };
        assert_eq!(result, Some(expected));
    }

    #[test]
    fn test_map_to_string() {
        let result = interpret("{ a: [1, 2], b: [3, 4] }.toString()");
        let expecteds = vec![
            new_string_obj("{ a: [1, 2], b: [3, 4] }"),
            new_string_obj("{ b: [3, 4], a: [1, 2] }"),
        ];
        assert!(expecteds.contains(&result.unwrap()));

        let result = interpret("{ a: true, b: false }.toString()");
        let expecteds = vec![
            new_string_obj("{ a: true, b: false }"),
            new_string_obj("{ b: false, a: true }"),
        ];
        assert!(expecteds.contains(&result.unwrap()));
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
    fn test_map_enumerate() {
        let result = interpret("{}.enumerate()");
        let expected = Value::new_array_obj(vec![]);
        assert_eq!(Some(expected), result);

        let result = interpret("{ a: 123, }.enumerate()");
        let expected = Value::new_array_obj(vec![
            Value::new_tuple_obj(vec![new_string_obj("a"), Value::Int(123)]),
        ]);
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
