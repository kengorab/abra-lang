use abra_native::{AbraType, abra_methods};
use crate::vm::value::Value;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use crate::vm::vm::VM;
use crate::builtins::native::common::invoke_fn;
use std::collections::HashSet;
use crate::builtins::arguments::Arguments;

#[derive(AbraType, Debug, Clone, Eq, PartialEq)]
#[abra_type(signature = "Set<T>", variant = "SetObj")]
pub struct NativeSet {
    pub _inner: HashSet<Value>,

    #[abra_field(name = "size", field_type = "Int", settable = false)]
    size: usize,
}

#[abra_methods]
impl NativeSet {
    #[abra_constructor]
    pub(crate) fn new(args: Vec<Value>) -> Self {
        Self { _inner: args.into_iter().collect(), size: 0 }
    }

    #[abra_getter(field = "size")]
    fn get_size(&self) -> Value {
        Value::Int(self._inner.len() as i64)
    }

    #[abra_method(signature = "isEmpty(): Bool")]
    fn is_empty(&self) -> Value {
        Value::Bool(self._inner.is_empty())
    }

    #[abra_method(signature = "enumerate(): (T, Int)[]")]
    fn enumerate(&self) -> Value {
        let tuples = self._inner.iter().enumerate()
            .map(|(idx, value)| {
                Value::new_tuple_obj(vec![value.clone(), Value::Int(idx as i64)])
            }).collect();
        Value::new_array_obj(tuples)
    }

    #[abra_method(signature = "contains(item: T): Bool")]
    fn contains(&self, mut args: Arguments) -> Value {
        let item = args.next_value();
        Value::Bool(self._inner.contains(&item))
    }

    #[abra_method(signature = "insert(item: T)")]
    fn insert(&mut self, mut args: Arguments) {
        let item = args.next_value();
        self._inner.insert(item);
    }

    #[abra_method(signature = "remove(item: T)")]
    fn remove(&mut self, mut args: Arguments) {
        let item = args.next_value();
        self._inner.remove(&item);
    }

    #[abra_method(signature = "map<U>(fn: (T) => U): U[]")]
    fn map(&mut self, mut args: Arguments, vm: &mut VM) -> Value {
        let callback = args.next_value();

        let mut array_items = Vec::new();
        for value in &self._inner {
            let args = vec![value.clone()];
            let value = invoke_fn(vm, &callback, args);
            array_items.push(value);
        }

        Value::new_array_obj(array_items)
    }

    #[abra_method(signature = "filter(fn: (T) => Bool): Set<T>")]
    fn filter(&mut self, mut args: Arguments, vm: &mut VM) -> Self {
        let callback = args.next_value();

        let mut new_set_items = Vec::new();
        for value in &self._inner {
            let args = vec![value.clone()];
            let ret_val = invoke_fn(vm, &callback, args);
            if let Value::Bool(true) = ret_val {
                new_set_items.push(value.clone());
            }
        }

        Self::new(new_set_items)
    }

    #[abra_method(signature = "reduce<U>(initialValue: U, fn: (U, T) => U): U")]
    fn reduce(&self, mut args: Arguments, vm: &mut VM) -> Value {
        let initial_value = args.next_value();
        let callback = args.next_value();

        let mut accumulator = initial_value;
        for value in &self._inner {
            let args = vec![accumulator, value.clone()];
            accumulator = invoke_fn(vm, &callback, args);
        }

        accumulator
    }

    #[abra_method(signature = "asArray(): T[]")]
    fn as_array(&self) -> Value {
        let items = self._inner.iter().map(|v| v.clone()).collect();
        Value::new_array_obj(items)
    }

    #[abra_method(signature = "union(other: Set<T>): Set<T>")]
    fn union(&self, mut args: Arguments) -> Self {
        let other = args.next_set();

        let new_set = self._inner.union(&other).map(|v| v.clone()).collect();
        Self::new(new_set)
    }

    #[abra_method(signature = "difference(other: Set<T>): Set<T>")]
    fn difference(&self, mut args: Arguments) -> Self {
        let other = args.next_set();

        let new_set = self._inner.difference(&other).map(|v| v.clone()).collect();
        Self::new(new_set)
    }

    #[abra_method(signature = "intersection(other: Set<T>): Set<T>")]
    fn intersection(&self, mut args: Arguments) -> Self {
        let other = args.next_set();

        let new_set = self._inner.intersection(&other).map(|v| v.clone()).collect();
        Self::new(new_set)
    }
}

impl Hash for NativeSet {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        for item in &self._inner {
            item.hash(hasher);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::native::test_utils::{interpret, new_string_obj};
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
    fn test_set_to_string() {
        let result = interpret("#{1, 2, 3}.toString()");
        let expecteds = vec![
            new_string_obj("#{1, 2, 3}"),
            new_string_obj("#{1, 3, 2}"),
            new_string_obj("#{2, 3, 1}"),
            new_string_obj("#{2, 1, 3}"),
            new_string_obj("#{3, 2, 1}"),
            new_string_obj("#{3, 1, 2}"),
        ];
        assert!(expecteds.contains(&result.unwrap()));

        let result = interpret("#{[1, 2], [3, 4]}.toString()");
        let expecteds = vec![
            new_string_obj("#{[1, 2], [3, 4]}"),
            new_string_obj("#{[3, 4], [1, 2]}"),
        ];
        assert!(expecteds.contains(&result.unwrap()));

        let result = interpret("#{(1, 2), (3, 4)}.toString()");
        let expecteds = vec![
            new_string_obj("#{(1, 2), (3, 4)}"),
            new_string_obj("#{(3, 4), (1, 2)}"),
        ];
        assert!(expecteds.contains(&result.unwrap()));
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
    fn test_set_enumerate() {
        let result = interpret("#{}.enumerate()");
        let expected = Value::new_array_obj(vec![]);
        assert_eq!(Some(expected), result);

        let result = interpret("#{\"a\"}.enumerate()");
        let expected = Value::new_array_obj(vec![
            Value::new_tuple_obj(vec![new_string_obj("a"), Value::Int(0)]),
        ]);
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
    fn test_set_insert() {
        let result = interpret(r#"
          val set = #{1}
          set.insert(3)
          set
        "#);
        let expected = set![Value::Int(1), Value::Int(3)];
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val set = #{1}
          set.insert(3)
          set.insert(3)
          set
        "#);
        let expected = set![Value::Int(1), Value::Int(3)];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_set_remove() {
        let result = interpret(r#"
          val set = #{1}
          set.remove(1)
          set
        "#);
        let expected = set![];
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val set: Set<Int> = #{}
          set.remove(1)
          set
        "#);
        let expected = set![];
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
