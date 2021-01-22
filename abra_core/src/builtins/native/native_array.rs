use abra_native::{AbraType, abra_methods};
use crate::vm::value::Value;
use std::fmt::Debug;
use std::hash::Hash;
use crate::vm::vm::VM;
use crate::builtins::native::to_string;
use itertools::Itertools;
use crate::builtins::native::common::invoke_fn;
use std::collections::{HashSet, HashMap};
use crate::builtins::arguments::Arguments;

#[derive(AbraType, Debug, Clone, Eq, Hash, PartialEq)]
#[abra_type(signature = "Array<T>")]
pub struct NativeArray {
    // This field needs to be public so vararg handlers can access the received array's values
    pub _inner: Vec<Value>,

    #[abra_field(name = "length", field_type = "Int")]
    length: usize,
}

#[abra_methods]
impl NativeArray {
    #[abra_constructor]
    pub(crate) fn new(args: Vec<Value>) -> Self {
        Self { _inner: args, length: 0 }
    }

    #[abra_getter(field = "length")]
    fn get_length(&self) -> Value {
        Value::Int(self._inner.len() as i64)
    }

    #[abra_setter(field = "length")]
    fn set_length(&mut self, value: Value) {
        self.length = *value.as_int() as usize;
    }

    #[abra_to_string]
    fn to_string(&self, vm: &mut VM) -> String {
        let items = self._inner.iter().map(|v| to_string(v, vm)).join(", ");
        format!("[{}]", items)
    }

    #[abra_static_method(signature = "fill<T1>(amount: Int, value: T1): T1[]")]
    fn fill(mut args: Arguments) -> Self {
        let amount = args.next_int() as usize;
        let value = args.next_value();

        let arr = std::iter::repeat(value).take(amount).collect();
        Self::new(arr)
    }

    #[abra_static_method(signature = "fillBy<T1>(amount: Int, fn: (Int) => T1): T1[]")]
    fn fill_by(mut args: Arguments, vm: &mut VM) -> Self {
        let amount = args.next_int() as usize;
        let fill_fn = args.next_value();

        let mut values = Vec::with_capacity(amount);
        for i in 0..(amount as usize) {
            let value = invoke_fn(vm, &fill_fn, vec![Value::Int(i as i64)]);
            values.push(value);
        }

        Self::new(values)
    }

    #[abra_method(signature = "isEmpty(): Bool")]
    fn is_empty(&self) -> Value {
        Value::Bool(self._inner.is_empty())
    }

    #[abra_method(signature = "enumerate(): (T, Int)[]")]
    fn enumerate(&self) -> Self {
        let tuples = self._inner.iter().enumerate()
            .map(|(idx, value)| {
                Value::new_tuple_obj(vec![value.clone(), Value::Int(idx as i64)])
            }).collect();
        Self::new(tuples)
    }

    #[abra_method(signature = "push(item: T, *others: T[])")]
    fn push(&mut self, mut args: Arguments) {
        let item = args.next_value();
        let others = args.varargs();

        self._inner.push(item);
        for item in others {
            self._inner.push(item);
        }
    }

    #[abra_method(signature = "pop(): T?")]
    fn pop(&mut self) -> Value {
        self._inner.pop().unwrap_or(Value::Nil)
    }

    #[abra_method(signature = "popFront(): T?")]
    fn pop_front(&mut self) -> Value {
        if self._inner.is_empty() {
            Value::Nil
        } else {
            self._inner.remove(0)
        }
    }

    #[abra_method(signature = "splitAt(index: Int): (T[], T[])")]
    fn split_at(&self, mut args: Arguments) -> Value {
        let index = args.next_int();

        let (p1, p2) = if index >= self._inner.len() as i64 {
            (self._inner.clone(), vec![])
        } else if index < -(self._inner.len() as i64) {
            (vec![], self._inner.clone())
        } else {
            let split_idx = ((self._inner.len() as i64 + index) % self._inner.len() as i64) as usize;
            let (h1, h2) = self._inner.split_at(split_idx);
            (h1.to_vec().clone(), h2.to_vec().clone())
        };
        let p1 = Value::new_array_obj(p1);
        let p2 = Value::new_array_obj(p2);

        Value::new_tuple_obj(vec![p1, p2])
    }

    #[abra_method(signature = "concat(other: T[]): T[]")]
    fn concat(&self, mut args: Arguments) -> Self {
        let other = args.next_array();

        let new_array = vec![self._inner.clone(), other].concat();

        Self::new(new_array)
    }

    #[abra_method(signature = "map<U>(fn: (T) => U): U[]")]
    fn map(&self, mut args: Arguments, vm: &mut VM) -> Self {
        let callback = args.next_value();

        let mut new_array_items = Vec::new();
        for value in &self._inner {
            let args = vec![value.clone()];
            let value = invoke_fn(vm, &callback, args);
            new_array_items.push(value);
        }

        Self::new(new_array_items)
    }

    #[abra_method(signature = "filter(fn: (T) => Bool): T[]")]
    fn filter(&self, mut args: Arguments, vm: &mut VM) -> Self {
        let callback = args.next_value();

        let mut new_array_items = Vec::new();
        for value in &self._inner {
            let args = vec![value.clone()];
            let ret_val = invoke_fn(vm, &callback, args);
            if let Value::Bool(true) = ret_val {
                new_array_items.push(value.clone());
            }
        }

        Self::new(new_array_items)
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

    #[abra_method(signature = "join(joiner?: String): String")]
    fn join(&self, mut args: Arguments, vm: &mut VM) -> Value {
        let joiner = args.next_string_or_default("");

        let joined = self._inner.iter()
            .map(|v| to_string(v, vm))
            .join(joiner.as_str());
        Value::new_string_obj(joined)
    }

    #[abra_method(signature = "contains(item: T): Bool")]
    fn contains(&self, mut args: Arguments, _vm: &mut VM) -> Value {
        let item = args.next_value();

        Value::Bool(self._inner.contains(&item))
    }

    #[abra_method(signature = "find<U>(fn: (T) => (Bool | U?)): T?")]
    fn find(&self, mut args: Arguments, vm: &mut VM) -> Value {
        let callback = args.next_value();

        let mut iter = self._inner.iter();
        loop {
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
        }
    }

    #[abra_method(signature = "findIndex<U>(fn: (T) => (Bool | U?)): (T, Int)?")]
    fn find_index(&self, mut args: Arguments, vm: &mut VM) -> Value {
        let callback = args.next_value();

        let mut iter = self._inner.iter().enumerate();
        loop {
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
        }
    }

    #[abra_method(signature = "any<U>(fn: (T) => (Bool | U?)): Bool")]
    fn any(&self, mut args: Arguments, vm: &mut VM) -> Value {
        let callback = args.next_value();

        let mut iter = self._inner.iter();
        loop {
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
        }
    }

    #[abra_method(signature = "all<U>(fn: (T) => (Bool | U?)): Bool")]
    fn all(&self, mut args: Arguments, vm: &mut VM) -> Value {
        let callback = args.next_value();

        let mut iter = self._inner.iter();
        loop {
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
        }
    }

    #[abra_method(signature = "none<U>(fn: (T) => (Bool | U?)): Bool")]
    fn none(&self, mut args: Arguments, vm: &mut VM) -> Value {
        let callback = args.next_value();

        let mut iter = self._inner.iter();
        loop {
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
        }
    }

    #[abra_method(signature = "sortBy(fn: (T) => Int, reverse?: Bool): T[]")]
    fn sort_by(&self, mut args: Arguments, vm: &mut VM) -> Self {
        let callback = args.next_value();
        let reverse = args.next_bool_or_default(false);

        let mut sort_values = self._inner.iter().enumerate().map(|(idx, item)| {
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
            .map(|(_, idx)| self._inner[*idx].clone())
            .collect();
        Self::new(items)
    }

    #[abra_method(signature = "dedupe(): T[]")]
    fn dedupe(&self) -> Self {
        let mut new_array_items = vec![];
        let mut seen = HashSet::new();

        for item in &self._inner {
            if seen.contains(item) {
                continue;
            }
            seen.insert(item);
            new_array_items.push(item.clone())
        }

        Self::new(new_array_items)
    }

    #[abra_method(signature = "dedupeBy<U>(fn: (T) => U): T[]")]
    fn dedupe_by(&self, mut args: Arguments, vm: &mut VM) -> Self {
        let callback = args.next_value();

        let mut new_array_items = vec![];
        let mut seen = HashSet::new();

        for item in &self._inner {
            let args = vec![item.clone()];
            let value = invoke_fn(vm, &callback, args);

            if seen.contains(&value) {
                continue;
            }
            seen.insert(value);
            new_array_items.push(item.clone())
        }

        Self::new(new_array_items)
    }

    #[abra_method(signature = "partition<U>(fn: (T) => U): Map<U, T[]>")]
    fn partition(&self, mut args: Arguments, vm: &mut VM) -> Value {
        let callback = args.next_value();

        let mut map = HashMap::new();

        for item in &self._inner {
            let args = vec![item.clone()];
            let value = invoke_fn(vm, &callback, args);

            map.entry(value).or_insert(vec![]).push(item.clone());
        }

        let items = map.into_iter()
            .flat_map(|(k, v)| vec![k, Value::new_array_obj(v)])
            .collect();
        Value::new_map_obj(items)
    }

    #[abra_method(signature = "tally(): Map<T, Int>")]
    fn tally(&self) -> Value {
        let mut map = HashMap::new();

        for item in &self._inner {
            *map.entry(item.clone()).or_insert(0) += 1;
        }

        let items = map.into_iter()
            .flat_map(|(k, v)| vec![k, Value::Int(v)])
            .collect();
        Value::new_map_obj(items)
    }

    #[abra_method(signature = "tallyBy<U>(fn: (T) => U): Map<U, Int>")]
    fn tally_by(&self, mut args: Arguments, vm: &mut VM) -> Value {
        let callback = args.next_value();

        let mut map = HashMap::new();

        for item in &self._inner {
            let args = vec![item.clone()];
            let value = invoke_fn(vm, &callback, args);

            *map.entry(value).or_insert(0) += 1;
        }

        let items = map.into_iter()
            .flat_map(|(k, v)| vec![k, Value::Int(v)])
            .collect();
        Value::new_map_obj(items)
    }

    #[abra_method(signature = "asSet(): Set<T>")]
    fn as_set(&self) -> Value {
        let set = self._inner.iter().map(|v| v.clone()).collect();
        Value::new_set_obj(set)
    }

    #[abra_method(signature = "getOrDefault(key: Int, default: T): T")]
    fn get_or_default(&self, mut args: Arguments, _vm: &mut VM) -> Value {
        let key = args.next_int() as usize;
        let default = args.next_value();

        self._inner.get(key)
            .map_or(default, |v| v.clone())
    }

    #[abra_method(signature = "getOrElse(key: Int, fn: () => T): T")]
    fn get_or_else(&self, mut args: Arguments, vm: &mut VM) -> Value {
        let key = args.next_int() as usize;
        let callback = args.next_value();

        self._inner.get(key)
            .map(|v| v.clone())
            .unwrap_or_else(|| invoke_fn(vm, &callback, vec![]))
    }

    #[abra_method(signature = "update(key: Int, fn: (T) => T)")]
    fn update(&mut self, mut args: Arguments, vm: &mut VM) {
        let key = args.next_int() as usize;
        let callback = args.next_value();

        if let Some(item) = self._inner.get_mut(key) {
            *item = invoke_fn(vm, &callback, vec![item.clone()]);
        }
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
    fn test_array_enumerate() {
        let result = interpret("[].enumerate()");
        let expected = Value::new_array_obj(vec![]);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\", \"b\"].enumerate()");
        let expected = Value::new_array_obj(vec![
            Value::new_tuple_obj(vec![new_string_obj("a"), Value::Int(0)]),
            Value::new_tuple_obj(vec![new_string_obj("b"), Value::Int(1)]),
        ]);
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
