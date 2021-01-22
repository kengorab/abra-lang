use crate::vm::value::{Value, Obj};
use std::vec::IntoIter;
use std::collections::HashSet;

#[derive(Debug)]
pub struct Arguments<'a> {
    fn_name: &'a str,
    expected_num_args: usize,
    args: IntoIter<Value>,
}

impl<'a> Arguments<'a> {
    pub fn new(fn_name: &'a str, expected_num_args: usize, args: Vec<Value>) -> Self {
        Self { fn_name, expected_num_args, args: args.into_iter() }
    }

    pub fn next_int(&mut self) -> i64 {
        match self.args.next() {
            Some(Value::Int(i)) => i,
            Some(v) => unreachable!(format!("Expected Int, received {}", v)),
            None => unreachable!(self.error_str()),
        }
    }

    pub fn next_int_or_default(&mut self, default: i64) -> i64 {
        match self.args.next() {
            Some(Value::Int(i)) => i,
            Some(Value::Nil) => default,
            Some(v) => unreachable!(format!("Expected Int, received {}", v)),
            None => unreachable!(self.error_str()),
        }
    }

    pub fn next_float(&mut self) -> f64 {
        match self.args.next() {
            Some(Value::Float(f)) => f,
            Some(v) => unreachable!(format!("Expected Float, received {}", v)),
            None => unreachable!(self.error_str()),
        }
    }

    pub fn next_float_or_default(&mut self, default: f64) -> f64 {
        match self.args.next() {
            Some(Value::Float(f)) => f,
            Some(Value::Nil) => default,
            Some(v) => unreachable!(format!("Expected Float, received {}", v)),
            None => unreachable!(self.error_str()),
        }
    }

    pub fn next_bool(&mut self) -> bool {
        match self.args.next() {
            Some(Value::Bool(b)) => b,
            Some(v) => unreachable!(format!("Expected Bool, received {}", v)),
            None => unreachable!(self.error_str()),
        }
    }

    pub fn next_bool_or_default(&mut self, default: bool) -> bool {
        match self.args.next() {
            Some(Value::Bool(b)) => b,
            Some(Value::Nil) => default,
            Some(v) => unreachable!(format!("Expected Bool, received {}", v)),
            None => unreachable!(self.error_str()),
        }
    }

    pub fn next_string(&mut self) -> String {
        match self.args.next() {
            Some(Value::Obj(obj)) => {
                match &*(obj.borrow()) {
                    Obj::StringObj(s) => s.clone(),
                    _ => unreachable!()
                }
            }
            Some(v) => unreachable!(format!("Expected String, received {}", v)),
            None => unreachable!(self.error_str()),
        }
    }

    pub fn next_string_or_default<S: AsRef<str>>(&mut self, default: S) -> String {
        match self.args.next() {
            Some(Value::Obj(obj)) => {
                match &*(obj.borrow()) {
                    Obj::StringObj(s) => s.clone(),
                    _ => unreachable!()
                }
            }
            Some(Value::Nil) => default.as_ref().to_string(),
            Some(v) => unreachable!(format!("Expected String, received {}", v)),
            None => unreachable!(self.error_str()),
        }
    }

    pub fn next_array(&mut self) -> Vec<Value> {
        match self.args.next() {
            Some(Value::Obj(obj)) => {
                match &*(obj.borrow()) {
                    Obj::NativeInstanceObj(i) => i.as_array().unwrap()._inner.clone(),
                    _ => unreachable!()
                }
            }
            Some(v) => unreachable!(format!("Expected Array, received {}", v)),
            None => unreachable!(self.error_str()),
        }
    }

    pub fn next_set(&mut self) -> HashSet<Value> {
        match self.args.next() {
            Some(Value::Obj(obj)) => {
                match &*(obj.borrow()) {
                    Obj::NativeInstanceObj(i) => i.as_set().unwrap()._inner.clone(),
                    _ => unreachable!()
                }
            }
            Some(v) => unreachable!(format!("Expected Set, received {}", v)),
            None => unreachable!(self.error_str()),
        }
    }

    pub fn next_value(&mut self) -> Value {
        match self.args.next() {
            Some(v) => v,
            None => unreachable!(self.error_str()),
        }
    }

    pub fn varargs(mut self) -> Vec<Value> {
        // Note:   ^ consumes self, since no other args can be accessed after varargs
        match self.args.next() {
            Some(Value::Obj(obj)) => {
                match &*(obj.borrow()) {
                    Obj::NativeInstanceObj(i) => i.as_array().unwrap()._inner.clone(),
                    _ => unreachable!()
                }
            }
            Some(Value::Nil) => vec![],
            Some(v) => unreachable!(format!("Expected Array, received {}", v)),
            None => unreachable!(self.error_str()),
        }
    }

    fn error_str(&self) -> String {
        let pluralization = if self.expected_num_args == 1 { "" } else { "s" };
        format!("{} requires {} argument{}", self.fn_name, self.expected_num_args, pluralization)
    }
}
