use crate::vm::vm;
use crate::vm::compiler::Upvalue;
use std::fmt::{Display, Formatter, Error};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::cell::RefCell;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Obj(Obj),
    Fn { name: String, code: Vec<u8>, upvalues: Vec<Upvalue> },
    Closure { name: String, code: Vec<u8>, captures: Vec<Arc<RefCell<vm::Upvalue>>> },
    Type(String),
    Nil,
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Int(val) => format!("{}", val),
            Value::Float(val) => format!("{}", val),
            Value::Bool(val) => format!("{}", val),
            Value::Obj(o) => o.to_string(),
            Value::Fn { name, .. } |
            Value::Closure { name, .. } => format!("<func {}>", name),
            Value::Type(name) => format!("<type {}>", name),
            Value::Nil => format!("nil"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Obj(o) => match o {
                Obj::StringObj { value } => write!(f, "\"{}\"", *value),
                o @ _ => write!(f, "{}", o.to_string()),
            }
            Value::Fn { name, .. } |
            Value::Closure { name, .. } => write!(f, "<func {}>", name),
            Value::Type(name) => write!(f, "<type {}>", name),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    StringObj { value: Box<String> },
    ArrayObj { value: Vec<Box<Value>> },
    OptionObj { value: Option<Box<Value>> },
    MapObj { value: HashMap<String, Value> },
}

impl Obj {
    pub fn to_string(&self) -> String {
        match self {
            Obj::StringObj { value } => *value.clone(),
            Obj::ArrayObj { value } => {
                let items = value.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("[{}]", items)
            }
            Obj::OptionObj { value } => match value {
                None => format!("None"),
                Some(value) => value.to_string()
            }
            Obj::MapObj { value } => {
                let items = value.iter()
                    .map(|(key, value)| format!("{}: {}", key.to_string(), value.to_string()))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{{ {} }}", items)
            }
        }
    }
}

impl PartialOrd for Obj {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Obj::StringObj { value: v1 }, Obj::StringObj { value: v2 }) => {
                Some(v1.cmp(v2))
            }
            (Obj::ArrayObj { value: v1 }, Obj::ArrayObj { value: v2 }) => {
                if v1.len() < v2.len() {
                    Some(Ordering::Less)
                } else if v1.len() > v2.len() {
                    Some(Ordering::Greater)
                } else {
                    for (i1, i2) in v1.iter().zip(v2.iter()) {
                        if let Some(o) = i1.partial_cmp(&i2) {
                            if o != Ordering::Equal {
                                return Some(o);
                            }
                        }
                    }
                    Some(Ordering::Equal)
                }
            }
            (Obj::OptionObj { value: v1 }, Obj::OptionObj { value: v2 }) => {
                match (v1, v2) {
                    (None, None) => Some(Ordering::Equal),
                    (Some(v1), Some(v2)) => v1.partial_cmp(&v2),
                    (None, Some(_)) => Some(Ordering::Less),
                    (Some(_), None) => Some(Ordering::Greater),
                }
            }
            (_, _) => None
        }
    }
}
