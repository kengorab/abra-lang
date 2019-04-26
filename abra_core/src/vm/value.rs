use std::fmt::{Display, Formatter, Error};
use std::cmp::Ordering;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Obj(Obj),
    Nil,
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Int(val) => format!("{}", val),
            Value::Float(val) => format!("{}", val),
            Value::Bool(val) => format!("{}", val),
            Value::Obj(o) => o.to_string(),
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
                o @ Obj::ArrayObj { .. } => write!(f, "{}", o.to_string()),
            }
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    StringObj { value: Box<String> },
    ArrayObj { value: Vec<Box<Value>> },
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
            (_, _) => None
        }
    }
}
