use std::fmt::{Display, Formatter, Error};
use std::cmp::Ordering;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Obj(Obj),
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Int(val) => format!("{}", val),
            Value::Float(val) => format!("{}", val),
            Value::Bool(val) => format!("{}", val),
            Value::Obj(o) => o.to_string(),
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
                Obj::StringObj { value } => write!(f, "\"{}\"", *value)
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Obj {
    StringObj { value: Box<String> }
}

impl Obj {
    pub fn to_string(&self) -> String {
        match self {
            Obj::StringObj { value } => *value.clone(),
        }
    }
}

impl PartialOrd for Obj {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Obj::StringObj { value: v1 }, Obj::StringObj { value: v2 }) => {
                Some(v1.cmp(v2))
            }
        }
    }
}
