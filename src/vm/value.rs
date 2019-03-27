use std::fmt::{Display, Formatter, Error};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Obj(Obj),
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Int(val) => format!("{}", val),
            Value::Float(val) => format!("{}", val),
            Value::Obj(o) => o.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Obj(o) => match o {
                Obj::StringObj { value } => write!(f, "\"{}\"", *value)
            }
        }
    }
}
