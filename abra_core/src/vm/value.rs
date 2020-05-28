use crate::vm::vm;
use crate::vm::compiler::Upvalue;
use std::fmt::{Display, Formatter, Error};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::cell::RefCell;
use std::sync::Arc;
use crate::builtins::native_fns::NativeFn;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct FnValue {
    pub name: String,
    pub code: Vec<u8>,
    pub upvalues: Vec<Upvalue>,
    pub receiver: Option<Arc<RefCell<InstanceObj>>>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ClosureValue {
    pub name: String,
    pub code: Vec<u8>,
    pub captures: Vec<Arc<RefCell<vm::Upvalue>>>,
    pub receiver: Option<Arc<RefCell<InstanceObj>>>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct TypeValue {
    pub name: String,
    pub methods: Vec<(String, FnValue)>,
    pub static_fields: Vec<(String, FnValue)>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    /// Represents a compile-time string constant (ie. the name of a function, or the key of a map).
    /// These are only transient values and should not remain on the stack. Compare to an actual,
    /// heap-allocated, run-time Value::Obj(Obj::StringObj) value.
    Str(String),
    Obj(Obj),
    Fn(FnValue),
    Closure(ClosureValue),
    NativeFn(NativeFn),
    Type(TypeValue),
    Nil,
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Int(val) => format!("{}", val),
            Value::Float(val) => format!("{}", val),
            Value::Bool(val) => format!("{}", val),
            Value::Str(val) => val.clone(),
            Value::Obj(o) => o.to_string(),
            Value::Fn(FnValue { name, .. }) |
            Value::Closure(ClosureValue { name, .. }) |
            Value::NativeFn(NativeFn { name, .. }) => format!("<func {}>", name),
            Value::Type(TypeValue { name, .. }) => format!("<type {}>", name),
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
            Value::Str(val) => write!(f, "{}", val),
            Value::Obj(o) => match o {
                Obj::StringObj(StringObj { value, .. }) => write!(f, "\"{}\"", *value),
                o @ _ => write!(f, "{}", o.to_string()),
            }
            Value::Fn(FnValue { name, .. }) |
            Value::Closure(ClosureValue { name, .. }) |
            Value::NativeFn(NativeFn { name, .. }) => write!(f, "<func {}>", name),
            Value::Type(TypeValue { name, .. }) => write!(f, "<type {}>", name),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct StringObj {
    pub value: String,
    pub fields: Vec<Value>,
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct InstanceObj {
    pub typ: Box<Value>,
    pub fields: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    StringObj(StringObj),
    ArrayObj { value: Vec<Box<Value>> },
    MapObj { value: HashMap<String, Value> },
    InstanceObj(Arc<RefCell<InstanceObj>>),
}

impl Obj {
    pub fn new_string_obj(value: String) -> Obj {
        let fields = vec![];
        Obj::StringObj(StringObj { value, fields })
    }
}

impl Obj {
    // TODO: Proper toString impl
    pub fn to_string(&self) -> String {
        match self {
            Obj::StringObj(StringObj { value, .. }) => value.clone(),
            Obj::ArrayObj { value } => {
                let items = value.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("[{}]", items)
            }
            Obj::MapObj { value } => {
                let items = value.iter()
                    .map(|(key, value)| format!("{}: {}", key.to_string(), value.to_string()))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{{ {} }}", items)
            }
            Obj::InstanceObj(inst) => {
                let typ = &inst.borrow().typ;
                match &**typ {
                    Value::Type(TypeValue { name, .. }) => format!("<instance {}>", name),
                    _ => unreachable!("Shouldn't have instances of non-struct types")
                }
            }
        }
    }
}

impl PartialOrd for Obj {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Obj::StringObj(StringObj { value: v1, .. }), Obj::StringObj(StringObj { value: v2, .. })) => {
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
