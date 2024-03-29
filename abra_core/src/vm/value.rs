use std::hash::{Hash, Hasher};
use crate::builtins::prelude::{NativeArray, NativeMap, NativeSet, NativeString};
use crate::builtins::native_value_trait::NativeValue;
use crate::common::util::integer_decode;
use crate::vm::vm;
use crate::vm::compiler::Upvalue;
use std::fmt::{Display, Formatter, Error, Debug};
use std::cell::RefCell;
use std::sync::Arc;
use crate::vm::opcode::Opcode;
use itertools::Itertools;
use crate::vm::vm::VM;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FnValue {
    pub name: String,
    pub code: Vec<Opcode>,
    pub upvalues: Vec<Upvalue>,
    pub receiver: Option<Box<Value>>,
}

impl Hash for FnValue {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.name.hash(hasher);
        self.code.hash(hasher);
        self.upvalues.hash(hasher);
        self.receiver.hash(hasher);
        hasher.finish();
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClosureValue {
    pub name: String,
    pub code: Vec<Opcode>,
    pub captures: Vec<Arc<RefCell<vm::Upvalue>>>,
    pub receiver: Option<Box<Value>>,
}

impl Hash for ClosureValue {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.name.hash(hasher);
        self.code.hash(hasher);
        for capture in &self.captures {
            let uv = &*capture.borrow();
            uv.hash(hasher);
        }
        self.receiver.hash(hasher);
        hasher.finish();
    }
}

type NativeAbraFn = fn(Option<Value>, Vec<Value>, &mut VM) -> Value;

#[derive(Clone)]
pub struct NativeFn {
    pub name: &'static str,
    pub receiver: Option<Box<Value>>,
    pub native_fn: NativeAbraFn,
}

impl Debug for NativeFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeFn {{ name: {}, .. }}", self.name)
    }
}

impl PartialEq for NativeFn {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(other.name)
    }
}

impl NativeFn {
    pub fn invoke(&self, args: Vec<Value>, vm_ref: &mut VM) -> Value {
        let func = self.native_fn;
        func(self.receiver.as_ref().map(|v| *v.clone()), args, vm_ref)
    }
}

#[derive(Debug, Default, Clone, Hash, Eq, PartialEq)]
pub struct TypeValue {
    pub name: String,
    pub module_name: String,
    pub constructor: Option<fn(usize, Vec<Value>) -> Value>,
    pub fields: Vec<String>,
    pub methods: Vec<(String, Value)>,
    pub static_fields: Vec<(String, Value)>,
}

impl TypeValue {
    pub fn construct(&self, type_id: usize, args: Vec<Value>) -> Value {
        match self.constructor {
            Some(constructor) => constructor(type_id, args),
            None => Value::new_instance_obj(type_id, args)
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct EnumValue {
    pub name: String,
    pub module_name: String,
    pub variants: Vec<(String, Value)>,
    pub methods: Vec<(String, Value)>,
    pub static_fields: Vec<(String, Value)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    StringObj(Arc<RefCell<NativeString>>),
    ArrayObj(Arc<RefCell<NativeArray>>),
    TupleObj(Arc<RefCell<Vec<Value>>>),
    SetObj(Arc<RefCell<NativeSet>>),
    MapObj(Arc<RefCell<NativeMap>>),
    InstanceObj(Arc<RefCell<InstanceObj>>),
    NativeInstanceObj(Arc<RefCell<NativeInstanceObj>>),
    EnumInstanceObj(Arc<RefCell<EnumInstanceObj>>),
    NativeEnumInstanceObj(Arc<RefCell<NativeEnumInstanceObj>>),
    Fn(FnValue),
    Closure(ClosureValue),
    NativeFn(NativeFn),
    Type(TypeValue),
    Enum(EnumValue),
    Module(String),
    Nil,
}

impl Value {
    pub fn new_string_obj(value: String) -> Value {
        Value::StringObj(Arc::new(RefCell::new(NativeString::create(value))))
    }

    pub fn new_array_obj(values: Vec<Value>) -> Value {
        Value::ArrayObj(Arc::new(RefCell::new(NativeArray::new(values))))
    }

    pub fn new_tuple_obj(values: Vec<Value>) -> Value {
        Value::TupleObj(Arc::new(RefCell::new(values)))
    }

    pub fn new_set_obj(values: Vec<Value>) -> Value {
        Value::SetObj(Arc::new(RefCell::new(NativeSet::new(values))))
    }

    pub fn new_map_obj(items: Vec<Value>) -> Value {
        Value::MapObj(Arc::new(RefCell::new(NativeMap::new(items))))
    }

    pub fn new_instance_obj(type_id: usize, fields: Vec<Value>) -> Value {
        let inst = InstanceObj { type_id, fields };
        Value::InstanceObj(Arc::new(RefCell::new(inst)))
    }

    pub fn new_native_instance_obj(type_id: usize, inst: Box<dyn NativeValue>) -> Value {
        let inst = NativeInstanceObj { type_id, inst };
        Value::NativeInstanceObj(Arc::new(RefCell::new(inst)))
    }

    pub fn new_enum_instance_obj(o: EnumInstanceObj) -> Value {
        Value::EnumInstanceObj(Arc::new(RefCell::new(o)))
    }

    pub fn new_native_enum_instance_obj(type_id: usize, idx: usize, inst: Box<dyn NativeValue>) -> Value {
        let inst = NativeEnumInstanceObj { type_id, idx, inst };
        Value::NativeEnumInstanceObj(Arc::new(RefCell::new(inst)))
    }

    pub fn bind_fn_value(&mut self, instance: Value) {
        match self {
            Value::Fn(fn_value) => fn_value.receiver = Some(Box::new(instance)),
            Value::NativeFn(native_fn_value) => native_fn_value.receiver = Some(Box::new(instance)),
            _ => unreachable!()
        }
    }

    pub fn as_int(&self) -> &i64 {
        if let Value::Int(i) = self { i } else { unreachable!() }
    }

    pub fn as_float(&self) -> &f64 {
        if let Value::Float(f) = self { f } else { unreachable!() }
    }

    pub fn as_bool(&self) -> &bool {
        if let Value::Bool(b) = self { b } else { unreachable!() }
    }

    pub fn as_string(&self) -> &Arc<RefCell<NativeString>> {
        if let Value::StringObj(o) = self { o } else { unreachable!() }
    }

    pub fn as_array(&self) -> &Arc<RefCell<NativeArray>> {
        if let Value::ArrayObj(o) = self { o } else { unreachable!() }
    }

    pub fn as_tuple(&self) -> &Arc<RefCell<Vec<Value>>> {
        if let Value::TupleObj(o) = self { o } else { unreachable!() }
    }

    pub fn as_set(&self) -> &Arc<RefCell<NativeSet>> {
        if let Value::SetObj(o) = self { o } else { unreachable!() }
    }

    pub fn as_map(&self) -> &Arc<RefCell<NativeMap>> {
        if let Value::MapObj(o) = self { o } else { unreachable!() }
    }

    pub fn as_instance_obj(&self) -> &Arc<RefCell<InstanceObj>> {
        if let Value::InstanceObj(o) = self { o } else { unreachable!() }
    }

    pub fn as_enum_variant(&self) -> &Arc<RefCell<EnumInstanceObj>> {
        if let Value::EnumInstanceObj(o) = self { o } else { unreachable!() }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::StringObj(o) => write!(f, "\"{}\"", o.borrow()._inner),
            Value::ArrayObj(o) => {
                let items = &*o.borrow()._inner.iter()
                    .map(|v| v.to_string())
                    .join(", ");
                write!(f, "[{}]", items)
            }
            Value::TupleObj(o) => {
                let items = &*o.borrow().iter()
                    .map(|v| v.to_string())
                    .join(", ");
                write!(f, "({})", items)
            }
            Value::SetObj(o) => {
                let items = &*o.borrow()._inner.iter()
                    .map(|v| v.to_string())
                    .join(", ");
                write!(f, "#{{{}}}", items)
            }
            Value::MapObj(o) => {
                let items = &*o.borrow()._inner.iter()
                    .map(|(k, v)| format!("{}: {}", k.to_string(), v.to_string()))
                    .join(", ");
                write!(f, "{{ {} }}", items)
            }
            Value::InstanceObj(o) => {
                let inst = &*o.borrow();
                write!(f, "<instance type_id={:?}>", &inst.type_id)
            }
            Value::NativeInstanceObj(o) => {
                let inst = &*o.borrow();
                write!(f, "<instance type_id={:?}>", &inst.type_id)
            }
            Value::EnumInstanceObj(o) => {
                let inst = &*o.borrow();
                write!(f, "<enum type_id={:?}>", &inst.type_id)
            }
            Value::NativeEnumInstanceObj(o) => {
                let inst = &*o.borrow();
                write!(f, "<enum type_id={:?}>", &inst.type_id)
            }
            Value::Fn(FnValue { name, .. }) |
            Value::Closure(ClosureValue { name, .. }) => write!(f, "<func {}>", name),
            Value::NativeFn(NativeFn { name, .. }) => write!(f, "<func {}>", name),
            Value::Type(TypeValue { name, .. }) => write!(f, "<type {}>", name),
            Value::Enum(EnumValue { name, .. }) => write!(f, "<enum {}>", name),
            Value::Module(module_name) => write!(f, "<module {}>", &module_name),
            Value::Nil => write!(f, "None"),
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match self {
            Value::Int(i) => i.hash(hasher),
            Value::Float(f) => integer_decode(*f).hash(hasher),
            Value::Bool(b) => b.hash(hasher),
            Value::StringObj(o) => (&*o.borrow()).hash(hasher),
            Value::ArrayObj(o) => (&*o.borrow()).hash(hasher),
            Value::TupleObj(o) => (&*o.borrow()).hash(hasher),
            Value::SetObj(o) => (&*o.borrow()).hash(hasher),
            Value::MapObj(o) => (&*o.borrow()).hash(hasher),
            Value::InstanceObj(o) => (&*o.borrow()).hash(hasher),
            Value::NativeInstanceObj(o) => (&*o.borrow()).inst.hash(hasher),
            Value::EnumInstanceObj(o) => (&*o.borrow()).hash(hasher),
            Value::NativeEnumInstanceObj(o) => (&*o.borrow()).inst.hash(hasher),
            Value::Fn(f) => f.hash(hasher),
            Value::Closure(c) => c.hash(hasher),
            Value::NativeFn(NativeFn { name, receiver, .. }) => {
                name.hash(hasher);
                if let Some(receiver) = receiver {
                    receiver.hash(hasher);
                }
            }
            Value::Type(tv) => tv.hash(hasher),
            Value::Enum(ev) => ev.hash(hasher),
            Value::Module(module_id) => module_id.hash(hasher),
            Value::Nil => 0.hash(hasher)
        }
        hasher.finish();
    }
}

impl Eq for Value {}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct InstanceObj {
    pub type_id: usize,
    pub fields: Vec<Value>,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct EnumInstanceObj {
    pub type_id: usize,
    pub idx: usize,
    pub values: Option<Vec<Value>>,
}

#[derive(Debug, Clone, Hash, Eq)]
pub struct EnumVariantObj {
    pub enum_name: String,
    pub enum_module_name: String,
    pub name: String,
    pub idx: usize,
    pub methods: Vec<Value>,
    pub arity: usize,
    pub values: Option<Vec<Value>>,
}

impl PartialEq for EnumVariantObj {
    fn eq(&self, other: &Self) -> bool {
        self.enum_module_name == other.enum_module_name &&
            self.enum_name == other.enum_name &&
            self.idx == other.idx
    }
}

#[derive(Debug)]
pub struct NativeInstanceObj {
    pub type_id: usize,
    pub inst: Box<dyn NativeValue>,
}

impl PartialEq for NativeInstanceObj {
    fn eq(&self, other: &Self) -> bool {
        self.inst.is_equal(&other.inst)
    }
}

#[derive(Debug)]
pub struct NativeEnumInstanceObj {
    pub type_id: usize,
    pub idx: usize,
    pub inst: Box<dyn NativeValue>,
}

impl PartialEq for NativeEnumInstanceObj {
    fn eq(&self, other: &Self) -> bool {
        self.inst.is_equal(&other.inst)
    }
}
