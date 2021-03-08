use crate::vm::value::Value;
use crate::vm::vm::VM;
use std::fmt::{Debug, Formatter};

// If a native function's return type is Unit, it should return None
pub type NativeAbraFn = fn(Option<Value>, Vec<Value>, &mut VM) -> Option<Value>;

#[derive(Clone)]
pub struct NativeFn {
    pub name: &'static str,
    pub receiver: Option<Box<Value>>,
    pub native_fn: NativeAbraFn,
    pub has_return: bool,
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
    pub fn invoke(&self, args: Vec<Value>, vm_ref: &mut VM) -> Option<Value> {
        let func = self.native_fn;
        func(self.receiver.as_ref().map(|v| *v.clone()), args, vm_ref)
    }
}
