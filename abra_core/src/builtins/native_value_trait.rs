use crate::vm::value::Value;
use crate::typechecker::types::{StructType, EnumType};
use std::fmt::Debug;
use downcast_rs::Downcast;
use std::hash::{Hash, Hasher};
use crate::vm::vm::VM;

pub trait NativeTyp {
    fn is_struct() -> bool where Self: Sized;
    fn get_struct_type() -> StructType where Self: Sized;
    fn get_enum_type() -> EnumType where Self: Sized;
}

pub trait NativeValue: NativeTyp + DynHash + Debug + Downcast {
    fn construct(type_id: usize, args: Vec<Value>) -> Value where Self: Sized;
    fn get_type_value() -> Value where Self: Sized;

    fn is_equal(&self, other: &Box<dyn NativeValue>) -> bool;
    fn method_to_string(&self, vm: &mut VM) -> Value;

    fn get_field_values(&self) -> Vec<Value>;

    fn get_field_value(&self, field_idx: usize) -> Value;
    fn set_field_value(&mut self, field_idx: usize, value: Value);
}

pub trait DynHash {
    fn dyn_hash(&self, state: &mut dyn Hasher);
}

impl<H: Hash + ?Sized> DynHash for H {
    fn dyn_hash(&self, mut state: &mut dyn Hasher) {
        self.hash(&mut state);
    }
}

impl Hash for dyn NativeValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.dyn_hash(state);
    }
}

// Use downcast_rs in order to enable downcasting `dyn NativeValue` instances
// to their underlying types.
downcast_rs::impl_downcast!(NativeValue);

// Note: If Clone is ever needed for NativeValue/NativeType, use the dyn-clone crate
