use crate::typechecker::types::Type;
use crate::vm::value::Value;
use std::collections::HashMap;

trait NativeType {
    fn fields(typ: &Type) -> HashMap<&str, Type>;
    fn get_field_value(&self, field: String) -> &Value;
}

pub fn fields_for_type(typ: &Type) -> Option<HashMap<&str, Type>> {
    match &typ {
        Type::Array(_) => Some(NativeArray::fields(typ)),
        Type::String => Some(NativeString::fields(typ)),
        _ => None
    }
}

pub struct NativeArray {
    pub length: Value,
}

impl NativeType for NativeArray {
    fn fields(typ: &Type) -> HashMap<&str, Type> {
        if let Type::Array(_inner_type) = typ { // <- The inner type is to support any generics
            let mut fields = HashMap::new();
            fields.insert("length", Type::Int);
            fields
        } else {
            unreachable!()
        }
    }

    fn get_field_value(&self, field: String) -> &Value {
        match field.as_str() {
            "length" => &self.length,
            _ => unreachable!()
        }
    }
}

pub struct NativeString {
    pub length: Value,
}

impl NativeType for NativeString {
    fn fields(_typ: &Type) -> HashMap<&str, Type> {
        let mut fields = HashMap::new();
        fields.insert("length", Type::Int);
        fields
    }

    fn get_field_value(&self, field: String) -> &Value {
        match field.as_str() {
            "length" => &self.length,
            _ => unreachable!()
        }
    }
}
