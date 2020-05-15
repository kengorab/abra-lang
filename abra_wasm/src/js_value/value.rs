use abra_core::builtins::native_fns::NativeFn;
use abra_core::vm::value::{Value, Obj, FnValue, ClosureValue, TypeValue};
use serde::{Serializer, Serialize};

pub struct JsWrappedValue<'a>(pub &'a Value);

impl<'a> Serialize for JsWrappedValue<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match &self.0 {
            Value::Int(val) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "int")?;
                obj.serialize_entry("value", &val)?;
                obj.end()
            }
            Value::Float(val) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "float")?;
                obj.serialize_entry("value", &val)?;
                obj.end()
            }
            Value::Bool(val) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "bool")?;
                obj.serialize_entry("value", &val)?;
                obj.end()
            }
            Value::Obj(o) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "obj")?;
                obj.serialize_entry("value", &JsWrappedObjValue(o))?;
                obj.end()
            }
            Value::Fn(FnValue { name: fn_name, .. }) |
            Value::Closure(ClosureValue { name: fn_name, .. }) |
            Value::NativeFn(NativeFn { name: fn_name, .. }) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "fn")?;
                obj.serialize_entry("name", &fn_name)?;
                obj.end()
            }
            Value::Type(TypeValue { name }) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "type")?;
                obj.serialize_entry("name", &name)?;
                obj.end()
            }
            Value::Nil => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("kind", "nil")?;
                obj.end()
            }
        }
    }
}

pub struct JsWrappedObjValue<'a>(pub &'a Obj);

impl<'a> Serialize for JsWrappedObjValue<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match &self.0 {
            Obj::StringObj { value } => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "stringObj")?;
                obj.serialize_entry("value", value)?;
                obj.end()
            }
            Obj::ArrayObj { value } => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "arrayObj")?;
                let value: Vec<JsWrappedValue> = value.iter().map(|i| JsWrappedValue(i)).collect();
                obj.serialize_entry("value", &value)?;
                obj.end()
            }
            Obj::MapObj { value } => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "mapObj")?;
                let value: Vec<(&String, JsWrappedValue)> = value.iter()
                    .map(|(key, value)| (key, JsWrappedValue(value)))
                    .collect();
                obj.serialize_entry("value", &value)?;
                obj.end()
            }
            Obj::InstanceObj { typ, fields } => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "instanceObj")?;
                obj.serialize_entry("type", &JsWrappedValue(typ))?;
                let value: Vec<JsWrappedValue> = fields.iter().map(|i| JsWrappedValue(i)).collect();
                obj.serialize_entry("value", &value)?;
                obj.end()
            }
        }
    }
}
