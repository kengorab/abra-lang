use abra_core::typechecker::types::Type;
use serde::{Serialize, Serializer};

pub struct JsType<'a>(pub &'a Type);

impl<'a> Serialize for JsType<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match &self.0 {
            Type::Int => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("kind", "Int")?;
                obj.end()
            }
            Type::Unit => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("kind", "Unit")?;
                obj.end()
            }
            Type::Any => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("kind", "Any")?;
                obj.end()
            }
            Type::Or(opts) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "Or")?;
                let opts: Vec<JsType> = opts.iter().map(|opt| JsType(opt)).collect();
                obj.serialize_entry("options", &opts)?;
                obj.end()
            }
            Type::Float => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("kind", "Float")?;
                obj.end()
            }
            Type::String => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("kind", "String")?;
                obj.end()
            }
            Type::Bool => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("kind", "Bool")?;
                obj.end()
            }
            Type::Array(inner_type) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "Array")?;
                obj.serialize_entry("innerType", &JsType(inner_type))?;
                obj.end()
            }
            Type::Option(inner_type) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "Option")?;
                obj.serialize_entry("innerType", &JsType(inner_type))?;
                obj.end()
            }
            Type::Fn(args, return_type) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "Fn")?;
                let args: Vec<(String, JsType)> = args.iter()
                    .map(|(name, typ, _)| (name.clone(), JsType(typ)))
                    .collect();
                obj.serialize_entry("args", &args)?;
                obj.serialize_entry("returnType", &JsType(return_type))?;
                obj.end()
            }
            Type::Unknown => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("kind", "Unknown")?;
                obj.end()
            }
            Type::Struct { name, fields } => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "Struct")?;
                obj.serialize_entry("name", name)?;
                let fields: Vec<(String, JsType)> = fields.iter()
                    .map(|(name, typ)| (name.clone(), JsType(typ)))
                    .collect();
                obj.serialize_entry("fields", &fields)?;
                obj.end()
            }
        }
    }
}
