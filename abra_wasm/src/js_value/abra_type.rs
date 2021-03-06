use abra_core::typechecker::types::{Type, StructType, FnType, EnumType, StructTypeField};
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
            Type::Union(opts) => {
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
            Type::Tuple(types) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "Tuple")?;
                let types: Vec<JsType> = types.iter().map(|t| JsType(t)).collect();
                obj.serialize_entry("types", &types)?;
                obj.end()
            }
            Type::Map(key_type, value_type) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "Map")?;
                obj.serialize_entry("keyType", &JsType(key_type))?;
                obj.serialize_entry("valueType", &JsType(value_type))?;
                obj.end()
            }
            Type::Set(inner_type) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "Set")?;
                obj.serialize_entry("innerType", &JsType(inner_type))?;
                obj.end()
            }
            Type::Option(inner_type) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "Option")?;
                obj.serialize_entry("innerType", &JsType(inner_type))?;
                obj.end()
            }
            Type::Fn(FnType { arg_types, type_args, ret_type, is_variadic }) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "Fn")?;
                let args: Vec<(String, JsType)> = arg_types.iter()
                    .map(|(name, typ, _)| (name.clone(), JsType(typ)))
                    .collect();
                obj.serialize_entry("args", &args)?;
                obj.serialize_entry("typeArgs", &type_args)?;
                obj.serialize_entry("returnType", &JsType(ret_type))?;
                obj.serialize_entry("isVariadic", is_variadic)?;
                obj.end()
            }
            Type::Type(name, _, _) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "Type")?;
                obj.serialize_entry("name", name)?;
                obj.end()
            }
            Type::Unknown => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("kind", "Unknown")?;
                obj.end()
            }
            Type::Struct(StructType { name, type_args, fields, methods, static_fields }) => {
                let mut obj = serializer.serialize_map(Some(6))?;
                obj.serialize_entry("kind", "Struct")?;
                obj.serialize_entry("name", name)?;
                let type_args: Vec<(String, JsType)> = type_args.iter()
                    .map(|(name, typ)| (name.clone(), JsType(typ)))
                    .collect();
                obj.serialize_entry("typeArgs", &type_args)?;
                let fields: Vec<(String, JsType)> = fields.iter()
                    .map(|StructTypeField { name, typ, .. }| (name.clone(), JsType(typ)))
                    .collect();
                obj.serialize_entry("fields", &fields)?;
                let static_fields: Vec<(String, JsType)> = static_fields.iter()
                    .map(|(name, typ, _)| (name.clone(), JsType(typ)))
                    .collect();
                obj.serialize_entry("staticFields", &static_fields)?;
                let methods: Vec<(String, JsType)> = methods.iter()
                    .map(|(name, typ)| (name.clone(), JsType(typ)))
                    .collect();
                obj.serialize_entry("methods", &methods)?;
                obj.end()
            }
            Type::Enum(EnumType { name, variants, static_fields, methods }) => {
                let mut obj = serializer.serialize_map(Some(6))?;
                obj.serialize_entry("kind", "Enum")?;
                obj.serialize_entry("name", name)?;
                let variants: Vec<String> = variants.iter()
                    .map(|variant| variant.name.clone())
                    .collect();
                obj.serialize_entry("variants", &variants)?;
                let static_fields: Vec<(String, JsType)> = static_fields.iter()
                    .map(|(name, typ, _)| (name.clone(), JsType(typ)))
                    .collect();
                obj.serialize_entry("staticFields", &static_fields)?;
                let methods: Vec<(String, JsType)> = methods.iter()
                    .map(|(name, typ)| (name.clone(), JsType(typ)))
                    .collect();
                obj.serialize_entry("methods", &methods)?;
                obj.end()
            }
            Type::EnumVariant(enum_type, variant, _) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "EnumVariant")?;
                obj.serialize_entry("enumType", &JsType(enum_type))?;
                obj.serialize_entry("variantName", &variant.name)?;
                obj.serialize_entry("index", &variant.variant_idx)?;
                obj.end()
            }
            Type::Placeholder => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("kind", "Placeholder")?;
                obj.end()
            }
            Type::Reference(name, type_args) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "Reference")?;
                obj.serialize_entry("name", name)?;
                let type_args: Vec<JsType> = type_args.iter()
                    .map(|typ| JsType(typ))
                    .collect();
                obj.serialize_entry("typeArgs", &type_args)?;
                obj.end()
            }
            Type::Generic(name) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "Generic")?;
                obj.serialize_entry("name", name)?;
                obj.end()
            }
        }
    }
}
