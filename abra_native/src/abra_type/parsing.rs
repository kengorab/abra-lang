use crate::abra_methods::parsing::{GetterSpec, SetterSpec};
use crate::parsing_common::{find_attr, parse_attr};
use proc_macro2::Span;
use syn::spanned::Spanned;
use crate::signature::{TypeRepr, parse_type};

#[derive(Debug)]
pub struct ParsedTypeAttr {
    pub(crate) type_name: String,
    pub(crate) module_name: String,
    pub(crate) type_args: Vec<String>,
    pub(crate) is_pseudotype: bool,
    pub(crate) is_noconstruct: bool,
    pub(crate) value_variant: Option<String>,
    pub(crate) is_enum: bool,
}

pub fn parse_abra_type_attr(attrs: &Vec<syn::Attribute>) -> Result<ParsedTypeAttr, syn::Error> {
    match find_attr(attrs, "abra_type") {
        Some((_, attr)) => {
            let attr_span = attr.span();
            let attr = parse_attr(attr);
            let is_pseudotype = attr.get("pseudotype").map(|v| v == "true").unwrap_or(false);
            let is_noconstruct = attr.get("noconstruct").map(|v| v == "true").unwrap_or(false);
            let value_variant = attr.get("variant").map(|v| v.clone());
            let is_enum = attr.get("is_enum").map(|v| v == "true").unwrap_or(false);

            let module_name = match attr.get("module") {
                Some(module_name) => module_name.clone(),
                None => {
                    let msg = "Missing required parameter `module` in #[abra_type] attribute".to_string();
                    return Err(syn::Error::new(attr_span, msg));
                }
            };

            match attr.get("signature") {
                Some(signature) => match parse_type(None, &vec![], signature) {
                    Ok(repr) => match repr {
                        TypeRepr::Ident(type_name, type_args) => {
                            let type_args = type_args.into_iter()
                                .map(|t| match t {
                                    TypeRepr::Ident(n, _) => n,
                                    r => unreachable!(format!("Unexpected type {:?}", r))
                                })
                                .collect();
                            let parsed = ParsedTypeAttr {
                                type_name,
                                module_name,
                                type_args,
                                is_pseudotype,
                                is_noconstruct,
                                value_variant,
                                is_enum,
                            };
                            Ok(parsed)
                        }
                        r => unreachable!(format!("Unexpected type {:?}", r))
                    },
                    Err(e) => {
                        let msg = format!("Invalid signature provided to #[abra_type]: {}", e);
                        Err(syn::Error::new(attr_span, msg))
                    }
                }
                None => {
                    let msg = "Missing required parameter `signature` in #[abra_type] attribute".to_string();
                    Err(syn::Error::new(attr_span, msg))
                }
            }
        }
        None => {
            let msg = "Missing required attribute `#[abra_type]`".to_string();
            Err(syn::Error::new(Span::call_site(), msg))
        }
    }
}

#[derive(Debug)]
pub struct FieldSpec {
    pub(crate) native_field_name: String,
    pub(crate) name: String,
    pub(crate) typ: TypeRepr,
    pub(crate) has_default: bool,
    pub(crate) readonly: bool,
    pub(crate) getter: Option<GetterSpec>,
    pub(crate) setter: Option<SetterSpec>,
}

pub fn parse_abra_field_attr(type_name: &String, struct_item: &syn::ItemStruct, type_args: &Vec<String>, field: &syn::Field) -> Result<Option<FieldSpec>, syn::Error> {
    let (_, attr) = match find_attr(&field.attrs, "abra_field") {
        Some(attr) => attr,
        _ => return Ok(None)
    };
    let field_attr = parse_attr(attr);

    let field_ident = field.ident.as_ref();
    let name = match field_ident {
        Some(name) => name.to_string(),
        None => {
            let msg = format!("A struct which derives AbraType must have named fields");
            return Err(syn::Error::new(struct_item.span(), msg));
        }
    };

    let typ = match field_attr.get("field_type") {
        Some(typ) => {
            match parse_type(Some(type_name), type_args, typ) {
                Ok(t) => t,
                Err(e) => {
                    let msg = format!("Invalid `field_type` provided to #[abra_field]: {}", e);
                    return Err(syn::Error::new(attr.span(), msg));
                }
            }
        }
        None => {
            let msg = format!("Missing required parameter `field_type` for field `{}`", name);
            return Err(syn::Error::new(attr.span(), msg));
        }
    };

    Ok(Some(FieldSpec {
        native_field_name: name.clone(),
        name: field_attr.get("name").unwrap_or(&name).clone(),
        typ,
        has_default: field_attr.get("has_default").map_or(false, |f| f == "true"),
        readonly: field_attr.get("readonly").map_or(false, |f| f == "true"),
        getter: None, // <
        setter: None, // < For now, will be set later when parsing methods
    }))
}
