use proc_macro::TokenStream;
use std::cell::RefCell;
use std::collections::HashMap;
use proc_macro2::Span;
use quote::quote;
use syn::{ImplItem, parse_macro_input, Type};
use syn::spanned::Spanned;
use crate::abra_function::{generate_function_code, parse_function};
use crate::parsing_common::{parse_attr_meta, find_attr};
use crate::abra_methods::parsing::{ConstructorSpec, parse_constructor, parse_static_method, parse_to_string_method, parse_getter, parse_setter, parse_pseudomethod, parse_method, parse_enum_variant, parse_enum_variant_data, EnumVariantDataSpec};
use crate::abra_type::parsing::{FieldSpec, ParsedTypeAttr, parse_abra_type_attr, parse_abra_field_attr};
use crate::abra_methods::gen::{gen_native_type_code, gen_native_value_code, TypeSpec};

mod abra_function;
mod abra_methods;
mod abra_type;
mod signature;
mod type_utils;
mod parsing_common;

#[derive(Debug)]
struct FirstPass {
    type_name: String,
    module_name: String,
    type_args: Vec<String>,
    field_specs: Vec<FieldSpec>,
    is_pseudotype: bool,
    is_noconstruct: bool,
    value_variant: Option<String>,
    is_enum: bool,
}

thread_local! {
  static TYPES: RefCell<HashMap<String, FirstPass>> = RefCell::new(HashMap::new());
}

#[proc_macro_derive(AbraType, attributes(abra_type, abra_field))]
pub fn abra_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::ItemStruct);
    let native_type_name = input.ident.to_string();

    let parsed_type_attr = match parse_abra_type_attr(&input.attrs) {
        Ok(res) => res,
        Err(e) => return e.to_compile_error().into()
    };
    let ParsedTypeAttr {
        type_name,
        module_name,
        type_args,
        is_pseudotype,
        is_noconstruct,
        value_variant ,
        is_enum,
    } = parsed_type_attr;

    let field_specs = input.fields.iter()
        .map(|f| parse_abra_field_attr(&type_name, &input, &type_args, f))
        .collect::<Result<Vec<_>, _>>();
    let field_specs = match field_specs {
        Ok(field_specs) => field_specs.into_iter()
            .filter_map(|fs| fs)
            .collect(),
        Err(e) => return e.to_compile_error().into()
    };

    let first_pass = FirstPass { type_name, module_name, type_args, field_specs, is_pseudotype, is_noconstruct, value_variant, is_enum };
    TYPES.with(|types| {
        types.borrow_mut().insert(native_type_name, first_pass)
    });

    (quote! {}).into()
}

#[proc_macro_attribute]
pub fn abra_methods(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as syn::ItemImpl);

    let native_type_name = match &*input.self_ty {
        Type::Path(p) => p.path.segments[0].ident.to_string(),
        _ => unimplemented!()
    };

    let first_pass_data = TYPES.with(|types| {
        let mut types = types.borrow_mut();
        types.remove(&native_type_name)
    });
    let first_pass_data = if let Some(first_pass_data) = first_pass_data {
        first_pass_data
    } else {
        let msg = format!("Cannot implement methods for struct {}, which does not derive AbraType", native_type_name);
        return syn::Error::new(Span::call_site(), msg).to_compile_error().into();
    };
    let FirstPass { type_name, module_name, type_args, mut field_specs, is_pseudotype, is_noconstruct, value_variant, is_enum } = first_pass_data;

    let mut methods = Vec::new();
    let mut static_methods = Vec::new();
    let mut enum_variants = Vec::new();
    let mut pseudo_methods = Vec::new();
    let mut constructor: Option<ConstructorSpec> = None;
    let mut enum_variant_data: Option<EnumVariantDataSpec> = None;
    let mut to_string_method_name: Option<String> = None;
    for item in &mut input.items {
        match item {
            ImplItem::Method(ref mut m) => {
                if let Some((_, attr)) = find_attr(&m.attrs, "abra_constructor") {
                    if is_noconstruct {
                        let msg = format!("This type has `noconstruct = true` in its #[abra_type] attribute, and cannot have a function bound via #[abra_constructor]");
                        return syn::Error::new(attr.span(), msg).to_compile_error().into();
                    }

                    if is_enum {
                        let msg = format!("This type has `is_enum = true` in its #[abra_type] attribute, and cannot have a function bound via #[abra_constructor]");
                        return syn::Error::new(attr.span(), msg).to_compile_error().into();
                    }

                    if constructor.is_some() {
                        let msg = format!("Duplicate constructor function. A type may only have 1 function with the #[abra_constructor] attribute");
                        return syn::Error::new(attr.span(), msg).to_compile_error().into();
                    }
                    constructor = match parse_constructor(m) {
                        Ok(c) => c,
                        Err(e) => return e.to_compile_error().into()
                    };
                    continue;
                }

                if let Some((_, attr)) = find_attr(&m.attrs, "abra_enum_variant") {
                    let attr_span = attr.span();
                    if !is_enum {
                        let msg = format!("Cannot use #[abra_enum_variant] in non-enum type. Did you forget to add `is_enum = true` to #[abra_type]?");
                        return syn::Error::new(attr_span, msg).to_compile_error().into();
                    }

                    match parse_enum_variant(&type_name, &type_args, m) {
                        Ok(Some(m)) => enum_variants.push(m),
                        Err(e) => return e.to_compile_error().into(),
                        _ => continue
                    }
                }

                if let Some((_, attr)) = find_attr(&m.attrs, "abra_enum_variant_data") {
                    let attr_span = attr.span();
                    if !is_enum {
                        let msg = format!("Cannot use #[abra_enum_variant_data] in non-enum type. Did you forget to add `is_enum = true` to #[abra_type]?");
                        return syn::Error::new(attr_span, msg).to_compile_error().into();
                    }

                    match parse_enum_variant_data(m) {
                        Ok(Some(m)) => enum_variant_data = Some(m),
                        Err(e) => return e.to_compile_error().into(),
                        _ => continue
                    }
                }

                if let Some(_) = find_attr(&m.attrs, "abra_static_method") {
                    match parse_static_method(&type_name, &type_args, m) {
                        Ok(Some(m)) => static_methods.push(m),
                        Err(e) => return e.to_compile_error().into(),
                        _ => continue
                    }
                }

                if let Some((_, attr)) = find_attr(&m.attrs, "abra_to_string") {
                    let attr_span = attr.span();
                    match parse_to_string_method(m) {
                        Ok(m) => {
                            if to_string_method_name.is_some() {
                                let msg = format!("Duplicate to-string function. A type may only have 1 function with the #[abra_to_string] attribute");
                                return syn::Error::new(attr_span, msg).to_compile_error().into();
                            }
                            to_string_method_name = m;
                            continue;
                        }
                        Err(e) => return e.to_compile_error().into()
                    }
                }

                if let Some((_, attr)) = find_attr(&m.attrs, "abra_getter") {
                    let attr_span = attr.span();
                    match parse_getter(m) {
                        Err(e) => return e.to_compile_error().into(),
                        Ok(Some((field_name, getter))) => {
                            let field = field_specs.iter_mut().find(|f| f.name == field_name);
                            match field {
                                Some(field) => field.getter = Some(getter),
                                None => {
                                    let msg = format!("Cannot bind getter for field {}; struct {} has no such field", field_name, type_name);
                                    return syn::Error::new(attr_span, msg).to_compile_error().into();
                                }
                            }
                        }
                        _ => {}
                    }
                    continue;
                }

                if let Some((_, attr)) = find_attr(&m.attrs, "abra_setter") {
                    let attr_span = attr.span();
                    match parse_setter(m) {
                        Err(e) => return e.to_compile_error().into(),
                        Ok(Some((field_name, setter))) => {
                            let field = field_specs.iter_mut().find(|f| f.name == field_name);
                            match field {
                                Some(field) => field.setter = Some(setter),
                                None => {
                                    let msg = format!("Cannot bind setter for field {}; struct {} has no such field", field_name, type_name);
                                    return syn::Error::new(attr_span, msg).to_compile_error().into();
                                }
                            }
                        }
                        _ => {}
                    }
                    continue;
                }

                if let Some((_, attr)) = find_attr(&m.attrs, "abra_pseudomethod") {
                    if !is_pseudotype {
                        let msg = "Pseudomethods not allowed unless pseudotype = true in #[abra_type] attribute".to_string();
                        return syn::Error::new(attr.span(), msg).to_compile_error().into();
                    }

                    match parse_pseudomethod(&type_args, m) {
                        Ok(Some(m)) => pseudo_methods.push(m),
                        Err(e) => return e.to_compile_error().into(),
                        _ => continue
                    }
                }

                match parse_method(&type_name, &type_args, m) {
                    Ok(Some(m)) => methods.push(m),
                    Err(e) => return e.to_compile_error().into(),
                    _ => continue
                }
            }
            _ => {}
        }
    }

    if constructor.is_none() && !(is_noconstruct || is_enum) {
        let msg = format!(
            "Missing required constructor function binding for struct {}.\n\
            Please add the #[abra_constructor] attribute to a function of type (Vec<Value>) -> Self.\n\
            You can also add `noconstruct = true` to the #[abra_type] attribute if you don't want to \n\
            provide an explicit constructor, or `is_enum = true` if you intended for this type to be an enum.",
            type_name
        );
        return syn::Error::new(Span::call_site(), msg).to_compile_error().into();
    }
    if enum_variant_data.is_none() && is_enum {
        let msg = format!(
            "Missing required enum variant data function binding for enum {}.\n\
            Please add the #[abra_enum_variant_data] attribute to a function of type (Self) -> (usize, Vec<&Value>).",
            type_name
        );
        return syn::Error::new(Span::call_site(), msg).to_compile_error().into();
    }

    for FieldSpec { name, readonly, setter, .. } in &field_specs {
        if !*readonly && setter.is_none() {
            let msg = format!(
                "Missing required setter function binding for field {}.\n\
                Please add the #[abra_setter(field = \"{}\")] attribute to a function of type (&self, Value) -> ()",
                name, name
            );
            return syn::Error::new(Span::call_site(), msg).to_compile_error().into();
        } else if *readonly && setter.is_some() {
            let msg = format!("Invalid setter function binding for readonly field {}", name);
            return syn::Error::new(Span::call_site(), msg).to_compile_error().into();
        }
    }

    let type_spec = TypeSpec {
        native_type_name,
        name: type_name,
        module_name,
        type_args,
        constructor,
        to_string_method: to_string_method_name,
        fields: field_specs,
        methods,
        pseudo_methods,
        static_methods,
        is_pseudotype,
        is_noconstruct,
        value_variant,
        is_enum,
        enum_variants,
        enum_variant_data,
    };

    let native_type_code = gen_native_type_code(&type_spec);
    let native_value_code = gen_native_value_code(&type_spec);

    let ts = quote! { #input };
    let mut ts = TokenStream::from(ts);
    ts.extend(native_type_code);
    ts.extend(native_value_code);
    ts.into()
}


#[proc_macro_attribute]
pub fn abra_function(attr: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as syn::AttributeArgs);
    let args = args.into_iter()
        .filter_map(|arg| parse_attr_meta(arg))
        .collect::<HashMap<_, _>>();

    let input = parse_macro_input!(input as syn::ItemFn);

    let gen_spec_method_code = match parse_function(args, &input) {
        Err(e) => return e.to_compile_error().into(),
        Ok(method_spec) => generate_function_code(method_spec),
    };

    let ts = quote! {
        #input
        #gen_spec_method_code
    };
    ts.into()
}
