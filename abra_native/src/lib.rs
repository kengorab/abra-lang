use proc_macro::TokenStream;
use quote::{quote, format_ident};
use syn::{parse_macro_input, NestedMeta, ImplItem, Type, FnArg, ReturnType};
use std::cell::RefCell;
use std::collections::HashMap;
use proc_macro2::Span;
use syn::spanned::Spanned;
use crate::signature::{TypeRepr, MethodArgSpec, parse_type, parse_fn_signature, Signature};

mod signature;

#[derive(Debug)]
struct TypeSpec {
    native_type_name: String,
    name: String,
    module_name: String,
    type_args: Vec<String>,
    constructor: Option<ConstructorSpec>,
    to_string_method: Option<String>,
    fields: Vec<FieldSpec>,
    methods: Vec<MethodSpec>,
    pseudo_methods: Vec<MethodSpec>,
    static_methods: Vec<MethodSpec>,
    is_pseudotype: bool,
    is_noconstruct: bool,
    value_variant: Option<String>,
}

#[derive(Debug)]
struct ConstructorSpec {
    native_fn_name: String,
}

#[derive(Debug)]
struct FieldSpec {
    native_field_name: String,
    name: String,
    typ: TypeRepr,
    has_default: bool,
    readonly: bool,
    getter: Option<GetterSpec>,
    setter: Option<SetterSpec>,
}

#[derive(Debug)]
struct MethodSpec {
    native_method_name: String,
    native_method_arity: usize,
    name: String,
    args: Vec<MethodArgSpec>,
    return_type: TypeRepr,
    is_static: bool,
    is_mut: bool,
    is_variadic: bool,
    is_pseudomethod: bool,
}

#[derive(Debug)]
struct GetterSpec {
    native_method_name: String,
}

#[derive(Debug)]
struct SetterSpec {
    native_method_name: String,
}

#[derive(Debug)]
struct FirstPass {
    type_name: String,
    module_name: String,
    type_args: Vec<String>,
    field_specs: Vec<FieldSpec>,
    is_pseudotype: bool,
    is_noconstruct: bool,
    value_variant: Option<String>,
}

thread_local! {
  static TYPES: RefCell<HashMap<String, FirstPass>> = RefCell::new(HashMap::new());
}

#[proc_macro_derive(AbraType, attributes(abra_type, abra_field))]
pub fn abra_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::ItemStruct);
    let native_type_name = input.ident.to_string();

    let (type_name, module_name, type_args, is_pseudotype, is_noconstruct, value_variant) = match parse_abra_type_attr(&input.attrs) {
        Ok(res) => res,
        Err(e) => return e.to_compile_error().into()
    };

    let field_specs = input.fields.iter()
        .map(|f| parse_abra_field_attr(&type_name, &input, &type_args, f))
        .collect::<Result<Vec<_>, _>>();
    let field_specs = match field_specs {
        Ok(field_specs) => field_specs.into_iter()
            .filter_map(|fs| fs)
            .collect(),
        Err(e) => return e.to_compile_error().into()
    };

    let first_pass = FirstPass { type_name, module_name, type_args, field_specs, is_pseudotype, is_noconstruct, value_variant };
    TYPES.with(|types| {
        types.borrow_mut().insert(native_type_name, first_pass)
    });

    (quote! {}).into()
}

fn parse_abra_field_attr(type_name: &String, struct_item: &syn::ItemStruct, type_args: &Vec<String>, field: &syn::Field) -> Result<Option<FieldSpec>, syn::Error> {
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

fn parse_abra_type_attr(attrs: &Vec<syn::Attribute>) -> Result<(String, String, Vec<String>, bool, bool, Option<String>), syn::Error> {
    match find_attr(attrs, "abra_type") {
        Some((_, attr)) => {
            let attr_span = attr.span();
            let attr = parse_attr(attr);
            let is_pseudotype = attr.get("pseudotype").map(|v| v == "true").unwrap_or(false);
            let is_noconstruct = attr.get("noconstruct").map(|v| v == "true").unwrap_or(false);
            let value_variant = attr.get("variant").map(|v| v.clone());

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
                            Ok((type_name, module_name, type_args, is_pseudotype, is_noconstruct, value_variant))
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
    let FirstPass { type_name, module_name, type_args, mut field_specs, is_pseudotype, is_noconstruct, value_variant } = first_pass_data;

    let mut methods = Vec::new();
    let mut static_methods = Vec::new();
    let mut pseudo_methods = Vec::new();
    let mut constructor: Option<ConstructorSpec> = None;
    let mut to_string_method_name: Option<String> = None;
    for item in &mut input.items {
        match item {
            ImplItem::Method(ref mut m) => {
                match find_attr(&m.attrs, "abra_constructor") {
                    None => {}
                    Some((_, attr)) => {
                        if is_noconstruct {
                            let msg = format!("This type has `noconstruct = true` in its #[abra_type] attribute, and cannot have a function bound via #[abra_constructor]");
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
                }

                match find_attr(&m.attrs, "abra_static_method") {
                    None => {}
                    Some(_) => {
                        match parse_static_method(&type_name, &type_args, m) {
                            Ok(Some(m)) => static_methods.push(m),
                            Err(e) => return e.to_compile_error().into(),
                            _ => continue
                        }
                    }
                }

                match find_attr(&m.attrs, "abra_to_string") {
                    None => {}
                    Some((_, attr)) => {
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
                }

                match find_attr(&m.attrs, "abra_getter") {
                    None => {}
                    Some((_, attr)) => {
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
                }

                match find_attr(&m.attrs, "abra_setter") {
                    None => {}
                    Some((_, attr)) => {
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
                }

                match find_attr(&m.attrs, "abra_pseudomethod") {
                    None => {}
                    Some((_, attr)) => {
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

    if constructor.is_none() && !is_noconstruct {
        let msg = format!(
            "Missing required constructor function binding for struct {}.\n\
            Please add the #[abra_constructor] attribute to a function of type (Vec<Value>) -> Self.\n\
            You can also add `noconstruct = true` to the #[abra_type] attribute if you don't want to provide an explicit constructor",
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
    };

    let native_type_code = gen_native_type_code(&type_spec);
    let native_value_code = gen_native_value_code(&type_spec);

    let ts = quote! { #input };
    let mut ts = TokenStream::from(ts);
    ts.extend(native_type_code);
    ts.extend(native_value_code);
    ts.into()
}

fn parse_method(type_name: &String, type_args: &Vec<String>, method: &mut syn::ImplItemMethod) -> Result<Option<MethodSpec>, syn::Error> {
    let (idx, abra_method_attr) = match find_attr(&method.attrs, "abra_method") {
        Some(attr) => attr,
        None => return Ok(None)
    };
    let attr_span = abra_method_attr.span();
    let attr = parse_attr(abra_method_attr);
    method.attrs.remove(idx);

    let signature = match attr.get("signature") {
        Some(signature) => match parse_fn_signature(Some(type_name), type_args, signature) {
            Ok(s) => s,
            Err(e) => {
                let msg = format!("Invalid signature provided to #[abra_method]: {}", e);
                return Err(syn::Error::new(attr_span, msg));
            }
        }
        None => {
            let msg = "Missing required parameter `signature` in #[abra_method] attribute".to_string();
            return Err(syn::Error::new(attr_span, msg));
        }
    };

    let is_mut = match method.sig.inputs.first() {
        Some(FnArg::Typed(_)) | None => {
            let msg = format!("The function bound via #[abra_method] must receive self");
            return Err(syn::Error::new(method.span(), msg));
        }
        Some(FnArg::Receiver(rcv)) => {
            if !rcv.reference.is_some() {
                let msg = format!("The function bound via #[abra_method] must receive a reference to self");
                return Err(syn::Error::new(rcv.span(), msg));
            }
            rcv.mutability.is_some()
        }
    };
    let native_method_arity = method.sig.inputs.len() - 1; // -1 to account for &self
    if native_method_arity > 2 {
        let msg = format!("The function bound via #[abra_method] receives too many parameters; bound functions can only receive up to 2 parameters (Arguments and VM)");
        return Err(syn::Error::new(method.sig.inputs.span(), msg));
    }

    if signature.return_type == TypeRepr::Unit && method.sig.output != syn::ReturnType::Default {
        let msg = format!("The function bound via #[abra_method] has no return type in its signature, and therefore must not have a return value");
        return Err(syn::Error::new(method.sig.output.span(), msg));
    }
    if let TypeRepr::SelfType(_) = &signature.return_type {
        let is_invalid = if let ReturnType::Type(_, t) = &method.sig.output {
            if let syn::Type::Path(type_path) = &**t {
                if let Some(seg) = type_path.path.segments.last() {
                    seg.ident.to_string() != "Self"
                } else { unreachable!() }
            } else { true }
        } else { true };
        if is_invalid {
            let msg = format!("The function bound via #[abra_method] has a signature which returns '{}', so it must return Self", type_name);
            return Err(syn::Error::new(method.sig.span(), msg));
        }
    }

    let is_variadic = signature.args.iter().any(|arg| arg.is_variadic);

    let method_name = method.sig.ident.to_string();
    Ok(Some(MethodSpec {
        native_method_name: method_name.clone(),
        native_method_arity,
        name: signature.func_name,
        args: signature.args,
        return_type: signature.return_type,
        is_static: false,
        is_mut,
        is_variadic,
        is_pseudomethod: false,
    }))
}

fn parse_pseudomethod(type_args: &Vec<String>, method: &mut syn::ImplItemMethod) -> Result<Option<MethodSpec>, syn::Error> {
    let (idx, abra_method_attr) = match find_attr(&method.attrs, "abra_pseudomethod") {
        Some(attr) => attr,
        None => return Ok(None)
    };
    let attr_span = abra_method_attr.span();
    let attr = parse_attr(abra_method_attr);
    method.attrs.remove(idx);

    let signature = match attr.get("signature") {
        Some(signature) => match parse_fn_signature(None, type_args, signature) {
            Ok(s) => s,
            Err(e) => {
                let msg = format!("Invalid signature provided to #[abra_pseudomethod]: {}", e);
                return Err(syn::Error::new(attr_span, msg));
            }
        }
        None => {
            let msg = "Missing required parameter `signature` in #[abra_pseudomethod] attribute".to_string();
            return Err(syn::Error::new(attr_span, msg));
        }
    };

    if let Some(FnArg::Receiver(_)) = method.sig.inputs.first() {
        let msg = format!("The function bound via #[abra_pseudomethod] must not receive self");
        return Err(syn::Error::new(method.span(), msg));
    }
    let native_method_arity = method.sig.inputs.len() - 1; // -1 to account for rcv
    if native_method_arity > 2 {
        let msg = format!("The function bound via #[abra_pseudomethod] receives too many parameters; bound functions can only receive up to 2 parameters (Arguments and VM)");
        return Err(syn::Error::new(method.sig.inputs.span(), msg));
    }

    if signature.return_type == TypeRepr::Unit && method.sig.output != syn::ReturnType::Default {
        let msg = format!("The function bound via #[abra_pseudomethod] has no return type in its signature, and therefore must not have a return value");
        return Err(syn::Error::new(method.sig.output.span(), msg));
    }

    let is_variadic = signature.args.iter().any(|arg| arg.is_variadic);

    let method_name = method.sig.ident.to_string();
    Ok(Some(MethodSpec {
        native_method_name: method_name.clone(),
        native_method_arity,
        name: signature.func_name,
        args: signature.args,
        return_type: signature.return_type,
        is_static: false,
        is_mut: false,
        is_variadic,
        is_pseudomethod: true,
    }))
}

fn parse_static_method(type_name: &String, type_args: &Vec<String>, method: &mut syn::ImplItemMethod) -> Result<Option<MethodSpec>, syn::Error> {
    let (idx, abra_method_attr) = match find_attr(&method.attrs, "abra_static_method") {
        Some(attr) => attr,
        None => return Ok(None)
    };
    let attr_span = abra_method_attr.span();
    let attr = parse_attr(abra_method_attr);
    method.attrs.remove(idx);

    if let Some(FnArg::Receiver(_)) = method.sig.inputs.first() {
        let msg = format!("The function bound via #[abra_static_method] must not receive self");
        return Err(syn::Error::new(method.span(), msg));
    }
    let native_method_arity = method.sig.inputs.len();
    if native_method_arity > 2 {
        let msg = format!("The function bound via #[abra_static_method] receives too many parameters; bound functions can only receive up to 2 parameters (Arguments and VM)");
        return Err(syn::Error::new(method.sig.inputs.span(), msg));
    }

    let signature = match attr.get("signature") {
        Some(signature) => match parse_fn_signature(Some(type_name), type_args, signature) {
            Ok(s) => s,
            Err(e) => {
                let msg = format!("Invalid signature provided to #[abra_static_method]: {}", e);
                return Err(syn::Error::new(attr_span, msg));
            }
        }
        None => {
            let msg = "Missing required parameter `signature` in #[abra_static_method] attribute".to_string();
            return Err(syn::Error::new(attr_span, msg));
        }
    };
    let Signature { func_name, args, return_type } = signature;

    if return_type == TypeRepr::Unit && method.sig.output != syn::ReturnType::Default {
        let msg = format!("The function bound via #[abra_static_method] has no return type in its signature, and therefore must not have a return value");
        return Err(syn::Error::new(method.sig.output.span(), msg));
    }
    if let TypeRepr::SelfType(_) = &return_type {
        let is_invalid = if let ReturnType::Type(_, t) = &method.sig.output {
            if let syn::Type::Path(type_path) = &**t {
                if let Some(seg) = type_path.path.segments.last() {
                    seg.ident.to_string() != "Self"
                } else { unreachable!() }
            } else { true }
        } else { true };
        if is_invalid {
            let msg = format!("The function bound via #[abra_static_method] has a signature which returns '{}', so it must return Self", type_name);
            return Err(syn::Error::new(method.sig.span(), msg));
        }
    }

    let is_variadic = args.iter().any(|arg| arg.is_variadic);

    let method_name = method.sig.ident.to_string();
    Ok(Some(MethodSpec {
        native_method_name: method_name.clone(),
        native_method_arity,
        name: func_name,
        args,
        return_type,
        is_static: true,
        is_mut: false,
        is_variadic,
        is_pseudomethod: false,
    }))
}

fn parse_constructor(method: &mut syn::ImplItemMethod) -> Result<Option<ConstructorSpec>, syn::Error> {
    let abra_constructor_attr = find_attr(&method.attrs, "abra_constructor");
    match abra_constructor_attr {
        Some((idx, _)) => method.attrs.remove(idx),
        _ => return Ok(None)
    };
    let method_name = method.sig.ident.to_string();
    if let Some(FnArg::Receiver(_)) = method.sig.inputs.first() {
        let msg = format!("The function bound via #[abra_constructor] must not receive self");
        return Err(syn::Error::new(method.span(), msg));
    }

    Ok(Some(ConstructorSpec { native_fn_name: method_name }))
}

fn parse_to_string_method(method: &mut syn::ImplItemMethod) -> Result<Option<String>, syn::Error> {
    let abra_constructor_attr = find_attr(&method.attrs, "abra_to_string");
    match abra_constructor_attr {
        Some((idx, _)) => method.attrs.remove(idx),
        _ => return Ok(None)
    };
    let method_name = method.sig.ident.to_string();
    if let ReturnType::Type(_, t) = &method.sig.output {
        if let syn::Type::Path(type_path) = &**t {
            if let Some(seg) = type_path.path.segments.last() {
                if seg.ident.to_string() != "String" {
                    let msg = "The function bound via #[abra_to_string] must return String".to_string();
                    return Err(syn::Error::new(method.sig.output.span(), msg));
                }
            }
        }
    }

    Ok(Some(method_name))
}

fn parse_getter(method: &mut syn::ImplItemMethod) -> Result<Option<(String, GetterSpec)>, syn::Error> {
    let method_name = method.sig.ident.to_string();

    let (idx, abra_getter_attr) = match find_attr(&method.attrs, "abra_getter") {
        Some(attr) => attr,
        None => return Ok(None)
    };
    let attr_span = abra_getter_attr.span();
    let attr = parse_attr(abra_getter_attr);
    method.attrs.remove(idx);

    let field = match attr.get("field") {
        Some(field) => field.clone(),
        None => {
            let msg = format!("Missing required parameter `field` in `abra_getter` attribute");
            return Err(syn::Error::new(attr_span, msg));
        }
    };

    Ok(Some((field, GetterSpec { native_method_name: method_name })))
}

fn parse_setter(method: &mut syn::ImplItemMethod) -> Result<Option<(String, SetterSpec)>, syn::Error> {
    let method_name = method.sig.ident.to_string();

    let (idx, abra_setter_attr) = match find_attr(&method.attrs, "abra_setter") {
        Some(attr) => attr,
        None => return Ok(None)
    };
    let attr_span = abra_setter_attr.span();
    let attr = parse_attr(abra_setter_attr);
    method.attrs.remove(idx);

    let field = match attr.get("field") {
        Some(field) => field.clone(),
        None => {
            let msg = format!("Missing required parameter `field` in `abra_setter` attribute");
            return Err(syn::Error::new(attr_span, msg));
        }
    };

    Ok(Some((field, SetterSpec { native_method_name: method_name })))
}

fn gen_rust_type_path(type_repr: &TypeRepr, module_name: &String, type_ref_name: &String) -> proc_macro2::TokenStream {
    match type_repr {
        TypeRepr::SelfType(type_args) => {
            if type_ref_name == "Array" {
                let arr_type_repr = TypeRepr::Array(Box::new(type_args[0].clone()));
                gen_rust_type_path(&arr_type_repr, module_name, type_ref_name)
            } else if type_ref_name == "String" {
                quote! { crate::typechecker::types::Type::String }
            } else if type_ref_name == "Set" {
                let inner_type = type_args.get(0).expect("Sets require T value");
                let inner_type_repr = gen_rust_type_path(inner_type, module_name, type_ref_name);

                quote! { crate::typechecker::types::Type::Set(std::boxed::Box::new(#inner_type_repr)) }
            } else if type_ref_name == "Map" {
                let key_type = type_args.get(0).expect("Maps require K value");
                let key_type_repr = gen_rust_type_path(key_type, module_name, type_ref_name);
                let val_type = type_args.get(1).expect("Maps require V value");
                let val_type_repr = gen_rust_type_path(val_type, module_name, type_ref_name);

                quote! {
                    crate::typechecker::types::Type::Map(
                        std::boxed::Box::new(#key_type_repr),
                        std::boxed::Box::new(#val_type_repr)
                    )
                }
            } else {
                let type_args = type_args.iter().map(|type_arg| {
                    gen_rust_type_path(type_arg, module_name, type_ref_name)
                });

                quote! { crate::typechecker::types::Type::Reference(#type_ref_name.to_string(), vec![ #(#type_args),* ]) }
            }
        }
        TypeRepr::Generic(g) => quote! { crate::typechecker::types::Type::Generic(#g.to_string()) },
        TypeRepr::Unit => quote! { crate::typechecker::types::Type::Unit },
        TypeRepr::Ident(i, type_args) => {
            match i.as_str() {
                "Unit" | "Any" | "Int" | "Float" | "String" | "Bool" => {
                    let typ = format_ident!("{}", i);
                    quote! { crate::typechecker::types::Type::#typ }
                }
                i => {
                    let type_args = type_args.iter().map(|type_arg| {
                        gen_rust_type_path(type_arg, module_name, type_ref_name)
                    });

                    quote! { crate::typechecker::types::Type::Reference(#i.to_string(), vec![ #(#type_args),* ]) }
                }
            }
        }
        TypeRepr::Fn(args, ret) => {
            let args = args.iter().map(|arg| {
                let arg_type = gen_rust_type_path(arg, module_name, type_ref_name);
                quote! { ("_".to_string(), #arg_type, false) }
            });
            let ret = gen_rust_type_path(ret, module_name, type_ref_name);

            quote! {
                crate::typechecker::types::Type::Fn(crate::typechecker::types::FnType {
                    arg_types: vec![ #(#args),* ],
                    type_args: vec![],
                    ret_type: std::boxed::Box::new(#ret),
                    is_variadic: false
                })
            }
        }
        TypeRepr::Tuple(types) => {
            let types = types.iter().map(|t| gen_rust_type_path(t, module_name, type_ref_name));
            quote! { crate::typechecker::types::Type::Tuple(vec![ #(#types),* ]) }
        }
        TypeRepr::Union(types) => {
            let types = types.iter().map(|t| gen_rust_type_path(t, module_name, type_ref_name));
            quote! { crate::typechecker::types::Type::Union(vec![ #(#types),* ]) }
        }
        TypeRepr::Array(t) => {
            let t = gen_rust_type_path(t, module_name, type_ref_name);
            quote! { crate::typechecker::types::Type::Array(std::boxed::Box::new(#t)) }
        }
        TypeRepr::Opt(t) => {
            let t = gen_rust_type_path(t, module_name, type_ref_name);
            quote! { crate::typechecker::types::Type::Option(std::boxed::Box::new(#t)) }
        }
    }
}

fn gen_native_type_code(type_spec: &TypeSpec) -> TokenStream {
    let type_name = &type_spec.name;
    let module_name = &type_spec.module_name;

    let type_args = type_spec.type_args.iter().map(|type_arg_name| {
        quote! {
            (#type_arg_name.to_string(), crate::typechecker::types::Type::Generic(#type_arg_name.to_string()))
        }
    });

    let fields = type_spec.fields.iter().map(|field| {
        let FieldSpec { name, typ, has_default, readonly, .. } = field;
        let typ = gen_rust_type_path(typ, module_name, type_name);

        quote! {
            crate::typechecker::types::StructTypeField {
                name: #name.to_string(),
                typ: #typ,
                has_default_value: #has_default,
                readonly: #readonly,
            }
        }
    });

    let all_methods = type_spec.methods.iter().chain(type_spec.pseudo_methods.iter());
    let methods = all_methods.map(|method| {
        let MethodSpec { name, args, return_type, is_variadic, .. } = method;

        let args = args.iter().map(|arg| {
            let MethodArgSpec { name, typ, is_optional, .. } = arg;
            let typ = gen_rust_type_path(typ, module_name, type_name);

            quote! { (#name.to_string(), #typ, #is_optional) }
        });

        let return_type = gen_rust_type_path(return_type, module_name, type_name);

        quote! {
            (
                #name.to_string(),
                crate::typechecker::types::Type::Fn(crate::typechecker::types::FnType {
                    arg_types: vec![#(#args),*],
                    type_args: vec![],
                    ret_type: Box::new(#return_type),
                    is_variadic: #is_variadic,
                })
            )
        }
    });

    let static_methods = type_spec.static_methods.iter().map(|static_method| {
        let MethodSpec { name, args, return_type, is_variadic, .. } = static_method;

        let args = args.iter().map(|arg| {
            let MethodArgSpec { name, typ, is_optional, .. } = arg;
            let typ = gen_rust_type_path(typ, module_name, type_name);

            quote! { (#name.to_string(), #typ, #is_optional) }
        });

        let return_type = gen_rust_type_path(return_type, module_name, type_name);

        quote! {
            (
                #name.to_string(),
                crate::typechecker::types::Type::Fn(crate::typechecker::types::FnType {
                    arg_types: vec![#(#args),*],
                    type_args: vec![],
                    ret_type: Box::new(#return_type),
                    is_variadic: #is_variadic,
                }),
                true
            )
        }
    });

    let native_type_name_ident = format_ident!("{}", &type_spec.native_type_name);
    let ts = quote! {
        impl crate::builtins::native_value_trait::NativeTyp for #native_type_name_ident {
            fn get_type() -> crate::typechecker::types::StructType where Self: Sized {
                crate::typechecker::types::StructType {
                    name: #type_name.to_string(),
                    type_args: vec![ #(#type_args),* ],
                    fields: vec![ #(#fields),* ],
                    static_fields: vec![ #(#static_methods),* ],
                    methods: vec![
                        ("toString".to_string(), crate::typechecker::types::Type::Fn(crate::typechecker::types::FnType {
                            arg_types: vec![],
                            type_args: vec![],
                            ret_type: std::boxed::Box::new(crate::typechecker::types::Type::String),
                            is_variadic: false,
                        })),
                        #(#methods),*
                    ],
                }
            }
        }
    };

    ts.into()
}

fn gen_native_value_code(type_spec: &TypeSpec) -> TokenStream {
    let construct_method_code = gen_construct_code(type_spec);
    let get_type_value_method_code = gen_get_type_value_method_code(type_spec);
    let to_string_method_code = gen_to_string_method_code(&type_spec.to_string_method);
    let get_field_values_method_code = gen_get_field_values_method_code(type_spec);
    let get_field_value_method_code = gen_get_field_value_method_code(type_spec);
    let set_field_value_method_code = gen_set_field_value_method_code(type_spec);

    let native_type_name_ident = format_ident!("{}", &type_spec.native_type_name);
    let ts = quote! {
        impl crate::builtins::native_value_trait::NativeValue for #native_type_name_ident {
            #construct_method_code
            #get_type_value_method_code
            fn is_equal(&self, other: &std::boxed::Box<dyn crate::builtins::native_value_trait::NativeValue>) -> bool {
                let other = other.downcast_ref::<Self>();
                other.map_or(false, |o| o.eq(&self))
            }
            #to_string_method_code
            #get_field_values_method_code
            #get_field_value_method_code
            #set_field_value_method_code
        }
    };
    ts.into()
}

fn gen_construct_code(type_spec: &TypeSpec) -> proc_macro2::TokenStream {
    let body = match &type_spec.constructor {
        Some(spec) => {
            let constructor_fn_name = &spec.native_fn_name;
            let constructor_fn_name_ident = format_ident!("{}", &constructor_fn_name);
            quote! {
                let inst = Self::#constructor_fn_name_ident(args);
                crate::vm::value::Value::new_native_instance_obj(module_idx, type_id, std::boxed::Box::new(inst))
            }
        }
        None => {
            quote! {
                unreachable!("The type bound to #type_name has not bound a function via #[abra_constructor]. \
                You were probably never intended to construct this type")
            }
        }
    };

    quote! {
        fn construct(module_idx: usize, type_id: usize, args: std::vec::Vec<crate::vm::value::Value>) -> crate::vm::value::Value where Self: core::marker::Sized {
            #body
        }
    }
}

fn gen_get_type_value_method_code(type_spec: &TypeSpec) -> proc_macro2::TokenStream {
    let type_name = &type_spec.name;
    let module_name = &type_spec.module_name;
    let fully_qualified_type_name = format!("{}/{}", module_name, type_name);

    let fields = type_spec.fields.iter().map(|field| {
        let name = &field.name;
        quote! { #name.to_string() }
    });

    let all_methods = type_spec.methods.iter().chain(type_spec.pseudo_methods.iter());
    let methods = all_methods.map(|method| {
        let name = &method.name;
        let num_args = method.args.len();
        let has_return = method.return_type != TypeRepr::Unit;
        let native_name_ident = format_ident!("{}", &method.native_method_name);
        let native_method_arity = method.native_method_arity;

        let arguments = match native_method_arity {
            0 => quote! {},
            1 => quote! {
                crate::builtins::arguments::Arguments::new(#name, #num_args, args)
            },
            2 => quote! {
                crate::builtins::arguments::Arguments::new(#name, #num_args, args), vm
            },
            _ => unreachable!()
        };

        let body = if method.is_pseudomethod {
            if type_spec.value_variant.is_some() {
                quote! { Self::#native_name_ident }
            } else {
                quote! { |rcv, args, vm| Some(Self::#native_name_ident(rcv.unwrap(), #arguments)) }
            }
        } else {
            let body_return = if has_return {
                if let TypeRepr::SelfType(_) = &method.return_type {
                    match &type_spec.value_variant {
                        Some(variant) => {
                            let variant = format_ident!("{}", variant);
                            quote! {
                                let inst = inst.#native_name_ident(#arguments);
                                Some(crate::vm::value::Value::#variant(
                                    std::sync::Arc::new(std::cell::RefCell::new(inst))
                                ))
                            }
                        }
                        None => {
                            quote! {
                                let inst = inst.#native_name_ident(#arguments);
                                let (module_idx, type_id) = vm.type_id_for_name(#fully_qualified_type_name);
                                Some(crate::vm::value::Value::new_native_instance_obj(
                                    module_idx,
                                    type_id,
                                    std::boxed::Box::new(inst)
                                ))
                            }
                        }
                    }
                } else {
                    quote! { Some(inst.#native_name_ident(#arguments)) }
                }
            } else {
                quote! {
                    inst.#native_name_ident(#arguments);
                    None
                }
            };
            let invocation = if method.is_mut {
                if type_spec.value_variant.is_some() {
                    quote! {
                        let inst = &mut *rcv_obj.borrow_mut();
                        #body_return
                    }
                } else {
                    quote! {
                        let mut rcv = &mut *rcv_obj.borrow_mut();
                        let mut inst = rcv.inst.downcast_mut::<Self>().unwrap();
                        #body_return
                    }
                }
            } else {
                if type_spec.value_variant.is_some() {
                    quote! {
                        let inst = &*rcv_obj.borrow();
                        #body_return
                    }
                } else {
                    quote! {
                        let rcv = &*rcv_obj.borrow();
                        let mut inst = rcv.inst.downcast_ref::<Self>().unwrap();
                        #body_return
                    }
                }
            };

            let b = match &type_spec.value_variant {
                Some(variant) => {
                    let variant = format_ident!("{}", variant);
                    quote! {
                        if let Some(crate::vm::value::Value::#variant(rcv_obj)) = rcv { #invocation } else { unreachable!() }
                    }
                }
                None => {
                    quote! {
                        if let Some(crate::vm::value::Value::NativeInstanceObj(rcv_obj)) = rcv { #invocation } else { unreachable!() }
                    }
                }
            };

            quote! {
                |rcv, args, vm| #b
            }
        };

        quote! {
            (#name.to_string(), crate::vm::value::Value::NativeFn(crate::vm::value::NativeFn {
                name: #name,
                receiver: None,
                native_fn: #body,
                has_return: #has_return,
            }))
        }
    });

    let static_methods = type_spec.static_methods.iter().map(|static_method| {
        let name = &static_method.name;
        let num_args = static_method.args.len();
        let has_return = static_method.return_type != TypeRepr::Unit;
        let native_name_ident = format_ident!("{}", &static_method.native_method_name);
        let native_method_arity = static_method.native_method_arity;

        let arguments = match native_method_arity {
            0 => quote! {},
            1 => quote! {
                crate::builtins::arguments::Arguments::new(#name, #num_args, args)
            },
            2 => quote! {
                crate::builtins::arguments::Arguments::new(#name, #num_args, args), vm
            },
            _ => unreachable!()
        };

        let body = if has_return {
            match &type_spec.value_variant {
                Some(variant) => {
                    let variant = format_ident!("{}", variant);
                    quote! {
                        let inst = Self::#native_name_ident(#arguments);
                        Some(crate::vm::value::Value::#variant(
                            std::sync::Arc::new(std::cell::RefCell::new(inst))
                        ))
                    }
                }
                None => {
                    if let TypeRepr::SelfType(_) = &static_method.return_type {
                        quote! {
                            let inst = Self::#native_name_ident(#arguments);
                            let (module_idx, type_id) = vm.type_id_for_name(#fully_qualified_type_name);
                            Some(crate::vm::value::Value::new_native_instance_obj(
                                module_idx,
                                type_id,
                                std::boxed::Box::new(inst)
                            ))
                        }
                    } else {
                        quote! { Some(Self::#native_name_ident(#arguments)) }
                    }
                }
            }
        } else {
            quote! {
                Self::#native_name_ident(#arguments);
                None
            }
        };

        quote! {
            (#name.to_string(), crate::vm::value::Value::NativeFn(crate::vm::value::NativeFn {
                name: #name,
                receiver: None,
                native_fn: |rcv, args, vm| { #body },
                has_return: #has_return,
            }))
        }
    });

    let to_string_method_code = if type_spec.is_pseudotype || type_spec.value_variant.is_some() {
        quote! {
            ("toString".to_string(), crate::vm::value::Value::NativeFn(crate::vm::value::NativeFn {
                name: "toString",
                receiver: None,
                native_fn: |rcv, _args, vm| {
                    Some(Value::new_string_obj(
                        crate::builtins::common::to_string(&rcv.unwrap(), vm))
                    )
                },
                has_return: true,
            })),
        }
    } else {
        quote! {
            ("toString".to_string(), crate::vm::value::Value::NativeFn(crate::vm::value::NativeFn {
                name: "toString",
                receiver: None,
                native_fn: |rcv, _args, vm| {
                    if let Some(crate::vm::value::Value::NativeInstanceObj(rcv_obj)) = rcv {
                        let rcv = &*rcv_obj.borrow();
                        let mut inst = rcv.inst.downcast_ref::<Self>().unwrap();
                        Some(rcv.inst.method_to_string(vm))
                    } else { unreachable!() }
                },
                has_return: true,
            })),
        }
    };

    quote! {
        fn get_type_value() -> crate::vm::value::TypeValue where Self: core::marker::Sized {
            crate::vm::value::TypeValue {
                name: #type_name.to_string(),
                module_name: #module_name.to_string(),
                fields: vec![ #(#fields),* ],
                constructor: Some(Self::construct),
                methods: vec![
                    #to_string_method_code
                    #(#methods),*
                ],
                static_fields: vec![ #(#static_methods),* ],
            }
        }
    }
}

fn gen_to_string_method_code(to_string_method_name: &Option<String>) -> proc_macro2::TokenStream {
    match to_string_method_name {
        Some(method_name) => {
            let method_name_ident = format_ident!("{}", method_name);
            quote! {
                fn method_to_string(&self, vm: &mut crate::vm::vm::VM) -> crate::vm::value::Value {
                    crate::vm::value::Value::new_string_obj(self.#method_name_ident(vm))
                }
            }
        }
        None => quote! {
            fn method_to_string(&self, vm: &mut crate::vm::vm::VM) -> crate::vm::value::Value {
                use itertools::Itertools;

                let typ_val = Self::get_type_value();
                let fields = typ_val.fields.into_iter().zip(self.get_field_values())
                    .map(|(field_name, field_value)| format!("{}: {}", field_name, crate::builtins::common::to_string(&field_value, vm)))
                    .join(", ");

                crate::vm::value::Value::new_string_obj(format!("{}({})", typ_val.name, fields))
            }
        }
    }
}

fn gen_get_field_values_method_code(type_spec: &TypeSpec) -> proc_macro2::TokenStream {
    let code = type_spec.fields.iter().filter_map(|field| {
        field.getter.as_ref().map(|getter| {
            let method_name_ident = format_ident!("{}", getter.native_method_name);
            quote! { self.#method_name_ident() }
        })
    });

    quote! {
        fn get_field_values(&self) -> std::vec::Vec<crate::vm::value::Value> {
            vec![ #(#code),* ]
        }
    }
}

fn gen_get_field_value_method_code(type_spec: &TypeSpec) -> proc_macro2::TokenStream {
    let code = type_spec.fields.iter().enumerate().map(|(idx, field)| {
        let method_name = &field.getter.as_ref().unwrap().native_method_name;
        let method_name_ident = format_ident!("{}", method_name);

        quote! { #idx => self.#method_name_ident() }
    });

    quote! {
        fn get_field_value(&self, field_idx: usize) -> crate::vm::value::Value {
            match field_idx {
                #(#code,)*
                _ => unreachable!(),
            }
        }
    }
}

fn gen_set_field_value_method_code(type_spec: &TypeSpec) -> proc_macro2::TokenStream {
    let code = type_spec.fields.iter().enumerate().filter_map(|(idx, field)| {
        field.setter.as_ref().map(|setter| {
            let method_name_ident = format_ident!("{}", setter.native_method_name);
            quote! { #idx => self.#method_name_ident(value) }
        })
    });

    quote! {
        fn set_field_value(&mut self, field_idx: usize, value: crate::vm::value::Value) {
            match field_idx {
                #(#code,)*
                _ => unreachable!(),
            }
        }
    }
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

fn parse_function(attr_args: HashMap<String, String>, function: &syn::ItemFn) -> Result<MethodSpec, syn::Error> {
    if let Some(FnArg::Receiver(_)) = function.sig.inputs.first() {
        let msg = format!("The function bound via #[abra_function] must not receive self");
        return Err(syn::Error::new(function.span(), msg));
    }
    let native_method_arity = function.sig.inputs.len();
    if native_method_arity > 2 {
        let msg = format!("The function bound via #[abra_function] receives too many parameters; bound functions can only receive up to 2 parameters (Arguments and VM)");
        return Err(syn::Error::new(function.sig.inputs.span(), msg));
    }

    let signature = match attr_args.get("signature") {
        Some(signature) => match parse_fn_signature(None, &vec![], signature) {
            Ok(s) => s,
            Err(e) => {
                let msg = format!("Invalid signature provided to #[abra_function]: {}", e);
                return Err(syn::Error::new(Span::call_site(), msg));
            }
        }
        None => {
            let msg = "Missing required parameter `signature` in #[abra_function] attribute".to_string();
            return Err(syn::Error::new(Span::call_site(), msg));
        }
    };
    let Signature { func_name, args, return_type } = signature;

    if return_type == TypeRepr::Unit && function.sig.output != syn::ReturnType::Default {
        let msg = format!("The function bound via #[abra_function] has no return type in its signature, and therefore must not have a return value");
        return Err(syn::Error::new(function.sig.output.span(), msg));
    }

    let is_variadic = args.iter().any(|arg| arg.is_variadic);

    let method_name = function.sig.ident.to_string();
    Ok(MethodSpec {
        native_method_name: method_name.clone(),
        native_method_arity,
        name: func_name,
        args,
        return_type,
        is_static: true,
        is_mut: false,
        is_variadic,
        is_pseudomethod: false,
    })
}

fn generate_function_code(static_method: MethodSpec) -> proc_macro2::TokenStream {
    let name = &static_method.name;
    let num_args = static_method.args.len();
    let has_return = static_method.return_type != TypeRepr::Unit;
    let native_name_ident = format_ident!("{}", &static_method.native_method_name);
    let native_method_arity = static_method.native_method_arity;

    let arguments = match native_method_arity {
        0 => quote! {},
        1 => quote! { crate::builtins::arguments::Arguments::new(#name, #num_args, args) },
        2 => quote! { crate::builtins::arguments::Arguments::new(#name, #num_args, args), vm },
        _ => unreachable!()
    };

    let body = if has_return {
        quote! { Some(#native_name_ident(#arguments)) }
    } else {
        quote! {
            #native_name_ident(#arguments);
            None
        }
    };

    let fn_type = {
        let args = static_method.args.iter().map(|arg| {
            let MethodArgSpec { name, typ, is_optional, .. } = arg;
            let typ = gen_rust_type_path(typ, &"BOGUS".to_string(), &"BOGUS".to_string());

            quote! { (#name.to_string(), #typ, #is_optional) }
        });

        let return_type = gen_rust_type_path(&static_method.return_type, &"BOGUS".to_string(), &"BOGUS".to_string());
        let is_variadic = &static_method.is_variadic;

        quote! {
            crate::typechecker::types::Type::Fn(crate::typechecker::types::FnType {
                arg_types: vec![#(#args),*],
                type_args: vec![],
                ret_type: Box::new(#return_type),
                is_variadic: #is_variadic,
            })
        }
    };

    let native_value = quote! {
        crate::vm::value::Value::NativeFn(crate::vm::value::NativeFn {
            name: #name,
            receiver: None,
            native_fn: |rcv, args, vm| { #body },
            has_return: #has_return,
        })
    };

    let gen_fn_name = format_ident!("{}__gen_spec", static_method.native_method_name);
    quote! {
        fn #gen_fn_name() -> (std::string::String, crate::typechecker::types::Type, crate::vm::value::Value) {
            (#name.to_string(), #fn_type, #native_value)
        }
    }
}

fn find_attr<'a>(attrs: &'a Vec<syn::Attribute>, name: &str) -> Option<(usize, &'a syn::Attribute)> {
    attrs.into_iter().enumerate().find(|(_, attr)| {
        attr.path.segments.first().map_or(false, |s| {
            s.ident.to_string() == name.to_string()
        })
    })
}

fn parse_attr(attr: &syn::Attribute) -> HashMap<String, String> {
    let mut map = HashMap::new();

    match attr.parse_meta() {
        Err(e) => eprintln!("{:#?}", e),
        Ok(syn::Meta::List(meta)) => {
            for nested in meta.nested {
                if let Some((arg, val)) = parse_attr_meta(nested) {
                    map.insert(arg, val);
                }
                // match nested {
                //     NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                //         let (arg, val) = parse_attr_namevalue(nv);
                //         map.insert(arg, val);
                //     }
                //     NestedMeta::Meta(syn::Meta::Path(path)) => {
                //         let bool_arg = path.segments[0].ident.to_string();
                //         map.insert(bool_arg, "true".to_string());
                //     }
                //     _ => {}
                // }
            }
        }
        _ => {}
    }

    map
}

fn parse_attr_meta(nested: syn::NestedMeta) -> Option<(String, String)> {
    match nested {
        NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
            let arg = nv.path.segments[0].ident.to_string();
            let val = match nv.lit {
                syn::Lit::Str(s) => s.value(),
                syn::Lit::Bool(b) => b.value.to_string(),
                _ => unreachable!()
            };
            Some((arg, val))
        }
        NestedMeta::Meta(syn::Meta::Path(path)) => {
            let bool_arg = path.segments[0].ident.to_string();
            Some((bool_arg, "true".to_string()))
        }
        _ => None
    }
}
