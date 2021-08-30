use syn::{FnArg, ReturnType};
use syn::spanned::Spanned;
use crate::signature::{TypeRepr, MethodArgSpec, parse_fn_signature, Signature, parse_type};
use crate::parsing_common::{find_attr, parse_attr};

#[derive(Debug)]
pub struct GetterSpec {
    pub(crate) native_method_name: String,
}

#[derive(Debug)]
pub struct SetterSpec {
    pub(crate) native_method_name: String,
}

pub fn parse_getter(method: &mut syn::ImplItemMethod) -> Result<Option<(String, GetterSpec)>, syn::Error> {
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

pub fn parse_setter(method: &mut syn::ImplItemMethod) -> Result<Option<(String, SetterSpec)>, syn::Error> {
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

#[derive(Debug)]
pub struct MethodSpec {
    pub(crate) native_method_name: String,
    pub(crate) native_method_arity: usize,
    pub(crate) name: String,
    pub(crate) args: Vec<MethodArgSpec>,
    pub(crate) return_type: TypeRepr,
    pub(crate) is_static: bool,
    pub(crate) is_mut: bool,
    pub(crate) is_variadic: bool,
    pub(crate) is_pseudomethod: bool,
}

pub fn parse_method(type_name: &String, type_args: &Vec<String>, method: &mut syn::ImplItemMethod) -> Result<Option<MethodSpec>, syn::Error> {
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

pub fn parse_pseudomethod(type_args: &Vec<String>, method: &mut syn::ImplItemMethod) -> Result<Option<MethodSpec>, syn::Error> {
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

pub fn parse_static_method(type_name: &String, type_args: &Vec<String>, method: &mut syn::ImplItemMethod) -> Result<Option<MethodSpec>, syn::Error> {
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

#[derive(Debug)]
pub struct ConstructorSpec {
    pub(crate) native_fn_name: String,
}

pub fn parse_constructor(method: &mut syn::ImplItemMethod) -> Result<Option<ConstructorSpec>, syn::Error> {
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

pub fn parse_to_string_method(method: &mut syn::ImplItemMethod) -> Result<Option<String>, syn::Error> {
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

#[derive(Debug)]
pub struct EnumVariantSpec {
    pub(crate) native_method_name: String,
    pub(crate) native_method_arity: usize,
    pub(crate) name: String,
    pub(crate) args: Vec<MethodArgSpec>,
    pub(crate) return_type: TypeRepr,
}

pub fn parse_enum_variant(type_name: &String, type_args: &Vec<String>, method: &mut syn::ImplItemMethod) -> Result<Option<EnumVariantSpec>, syn::Error> {
    let (idx, abra_method_attr) = match find_attr(&method.attrs, "abra_enum_variant") {
        Some(attr) => attr,
        None => return Ok(None)
    };
    let attr_span = abra_method_attr.span();
    let attr = parse_attr(abra_method_attr);
    method.attrs.remove(idx);

    if let Some(FnArg::Receiver(_)) = method.sig.inputs.first() {
        let msg = format!("The function bound via #[abra_enum_variant] must not receive self");
        return Err(syn::Error::new(method.span(), msg));
    }
    let native_method_arity = method.sig.inputs.len();
    if native_method_arity > 2 {
        let msg = format!("The function bound via #[abra_enum_variant] receives too many parameters; bound functions can only receive up to 2 parameters (Arguments and VM)");
        return Err(syn::Error::new(method.sig.inputs.span(), msg));
    }

    let signature = match attr.get("signature") {
        Some(signature) => match parse_fn_signature(Some(type_name), type_args, signature) {
            Ok(s) => s,
            Err(e) => {
                let msg = format!("Invalid `signature` provided to #[abra_enum_variant]: {}", e);
                return Err(syn::Error::new(attr_span, msg));
            }
        }
        None => {
            let msg = "Missing required parameter `signature` in #[abra_enum_variant] attribute".to_string();
            return Err(syn::Error::new(attr_span, msg));
        }
    };
    let return_type = match attr.get("return_type") {
        Some(signature) => match parse_type(Some(type_name), type_args, signature) {
            Ok(s) => s,
            Err(e) => {
                let msg = format!("Invalid `return_type` provided to #[abra_enum_variant]: {}", e);
                return Err(syn::Error::new(attr_span, msg));
            }
        }
        None => {
            let msg = "Missing required parameter `return_type` in #[abra_enum_variant] attribute".to_string();
            return Err(syn::Error::new(attr_span, msg));
        }
    };
    let Signature { func_name, args, .. } = signature;

    if return_type == TypeRepr::Unit && method.sig.output != syn::ReturnType::Default {
        let msg = format!("The function bound via #[abra_enum_variant] has no return type in its signature, and therefore must not have a return value");
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
            let msg = format!("The function bound via #[abra_enum_variant] has a signature which returns '{}', so it must return Self", type_name);
            return Err(syn::Error::new(method.sig.span(), msg));
        }
    }

    let method_name = method.sig.ident.to_string();
    Ok(Some(EnumVariantSpec {
        native_method_name: method_name,
        native_method_arity,
        name: func_name,
        args,
        return_type,
    }))
}

#[derive(Debug)]
pub struct EnumVariantDataSpec {
    pub(crate) native_method_name: String,
    pub(crate) native_method_arity: usize,
}

pub fn parse_enum_variant_data(method: &mut syn::ImplItemMethod) -> Result<Option<EnumVariantDataSpec>, syn::Error> {
    let (idx, _) = match find_attr(&method.attrs, "abra_enum_variant_data") {
        Some(attr) => attr,
        None => return Ok(None)
    };
    method.attrs.remove(idx);

    match method.sig.inputs.first() {
        Some(FnArg::Typed(_)) | None => {
            let msg = format!("The function bound via #[abra_enum_variant_data] must receive self");
            return Err(syn::Error::new(method.span(), msg));
        }
        Some(FnArg::Receiver(rcv)) if !rcv.reference.is_some() => {
            let msg = format!("The function bound via #[abra_enum_variant_data] must receive a reference to self");
            return Err(syn::Error::new(rcv.span(), msg));
        }
        _ => {}
    }
    let native_method_arity = method.sig.inputs.len();
    if native_method_arity != 1 {
        let msg = format!("The function bound via #[abra_enum_variant_data] receives too many parameters; bound functions can only receive 1 parameter (self)");
        return Err(syn::Error::new(method.sig.inputs.span(), msg));
    }

    let method_name = method.sig.ident.to_string();
    Ok(Some(EnumVariantDataSpec { native_method_name: method_name, native_method_arity }))
}
