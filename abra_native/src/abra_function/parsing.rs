use syn::FnArg;
use std::collections::HashMap;
use proc_macro2::Span;
use syn::spanned::Spanned;
use crate::signature::{TypeRepr, parse_fn_signature, Signature};
use crate::abra_methods::parsing::MethodSpec;

pub fn parse_function(attr_args: HashMap<String, String>, function: &syn::ItemFn) -> Result<MethodSpec, syn::Error> {
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
        // is_static: true,
        is_mut: false,
        is_variadic,
        is_pseudomethod: false,
    })
}
