use quote::{quote, format_ident};
use crate::signature::{TypeRepr, MethodArgSpec};
use crate::type_utils::gen_rust_type_path;
use crate::abra_methods::parsing::MethodSpec;

pub fn generate_function_code(static_method: MethodSpec) -> proc_macro2::TokenStream {
    let name = &static_method.name;
    let num_args = static_method.args.len();
    let native_name_ident = format_ident!("{}", &static_method.native_method_name);
    let native_method_arity = static_method.native_method_arity;

    let arguments = match native_method_arity {
        0 => quote! {},
        1 => quote! { crate::builtins::arguments::Arguments::new(#name, #num_args, args) },
        2 => quote! { crate::builtins::arguments::Arguments::new(#name, #num_args, args), vm },
        _ => unreachable!()
    };

    let body = if static_method.return_type != TypeRepr::Unit {
        quote! { #native_name_ident(#arguments) }
    } else {
        quote! {
            #native_name_ident(#arguments);
            crate::vm::value::Value::Nil
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
                is_enum_constructor: false,
            })
        }
    };

    let native_value = quote! {
        crate::vm::value::Value::NativeFn(crate::vm::value::NativeFn {
            name: #name,
            receiver: None,
            native_fn: |rcv, args, vm| { #body },
        })
    };

    let gen_fn_name = format_ident!("{}__gen_spec", static_method.native_method_name);
    quote! {
        fn #gen_fn_name() -> (std::string::String, crate::typechecker::types::Type, crate::vm::value::Value) {
            (#name.to_string(), #fn_type, #native_value)
        }
    }
}
