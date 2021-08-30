use crate::signature::TypeRepr;
use quote::{quote, format_ident};

pub fn gen_rust_type_path(type_repr: &TypeRepr, module_name: &String, type_ref_name: &String) -> proc_macro2::TokenStream {
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
                "Unit" | "Any" | "Int" | "Float" | "String" | "Bool" | "Placeholder" => {
                    let typ = format_ident!("{}", i);
                    quote! { crate::typechecker::types::Type::#typ }
                }
                "Set" => {
                    let inner_type = type_args.get(0).expect("Sets require T value");
                    let inner_type_repr = gen_rust_type_path(inner_type, module_name, type_ref_name);

                    quote! { crate::typechecker::types::Type::Set(std::boxed::Box::new(#inner_type_repr)) }
                }
                "Map" => {
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
                    is_variadic: false,
                    is_enum_constructor: false,
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
