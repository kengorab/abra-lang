extern crate heck;
extern crate quote;

use abra_core::lexer::lexer::tokenize;
use abra_core::lexer::tokens::Token;
use abra_core::parser::parser::parse;
use abra_core::typechecker::typechecker::typecheck;
use abra_core::typechecker::typed_ast::TypedAstNode;
use abra_core::typechecker::types::{Type, StructType, FnType};

use std::fs::{read_to_string, write};
use std::path::{Path, PathBuf};
use std::process;

use heck::SnakeCase;
use proc_macro2::TokenStream;
use quote::{quote, format_ident};

fn main() {
    let builtins_dir_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("abra_core")
        .join("src")
        .join("builtins");

    let rust_code = read_typedefs_file(&builtins_dir_path).into_iter()
        .map(generate_code_for_typedef);

    let imports_code = quote! {
        use crate::builtins::native_fns::NativeFn;
        use crate::builtins::native::NativeType;
        use crate::typechecker::types::{Type, FnType};
        use crate::vm::value::Value;
        use crate::vm::vm::VM;
    };

    write_rust_file(&builtins_dir_path, imports_code, rust_code);
}

fn generate_code_for_type(typ: &Type) -> TokenStream {
    let repr = match typ {
        Type::Bool => "Bool".to_string(),
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::String => "String".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Any => "Any".to_string(),
        Type::Reference(ref_name, _) => ref_name.replace("Native", ""),
        Type::Array(inner_type) => {
            let inner_type_code = generate_code_for_type(inner_type);
            return quote! { Type::Array(Box::new(#inner_type_code)) };
        }
        Type::Set(inner_type) => {
            let inner_type_code = generate_code_for_type(inner_type);
            return quote! { Type::Set(Box::new(#inner_type_code)) };
        }
        Type::Map(key_type, value_type) => {
            let key_type_code = generate_code_for_type(key_type);
            let value_type_code = generate_code_for_type(value_type);
            return quote! { Type::Map(Box::new(#key_type_code), Box::new(#value_type_code)) };
        }
        Type::Option(inner_type) => {
            let inner_type_code = generate_code_for_type(inner_type);
            return quote! { Type::Option(Box::new(#inner_type_code)) };
        }
        Type::Generic(name) => {
            return quote! {
              Type::Generic(#name.to_string())
            };
        }
        Type::Union(types) => {
            let types = types.iter().map(|t| generate_code_for_type(t)).collect::<Vec<_>>();
            return quote! {
              Type::Union(vec![#(#types),*])
            };
        }
        Type::Tuple(types) => {
            let types = types.iter().map(|t| generate_code_for_type(t)).collect::<Vec<_>>();
            return quote! {
              Type::Tuple(vec![#(#types),*])
            };
        }
        Type::Fn(FnType { type_args, arg_types, ret_type, is_variadic }) => {
            let arg_types_code = arg_types.into_iter().map(|(name, typ, is_optional)| {
                let arg_type_code = generate_code_for_type(typ);
                quote! { (#name.to_string(), #arg_type_code, #is_optional)}
            });
            let ret_type_code = generate_code_for_type(ret_type);
            return quote! {
                Type::Fn(FnType {
                    type_args: vec![#(#type_args.to_string()),*],
                    arg_types: vec![#(#arg_types_code),*],
                    ret_type: Box::new(#ret_type_code),
                    is_variadic: #is_variadic
                })
            };
        }
        _ => todo!()
    };
    let repr = format_ident!("{}", repr);
    quote! { Type::#repr }
}

fn generate_code_for_typedef(typ: StructType) -> TokenStream {
    let StructType { name: type_name, fields, methods, static_fields, .. } = typ;

    let type_name_ident = format_ident!("{}", type_name);
    let trait_name_ident = format_ident!("{}MethodsAndFields", type_name);

    let mut field_idx: usize = 0;
    let mut trait_fields_code = Vec::new();
    let mut native_type_fields_code = Vec::new();
    let mut get_field_value_code = Vec::new();
    for (field_name, field_type, _) in fields {
        let field_method_name_ident = format_ident!("field_{}", field_name);
        trait_fields_code.push(quote! {
            fn #field_method_name_ident(obj: Box<Value>) -> Value;
        });

        let field_type_code = generate_code_for_type(&field_type);

        get_field_value_code.push(quote! {
            #field_idx => Self::#field_method_name_ident(obj)
        });

        native_type_fields_code.push(quote! {
            #field_name => Some((#field_idx, #field_type_code))
        });

        field_idx += 1;
    }

    let mut method_idx: usize = 0;
    let mut trait_methods_code = Vec::new();
    let mut native_type_methods_code = Vec::new();
    let mut get_method_value_code = Vec::new();
    for (method_name, method_type) in methods {
        let method_method_name_ident = format_ident!("method_{}", method_name.to_snake_case());
        trait_methods_code.push(quote! {
            fn #method_method_name_ident(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
        });

        let FnType { ret_type, .. } = if let Type::Fn(t) = &method_type { t } else { unreachable!() };

        let has_return = **ret_type != Type::Unit;
        get_method_value_code.push(quote! {
            #method_idx => Value::NativeFn(NativeFn {
                name: #method_name,
                receiver: Some(obj),
                native_fn: Self::#method_method_name_ident,
                has_return: #has_return,
            })
        });

        let method_type_code = generate_code_for_type(&method_type);
        native_type_methods_code.push(quote! {
            #method_name => Some((#method_idx, #method_type_code))
        });

        method_idx += 1;
    }

    let mut static_field_idx: usize = 0;
    let mut trait_static_field_methods_code = Vec::new();
    let mut get_static_method_value_code = Vec::new();
    let mut native_type_static_fields_and_methods_code = Vec::new();
    for (name, typ, _) in static_fields {
        let FnType { ret_type, .. } = if let Type::Fn(t) = &typ { t } else { unimplemented!("Only implemented for static methods at the moment") };

        let static_method_name_ident = format_ident!("static_method_{}", name.to_snake_case());
        trait_static_field_methods_code.push(quote! {
            fn #static_method_name_ident(_receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
        });

        let has_return = **ret_type != Type::Unit;
        get_static_method_value_code.push(quote! {
            (#name.to_string(), Value::NativeFn(NativeFn {
                name: #name,
                receiver: None,
                native_fn: Self::#static_method_name_ident,
                has_return: #has_return,
            }))
        });

        let static_method_type_code = generate_code_for_type(&typ);
        native_type_static_fields_and_methods_code.push(quote! {
            #name => Some((#static_field_idx, #static_method_type_code))
        });

        static_field_idx += 1;
    }

    // For some reason I keep getting a warning on the for-loop above, which goes away when this line is here. Weird
    let _ = 0;

    let get_static_field_value_code = if get_static_method_value_code.is_empty() {
        quote! {
            fn get_static_field_values() -> Vec<(String, Value)> { vec![] }
        }
    } else {
        quote! {
            fn get_static_field_values() -> Vec<(String, Value)> { vec![#(#get_static_method_value_code,)*] }
        }
    };

    let get_static_field_or_method_code = if native_type_static_fields_and_methods_code.is_empty() {
        quote! {
            fn get_static_field_or_method(_name: &str) -> Option<(usize, Type)> { None }
        }
    } else {
        quote! {
            fn get_static_field_or_method(name: &str) -> Option<(usize, Type)> {
                match name {
                    #(#native_type_static_fields_and_methods_code,)*
                    _ => None
                }
            }
        }
    };

    quote! {
        pub struct #type_name_ident;

        pub trait #trait_name_ident {
            #(#trait_fields_code)*
            #(#trait_static_field_methods_code)*
            #(#trait_methods_code)*
        }

        impl NativeType for #type_name_ident {
            fn get_field_type(name: &str) -> Option<(usize, Type)> {
                match name {
                    #(#native_type_fields_code,)*
                    _ => None
                }
            }

            fn get_method_type(name: &str) -> Option<(usize, Type)> {
                match name {
                    #(#native_type_methods_code,)*
                    _ => None
                }
            }

            #get_static_field_or_method_code

            fn get_field_value(obj: Box<Value>, field_idx: usize) -> Value {
                match field_idx {
                    #(#get_field_value_code,)*
                    _ => unreachable!()
                }
            }

            fn get_method_value(obj: Box<Value>, method_idx: usize) -> Value {
                match method_idx {
                    #(#get_method_value_code,)*
                    _ => unreachable!()
                }
            }

            #get_static_field_value_code
        }
    }
}

fn read_typedefs_file(builtins_dir_path: &PathBuf) -> Vec<StructType> {
    let definitions_file = read_to_string(builtins_dir_path.join("native_types.abra")).unwrap();
    let tokens = tokenize(&definitions_file).unwrap();
    let ast = parse(tokens).unwrap();

    let (typechecker, typed_ast) = typecheck(ast).unwrap();
    if typed_ast.is_empty() {
        panic!("Empty native_types.abra typedefs file, cannot proceed!")
    }
    let ref_types = typechecker.get_referencable_types();

    typed_ast.into_iter()
        .filter_map(|node|
            if let TypedAstNode::TypeDecl(_, typed_ast_node) = node {
                let name = Token::get_ident_name(&typed_ast_node.name);
                let ref_type = ref_types.clone()[&name].clone();
                if let Type::Struct(t) = ref_type {
                    Some(t)
                } else { None }
            } else { None }
        )
        .collect()
}

fn write_rust_file<I>(builtins_dir_path: &PathBuf, top_code: TokenStream, rust_code: I)
    where I: std::iter::Iterator<Item=TokenStream>
{
    let file = quote! {
        #top_code
        #(#rust_code)*
    };
    let contents = format!("// Auto-generated file, do not modify\n\n{}", file.to_string());

    let gen_native_types_file_path = builtins_dir_path.join("gen_native_types.rs");
    write(&gen_native_types_file_path, contents).unwrap();

    process::Command::new("rustfmt")
        .arg("--emit")
        .arg("files")
        .arg(gen_native_types_file_path)
        .spawn()
        .expect("rustfmt failed to run");
}
