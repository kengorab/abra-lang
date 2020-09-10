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
        use crate::builtins::native_types::NativeType;
        use crate::typechecker::types::{Type, FnType};
        use crate::vm::value::{Obj, Value};
        use crate::vm::vm::VM;
        use std::sync::Arc;
        use std::cell::RefCell;
    };

    write_rust_file(&builtins_dir_path, imports_code, rust_code);
}

fn generate_code_for_type(typ: &Type) -> TokenStream {
    let repr = match typ {
        Type::Bool => "Bool".to_string(),
        Type::Int => "Int".to_string(),
        Type::String => "String".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Reference(ref_name, _) => ref_name.replace("Native", ""),
        Type::Array(inner_type) => {
            let inner_type_code = generate_code_for_type(inner_type);
            return quote! { Type::Array(Box::new(#inner_type_code)) };
        }
        Type::Generic(name) => {
            return quote! {
              Type::Generic(#name.to_string())
            };
        }
        Type::Fn(FnType { type_args, arg_types, ret_type }) => {
            let arg_types_code = arg_types.into_iter().map(|(name, typ, is_optional)| {
                let arg_type_code = generate_code_for_type(typ);
                quote! { (#name.to_string(), #arg_type_code, #is_optional)}
            });
            let ret_type_code = generate_code_for_type(ret_type);
            return quote! {
                Type::Fn(FnType {
                    type_args: vec![#(#type_args),*],
                    arg_types: vec![#(#arg_types_code),*],
                    ret_type: Box::new(#ret_type_code)
                })
            };
        }
        _ => todo!()
    };
    let repr = format_ident!("{}", repr);
    quote! { Type::#repr }
}

fn generate_code_for_typedef(typ: StructType) -> TokenStream {
    let StructType { name: type_name, fields, methods, .. } = typ;

    let obj_type_name = match type_name.as_str() {
        "NativeString" => "StringObj",
        "NativeArray" => "ArrayObj",
        _ => todo!()
    };
    let obj_type_name_ident = format_ident!("{}", obj_type_name);

    let type_name_ident = format_ident!("{}", type_name);
    let trait_name_ident = format_ident!("{}MethodsAndFields", type_name);

    let mut field_idx: usize = 0;

    let mut trait_field_methods_code = Vec::new();
    let mut native_type_fields_and_methods_code = Vec::new();
    let mut get_field_value_code = Vec::new();
    for (field_name, field_type, _) in fields {
        let field_method_name_ident = format_ident!("field_{}", field_name);
        trait_field_methods_code.push(quote! {
            fn #field_method_name_ident(obj: &Arc<RefCell<Obj>>) -> Value;
        });

        let field_type_code = generate_code_for_type(&field_type);

        get_field_value_code.push(quote! {
            #field_idx => Self::#field_method_name_ident(obj)
        });

        native_type_fields_and_methods_code.push(quote! {
            #field_name => Some((#field_idx, #field_type_code))
        });

        field_idx += 1;
    }

    let mut trait_method_methods_code = Vec::new();
    let mut get_method_value_code = Vec::new();
    for (method_name, method_type) in methods {
        let method_method_name_ident = format_ident!("method_{}", method_name.to_snake_case());
        trait_method_methods_code.push(quote! {
            fn #method_method_name_ident(receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>, vm: &mut VM) -> Option<Value>;
        });

        let FnType { type_args, arg_types, ret_type } = if let Type::Fn(t) = method_type { t } else { unreachable!() };

        let has_return = *ret_type != Type::Unit;
        get_method_value_code.push(quote! {
            #field_idx => Value::NativeFn(NativeFn {
                name: #method_name,
                receiver: Some(obj.clone()),
                native_fn: Self::#method_method_name_ident,
                has_return: #has_return,
            })
        });

        let mut all_arg_types_code = Vec::new();
        for (arg_name, arg_type, is_opt) in arg_types {
            let arg_type_code = generate_code_for_type(&arg_type);
            all_arg_types_code.push(quote! {
                (#arg_name.to_string(), #arg_type_code, #is_opt)
            })
        }
        let ret_type_code = generate_code_for_type(&ret_type);
        let method_type_code = quote! {
            Type::Fn(FnType {
                type_args: vec![#(#type_args.to_string()),*],
                arg_types: vec![#(#all_arg_types_code),*],
                ret_type: Box::new(#ret_type_code)
            })
        };
        native_type_fields_and_methods_code.push(quote! {
            #method_name => Some((#field_idx, #method_type_code))
        });

        field_idx += 1;
    }

    // For some reason I keep getting a warning on the for-loop above, which goes away when this line is here. Weird
    let _ = 0;

    quote! {
        pub struct #type_name_ident;

        pub trait #trait_name_ident {
            #(#trait_field_methods_code)*
            #(#trait_method_methods_code)*
        }

        impl NativeType for #type_name_ident {
            fn get_field_or_method(name: &str) -> Option<(usize, Type)> {
                match name {
                    #(#native_type_fields_and_methods_code,)*
                    _ => None
                }
            }

            fn get_field_value(obj: &Arc<RefCell<Obj>>, field_idx: usize) -> Value {
                match &*obj.borrow() {
                    Obj::#obj_type_name_ident(_) => {
                        match field_idx {
                            #(#get_field_value_code,)*
                            #(#get_method_value_code,)*
                            _ => unreachable!()
                        }
                    }
                    _ => unreachable!()
                }
            }
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