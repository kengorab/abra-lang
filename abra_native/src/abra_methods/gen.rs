use proc_macro::TokenStream;
use quote::{format_ident, quote};
use crate::signature::{MethodArgSpec, TypeRepr};
use crate::type_utils::gen_rust_type_path;
use crate::abra_methods::parsing::{ConstructorSpec, MethodSpec, EnumVariantSpec, EnumVariantDataSpec};
use crate::abra_type::parsing::FieldSpec;

#[derive(Debug)]
pub struct TypeSpec {
    pub(crate) native_type_name: String,
    pub(crate) name: String,
    pub(crate) module_name: String,
    pub(crate) type_args: Vec<String>,
    pub(crate) constructor: Option<ConstructorSpec>,
    pub(crate) to_string_method: Option<String>,
    pub(crate) fields: Vec<FieldSpec>,
    pub(crate) methods: Vec<MethodSpec>,
    pub(crate) pseudo_methods: Vec<MethodSpec>,
    pub(crate) static_methods: Vec<MethodSpec>,
    pub(crate) is_pseudotype: bool,
    pub(crate) is_noconstruct: bool,
    pub(crate) value_variant: Option<String>,
    pub(crate) is_enum: bool,
    pub(crate) enum_variants: Vec<EnumVariantSpec>,
    pub(crate) enum_variant_data: Option<EnumVariantDataSpec>,
}

pub fn gen_native_type_code(type_spec: &TypeSpec) -> TokenStream {
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
                    is_enum_constructor: false,
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
                    is_enum_constructor: false,
                }),
                true
            )
        }
    });

    let native_type_name_ident = format_ident!("{}", &type_spec.native_type_name);
    let is_constructable = !type_spec.is_noconstruct;
    let ts = if !type_spec.is_enum {
        quote! {
            impl crate::builtins::native_value_trait::NativeTyp for #native_type_name_ident {
                fn is_struct() -> bool { true }
                fn get_struct_type() -> crate::typechecker::types::StructType where Self: Sized {
                    crate::typechecker::types::StructType {
                        name: #type_name.to_string(),
                        type_args: vec![ #(#type_args),* ],
                        constructable: #is_constructable,
                        fields: vec![ #(#fields),* ],
                        static_fields: vec![ #(#static_methods),* ],
                        methods: vec![
                            ("toString".to_string(), crate::typechecker::types::Type::Fn(crate::typechecker::types::FnType {
                                arg_types: vec![],
                                type_args: vec![],
                                ret_type: std::boxed::Box::new(crate::typechecker::types::Type::String),
                                is_variadic: false,
                                is_enum_constructor: false,
                            })),
                            #(#methods),*
                        ],
                    }
                }
                fn get_enum_type() -> crate::typechecker::types::EnumType where Self: Sized {
                    unreachable!("This is not an enum type");
                }
            }
        }
    } else {
        let variants = type_spec.enum_variants.iter().map(|variant| {
            let EnumVariantSpec { name, args, return_type, .. } = variant;

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
                        arg_types: vec![ #(#args),* ],
                        type_args: vec![],
                        ret_type: std::boxed::Box::new(#return_type),
                        is_variadic: false,
                        is_enum_constructor: true,
                    })
                )
            }
        });

        quote! {
            impl crate::builtins::native_value_trait::NativeTyp for #native_type_name_ident {
                fn is_struct() -> bool { false }
                fn get_struct_type() -> crate::typechecker::types::StructType where Self: Sized {
                    unreachable!("This is not a struct type");
                }
                fn get_enum_type() -> crate::typechecker::types::EnumType where Self: Sized {
                    crate::typechecker::types::EnumType {
                        name: "Result".to_string(),
                        type_args: vec![ #(#type_args),* ],
                        variants: vec![ #(#variants),* ],
                        static_fields: vec![ #(#static_methods),* ],
                        methods: vec![
                            ("toString".to_string(), crate::typechecker::types::Type::Fn(crate::typechecker::types::FnType {
                                arg_types: vec![],
                                type_args: vec![],
                                ret_type: std::boxed::Box::new(crate::typechecker::types::Type::String),
                                is_variadic: false,
                                is_enum_constructor: false,
                            })),
                            #(#methods),*
                        ],
                    }
                }
            }
        }
    };

    ts.into()
}

pub fn gen_native_value_code(type_spec: &TypeSpec) -> TokenStream {
    let construct_method_code = gen_construct_code(type_spec);
    let get_type_value_method_code = gen_get_type_value_method_code(type_spec);
    let to_string_method_code = gen_to_string_method_code(type_spec);
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
                crate::vm::value::Value::new_native_instance_obj(type_id, std::boxed::Box::new(inst))
            }
        }
        None => {
            quote! { unreachable!("The type bound to #type_name has not bound a function via #[abra_constructor]. You were probably never intended to construct this type") }
        }
    };

    quote! {
        fn construct(type_id: usize, args: std::vec::Vec<crate::vm::value::Value>) -> crate::vm::value::Value where Self: core::marker::Sized {
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
                quote! { |rcv, args, vm| Self::#native_name_ident(rcv.unwrap(), #arguments) }
            }
        } else {
            let body_return = if method.return_type != TypeRepr::Unit {
                if let TypeRepr::SelfType(_) = &method.return_type {
                    match &type_spec.value_variant {
                        Some(variant) => {
                            let variant = format_ident!("{}", variant);
                            quote! {
                                let inst = inst.#native_name_ident(#arguments);
                                crate::vm::value::Value::#variant(
                                    std::sync::Arc::new(std::cell::RefCell::new(inst))
                                )
                            }
                        }
                        None => {
                            if type_spec.is_enum {
                                let enum_variant_data_fn_spec = &type_spec.enum_variant_data.as_ref().unwrap();
                                let fn_name = &enum_variant_data_fn_spec.native_method_name;
                                let fn_name_ident = format_ident!("{}", &fn_name);

                                quote! {
                                    let inst = inst.#native_name_ident(#arguments);
                                    let type_id = vm.type_id_for_name(#fully_qualified_type_name);
                                    let (variant_idx, _) = inst.#fn_name_ident();
                                    crate::vm::value::Value::new_native_enum_instance_obj(
                                        type_id,
                                        variant_idx,
                                        std::boxed::Box::new(inst)
                                    )
                                }
                            } else {
                                quote! {
                                    let inst = inst.#native_name_ident(#arguments);
                                    let type_id = vm.type_id_for_name(#fully_qualified_type_name);
                                    crate::vm::value::Value::new_native_instance_obj(
                                        type_id,
                                        std::boxed::Box::new(inst)
                                    )
                                }
                            }
                        }
                    }
                } else {
                    quote! { inst.#native_name_ident(#arguments) }
                }
            } else {
                quote! {
                    inst.#native_name_ident(#arguments);
                    crate::vm::value::Value::Nil
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
                    let t = if type_spec.is_enum {
                        quote! { crate::vm::value::Value::NativeEnumInstanceObj }
                    } else {
                        quote! { crate::vm::value::Value::NativeInstanceObj }
                    };

                    quote! {
                        if let Some(#t(rcv_obj)) = rcv { #invocation } else { unreachable!() }
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
            }))
        }
    });

    let static_methods = type_spec.static_methods.iter().map(|static_method| {
        let name = &static_method.name;
        let num_args = static_method.args.len();
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

        let body = if static_method.return_type != TypeRepr::Unit {
            match &type_spec.value_variant {
                Some(variant) => {
                    let variant = format_ident!("{}", variant);
                    quote! {
                        let inst = Self::#native_name_ident(#arguments);
                        crate::vm::value::Value::#variant(
                            std::sync::Arc::new(std::cell::RefCell::new(inst))
                        )
                    }
                }
                None => {
                    if let TypeRepr::SelfType(_) = &static_method.return_type {
                        if type_spec.is_enum {
                            let enum_variant_data_fn_spec = &type_spec.enum_variant_data.as_ref().unwrap();
                            let fn_name = &enum_variant_data_fn_spec.native_method_name;
                            let fn_name_ident = format_ident!("{}", &fn_name);

                            quote! {
                                let inst = Self::#native_name_ident(#arguments);
                                let type_id = vm.type_id_for_name(#fully_qualified_type_name);
                                let (variant_idx, _) = inst.#fn_name_ident();
                                crate::vm::value::Value::new_native_enum_instance_obj(
                                    type_id,
                                    variant_idx,
                                    std::boxed::Box::new(inst)
                                )
                            }
                        } else {
                            quote! {
                                let inst = Self::#native_name_ident(#arguments);
                                let type_id = vm.type_id_for_name(#fully_qualified_type_name);
                                crate::vm::value::Value::new_native_instance_obj(
                                    type_id,
                                    std::boxed::Box::new(inst)
                                )
                            }
                        }
                    } else {
                        quote! { Self::#native_name_ident(#arguments) }
                    }
                }
            }
        } else {
            quote! {
                Self::#native_name_ident(#arguments);
                crate::vm::value::Value::Nil
            }
        };

        quote! {
            (#name.to_string(), crate::vm::value::Value::NativeFn(crate::vm::value::NativeFn {
                name: #name,
                receiver: None,
                native_fn: |rcv, args, vm| { #body },
            }))
        }
    });

    let to_string_method_code = if type_spec.is_pseudotype || type_spec.value_variant.is_some() {
        quote! {
            ("toString".to_string(), crate::vm::value::Value::NativeFn(crate::vm::value::NativeFn {
                name: "toString",
                receiver: None,
                native_fn: |rcv, _args, vm| {
                    Value::new_string_obj(
                        crate::builtins::common::to_string(&rcv.unwrap(), vm)
                    )
                },
            })),
        }
    } else {
        let t = if type_spec.is_enum {
            quote! { crate::vm::value::Value::NativeEnumInstanceObj }
        } else {
            quote! { crate::vm::value::Value::NativeInstanceObj }
        };

        quote! {
            ("toString".to_string(), crate::vm::value::Value::NativeFn(crate::vm::value::NativeFn {
                name: "toString",
                receiver: None,
                native_fn: |rcv, _args, vm| {
                    if let Some(#t(rcv_obj)) = rcv {
                        let rcv = &*rcv_obj.borrow();
                        let mut inst = rcv.inst.downcast_ref::<Self>().unwrap();
                        rcv.inst.method_to_string(vm)
                    } else { unreachable!() }
                },
            })),
        }
    };

    if !type_spec.is_enum {
        quote! {
            fn get_type_value() -> crate::vm::value::Value where Self: core::marker::Sized {
                crate::vm::value::Value::Type(crate::vm::value::TypeValue {
                    name: #type_name.to_string(),
                    module_name: #module_name.to_string(),
                    fields: vec![ #(#fields),* ],
                    constructor: Some(Self::construct),
                    methods: vec![
                        #to_string_method_code
                        #(#methods),*
                    ],
                    static_fields: vec![ #(#static_methods),* ],
                })
            }
        }
    } else {
        let variants = type_spec.enum_variants.iter().enumerate().map(|(idx, variant)| {
            let EnumVariantSpec { native_method_name, native_method_arity, name, args, .. } = variant;

            let num_args = args.len();
            let native_name_ident = format_ident!("{}", &native_method_name);

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

            quote! {
                (
                    #name.to_string(),
                    crate::vm::value::Value::NativeFn(crate::vm::value::NativeFn {
                        name: #name,
                        receiver: None,
                        native_fn: |_rcv, args, vm| {
                            let type_id = vm.type_id_for_name(#fully_qualified_type_name);
                            let inst = Self::#native_name_ident(#arguments);
                            crate::vm::value::Value::new_native_enum_instance_obj(type_id, #idx, Box::new(inst))
                        },
                    })
                )
            }
        });

        quote! {
            fn get_type_value() -> crate::vm::value::Value where Self: core::marker::Sized {
                crate::vm::value::Value::Enum(crate::vm::value::EnumValue {
                    name: #type_name.to_string(),
                    module_name: #module_name.to_string(),
                    variants: vec![ #(#variants),* ],
                    methods: vec![
                        #to_string_method_code
                        #(#methods),*
                    ],
                    static_fields: vec![ #(#static_methods),* ],
                })
            }
        }
    }
}

fn gen_to_string_method_code(type_spec: &TypeSpec) -> proc_macro2::TokenStream {
    match &type_spec.to_string_method {
        Some(method_name) => {
            let method_name_ident = format_ident!("{}", method_name);
            quote! {
                fn method_to_string(&self, vm: &mut crate::vm::vm::VM) -> crate::vm::value::Value {
                    crate::vm::value::Value::new_string_obj(self.#method_name_ident(vm))
                }
            }
        }
        None => {
            if !type_spec.is_enum {
                let mut format_str = vec![format!("{}(", type_spec.name)];
                let mut fields_code = Vec::new();
                let num_fields = type_spec.fields.len();
                for (idx, field) in type_spec.fields.iter().enumerate() {
                    format_str.push(format!(
                        "{}{}: {{}}{}",
                        if idx > 0 { ", " } else { "" },
                        &field.name,
                        if idx == num_fields - 1 { ")" } else { "" }
                    ));

                    let getter_name = &field.getter.as_ref().unwrap().native_method_name;
                    let method_name_ident = format_ident!("{}", getter_name);
                    fields_code.push(quote! { self.#method_name_ident() });
                }
                let format_str = format_str.join("");

                quote! {
                    fn method_to_string(&self, vm: &mut crate::vm::vm::VM) -> crate::vm::value::Value {
                        crate::vm::value::Value::new_string_obj(format!(#format_str, #(#fields_code),*))
                    }
                }
            } else {
                let enum_variant_data_fn_spec = &type_spec.enum_variant_data.as_ref().unwrap();
                let fn_name = &enum_variant_data_fn_spec.native_method_name;
                let fn_name_ident = format_ident!("{}", &fn_name);

                let variant_str_branches = type_spec.enum_variants.iter().enumerate().map(|(idx, variant)| {
                    let variant_name = format!("{}.{}(", type_spec.name, variant.name);
                    quote! {
                        #idx => #variant_name
                    }
                });

                quote! {
                    fn method_to_string(&self, vm: &mut crate::vm::vm::VM) -> crate::vm::value::Value {
                        use itertools::Itertools;

                        let (idx, values) = self.#fn_name_ident();
                        let prefix = match idx {
                            #(#variant_str_branches,)*
                            _ => unreachable!(),
                        };
                        let values = values.iter().map(|v| v.to_string()).join(", ");
                        crate::vm::value::Value::new_string_obj(format!("{}{})", prefix, values))
                    }
                }
            }
        }
    }
}

fn gen_get_field_values_method_code(type_spec: &TypeSpec) -> proc_macro2::TokenStream {
    if type_spec.is_enum {
        let enum_variant_data_fn_spec = &type_spec.enum_variant_data.as_ref().unwrap();
        let fn_name = &enum_variant_data_fn_spec.native_method_name;
        let fn_name_ident = format_ident!("{}", &fn_name);

        return quote! {
            fn get_field_values(&self) -> std::vec::Vec<crate::vm::value::Value> {
                self.#fn_name_ident().1.iter().map(|v| (*v).clone()).collect()
            }
        };
    }

    let code = type_spec.fields.iter().map(|field| {
        let getter_name = &field.getter.as_ref().unwrap().native_method_name;
        let method_name_ident = format_ident!("{}", getter_name);
        quote! { self.#method_name_ident() }
    });

    quote! {
        fn get_field_values(&self) -> std::vec::Vec<crate::vm::value::Value> {
            vec![ #(#code),* ]
        }
    }
}

fn gen_get_field_value_method_code(type_spec: &TypeSpec) -> proc_macro2::TokenStream {
    if type_spec.is_enum {
        let enum_variant_data_fn_spec = &type_spec.enum_variant_data.as_ref().unwrap();
        let fn_name = &enum_variant_data_fn_spec.native_method_name;
        let fn_name_ident = format_ident!("{}", &fn_name);

        return quote! {
            fn get_field_value(&self, field_idx: usize) -> crate::vm::value::Value {
                let (_, values) = self.#fn_name_ident();
                values[field_idx].clone()
            }
        };
    }

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
    if type_spec.is_enum {
        return quote! {
            fn set_field_value(&mut self, _field_idx: usize, _value: crate::vm::value::Value) {
                unreachable!("Enum variant fields cannot be directly set");
            }
        };
    }

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
