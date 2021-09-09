use crate::parser::ast::{TypeIdentifier, ModuleId};
use crate::lexer::tokens::Token;
use std::collections::{HashMap, HashSet};
use itertools::Itertools;

#[derive(Debug, Clone, Eq, Hash)]
pub enum Type {
    Unit,
    Any,
    Union(Vec<Type>),
    Int,
    Float,
    String,
    Bool,
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Set(Box<Type>),
    Map(/* key_type: */ Box<Type>, /* value_type: */ Box<Type>),
    Option(Box<Type>),
    Fn(FnType),
    Type(/* type_name: */ String, /* underlying_type: */ Box<Type>, /* is_enum: */ bool),
    Struct(StructType),
    Enum(EnumType),
    Module(ModuleId),
    // Acts as a sentinel value, right now only for when a function is referenced recursively without an explicit return type
    Unknown,
    Placeholder,
    Generic(/* name: */ String),
    Reference(/* name: */ String, /* type_args: */ Vec<Type>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (&self, &other) {
            (Type::Union(ts1), Type::Union(ts2)) => {
                let ts1 = ts1.iter().collect::<HashSet<_>>();
                let ts2 = ts2.iter().collect::<HashSet<_>>();
                ts1.eq(&ts2)
            }
            (Type::Option(inner1), Type::Option(inner2)) |
            (Type::Array(inner1), Type::Array(inner2)) |
            (Type::Set(inner1), Type::Set(inner2)) => inner1.eq(&inner2),
            (Type::Tuple(inners1), Type::Tuple(inners2)) => inners1.eq(inners2),
            (Type::Map(kt1, vt1), Type::Map(kt2, vt2)) => {
                kt1.eq(&kt2) && vt1.eq(&vt2)
            }
            (Type::Fn(ft1), Type::Fn(ft2)) => ft1.eq(&ft2),
            (Type::Type(n1, t1, e1), Type::Type(n2, t2, e2)) => {
                n1.eq(n2) && t1.eq(&t2) && e1 == e2
            }
            (Type::Struct(st1), Type::Struct(st2)) => st1.eq(&st2),
            (Type::Enum(et1), Type::Enum(et2)) => et1.eq(&et2),
            (Type::Generic(n1), Type::Generic(n2)) => n1.eq(n2),
            (Type::Reference(n1, g1), Type::Reference(n2, g2)) => {
                n1.eq(n2) && g1.eq(g2)
            }
            (Type::Unit, Type::Unit) |
            (Type::Any, Type::Any) |
            (Type::Int, Type::Int) |
            (Type::Float, Type::Float) |
            (Type::String, Type::String) |
            (Type::Bool, Type::Bool) |
            (Type::Unknown, Type::Unknown) |
            (Type::Placeholder, Type::Placeholder) => true,
            (Type::Module(m1), Type::Module(m2)) => m1.eq(&m2),
            (_, _) => false,
        }
    }
}

#[derive(Debug, Clone, Eq, Hash)]
pub struct FnType {
    pub arg_types: Vec<(/* arg_name: */ String, /* arg_type: */ Type, /* is_optional: */ bool)>,
    pub type_args: Vec<String>,
    pub ret_type: Box<Type>,
    pub is_variadic: bool,
    pub is_enum_constructor: bool,
}

impl PartialEq for FnType {
    fn eq(&self, other: &Self) -> bool {
        if !self.type_args.eq(&other.type_args) || !self.ret_type.eq(&other.ret_type) {
            false
        } else {
            let arg_types = self.arg_types.iter().map(|(_, ty, is_o)| (ty, is_o)).collect::<Vec<_>>();
            let other_arg_types = other.arg_types.iter().map(|(_, ty, is_o)| (ty, is_o)).collect::<Vec<_>>();
            arg_types.eq(&other_arg_types)
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructType {
    pub name: String,
    pub type_args: Vec<(String, Type)>,
    pub constructable: bool,
    pub fields: Vec<StructTypeField>,
    pub static_fields: Vec<(/* name: */ String, /* type: */ Type, /* has_default_value: */ bool)>,
    pub methods: Vec<(String, Type)>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructTypeField {
    pub name: String,
    pub typ: Type,
    pub has_default_value: bool,
    pub readonly: bool,
}

pub struct FieldSpec {
    pub idx: usize,
    pub typ: Type,
    pub is_method: bool,
    pub readonly: bool,
}

impl StructType {
    pub fn get_field_or_method<S: AsRef<str>>(&self, name: S) -> Option<FieldSpec> {
        self.fields.iter().enumerate()
            .find(|(_, f)| f.name == name.as_ref())
            .map(|(idx, f)| FieldSpec { idx, typ: f.typ.clone(), is_method: false, readonly: f.readonly })
            .or_else(|| {
                self.methods.iter().enumerate()
                    .find(|(_, f)| f.0 == name.as_ref())
                    .map(|(idx, f)| FieldSpec { idx, typ: f.1.clone(), is_method: true, readonly: true })
            })
    }

    pub fn get_static_field_or_method<S: AsRef<str>>(&self, name: S) -> Option<FieldSpec> {
        self.static_fields.iter().enumerate()
            .find(|(_, f)| f.0 == name.as_ref())
            .map(|(idx, f)| FieldSpec { idx, typ: f.1.clone(), is_method: true, readonly: true }) // All static fields are methods atm
    }

    pub fn get_field_idx<S: AsRef<str>>(&self, name: S) -> Option<usize> {
        self.fields.iter().enumerate()
            .find(|(_, f)| f.name == name.as_ref())
            .map(|(idx, _)| idx)
    }

    pub fn get_method_idx<S: AsRef<str>>(&self, name: S) -> Option<usize> {
        self.methods.iter().enumerate()
            .find(|(_, f)| f.0 == name.as_ref())
            .map(|(idx, _)| idx)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct EnumType {
    pub name: String,
    pub type_args: Vec<(String, Type)>,
    pub variants: Vec<(String, Type)>,
    pub static_fields: Vec<(/* name: */ String, /* type: */ Type, /* has_default_value: */ bool)>,
    pub methods: Vec<(String, Type)>,
}

impl Type {
    pub fn repr(&self) -> String {
        #[inline]
        fn wrap_type_repr(t: &Type) -> String {
            let wrap = if let Type::Fn(_) = t { true } else { false };
            if wrap {
                format!("({})", t.repr())
            } else {
                t.repr()
            }
        }

        match self {
            Type::Unit => "Unit".to_string(),
            Type::Any => "Any".to_string(),
            Type::Int => "Int".to_string(),
            Type::Float => "Float".to_string(),
            Type::String => "String".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Union(options) => {
                let type_opts: Vec<String> = options.iter()
                    .map(|t| wrap_type_repr(t))
                    .collect();
                format!("{}", type_opts.join(" | "))
            }
            Type::Array(typ) => format!("{}[]", wrap_type_repr(&typ)),
            Type::Tuple(types) => {
                let types = types.iter().map(|t| t.repr()).join(", ");
                format!("({})", types)
            }
            Type::Set(typ) => {
                format!("Set<{}>", typ.repr())
            }
            Type::Map(key_type, value_type) => {
                format!("Map<{}, {}>", key_type.repr(), value_type.repr())
            }
            Type::Option(typ) => format!("{}?", wrap_type_repr(&typ)),
            Type::Fn(FnType { arg_types, ret_type, .. }) => {
                let args = arg_types.iter().map(|(_, arg_type, _)| arg_type.repr()).collect::<Vec<String>>().join(", ");
                format!("({}) => {}", args, ret_type.repr())
            }
            Type::Type(name, _, _) => name.to_string(),
            Type::Unknown => "Unknown".to_string(),
            Type::Struct(StructType { name, type_args, .. }) => {
                if type_args.is_empty() { return name.clone(); }

                let type_args_repr = type_args.iter()
                    .map(|(_, typ)| typ.repr())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{}<{}>", name, type_args_repr)
            }
            Type::Enum(EnumType { name, .. }) => format!("{}", name),
            Type::Module(_) => "Module".to_string(),
            Type::Placeholder => "_".to_string(),
            Type::Generic(name) => name.clone(),
            Type::Reference(name, type_args) => {
                if type_args.is_empty() { return name.clone(); }

                let type_args_repr = type_args.iter()
                    .map(|typ| typ.repr())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{}<{}>", name, type_args_repr)
            }
        }
    }

    pub fn flatten(types: Vec<Type>) -> Type {
        let mut all_same_ref = true;
        let mut type_ref_name = &"".to_string();
        let iter = types.iter()
            .filter_map(|t| if let Type::Reference(r, g) = t {
                if type_ref_name == "" { type_ref_name = r }
                else if r != type_ref_name { all_same_ref = false }
                Some((r, g))
            } else { all_same_ref = false; None } )
            .collect::<Vec<_>>();

        if all_same_ref && !iter.is_empty() {
            let mut iter = iter.into_iter();
            let (type_ref_name, type_ref_args) = iter.next().expect("iter is non-empty");
            let mut type_ref_args = type_ref_args.clone();
            for (_, type_args) in iter {
                if type_ref_args.len() != type_args.len() { break; }

                // Merge typeargs vecs together, overwriting placeholders with real values. If neither
                // slot has a placeholder, then this cannot be flattened, so return a Union of everything.
                for i in 0..type_ref_args.len() {
                    if type_ref_args[i] != Type::Placeholder && type_args[i] != Type::Placeholder {
                        return Type::Union(types);
                    }

                    if type_ref_args[i] == Type::Placeholder {
                        type_ref_args[i] = type_args[i].clone();
                    }
                }
            }
            Type::Reference(type_ref_name.clone(), type_ref_args)
        } else {
            Type::Union(types)
        }
    }

    pub fn unwrapped_tryable_type(&self) -> Option<&Type> {
        match self {
            Type::Reference(ref_name, type_args) if ref_name == "Result" => {
                type_args.get(0)
            }
            _ => None
        }
    }

    pub fn is_equivalent_to<'a, F>(&self, target_type: &Type, resolve_type: &'a F) -> bool
        where F: Fn(&String) -> Option<&'a Type>
    {
        use self::Type::*;

        // TODO: Test this, esp the complex cases
        match (self, target_type) {
            // Easy cases
            (Unit, Unit) | (Int, Int) | (Float, Float) |
            (String, String) | (Bool, Bool) | (Any, Any) | (Unknown, Unknown) => true,
            // For Array / Option types, compare inner type
            (Array(t1), Array(t2)) => t1.is_equivalent_to(t2, resolve_type),
            (Set(t1), Set(t2)) => t1.is_equivalent_to(t2, resolve_type),
            (Tuple(ts1), Tuple(ts2)) => {
                if ts1.len() != ts2.len() {
                    false
                } else {
                    ts1.iter().zip(ts2).all(|(t1, t2)| t1.is_equivalent_to(t2, resolve_type))
                }
            }
            // When comparing Option types, make sure to flatten, and then compare the root inner type
            // (ie. String??? == String?, but Int?? != String?)
            (Option(t1), Option(t2)) => {
                let mut t1 = t1;
                while let self::Type::Option(ref inner) = **t1 { t1 = inner }

                let mut t2 = t2;
                while let self::Type::Option(ref inner) = **t2 { t2 = inner }

                t1.is_equivalent_to(t2, resolve_type)
            }
            // A non-optional instance of a type should be assignable up to an optional version of it
            // (ie. val i: Int? = 1)
            (t1, Option(t2)) => t1.is_equivalent_to(t2, resolve_type),
            (Union(t1s), Union(t2s)) => {
                for t1 in t1s {
                    let has_match = t2s.iter()
                        .find(|t2| t1.is_equivalent_to(t2, resolve_type))
                        .is_some();
                    if !has_match {
                        return false;
                    }
                }
                true
            }
            (t1, Union(ts)) => {
                for t in ts {
                    if t1.is_equivalent_to(t, resolve_type) {
                        return true;
                    }
                }
                false
            }
            // For Fn types compare arities, param types, and return type
            (Fn(FnType { arg_types: args, ret_type: ret, .. }), Fn(FnType { arg_types: target_args, ret_type: target_ret, .. })) => {
                let num_req_args = args.iter().filter(|(_, _, has_default)| !*has_default).count();
                let num_target_req_args = target_args.iter().filter(|(_, _, has_default)| !*has_default).count();
                if num_req_args != num_target_req_args {
                    return false;
                }

                // If the provided type has non-required arguments, in addition to the arguments that match the
                // target_type, then that's ok
                for ((_, target_arg, _), (_, arg, _)) in target_args.iter().zip(args.iter()) {
                    if !arg.is_equivalent_to(target_arg, resolve_type) {
                        return false;
                    }
                }

                if **target_ret == Unit {
                    true
                } else if !ret.is_equivalent_to(target_ret, resolve_type) {
                    false
                } else {
                    true
                }
            }
            // TODO
            (Type(_name1, _t1, _is_enum1), Type(_name2, _t2, _is_enum2)) => {
                false
            }
            (Struct(StructType { name: name1, type_args: type_args1, .. }), Struct(StructType { name: name2, type_args: type_args2, .. })) |
            (Enum(EnumType { name: name1, type_args: type_args1, .. }), Enum(EnumType { name: name2, type_args: type_args2, .. })) => {
                if name1 != name2 { return false; }

                if type_args1.len() != type_args2.len() { return false; }
                for ((name1, type1), (name2, type2)) in type_args1.iter().zip(type_args2.iter()) {
                    if name1 != name2 { return false; }

                    // For `type List<T> { items: T[] }`, if we want to do `val list: List<String> = List(items: [])`,
                    // and test the equivalence of the two types, we'll be comparing List<T> (rhs) to List<String> (lhs),
                    // since the `T` on the rhs has yet to be resolved. In this case, essentially skip that type_arg.
                    if let Generic(_) = type1 { continue; }
                    if !type1.is_equivalent_to(type2, resolve_type) { return false; }
                }
                true
            }
            (Map(k_type_1, v_type_1), Map(k_type_2, v_type_2)) => {
                if !k_type_1.is_equivalent_to(&k_type_2, resolve_type) {
                    return false;
                }
                v_type_1.is_equivalent_to(&v_type_2, resolve_type)
            }
            // All types can be assignable up to Any (ie. val a: Any = 1; val b: Any = ["asdf"]). Unit types should not be assignable to anything
            (t, Any) => t != &Unit,
            (Placeholder, _) | (_, Placeholder) => true,
            (Reference(name, _type_args), other) => {
                if let Some(referenced_type) = resolve_type(name) {
                    referenced_type.is_equivalent_to(other, resolve_type)
                } else { false }
            }
            (other, Reference(name, type_args)) => {
                if let Some(referenced_type) = Self::hydrate_reference_type(name, type_args, resolve_type) {
                    other.is_equivalent_to(&referenced_type, resolve_type)
                } else { false }
            }
            (Generic(t1), Generic(t2)) => t1 == t2,
            (_, _) => false
        }
    }

    pub fn hydrate_reference_type<'a, F>(name: &String, type_args: &Vec<Type>, resolve_type: &'a F) -> Option<Type>
        where F: Fn(&String) -> Option<&'a Type>
    {
        if let Some(referenced_type) = resolve_type(name) {
            match referenced_type.clone() {
                Type::Struct(struct_type) => {
                    // Hydrate the struct's type arguments from the Reference
                    let type_args = struct_type.type_args.iter().zip(type_args.iter())
                        .map(|((name, _), typ)| (name.clone(), typ.clone()))
                        .collect();
                    Some(Type::Struct(StructType { type_args, ..struct_type }))
                }
                Type::Enum(enum_type) => {
                    // Hydrate the enum's type arguments from the Reference
                    let type_args = enum_type.type_args.iter().zip(type_args.iter())
                        .map(|((name, _), typ)| (name.clone(), typ.clone()))
                        .collect();
                    Some(Type::Enum(EnumType { type_args, ..enum_type }))
                }
                Type::Array(_) => {
                    let inner_type = type_args.iter().next().unwrap_or(&Type::Unknown).clone();
                    Some(Type::Array(Box::new(inner_type)))
                }
                Type::Set(_) => {
                    let inner_type = type_args.iter().next().unwrap_or(&Type::Unknown).clone();
                    Some(Type::Set(Box::new(inner_type)))
                }
                Type::Map(_, _) => {
                    let mut type_args = type_args.iter();
                    let key_type = type_args.next().unwrap_or(&Type::Unknown).clone();
                    let value_type = type_args.next().unwrap_or(&Type::Unknown).clone();
                    Some(Type::Map(Box::new(key_type), Box::new(value_type)))
                }
                _ => unimplemented!()
            }
        } else { None }
    }

    pub fn is_unknown<'a, F>(&self, resolve_type: &'a F) -> bool
        where F: Fn(&String) -> Option<&'a Type>
    {
        match self {
            Type::Union(type_opts) |
            Type::Tuple(type_opts) => type_opts.iter().any(|typ| typ.is_unknown(resolve_type)),
            Type::Array(inner_type) |
            Type::Set(inner_type) |
            Type::Option(inner_type) => inner_type.is_unknown(resolve_type),
            Type::Map(key_type, value_type) => key_type.is_unknown(resolve_type) || value_type.is_unknown(resolve_type),
            Type::Fn(FnType { arg_types, ret_type, .. }) => {
                let has_unknown_arg = arg_types.iter().any(|(_, typ, _)| typ.is_unknown(resolve_type));
                if has_unknown_arg {
                    return true;
                } else {
                    ret_type.is_unknown(resolve_type)
                }
            }
            Type::Reference(name, _) => {
                if let Some(referenced_type) = resolve_type(name) {
                    referenced_type.is_unknown(resolve_type)
                } else { false }
            }
            Type::Unknown => true,
            _ => false
        }
    }

    pub fn is_unit<'a, F>(&self, resolve_type: &'a F) -> bool
        where F: Fn(&String) -> Option<&'a Type>
    {
        match self {
            Type::Union(type_opts) |
            Type::Tuple(type_opts) => type_opts.iter().any(|typ| typ.is_unit(resolve_type)),
            Type::Array(inner_type) |
            Type::Set(inner_type) |
            Type::Option(inner_type) => inner_type.is_unit(resolve_type),
            Type::Reference(name, _) => {
                if let Some(referenced_type) = resolve_type(name) {
                    referenced_type.is_unit(resolve_type)
                } else { false }
            }
            Type::Unit => true,
            _ => false
        }
    }

    pub fn is_opt(&self) -> bool {
        if let Type::Option(_) = self { true } else { false }
    }

    pub fn get_opt_unwrapped(&self) -> Type {
        let mut typ = self.clone();
        while let Type::Option(inner) = typ { typ = *inner }
        typ
    }

    pub fn extract_unbound_generics(&self) -> Vec<String> {
        match self {
            Type::Generic(name) => vec![name.clone()],
            Type::Union(type_opts) |
            Type::Tuple(type_opts) => {
                type_opts.iter()
                    .flat_map(|typ| typ.extract_unbound_generics())
                    .collect()
            }
            Type::Array(inner_type) |
            Type::Set(inner_type) |
            Type::Option(inner_type) => inner_type.extract_unbound_generics(),
            Type::Map(key_type, value_type) => {
                vec![
                    key_type.extract_unbound_generics(),
                    value_type.extract_unbound_generics(),
                ].concat()
            }
            Type::Fn(FnType { arg_types, ret_type, .. }) => {
                arg_types.iter()
                    .map(|(_, typ, _)| typ)
                    .chain(std::iter::once(&**ret_type))
                    .flat_map(|typ| typ.extract_unbound_generics())
                    .collect::<Vec<String>>()
            }
            Type::Reference(_, type_args) => {
                type_args.iter()
                    .flat_map(|typ| typ.extract_unbound_generics())
                    .collect()
            }
            Type::Type(_, typ, _) => typ.extract_unbound_generics(),
            _ => vec![]
        }
    }

    pub fn has_unbound_generic(&self) -> bool {
        !self.extract_unbound_generics().is_empty()
    }

    pub fn substitute_generics(typ: &Type, available_generics: &HashMap<String, Type>) -> Type {
        match typ {
            Type::Generic(name) => available_generics.get(name).unwrap_or(typ).clone(),
            Type::Array(inner_type) => Type::Array(Box::new(Type::substitute_generics(inner_type, available_generics))),
            Type::Set(inner_type) => Type::Set(Box::new(Type::substitute_generics(inner_type, available_generics))),
            Type::Map(key_type, value_type) => {
                let key_type = Box::new(Type::substitute_generics(key_type, available_generics));
                let value_type = Box::new(Type::substitute_generics(value_type, available_generics));
                Type::Map(key_type, value_type)
            }
            Type::Option(inner_type) => Type::Option(Box::new(Type::substitute_generics(inner_type, available_generics))),
            Type::Fn(FnType { arg_types, type_args, ret_type, is_variadic, is_enum_constructor }) => {
                let arg_types = arg_types.iter()
                    .map(|(name, typ, is_optional)| {
                        (name.clone(), Type::substitute_generics(typ, available_generics), is_optional.clone())
                    })
                    .collect();
                let ret_type = Type::substitute_generics(ret_type, available_generics);
                Type::Fn(FnType { arg_types, type_args: type_args.clone(), ret_type: Box::new(ret_type), is_variadic: is_variadic.clone(), is_enum_constructor: *is_enum_constructor })
            }
            Type::Reference(name, type_args) => {
                let type_args = type_args.iter().map(|t| Type::substitute_generics(t, available_generics)).collect();
                Type::Reference(name.clone(), type_args)
            }
            Type::Type(name, typ, is_enum) => Type::Type(name.clone(), Box::new(Type::substitute_generics(typ, available_generics)), *is_enum),
            Type::Tuple(types) => {
                let types = types.iter().map(|t| Type::substitute_generics(t, available_generics)).collect();
                Type::Tuple(types)
            }
            Type::Union(types) => {
                let mut opts = Vec::new();
                let mut types_iter = types.into_iter();
                let typ = types_iter.next().expect("Union types should never be empty");
                let mut queue = vec![typ.clone()];

                while let Some(t) = queue.pop() {
                    if let Type::Union(opts) = t {
                        let mut opts = opts.into_iter().collect();
                        queue.append(&mut opts);
                    } else {
                        let t = Type::substitute_generics(&t, available_generics);
                        if let Type::Union(opts) = t {
                            let mut opts = opts.into_iter().collect();
                            queue.append(&mut opts);
                        } else {
                            opts.push(t);
                            types_iter.next().map(|t| queue.push(t.clone()));
                        }
                    }
                }
                Type::Union(opts)
            }
            t => t.clone(),
        }
    }

    pub fn try_fit_generics(typ: &Type, target_type: &Type) -> Option<Vec<(String, Type)>> {
        if !target_type.has_unbound_generic() { return None; }

        match (target_type, typ) {
            (Type::Generic(name), target_type) => Some(vec![(name.clone(), target_type.clone())]),
            (Type::Fn(FnType { arg_types: target_arg_types, ret_type: target_ret_type, .. }), Type::Fn(FnType { arg_types, ret_type, .. })) => {
                let mut pairs = Vec::new();
                let mut seen = HashSet::new();
                for ((_, target_arg_type, _), (_, arg_type, _)) in target_arg_types.iter().zip(arg_types.iter()) {
                    if let Some(items) = Self::try_fit_generics(arg_type, target_arg_type) {
                        for (type_arg_name, potential_type) in items {
                            if seen.contains(&type_arg_name) { continue; }
                            seen.insert(type_arg_name.clone());
                            pairs.push((type_arg_name, potential_type))
                        }
                    }
                }
                if let Some(items) = Self::try_fit_generics(ret_type, target_ret_type) {
                    for (type_arg_name, potential_type) in items {
                        if seen.contains(&type_arg_name) { continue; }
                        seen.insert(type_arg_name.clone());
                        pairs.push((type_arg_name, potential_type))
                    }
                }
                Some(pairs)
            }
            (Type::Option(target_inner), Type::Option(inner)) => Self::try_fit_generics(inner, target_inner),
            (Type::Array(target_inner), Type::Array(inner)) => Self::try_fit_generics(inner, target_inner),
            (Type::Set(target_inner), Type::Set(inner)) => Self::try_fit_generics(inner, target_inner),
            (Type::Tuple(target_types), Type::Tuple(types)) => {
                if target_types.len() != types.len() {
                    None
                } else {
                    let generics = target_types.iter().zip(types)
                        .map(|(target_type, typ)| Self::try_fit_generics(typ, target_type).unwrap_or(vec![]))
                        .collect::<Vec<_>>()
                        .concat();
                    if generics.is_empty() { None } else { Some(generics) }
                }
            }
            (Type::Map(target_key_type, target_value_type), Type::Map(key_type, value_type)) => {
                let key_generics = Self::try_fit_generics(key_type, target_key_type);
                let value_generics = Self::try_fit_generics(value_type, target_value_type);
                let generics = vec![
                    key_generics.unwrap_or(vec![]),
                    value_generics.unwrap_or(vec![]),
                ].concat();
                if generics.is_empty() { None } else { Some(generics) }
            }
            (Type::Union(target_opts), t) => {
                let (generics, target_opts) = target_opts.into_iter().map(Type::clone)
                    .partition::<Vec<Type>, _>(|t| if let Type::Generic(_) = t { true } else { false });

                let target_opts = target_opts.into_iter().map(|t| t.clone()).collect::<HashSet<Type>>();
                let opts = match t {
                    Type::Union(opts) => opts.clone(),
                    t => vec![t.clone()]
                }.into_iter().collect::<HashSet<Type>>();
                let possibilities = opts.difference(&target_opts).map(|t| t.clone()).collect::<Vec<_>>();
                let typ = if possibilities.is_empty() {
                    t.clone()
                } else if possibilities.len() == 1 {
                    possibilities.into_iter().next().unwrap()
                } else {
                    Type::Union(possibilities)
                };

                let pairs = generics.into_iter()
                    .map(|t| if let Type::Generic(name) = t { name } else { unreachable!() })
                    .map(|t| (t, typ.clone()))
                    .collect();

                Some(pairs)
            }
            _ => None
        }
    }

    pub fn from_type_ident(type_ident: &TypeIdentifier, types: &HashMap<String, Type>) -> Result<Type, Token> {
        match type_ident {
            TypeIdentifier::Normal { ident, type_args } => {
                let type_name = Token::get_ident_name(ident);
                if type_name == "Placeholder" { return Ok(Type::Placeholder); }

                let mut typ = types.get(&type_name).map(|t| t.clone()).ok_or(ident.clone())?;
                if let Type::Reference(_, ref mut ref_type_args) = typ {
                    *ref_type_args = type_args.as_ref().unwrap_or(&vec![]).iter()
                        .map(|type_ident| Self::from_type_ident(type_ident, types))
                        .collect::<Result<Vec<_>, _>>()?;
                }
                Ok(typ)
            }
            TypeIdentifier::Array { inner } => {
                let typ = Type::from_type_ident(inner, types)?;
                Ok(Type::Array(Box::new(typ)))
            }
            TypeIdentifier::Tuple { types: tuple_types } => {
                let types = tuple_types.iter()
                    .map(|t| Type::from_type_ident(t, types))
                    .collect::<Result<_, _>>()?;
                Ok(Type::Tuple(types))
            }
            TypeIdentifier::Option { inner } => {
                let typ = Type::from_type_ident(inner, types)?;
                Ok(Type::Option(Box::new(typ)))
            }
            TypeIdentifier::Union { left, right } => {
                let mut union_types = Vec::new();
                let left = Type::from_type_ident(left, types)?;
                union_types.push(left);

                let mut right_iter = right;
                loop {
                    if let TypeIdentifier::Union { left, right } = &**right_iter {
                        let typ = Type::from_type_ident(left, types)?;
                        union_types.push(typ);
                        right_iter = right;
                    } else {
                        let typ = Type::from_type_ident(right_iter, types)?;
                        union_types.push(typ);
                        break;
                    }
                }
                Ok(Type::Union(union_types))
            }
            TypeIdentifier::Func { args, ret } => {
                let arg_types = args.into_iter()
                    .map(|arg| Type::from_type_ident(arg, types))
                    .collect::<Result<Vec<_>, _>>();
                let arg_types = arg_types?.into_iter().map(|arg_type| ("_".to_string(), arg_type, false)).collect();
                let ret_type = Type::from_type_ident(ret, types)?;
                Ok(Type::Fn(FnType { arg_types, type_args: vec![], ret_type: Box::new(ret_type), is_variadic: false, is_enum_constructor: false }))
            }
            // TODO: Choice type ident, eg. Int | Float
        }
    }
}

#[cfg(test)]
mod test {
    use crate::typechecker::types::Type::*;
    use crate::parser::parser::{parse, ParseResult};
    use crate::lexer::lexer::tokenize;
    use crate::parser::ast::{AstNode, BindingDeclNode, ModuleId};
    use super::*;

    fn parse_type_ident_with_types<S: Into<std::string::String>>(input: S, base_types: &HashMap<std::string::String, super::Type>) -> super::Type {
        let type_ident: std::string::String = input.into();
        let val_stmt = format!("val a: {}", type_ident);
        let module_id = ModuleId::from_name("test");
        let tokens = tokenize(&module_id, &val_stmt).unwrap();
        let ParseResult { nodes, .. } = parse(module_id, tokens).unwrap();
        match nodes.first().unwrap() {
            AstNode::BindingDecl(_, BindingDeclNode { type_ann: Some(type_ann), .. }) => {
                super::Type::from_type_ident(&type_ann, base_types).unwrap()
            }
            _ => unreachable!()
        }
    }

    fn base_types() -> HashMap<std::string::String, super::Type> {
        let mut types = HashMap::new();
        types.insert("Int".to_string(), Int);
        types.insert("Float".to_string(), Float);
        types.insert("Bool".to_string(), Bool);
        types.insert("String".to_string(), String);
        types
    }

    fn parse_type_ident<S: Into<std::string::String>>(input: S) -> super::Type {
        parse_type_ident_with_types(input, &base_types())
    }

    #[test]
    fn from_type_ident_normal() {
        assert_eq!(Int, parse_type_ident("Int"));
        assert_eq!(Float, parse_type_ident("Float"));
        assert_eq!(Bool, parse_type_ident("Bool"));
        assert_eq!(String, parse_type_ident("String"));
    }

    #[test]
    fn from_type_ident_array_and_option() {
        assert_eq!(Array(Box::new(Int)), parse_type_ident("Int[]"));
        assert_eq!(Array(Box::new(Array(Box::new(Int)))), parse_type_ident("Int[][]"));
        assert_eq!(Array(Box::new(Array(Box::new(Array(Box::new(Int)))))), parse_type_ident("Int[][][]"));

        assert_eq!(Option(Box::new(Int)), parse_type_ident("Int?"));
        assert_eq!(Option(Box::new(Option(Box::new(Int)))), parse_type_ident("Int??")); // <- I don't know why you'd want this type, but it works

        assert_eq!(Array(Box::new(Option(Box::new(Int)))), parse_type_ident("Int?[]"));
        assert_eq!(Option(Box::new(Array(Box::new(Int)))), parse_type_ident("Int[]?"));
    }

    #[test]
    fn from_type_ident_unions() {
        let expected = Union(vec![Int, Float]);
        assert_eq!(expected, parse_type_ident("Int | Float"));

        let expected = Union(vec![String, Int, Float]);
        assert_eq!(expected, parse_type_ident("String | Int | Float"));

        let expected = Union(vec![Option(Box::new(String)), Option(Box::new(Int)), Float]);
        assert_eq!(expected, parse_type_ident("String? | Int? | Float"));

        let expected = Fn(FnType {
            type_args: vec![],
            arg_types: vec![
                ("_".to_string(), Union(vec![Int, Float]), false)
            ],
            ret_type: Box::new(Union(vec![Int, Float])),
            is_variadic: false,
            is_enum_constructor: false,
        });
        assert_eq!(expected, parse_type_ident("(Int | Float) => Int | Float"));

        let expected = Option(Box::new(Union(vec![Int, Float])));
        assert_eq!(expected, parse_type_ident("(Int | Float)?"));
    }

    #[test]
    fn from_type_ident_parameterized_type() {
        let types = {
            let mut base_types = base_types();
            base_types.insert("List".to_string(), Reference("List".to_string(), vec![]));
            base_types
        };
        let parsed_type = parse_type_ident_with_types("List<Int>", &types);
        assert_eq!(Reference("List".to_string(), vec![Int]), parsed_type);

        let parsed_type = parse_type_ident_with_types("List<List<Int>>", &types);
        assert_eq!(Reference("List".to_string(), vec![Reference("List".to_string(), vec![Int])]), parsed_type);
    }

    #[test]
    fn is_equivalent_to_any() {
        assert_eq!(false, Any.is_equivalent_to(&Bool, &|_| None));
        assert_eq!(true, Bool.is_equivalent_to(&Any, &|_| None));

        assert_eq!(true, Array(Box::new(Int)).is_equivalent_to(&Array(Box::new(Any)), &|_| None));
        assert_eq!(true, Array(Box::new(Array(Box::new(Int)))).is_equivalent_to(&Array(Box::new(Any)), &|_| None));
    }

    #[test]
    fn is_equivalent_to_flattening_optional() {
        let t1 = Option(Box::new(Int));
        let t2 = Option(Box::new(Option(Box::new(Int))));
        assert_eq!(true, t1.is_equivalent_to(&t2, &|_| None));

        let t1 = Option(Box::new(Option(Box::new(Int))));
        let t2 = Option(Box::new(Option(Box::new(Option(Box::new(Int))))));
        assert_eq!(true, t1.is_equivalent_to(&t2, &|_| None));
    }

    #[test]
    fn is_equivalent_to_assigning_up_to_option() {
        let t1 = Int;
        let t2 = Option(Box::new(Int));
        assert_eq!(true, t1.is_equivalent_to(&t2, &|_| None));

        let t1 = Int;
        let t2 = Option(Box::new(Option(Box::new(Int))));
        assert_eq!(true, t1.is_equivalent_to(&t2, &|_| None));

        let t1 = Int;
        let t2 = Option(Box::new(Int));
        assert_eq!(false, t2.is_equivalent_to(&t1, &|_| None));
    }

    #[test]
    fn is_equivalent_to_parameterized_structs() {
        let struct_type = StructType {
            name: "List".to_string(),
            type_args: vec![("T".to_string(), Generic("T".to_string()))],
            constructable: true,
            fields: vec![],
            static_fields: vec![],
            methods: vec![],
        };
        let referencable_types = {
            let mut t = HashMap::new();
            t.insert("List".to_string(), Struct(struct_type.clone()));
            t
        };

        // val l: List<Int> = List(items: [1, 2])
        let rhs = Struct(StructType {
            type_args: vec![("T".to_string(), Int)],
            ..struct_type.clone()
        });
        let lhs = Reference("List".to_string(), vec![Int]);
        assert_eq!(true, rhs.is_equivalent_to(&lhs, &|type_name| { referencable_types.get(type_name) }));

        // val l: List<Int> = List(items: [])
        let rhs = Struct(StructType {
            type_args: vec![("T".to_string(), Generic("T".to_string()))],
            ..struct_type.clone()
        });
        let lhs = Reference("List".to_string(), vec![Int]);
        assert_eq!(true, rhs.is_equivalent_to(&lhs, &|type_name| { referencable_types.get(type_name) }));

        // val l: List<String> = List(items: [1, 2])
        let rhs = Struct(StructType {
            type_args: vec![("T".to_string(), Int)],
            ..struct_type
        });
        let lhs = Reference("List".to_string(), vec![String]);
        assert_eq!(false, rhs.is_equivalent_to(&lhs, &|type_name| { referencable_types.get(type_name) }));
    }

    #[test]
    fn is_equivalent_to_unions() {
        // Int | String  vs  String | Int  => true
        let t1 = Union(vec![Int, String]);
        let t2 = Union(vec![String, Int]);
        assert_eq!(true, t1.is_equivalent_to(&t2, &|_| None));

        // Int | String | (Int | String)[]  vs  String | Int | (String | Int)[]  => true
        let t1 = Union(vec![Int, String, Array(Box::new(Union(vec![Int, String])))]);
        let t2 = Union(vec![String, Int, Array(Box::new(Union(vec![String, Int])))]);
        assert_eq!(true, t1.is_equivalent_to(&t2, &|_| None));

        // String? | Int  vs  Int | String?  => true
        let t1 = Union(vec![Option(Box::new(String)), Int]);
        let t2 = Union(vec![Int, Option(Box::new(String))]);
        assert_eq!(true, t1.is_equivalent_to(&t2, &|_| None));

        // String | Int  vs  Int | Bool  => false
        let t1 = Union(vec![String, Int]);
        let t2 = Union(vec![Int, Bool]);
        assert_eq!(false, t1.is_equivalent_to(&t2, &|_| None));

        // String? | Int  vs  Int? | String  => false
        let t1 = Union(vec![Option(Box::new(String)), Int]);
        let t2 = Union(vec![Option(Box::new(Int)), String]);
        assert_eq!(false, t1.is_equivalent_to(&t2, &|_| None));
    }

    #[test]
    fn try_fit_generics_tests() {
        // Given: String[] vs Int[]; Expect: { }
        let t = Array(Box::new(String));
        let target = Array(Box::new(Int));
        let result = super::Type::try_fit_generics(&t, &target);
        assert_eq!(None, result);

        // Given: T[] & Int[]; Expect: { T: Int }
        let t = Array(Box::new(Int));
        let target = Array(Box::new(Generic("T".to_string())));
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![("T".to_string(), Int)];
        assert_eq!(Some(expected), result);

        // Given: T[][] & Int[][]; Expect: { T: Int }
        let t = Array(Box::new(Array(Box::new(Int))));
        let target = Array(Box::new(Array(Box::new(Generic("T".to_string())))));
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![("T".to_string(), Int)];
        assert_eq!(Some(expected), result);

        // Given: T? & String?; Expect: { T: String }
        let t = Option(Box::new(String));
        let target = Option(Box::new(Generic("T".to_string())));
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![("T".to_string(), String)];
        assert_eq!(Some(expected), result);

        // Given: T? & String??; Expect: { T: String? }
        let t = Option(Box::new(Option(Box::new(String))));
        let target = Option(Box::new(Generic("T".to_string())));
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![("T".to_string(), Option(Box::new(String)))];
        assert_eq!(Some(expected), result);

        // Given: T?[] & Int?[]; Expect: { T: Int }
        let t = Array(Box::new(Option(Box::new(Int))));
        let target = Array(Box::new(Option(Box::new(Generic("T".to_string())))));
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![("T".to_string(), Int)];
        assert_eq!(Some(expected), result);

        // Given: (T) => Int & (String) => Int; Expect: { T: String }
        let t = Fn(FnType { arg_types: vec![("_".to_string(), String, false)], type_args: vec![], ret_type: Box::new(Int), is_variadic: false, is_enum_constructor: false });
        let target = Fn(FnType { arg_types: vec![("_".to_string(), Generic("T".to_string()), false)], type_args: vec![], ret_type: Box::new(Int), is_variadic: false, is_enum_constructor: false });
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![("T".to_string(), String)];
        assert_eq!(Some(expected), result);

        // Given: (T[], Int) => Int & (Int[][], Int) => Int; Expect: { T: Int[] }
        let t = Fn(FnType {
            arg_types: vec![("_".to_string(), Array(Box::new(Array(Box::new(Int)))), false), ("_".to_string(), Int, false)],
            type_args: vec![],
            ret_type: Box::new(Int),
            is_variadic: false,
            is_enum_constructor: false,
        });
        let target = Fn(FnType {
            arg_types: vec![("_".to_string(), Array(Box::new(Generic("T".to_string()))), false), ("_".to_string(), Int, false)],
            type_args: vec![],
            ret_type: Box::new(Int),
            is_variadic: false,
            is_enum_constructor: false,
        });
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![("T".to_string(), Array(Box::new(Int)))];
        assert_eq!(Some(expected), result);

        // Given: (T, U) => Int & (Int[], String) => Int; Expect: { T: Int[], U: String }
        let t = Fn(FnType {
            arg_types: vec![("_".to_string(), Array(Box::new(Int)), false), ("_".to_string(), String, false)],
            type_args: vec![],
            ret_type: Box::new(Int),
            is_variadic: false,
            is_enum_constructor: false,
        });
        let target = Fn(FnType {
            arg_types: vec![("_".to_string(), Generic("T".to_string()), false), ("_".to_string(), Generic("U".to_string()), false)],
            type_args: vec![],
            ret_type: Box::new(Int),
            is_variadic: false,
            is_enum_constructor: false,
        });
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![
            ("T".to_string(), Array(Box::new(Int))),
            ("U".to_string(), String),
        ];
        assert_eq!(Some(expected), result);

        // Given: (T, T) => Int & (Int, String) => Int; Expect: { T: Int }
        let t = Fn(FnType {
            arg_types: vec![("_".to_string(), Int, false), ("_".to_string(), String, false)],
            type_args: vec![],
            ret_type: Box::new(Int),
            is_variadic: false,
            is_enum_constructor: false,
        });
        let target = Fn(FnType {
            arg_types: vec![("_".to_string(), Generic("T".to_string()), false), ("_".to_string(), Generic("T".to_string()), false)],
            type_args: vec![],
            ret_type: Box::new(Int),
            is_variadic: false,
            is_enum_constructor: false,
        });
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![("T".to_string(), Int)];
        assert_eq!(Some(expected), result);

        // Given: (T) => U & (Int) => Int; Expect: { T: Int, U: Int }
        let t = Fn(FnType {
            arg_types: vec![("_".to_string(), Int, false)],
            type_args: vec![],
            ret_type: Box::new(Int),
            is_variadic: false,
            is_enum_constructor: false,
        });
        let target = Fn(FnType {
            arg_types: vec![("_".to_string(), Generic("T".to_string()), false)],
            type_args: vec![],
            ret_type: Box::new(Generic("U".to_string())),
            is_variadic: false,
            is_enum_constructor: false,
        });
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![
            ("T".to_string(), Int),
            ("U".to_string(), Int),
        ];
        assert_eq!(Some(expected), result);

        // Given: ((T) => U) => U & ((Int) => String) => String; Expect: { T: Int, U: String }
        let t = Fn(FnType {
            arg_types: vec![
                ("_".to_string(), Fn(FnType {
                    arg_types: vec![("_".to_string(), Int, false)],
                    type_args: vec![],
                    ret_type: Box::new(String),
                    is_variadic: false,
                    is_enum_constructor: false,
                }),
                 false
                )],
            type_args: vec![],
            ret_type: Box::new(String),
            is_variadic: false,
            is_enum_constructor: false,
        });
        let target = Fn(FnType {
            arg_types: vec![
                ("_".to_string(),
                 Fn(FnType {
                     arg_types: vec![("_".to_string(), Generic("T".to_string()), false)],
                     type_args: vec![],
                     ret_type: Box::new(Generic("U".to_string())),
                     is_variadic: false,
                     is_enum_constructor: false,
                 }),
                 false)
            ],
            type_args: vec![],
            ret_type: Box::new(Generic("U".to_string())),
            is_variadic: false,
            is_enum_constructor: false,
        });
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![
            ("T".to_string(), Int),
            ("U".to_string(), String),
        ];
        assert_eq!(Some(expected), result);

        #[inline]
        fn assert_union_eq(expected: Vec<super::Type>, actual: super::Type) {
            // Pretty annoying: it's not deterministic which order the union type's types will be in - compare ignoring order
            if let Union(opts) = actual {
                let opts = opts.into_iter().collect::<HashSet<_>>();
                let expected = expected.into_iter().collect::<HashSet<_>>();
                assert_eq!(expected, opts);
            } else { assert!(false, "The type should be a Union") }
        }

        // Given: T | Int & Int | Float; Expect: { T: Float }
        let t = Union(vec![Int, Float]);
        let target = Union(vec![Generic("T".to_string()), Int]);
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![
            ("T".to_string(), Float),
        ];
        assert_eq!(Some(expected), result);

        // Given: T | Float | Int & Int | String | Float; Expect: { T: String }
        let t = Union(vec![Int, String, Float]);
        let target = Union(vec![Generic("T".to_string()), Float, Int]);
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![
            ("T".to_string(), String),
        ];
        assert_eq!(Some(expected), result);

        // Given: T | Int & Int | String | Float; Expect: { T: String | Float }
        let t = Union(vec![Int, String, Float]);
        let target = Union(vec![Generic("T".to_string()), Int]);
        let result = super::Type::try_fit_generics(&t, &target);
        let (name, typ) = result.unwrap().into_iter().next().unwrap();
        assert_eq!("T".to_string(), name);
        assert_union_eq(vec![String, Float], typ);

        // Given: T | U | Int & Int | String | Float; Expect: { T: String | Float, U: String | Float }
        let t = Union(vec![Int, String, Float]);
        let target = Union(vec![Generic("T".to_string()), Generic("U".to_string()), Int]);
        let result = super::Type::try_fit_generics(&t, &target);
        let mut results = result.unwrap().into_iter();
        let (name, typ) = results.next().unwrap();
        assert_eq!("T".to_string(), name);
        assert_union_eq(vec![String, Float], typ);
        let (name, typ) = results.next().unwrap();
        assert_eq!("U".to_string(), name);
        assert_union_eq(vec![String, Float], typ);

        // Given: T | Float | Int & Int | Float; Expect: { T: Int | Float }
        let t = Union(vec![Int, Float]);
        let target = Union(vec![Generic("T".to_string()), Int, Float]);
        let result = super::Type::try_fit_generics(&t, &target);
        let (name, typ) = result.unwrap().into_iter().next().unwrap();
        assert_eq!("T".to_string(), name);
        assert_union_eq(vec![Int, Float], typ);
    }

    #[test]
    fn flatten_tests() {
        let types = vec![
            Reference("Result".to_string(), vec![Placeholder, Int]),
            Reference("Result".to_string(), vec![Int, Placeholder]),
        ];
        let res = crate::typechecker::types::Type::flatten(types);
        let expected = Reference("Result".to_string(), vec![Int, Int]);
        assert_eq!(expected, res);

        let types = vec![
            Reference("Result".to_string(), vec![Placeholder, Int]),
            Reference("Result".to_string(), vec![Int, Placeholder]),
            Reference("Result".to_string(), vec![Int, String]),
        ];
        let res = crate::typechecker::types::Type::flatten(types);
        let expected = Union(vec![
            Reference("Result".to_string(), vec![Placeholder, Int]),
            Reference("Result".to_string(), vec![Int, Placeholder]),
            Reference("Result".to_string(), vec![Int, String]),
        ]);
        assert_eq!(expected, res);
    }
}
