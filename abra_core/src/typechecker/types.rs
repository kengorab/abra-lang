use crate::parser::ast::TypeIdentifier;
use crate::lexer::tokens::Token;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Unit,
    Any,
    Or(Vec<Type>),
    Int,
    Float,
    String,
    Bool,
    Array(Box<Type>),
    Map(/* fields: */ Vec<(String, Type)>, /* homogeneous_type: */ Option<Box<Type>>),
    Option(Box<Type>),
    // Fn(/* arg_types: */ Vec<(/* arg_name: */ String, /* arg_type: */ Type, /* is_optional: */ bool)>, /* ret_type: */ Box<Type>),
    Fn(FnType),
    Type(/* type_name: */ String, /* underlying_type: */ Box<Type>),
    Struct(StructType),
    // Acts as a sentinel value, right now only for when a function is referenced recursively without an explicit return type
    Unknown,
    Placeholder,
    Generic(/* name: */ String),
    Reference(/* name: */ String),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FnType {
    pub arg_types: Vec<(/* arg_name: */ String, /* arg_type: */ Type, /* is_optional: */ bool)>,
    pub type_args: Vec<String>,
    pub ret_type: Box<Type>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(/* name: */ String, /* type: */ Type, /* has_default_value: */ bool)>,
    pub static_fields: Vec<(/* name: */ String, /* type: */ Type, /* has_default_value: */ bool)>,
    pub methods: Vec<(String, Type)>,
}

impl Type {
    pub fn is_equivalent_to(&self, target_type: &Type, referencable_types: &HashMap<String, Type>) -> bool {
        use self::Type::*;

        // TODO: Test this, esp the complex cases
        match (self, target_type) {
            // Easy cases
            (Unit, Unit) | (Int, Int) | (Float, Float) |
            (String, String) | (Bool, Bool) | (Any, Any) => true,
            // For Array / Option types, compare inner type
            (Array(t1), Array(t2)) => t1.is_equivalent_to(t2, referencable_types),
            // When comparing Option types, make sure to flatten, and then compare the root inner type
            // (ie. String??? == String?, but Int?? != String?)
            (Option(t1), Option(t2)) => {
                let mut t1 = t1;
                while let self::Type::Option(ref inner) = **t1 { t1 = inner }

                let mut t2 = t2;
                while let self::Type::Option(ref inner) = **t2 { t2 = inner }

                t1.is_equivalent_to(t2, referencable_types)
            }
            // A non-optional instance of a type should be assignable up to an optional version of it
            // (ie. val i: Int? = 1)
            (t1, Option(t2)) => t1.is_equivalent_to(t2, referencable_types),
            (Or(t1s), Or(t2s)) => {
                let t1s = HashSet::<self::Type>::from_iter(t1s.clone().into_iter());
                let t2s = HashSet::<self::Type>::from_iter(t2s.clone().into_iter());
                for (t1, t2) in t1s.iter().zip(t2s.iter()) {
                    if !t1.is_equivalent_to(t2, referencable_types) {
                        return false;
                    }
                }
                true
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
                    if !arg.is_equivalent_to(target_arg, referencable_types) {
                        return false;
                    }
                }
                if !ret.is_equivalent_to(target_ret, referencable_types) {
                    return false;
                }
                true
            }
            // TODO
            (Type(_name1, _t1), Type(_name2, _t2)) => {
                false
            }
            // TODO (improve this (obviously))
            (Struct(StructType { name: name1, .. }), Struct(StructType { name: name2, .. })) => {
                name1 == name2
            }
            // TODO (This should be unreachable right now anwyay...)
            (Map(_fields1, _), Map(_fields2, _)) => {
                false
            }
            // TODO
            (Struct(StructType { name: _name, .. }), Map(_fields2, _)) => {
                false
            }
            // TODO
            (Map(provided_fields, _), Struct(StructType { fields: required_fields, .. })) => {
                let provided_fields = provided_fields.iter()
                    .map(|(name, typ)| (name.clone(), typ.clone()))
                    .collect::<HashMap<_, _>>();
                for (req_name, req_type, has_default_value) in required_fields {
                    if *has_default_value { continue; }

                    match provided_fields.get(req_name) {
                        None => return false,
                        Some(provided_type) => {
                            if !provided_type.is_equivalent_to(req_type, referencable_types) {
                                return false;
                            } else {
                                continue;
                            }
                        }
                    }
                }
                true
            }
            // All types can be assignable up to Any (ie. val a: Any = 1; val b: Any = ["asdf"])
            (_, Any) => true,
            (Placeholder, _) | (_, Placeholder) => true,
            (Reference(name), other) => {
                if let Some(referenced_type) = referencable_types.get(name) {
                    referenced_type.is_equivalent_to(other, referencable_types)
                } else { false }
            }
            (other, Reference(name)) => {
                if let Some(referenced_type) = referencable_types.get(name) {
                    other.is_equivalent_to(&referenced_type, referencable_types)
                } else { false }
            }
            (Generic(t1), Generic(t2)) => t1 == t2,
            (_, _) => false
        }
    }

    pub fn is_unknown(&self, referencable_types: &HashMap<String, Type>) -> bool {
        match self {
            Type::Or(type_opts) => type_opts.iter().any(|typ| typ.is_unknown(referencable_types)),
            Type::Array(inner_type) |
            Type::Option(inner_type) => inner_type.is_unknown(referencable_types),
            Type::Fn(FnType { arg_types, ret_type, .. }) => {
                let has_unknown_arg = arg_types.iter().any(|(_, typ, _)| typ.is_unknown(referencable_types));
                if has_unknown_arg {
                    return true;
                } else {
                    ret_type.is_unknown(referencable_types)
                }
            }
            Type::Reference(name) => {
                if let Some(referenced_type) = referencable_types.get(name) {
                    referenced_type.is_unknown(referencable_types)
                } else { false }
            }
            Type::Unknown => true,
            _ => false
        }
    }

    pub fn extract_generics(&self) -> Vec<String> {
        match self {
            Type::Generic(name) => vec![name.clone()],
            Type::Or(type_opts) => {
                type_opts.iter()
                    .flat_map(|typ| typ.extract_generics().into_iter())
                    .collect::<Vec<String>>()
            },
            Type::Array(inner_type) |
            Type::Option(inner_type) => inner_type.extract_generics(),
            Type::Fn(FnType { arg_types, ret_type, .. }) => {
                arg_types.iter()
                    .map(|(_, typ, _)| typ)
                    .chain(std::iter::once(&**ret_type))
                    .flat_map(|typ| typ.extract_generics().into_iter())
                    .collect::<Vec<String>>()
            }
            _ => vec![]
        }
    }

    pub fn is_generic(&self) -> bool {
        !self.extract_generics().is_empty()
    }

    pub fn substitute_generics(typ: &Type, available_generics: &HashMap<String, Type>) -> Type {
        match typ {
            Type::Generic(name) => available_generics.get(name).unwrap_or(typ).clone(),
            Type::Array(inner_type) => Type::Array(Box::new(Type::substitute_generics(inner_type, available_generics))),
            Type::Option(inner_type) => Type::Option(Box::new(Type::substitute_generics(inner_type, available_generics))),
            Type::Fn(FnType { arg_types, type_args, ret_type }) => {
                let arg_types = arg_types.iter()
                    .map(|(name, typ, is_optional)| {
                        (name.clone(), Type::substitute_generics(typ, available_generics), is_optional.clone())
                    })
                    .collect();
                let ret_type = Type::substitute_generics(ret_type, available_generics);
                Type::Fn(FnType { arg_types, type_args: type_args.clone(), ret_type: Box::new(ret_type) })
            }
            // Type::Struct(_) => {},
            // Type::Or(_) => {},
            // Type::Map(_, _) => {},
            t @ _ => t.clone(),
        }
    }

    pub fn try_fit_generics(typ: &Type, target_type: &Type) -> Option<Vec<(String, Type)>> {
        if !target_type.is_generic() { return None; }

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
            _ => None
        }
    }

    pub fn from_type_ident(type_ident: &TypeIdentifier, types: &HashMap<String, Type>) -> Result<Type, Token> {
        match type_ident {
            TypeIdentifier::Normal { ident } => {
                let type_name = Token::get_ident_name(ident);
                types.get(&type_name).map(|t| t.clone()).ok_or(ident.clone())
            }
            TypeIdentifier::Array { inner } => {
                let typ = Type::from_type_ident(inner, types)?;
                Ok(Type::Array(Box::new(typ)))
            }
            TypeIdentifier::Option { inner } => {
                let typ = Type::from_type_ident(inner, types)?;
                Ok(Type::Option(Box::new(typ)))
            }
            TypeIdentifier::Func { args, ret } => {
                let arg_types = args.into_iter()
                    .map(|arg| Type::from_type_ident(arg, types))
                    .collect::<Result<Vec<_>, _>>();
                let arg_types = arg_types?.into_iter().map(|arg_type| ("_".to_string(), arg_type, false)).collect();
                let ret_type = Type::from_type_ident(ret, types)?;
                Ok(Type::Fn(FnType { arg_types, type_args: vec![], ret_type: Box::new(ret_type) }))
            }
            // TODO: Choice type ident, eg. Int | Float
        }
    }
}

#[cfg(test)]
mod test {
    use crate::typechecker::types::Type::*;
    use crate::parser::parser::parse;
    use crate::lexer::lexer::tokenize;
    use crate::parser::ast::{AstNode, BindingDeclNode};
    use super::*;

    fn parse_type_ident<S: Into<std::string::String>>(input: S) -> super::Type {
        let base_types: HashMap<std::string::String, super::Type> = {
            let mut types = HashMap::new();
            types.insert("Int".to_string(), Int);
            types.insert("Float".to_string(), Float);
            types.insert("Bool".to_string(), Bool);
            types.insert("String".to_string(), String);
            types
        };

        let type_ident: std::string::String = input.into();
        let val_stmt = format!("val a: {}", type_ident);
        let tokens = tokenize(&val_stmt).unwrap();
        let ast = parse(tokens).unwrap();
        match ast.first().unwrap() {
            AstNode::BindingDecl(_, BindingDeclNode { type_ann: Some(type_ann), .. }) => {
                super::Type::from_type_ident(&type_ann, &base_types).unwrap()
            }
            _ => unreachable!()
        }
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
    fn is_equivalent_to_any() {
        let referencable_types = HashMap::new();

        assert_eq!(false, Any.is_equivalent_to(&Bool, &referencable_types));
        assert_eq!(true, Bool.is_equivalent_to(&Any, &referencable_types));

        assert_eq!(true, Array(Box::new(Int)).is_equivalent_to(&Array(Box::new(Any)), &referencable_types));
        assert_eq!(true, Array(Box::new(Array(Box::new(Int)))).is_equivalent_to(&Array(Box::new(Any)), &referencable_types));
    }

    #[test]
    fn is_equivalent_to_flattening_optional() {
        let referencable_types = HashMap::new();

        let t1 = Option(Box::new(Int));
        let t2 = Option(Box::new(Option(Box::new(Int))));
        assert_eq!(true, t1.is_equivalent_to(&t2, &referencable_types));

        let t1 = Option(Box::new(Option(Box::new(Int))));
        let t2 = Option(Box::new(Option(Box::new(Option(Box::new(Int))))));
        assert_eq!(true, t1.is_equivalent_to(&t2, &referencable_types));
    }

    #[test]
    fn is_equivalent_to_assigning_up_to_option() {
        let referencable_types = HashMap::new();

        let t1 = Int;
        let t2 = Option(Box::new(Int));
        assert_eq!(true, t1.is_equivalent_to(&t2, &referencable_types));

        let t1 = Int;
        let t2 = Option(Box::new(Option(Box::new(Int))));
        assert_eq!(true, t1.is_equivalent_to(&t2, &referencable_types));

        let t1 = Int;
        let t2 = Option(Box::new(Int));
        assert_eq!(false, t2.is_equivalent_to(&t1, &referencable_types));
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
        let t = Fn(FnType { arg_types: vec![("_".to_string(), String, false)], type_args: vec![], ret_type: Box::new(Int) });
        let target = Fn(FnType { arg_types: vec![("_".to_string(), Generic("T".to_string()), false)], type_args: vec![], ret_type: Box::new(Int) });
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![("T".to_string(), String)];
        assert_eq!(Some(expected), result);

        // Given: (T[], Int) => Int & (Int[][], Int) => Int; Expect: { T: Int[] }
        let t = Fn(FnType {
            arg_types: vec![("_".to_string(), Array(Box::new(Array(Box::new(Int)))), false), ("_".to_string(), Int, false)],
            type_args: vec![],
            ret_type: Box::new(Int),
        });
        let target = Fn(FnType {
            arg_types: vec![("_".to_string(), Array(Box::new(Generic("T".to_string()))), false), ("_".to_string(), Int, false)],
            type_args: vec![],
            ret_type: Box::new(Int),
        });
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![("T".to_string(), Array(Box::new(Int)))];
        assert_eq!(Some(expected), result);

        // Given: (T, U) => Int & (Int[], String) => Int; Expect: { T: Int[], U: String }
        let t = Fn(FnType {
            arg_types: vec![("_".to_string(), Array(Box::new(Int)), false), ("_".to_string(), String, false)],
            type_args: vec![],
            ret_type: Box::new(Int),
        });
        let target = Fn(FnType {
            arg_types: vec![("_".to_string(), Generic("T".to_string()), false), ("_".to_string(), Generic("U".to_string()), false)],
            type_args: vec![],
            ret_type: Box::new(Int),
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
        });
        let target = Fn(FnType {
            arg_types: vec![("_".to_string(), Generic("T".to_string()), false), ("_".to_string(), Generic("T".to_string()), false)],
            type_args: vec![],
            ret_type: Box::new(Int),
        });
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![("T".to_string(), Int)];
        assert_eq!(Some(expected), result);

        // Given: (T) => U & (Int) => Int; Expect: { T: Int, U: Int }
        let t = Fn(FnType {
            arg_types: vec![("_".to_string(), Int, false)],
            type_args: vec![],
            ret_type: Box::new(Int),
        });
        let target = Fn(FnType {
            arg_types: vec![("_".to_string(), Generic("T".to_string()), false)],
            type_args: vec![],
            ret_type: Box::new(Generic("U".to_string())),
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
                }),
                 false
                )],
            type_args: vec![],
            ret_type: Box::new(String),
        });
        let target = Fn(FnType {
            arg_types: vec![
                ("_".to_string(),
                 Fn(FnType {
                     arg_types: vec![("_".to_string(), Generic("T".to_string()), false)],
                     type_args: vec![],
                     ret_type: Box::new(Generic("U".to_string())),
                 }),
                 false)
            ],
            type_args: vec![],
            ret_type: Box::new(Generic("U".to_string())),
        });
        let result = super::Type::try_fit_generics(&t, &target);
        let expected = vec![
            ("T".to_string(), Int),
            ("U".to_string(), String),
        ];
        assert_eq!(Some(expected), result);
    }
}
