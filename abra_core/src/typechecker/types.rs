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
    Fn(/* arg_types: */ Vec<(/* arg_name: */ String, /* arg_type: */ Type, /* is_optional: */ bool)>, /* ret_type: */ Box<Type>),
    Type(/* type_name: */ String, /* underlying_type: */ Box<Type>),
    Struct(StructType),
    // Acts as a sentinel value, right now only for when a function is referenced recursively without an explicit return type
    Unknown,
    Placeholder,
    Reference(/* name: */ String),
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
            (Array(t1), Array(t2)) => self::Type::is_equivalent_to(t1, t2, referencable_types),
            // When comparing Option types, make sure to flatten, and then compare the root inner type
            // (ie. String??? == String?, but Int?? != String?)
            (Option(t1), Option(t2)) => {
                let mut t1 = t1;
                while let self::Type::Option(ref inner) = **t1 { t1 = inner }

                let mut t2 = t2;
                while let self::Type::Option(ref inner) = **t2 { t2 = inner }

                self::Type::is_equivalent_to(t1, t2, referencable_types)
            }
            // A non-optional instance of a type should be assignable up to an optional version of it
            // (ie. val i: Int? = 1)
            (t1, Option(t2)) => self::Type::is_equivalent_to(t1, t2, referencable_types),
            (Or(t1s), Or(t2s)) => {
                let t1s = HashSet::<self::Type>::from_iter(t1s.clone().into_iter());
                let t2s = HashSet::<self::Type>::from_iter(t2s.clone().into_iter());
                for (t1, t2) in t1s.iter().zip(t2s.iter()) {
                    if !self::Type::is_equivalent_to(t1, t2, referencable_types) {
                        return false;
                    }
                }
                true
            }
            // For Fn types compare arities, param types, and return type
            (Fn(args1, ret1), Fn(args2, ret2)) => {
                if args1.len() != args2.len() {
                    return false;
                }

                // TODO: Factor in optional params here
                // func abc(a: Int, b = 3) = a + b should satisfy a type of (Int, Int) => Int and also (Int) => Int
                for ((_, t1, _), (_, t2, _)) in args1.iter().zip(args2.iter()) {
                    if !self::Type::is_equivalent_to(t1, t2, referencable_types) {
                        return false;
                    }
                }
                if !self::Type::is_equivalent_to(ret1, ret2, referencable_types) {
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
            (_, _) => false
        }
    }

    pub fn is_unknown(&self, referencable_types: &HashMap<String, Type>) -> bool {
        match self {
            Type::Or(type_opts) => type_opts.iter().any(|typ| typ.is_unknown(referencable_types)),
            Type::Array(inner_type) |
            Type::Option(inner_type) => inner_type.is_unknown(referencable_types),
            Type::Fn(arg_types, ret_type) => {
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

    pub fn from_type_ident(type_ident: &TypeIdentifier, types: &HashMap<String, Type>) -> Option<Type> {
        match type_ident {
            TypeIdentifier::Normal { ident } => {
                let type_name = Token::get_ident_name(ident);
                types.get(&type_name).map(|t| t.clone())
            }
            TypeIdentifier::Array { inner } => {
                let typ = Type::from_type_ident(inner, types)?;
                Some(Type::Array(Box::new(typ)))
            }
            TypeIdentifier::Option { inner } => {
                let typ = Type::from_type_ident(inner, types)?;
                Some(Type::Option(Box::new(typ)))
            }
            // TODO: Function type ident, eg. (Int, Bool) => String
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
}
