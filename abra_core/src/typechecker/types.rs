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
    Option(Box<Type>),
    Fn(Vec<(String, Type)>, Box<Type>),
}

impl Type {
    pub fn is_equivalent_to(&self, target_type: &Type) -> bool {
        use Type::*;

        // TODO: Test this, esp the complex cases
        match (self, target_type) {
            // Easy cases
            (Unit, Unit) | (Int, Int) | (Float, Float) |
            (String, String) | (Bool, Bool) | (Any, Any) => true,
            // For Array / Option types, compare inner type
            (Array(t1), Array(t2)) |
            (Option(t1), Option(t2)) => Type::is_equivalent_to(t1, t2),
            (Or(t1s), Or(t2s)) => {
                let t1s = HashSet::<Type>::from_iter(t1s.clone().into_iter());
                let t2s = HashSet::<Type>::from_iter(t2s.clone().into_iter());
                for (t1, t2) in t1s.iter().zip(t2s.iter()) {
                    if !Type::is_equivalent_to(t1, t2) {
                        return false;
                    }
                }
                true
            }
            // For Fn types compare arities, param types, and return type
            (Fn(args1, ret1), Fn(args2, ret2)) => {
                for ((_, t1), (_, t2)) in args1.iter().zip(args2.iter()) {
                    if !Type::is_equivalent_to(t1, t2) {
                        return false;
                    }
                }
                if !Type::is_equivalent_to(ret1, ret2) {
                    return false;
                }
                true
            }
            (_, _) => false
        }
    }

    pub fn from_type_ident(type_ident: &TypeIdentifier, types: HashMap<String, Type>) -> Option<Type> {
        match type_ident {
            TypeIdentifier::Normal { ident } => {
                let type_name = Token::get_ident_name(ident);
                types.get(type_name).map(|t| t.clone())
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
