use crate::typechecker::typed_ast::{TypedAstNode, TypedInvocationNode, TypedLambdaNode};
use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;
use std::sync::atomic::{AtomicUsize, Ordering};

pub static ANON_IDX: AtomicUsize = AtomicUsize::new(0);

pub fn get_anon_name() -> String {
    format!("$anon_{}", ANON_IDX.fetch_add(1, Ordering::Relaxed))
}

// An IIFE (immediately-invoked function expression) denotes a block of code which is meant to run
// in its own isolated scope, without polluting the outer scope. This is especially useful/needed
// for if-expressions and expressions which compile down to if-expressions (opt-safe accessors and
// coalesce operations).
// This version of the `wrap_in_iife` is meant to be called from the compiler, and passes a bogus
// values for `typ` (since post-typechecker stages don't care about it).
pub fn wrap_in_iife(token: &Token, expr_node: TypedAstNode) -> TypedAstNode {
    wrap_in_proper_iife(token, expr_node, &Type::Placeholder)
}

// Unlike `wrap_in_iife`, `wrap_in_proper_iife` allows to specify `typ`, and is
// meant to be called from the typechecker, where that field is used.
pub fn wrap_in_proper_iife(token: &Token, expr_node: TypedAstNode, typ: &Type) -> TypedAstNode {
    TypedAstNode::Invocation(
        Token::LParen(token.get_position(), false),
        TypedInvocationNode {
            typ: typ.clone(),
            target: Box::new(TypedAstNode::Lambda(
                Token::Arrow(token.get_position()),
                TypedLambdaNode {
                    typ: Type::Fn(vec![], Box::new(typ.clone())),
                    args: vec![],
                    typed_body: Some(vec![expr_node]),
                    orig_node: None,
                },
            )),
            args: vec![],
        },
    )
}
