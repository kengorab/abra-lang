use crate::typechecker::typed_ast::{TypedAstNode, TypedInvocationNode, TypedFunctionDeclNode};
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
// This version of the `wrap_in_iife` is meant to be called from the compiler, and passes bogus
// values for `typ` and `scope_depth` (since post-typechecker stages don't care about those values).
pub fn wrap_in_iife(token: &Token, expr_node: TypedAstNode) -> TypedAstNode {
    wrap_in_proper_iife(token, expr_node, &Type::Placeholder, 0)
}

// Unlike `wrap_in_iife`, `wrap_in_proper_iife` allows to specify `typ` and `scope_depth`, and is
// meant to be called from the typechecker, where those fields are used.
pub fn wrap_in_proper_iife(
    token: &Token,
    expr_node: TypedAstNode,
    typ: &Type,
    scope_depth: usize,
) -> TypedAstNode {
    let anon_fn_name = get_anon_name();

    TypedAstNode::Invocation(
        Token::LParen(token.get_position(), false),
        TypedInvocationNode {
            typ: typ.clone(),
            target: Box::new(TypedAstNode::FunctionDecl(
                Token::Func(token.get_position()),
                TypedFunctionDeclNode {
                    name: Token::Ident(token.get_position(), anon_fn_name),
                    args: vec![],
                    ret_type: typ.clone(),
                    body: vec![expr_node],
                    scope_depth,
                    is_recursive: false,
                    is_anon: true,
                },
            )),
            args: vec![],
        },
    )
}
