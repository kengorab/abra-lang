use crate::parser::ast::{AstNode, AstLiteralNode, UnaryNode, BinaryNode, ArrayNode, BindingDeclNode};
use crate::parser::ast::AstNode::*;
use crate::lexer::tokens::Token;

pub trait AstVisitor<V, E> {
    fn visit(&self, node: AstNode) -> Result<V, E> {
        match node {
            Literal(tok, node) => self.visit_literal(tok, node),
            Unary(tok, node) => self.visit_unary(tok, node),
            Binary(tok, node) => self.visit_binary(tok, node),
            Array(tok, node) => self.visit_array(tok, node),
            BindingDecl(tok, node) => self.visit_binding_decl(tok, node),
        }
    }

    fn visit_literal(&self, token: Token, node: AstLiteralNode) -> Result<V, E>;
    fn visit_unary(&self, token: Token, node: UnaryNode) -> Result<V, E>;
    fn visit_binary(&self, token: Token, node: BinaryNode) -> Result<V, E>;
    fn visit_array(&self, token: Token, node: ArrayNode) -> Result<V, E>;
    fn visit_binding_decl(&self, token: Token, node: BindingDeclNode) -> Result<V, E>;
}
