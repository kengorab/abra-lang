use crate::parser::ast::{AstNode, AstLiteralNode, UnaryNode, BinaryNode};
use crate::parser::ast::AstNode::*;
use crate::lexer::tokens::Token;

pub trait AstVisitor<V, E> {
    fn visit(&self, node: AstNode) -> Result<V, E> {
        match node {
            Literal(tok, node) => self.visit_literal(tok, node),
            Unary(tok, node) => self.visit_unary(tok, node),
            Binary(tok, node) => self.visit_binary(tok, node),
        }
    }

    fn visit_literal(&self, token: Token, node: AstLiteralNode) -> Result<V, E>;
    fn visit_unary(&self, token: Token, node: UnaryNode) -> Result<V, E>;
    fn visit_binary(&self, token: Token, node: BinaryNode) -> Result<V, E>;
}
