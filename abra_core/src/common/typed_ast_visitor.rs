use crate::lexer::tokens::Token;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedBinaryNode, TypedUnaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode};
use crate::typechecker::typed_ast::TypedAstNode::*;
use crate::typechecker::types::Type;

pub trait TypedAstVisitor<V, E> {
    fn visit(&mut self, node: TypedAstNode) -> Result<V, E> {
        match node {
            Literal(tok, node) => self.visit_literal(tok, node),
            Unary(tok, node) => self.visit_unary(tok, node),
            Binary(tok, node) => self.visit_binary(tok, node),
            Grouped(tok, node) => self.visit_grouped(tok, node),
            Array(tok, node) => self.visit_array(tok, node),
            BindingDecl(tok, node) => self.visit_binding_decl(tok, node),
            Identifier(tok, typ, is_mutable) => self.visit_identifier(tok, typ, is_mutable),
            Assignment(tok, typ) => self.visit_assignment(tok, typ),
            Indexing(tok, typ) => self.visit_indexing(tok, typ),
            IfStatement(tok, typ) => self.visit_if_statement(tok, typ),
        }
    }

    fn visit_literal(&mut self, token: Token, node: TypedLiteralNode) -> Result<V, E>;
    fn visit_unary(&mut self, token: Token, node: TypedUnaryNode) -> Result<V, E>;
    fn visit_binary(&mut self, token: Token, node: TypedBinaryNode) -> Result<V, E>;
    fn visit_grouped(&mut self, token: Token, node: TypedGroupedNode) -> Result<V, E>;
    fn visit_array(&mut self, token: Token, node: TypedArrayNode) -> Result<V, E>;
    fn visit_binding_decl(&mut self, token: Token, node: TypedBindingDeclNode) -> Result<V, E>;
    fn visit_identifier(&mut self, token: Token, typ: Type, is_mutable: bool) -> Result<V, E>;
    fn visit_assignment(&mut self, token: Token, node: TypedAssignmentNode) -> Result<V, E>;
    fn visit_indexing(&mut self, token: Token, node: TypedIndexingNode) -> Result<V, E>;
    fn visit_if_statement(&mut self, token: Token, node: TypedIfNode) -> Result<V, E>;
}
