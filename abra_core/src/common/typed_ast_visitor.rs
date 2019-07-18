use crate::lexer::tokens::Token;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedBinaryNode, TypedUnaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode, TypedInvocationNode};
use crate::typechecker::typed_ast::TypedAstNode::*;

pub trait TypedAstVisitor<V, E> {
    fn visit(&mut self, node: TypedAstNode) -> Result<V, E> {
        match node {
            Literal(tok, node) => self.visit_literal(tok, node),
            Unary(tok, node) => self.visit_unary(tok, node),
            Binary(tok, node) => self.visit_binary(tok, node),
            Grouped(tok, node) => self.visit_grouped(tok, node),
            Array(tok, node) => self.visit_array(tok, node),
            BindingDecl(tok, node) => self.visit_binding_decl(tok, node),
            FunctionDecl(tok, node) => self.visit_function_decl(tok, node),
            Identifier(tok, node) => self.visit_identifier(tok, node),
            Assignment(tok, node) => self.visit_assignment(tok, node),
            Indexing(tok, node) => self.visit_indexing(tok, node),
            IfStatement(tok, node) => self.visit_if_statement(true, tok, node),
            IfExpression(tok, node) => self.visit_if_expression(tok, node),
            Invocation(tok, node) => self.visit_invocation(tok, node),
        }
    }

    fn visit_literal(&mut self, token: Token, node: TypedLiteralNode) -> Result<V, E>;
    fn visit_unary(&mut self, token: Token, node: TypedUnaryNode) -> Result<V, E>;
    fn visit_binary(&mut self, token: Token, node: TypedBinaryNode) -> Result<V, E>;
    fn visit_grouped(&mut self, token: Token, node: TypedGroupedNode) -> Result<V, E>;
    fn visit_array(&mut self, token: Token, node: TypedArrayNode) -> Result<V, E>;
    fn visit_binding_decl(&mut self, token: Token, node: TypedBindingDeclNode) -> Result<V, E>;
    fn visit_function_decl(&mut self, token: Token, node: TypedFunctionDeclNode) -> Result<V, E>;
    fn visit_identifier(&mut self, token: Token, node: TypedIdentifierNode) -> Result<V, E>;
    fn visit_assignment(&mut self, token: Token, node: TypedAssignmentNode) -> Result<V, E>;
    fn visit_indexing(&mut self, token: Token, node: TypedIndexingNode) -> Result<V, E>;
    fn visit_if_statement(&mut self, is_stmt: bool, token: Token, node: TypedIfNode) -> Result<V, E>;
    fn visit_if_expression(&mut self, token: Token, node: TypedIfNode) -> Result<V, E>;
    fn visit_invocation(&mut self, token: Token, node: TypedInvocationNode) -> Result<V, E>;
}
