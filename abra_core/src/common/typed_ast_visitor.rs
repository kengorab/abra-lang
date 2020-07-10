use crate::lexer::tokens::Token;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedBinaryNode, TypedUnaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode, TypedInvocationNode, TypedWhileLoopNode, TypedForLoopNode, TypedTypeDeclNode, TypedMapNode, TypedAccessorNode, TypedInstantiationNode, TypedLambdaNode};
use crate::typechecker::typed_ast::TypedAstNode::*;

pub trait TypedAstVisitor<V, E> {
    fn visit(&mut self, node: TypedAstNode) -> Result<V, E> {
        match node {
            Literal(tok, node) => self.visit_literal(tok, node),
            Unary(tok, node) => self.visit_unary(tok, node),
            Binary(tok, node) => self.visit_binary(tok, node),
            Grouped(tok, node) => self.visit_grouped(tok, node),
            Array(tok, node) => self.visit_array(tok, node),
            Map(tok, node) => self.visit_map(tok, node),
            Lambda(tok, node) => self.visit_lambda(tok, node),
            BindingDecl(tok, node) => self.visit_binding_decl(tok, node),
            FunctionDecl(tok, node) => self.visit_function_decl(tok, node),
            TypeDecl(tok, node) => self.visit_type_decl(tok, node),
            Identifier(tok, node) => self.visit_identifier(tok, node),
            Assignment(tok, node) => self.visit_assignment(tok, node),
            Indexing(tok, node) => self.visit_indexing(tok, node),
            IfStatement(tok, node) => self.visit_if_statement(true, tok, node),
            IfExpression(tok, node) => self.visit_if_expression(tok, node),
            Invocation(tok, node) => self.visit_invocation(tok, node),
            Instantiation(tok, node) => self.visit_instantiation(tok, node),
            Accessor(tok, node) => self.visit_accessor(tok, node),
            ForLoop(tok, node) => self.visit_for_loop(tok, node),
            WhileLoop(tok, node) => self.visit_while_loop(tok, node),
            Break(tok) => self.visit_break(tok),
            _Nil(tok) => self.visit_nil(tok),
        }
    }

    fn visit_literal(&mut self, token: Token, node: TypedLiteralNode) -> Result<V, E>;
    fn visit_unary(&mut self, token: Token, node: TypedUnaryNode) -> Result<V, E>;
    fn visit_binary(&mut self, token: Token, node: TypedBinaryNode) -> Result<V, E>;
    fn visit_grouped(&mut self, token: Token, node: TypedGroupedNode) -> Result<V, E>;
    fn visit_array(&mut self, token: Token, node: TypedArrayNode) -> Result<V, E>;
    fn visit_map(&mut self, token: Token, node: TypedMapNode) -> Result<V, E>;
    fn visit_lambda(&mut self, token: Token, node: TypedLambdaNode) -> Result<V, E>;
    fn visit_binding_decl(&mut self, token: Token, node: TypedBindingDeclNode) -> Result<V, E>;
    fn visit_function_decl(&mut self, token: Token, node: TypedFunctionDeclNode) -> Result<V, E>;
    fn visit_type_decl(&mut self, token: Token, node: TypedTypeDeclNode) -> Result<V, E>;
    fn visit_identifier(&mut self, token: Token, node: TypedIdentifierNode) -> Result<V, E>;
    fn visit_assignment(&mut self, token: Token, node: TypedAssignmentNode) -> Result<V, E>;
    fn visit_indexing(&mut self, token: Token, node: TypedIndexingNode) -> Result<V, E>;
    fn visit_if_statement(&mut self, is_stmt: bool, token: Token, node: TypedIfNode) -> Result<V, E>;
    fn visit_if_expression(&mut self, token: Token, node: TypedIfNode) -> Result<V, E>;
    fn visit_invocation(&mut self, token: Token, node: TypedInvocationNode) -> Result<V, E>;
    fn visit_instantiation(&mut self, token: Token, node: TypedInstantiationNode) -> Result<V, E>;
    fn visit_accessor(&mut self, token: Token, node: TypedAccessorNode) -> Result<V, E>;
    fn visit_for_loop(&mut self, token: Token, node: TypedForLoopNode) -> Result<V, E>;
    fn visit_while_loop(&mut self, token: Token, node: TypedWhileLoopNode) -> Result<V, E>;
    fn visit_break(&mut self, token: Token) -> Result<V, E>;
    fn visit_nil(&mut self, token: Token) -> Result<V, E>;
}
