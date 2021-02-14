use crate::lexer::tokens::Token;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedBinaryNode, TypedUnaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode, TypedInvocationNode, TypedWhileLoopNode, TypedForLoopNode, TypedTypeDeclNode, TypedMapNode, TypedAccessorNode, TypedInstantiationNode, TypedLambdaNode, TypedEnumDeclNode, TypedMatchNode, TypedReturnNode, TypedTupleNode, TypedSetNode};
use crate::typechecker::typed_ast::TypedAstNode::*;
use crate::parser::ast::ImportNode;

pub trait TypedAstVisitor<V, E> {
    fn visit(&mut self, node: TypedAstNode) -> Result<V, E> {
        match node {
            Literal(tok, node) => self.visit_literal(tok, node),
            Unary(tok, node) => self.visit_unary(tok, node),
            Binary(tok, node) => self.visit_binary(tok, node),
            Grouped(tok, node) => self.visit_grouped(tok, node),
            Array(tok, node) => self.visit_array(tok, node),
            Tuple(tok, node) => self.visit_tuple(tok, node),
            Map(tok, node) => self.visit_map(tok, node),
            Set(tok, node) => self.visit_set(tok, node),
            Lambda(tok, node) => self.visit_lambda(tok, node),
            BindingDecl(tok, node) => self.visit_binding_decl(tok, node),
            FunctionDecl(tok, node) => self.visit_function_decl(tok, node),
            TypeDecl(tok, node) => self.visit_type_decl(tok, node),
            EnumDecl(tok, node) => self.visit_enum_decl(tok, node),
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
            ReturnStatement(tok, node) => self.visit_return(tok, node),
            MatchStatement(tok, node) => self.visit_match_statement(true, tok, node),
            MatchExpression(tok, node) => self.visit_match_expression(tok, node),
            ImportStatement(tok, node) => self.visit_import_statement(tok, node),
            _Nil(tok) => self.visit_nil(tok),
        }
    }

    fn visit_literal(&mut self, token: Token, node: TypedLiteralNode) -> Result<V, E>;
    fn visit_unary(&mut self, token: Token, node: TypedUnaryNode) -> Result<V, E>;
    fn visit_binary(&mut self, token: Token, node: TypedBinaryNode) -> Result<V, E>;
    fn visit_grouped(&mut self, token: Token, node: TypedGroupedNode) -> Result<V, E>;
    fn visit_array(&mut self, token: Token, node: TypedArrayNode) -> Result<V, E>;
    fn visit_tuple(&mut self, token: Token, node: TypedTupleNode) -> Result<V, E>;
    fn visit_map(&mut self, token: Token, node: TypedMapNode) -> Result<V, E>;
    fn visit_set(&mut self, token: Token, node: TypedSetNode) -> Result<V, E>;
    fn visit_lambda(&mut self, token: Token, node: TypedLambdaNode) -> Result<V, E>;
    fn visit_binding_decl(&mut self, token: Token, node: TypedBindingDeclNode) -> Result<V, E>;
    fn visit_function_decl(&mut self, token: Token, node: TypedFunctionDeclNode) -> Result<V, E>;
    fn visit_type_decl(&mut self, token: Token, node: TypedTypeDeclNode) -> Result<V, E>;
    fn visit_enum_decl(&mut self, token: Token, node: TypedEnumDeclNode) -> Result<V, E>;
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
    fn visit_return(&mut self, token: Token, node: TypedReturnNode) -> Result<V, E>;
    fn visit_match_statement(&mut self, is_stmt: bool, token: Token, node: TypedMatchNode) -> Result<V, E>;
    fn visit_match_expression(&mut self, token: Token, node: TypedMatchNode) -> Result<V, E>;
    fn visit_import_statement(&mut self, token: Token, node: ImportNode) -> Result<V, E>;
    fn visit_nil(&mut self, token: Token) -> Result<V, E>;
}
