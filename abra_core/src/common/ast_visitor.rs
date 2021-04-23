use crate::parser::ast::{AstNode, AstLiteralNode, UnaryNode, BinaryNode, ArrayNode, BindingDeclNode, AssignmentNode, IndexingNode, GroupedNode, IfNode, FunctionDeclNode, InvocationNode, WhileLoopNode, ForLoopNode, TypeDeclNode, MapNode, AccessorNode, LambdaNode, TypeIdentifier, EnumDeclNode, MatchNode, SetNode, ImportNode};
use crate::parser::ast::AstNode::*;
use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;
use crate::typechecker::typed_ast::TypedAstNode;

pub trait AstVisitor<V, E> {
    fn visit(&mut self, node: AstNode) -> Result<V, E> {
        match node {
            Literal(tok, node) => self.visit_literal(tok, node),
            Unary(tok, node) => self.visit_unary(tok, node),
            Binary(tok, node) => self.visit_binary(tok, node),
            Grouped(tok, node) => self.visit_grouped(tok, node),
            Array(tok, node) => self.visit_array(tok, node),
            Map(tok, node) => self.visit_map_literal(tok, node),
            Set(tok, node) => self.visit_set_literal(tok, node),
            BindingDecl(tok, node) => self.visit_binding_decl(tok, node),
            FunctionDecl(tok, node) => self.visit_func_decl(tok, node),
            TypeDecl(tok, node) => self.visit_type_decl(tok, node),
            EnumDecl(tok, node)=> self.visit_enum_decl(tok, node),
            Identifier(tok, type_args) => self.visit_ident(tok, type_args),
            Assignment(tok, node) => self.visit_assignment(tok, node),
            Indexing(tok, node) => self.visit_indexing(tok, node),
            IfStatement(tok, node) => self.visit_if_statement(tok, node),
            IfExpression(tok, node) => self.visit_if_expression(tok, node),
            MatchStatement(tok, node) => self.visit_match_statement(tok, node),
            MatchExpression(tok, node) => self.visit_match_expression(tok, node),
            Invocation(tok, node) => self.visit_invocation(tok, node),
            WhileLoop(tok, node) => self.visit_while_loop(tok, node),
            Break(tok) => self.visit_break(tok),
            Continue(tok) => self.visit_continue(tok),
            ReturnStatement(tok, node) => self.visit_return(tok, node),
            ImportStatement(tok, node) => self.visit_import(tok, node),
            ForLoop(tok, node) => self.visit_for_loop(tok, node),
            Accessor(tok, node) => self.visit_accessor(tok, node),
            Lambda(tok, node) => self.visit_lambda(tok, node, None),
            Tuple(tok, nodes) => self.visit_tuple(tok, nodes),
        }
    }

    fn visit_literal(&mut self, token: Token, node: AstLiteralNode) -> Result<V, E>;
    fn visit_unary(&mut self, token: Token, node: UnaryNode) -> Result<V, E>;
    fn visit_binary(&mut self, token: Token, node: BinaryNode) -> Result<V, E>;
    fn visit_grouped(&mut self, token: Token, node: GroupedNode) -> Result<V, E>;
    fn visit_array(&mut self, token: Token, node: ArrayNode) -> Result<V, E>;
    fn visit_map_literal(&mut self, token: Token, node: MapNode) -> Result<V, E>;
    fn visit_set_literal(&mut self, token: Token, node: SetNode) -> Result<V, E>;
    fn visit_binding_decl(&mut self, token: Token, node: BindingDeclNode) -> Result<V, E>;
    fn visit_func_decl(&mut self, token: Token, node: FunctionDeclNode) -> Result<V, E>;
    fn visit_type_decl(&mut self, token: Token, node: TypeDeclNode) -> Result<V, E>;
    fn visit_enum_decl(&mut self, token: Token, node: EnumDeclNode) -> Result<V, E>;
    fn visit_ident(&mut self, token: Token, type_args: Option<Vec<TypeIdentifier>>) -> Result<V, E>;
    fn visit_assignment(&mut self, token: Token, node: AssignmentNode) -> Result<V, E>;
    fn visit_indexing(&mut self, token: Token, node: IndexingNode) -> Result<V, E>;
    fn visit_if_statement(&mut self, token: Token, node: IfNode) -> Result<V, E>;
    fn visit_if_expression(&mut self, token: Token, node: IfNode) -> Result<V, E>;
    fn visit_match_statement(&mut self, token: Token, node: MatchNode) -> Result<V, E>;
    fn visit_match_expression(&mut self, token: Token, node: MatchNode) -> Result<V, E>;
    fn visit_invocation(&mut self, token: Token, node: InvocationNode) -> Result<V, E>;
    fn visit_for_loop(&mut self, token: Token, node: ForLoopNode) -> Result<V, E>;
    fn visit_while_loop(&mut self, token: Token, node: WhileLoopNode) -> Result<V, E>;
    fn visit_break(&mut self, token: Token) -> Result<V, E>;
    fn visit_continue(&mut self, token: Token) -> Result<V, E>;
    fn visit_return(&mut self, token: Token, node: Option<Box<AstNode>>) -> Result<V, E>;
    fn visit_import(&mut self, token: Token, node: ImportNode) -> Result<V, E>;
    fn visit_accessor(&mut self, token: Token, node: AccessorNode) -> Result<V, E>;
    fn visit_lambda(&mut self, token: Token, node: LambdaNode, args_override: Option<Vec<(Token, Type, Option<TypedAstNode>)>>) -> Result<V, E>;
    fn visit_tuple(&mut self, token: Token, nodes: Vec<AstNode>) -> Result<V, E>;
}
