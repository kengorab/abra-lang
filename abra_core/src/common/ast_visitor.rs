use crate::parser::ast::{AstNode, AstLiteralNode, UnaryNode, BinaryNode, ArrayNode, BindingDeclNode, AssignmentNode, IndexingNode, GroupedNode, IfNode, FunctionDeclNode, InvocationNode, WhileLoopNode, ForLoopNode};
use crate::parser::ast::AstNode::*;
use crate::lexer::tokens::Token;

pub trait AstVisitor<V, E> {
    fn visit(&mut self, node: AstNode) -> Result<V, E> {
        match node {
            Literal(tok, node) => self.visit_literal(tok, node),
            Unary(tok, node) => self.visit_unary(tok, node),
            Binary(tok, node) => self.visit_binary(tok, node),
            Grouped(tok, node) => self.visit_grouped(tok, node),
            Array(tok, node) => self.visit_array(tok, node),
            BindingDecl(tok, node) => self.visit_binding_decl(tok, node),
            FunctionDecl(tok, node) => self.visit_func_decl(tok, node),
            Identifier(tok) => self.visit_ident(tok),
            Assignment(tok, node) => self.visit_assignment(tok, node),
            Indexing(tok, node) => self.visit_indexing(tok, node),
            IfStatement(tok, node) => self.visit_if_statement(tok, node),
            IfExpression(tok, node) => self.visit_if_expression(tok, node),
            Invocation(tok, node) => self.visit_invocation(tok, node),
            WhileLoop(tok, node) => self.visit_while_loop(tok, node),
            Break(tok) => self.visit_break(tok),
            ForLoop(tok, node) => self.visit_for_loop(tok, node),
        }
    }

    fn visit_literal(&mut self, token: Token, node: AstLiteralNode) -> Result<V, E>;
    fn visit_unary(&mut self, token: Token, node: UnaryNode) -> Result<V, E>;
    fn visit_binary(&mut self, token: Token, node: BinaryNode) -> Result<V, E>;
    fn visit_grouped(&mut self, token: Token, node: GroupedNode) -> Result<V, E>;
    fn visit_array(&mut self, token: Token, node: ArrayNode) -> Result<V, E>;
    fn visit_binding_decl(&mut self, token: Token, node: BindingDeclNode) -> Result<V, E>;
    fn visit_func_decl(&mut self, token: Token, node: FunctionDeclNode) -> Result<V, E>;
    fn visit_ident(&mut self, token: Token) -> Result<V, E>;
    fn visit_assignment(&mut self, token: Token, node: AssignmentNode) -> Result<V, E>;
    fn visit_indexing(&mut self, token: Token, node: IndexingNode) -> Result<V, E>;
    fn visit_if_statement(&mut self, token: Token, node: IfNode) -> Result<V, E>;
    fn visit_if_expression(&mut self, token: Token, node: IfNode) -> Result<V, E>;
    fn visit_invocation(&mut self, token: Token, node: InvocationNode) -> Result<V, E>;
    fn visit_for_loop(&mut self, token: Token, node: ForLoopNode) -> Result<V, E>;
    fn visit_while_loop(&mut self, token: Token, node: WhileLoopNode) -> Result<V, E>;
    fn visit_break(&mut self, token: Token) -> Result<V, E>;
}
