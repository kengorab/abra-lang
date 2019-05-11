use crate::typechecker::types::Type;
use crate::parser::ast::{UnaryOp, BinaryOp, IndexingMode};
use crate::lexer::tokens::Token;

#[derive(Debug, PartialEq)]
pub enum TypedAstNode {
    Literal(Token, TypedLiteralNode),
    Unary(Token, TypedUnaryNode),
    Binary(Token, TypedBinaryNode),
    Grouped(Token, TypedGroupedNode),
    Array(Token, TypedArrayNode),
    BindingDecl(Token, TypedBindingDeclNode),
    Identifier(Token, Type, bool),
    Assignment(Token, TypedAssignmentNode),
    Indexing(Token, TypedIndexingNode),
}

impl TypedAstNode {
    pub fn get_token(&self) -> &Token {
        match self {
            TypedAstNode::Literal(token, _) => token,
            TypedAstNode::Unary(token, _) => token,
            TypedAstNode::Binary(token, _) => token,
            TypedAstNode::Grouped(token, _) => token,
            TypedAstNode::Array(token, _) => token,
            TypedAstNode::BindingDecl(token, _) => token,
            TypedAstNode::Identifier(token, _, _) => token,
            TypedAstNode::Assignment(token, _) => token,
            TypedAstNode::Indexing(token, _) => token,
        }
    }

    pub fn get_type(&self) -> Type {
        match &self {
            TypedAstNode::Literal(_, node) => match node {
                TypedLiteralNode::IntLiteral(_) => Type::Int,
                TypedLiteralNode::FloatLiteral(_) => Type::Float,
                TypedLiteralNode::StringLiteral(_) => Type::String,
                TypedLiteralNode::BoolLiteral(_) => Type::Bool,
            },
            TypedAstNode::Unary(_, node) => node.typ.clone(),
            TypedAstNode::Binary(_, node) => node.typ.clone(),
            TypedAstNode::Grouped(_, node) => node.typ.clone(),
            TypedAstNode::Array(_, node) => node.typ.clone(),
            TypedAstNode::BindingDecl(_, _) => Type::Unit,
            TypedAstNode::Identifier(_, typ, _) => typ.clone(),
            TypedAstNode::Assignment(_, node) => node.typ.clone(),
            TypedAstNode::Indexing(_, node) => node.typ.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TypedLiteralNode {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
}

#[derive(Debug, PartialEq)]
pub struct TypedUnaryNode {
    pub typ: Type,
    pub op: UnaryOp,
    pub expr: Box<TypedAstNode>,
}

#[derive(Debug, PartialEq)]
pub struct TypedBinaryNode {
    pub typ: Type,
    pub right: Box<TypedAstNode>,
    pub op: BinaryOp,
    pub left: Box<TypedAstNode>,
}

#[derive(Debug, PartialEq)]
pub struct TypedGroupedNode {
    pub typ: Type,
    pub expr: Box<TypedAstNode>,
}

#[derive(Debug, PartialEq)]
pub struct TypedArrayNode {
    pub typ: Type,
    pub items: Vec<Box<TypedAstNode>>,
}

#[derive(Debug, PartialEq)]
pub struct TypedBindingDeclNode {
    // Must be a Token::Ident
    pub ident: Token,
    pub expr: Option<Box<TypedAstNode>>,
    pub is_mutable: bool,
}

#[derive(Debug, PartialEq)]
pub struct TypedAssignmentNode {
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    pub expr: Box<TypedAstNode>,
}

#[derive(Debug, PartialEq)]
pub struct TypedIndexingNode {
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    pub index: IndexingMode<TypedAstNode>,
}