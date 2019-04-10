use crate::typechecker::types::Type;
use crate::parser::ast::{UnaryOp, BinaryOp};
use crate::lexer::tokens::Token;

#[derive(Debug, PartialEq)]
pub enum TypedAstNode {
    Literal(Token, TypedLiteralNode),
    Unary(Token, TypedUnaryNode),
    Binary(Token, TypedBinaryNode),
    Array(Token, TypedArrayNode),
    BindingDecl(Token, TypedBindingDeclNode),
}

impl TypedAstNode {
    pub fn get_token(&self) -> &Token {
        match self {
            TypedAstNode::Literal(token, _) => token,
            TypedAstNode::Unary(token, _) => token,
            TypedAstNode::Binary(token, _) => token,
            TypedAstNode::Array(token, _) => token,
            TypedAstNode::BindingDecl(token, _) => token,
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
            TypedAstNode::Array(_, node) => node.typ.clone(),
            TypedAstNode::BindingDecl(_, _) => Type::Unit,
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
