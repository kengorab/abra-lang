use crate::typechecker::types::Type;
use crate::parser::ast::{UnaryOp, BinaryOp, IndexingMode};
use crate::lexer::tokens::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum TypedAstNode {
    Literal(Token, TypedLiteralNode),
    Unary(Token, TypedUnaryNode),
    Binary(Token, TypedBinaryNode),
    Grouped(Token, TypedGroupedNode),
    Array(Token, TypedArrayNode),
    Map(Token, TypedMapNode),
    BindingDecl(Token, TypedBindingDeclNode),
    FunctionDecl(Token, TypedFunctionDeclNode),
    TypeDecl(Token, TypedTypeDeclNode),
    Identifier(Token, TypedIdentifierNode),
    Assignment(Token, TypedAssignmentNode),
    Indexing(Token, TypedIndexingNode),
    IfStatement(Token, TypedIfNode),
    IfExpression(Token, TypedIfNode),
    Invocation(Token, TypedInvocationNode),
    Instantiation(Token, TypedInstantiationNode),
    ForLoop(Token, TypedForLoopNode),
    WhileLoop(Token, TypedWhileLoopNode),
    Break(Token),
    Accessor(Token, TypedAccessorNode),
    _Nil(Token),
}

impl TypedAstNode {
    pub fn get_token(&self) -> &Token {
        match self {
            TypedAstNode::Literal(token, _) => token,
            TypedAstNode::Unary(token, _) => token,
            TypedAstNode::Binary(token, _) => token,
            TypedAstNode::Grouped(token, _) => token,
            TypedAstNode::Array(token, _) => token,
            TypedAstNode::Map(token, _) => token,
            TypedAstNode::BindingDecl(token, _) => token,
            TypedAstNode::FunctionDecl(token, _) => token,
            TypedAstNode::TypeDecl(token, _) => token,
            TypedAstNode::Identifier(token, _) => token,
            TypedAstNode::Assignment(token, _) => token,
            TypedAstNode::Indexing(token, _) => token,
            TypedAstNode::IfStatement(token, _) => token,
            TypedAstNode::IfExpression(token, _) => token,
            TypedAstNode::Invocation(token, _) => token,
            TypedAstNode::Instantiation(token, _) => token,
            TypedAstNode::ForLoop(token, _) => token,
            TypedAstNode::WhileLoop(token, _) => token,
            TypedAstNode::Break(token) => token,
            TypedAstNode::Accessor(token, _) => token,
            TypedAstNode::_Nil(token) => token,
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
            TypedAstNode::Map(_, node) => node.typ.clone(),
            TypedAstNode::BindingDecl(_, _) |
            TypedAstNode::FunctionDecl(_, _) |
            TypedAstNode::TypeDecl(_, _) |
            TypedAstNode::WhileLoop(_, _) |
            TypedAstNode::Break(_) |
            TypedAstNode::ForLoop(_, _) => Type::Unit,
            TypedAstNode::Identifier(_, node) => node.typ.clone(),
            TypedAstNode::Assignment(_, node) => node.typ.clone(),
            TypedAstNode::Indexing(_, node) => node.typ.clone(),
            TypedAstNode::IfStatement(_, node) => node.typ.clone(),
            TypedAstNode::IfExpression(_, node) => node.typ.clone(),
            TypedAstNode::Invocation(_, node) => node.typ.clone(),
            TypedAstNode::Instantiation(_, node) => node.typ.clone(),
            TypedAstNode::Accessor(_, node) => node.typ.clone(),
            TypedAstNode::_Nil(_) => Type::Any,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedLiteralNode {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedUnaryNode {
    pub typ: Type,
    pub op: UnaryOp,
    pub expr: Box<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedBinaryNode {
    pub typ: Type,
    pub right: Box<TypedAstNode>,
    pub op: BinaryOp,
    pub left: Box<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedGroupedNode {
    pub typ: Type,
    pub expr: Box<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedArrayNode {
    pub typ: Type,
    pub items: Vec<Box<TypedAstNode>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedMapNode {
    pub typ: Type,
    pub items: Vec<(Token, TypedAstNode)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedBindingDeclNode {
    // Must be a Token::Ident
    pub ident: Token,
    pub expr: Option<Box<TypedAstNode>>,
    pub is_mutable: bool,
    pub scope_depth: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedFunctionDeclNode {
    // Must be a Token::Ident
    pub name: Token,
    // Tokens represent arg idents, and must be Token::Ident
    pub args: Vec<(Token, Type, Option<TypedAstNode>)>,
    pub ret_type: Type,
    pub body: Vec<TypedAstNode>,
    pub scope_depth: usize,
    pub is_recursive: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedTypeDeclNode {
    // Must be a Token::Ident
    pub name: Token,
    // Tokens represent arg idents, and must be Token::Ident
    pub fields: Vec<(Token, Type, Option<TypedAstNode>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedIdentifierNode {
    pub typ: Type,
    pub name: String,
    pub is_mutable: bool,
    pub scope_depth: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedAssignmentNode {
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    pub expr: Box<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedIndexingNode {
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    pub index: IndexingMode<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedIfNode {
    pub typ: Type,
    pub condition: Box<TypedAstNode>,
    pub if_block: Vec<TypedAstNode>,
    pub else_block: Option<Vec<TypedAstNode>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedInvocationNode {
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    pub args: Vec<Option<TypedAstNode>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedInstantiationNode {
    pub typ: Type,
    pub fields: Vec<(String, TypedAstNode)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedWhileLoopNode {
    pub condition: Box<TypedAstNode>,
    pub body: Vec<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedForLoopNode {
    pub iteratee: Token,
    pub index_ident: Option<Token>,
    pub iterator: Box<TypedAstNode>,
    pub body: Vec<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedAccessorNode {
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    pub field: Token,
}

