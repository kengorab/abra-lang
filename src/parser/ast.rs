use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Literal(Token, AstLiteralNode),
    Unary(Token, UnaryNode),
    Binary(Token, BinaryNode),
}

impl AstNode {
    pub fn get_token(&self) -> Token {
        match self {
            AstNode::Literal(token, _) => token.clone(),
            AstNode::Unary(token, _) => token.clone(),
            AstNode::Binary(token, _) => token.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AstLiteralNode {
    IntLiteral(i64),
    FloatLiteral(f64),
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Minus
}

#[derive(Debug, PartialEq)]
pub struct UnaryNode {
    pub typ: Option<Type>,
    pub op: UnaryOp,
    pub expr: Box<AstNode>,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub struct BinaryNode {
    pub typ: Option<Type>,
    pub right: Box<AstNode>,
    pub op: BinaryOp,
    pub left: Box<AstNode>,
}
