use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Literal(Token, AstLiteralNode),
    Unary(Token, UnaryNode),
    Binary(Token, BinaryNode),
}

#[derive(Debug, PartialEq)]
pub enum AstLiteralNode {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Minus
}

#[derive(Debug, PartialEq)]
pub struct UnaryNode {
    pub op: UnaryOp,
    pub expr: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub struct BinaryNode {
    pub right: Box<AstNode>,
    pub op: BinaryOp,
    pub left: Box<AstNode>,
}
