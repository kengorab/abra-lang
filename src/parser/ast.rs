use crate::lexer::tokens::Token;
use std::cmp::Ordering;

pub enum Precedence {
    None,
    // =
    Assignment,
    // ||
    Or,
    // &&
    And,
    // == !=
    Equality,
    // < > <= >=
    Comparison,
    // + -
    Addition,
    // * /
    Multiplication,
    // ! - +
    Unary,
    // . () []
    Call,
}

impl Into<u8> for Precedence {
    fn into(self) -> u8 {
        match self {
            Precedence::None => 0,
            Precedence::Assignment => 1,
            Precedence::Or => 2,
            Precedence::And => 3,
            Precedence::Equality => 4,
            Precedence::Comparison => 5,
            Precedence::Addition => 6,
            Precedence::Multiplication => 7,
            Precedence::Unary => 8,
            Precedence::Call => 9,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Minus
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Literal(Token, AstLiteralNode),
    Unary(Token, UnaryOp, Box<AstNode>),
    Binary(Token, Box<AstNode>, BinaryOp, Box<AstNode>),
}

#[derive(Debug, PartialEq)]
pub enum AstLiteralNode {
    IntLiteral(i64),
    FloatLiteral(f64),
}