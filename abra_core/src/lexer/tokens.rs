use std::fmt::{Display, Formatter, Error};

#[derive(Debug, Clone, PartialEq)]
pub struct Position { pub line: usize, pub col: usize }

impl Position {
    pub fn new(line: usize, col: usize) -> Self { Position { line, col } }
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    True,
    False,
    Val,
    Var,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Int(Position, i64),
    Float(Position, f64),
    String(Position, String),
    Bool(Position, bool),

    Val(Position),
    Var(Position),
    Ident(Position, String),

    Assign(Position),
    Plus(Position),
    Minus(Position),
    Star(Position),
    Slash(Position),
    And(Position),
    Or(Position),
    GT(Position),
    GTE(Position),
    LT(Position),
    LTE(Position),
    Eq(Position),
    Neq(Position),
    Bang(Position),

    LBrack(Position),
    RBrack(Position),
    Comma(Position),
    Colon(Position),
}

impl Token {
    pub fn get_position(&self) -> Position {
        let pos = match self {
            Token::Int(pos, _) |
            Token::Float(pos, _) |
            Token::String(pos, _) |
            Token::Bool(pos, _) |
            Token::Ident(pos, _) => pos,

            Token::Val(pos) |
            Token::Var(pos) |
            Token::Assign(pos) |
            Token::Plus(pos) |
            Token::Minus(pos) |
            Token::Star(pos) |
            Token::Slash(pos) |
            Token::And(pos) |
            Token::Or(pos) |
            Token::GT(pos) |
            Token::GTE(pos) |
            Token::LT(pos) |
            Token::LTE(pos) |
            Token::Eq(pos) |
            Token::Neq(pos) |
            Token::Bang(pos) |
            Token::LBrack(pos) |
            Token::RBrack(pos) |
            Token::Colon(pos) |
            Token::Comma(pos) => pos
        };
        pos.clone()
    }

    pub fn get_ident_name(token: &Token) -> &String {
        match token {
            Token::Ident(_, ident) => ident,
            _ => unreachable!()
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Token::Int(_, val) => write!(f, "{}", val),
            Token::Float(_, val) => write!(f, "{}", val),
            Token::String(_, val) => write!(f, "\"{}\"", val),
            Token::Bool(_, val) => write!(f, "{}", val),
            Token::Ident(_, name) => write!(f, "{}", name),

            Token::Val(_) => write!(f, "val"),
            Token::Var(_) => write!(f, "var"),
            Token::Assign(_) => write!(f, "="),
            Token::Plus(_) => write!(f, "+"),
            Token::Minus(_) => write!(f, "-"),
            Token::Star(_) => write!(f, "*"),
            Token::Slash(_) => write!(f, "/"),
            Token::And(_) => write!(f, "&&"),
            Token::Or(_) => write!(f, "||"),
            Token::GT(_) => write!(f, ">"),
            Token::GTE(_) => write!(f, ">="),
            Token::LT(_) => write!(f, "<"),
            Token::LTE(_) => write!(f, "<="),
            Token::Eq(_) => write!(f, "=="),
            Token::Neq(_) => write!(f, "!="),
            Token::Bang(_) => write!(f, "!"),

            Token::LBrack(_) => write!(f, "["),
            Token::RBrack(_) => write!(f, "]"),
            Token::Comma(_) => write!(f, ","),
            Token::Colon(_) => write!(f, ":"),
        }
    }
}