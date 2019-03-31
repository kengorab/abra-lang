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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Int(Position, i64),
    Float(Position, f64),
    String(Position, String),
    Bool(Position, bool),

    Plus(Position),
    Minus(Position),
    Star(Position),
    Slash(Position),
    And(Position),
    Or(Position),
}

impl Token {
    pub fn get_position(&self) -> Position {
        let pos = match self {
            Token::Int(pos, _) |
            Token::Float(pos, _) |
            Token::String(pos, _) |
            Token::Bool(pos, _) => pos,

            Token::Plus(pos) |
            Token::Minus(pos) |
            Token::Star(pos) |
            Token::Slash(pos) |
            Token::And(pos) |
            Token::Or(pos) => pos
        };
        pos.clone()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Token::Int(_, val) => write!(f, "{}", val),
            Token::Float(_, val) => write!(f, "{}", val),
            Token::String(_, val) => write!(f, "\"{}\"", val),
            Token::Bool(_, val) => write!(f, "{}", val),

            Token::Plus(_) => write!(f, "+"),
            Token::Minus(_) => write!(f, "-"),
            Token::Star(_) => write!(f, "*"),
            Token::Slash(_) => write!(f, "/"),
            Token::And(_) => write!(f, "&&"),
            Token::Or(_) => write!(f, "||"),
        }
    }
}