use std::fmt::{Display, Formatter, Error};

#[derive(Debug, Clone, PartialEq)]
pub struct Position { pub line: usize, pub col: usize }

impl Position {
    pub fn new(line: usize, col: usize) -> Self { Position { line, col } }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Int(Position, i64),
    Float(Position, f64),

    Plus(Position),
    Minus(Position),
    Star(Position),
    Slash(Position),
}

impl Token {
    pub fn get_position(&self) -> Position {
        let pos = match self {
            Token::Int(pos, _) | Token::Float(pos, _) => pos,
            Token::Plus(pos) |
            Token::Minus(pos) |
            Token::Star(pos) |
            Token::Slash(pos) => pos
        };
        pos.clone()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Token::Int(_, val) => write!(f, "{}", val),
            Token::Float(_, val) => write!(f, "{}", val),
            Token::Plus(_) => write!(f, "+"),
            Token::Minus(_) => write!(f, "-"),
            Token::Star(_) => write!(f, "*"),
            Token::Slash(_) => write!(f, "/"),
        }
    }
}