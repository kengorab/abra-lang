#[derive(Debug, Clone, PartialEq)]
pub struct Position { pub line: u32, pub col: u32 }

impl Position {
    pub fn new(line: u32, col: u32) -> Self { Position { line, col } }
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