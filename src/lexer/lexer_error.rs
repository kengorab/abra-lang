use crate::lexer::tokens::Position;

#[derive(Debug, PartialEq)]
pub enum LexerError {
    UnexpectedChar(Position, String),
    UnterminatedString(Position),
    UnexpectedEof(Position),
}