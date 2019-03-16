use crate::lexer::tokens::Token;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedEof,
    UnexpectedToken(Token),
    Raw(String)
}