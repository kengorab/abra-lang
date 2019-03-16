use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;

#[derive(Debug, PartialEq)]
pub enum TypecheckerError {
    Mismatch { token: Token, expected: Type, actual: Type },
    Placeholder,
}
