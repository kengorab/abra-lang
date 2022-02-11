use std::str::FromStr;
use crate::lexer::tokens::{Token, TokenType, Position, Range};
use crate::common::display_error::{DisplayError, IND_AMT};
use crate::parser::ast::ModuleId;
use itertools::Itertools;

#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    UnexpectedEof(Range),
    UnexpectedToken(Token),
    ExpectedToken(TokenType, Token),
    ExpectedOneOf(Vec<TokenType>, Token),
    InvalidImportPath(Token),
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub module_id: ModuleId,
    pub kind: ParseErrorKind,
}

impl DisplayError for ParseError {
    fn message_for_error(&self, file_name: &String, lines: &Vec<&str>) -> String {
        match &self.kind {
            ParseErrorKind::UnexpectedToken(token) => {
                let pos = token.get_position();
                let message = Self::get_underlined_line(lines, token);

                format!(
                    "Error at {}:{}:{}\nUnexpected token '{}'\n{}",
                    file_name, pos.line, pos.col, token.to_string(), message
                )
            }
            ParseErrorKind::UnexpectedEof(range) => {
                let Position { line, col } = range.end;
                let last_line = lines.get(line - 1).expect("There should be a last line");

                let cursor = format!("{}^", " ".repeat(2 * IND_AMT + col));
                let indent = Self::indent();
                let message = format!("{}|{}{}\n{}", indent, indent, last_line, cursor);

                format!(
                    "Error at {}:{}:{}\nUnexpected end of file\n{}",
                    file_name, line, col, message
                )
            }
            ParseErrorKind::ExpectedToken(expected, actual) => {
                let pos = actual.get_position();
                let message = Self::get_underlined_line(lines, actual);

                // Convert from TokenType to Token, to make use of the #[strum(to_string)] meta,
                // since strum doesn't apply the #[strum(to_string)] to the discriminants.
                let expected: Token = Token::from_str(&expected.to_string()).unwrap();
                format!(
                    "Error at {}:{}:{}\nExpected token '{}', saw '{}'\n{}",
                    file_name, pos.line, pos.col, expected.to_string(), actual.to_string(), message
                )
            }
            ParseErrorKind::ExpectedOneOf(expected, actual) => {
                let pos = actual.get_position();
                let message = Self::get_underlined_line(lines, actual);

                // Convert from TokenTypes to Tokens, to make use of the #[strum(to_string)] meta,
                // since strum doesn't apply the #[strum(to_string)] to the discriminants.
                let expecteds = expected.iter()
                    .map(|token_type| format!("'{}'", Token::from_str(&token_type.to_string()).unwrap()))
                    .join(" | ");
                format!(
                    "Error at {}:{}:{}\nExpected one of {}, saw '{}'\n{}",
                    file_name, pos.line, pos.col, expecteds, actual.to_string(), message
                )
            }
            ParseErrorKind::InvalidImportPath(token) => {
                let pos = token.get_position();
                let message = Self::get_underlined_line(lines, token);
                format!(
                    "Error at {}:{}:{}\nInvalid import path\n{}",
                    file_name, pos.line, pos.col, message
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ParseErrorKind;
    use crate::lexer::tokens::{Token, TokenType, Position, Range};
    use crate::common::display_error::DisplayError;
    use crate::parser::ast::ModuleId;
    use crate::parser::parse_error::ParseError;

    #[test]
    fn test_unexpected_token_error() {
        let module_id = ModuleId::from_name("test");
        let src = "-+".to_string();
        let token = Token::Plus(Position::new(1, 2));
        let err = ParseError { module_id, kind: ParseErrorKind::UnexpectedToken(token) };

        let expected = format!("\
Error at /tests/test.abra:1:2
Unexpected token '+'
  |  -+
      ^");
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(), &src));
    }

    #[test]
    fn test_expected_token_error() {
        let module_id = ModuleId::from_name("test");
        let src = "val a: = 123".to_string();
        let err = ParseError {
            module_id,
            kind: ParseErrorKind::ExpectedToken(TokenType::Ident, Token::Assign(Position::new(1, 8))),
        };

        let expected = format!("\
Error at /tests/test.abra:1:8
Expected token 'identifier', saw '='
  |  val a: = 123
            ^");
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(), &src));
    }

    #[test]
    fn test_unexpected_eof_error() {
        let module_id = ModuleId::from_name("test");
        let src = "-".to_string();
        let err = ParseError { module_id, kind: ParseErrorKind::UnexpectedEof(Range::with_length(&Position::new(1, 1), 1)) };

        let expected = format!("\
Error at /tests/test.abra:1:2
Unexpected end of file
  |  -
      ^");
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(), &src));
    }
}
