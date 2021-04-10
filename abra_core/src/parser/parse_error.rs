use std::str::FromStr;
use crate::lexer::tokens::{Token, TokenType, Position, Range};
use crate::common::display_error::{DisplayError, IND_AMT};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedEof(Range),
    UnexpectedToken(Token),
    ExpectedToken(TokenType, Token),
}

impl DisplayError for ParseError {
    fn message_for_error(&self, lines: &Vec<&str>) -> String {
        match self {
            ParseError::UnexpectedToken(token) => {
                let pos = token.get_position();
                let message = Self::get_underlined_line(lines, token);

                format!("Unexpected token '{}' ({}:{})\n{}", token.to_string(), pos.line, pos.col, message)
            }
            ParseError::UnexpectedEof(range) => {
                let Position { line, col } = range.end;
                let last_line = lines.get(line - 1).expect("There should be a last line");

                let cursor = format!("{}^", " ".repeat(2 * IND_AMT + col));
                let indent = Self::indent();
                let message = format!("{}|{}{}\n{}", indent, indent, last_line, cursor);

                format!("Unexpected end of file ({}:{})\n{}", line, col, message)
            }
            ParseError::ExpectedToken(expected, actual) => {
                let pos = actual.get_position();
                let message = Self::get_underlined_line(lines, actual);

                // Convert from TokenType to Token, to make use of the #[strum(to_string)] meta,
                // since strum doesn't apply the #[strum(to_string)] to the discriminants.
                let expected: Token = Token::from_str(&expected.to_string()).unwrap();
                format!("Expected token '{}', saw '{}' ({}:{})\n{}", expected.to_string(), actual.to_string(), pos.line, pos.col, message)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ParseError;
    use crate::lexer::tokens::{Token, TokenType, Position, Range};
    use crate::common::display_error::DisplayError;

    #[test]
    fn test_unexpected_token_error() {
        let src = "-+".to_string();
        let token = Token::Plus(Position::new(1, 2));
        let err = ParseError::UnexpectedToken(token);

        let expected = format!("\
Unexpected token '+' (1:2)
  |  -+
      ^");
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_expected_token_error() {
        let src = "val a: = 123".to_string();
        let err = ParseError::ExpectedToken(
            TokenType::Ident,
            Token::Assign(Position::new(1, 8)),
        );

        let expected = format!("\
Expected token 'identifier', saw '=' (1:8)
  |  val a: = 123
            ^");
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_unexpected_eof_error() {
        let src = "-".to_string();
        let err = ParseError::UnexpectedEof(Range::with_length(&Position::new(1, 1), 1));

        let expected = format!("\
Unexpected end of file (1:2)
  |  -
      ^");
        assert_eq!(expected, err.get_message(&src));
    }
}
