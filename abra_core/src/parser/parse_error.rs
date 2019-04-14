use crate::lexer::tokens::Token;
use crate::common::display_error::{DisplayError, IND_AMT};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedEof,
    UnexpectedToken(Token),
    Raw(String),
}

impl DisplayError for ParseError {
    fn message_for_error(&self, lines: &Vec<&str>) -> String {
        match self {
            ParseError::UnexpectedToken(token) => {
                let pos = token.get_position();
                let line = lines.get(pos.line - 1).expect("There should be a line");

                let cursor = Self::get_cursor(2 * IND_AMT + pos.col);
                let indent = Self::indent();
                let message = format!("{}|{}{}\n{}", indent, indent, line, cursor);

                format!("Unexpected token '{}' ({}:{})\n{}", token.to_string(), pos.line, pos.col, message)
            }
            ParseError::UnexpectedEof => {
                let last_line = lines.last().expect("There should be a last line");
                let line_num = lines.len();
                let col_num = last_line.len() + 1;

                let cursor = Self::get_cursor(2 * IND_AMT + col_num);
                let indent = Self::indent();
                let message = format!("{}|{}{}\n{}", indent, indent, last_line, cursor);

                format!("Unexpected end of file ({}:{})\n{}", line_num, col_num, message)
            }
            ParseError::Raw(msg) => format!("{}", msg)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ParseError;
    use crate::lexer::tokens::{Token, Position};
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
    fn test_unexpected_eof_error() {
        let src = "-".to_string();
        let err = ParseError::UnexpectedEof;

        let expected = format!("\
Unexpected end of file (1:2)
  |  -
      ^");
        assert_eq!(expected, err.get_message(&src));
    }
}
