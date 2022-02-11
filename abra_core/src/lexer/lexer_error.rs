use crate::lexer::tokens::{Position, Range};
use crate::common::display_error::{DisplayError, IND_AMT};
use crate::parser::ast::ModuleId;

#[derive(Debug, PartialEq)]
pub struct LexerError {
    pub module_id: ModuleId,
    pub kind: LexerErrorKind,
}

#[derive(Debug, PartialEq)]
pub enum LexerErrorKind {
    UnexpectedChar(Position, String),
    // Store position of string start and its invalid termination
    UnterminatedString(Position, Position),
    UnexpectedEof(Position),
    UnsupportedEscapeSequence(Position, String, /* is_unicode: */ bool),
}

impl LexerError {
    pub fn get_range(&self) -> Range {
        match &self.kind {
            LexerErrorKind::UnexpectedChar(pos, _) => Range::with_length(pos, 0),
            LexerErrorKind::UnterminatedString(start, end) => Range { start: start.clone(), end: end.clone() },
            LexerErrorKind::UnexpectedEof(pos) => Range::with_length(pos, 0),
            LexerErrorKind::UnsupportedEscapeSequence(pos, s, _) => Range::with_length(pos, s.len() - 1)
        }
    }
}

fn get_cursor(left_padding: usize) -> String {
    format!("{}^", " ".repeat(left_padding))
}

impl DisplayError for LexerError {
    fn message_for_error(&self, file_name: &String, lines: &Vec<&str>) -> String {
        match &self.kind {
            LexerErrorKind::UnexpectedChar(pos, string) => {
                let cursor_line = Self::get_underlined_line_no_token(lines, pos, &self.get_range());

                format!(
                    "Error at {}:{}:{}\nUnexpected character '{}':\n{}",
                    file_name, pos.line, pos.col, string, cursor_line
                )
            }
            LexerErrorKind::UnterminatedString(start_pos, end_pos) => {
                let start_line = lines.get(start_pos.line - 1).expect("There should be a line");
                let end_line = lines.get(end_pos.line - 1).expect("There should be a line");

                let indent = Self::indent();
                let start_message = {
                    let cursor = get_cursor(2 * IND_AMT + start_pos.col);
                    format!("{}String begins at ({}:{})\n{}|{}{}\n{}", indent, start_pos.line, start_pos.col, indent, indent, start_line, cursor)
                };
                let end_message = {
                    let cursor = get_cursor(2 * IND_AMT + end_pos.col);
                    format!("{}String is terminated at ({}:{})\n{}|{}{}\n{}", indent, end_pos.line, end_pos.col, indent, indent, end_line, cursor)
                };

                format!(
                    "Error at {}:{}:{}\nUnterminated string:\n{}\n{}",
                    file_name, end_pos.line, end_pos.col, start_message, end_message
                )
            }
            LexerErrorKind::UnexpectedEof(pos) => {
                let cursor_line = Self::get_underlined_line_no_token(lines, pos, &self.get_range());

                format!(
                    "Error at {}:{}:{}\nUnexpected end of file:\n{}",
                    file_name, pos.line, pos.col, cursor_line
                )
            }
            LexerErrorKind::UnsupportedEscapeSequence(pos, _, is_unicode) => {
                let cursor_line = Self::get_underlined_line_no_token(lines, pos, &self.get_range());

                let msg = if *is_unicode {
                    "\nUnicode escape sequences must be \\u followed by 4 hexadecimal characters (between 0000 and 7FFF)"
                } else { "" };

                format!(
                    "Error at {}:{}:{}\nUnsupported escape sequence:\n{}{}",
                    file_name, pos.line, pos.col, cursor_line, msg
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer_error::{LexerErrorKind, LexerError};
    use crate::lexer::tokens::Position;
    use crate::common::display_error::DisplayError;
    use crate::parser::ast::ModuleId;

    #[test]
    fn test_unexpected_char_error() {
        let module_id = ModuleId::from_name("test");
        let src = "1..23".to_string();
        let err = LexerError { module_id, kind: LexerErrorKind::UnexpectedChar(Position::new(1, 3), ".".to_string()) };

        let expected = format!("\
Error at ./tests/test.abra:1:3
Unexpected character '.':
  |  1..23
       ^");
        assert_eq!(expected, err.get_message(&"./tests/test.abra".to_string(), &src));
    }

    #[test]
    fn test_unterminated_string_error() {
        let module_id = ModuleId::from_name("test");
        let src = "\"this is a string\n".to_string();
        let err = LexerError { module_id, kind: LexerErrorKind::UnterminatedString(Position::new(1, 1), Position::new(1, 18)) };

        let expected = format!("\
Error at ./tests/test.abra:1:18
Unterminated string:
  String begins at (1:1)
  |  \"this is a string
     ^
  String is terminated at (1:18)
  |  \"this is a string
                      ^");
        assert_eq!(expected, err.get_message(&"./tests/test.abra".to_string(), &src));
    }

    #[test]
    fn test_unexpected_eof_error() {
        let module_id = ModuleId::from_name("test");
        let src = "1.".to_string();
        let err = LexerError { module_id, kind: LexerErrorKind::UnexpectedEof(Position::new(1, 3)) };

        let expected = format!("\
Error at ./tests/test.abra:1:3
Unexpected end of file:
  |  1.
       ^");
        assert_eq!(expected, err.get_message(&"./tests/test.abra".to_string(), &src));
    }

    #[test]
    fn test_unsupported_escape_sequence() {
        let module_id = ModuleId::from_name("./test");
        let src = "\"a\\qb\"".to_string();
        let err = LexerError { module_id, kind: LexerErrorKind::UnsupportedEscapeSequence(Position::new(1, 2), "\\q".to_string(), false) };

        let expected = format!("\
Error at ./tests/test.abra:1:2
Unsupported escape sequence:
  |  \"a\\qb\"
      ^^");
        assert_eq!(expected, err.get_message(&"./tests/test.abra".to_string(), &src));
    }
}
