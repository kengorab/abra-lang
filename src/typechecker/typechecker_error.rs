use crate::common::display_error::{DisplayError, IND_AMT};
use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;

#[derive(Debug, PartialEq)]
pub enum TypecheckerError {
    Mismatch { token: Token, expected: Type, actual: Type },
    Placeholder,
}

// TODO: Replace this when I do more work on Type representations
fn type_repr(t: &Type) -> String {
    match t {
        Type::Float => "Float".to_string(),
        Type::Int => "Int".to_string(),
        Type::Or(options) => {
            let type_opts: Vec<String> = options.iter()
                .map(|t| type_repr(t))
                .collect();
            format!("one of ({})", type_opts.join(", "))
        }
        Type::Unknown => "Unknown".to_string()
    }
}

impl DisplayError for TypecheckerError {
    fn message_for_error(&self, lines: &Vec<&str>) -> String {
        match self {
            TypecheckerError::Mismatch { token, expected, actual } => {
                let pos = token.get_position();
                let line = lines.get(pos.line - 1).expect("There should be a line");

                let cursor = Self::get_cursor(2 * IND_AMT + pos.col);
                let indent = Self::indent();

                let cursor_line = format!("{}|{}{}\n{}", indent, indent, line, cursor);
                let message = format!("{}Expected {}, got {}", indent, type_repr(expected), type_repr(actual));

                format!("Type mismatch ({}:{})\n{}\n{}", pos.line, pos.col, cursor_line, message)
            }
            _ => "".to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TypecheckerError;
    use crate::lexer::tokens::{Token, Position};
    use crate::typechecker::types::Type;
    use crate::common::display_error::DisplayError;

    #[test]
    fn test_mismatch_error() {
        let src = "1 + 4.4".to_string();
        let token = Token::Float(Position::new(1, 5), 4.4);
        let err = TypecheckerError::Mismatch { token, expected: Type::Int, actual: Type::Float };

        let expected = format!("\
Type mismatch (1:5)
  |  1 + 4.4
         ^
  Expected Int, got Float"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_mismatch_error_with_ortype() {
        let src = "1 + 4.4".to_string();
        let token = Token::Float(Position::new(1, 5), 4.4);
        let err = TypecheckerError::Mismatch { token, expected: Type::Or(vec![Type::Int, Type::Float]), actual: Type::Unknown };

        let expected = format!("\
Type mismatch (1:5)
  |  1 + 4.4
         ^
  Expected one of (Int, Float), got Unknown"
        );
        assert_eq!(expected, err.get_message(&src));
    }
}