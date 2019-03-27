use crate::common::display_error::{DisplayError, IND_AMT};
use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;
use crate::parser::ast::BinaryOp;

#[derive(Debug, PartialEq)]
pub enum TypecheckerError {
    Mismatch { token: Token, expected: Type, actual: Type },
    InvalidOperator { token: Token, op: BinaryOp, ltype: Type, rtype: Type },
}

// TODO: Replace this when I do more work on Type representations
fn type_repr(t: &Type) -> String {
    match t {
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::String => "String".to_string(),
        Type::Or(options) => {
            let type_opts: Vec<String> = options.iter()
                .map(|t| type_repr(t))
                .collect();
            format!("one of ({})", type_opts.join(", "))
        }
    }
}

// TODO: Replace this
fn op_repr(op: &BinaryOp) -> String {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
    }.to_string()
}

impl DisplayError for TypecheckerError {
    fn message_for_error(&self, lines: &Vec<&str>) -> String {
        let pos = match self {
            TypecheckerError::Mismatch { token, .. } => token.get_position(),
            TypecheckerError::InvalidOperator { token, .. } => token.get_position(),
        };
        let line = lines.get(pos.line - 1).expect("There should be a line");

        let cursor = Self::get_cursor(2 * IND_AMT + pos.col);
        let indent = Self::indent();
        let cursor_line = format!("{}|{}{}\n{}", indent, indent, line, cursor);

        match self {
            TypecheckerError::Mismatch { expected, actual, .. } => {
                let message = format!("{}Expected {}, got {}", indent, type_repr(expected), type_repr(actual));

                format!("Type mismatch ({}:{})\n{}\n{}", pos.line, pos.col, cursor_line, message)
            }
            TypecheckerError::InvalidOperator { op, ltype, rtype, .. } => {
                let message = format!(
                    "{}No operator exists to satisfy {} {} {}",
                    indent, type_repr(ltype), op_repr(op), type_repr(rtype)
                );

                format!("Invalid operator ({}:{})\n{}\n{}", pos.line, pos.col, cursor_line, message)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TypecheckerError;
    use crate::lexer::tokens::{Token, Position};
    use crate::typechecker::types::Type;
    use crate::common::display_error::DisplayError;
    use crate::parser::ast::BinaryOp;

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
        let err = TypecheckerError::Mismatch { token, expected: Type::Or(vec![Type::Int, Type::Float]), actual: Type::Int };

        let expected = format!("\
Type mismatch (1:5)
  |  1 + 4.4
         ^
  Expected one of (Int, Float), got Int"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_invalid_operator() {
        let src = "1 - \"some string\"".to_string();
        let token = Token::Minus(Position::new(1, 3));
        let err = TypecheckerError::InvalidOperator { token, op: BinaryOp::Sub, ltype: Type::Int, rtype: Type::String };

        let expected = format!("\
Invalid operator (1:3)
  |  1 - \"some string\"
       ^
  No operator exists to satisfy Int - String"
        );
        assert_eq!(expected, err.get_message(&src));
    }
}