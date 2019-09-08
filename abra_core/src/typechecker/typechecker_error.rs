use crate::common::display_error::{DisplayError, IND_AMT};
use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;
use crate::parser::ast::BinaryOp;

#[derive(Debug, PartialEq)]
pub enum TypecheckerError {
    Mismatch { token: Token, expected: Type, actual: Type },
    InvalidOperator { token: Token, op: BinaryOp, ltype: Type, rtype: Type },
    MissingRequiredAssignment { ident: Token },
    DuplicateBinding { ident: Token, orig_ident: Token },
    DuplicateType { ident: Token, orig_ident: Option<Token> },
    UnknownIdentifier { ident: Token },
    InvalidAssignmentTarget { token: Token },
    AssignmentToImmutable { orig_ident: Token, token: Token },
    UnannotatedUninitialized { ident: Token, is_mutable: bool },
    UnknownType { type_ident: Token },
    MissingIfExprBranch { if_token: Token, is_if_branch: bool },
    IfExprBranchMismatch { if_token: Token, if_type: Type, else_type: Type },
    InvalidInvocationTarget { token: Token },
    IncorrectArity { token: Token, expected: usize, actual: usize },
    ParamNameMismatch { token: Token, expected: String, actual: String },
    RecursiveRefWithoutReturnType { orig_token: Token, token: Token },
    InvalidBreak(Token),
    InvalidRequiredArgPosition(Token),
}

// TODO: Replace this when I do more work on Type representations
fn type_repr(t: &Type) -> String {
    match t {
        Type::Unit => "()".to_string(),
        Type::Any => "Any".to_string(),
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::String => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Or(options) => {
            let type_opts: Vec<String> = options.iter()
                .map(|t| type_repr(t))
                .collect();
            format!("one of ({})", type_opts.join(", "))
        }
        Type::Array(typ) => format!("{}[]", type_repr(typ)),
        Type::Option(typ) => format!("{}?", type_repr(typ)),
        Type::Fn(args, ret_type) => {
            let args = args.iter().map(|(_, arg_type, _)| type_repr(arg_type)).collect::<Vec<String>>().join(", ");
            format!("({}) => {}", args, type_repr(ret_type))
        }
        Type::Type(name, _) => name.to_string(),
        Type::Unknown => "Unknown".to_string(),
        Type::Struct { name, .. } => name.to_string(),
    }
}

// TODO: Replace this
fn op_repr(op: &BinaryOp) -> String {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Mod => "%",
        BinaryOp::And => "&&",
        BinaryOp::Coalesce => "?:",
        BinaryOp::Or => "||",
        BinaryOp::Lt => "<",
        BinaryOp::Lte => "<=",
        BinaryOp::Gt => ">",
        BinaryOp::Gte => ">=",
        BinaryOp::Neq => "!=",
        BinaryOp::Eq => "==",
    }.to_string()
}

impl DisplayError for TypecheckerError {
    fn message_for_error(&self, lines: &Vec<&str>) -> String {
        let pos = match self {
            TypecheckerError::Mismatch { token, .. } => token.get_position(),
            TypecheckerError::InvalidOperator { token, .. } => token.get_position(),
            TypecheckerError::MissingRequiredAssignment { ident } => ident.get_position(),
            TypecheckerError::DuplicateBinding { ident, .. } => ident.get_position(),
            TypecheckerError::DuplicateType { ident, .. } => ident.get_position(),
            TypecheckerError::UnknownIdentifier { ident } => ident.get_position(),
            TypecheckerError::InvalidAssignmentTarget { token } => token.get_position(),
            TypecheckerError::AssignmentToImmutable { token, .. } => token.get_position(),
            TypecheckerError::UnannotatedUninitialized { ident, .. } => ident.get_position(),
            TypecheckerError::UnknownType { type_ident } => type_ident.get_position(),
            TypecheckerError::MissingIfExprBranch { if_token, .. } => if_token.get_position(),
            TypecheckerError::IfExprBranchMismatch { if_token, .. } => if_token.get_position(),
            TypecheckerError::InvalidInvocationTarget { token } => token.get_position(),
            TypecheckerError::IncorrectArity { token, .. } => token.get_position(),
            TypecheckerError::ParamNameMismatch { token, .. } => token.get_position(),
            TypecheckerError::RecursiveRefWithoutReturnType { token, .. } => token.get_position(),
            TypecheckerError::InvalidBreak(token) => token.get_position(),
            TypecheckerError::InvalidRequiredArgPosition(token) => token.get_position(),
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
            TypecheckerError::MissingRequiredAssignment { ident } => {
                let ident = Token::get_ident_name(&ident);
                let message = format!("'val' bindings must be initialized");
                format!(
                    "Expected assignment for variable '{}' ({}:{})\n{}\n{}",
                    ident, pos.line, pos.col, cursor_line, message
                )
            }
            TypecheckerError::DuplicateBinding { ident, orig_ident } => {
                let ident = Token::get_ident_name(&ident);
                let first_msg = format!("Duplicate variable '{}' ({}:{})\n{}", ident, pos.line, pos.col, cursor_line);

                let pos = orig_ident.get_position();
                let line = lines.get(pos.line - 1).expect("There should be a line");

                let cursor = Self::get_cursor(2 * IND_AMT + pos.col);
                let cursor_line = format!("{}|{}{}\n{}", indent, indent, line, cursor);

                let second_msg = format!("Binding already declared in scope at ({}:{})\n{}", pos.line, pos.col, cursor_line);

                format!("{}\n{}", first_msg, second_msg)
            }
            TypecheckerError::DuplicateType { ident: _, orig_ident: _ } => { // orig_ident will be None if it's a builtin type
//                let ident = Token::get_ident_name(&ident);
//                let first_msg = format!("Duplicate type '{}' ({}:{})\n{}", ident, pos.line, pos.col, cursor_line);
//
//                let pos = orig_ident.get_position();
//                let line = lines.get(pos.line - 1).expect("There should be a line");
//
//                let cursor = Self::get_cursor(2 * IND_AMT + pos.col);
//                let cursor_line = format!("{}|{}{}\n{}", indent, indent, line, cursor);
//
//                let second_msg = format!("Type already declared in scope at ({}:{})\n{}", pos.line, pos.col, cursor_line);
//
//                format!("{}\n{}", first_msg, second_msg)
                unimplemented!()
            }
            TypecheckerError::UnknownIdentifier { ident } => {
                let ident = Token::get_ident_name(&ident);
                format!(
                    "Unknown identifier '{}' ({}:{})\n{}\nNo binding with that name is visible in current scope",
                    ident, pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::InvalidAssignmentTarget { token: _ } => {
                let msg = "Left-hand side of assignment must be a valid identifier";
                format!(
                    "Cannot perform assignment ({}:{})\n{}\n{}",
                    pos.line, pos.col, cursor_line, msg
                )
            }
            TypecheckerError::AssignmentToImmutable { orig_ident, token: _ } => {
                let ident = Token::get_ident_name(&orig_ident);
                let first_msg = format!("Cannot assign to variable '{}' ({}:{})\n{}", ident, pos.line, pos.col, cursor_line);

                let pos = orig_ident.get_position();
                let line = lines.get(pos.line - 1).expect("There should be a line");

                let cursor = Self::get_cursor(2 * IND_AMT + pos.col);
                let cursor_line = format!("{}|{}{}\n{}", indent, indent, line, cursor);

                let second_msg = format!("The binding has been declared in scope as immutable at ({}:{})\n{}", pos.line, pos.col, cursor_line);

                format!("{}\n{}\nUse 'var' instead of 'val' to create a mutable binding", first_msg, second_msg)
            }
            TypecheckerError::UnannotatedUninitialized { ident, is_mutable } => {
                let ident = Token::get_ident_name(&ident);
                let msg = if *is_mutable {
                    "Since it's a 'var', you can either provide an initial value or a type annotation"
                } else {
                    "Since it's a 'val', you must provide an initial value"
                };

                let modifier = if *is_mutable { "mutable" } else { "immutable" };
                format!(
                    "Could not determine type of {} variable '{}' ({}:{})\n{}\n{}",
                    modifier, ident, pos.line, pos.col, cursor_line, msg
                )
            }
            TypecheckerError::UnknownType { type_ident } => {
                let ident = Token::get_ident_name(type_ident);
                format!(
                    "Unknown type '{}' ({}:{})\n{}\nNo type with that name is visible in current scope",
                    ident, pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::MissingIfExprBranch { if_token: _, is_if_branch } => {
                format!(
                    "Missing {}-branch in if-else expression: ({}:{})\n{}\n\
                    Both branches must have some value when used as an expression",
                    if *is_if_branch { "if" } else { "else" }, pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::IfExprBranchMismatch { if_token: _, if_type, else_type } => {
                format!(
                    "Type mismatch between the if-else expression branches: ({}:{})\n{}\n\
                    The if-branch had type {}, but the else-branch had type {}",
                    pos.line, pos.col, cursor_line, type_repr(if_type), type_repr(else_type)
                )
            }
            TypecheckerError::InvalidInvocationTarget { token: _ } => {
                unimplemented!()
            }
            TypecheckerError::IncorrectArity { token: _, expected: _, actual: _ } => {
                unimplemented!()
            }
            TypecheckerError::ParamNameMismatch { token: _, expected: _, actual: _ } => {
                unimplemented!()
            }
            TypecheckerError::RecursiveRefWithoutReturnType { orig_token, token: _ } => {
                let secondary_pos = orig_token.get_position();
                let line = lines.get(secondary_pos.line - 1).expect("There should be a line");

                let cursor = Self::get_cursor(2 * IND_AMT + secondary_pos.col);
                let secondary_cursor_line = format!("{}|{}{}\n{}", indent, indent, line, cursor);

                format!(
                    "Missing return type declaration: ({}:{})\n{}\n\
                    Functions that contain recursive references must explicitly declare their return type\n\
                    Missing return type annotation here ({}:{}):\n{}",
                    pos.line, pos.col, cursor_line,
                    secondary_pos.line, secondary_pos.col, secondary_cursor_line
                )
            }
            TypecheckerError::InvalidBreak(_token) => {
                unimplemented!()
            }
            TypecheckerError::InvalidRequiredArgPosition(_token) => {
                unimplemented!()
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

    #[test]
    fn test_missing_required_assignment() {
        let src = "val abc".to_string();
        let err = TypecheckerError::MissingRequiredAssignment {
            ident: Token::Ident(Position::new(1, 5), "abc".to_string())
        };

        let expected = format!("\
Expected assignment for variable 'abc' (1:5)
  |  val abc
         ^
'val' bindings must be initialized"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_duplicate_binding() {
        let src = "val abc = 123\nval abc = 5".to_string();
        let err = TypecheckerError::DuplicateBinding {
            ident: Token::Ident(Position::new(2, 5), "abc".to_string()),
            orig_ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
        };

        let expected = format!("\
Duplicate variable 'abc' (2:5)
  |  val abc = 5
         ^
Binding already declared in scope at (1:5)
  |  val abc = 123
         ^"
        );

        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_unknown_identifier() {
        let src = "abcd".to_string();
        let err = TypecheckerError::UnknownIdentifier {
            ident: Token::Ident(Position::new(1, 1), "abcd".to_string())
        };

        let expected = format!("\
Unknown identifier 'abcd' (1:1)
  |  abcd
     ^
No binding with that name is visible in current scope"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_unannotated_and_uninitialized() {
        let src = "var abcd".to_string();
        let err = TypecheckerError::UnannotatedUninitialized {
            ident: Token::Ident(Position::new(1, 5), "abcd".to_string()),
            is_mutable: true,
        };

        let expected = format!("\
Could not determine type of mutable variable 'abcd' (1:5)
  |  var abcd
         ^
Since it's a 'var', you can either provide an initial value or a type annotation"
        );
        assert_eq!(expected, err.get_message(&src));

        let src = "val abcd".to_string();
        let err = TypecheckerError::UnannotatedUninitialized {
            ident: Token::Ident(Position::new(1, 5), "abcd".to_string()),
            is_mutable: false,
        };

        let expected = format!("\
Could not determine type of immutable variable 'abcd' (1:5)
  |  val abcd
         ^
Since it's a 'val', you must provide an initial value"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_invalid_assignment_target() {
        let src = "true = \"abc\"".to_string();
        let err = TypecheckerError::InvalidAssignmentTarget {
            token: Token::Assign(Position::new(1, 6))
        };

        let expected = format!("\
Cannot perform assignment (1:6)
  |  true = \"abc\"
          ^
Left-hand side of assignment must be a valid identifier"
        );

        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_assignment_to_immutable() {
        let src = "val abc = 1\n\nabc = 3".to_string();
        let err = TypecheckerError::AssignmentToImmutable {
            orig_ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
            token: Token::Assign(Position::new(3, 5)),
        };

        let expected = format!("\
Cannot assign to variable 'abc' (3:5)
  |  abc = 3
         ^
The binding has been declared in scope as immutable at (1:5)
  |  val abc = 1
         ^
Use 'var' instead of 'val' to create a mutable binding"
        );

        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_unknown_type() {
        let src = "val abcd: NonExistentType = 432".to_string();
        let err = TypecheckerError::UnknownType {
            type_ident: Token::Ident(Position::new(1, 11), "NonExistentType".to_string())
        };

        let expected = format!("\
Unknown type 'NonExistentType' (1:11)
  |  val abcd: NonExistentType = 432
               ^
No type with that name is visible in current scope"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_missing_if_expr_branch() {
        let src = "val a = if (true) {} else 123".to_string();
        let err = TypecheckerError::MissingIfExprBranch {
            if_token: Token::If(Position::new(1, 9)),
            is_if_branch: true,
        };

        let expected = format!("\
Missing if-branch in if-else expression: (1:9)
  |  val a = if (true) {{}} else 123
             ^
Both branches must have some value when used as an expression"
        );
        assert_eq!(expected, err.get_message(&src));

        let src = "val a = if (true) 123 else {}".to_string();
        let err = TypecheckerError::MissingIfExprBranch {
            if_token: Token::If(Position::new(1, 9)),
            is_if_branch: false,
        };

        let expected = format!("\
Missing else-branch in if-else expression: (1:9)
  |  val a = if (true) 123 else {{}}
             ^
Both branches must have some value when used as an expression"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_if_expr_branch_mismatch() {
        let src = "val a = if (true) \"hello\" else 123".to_string();
        let err = TypecheckerError::IfExprBranchMismatch {
            if_token: Token::If(Position::new(1, 9)),
            if_type: Type::String,
            else_type: Type::Int,
        };

        let expected = format!("\
Type mismatch between the if-else expression branches: (1:9)
  |  val a = if (true) \"hello\" else 123
             ^
The if-branch had type String, but the else-branch had type Int"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_recursive_call_no_return_type() {
        let src = "func abc() {\nabc()\n}".to_string();
        let err = TypecheckerError::RecursiveRefWithoutReturnType {
            orig_token: Token::Ident(Position::new(1, 6), "abc".to_string()),
            token: Token::Ident(Position::new(2, 1), "abc".to_string()),
        };

        let expected = format!("\
Missing return type declaration: (2:1)
  |  abc()
     ^
Functions that contain recursive references must explicitly declare their return type
Missing return type annotation here (1:6):
  |  func abc() {{
          ^"
        );
        assert_eq!(expected, err.get_message(&src));
    }
}
