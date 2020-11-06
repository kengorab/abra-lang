use crate::common::display_error::DisplayError;
use crate::lexer::tokens::Token;
use crate::typechecker::types::{Type, StructType, FnType, EnumType};
use crate::parser::ast::BinaryOp;

#[derive(Debug, PartialEq)]
pub enum InvalidAssignmentTargetReason {
    IndexingMode,
    StringTarget,
}

#[derive(Debug, PartialEq)]
pub enum TypecheckerError {
    Mismatch { token: Token, expected: Type, actual: Type },
    InvalidIfConditionType { token: Token, actual: Type },
    InvalidOperator { token: Token, op: BinaryOp, ltype: Type, rtype: Type },
    MissingRequiredAssignment { ident: Token },
    DuplicateBinding { ident: Token, orig_ident: Token },
    DuplicateField { ident: Token, orig_ident: Token, orig_is_field: bool, orig_is_enum_variant: bool },
    DuplicateType { ident: Token, orig_ident: Option<Token> },
    DuplicateTypeArgument { ident: Token, orig_ident: Token },
    UnboundGeneric(Token, String),
    UnknownIdentifier { ident: Token },
    InvalidAssignmentTarget { token: Token, reason: Option<InvalidAssignmentTargetReason> },
    AssignmentToImmutable { orig_ident: Token, token: Token },
    UnannotatedUninitialized { ident: Token, is_mutable: bool },
    UnknownType { type_ident: Token },
    MissingIfExprBranch { if_token: Token, is_if_branch: bool },
    IfExprBranchMismatch { if_token: Token, if_type: Type, else_type: Type },
    InvalidInvocationTarget { token: Token, target_type: Type },
    IncorrectArity { token: Token, expected: usize, actual: usize },
    UnexpectedParamName { token: Token },
    DuplicateParamName { token: Token },
    RecursiveRefWithoutReturnType { orig_token: Token, token: Token },
    InvalidBreak(Token),
    InvalidRequiredArgPosition(Token),
    InvalidIndexingTarget { token: Token, target_type: Type },
    InvalidIndexingSelector { token: Token, target_type: Type, selector_type: Type },
    UnknownMember { token: Token, target_type: Type },
    MissingRequiredParams { token: Token, missing_params: Vec<String> },
    InvalidMixedParamType { token: Token },
    InvalidTypeFuncInvocation { token: Token },
    InvalidSelfParamPosition { token: Token },
    InvalidSelfParam { token: Token },
    MissingRequiredTypeAnnotation { token: Token },
    InvalidTypeDeclDepth { token: Token },
    ForbiddenVariableType { token: Token, typ: Type },
    InvalidInstantiation { token: Token, typ: Type },
    InvalidTypeArgumentArity { token: Token, expected: usize, actual: usize, actual_type: Type },
    UnreachableMatchCase { token: Token, typ: Option<Type>, is_unreachable_none: bool },
    DuplicateMatchCase { token: Token },
    NonExhaustiveMatch { token: Token },
    EmptyMatchBlock { token: Token },
    MatchBranchMismatch { token: Token, expected: Type, actual: Type },
}

impl TypecheckerError {
    pub fn get_token(&self) -> &Token {
        match self {
            TypecheckerError::Mismatch { token, .. } => token,
            TypecheckerError::InvalidIfConditionType { token, .. } => token,
            TypecheckerError::InvalidOperator { token, .. } => token,
            TypecheckerError::MissingRequiredAssignment { ident } => ident,
            TypecheckerError::DuplicateBinding { ident, .. } => ident,
            TypecheckerError::DuplicateType { ident, .. } => ident,
            TypecheckerError::DuplicateTypeArgument { ident, .. } => ident,
            TypecheckerError::UnboundGeneric(token, _) => token,
            TypecheckerError::DuplicateField { ident, .. } => ident,
            TypecheckerError::UnknownIdentifier { ident } => ident,
            TypecheckerError::InvalidAssignmentTarget { token, .. } => token,
            TypecheckerError::AssignmentToImmutable { token, .. } => token,
            TypecheckerError::UnannotatedUninitialized { ident, .. } => ident,
            TypecheckerError::UnknownType { type_ident } => type_ident,
            TypecheckerError::MissingIfExprBranch { if_token, .. } => if_token,
            TypecheckerError::IfExprBranchMismatch { if_token, .. } => if_token,
            TypecheckerError::InvalidInvocationTarget { token, .. } => token,
            TypecheckerError::IncorrectArity { token, .. } => token,
            TypecheckerError::UnexpectedParamName { token } => token,
            TypecheckerError::DuplicateParamName { token } => token,
            TypecheckerError::RecursiveRefWithoutReturnType { token, .. } => token,
            TypecheckerError::InvalidBreak(token) => token,
            TypecheckerError::InvalidRequiredArgPosition(token) => token,
            TypecheckerError::InvalidIndexingTarget { token, .. } => token,
            TypecheckerError::InvalidIndexingSelector { token, .. } => token,
            TypecheckerError::UnknownMember { token, .. } => token,
            TypecheckerError::MissingRequiredParams { token, .. } => token,
            TypecheckerError::InvalidMixedParamType { token } => token,
            TypecheckerError::InvalidTypeFuncInvocation { token } => token,
            TypecheckerError::InvalidSelfParamPosition { token } => token,
            TypecheckerError::InvalidSelfParam { token } => token,
            TypecheckerError::MissingRequiredTypeAnnotation { token } => token,
            TypecheckerError::InvalidTypeDeclDepth { token } => token,
            TypecheckerError::ForbiddenVariableType { token, .. } => token,
            TypecheckerError::InvalidInstantiation { token, .. } => token,
            TypecheckerError::InvalidTypeArgumentArity { token, .. } => token,
            TypecheckerError::UnreachableMatchCase { token, .. } => token,
            TypecheckerError::DuplicateMatchCase { token, .. } => token,
            TypecheckerError::NonExhaustiveMatch { token } => token,
            TypecheckerError::EmptyMatchBlock { token } => token,
            TypecheckerError::MatchBranchMismatch { token, .. } => token,
        }
    }
}

// TODO: Replace this when I do more work on Type representations
fn type_repr(t: &Type) -> String {
    #[inline]
    fn wrap_type_repr(t: &Type) -> String {
        let wrap = if let Type::Fn(_) = t { true } else { false };

        if wrap {
            format!("({})", type_repr(t))
        } else {
            type_repr(t)
        }
    }

    match t {
        Type::Unit => "Unit".to_string(),
        Type::Any => "Any".to_string(),
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::String => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Union(options) => {
            let type_opts: Vec<String> = options.iter()
                .map(|t| wrap_type_repr(t))
                .collect();
            format!("{}", type_opts.join(" | "))
        }
        Type::Array(typ) => format!("{}[]", wrap_type_repr(typ)),
        Type::Map(fields, _) => {
            if fields.is_empty() {
                format!("{{}}")
            } else {
                let pairs = fields.iter()
                    .map(|(name, typ)| format!("{}: {}", name, type_repr(typ)))
                    .collect::<Vec<String>>().join(", ");
                format!("{{ {} }}", pairs)
            }
        }
        Type::Option(typ) => format!("{}?", wrap_type_repr(typ)),
        Type::Fn(FnType { arg_types, ret_type, .. }) => {
            let args = arg_types.iter().map(|(_, arg_type, _)| type_repr(arg_type)).collect::<Vec<String>>().join(", ");
            format!("({}) => {}", args, type_repr(ret_type))
        }
        Type::Type(name, _, _) => name.to_string(),
        Type::Unknown => "Unknown".to_string(),
        Type::Struct(StructType { name, type_args, .. }) => {
            if type_args.is_empty() { return name.clone(); }

            let type_args_repr = type_args.iter()
                .map(|(_, typ)| type_repr(typ))
                .collect::<Vec<String>>()
                .join(", ");
            format!("{}<{}>", name, type_args_repr)
        }
        Type::Enum(EnumType { name, .. }) => {
            format!("{}", name)
        }
        Type::Placeholder => "_".to_string(),
        Type::Generic(name) => name.clone(),
        Type::Reference(name, type_args) => {
            if type_args.is_empty() { return name.clone(); }

            let type_args_repr = type_args.iter()
                .map(|typ| type_repr(typ))
                .collect::<Vec<String>>()
                .join(", ");
            format!("{}<{}>", name, type_args_repr)
        }
    }
}

// TODO: Replace this
fn op_repr(op: &BinaryOp) -> String {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::AddEq => "+=",
        BinaryOp::Sub => "-",
        BinaryOp::SubEq => "-=",
        BinaryOp::Mul => "*",
        BinaryOp::MulEq => "*=",
        BinaryOp::Div => "/",
        BinaryOp::DivEq => "/=",
        BinaryOp::Mod => "%",
        BinaryOp::ModEq => "%=",
        BinaryOp::And => "&&",
        BinaryOp::AndEq => "&&=",
        BinaryOp::Or => "||",
        BinaryOp::OrEq => "||=",
        BinaryOp::Coalesce => "?:",
        BinaryOp::CoalesceEq => "?:=",
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
        let pos = self.get_token().get_position();
        let indent = Self::indent();

        let cursor_line = Self::get_underlined_line(lines, self.get_token());

        match self {
            TypecheckerError::Mismatch { expected, actual, .. } => {
                let message = format!("{}Expected {}, got {}", indent, type_repr(expected), type_repr(actual));

                format!("Type mismatch ({}:{})\n{}\n{}", pos.line, pos.col, cursor_line, message)
            }
            TypecheckerError::InvalidIfConditionType { actual, .. } => {
                dbg!(actual);
                unreachable!()
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
                let cursor_line = Self::get_underlined_line(lines, orig_ident);
                let second_msg = format!("Binding already declared in scope at ({}:{})\n{}", pos.line, pos.col, cursor_line);

                format!("{}\n{}", first_msg, second_msg)
            }
            TypecheckerError::DuplicateField { ident, orig_ident, orig_is_field, orig_is_enum_variant } => {
                let ident = Token::get_ident_name(&ident);
                let first_msg = format!("Duplicate field '{}' ({}:{})\n{}", ident, pos.line, pos.col, cursor_line);

                let pos = orig_ident.get_position();
                let cursor_line = Self::get_underlined_line(lines, orig_ident);

                let noun = if *orig_is_field { "Field" } else if *orig_is_enum_variant { "Enum variant" } else { "Method" };
                let second_msg = format!("{} with that name is already declared in scope at ({}:{})\n{}", noun, pos.line, pos.col, cursor_line);

                format!("{}\n{}", first_msg, second_msg)
            }
            TypecheckerError::DuplicateType { ident, orig_ident } => { // orig_ident will be None if it's a builtin type
                let ident = Token::get_ident_name(&ident);
                let first_msg = format!("Duplicate type '{}' ({}:{})\n{}", ident, pos.line, pos.col, cursor_line);

                let second_msg = match orig_ident {
                    Some(orig_ident) => {
                        let pos = orig_ident.get_position();
                        let cursor_line = Self::get_underlined_line(lines, orig_ident);

                        format!("Type already declared in scope at ({}:{})\n{}", pos.line, pos.col, cursor_line)
                    }
                    None => format!("'{}' already declared as built-in type", ident)
                };

                format!("{}\n{}", first_msg, second_msg)
            }
            TypecheckerError::DuplicateTypeArgument { ident, orig_ident } => { // orig_ident will be None if it's a builtin type
                let ident = Token::get_ident_name(&ident);
                let first_msg = format!("Duplicate type argument '{}' ({}:{})\n{}", ident, pos.line, pos.col, cursor_line);

                let pos = orig_ident.get_position();
                let cursor_line = Self::get_underlined_line(lines, orig_ident);

                let second_msg = format!("Type already declared in scope at ({}:{})\n{}", pos.line, pos.col, cursor_line);
                format!("{}\n{}", first_msg, second_msg)
            }
            TypecheckerError::UnboundGeneric(_, type_arg_ident) => {
                let first_msg = format!("Type argument '{}' is unbound ({}:{})\n{}", type_arg_ident, pos.line, pos.col, cursor_line);
                let second_msg = format!("There is not enough information to determine a possible value for '{}'", type_arg_ident);

                format!("{}\n{}", first_msg, second_msg)
            }
            TypecheckerError::UnknownIdentifier { ident } => {
                let ident = Token::get_ident_name(&ident);
                format!(
                    "Unknown identifier '{}' ({}:{})\n{}\nNo binding with that name is visible in current scope",
                    ident, pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::InvalidAssignmentTarget { reason: _, .. } => {
                // TODO: Use reason
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
                let cursor_line = Self::get_underlined_line(lines, orig_ident);
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
            TypecheckerError::InvalidInvocationTarget { target_type, .. } => {
                format!(
                    "Cannot call target as function: ({}:{})\n{}\n\
                    Type {} is not invokeable",
                    pos.line, pos.col, cursor_line, type_repr(target_type)
                )
            }
            TypecheckerError::IncorrectArity { expected, actual, .. } => {
                format!(
                    "Incorrect arity for invocation: ({}:{})\n{}\n\
                    Expected {} required argument{}, but {} were passed",
                    pos.line, pos.col, cursor_line,
                    expected, if *expected == 1 { "" } else { "s" }, actual
                )
            }
            TypecheckerError::UnexpectedParamName { token } => {
                let param_name = Token::get_ident_name(token);
                format!(
                    "Unexpected parameter name: ({}:{})\n{}\nThis function doesn't have a parameter called '{}'",
                    pos.line, pos.col, cursor_line, param_name,
                )
            }
            TypecheckerError::DuplicateParamName { .. } => {
                format!(
                    "Duplicate parameter name: ({}:{})\n{}\n\
                    A parameter of this name has already been passed",
                    pos.line, pos.col, cursor_line,
                )
            }
            TypecheckerError::RecursiveRefWithoutReturnType { orig_token, token: _ } => {
                let secondary_pos = orig_token.get_position();
                let secondary_cursor_line = Self::get_underlined_line(lines, orig_token);

                format!(
                    "Missing return type declaration: ({}:{})\n{}\n\
                    Functions that contain recursive references must explicitly declare their return type\n\
                    Missing return type annotation here ({}:{}):\n{}",
                    pos.line, pos.col, cursor_line,
                    secondary_pos.line, secondary_pos.col, secondary_cursor_line
                )
            }
            TypecheckerError::InvalidBreak(_token) => {
                format!(
                    "Unexpected break keyword: ({}:{})\n{}\nA break keyword cannot appear outside of a loop",
                    pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::InvalidRequiredArgPosition(_token) => {
                format!(
                    "Invalid position for non-optional parameter: ({}:{})\n{}\n\
                    Required parameters must all be listed before any optional parameters",
                    pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::InvalidIndexingTarget { target_type, .. } => {
                let first_line = format!(
                    "Unsupported indexing operation: ({}:{})\n{}",
                    pos.line, pos.col, cursor_line
                );
                let second_line = match target_type {
                    Type::Map(_, _) => format!("Cannot index into maps whose values are not all the same type"),
                    typ @ _ => format!("Type {} is not indexable", type_repr(typ))
                };
                format!("{}\n{}", first_line, second_line)
            }
            TypecheckerError::InvalidIndexingSelector { target_type, selector_type, .. } => {
                format!(
                    "Invalid type for indexing operator argument: ({}:{})\n{}\n\
                    Cannot index into a target of type {}, using a selector of type {}",
                    pos.line, pos.col, cursor_line, type_repr(target_type), type_repr(selector_type)
                )
            }
            TypecheckerError::UnknownMember { token, target_type } => {
                let field_name = Token::get_ident_name(token);

                format!(
                    "Unknown member '{}': ({}:{})\n{}\n\
                    Type {} does not have a member with name '{}'",
                    field_name, pos.line, pos.col,
                    cursor_line,
                    type_repr(target_type), field_name
                )
            }
            TypecheckerError::MissingRequiredParams { missing_params, .. } => {
                let missing_params = missing_params.join(", ");
                format!(
                    "Missing required parameters in function call: ({}:{})\n{}\n\
                    These parameters are required but missing: {}",
                    pos.line, pos.col,
                    cursor_line,
                    missing_params
                )
            }
            TypecheckerError::InvalidMixedParamType { .. } => {
                format!(
                    "Invalid function call: ({}:{})\n{}\n\
                    Cannot mix named and positional arguments.",
                    pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::InvalidTypeFuncInvocation { .. } => {
                format!(
                    "Invalid instantiation call: ({}:{})\n{}\n\
                    Constructor functions must be called with named parameters.",
                    pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::InvalidSelfParamPosition { .. } => {
                format!(
                    "Invalid position for `self`: ({}:{})\n{}\n\
                    `self` must appear as the first parameter",
                    pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::InvalidSelfParam { .. } => {
                format!(
                    "Invalid usage of `self` parameter: ({}:{})\n{}\n\
                    `self` can only appear within methods on types",
                    pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::MissingRequiredTypeAnnotation { .. } => {
                format!(
                    "Missing required type annotation: ({}:{})\n{}\n\
                    Methods must have return type explicitly annotated",
                    pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::InvalidTypeDeclDepth { .. } => {
                format!(
                    "Invalid location for type declaration: ({}:{})\n{}\n\
                    Types may only be declared at the root level",
                    pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::ForbiddenVariableType { typ, .. } => {
                match typ {
                    Type::Unknown => format!(
                        "Could not determine type: ({}:{})\n{}\n\
                        Please use an explicit type annotation to denote the type",
                        pos.line, pos.col, cursor_line
                    ),
                    Type::Unit => format!(
                        "Forbidden type for variable: ({}:{})\n{}\n\
                        Variables cannot be of type {}",
                        pos.line, pos.col, cursor_line, type_repr(&Type::Unit)
                    ),
                    _ => unreachable!()
                }
            }
            TypecheckerError::InvalidInstantiation { typ, .. } => {
                format!(
                    "Cannot create an instance of type {}: ({}:{})\n{}",
                    type_repr(typ), pos.line, pos.col, cursor_line
                )
            }
            TypecheckerError::InvalidTypeArgumentArity { actual_type, actual, expected, .. } => {
                format!(
                    "Expected {} type argument{}, but {} {} provided: ({}:{})\n{}\n{}",
                    expected, if *expected == 1 { "" } else { "s" }, actual, if *actual == 1 { "was" } else { "were" }, pos.line, pos.col, cursor_line,
                    format!(
                        "Provide {} type argument{} to match type {}",
                        expected, if *expected == 1 { "" } else { "s" }, type_repr(actual_type)
                    )
                )
            }
            TypecheckerError::UnreachableMatchCase { typ, is_unreachable_none, .. } => {
                format!(
                    "Unreachable match case: ({}:{})\n{}\n{}",
                    pos.line, pos.col, cursor_line,
                    if *is_unreachable_none {
                        "Value cannot possibly be None".to_string()
                    } else if let Some(typ) = typ {
                        format!("Value cannot possibly be of type {}", type_repr(typ))
                    } else {
                        "All possible cases have already been handled".to_string()
                    }
                )
            }
            TypecheckerError::DuplicateMatchCase { .. } => {
                format!("Duplicate match case: ({}:{})\n{}", pos.line, pos.col, cursor_line)
            }
            TypecheckerError::NonExhaustiveMatch { .. } => {
                format!(
                    "Non-exhaustive match: ({}:{})\n{}\n{}",
                    pos.line, pos.col, cursor_line,
                    "Please ensure each possible case is handled, or use the wildcard (_)"
                )
            }
            TypecheckerError::EmptyMatchBlock { .. } => {
                format!(
                    "Empty block for match case: ({}:{})\n{}\n{}",
                    pos.line, pos.col, cursor_line,
                    "Each case in a match expression must result in a value"
                )
            }
            TypecheckerError::MatchBranchMismatch { expected, actual, .. } => {
                format!(
                    "Type mismatch among the match-expression branches: ({}:{})\n{}\n\
                    The type {} does not match with the type {} of the other branches",
                    pos.line, pos.col, cursor_line, type_repr(actual), type_repr(expected)
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TypecheckerError;
    use crate::lexer::tokens::{Token, Position};
    use crate::typechecker::types::{Type, StructType};
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
         ^^^
  Expected Int, got Float"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_mismatch_error_with_ortype() {
        let src = "1 + 4.4".to_string();
        let token = Token::Float(Position::new(1, 5), 4.4);
        let err = TypecheckerError::Mismatch { token, expected: Type::Union(vec![Type::Int, Type::Float]), actual: Type::Int };

        let expected = format!("\
Type mismatch (1:5)
  |  1 + 4.4
         ^^^
  Expected Int | Float, got Int"
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
         ^^^
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
         ^^^
Binding already declared in scope at (1:5)
  |  val abc = 123
         ^^^"
        );

        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_duplicate_type() {
        let src = "type Abc {}\ntype Abc { a: Int }".to_string();
        let err = TypecheckerError::DuplicateType {
            ident: Token::Ident(Position::new(2, 6), "Abc".to_string()),
            orig_ident: Some(Token::Ident(Position::new(1, 6), "Abc".to_string())),
        };

        let expected = format!("\
Duplicate type 'Abc' (2:6)
  |  type Abc {{ a: Int }}
          ^^^
Type already declared in scope at (1:6)
  |  type Abc {{}}
          ^^^"
        );
        assert_eq!(expected, err.get_message(&src));

        // Test builtin type
        let src = "type Int {}".to_string();
        let err = TypecheckerError::DuplicateType {
            ident: Token::Ident(Position::new(1, 6), "Int".to_string()),
            orig_ident: None,
        };

        let expected = format!("\
Duplicate type 'Int' (1:6)
  |  type Int {{}}
          ^^^
'Int' already declared as built-in type"
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
     ^^^^
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
         ^^^^
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
         ^^^^
Since it's a 'val', you must provide an initial value"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_invalid_assignment_target() {
        let src = "true = \"abc\"".to_string();
        let err = TypecheckerError::InvalidAssignmentTarget {
            token: Token::Assign(Position::new(1, 6)),
            reason: None,
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
         ^^^
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
               ^^^^^^^^^^^^^^^
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
             ^^
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
             ^^
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
             ^^
The if-branch had type String, but the else-branch had type Int"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_invalid_invocation_target() {
        let src = "\"hello\"(a: 1, b: 4)".to_string();
        let err = TypecheckerError::InvalidInvocationTarget {
            token: Token::String(Position::new(1, 1), "hello".to_string()),
            target_type: Type::String,
        };

        let expected = format!("\
Cannot call target as function: (1:1)
  |  \"hello\"(a: 1, b: 4)
     ^^^^^^^
Type String is not invokeable"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_incorrect_arity() {
        let src = "func abc(a: Int) { a }\nabc(1, 2, 3)".to_string();
        let err = TypecheckerError::IncorrectArity {
            token: Token::Ident(Position::new(2, 1), "abc".to_string()),
            expected: 1,
            actual: 3,
        };

        let expected = format!("\
Incorrect arity for invocation: (2:1)
  |  abc(1, 2, 3)
     ^^^
Expected 1 required argument, but 3 were passed"
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
     ^^^
Functions that contain recursive references must explicitly declare their return type
Missing return type annotation here (1:6):
  |  func abc() {{
          ^^^"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_invalid_break() {
        let src = "func abc() { break }".to_string();
        let err = TypecheckerError::InvalidBreak(Token::Break(Position::new(1, 14)));

        let expected = format!("\
Unexpected break keyword: (1:14)
  |  func abc() {{ break }}
                  ^^^^^
A break keyword cannot appear outside of a loop"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_invalid_required_arg_position() {
        let src = "func abc(a = 3, b: Int) = a + b".to_string();
        let err = TypecheckerError::InvalidRequiredArgPosition(Token::Ident(Position::new(1, 17), "b".to_string()));

        let expected = format!("\
Invalid position for non-optional parameter: (1:17)
  |  func abc(a = 3, b: Int) = a + b
                     ^
Required parameters must all be listed before any optional parameters"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_invalid_indexing_target() {
        // Testing non-homogeneous maps
        let src = "val m = { a: \"hello\", b: 3 }\nm[\"a\"]".to_string();
        let err = TypecheckerError::InvalidIndexingTarget {
            token: Token::LBrack(Position::new(2, 2), false),
            target_type: Type::Map(
                vec![("a".to_string(), Type::String), ("b".to_string(), Type::Int)],
                None,
            ),
        };

        let expected = format!("\
Unsupported indexing operation: (2:2)
  |  m[\"a\"]
      ^
Cannot index into maps whose values are not all the same type"
        );
        assert_eq!(expected, err.get_message(&src));

        // Testing other types
        let src = "123[1]".to_string();
        let err = TypecheckerError::InvalidIndexingTarget {
            token: Token::LBrack(Position::new(1, 4), false),
            target_type: Type::Int,
        };

        let expected = format!("\
Unsupported indexing operation: (1:4)
  |  123[1]
        ^
Type Int is not indexable"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_invalid_indexing_selector() {
        let src = "\"abc\"[\"d\"]".to_string();
        let err = TypecheckerError::InvalidIndexingSelector {
            token: Token::LBrack(Position::new(1, 6), false),
            target_type: Type::String,
            selector_type: Type::String,
        };

        let expected = format!("\
Invalid type for indexing operator argument: (1:6)
  |  \"abc\"[\"d\"]
          ^
Cannot index into a target of type String, using a selector of type String"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_unknown_member() {
        let src = "[1, 2, 3].size".to_string();
        let err = TypecheckerError::UnknownMember {
            token: Token::Ident(Position::new(1, 11), "size".to_string()),
            target_type: Type::Array(Box::new(Type::Int)),
        };

        let expected = format!("\
Unknown member 'size': (1:11)
  |  [1, 2, 3].size
               ^^^^
Type Int[] does not have a member with name 'size'"
        );
        assert_eq!(expected, err.get_message(&src));

        let src = "type P { name: String}\nval p = Person({ nAme: \"hello\" })".to_string();
        let err = TypecheckerError::UnknownMember {
            token: Token::Ident(Position::new(2, 18), "nAme".to_string()),
            target_type: Type::Struct(StructType {
                name: "Person".to_string(),
                type_args: vec![],
                fields: vec![("name".to_string(), Type::String, false)],
                static_fields: vec![],
                methods: vec![],
            }),
        };

        let expected = format!("\
Unknown member 'nAme': (2:18)
  |  val p = Person({{ nAme: \"hello\" }})
                      ^^^^
Type Person does not have a member with name 'nAme'"
        );
        assert_eq!(expected, err.get_message(&src));
    }

    #[test]
    fn test_invalid_instantiation() {
        let src = "val u = Unit()".to_string();
        let err = TypecheckerError::InvalidInstantiation {
            token: ident_token!((1, 9), "Unit"),
            typ: Type::Unit,
        };

        let expected = format!("\
Cannot create an instance of type Unit: (1:9)
  |  val u = Unit()
             ^^^^");
        assert_eq!(expected, err.get_message(&src));
    }
}
