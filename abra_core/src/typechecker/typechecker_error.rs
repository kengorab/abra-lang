use crate::common::display_error::DisplayError;
use crate::lexer::tokens::Token;
use crate::typechecker::types::{Type, StructType, FnType, EnumType};
use crate::parser::ast::{BinaryOp, IndexingMode, AstNode, BindingPattern, ModuleId};
use itertools::Itertools;

#[derive(Debug, PartialEq)]
pub enum InvalidAssignmentTargetReason {
    IllegalTarget,
    IndexingMode,
    StringTarget,
    OptionalTarget,
    MethodTarget,
}

#[derive(Debug, PartialEq)]
pub enum TypecheckerErrorKind {
    Unimplemented(Token, String),
    Mismatch { token: Token, expected: Type, actual: Type },
    InvalidIfConditionType { token: Token, actual: Type },
    InvalidLoopTarget { token: Token, target_type: Type },
    InvalidOperator { token: Token, op: BinaryOp, ltype: Type, rtype: Type },
    MissingRequiredAssignment { ident: Token },
    DuplicateBinding { ident: Token, orig_ident: Option<Token> },
    DuplicateField { ident: Token, orig_ident: Token, orig_is_field: bool, orig_is_enum_variant: bool },
    DuplicateType { ident: Token, orig_ident: Option<Token> },
    DuplicateTypeArgument { ident: Token, orig_ident: Token },
    UnboundGeneric(Token, String),
    UnknownIdentifier { ident: Token },
    InvalidAssignmentTarget { token: Token, typ: Option<Type>, reason: InvalidAssignmentTargetReason },
    AssignmentToImmutable { orig_ident: Token, token: Token },
    UnannotatedUninitialized { ident: Token, is_mutable: bool },
    UnknownType { type_ident: Token },
    MissingIfExprBranch { if_token: Token, is_if_branch: bool },
    IfExprBranchMismatch { if_token: Token, if_type: Type, else_type: Type },
    InvalidInvocationTarget { token: Token, target_type: Type },
    IncorrectArity { token: Token, expected: usize, actual: usize },
    UnexpectedParamName { token: Token },
    DuplicateParamName { token: Token },
    InvalidTerminator(Token),
    InvalidRequiredArgPosition(Token),
    InvalidVarargPosition(Token),
    InvalidVarargUsage(Token),
    InvalidIndexingTarget { token: Token, target_type: Type, index_mode: IndexingMode<AstNode> },
    InvalidIndexingSelector { token: Token, target_type: Type, selector_type: Type },
    InvalidTupleIndexingSelector { token: Token, types: Vec<Type>, non_constant: bool, index: i64 },
    UnknownMember { token: Token, target_type: Type },
    MissingRequiredParams { token: Token, missing_params: Vec<String> },
    InvalidMixedParamType { token: Token },
    InvalidTypeFuncInvocation { token: Token },
    InvalidSelfParamPosition { token: Token },
    InvalidSelfParam { token: Token },
    InvalidTypeDeclDepth { token: Token },
    InvalidExportDepth { token: Token },
    ForbiddenVariableType { binding: BindingPattern, typ: Type },
    InvalidInstantiation { token: Token, typ: Type },
    InvalidTypeArgumentArity { token: Token, expected: usize, actual: usize, actual_type: Type },
    UnreachableMatchCase { token: Token, typ: Option<Type>, is_unreachable_none: bool },
    DuplicateMatchCase { token: Token },
    NonExhaustiveMatch { token: Token },
    EmptyMatchBlock { token: Token },
    MatchBranchMismatch { token: Token, expected: Type, actual: Type },
    InvalidUninitializedEnumVariant { token: Token },
    InvalidMatchCaseDestructuring { token: Token, typ: Option<Type>, enum_variant: Option<String> },
    InvalidMatchCaseDestructuringArity { token: Token, typ: Type, enum_variant: Option<String>, expected: usize, actual: usize },
    InvalidAssignmentDestructuring { binding: BindingPattern, typ: Type },
    DuplicateSplatDestructuring { token: Token },
    UnreachableCode { token: Token },
    ReturnTypeMismatch { token: Token, fn_name: String, fn_missing_ret_ann: bool, bare_return: bool, expected: Type, actual: Type },
    InvalidProtocolMethod { token: Token, fn_name: String, expected: Type, actual: Type },
    VarargMismatch { token: Token, typ: Type },
    InvalidAccess { token: Token, is_field: bool, is_get: bool },
    InvalidModuleImport { token: Token, module_name: String, circular: bool },
    InvalidImportValue { ident: Token },
}

#[derive(Debug, PartialEq)]
pub struct TypecheckerError {
    pub module_id: ModuleId,
    pub kind: TypecheckerErrorKind,
}

impl TypecheckerError {
    pub fn get_token(&self) -> &Token {
        match &self.kind {
            TypecheckerErrorKind::Unimplemented(token, _) => token,
            TypecheckerErrorKind::Mismatch { token, .. } => token,
            TypecheckerErrorKind::InvalidIfConditionType { token, .. } => token,
            TypecheckerErrorKind::InvalidLoopTarget { token, .. } => token,
            TypecheckerErrorKind::InvalidOperator { token, .. } => token,
            TypecheckerErrorKind::MissingRequiredAssignment { ident } => ident,
            TypecheckerErrorKind::DuplicateBinding { ident, .. } => ident,
            TypecheckerErrorKind::DuplicateType { ident, .. } => ident,
            TypecheckerErrorKind::DuplicateTypeArgument { ident, .. } => ident,
            TypecheckerErrorKind::UnboundGeneric(token, _) => token,
            TypecheckerErrorKind::DuplicateField { ident, .. } => ident,
            TypecheckerErrorKind::UnknownIdentifier { ident } => ident,
            TypecheckerErrorKind::InvalidAssignmentTarget { token, .. } => token,
            TypecheckerErrorKind::AssignmentToImmutable { token, .. } => token,
            TypecheckerErrorKind::UnannotatedUninitialized { ident, .. } => ident,
            TypecheckerErrorKind::UnknownType { type_ident } => type_ident,
            TypecheckerErrorKind::MissingIfExprBranch { if_token, .. } => if_token,
            TypecheckerErrorKind::IfExprBranchMismatch { if_token, .. } => if_token,
            TypecheckerErrorKind::InvalidInvocationTarget { token, .. } => token,
            TypecheckerErrorKind::IncorrectArity { token, .. } => token,
            TypecheckerErrorKind::UnexpectedParamName { token } => token,
            TypecheckerErrorKind::DuplicateParamName { token } => token,
            TypecheckerErrorKind::InvalidTerminator(token) => token,
            TypecheckerErrorKind::InvalidRequiredArgPosition(token) => token,
            TypecheckerErrorKind::InvalidVarargPosition(token) => token,
            TypecheckerErrorKind::InvalidVarargUsage(token) => token,
            TypecheckerErrorKind::InvalidIndexingTarget { token, .. } => token,
            TypecheckerErrorKind::InvalidIndexingSelector { token, .. } => token,
            TypecheckerErrorKind::InvalidTupleIndexingSelector { token, .. } => token,
            TypecheckerErrorKind::UnknownMember { token, .. } => token,
            TypecheckerErrorKind::MissingRequiredParams { token, .. } => token,
            TypecheckerErrorKind::InvalidMixedParamType { token } => token,
            TypecheckerErrorKind::InvalidTypeFuncInvocation { token } => token,
            TypecheckerErrorKind::InvalidSelfParamPosition { token } => token,
            TypecheckerErrorKind::InvalidSelfParam { token } => token,
            TypecheckerErrorKind::InvalidTypeDeclDepth { token } => token,
            TypecheckerErrorKind::InvalidExportDepth { token } => token,
            TypecheckerErrorKind::ForbiddenVariableType { binding, .. } => binding.get_token(),
            TypecheckerErrorKind::InvalidInstantiation { token, .. } => token,
            TypecheckerErrorKind::InvalidTypeArgumentArity { token, .. } => token,
            TypecheckerErrorKind::UnreachableMatchCase { token, .. } => token,
            TypecheckerErrorKind::DuplicateMatchCase { token, .. } => token,
            TypecheckerErrorKind::NonExhaustiveMatch { token } => token,
            TypecheckerErrorKind::EmptyMatchBlock { token } => token,
            TypecheckerErrorKind::MatchBranchMismatch { token, .. } => token,
            TypecheckerErrorKind::InvalidUninitializedEnumVariant { token } => token,
            TypecheckerErrorKind::InvalidMatchCaseDestructuring { token, .. } => token,
            TypecheckerErrorKind::InvalidMatchCaseDestructuringArity { token, .. } => token,
            TypecheckerErrorKind::InvalidAssignmentDestructuring { binding, .. } => binding.get_token(),
            TypecheckerErrorKind::DuplicateSplatDestructuring { token } => token,
            TypecheckerErrorKind::UnreachableCode { token } => token,
            TypecheckerErrorKind::ReturnTypeMismatch { token, .. } => token,
            TypecheckerErrorKind::InvalidProtocolMethod { token, .. } => token,
            TypecheckerErrorKind::VarargMismatch { token, .. } => token,
            TypecheckerErrorKind::InvalidAccess { token, .. } => token,
            TypecheckerErrorKind::InvalidModuleImport { token, .. } => token,
            TypecheckerErrorKind::InvalidImportValue { ident: token } => token,
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
        Type::Tuple(types) => {
            let types = types.iter().map(|t| type_repr(t)).join(", ");
            format!("({})", types)
        }
        Type::Set(typ) => {
            format!("Set<{}>", type_repr(typ))
        }
        Type::Map(key_type, value_type) => {
            format!("Map<{}, {}>", type_repr(key_type), type_repr(value_type))
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
        Type::Enum(EnumType { name, .. }) => format!("{}", name),
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
        BinaryOp::Xor => "^",
        BinaryOp::Coalesce => "?:",
        BinaryOp::CoalesceEq => "?:=",
        BinaryOp::Lt => "<",
        BinaryOp::Lte => "<=",
        BinaryOp::Gt => ">",
        BinaryOp::Gte => ">=",
        BinaryOp::Neq => "!=",
        BinaryOp::Eq => "==",
        BinaryOp::Pow => "**",
    }.to_string()
}

impl DisplayError for TypecheckerError {
    fn message_for_error(&self, file_name: &String, lines: &Vec<&str>) -> String {
        let pos = self.get_token().get_position();
        let cursor_line = Self::get_underlined_line(lines, self.get_token());

        let msg = match &self.kind {
            TypecheckerErrorKind::Unimplemented(_, message) => {
                format!(
                    "This feature is not yet implemented\n{}\n{}",
                    cursor_line, message
                )
            }
            TypecheckerErrorKind::Mismatch { expected, actual, .. } => {
                format!(
                    "Type mismatch\n{}\n\
                    Expected {}, got {}",
                    cursor_line,
                    type_repr(expected), type_repr(actual)
                )
            }
            TypecheckerErrorKind::InvalidIfConditionType { actual, .. } => {
                format!(
                    "Invalid type for condition\n{}\n\
                    Conditions must be an Option or Bool, got {}",
                    cursor_line, type_repr(actual)
                )
            }
            TypecheckerErrorKind::InvalidLoopTarget { target_type: actual, .. } => {
                format!(
                    "Invalid type for for-loop target\n{}\n\
                    Type {} is not iterable",
                    cursor_line, type_repr(actual)
                )
            }
            TypecheckerErrorKind::InvalidOperator { op, ltype, rtype, .. } => {
                format!(
                    "Invalid operator\n{}\n\
                    No operator exists to satisfy {} {} {}",
                    cursor_line,
                    type_repr(ltype), op_repr(op), type_repr(rtype)
                )
            }
            TypecheckerErrorKind::MissingRequiredAssignment { ident } => {
                let ident = Token::get_ident_name(&ident);
                format!(
                    "Expected assignment for variable '{}'\n{}\n\
                    Variables declared with 'val' must be initialized",
                    ident, cursor_line
                )
            }
            TypecheckerErrorKind::DuplicateBinding { ident, orig_ident } => {
                let ident = Token::get_ident_name(&ident);
                let first_msg = format!("Duplicate variable '{}'\n{}", &ident, cursor_line);

                let second_msg = if let Some(orig_ident) = orig_ident {
                    let pos = orig_ident.get_position();
                    let cursor_line = Self::get_underlined_line(lines, orig_ident);
                    format!("'{}' already declared in scope at ({}:{})\n{}", ident, pos.line, pos.col, cursor_line)
                } else {
                    format!("'{}' already declared as built-in value", ident)
                };

                format!("{}\n{}", first_msg, second_msg)
            }
            TypecheckerErrorKind::DuplicateField { ident, orig_ident, orig_is_field, orig_is_enum_variant } => {
                let ident = Token::get_ident_name(&ident);
                let first_msg = format!("Duplicate field '{}'\n{}", ident, cursor_line);

                let pos = orig_ident.get_position();
                let cursor_line = Self::get_underlined_line(lines, orig_ident);

                let noun = if *orig_is_field { "Field" } else if *orig_is_enum_variant { "Enum variant" } else { "Method" };
                let second_msg = format!("{} with that name is already declared in scope at ({}:{})\n{}", noun, pos.line, pos.col, cursor_line);

                format!("{}\n{}", first_msg, second_msg)
            }
            TypecheckerErrorKind::DuplicateType { ident, orig_ident } => { // orig_ident will be None if it's a builtin type
                let ident = Token::get_ident_name(&ident);
                let first_msg = format!("Duplicate type '{}'\n{}", ident, cursor_line);

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
            TypecheckerErrorKind::DuplicateTypeArgument { ident, orig_ident } => { // orig_ident will be None if it's a builtin type
                let ident = Token::get_ident_name(&ident);
                let first_msg = format!("Duplicate type argument '{}'\n{}", ident, cursor_line);

                let pos = orig_ident.get_position();
                let cursor_line = Self::get_underlined_line(lines, orig_ident);

                let second_msg = format!("Type already declared in scope at ({}:{})\n{}", pos.line, pos.col, cursor_line);
                format!("{}\n{}", first_msg, second_msg)
            }
            TypecheckerErrorKind::UnboundGeneric(_, type_arg_ident) => {
                format!(
                    "Type argument '{}' is unbound\n{}\n\
                    There is not enough information to determine a possible value for '{}'",
                    type_arg_ident, cursor_line, type_arg_ident
                )
            }
            TypecheckerErrorKind::UnknownIdentifier { ident } => {
                let ident = Token::get_ident_name(&ident);
                if &ident == "_" {
                    format!(
                        "Unknown identifier '{}'\n{}\n\
                        The _ represents an anonymous identifier; please give the variable a name if you want to reference it",
                        ident, cursor_line
                    )
                } else {
                    format!(
                        "Unknown identifier '{}'\n{}\n\
                        No variable with that name is visible in current scope",
                        ident, cursor_line
                    )
                }
            }
            TypecheckerErrorKind::InvalidAssignmentTarget { typ, reason, .. } => {
                let msg = match reason {
                    InvalidAssignmentTargetReason::IllegalTarget => "Left-hand side of assignment must be a valid identifier".to_string(),
                    InvalidAssignmentTargetReason::IndexingMode |
                    InvalidAssignmentTargetReason::StringTarget => "Cannot assign to sub-range of target".to_string(),
                    InvalidAssignmentTargetReason::OptionalTarget => format!(
                        "Cannot assign by indexing into type {}, which is potentially None",
                        type_repr(typ.as_ref().unwrap())
                    ),
                    InvalidAssignmentTargetReason::MethodTarget => "Methods cannot be reassigned to".to_string(),
                };
                format!(
                    "Cannot perform assignment\n{}\n{}",
                    cursor_line, msg
                )
            }
            TypecheckerErrorKind::AssignmentToImmutable { orig_ident, token: _ } => {
                let ident = Token::get_ident_name(&orig_ident);
                let first_msg = format!("Cannot assign to variable '{}'\n{}", ident, cursor_line);

                let pos = orig_ident.get_position();
                let cursor_line = Self::get_underlined_line(lines, orig_ident);
                let second_msg = format!("The variable has been declared in scope as immutable at ({}:{})\n{}", pos.line, pos.col, cursor_line);

                format!("{}\n{}\nUse 'var' instead of 'val' to create a mutable variable", first_msg, second_msg)
            }
            TypecheckerErrorKind::UnannotatedUninitialized { ident, is_mutable } => {
                let ident = Token::get_ident_name(&ident);
                let msg = if *is_mutable {
                    "Since it's a 'var', you can either provide an initial value or a type annotation"
                } else {
                    "Since it's a 'val', you must provide an initial value"
                };

                let modifier = if *is_mutable { "mutable" } else { "immutable" };
                format!(
                    "Could not determine type of {} variable '{}'\n{}\n{}",
                    modifier, ident, cursor_line, msg
                )
            }
            TypecheckerErrorKind::UnknownType { type_ident } => {
                let ident = Token::get_ident_name(type_ident);
                format!(
                    "Unknown type '{}'\n{}\nNo type with that name is visible in current scope",
                    ident, cursor_line
                )
            }
            TypecheckerErrorKind::MissingIfExprBranch { if_token: _, is_if_branch } => {
                format!(
                    "Missing {}-branch in if-else expression\n{}\n\
                    Both branches must have some value when used as an expression",
                    if *is_if_branch { "if" } else { "else" }, cursor_line
                )
            }
            TypecheckerErrorKind::IfExprBranchMismatch { if_token: _, if_type, else_type } => {
                format!(
                    "Type mismatch between the if-else expression branches\n{}\n\
                    The if-branch had type {}, but the else-branch had type {}",
                    cursor_line, type_repr(if_type), type_repr(else_type)
                )
            }
            TypecheckerErrorKind::InvalidInvocationTarget { target_type, .. } => {
                format!(
                    "Cannot call target as function\n{}\n\
                    Type {} is not invokeable",
                    cursor_line, type_repr(target_type)
                )
            }
            TypecheckerErrorKind::IncorrectArity { expected, actual, .. } => {
                format!(
                    "Incorrect arity for invocation\n{}\n\
                    Expected {} required argument{}, but {} were passed",
                    cursor_line,
                    expected, if *expected == 1 { "" } else { "s" }, actual
                )
            }
            TypecheckerErrorKind::UnexpectedParamName { token } => {
                let param_name = Token::get_ident_name(token);
                format!(
                    "Unexpected parameter name '{}'\n{}\n\
                    This function doesn't have a parameter called '{}'",
                    param_name, cursor_line, param_name,
                )
            }
            TypecheckerErrorKind::DuplicateParamName { .. } => {
                format!(
                    "Duplicate parameter name\n{}\n\
                    A parameter of this name has already been passed",
                    cursor_line,
                )
            }
            TypecheckerErrorKind::InvalidTerminator(token) => {
                let (keyword, msg) = match token {
                    Token::Break(_) => ("break", "A break keyword cannot appear outside of a loop"),
                    Token::Continue(_) => ("continue", "A continue keyword cannot appear outside of a loop"),
                    Token::Return(_, _) => ("return", "A return keyword cannot appear outside of a function"),
                    _ => unreachable!()
                };

                format!(
                    "Unexpected {} keyword\n{}\n{}",
                    keyword, cursor_line, msg
                )
            }
            TypecheckerErrorKind::InvalidRequiredArgPosition(_token) => {
                format!(
                    "Invalid position for non-optional parameter\n{}\n\
                    Required parameters must all be listed before any optional parameters",
                    cursor_line
                )
            }
            TypecheckerErrorKind::InvalidVarargPosition(_token) => {
                format!(
                    "Invalid position for vararg parameter\n{}\n\
                    Vararg parameters must be the last in the parameter list",
                    cursor_line
                )
            }
            TypecheckerErrorKind::InvalidVarargUsage(_token) => {
                format!(
                    "Invalid usage of vararg parameter\n{}\n\
                    Vararg parameters cannot be used in this context",
                    cursor_line
                )
            }
            TypecheckerErrorKind::InvalidIndexingTarget { target_type, index_mode, .. } => {
                let context = if let IndexingMode::Range(_, _) = index_mode { " as a range" } else { "" };
                format!(
                    "Unsupported indexing operation\n{}\n\
                    Type {} is not indexable{}",
                    cursor_line, type_repr(target_type), context
                )
            }
            TypecheckerErrorKind::InvalidIndexingSelector { target_type, selector_type, .. } => {
                format!(
                    "Invalid type for indexing operator argument\n{}\n\
                    Cannot index into a target of type {}, using a selector of type {}",
                    cursor_line, type_repr(target_type), type_repr(selector_type)
                )
            }
            TypecheckerErrorKind::InvalidTupleIndexingSelector { types, non_constant, index, .. } => {
                let message = if *non_constant {
                    "\nIndex values for tuples must be constant integers".to_string()
                } else if *index != -1 {
                    format!(
                        "\nNo value at index {} for tuple {}",
                        index, type_repr(&Type::Tuple(types.clone()))
                    )
                } else { "".to_string() };

                format!(
                    "Unsupported indexing into tuple\n{}{}",
                    cursor_line, message
                )
            }
            TypecheckerErrorKind::UnknownMember { token, target_type } => {
                let field_name = Token::get_ident_name(token);

                format!(
                    "Unknown member '{}'\n{}\n\
                    Type {} does not have a member with name '{}'",
                    field_name, cursor_line,
                    type_repr(target_type), field_name
                )
            }
            TypecheckerErrorKind::MissingRequiredParams { missing_params, .. } => {
                let missing_params = missing_params.join(", ");
                format!(
                    "Missing required parameters in function call\n{}\n\
                    These parameters are required but missing: {}",
                    cursor_line,
                    missing_params
                )
            }
            TypecheckerErrorKind::InvalidMixedParamType { .. } => {
                format!(
                    "Invalid function call\n{}\n\
                    Cannot mix named and positional arguments.",
                    cursor_line
                )
            }
            TypecheckerErrorKind::InvalidTypeFuncInvocation { .. } => {
                format!(
                    "Invalid instantiation call\n{}\n\
                    Constructor functions must be called with named parameters.",
                    cursor_line
                )
            }
            TypecheckerErrorKind::InvalidSelfParamPosition { .. } => {
                format!(
                    "Invalid position for `self`\n{}\n\
                    `self` must appear as the first parameter",
                    cursor_line
                )
            }
            TypecheckerErrorKind::InvalidSelfParam { .. } => {
                format!(
                    "Invalid usage of `self` parameter\n{}\n\
                    `self` can only appear within methods on types",
                    cursor_line
                )
            }
            TypecheckerErrorKind::InvalidTypeDeclDepth { .. } => {
                format!(
                    "Invalid location for type declaration\n{}\n\
                    Types may only be declared at the root level",
                    cursor_line
                )
            }
            TypecheckerErrorKind::InvalidExportDepth { .. } => {
                format!(
                    "Invalid export modifier\n{}\n\
                    Exported values may only appear at the top level scope",
                    cursor_line
                )
            }
            TypecheckerErrorKind::ForbiddenVariableType { typ, .. } => {
                match typ {
                    Type::Unknown => format!(
                        "Could not determine type\n{}\n\
                        Please use an explicit type annotation to denote the type",
                        cursor_line
                    ),
                    Type::Unit => format!(
                        "Forbidden type for variable\n{}\n\
                        Variables cannot be of type {}",
                        cursor_line, type_repr(&Type::Unit)
                    ),
                    _ => unreachable!()
                }
            }
            TypecheckerErrorKind::InvalidInstantiation { typ, .. } => {
                format!(
                    "Cannot create an instance of type {}\n{}",
                    type_repr(typ), cursor_line
                )
            }
            TypecheckerErrorKind::InvalidTypeArgumentArity { actual_type, actual, expected, .. } => {
                format!(
                    "Expected {} type argument{}, but {} {} provided\n{}{}",
                    expected, if *expected == 1 { "" } else { "s" }, actual, if *actual == 1 { "was" } else { "were" }, cursor_line,
                    if *expected > 0 {
                        format!(
                            "\nProvide {} type argument{} to match type {}",
                            expected, if *expected == 1 { "" } else { "s" }, type_repr(actual_type)
                        )
                    } else { "".to_string() }
                )
            }
            TypecheckerErrorKind::UnreachableMatchCase { typ, is_unreachable_none, .. } => {
                format!(
                    "Unreachable match case\n{}\n{}",
                    cursor_line,
                    if *is_unreachable_none {
                        "Value cannot possibly be None at this point".to_string()
                    } else if let Some(typ) = typ {
                        format!("Value cannot possibly be of type {} at this point", type_repr(typ))
                    } else {
                        "All possible cases have already been handled".to_string()
                    }
                )
            }
            TypecheckerErrorKind::DuplicateMatchCase { .. } => {
                format!("Duplicate match case\n{}", cursor_line)
            }
            TypecheckerErrorKind::NonExhaustiveMatch { .. } => {
                format!(
                    "Non-exhaustive match\n{}\n{}",
                    cursor_line,
                    "Please ensure each possible case is handled, or use the wildcard (_)"
                )
            }
            TypecheckerErrorKind::EmptyMatchBlock { .. } => {
                format!(
                    "Empty block for match case\n{}\n{}",
                    cursor_line,
                    "Each case in a match expression must result in a value"
                )
            }
            TypecheckerErrorKind::MatchBranchMismatch { expected, actual, .. } => {
                format!(
                    "Type mismatch among the match-expression branches\n{}\n\
                    The type {} does not match with the type {} of the other branches",
                    cursor_line, type_repr(actual), type_repr(expected)
                )
            }
            TypecheckerErrorKind::InvalidUninitializedEnumVariant { .. } => {
                format!(
                    "Invalid usage of enum variant\n{}\n\
                    This enum variant requires arguments",
                    cursor_line
                )
            }
            TypecheckerErrorKind::InvalidMatchCaseDestructuring { typ, enum_variant, .. } => {
                let msg = match typ {
                    Some(typ) => match enum_variant {
                        Some(variant_name) => format!("Cannot destructure variant {} of enum {}", variant_name, type_repr(typ)),
                        None => format!("Cannot destructure an instance of type {}", type_repr(typ))
                    },
                    None => "Cannot destructure instance of None".to_string(),
                };
                format!(
                    "Invalid destructuring for match\n{}\n\n{}",
                    cursor_line, msg
                )
            }
            TypecheckerErrorKind::InvalidMatchCaseDestructuringArity { typ, enum_variant, expected, actual, .. } => {
                let type_displ = match enum_variant {
                    Some(variant_name) => format!("{}.{}", type_repr(typ), variant_name),
                    None => format!("type {}", type_repr(typ))
                };
                format!(
                    "Invalid destructuring pattern for match\n{}\n\
                    Instances of {} have {} field{}, but the pattern attempts to extract {}",
                    cursor_line,
                    type_displ, expected, if *expected == 1 { "" } else { "s" }, actual
                )
            }
            TypecheckerErrorKind::InvalidAssignmentDestructuring { binding, typ } => {
                let msg = match (binding, typ) {
                    (BindingPattern::Tuple(_, dest_args), Type::Tuple(type_opts)) if dest_args.len() != type_opts.len() => {
                        format!("Cannot destructure a tuple of {} elements into {} values", type_opts.len(), dest_args.len())
                    }
                    (BindingPattern::Tuple(_, _), typ) => {
                        format!("Cannot destructure a value of type {} as a tuple", type_repr(typ))
                    }
                    // TODO: Proper error message here, this is totally a reachable case!
                    _ => unreachable!()
                };

                format!(
                    "Invalid destructuring pattern for assignment\n{}\n{}",
                    cursor_line, msg
                )
            }
            TypecheckerErrorKind::DuplicateSplatDestructuring { .. } => {
                format!(
                    "Invalid destructuring pattern for assignment\n{}\n\
                    Cannot have more than one splat (*) instance in an array destructuring",
                    cursor_line
                )
            }
            TypecheckerErrorKind::UnreachableCode { .. } => {
                format!(
                    "Unreachable code\n{}\n\
                    Code comes after a return statement and will never be called",
                    cursor_line
                )
            }
            TypecheckerErrorKind::ReturnTypeMismatch { fn_name, fn_missing_ret_ann, bare_return, expected, actual, .. } => {
                let actual = match &actual {
                    Type::Option(i) if **i == Type::Placeholder => {
                        Type::Option(Box::new(expected.clone()))
                    }
                    _ => actual.clone()
                };
                let msg = if *bare_return {
                    format!(
                        "Function '{}' has return type {}, but no expression was returned",
                        fn_name, type_repr(expected)
                    )
                } else {
                    format!(
                        "Function '{}' has return type {}, but this is of type {}",
                        fn_name, type_repr(expected), type_repr(&actual),
                    )
                };
                let hint = if actual == Type::Option(Box::new(Type::Unit)) {
                    format!("\n(Note: Values of type {} are redundant and can probably just be removed)", type_repr(&actual))
                } else if expected == &Type::Unit && *fn_missing_ret_ann {
                    "\n(Note: A function without a return type annotation is assumed to return Unit.\n       Try adding a return type annotation to the function.)".to_string()
                } else { "".to_string() };

                format!(
                    "Invalid return type\n{}\n{}{}",
                    cursor_line,
                    msg, hint
                )
            }
            TypecheckerErrorKind::InvalidProtocolMethod { fn_name, expected, actual, .. } => {
                format!(
                    "Invalid type for method\n{}\n\
                    Expected method {} to be of type {}, but instead got {}",
                    cursor_line,
                    fn_name, type_repr(expected), type_repr(actual)
                )
            }
            TypecheckerErrorKind::VarargMismatch { typ, .. } => {
                format!(
                    "Invalid type for vararg parameter\n{}\n\
                    Vararg parameters must be an Array type, but got {}",
                    cursor_line, type_repr(typ)
                )
            }
            TypecheckerErrorKind::InvalidAccess { token, is_field, is_get, .. } => {
                let target = Token::get_ident_name(token);
                let target_kind = if *is_field { "field" } else { "method" };
                let access_kind = if *is_get { "read" } else { "write" };
                let access_str = if *is_get { "get" } else { "set" };
                format!(
                    "Invalid access for {} '{}'\n{}\n\
                    Cannot {} field '{}' since it is not {}table",
                    target_kind, target, cursor_line,
                    access_kind, target, access_str
                )
            }
            TypecheckerErrorKind::InvalidModuleImport { module_name, circular, .. } => {
                let reason = if *circular {
                    format!(
                        "Circular dependency detected within the module '{}'. It appears that some \
                        module imported by '{}' is trying to import '{}', which resulted in a cycle.",
                        module_name, module_name, module_name
                    )
                } else {
                    format!(
                        "The module '{}' could not be loaded. Please ensure that the import path of the \
                        module is correct, and that there is a module at that location",
                        module_name
                    )
                };

                format!(
                    "Could not import module\n{}\n{}",
                    cursor_line, reason
                )
            }
            TypecheckerErrorKind::InvalidImportValue { ident } => {
                format!(
                    "Invalid import\n{}\n\
                    This module does not export any value called '{}'",
                    cursor_line,
                    Token::get_ident_name(ident)
                )
            }
        };

        let error_line = format!("Error at {}:{}:{}", file_name, pos.line, pos.col);
        format!("{}\n{}", error_line, msg)
    }
}

#[cfg(test)]
mod tests {
    use super::TypecheckerErrorKind;
    use crate::lexer::tokens::{Token, Position};
    use crate::typechecker::types::{Type, StructType, StructTypeField};
    use crate::common::display_error::DisplayError;
    use crate::parser::ast::{BinaryOp, AstNode, AstLiteralNode, IndexingMode, ModuleId};
    use crate::typechecker::typechecker_error::{InvalidAssignmentTargetReason, TypecheckerError};

    #[test]
    fn test_mismatch_error() {
        let module_id = ModuleId::from_name("test");
        let src = "1 + 4.4".to_string();
        let token = Token::Float(Position::new(1, 5), 4.4);
        let err = TypecheckerError { module_id, kind: TypecheckerErrorKind::Mismatch { token, expected: Type::Int, actual: Type::Float } };

        let expected = format!("\
Error at /tests/test.abra:1:5
Type mismatch
  |  1 + 4.4
         ^^^
Expected Int, got Float"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(), &src));

        let module_id = ModuleId::from_name("test");
        let src = "1 + 4.4".to_string();
        let token = Token::Float(Position::new(1, 5), 4.4);
        let err = TypecheckerError { module_id, kind: TypecheckerErrorKind::Mismatch { token, expected: Type::Union(vec![Type::Int, Type::Float]), actual: Type::Int } };

        let expected = format!("\
Error at /tests/test.abra:1:5
Type mismatch
  |  1 + 4.4
         ^^^
Expected Int | Float, got Int"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(), &src));
    }

    #[test]
    fn test_invalid_operator() {
        let module_id = ModuleId::from_name("test");
        let src = "1 - \"some string\"".to_string();
        let token = Token::Minus(Position::new(1, 3));
        let err = TypecheckerError { module_id, kind: TypecheckerErrorKind::InvalidOperator { token, op: BinaryOp::Sub, ltype: Type::Int, rtype: Type::String } };

        let expected = format!("\
Error at /tests/test.abra:1:3
Invalid operator
  |  1 - \"some string\"
       ^
No operator exists to satisfy Int - String"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_missing_required_assignment() {
        let module_id = ModuleId::from_name("test");
        let src = "val abc".to_string();
        let err = TypecheckerError { module_id, kind: TypecheckerErrorKind::MissingRequiredAssignment { ident: ident_token!((1, 5), "abc") } };

        let expected = format!("\
Error at /tests/test.abra:1:5
Expected assignment for variable 'abc'
  |  val abc
         ^^^
Variables declared with 'val' must be initialized"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_duplicate_binding() {
        let module_id = ModuleId::from_name("test");
        let src = "val abc = 123\nval abc = 5".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::DuplicateBinding {
                ident: Token::Ident(Position::new(2, 5), "abc".to_string()),
                orig_ident: Some(Token::Ident(Position::new(1, 5), "abc".to_string())),
            },
        };
        let expected = format!("\
Error at /tests/test.abra:2:5
Duplicate variable 'abc'
  |  val abc = 5
         ^^^
'abc' already declared in scope at (1:5)
  |  val abc = 123
         ^^^"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));

        // Test with prelude
        let module_id = ModuleId::from_name("test");
        let src = "func println() {}".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::DuplicateBinding {
                ident: Token::Ident(Position::new(1, 6), "println".to_string()),
                orig_ident: None,
            },
        };
        let expected = format!("\
Error at /tests/test.abra:1:6
Duplicate variable 'println'
  |  func println() {{}}
          ^^^^^^^
'println' already declared as built-in value"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_duplicate_type() {
        let module_id = ModuleId::from_name("test");
        let src = "type Abc {}\ntype Abc { a: Int }".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::DuplicateType {
                ident: Token::Ident(Position::new(2, 6), "Abc".to_string()),
                orig_ident: Some(Token::Ident(Position::new(1, 6), "Abc".to_string())),
            },
        };

        let expected = format!("\
Error at /tests/test.abra:2:6
Duplicate type 'Abc'
  |  type Abc {{ a: Int }}
          ^^^
Type already declared in scope at (1:6)
  |  type Abc {{}}
          ^^^"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));

        // Test builtin type
        let module_id = ModuleId::from_name("test");
        let src = "type Int {}".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::DuplicateType {
                ident: Token::Ident(Position::new(1, 6), "Int".to_string()),
                orig_ident: None,
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:6
Duplicate type 'Int'
  |  type Int {{}}
          ^^^
'Int' already declared as built-in type"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_unknown_identifier() {
        let module_id = ModuleId::from_name("test");
        let src = "abcd".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::UnknownIdentifier {
                ident: Token::Ident(Position::new(1, 1), "abcd".to_string())
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:1
Unknown identifier 'abcd'
  |  abcd
     ^^^^
No variable with that name is visible in current scope"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));

        let module_id = ModuleId::from_name("test");
        let src = "println(_)".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::UnknownIdentifier {
                ident: Token::Ident(Position::new(1, 9), "_".to_string())
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:9
Unknown identifier '_'
  |  println(_)
             ^
The _ represents an anonymous identifier; please give the variable a name if you want to reference it"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_unannotated_and_uninitialized() {
        let module_id = ModuleId::from_name("test");
        let src = "var abcd".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::UnannotatedUninitialized {
                ident: Token::Ident(Position::new(1, 5), "abcd".to_string()),
                is_mutable: true,
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:5
Could not determine type of mutable variable 'abcd'
  |  var abcd
         ^^^^
Since it's a 'var', you can either provide an initial value or a type annotation"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));

        let module_id = ModuleId::from_name("test");
        let src = "val abcd".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::UnannotatedUninitialized {
                ident: Token::Ident(Position::new(1, 5), "abcd".to_string()),
                is_mutable: false,
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:5
Could not determine type of immutable variable 'abcd'
  |  val abcd
         ^^^^
Since it's a 'val', you must provide an initial value"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_invalid_assignment_target() {
        let module_id = ModuleId::from_name("test");
        let src = "true = \"abc\"".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::InvalidAssignmentTarget {
                token: Token::Assign(Position::new(1, 6)),
                typ: None,
                reason: InvalidAssignmentTargetReason::IllegalTarget,
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:6
Cannot perform assignment
  |  true = \"abc\"
          ^
Left-hand side of assignment must be a valid identifier"
        );

        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_assignment_to_immutable() {
        let module_id = ModuleId::from_name("test");
        let src = "val abc = 1\n\nabc = 3".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::AssignmentToImmutable {
                orig_ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                token: Token::Assign(Position::new(3, 5)),
            },
        };

        let expected = format!("\
Error at /tests/test.abra:3:5
Cannot assign to variable 'abc'
  |  abc = 3
         ^
The variable has been declared in scope as immutable at (1:5)
  |  val abc = 1
         ^^^
Use 'var' instead of 'val' to create a mutable variable"
        );

        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_unknown_type() {
        let module_id = ModuleId::from_name("test");
        let src = "val abcd: NonExistentType = 432".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::UnknownType {
                type_ident: Token::Ident(Position::new(1, 11), "NonExistentType".to_string())
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:11
Unknown type 'NonExistentType'
  |  val abcd: NonExistentType = 432
               ^^^^^^^^^^^^^^^
No type with that name is visible in current scope"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_missing_if_expr_branch() {
        let module_id = ModuleId::from_name("test");
        let src = "val a = if (true) {} else 123".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::MissingIfExprBranch {
                if_token: Token::If(Position::new(1, 9)),
                is_if_branch: true,
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:9
Missing if-branch in if-else expression
  |  val a = if (true) {{}} else 123
             ^^
Both branches must have some value when used as an expression"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));

        let module_id = ModuleId::from_name("test");
        let src = "val a = if (true) 123 else {}".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::MissingIfExprBranch {
                if_token: Token::If(Position::new(1, 9)),
                is_if_branch: false,
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:9
Missing else-branch in if-else expression
  |  val a = if (true) 123 else {{}}
             ^^
Both branches must have some value when used as an expression"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_if_expr_branch_mismatch() {
        let module_id = ModuleId::from_name("test");
        let src = "val a = if (true) \"hello\" else 123".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::IfExprBranchMismatch {
                if_token: Token::If(Position::new(1, 9)),
                if_type: Type::String,
                else_type: Type::Int,
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:9
Type mismatch between the if-else expression branches
  |  val a = if (true) \"hello\" else 123
             ^^
The if-branch had type String, but the else-branch had type Int"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_invalid_invocation_target() {
        let module_id = ModuleId::from_name("test");
        let src = "\"hello\"(a: 1, b: 4)".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::InvalidInvocationTarget {
                token: Token::String(Position::new(1, 1), "hello".to_string()),
                target_type: Type::String,
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:1
Cannot call target as function
  |  \"hello\"(a: 1, b: 4)
     ^^^^^^^
Type String is not invokeable"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_incorrect_arity() {
        let module_id = ModuleId::from_name("test");
        let src = "func abc(a: Int) { a }\nabc(1, 2, 3)".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::IncorrectArity {
                token: Token::Ident(Position::new(2, 1), "abc".to_string()),
                expected: 1,
                actual: 3,
            },
        };

        let expected = format!("\
Error at /tests/test.abra:2:1
Incorrect arity for invocation
  |  abc(1, 2, 3)
     ^^^
Expected 1 required argument, but 3 were passed"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_invalid_terminator() {
        let module_id = ModuleId::from_name("test");
        let src = "func abc() { break }".to_string();
        let err = TypecheckerError { module_id, kind: TypecheckerErrorKind::InvalidTerminator(Token::Break(Position::new(1, 14))) };

        let expected = format!("\
Error at /tests/test.abra:1:14
Unexpected break keyword
  |  func abc() {{ break }}
                  ^^^^^
A break keyword cannot appear outside of a loop"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));

        let module_id = ModuleId::from_name("test");
        let src = "func abc() { continue }".to_string();
        let err = TypecheckerError { module_id, kind: TypecheckerErrorKind::InvalidTerminator(Token::Continue(Position::new(1, 14))) };

        let expected = format!("\
Error at /tests/test.abra:1:14
Unexpected continue keyword
  |  func abc() {{ continue }}
                  ^^^^^^^^
A continue keyword cannot appear outside of a loop"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));

        let module_id = ModuleId::from_name("test");
        let src = "while true { return }".to_string();
        let err = TypecheckerError { module_id, kind: TypecheckerErrorKind::InvalidTerminator(Token::Return(Position::new(1, 14), false)) };

        let expected = format!("\
Error at /tests/test.abra:1:14
Unexpected return keyword
  |  while true {{ return }}
                  ^^^^^^
A return keyword cannot appear outside of a function"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_invalid_required_arg_position() {
        let module_id = ModuleId::from_name("test");
        let src = "func abc(a = 3, b: Int) = a + b".to_string();
        let err = TypecheckerError { module_id, kind: TypecheckerErrorKind::InvalidRequiredArgPosition(Token::Ident(Position::new(1, 17), "b".to_string())) };

        let expected = format!("\
Error at /tests/test.abra:1:17
Invalid position for non-optional parameter
  |  func abc(a = 3, b: Int) = a + b
                     ^
Required parameters must all be listed before any optional parameters"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_invalid_indexing_target() {
        let module_id = ModuleId::from_name("test");
        let src = "123[1]".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::InvalidIndexingTarget {
                token: Token::LBrack(Position::new(1, 4), false),
                target_type: Type::Int,
                index_mode: IndexingMode::Index(Box::new(
                    AstNode::Literal(
                        Token::Int(Position::new(1, 5), 1),
                        AstLiteralNode::IntLiteral(1),
                    )
                )),
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:4
Unsupported indexing operation
  |  123[1]
        ^
Type Int is not indexable"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));

        let module_id = ModuleId::from_name("test");
        let src = "123[1:2]".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::InvalidIndexingTarget {
                token: Token::LBrack(Position::new(1, 4), false),
                target_type: Type::Int,
                index_mode: IndexingMode::Range(
                    Some(Box::new(
                        AstNode::Literal(
                            Token::Int(Position::new(1, 5), 1),
                            AstLiteralNode::IntLiteral(1),
                        )
                    )),
                    Some(Box::new(
                        AstNode::Literal(
                            Token::Int(Position::new(1, 7), 2),
                            AstLiteralNode::IntLiteral(2),
                        )
                    )),
                ),
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:4
Unsupported indexing operation
  |  123[1:2]
        ^
Type Int is not indexable as a range"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_invalid_indexing_selector() {
        let module_id = ModuleId::from_name("test");
        let src = "\"abc\"[\"d\"]".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::InvalidIndexingSelector {
                token: Token::LBrack(Position::new(1, 6), false),
                target_type: Type::String,
                selector_type: Type::String,
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:6
Invalid type for indexing operator argument
  |  \"abc\"[\"d\"]
          ^
Cannot index into a target of type String, using a selector of type String"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_unknown_member() {
        let module_id = ModuleId::from_name("test");
        let src = "[1, 2, 3].size".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::UnknownMember {
                token: Token::Ident(Position::new(1, 11), "size".to_string()),
                target_type: Type::Array(Box::new(Type::Int)),
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:11
Unknown member 'size'
  |  [1, 2, 3].size
               ^^^^
Type Int[] does not have a member with name 'size'"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));

        let module_id = ModuleId::from_name("test");
        let src = "type P { name: String}\nval p = Person({ nAme: \"hello\" })".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::UnknownMember {
                token: Token::Ident(Position::new(2, 18), "nAme".to_string()),
                target_type: Type::Struct(StructType {
                    name: "Person".to_string(),
                    type_args: vec![],
                    fields: vec![
                        StructTypeField { name: "name".to_string(), typ: Type::String, has_default_value: false, readonly: true }
                    ],
                    static_fields: vec![],
                    methods: vec![],
                }),
            },
        };

        let expected = format!("\
Error at /tests/test.abra:2:18
Unknown member 'nAme'
  |  val p = Person({{ nAme: \"hello\" }})
                      ^^^^
Type Person does not have a member with name 'nAme'"
        );
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }

    #[test]
    fn test_invalid_instantiation() {
        let module_id = ModuleId::from_name("test");
        let src = "val u = Unit()".to_string();
        let err = TypecheckerError {
            module_id,
            kind: TypecheckerErrorKind::InvalidInstantiation {
                token: ident_token!((1, 9), "Unit"),
                typ: Type::Unit,
            },
        };

        let expected = format!("\
Error at /tests/test.abra:1:9
Cannot create an instance of type Unit
  |  val u = Unit()
             ^^^^");
        assert_eq!(expected, err.get_message(&"/tests/test.abra".to_string(),&src));
    }
}
