use crate::js_value::token::{JsToken, JsRange};
use crate::js_value::position::JsPosition;
use crate::js_value::abra_type::JsType;
use crate::js_value::binary_op::JsBinaryOp;
use abra_core::Error;
use abra_core::parser::parse_error::ParseError;
use abra_core::lexer::lexer_error::LexerError;
use abra_core::typechecker::typechecker_error::TypecheckerError;
use abra_core::vm::vm::InterpretError;
use serde::{Serialize, Serializer};

pub struct JsWrappedError<'a>(pub &'a Error);

impl<'a> Serialize for JsWrappedError<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match &self.0 {
            Error::ParseError(parse_error) => match parse_error {
                ParseError::UnexpectedToken(token) => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "parseError")?;
                    obj.serialize_entry("subKind", "unexpectedToken")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&token.get_range()))?;
                    obj.end()
                }
                ParseError::UnexpectedEof => {
                    let mut obj = serializer.serialize_map(Some(2))?;
                    obj.serialize_entry("kind", "parseError")?;
                    obj.serialize_entry("subKind", "unexpectedEof")?;
                    obj.end()
                }
                ParseError::ExpectedToken(token_type, token) => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "parseError")?;
                    obj.serialize_entry("subKind", "expectedToken")?;
                    obj.serialize_entry("expectedType", &token_type.to_string())?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&token.get_range()))?;
                    obj.end()
                }
            }
            Error::LexerError(lexer_error) => match lexer_error {
                LexerError::UnexpectedEof(pos) => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "lexerError")?;
                    obj.serialize_entry("subKind", "unexpectedEof")?;
                    obj.serialize_entry("pos", &JsPosition(pos))?;
                    obj.end()
                }
                LexerError::UnexpectedChar(pos, ch) => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "lexerError")?;
                    obj.serialize_entry("subKind", "unexpectedChar")?;
                    obj.serialize_entry("pos", &JsPosition(pos))?;
                    obj.serialize_entry("char", ch)?;
                    obj.end()
                }
                LexerError::UnterminatedString(start_pos, end_pos) => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "lexerError")?;
                    obj.serialize_entry("subKind", "unterminatedString")?;
                    obj.serialize_entry("startPos", &JsPosition(start_pos))?;
                    obj.serialize_entry("endPos", &JsPosition(end_pos))?;
                    obj.end()
                }
            }
            Error::TypecheckerError(typechecker_error) => match typechecker_error {
                TypecheckerError::Mismatch { token, expected, actual } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "mismatch")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("expected", &JsType(expected))?;
                    obj.serialize_entry("actual", &JsType(actual))?;
                    obj.end()
                }
                TypecheckerError::InvalidIfConditionType { token, actual } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "mismatch")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("actual", &JsType(actual))?;
                    obj.end()
                }
                TypecheckerError::InvalidOperator { token, op, ltype, rtype } => {
                    let mut obj = serializer.serialize_map(Some(6))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidOperator")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("op", &JsBinaryOp(op))?;
                    obj.serialize_entry("lType", &JsType(ltype))?;
                    obj.serialize_entry("rType", &JsType(rtype))?;
                    obj.end()
                }
                TypecheckerError::MissingRequiredAssignment { ident } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "missingRequiredAssignment")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.end()
                }
                TypecheckerError::DuplicateBinding { ident, orig_ident } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "duplicateBinding")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.serialize_entry("origIdent", &JsToken(orig_ident))?;
                    obj.end()
                }
                TypecheckerError::DuplicateField { ident, orig_ident, orig_is_field } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "duplicateField")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.serialize_entry("origIdent", &JsToken(orig_ident))?;
                    obj.serialize_entry("origType", if *orig_is_field { "field" } else { "method" })?;
                    obj.end()
                }
                TypecheckerError::DuplicateType { ident, orig_ident } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "duplicateType")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    if let Some(orig_ident) = orig_ident {
                        obj.serialize_entry("origIdent", &JsToken(orig_ident))?;
                    }
                    obj.end()
                }
                TypecheckerError::UnknownIdentifier { ident } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unknownIdentifier")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.end()
                }
                TypecheckerError::InvalidAssignmentTarget { token, .. } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidAssignmentTarget")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::AssignmentToImmutable { orig_ident, token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "assignmentToImmutable")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("origIdent", &JsToken(orig_ident))?;
                    obj.end()
                }
                TypecheckerError::UnannotatedUninitialized { ident, is_mutable } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unannotatedUninitialized")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.serialize_entry("isMutable", is_mutable)?;
                    obj.end()
                }
                TypecheckerError::UnknownType { type_ident } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unknownType")?;
                    obj.serialize_entry("typeIdent", &JsToken(type_ident))?;
                    obj.end()
                }
                TypecheckerError::MissingIfExprBranch { if_token, is_if_branch } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "missingIfExprBranch")?;
                    obj.serialize_entry("ifToken", &JsToken(if_token))?;
                    obj.serialize_entry("isIfBranch", is_if_branch)?;
                    obj.end()
                }
                TypecheckerError::IfExprBranchMismatch { if_token, if_type, else_type } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "ifExprBranchMismatch")?;
                    obj.serialize_entry("ifToken", &JsToken(if_token))?;
                    obj.serialize_entry("ifType", &JsType(if_type))?;
                    obj.serialize_entry("elseType", &JsType(else_type))?;
                    obj.end()
                }
                TypecheckerError::InvalidInvocationTarget { token, target_type } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidInvocationTarget")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("targetType", &JsType(target_type))?;
                    obj.end()
                }
                TypecheckerError::IncorrectArity { token, expected, actual } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "incorrectArity")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("expected", expected)?;
                    obj.serialize_entry("actual", actual)?;
                    obj.end()
                }
                TypecheckerError::UnexpectedParamName { token } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unexpectedParamName")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::DuplicateParamName { token } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "duplicateParamName")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::RecursiveRefWithoutReturnType { orig_token, token } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "recursiveRefWithoutReturnType")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("origToken", &JsToken(orig_token))?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::InvalidBreak(token) => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidBreak")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::InvalidRequiredArgPosition(token) => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidRequiredArgPosition")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::InvalidIndexingTarget { token, target_type } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidIndexingTarget")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("targetType", &JsType(target_type))?;
                    obj.end()
                }
                TypecheckerError::InvalidIndexingSelector { token, target_type, selector_type } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidIndexingSelector")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("targetType", &JsType(target_type))?;
                    obj.serialize_entry("selectorType", &JsType(selector_type))?;
                    obj.end()
                }
                TypecheckerError::UnknownMember { token, target_type } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unknownMember")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("targetType", &JsType(target_type))?;
                    obj.end()
                }
                TypecheckerError::MissingRequiredParams { token, missing_params } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "missingRequiredParams")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("missingParams", missing_params)?;
                    obj.end()
                }
                TypecheckerError::InvalidMixedParamType { token } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidMixedParamType")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::InvalidTypeFuncInvocation { token } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidTypeFuncInvocation")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::InvalidSelfParamPosition { token } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidSelfParamPosition")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::InvalidSelfParam { token } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidSelfParam")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::MissingRequiredTypeAnnotation { token } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "missingRequiredTypeAnnotation")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::InvalidTypeDeclDepth { token } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidTypeDeclDepth")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::ForbiddenUnknownType { token, .. } => {
                    let mut obj = serializer.serialize_map(Some(3))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "forbiddenUnknownType")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.end()
                }
                TypecheckerError::InvalidInstantiation { token, typ } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidInstantiation")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("type", &JsType(typ))?;
                    obj.end()
                }
            }
            Error::InterpretError(interpret_error) => match interpret_error {
                InterpretError::StackEmpty => {
                    let mut obj = serializer.serialize_map(Some(2))?;
                    obj.serialize_entry("kind", "interpretError")?;
                    obj.serialize_entry("subKind", "stackEmpty")?;
                    obj.end()
                }
                InterpretError::ConstIdxOutOfBounds => {
                    let mut obj = serializer.serialize_map(Some(2))?;
                    obj.serialize_entry("kind", "interpretError")?;
                    obj.serialize_entry("subKind", "constIdxOutOfBounds")?;
                    obj.end()
                }
                InterpretError::EndOfBytes => {
                    let mut obj = serializer.serialize_map(Some(2))?;
                    obj.serialize_entry("kind", "interpretError")?;
                    obj.serialize_entry("subKind", "endOfBytes")?;
                    obj.end()
                }
                InterpretError::TypeError(expected, actual) => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "interpretError")?;
                    obj.serialize_entry("subKind", "typeError")?;
                    obj.serialize_entry("expected", expected)?;
                    obj.serialize_entry("actual", actual)?;
                    obj.end()
                }
                InterpretError::StackOverflow => {
                    let mut obj = serializer.serialize_map(Some(2))?;
                    obj.serialize_entry("kind", "interpretError")?;
                    obj.serialize_entry("subKind", "stackOverflow")?;
                    obj.end()
                }
            }
        }
    }
}
