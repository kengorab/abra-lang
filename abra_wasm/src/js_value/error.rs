use crate::js_value::token::{JsToken, JsRange};
use crate::js_value::position::JsPosition;
use crate::js_value::abra_type::JsType;
use crate::js_value::binary_op::JsBinaryOp;
use abra_core::Error;
use abra_core::parser::parse_error::ParseErrorKind;
use abra_core::lexer::lexer_error::LexerErrorKind;
use abra_core::typechecker::typechecker_error::TypecheckerErrorKind;
use abra_core::vm::vm::InterpretError;
use serde::{Serialize, Serializer};
use abra_core::parser::ast::BindingPattern;

pub struct JsBindingPattern<'a>(pub &'a BindingPattern);

impl<'a> Serialize for JsBindingPattern<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match &self.0 {
            BindingPattern::Variable(ident) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "variable")?;
                obj.serialize_entry("ident", &JsToken(ident))?;
                obj.end()
            }
            BindingPattern::Tuple(lparen_tok, patterns) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "tuple")?;
                obj.serialize_entry("lparenToken", &JsToken(lparen_tok))?;
                obj.serialize_entry("patterns", &patterns.iter().map(|p| JsBindingPattern(p)).collect::<Vec<_>>())?;
                obj.end()
            }
            BindingPattern::Array(lbrack_tok, patterns, is_string) => {
                let mut obj = serializer.serialize_map(Some(4))?;
                obj.serialize_entry("kind", "array")?;
                obj.serialize_entry("lbrackToken", &JsToken(lbrack_tok))?;
                obj.serialize_entry("isString", is_string)?;
                obj.serialize_entry("patterns", &patterns.iter().map(|(pat, is_splat)| (JsBindingPattern(pat), is_splat)).collect::<Vec<_>>())?;
                obj.end()
            }
        }
    }
}

pub struct JsWrappedError<'a>(pub &'a Error, pub &'a str);

impl<'a> Serialize for JsWrappedError<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match &self.0 {
            Error::ParseError(parse_error) => match &parse_error.kind {
                ParseErrorKind::UnexpectedToken(token) => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "parseError")?;
                    obj.serialize_entry("subKind", "unexpectedToken")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&token.get_range()))?;
                    obj.end()
                }
                ParseErrorKind::UnexpectedEof(range) => {
                    let mut obj = serializer.serialize_map(Some(2))?;
                    obj.serialize_entry("kind", "parseError")?;
                    obj.serialize_entry("subKind", "unexpectedEof")?;
                    obj.serialize_entry("range", &JsRange(range))?;
                    obj.end()
                }
                ParseErrorKind::ExpectedToken(token_type, token) => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "parseError")?;
                    obj.serialize_entry("subKind", "expectedToken")?;
                    obj.serialize_entry("expectedType", &token_type.to_string())?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&token.get_range()))?;
                    obj.end()
                }
                ParseErrorKind::ExpectedOneOf(token_types, token) => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "parseError")?;
                    obj.serialize_entry("subKind", "expectedOneOf")?;
                    obj.serialize_entry("expected", &token_types.iter().map(|tt| tt.to_string()).collect::<Vec<_>>())?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&token.get_range()))?;
                    obj.end()
                }
            }
            Error::LexerError(lexer_error) => match &lexer_error.kind {
                LexerErrorKind::UnexpectedEof(pos) => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "lexerError")?;
                    obj.serialize_entry("subKind", "unexpectedEof")?;
                    obj.serialize_entry("pos", &JsPosition(pos))?;
                    obj.serialize_entry("range", &JsRange(&lexer_error.get_range()))?;
                    obj.end()
                }
                LexerErrorKind::UnexpectedChar(pos, ch) => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "lexerError")?;
                    obj.serialize_entry("subKind", "unexpectedChar")?;
                    obj.serialize_entry("pos", &JsPosition(pos))?;
                    obj.serialize_entry("char", ch)?;
                    obj.serialize_entry("range", &JsRange(&lexer_error.get_range()))?;
                    obj.end()
                }
                LexerErrorKind::UnterminatedString(start_pos, end_pos) => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "lexerError")?;
                    obj.serialize_entry("subKind", "unterminatedString")?;
                    obj.serialize_entry("startPos", &JsPosition(start_pos))?;
                    obj.serialize_entry("endPos", &JsPosition(end_pos))?;
                    obj.serialize_entry("range", &JsRange(&lexer_error.get_range()))?;
                    obj.end()
                }
                LexerErrorKind::UnsupportedEscapeSequence(pos, s, is_unicode) => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "lexerError")?;
                    obj.serialize_entry("subKind", "unsupportedEscapeSequence")?;
                    obj.serialize_entry("pos", &JsPosition(pos))?;
                    obj.serialize_entry("string", s)?;
                    obj.serialize_entry("isUnicode", is_unicode)?;
                    obj.serialize_entry("range", &JsRange(&lexer_error.get_range()))?;
                    obj.end()
                }
            }
            Error::TypecheckerError(typechecker_error) => match &typechecker_error.kind {
                TypecheckerErrorKind::Unimplemented(token, message) => {
                    let mut obj = serializer.serialize_map(Some(6))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unimplemented")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("message", message)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::Mismatch { token, expected, actual } => {
                    let mut obj = serializer.serialize_map(Some(6))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "mismatch")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("expected", &JsType(expected))?;
                    obj.serialize_entry("actual", &JsType(actual))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidIfConditionType { token, actual } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidIfConditionType")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("actual", &JsType(actual))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidLoopTarget { target_type, .. } => {
                    let mut obj = serializer.serialize_map(Some(2))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidLoopTarget")?;
                    obj.serialize_entry("targetType", &JsType(target_type))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidOperator { token, op, ltype, rtype } => {
                    let mut obj = serializer.serialize_map(Some(7))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidOperator")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("op", &JsBinaryOp(op))?;
                    obj.serialize_entry("lType", &JsType(ltype))?;
                    obj.serialize_entry("rType", &JsType(rtype))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::MissingRequiredAssignment { ident } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "missingRequiredAssignment")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::DuplicateBinding { ident, orig_ident } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "duplicateBinding")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    if let Some(orig_ident) = orig_ident {
                        obj.serialize_entry("origIdent", &JsToken(orig_ident))?;
                    }
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::DuplicateField { ident, orig_ident, orig_is_field, orig_is_enum_variant } => {
                    let mut obj = serializer.serialize_map(Some(6))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "duplicateField")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.serialize_entry("origIdent", &JsToken(orig_ident))?;
                    obj.serialize_entry("origType", if *orig_is_field { "field" } else if *orig_is_enum_variant { "enum variant" } else { "method" })?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::DuplicateType { ident, orig_ident } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "duplicateType")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    if let Some(orig_ident) = orig_ident {
                        obj.serialize_entry("origIdent", &JsToken(orig_ident))?;
                    }
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::DuplicateTypeArgument { ident, orig_ident } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "duplicateTypeArgument")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.serialize_entry("origIdent", &JsToken(orig_ident))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::UnboundGeneric(ident, generic_name) => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unboundGeneric")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.serialize_entry("genericName", generic_name)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::UnknownIdentifier { ident } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unknownIdentifier")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidAssignmentTarget { token, .. } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidAssignmentTarget")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::AssignmentToImmutable { orig_ident, token } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "assignmentToImmutable")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("origIdent", &JsToken(orig_ident))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::UnannotatedUninitialized { ident, is_mutable } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unannotatedUninitialized")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.serialize_entry("isMutable", is_mutable)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::UnknownType { type_ident } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unknownType")?;
                    obj.serialize_entry("typeIdent", &JsToken(type_ident))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::MissingIfExprBranch { if_token, is_if_branch } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "missingIfExprBranch")?;
                    obj.serialize_entry("ifToken", &JsToken(if_token))?;
                    obj.serialize_entry("isIfBranch", is_if_branch)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::IfExprBranchMismatch { if_token, if_type, else_type } => {
                    let mut obj = serializer.serialize_map(Some(6))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "ifExprBranchMismatch")?;
                    obj.serialize_entry("ifToken", &JsToken(if_token))?;
                    obj.serialize_entry("ifType", &JsType(if_type))?;
                    obj.serialize_entry("elseType", &JsType(else_type))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidInvocationTarget { token, target_type } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidInvocationTarget")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("targetType", &JsType(target_type))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::IncorrectArity { token, expected, actual } => {
                    let mut obj = serializer.serialize_map(Some(6))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "incorrectArity")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("expected", expected)?;
                    obj.serialize_entry("actual", actual)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::UnexpectedParamName { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unexpectedParamName")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::DuplicateParamName { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "duplicateParamName")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidTerminator(token) => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidBreak")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                // TypecheckerError::InvalidReturn(token) => {
                //     let mut obj = serializer.serialize_map(Some(4))?;
                //     obj.serialize_entry("kind", "typecheckerError")?;
                //     obj.serialize_entry("subKind", "invalidReturn")?;
                //     obj.serialize_entry("token", &JsToken(token))?;
                //     obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                //     obj.end()
                // }
                TypecheckerErrorKind::InvalidRequiredArgPosition(token) => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidRequiredArgPosition")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidIndexingTarget { token, target_type, .. } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidIndexingTarget")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("targetType", &JsType(target_type))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidIndexingSelector { token, target_type, selector_type } => {
                    let mut obj = serializer.serialize_map(Some(6))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidIndexingSelector")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("targetType", &JsType(target_type))?;
                    obj.serialize_entry("selectorType", &JsType(selector_type))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidTupleIndexingSelector { token, types, non_constant, index } => {
                    let mut obj = serializer.serialize_map(Some(6))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidTupleIndexingSelector")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    let types: Vec<JsType> = types.iter().map(|t| JsType(t)).collect();
                    obj.serialize_entry("type", &types)?;
                    obj.serialize_entry("nonConstant", &non_constant)?;
                    obj.serialize_entry("index", &index)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::UnknownMember { token, target_type } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unknownMember")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("targetType", &JsType(target_type))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::MissingRequiredParams { token, missing_params } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "missingRequiredParams")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("missingParams", missing_params)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidMixedParamType { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidMixedParamType")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidTypeFuncInvocation { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidTypeFuncInvocation")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidSelfParamPosition { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidSelfParamPosition")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidSelfParam { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidSelfParam")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidTypeDeclDepth { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidTypeDeclDepth")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::ForbiddenVariableType { binding, .. } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "forbiddenVariableType")?;
                    obj.serialize_entry("binding", &JsBindingPattern(binding))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidInstantiation { token, typ } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidInstantiation")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("type", &JsType(typ))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidTypeArgumentArity { token, actual_type, expected, actual } => {
                    let mut obj = serializer.serialize_map(Some(7))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "missingTypeArguments")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("actualType", &JsType(actual_type))?;
                    obj.serialize_entry("expected", expected)?;
                    obj.serialize_entry("actual", actual)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::UnreachableMatchCase { token, typ, is_unreachable_none } => {
                    let mut obj = serializer.serialize_map(Some(6))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unreachableMatchCase")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    if let Some(typ) = typ {
                        obj.serialize_entry("type", &JsType(typ))?;
                    }
                    obj.serialize_entry("isUnreachableNone", is_unreachable_none)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::DuplicateMatchCase { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "duplicateMatchCase")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::NonExhaustiveMatch { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "nonExhaustiveMatch")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::EmptyMatchBlock { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "emptyMatchBlock")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::MatchBranchMismatch { token, expected, actual } => {
                    let mut obj = serializer.serialize_map(Some(6))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "matchBranchMismatch")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("expected", &JsType(expected))?;
                    obj.serialize_entry("actual", &JsType(actual))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidUninitializedEnumVariant { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidUninitializedEnumVariant")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidMatchCaseDestructuring { token, typ, enum_variant } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidDestructuring")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    if let Some(typ) = typ {
                        obj.serialize_entry("type", &JsType(typ))?;
                    }
                    if let Some(variant_name) = enum_variant {
                        obj.serialize_entry("variantName", variant_name)?;
                    }
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidMatchCaseDestructuringArity { token, typ, enum_variant, expected, actual } => {
                    let mut obj = serializer.serialize_map(Some(7))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidDestructuring")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("type", &JsType(typ))?;
                    if let Some(variant_name) = enum_variant {
                        obj.serialize_entry("variantName", variant_name)?;
                    }
                    obj.serialize_entry("expected", expected)?;
                    obj.serialize_entry("actual", actual)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidAssignmentDestructuring { binding, typ } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidDestructuring")?;
                    obj.serialize_entry("binding", &JsBindingPattern(binding))?;
                    obj.serialize_entry("type", &JsType(typ))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::UnreachableCode { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "unreachableCode")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::ReturnTypeMismatch { token, fn_name, fn_missing_ret_ann, bare_return, expected, actual } => {
                    let mut obj = serializer.serialize_map(Some(9))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "returnTypeMismatch")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("fnName", fn_name)?;
                    obj.serialize_entry("fnMissingReturnAnnotation", fn_missing_ret_ann)?;
                    obj.serialize_entry("bareReturn", bare_return)?;
                    obj.serialize_entry("expected", &JsType(expected))?;
                    obj.serialize_entry("actual", &JsType(actual))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::DuplicateSplatDestructuring { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "duplicateSplatDestructuring")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidProtocolMethod { token, fn_name, expected, actual } => {
                    let mut obj = serializer.serialize_map(Some(7))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidProtocolMethod")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("fnName", fn_name)?;
                    obj.serialize_entry("expected", &JsType(expected))?;
                    obj.serialize_entry("actual", &JsType(actual))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidVarargPosition(token) => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidVarargPosition")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidVarargUsage(token) => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidVarargUsage")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::VarargMismatch { token, typ } => {
                    let mut obj = serializer.serialize_map(Some(5))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "varargMismatch")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("type", &JsType(typ))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidAccess { token, is_field, is_get } => {
                    let mut obj = serializer.serialize_map(Some(6))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidAccess")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("isField", is_field)?;
                    obj.serialize_entry("isGet", is_get)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidExportDepth { token } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidExportDepth")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidImportValue { ident } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidImportValue")?;
                    obj.serialize_entry("ident", &JsToken(ident))?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
                    obj.end()
                }
                TypecheckerErrorKind::InvalidModuleImport { token, module_name, circular } => {
                    let mut obj = serializer.serialize_map(Some(4))?;
                    obj.serialize_entry("kind", "typecheckerError")?;
                    obj.serialize_entry("subKind", "invalidModuleImport")?;
                    obj.serialize_entry("token", &JsToken(token))?;
                    obj.serialize_entry("moduleName", module_name)?;
                    obj.serialize_entry("circular", circular)?;
                    obj.serialize_entry("range", &JsRange(&typechecker_error.get_token().get_range()))?;
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
