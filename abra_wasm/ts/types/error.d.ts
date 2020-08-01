import { Token } from './token'
import { Position, Range } from './position'
import { Type } from './abra-types'
import { BinaryOp } from './binary-op'

export type Error
    = Errors.ParseError
    | Errors.LexerError
    | Errors.TypecheckerError
    | Errors.InterpretError

interface BaseError {
    range: Range | null
}

export namespace Errors {
    export type ParseError
        = ParseErrors.UnexpectedToken
        | ParseErrors.UnexpectedEof
        | ParseErrors.ExpectedToken

    export namespace ParseErrors {
        interface UnexpectedToken extends BaseError {
            kind: 'parseError',
            subKind: 'unexpectedToken',
            token: Token
        }

        interface UnexpectedEof extends BaseError {
            kind: 'parseError',
            subKind: 'unexpectedEof'
        }

        interface ExpectedToken extends BaseError {
            kind: 'parseError',
            subKind: 'expectedToken',
            expectedType: string,
            token: Token
        }
    }

    export type LexerError
        = LexerErrors.UnexpectedEof
        | LexerErrors.UnexpectedChar
        | LexerErrors.UnterminatedString

    export namespace LexerErrors {
        interface UnexpectedEof extends BaseError {
            kind: 'lexerError',
            subKind: 'unexpectedEof',
            pos: Position
        }

        interface UnexpectedChar extends BaseError {
            kind: 'lexerError',
            subKind: 'unexpectedChar',
            pos: Position,
            char: string
        }

        interface UnterminatedString extends BaseError {
            kind: 'lexerError',
            subKind: 'unterminatedString',
            startPos: Position,
            endPos: Position
        }
    }

    export type TypecheckerError
        = TypecheckerErrors.Mismatch
        | TypecheckerErrors.InvalidIfConditionType
        | TypecheckerErrors.InvalidOperator
        | TypecheckerErrors.MissingRequiredAssignment
        | TypecheckerErrors.DuplicateBinding
        | TypecheckerErrors.DuplicateField
        | TypecheckerErrors.DuplicateType
        | TypecheckerErrors.UnknownIdentifier
        | TypecheckerErrors.InvalidAssignmentTarget
        | TypecheckerErrors.AssignmentToImmutable
        | TypecheckerErrors.UnannotatedUninitialized
        | TypecheckerErrors.UnknownType
        | TypecheckerErrors.MissingIfExprBranch
        | TypecheckerErrors.IfExprBranchMismatch
        | TypecheckerErrors.InvalidInvocationTarget
        | TypecheckerErrors.IncorrectArity
        | TypecheckerErrors.UnexpectedParamName
        | TypecheckerErrors.DuplicateParamName
        | TypecheckerErrors.RecursiveRefWithoutReturnType
        | TypecheckerErrors.InvalidBreak
        | TypecheckerErrors.InvalidRequiredArgPosition
        | TypecheckerErrors.InvalidIndexingTarget
        | TypecheckerErrors.InvalidIndexingSelector
        | TypecheckerErrors.UnknownMember
        | TypecheckerErrors.MissingRequiredParams
        | TypecheckerErrors.InvalidMixedParamType
        | TypecheckerErrors.InvalidTypeFuncInvocation
        | TypecheckerErrors.InvalidSelfParamPosition
        | TypecheckerErrors.InvalidSelfParam
        | TypecheckerErrors.MissingRequiredTypeAnnotation
        | TypecheckerErrors.InvalidTypeDeclDepth
        | TypecheckerErrors.ForbiddenUnknownType
        | TypecheckerErrors.InvalidInstantiation

    export namespace TypecheckerErrors {
        interface Mismatch extends BaseError {
            kind: 'typecheckerError',
            subKind: 'mismatch',
            token: Token,
            expected: Type,
            actual: Type
        }

        interface InvalidIfConditionType extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidIfConditionType',
            token: Token,
            actual: Type
        }

        interface InvalidOperator extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidOperator',
            token: Token,
            op: BinaryOp,
            lType: Type,
            rType: Type
        }

        interface MissingRequiredAssignment extends BaseError {
            kind: 'typecheckerError',
            subKind: 'missingRequiredAssignment',
            ident: Token
        }

        interface DuplicateBinding extends BaseError {
            kind: 'typecheckerError',
            subKind: 'duplicateBinding',
            ident: Token,
            origIdent: Token
        }

        interface DuplicateField extends BaseError {
            kind: 'typecheckerError',
            subKind: 'duplicateField',
            ident: Token,
            origIdent: Token
            origType: Type
        }

        interface DuplicateType extends BaseError {
            kind: 'typecheckerError',
            subKind: 'duplicateType',
            ident: Token,
            origIdent: Token | null
        }

        interface UnknownIdentifier extends BaseError {
            kind: 'typecheckerError',
            subKind: 'unknownIdentifier',
            ident: Token
        }

        interface InvalidAssignmentTarget extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidAssignmentTarget',
            token: Token
        }

        interface AssignmentToImmutable extends BaseError {
            kind: 'typecheckerError',
            subKind: 'assignmentToImmutable',
            token: Token,
            origIdent: Token,
        }

        interface UnannotatedUninitialized extends BaseError {
            kind: 'typecheckerError',
            subKind: 'unannotatedUninitialized',
            ident: Token,
            isMutable: boolean
        }

        interface UnknownType extends BaseError {
            kind: 'typecheckerError',
            subKind: 'unknownType',
            typeIdent: Token
        }

        interface MissingIfExprBranch extends BaseError {
            kind: 'typecheckerError',
            subKind: 'missingIfExprBranch',
            ifToken: Token,
            isIfBranch: boolean
        }

        interface IfExprBranchMismatch extends BaseError {
            kind: 'typecheckerError',
            subKind: 'ifExprBranchMismatch',
            ifToken: Token,
            ifType: Type,
            elseType: Type,
        }

        interface InvalidInvocationTarget extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidInvocationTarget',
            token: Token,
            targetType: Type
        }

        interface IncorrectArity extends BaseError {
            kind: 'typecheckerError',
            subKind: 'incorrectArity',
            token: Token,
            expected: number,
            actual: number
        }

        interface UnexpectedParamName extends BaseError {
            kind: 'typecheckerError',
            subKind: 'unexpectedParamName',
            token: Token
        }

        interface DuplicateParamName extends BaseError {
            kind: 'typecheckerError',
            subKind: 'duplicateParamName',
            token: Token
        }

        interface RecursiveRefWithoutReturnType extends BaseError {
            kind: 'typecheckerError',
            subKind: 'recursiveRefWithoutReturnType',
            token: Token,
            origToken: Token
        }

        interface InvalidBreak extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidBreak',
            token: Token
        }

        interface InvalidRequiredArgPosition extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidRequiredArgPosition',
            token: Token
        }

        interface InvalidIndexingTarget extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidIndexingTarget',
            token: Token,
            targetType: Type
        }

        interface InvalidIndexingSelector extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidIndexingSelector',
            token: Token,
            targetType: Type,
            selectorType: Type
        }

        interface UnknownMember extends BaseError {
            kind: 'typecheckerError',
            subKind: 'unknownMember',
            token: Token,
            targetType: Type
        }

        interface MissingRequiredParams extends BaseError {
            kind: 'typecheckerError',
            subKind: 'missingRequiredParams',
            token: Token,
            missingParams: string[]
        }

        interface InvalidMixedParamType extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidMixedParamType',
            token: Token
        }

        interface InvalidTypeFuncInvocation extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidTypeFuncInvocation',
            token: Token
        }

        interface InvalidSelfParamPosition extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidSelfParamPosition',
            token: Token
        }

        interface InvalidSelfParam extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidSelfParam',
            token: Token
        }

        interface MissingRequiredTypeAnnotation extends BaseError {
            kind: 'typecheckerError',
            subKind: 'missingRequiredTypeAnnotation',
            token: Token
        }

        interface InvalidTypeDeclDepth extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidTypeDeclDepth',
            token: Token
        }

        interface ForbiddenUnknownType extends BaseError {
            kind: 'typecheckerError',
            subKind: 'forbiddenUnknownType',
            token: Token
        }

        interface InvalidInstantiation extends BaseError {
            kind: 'typecheckerError',
            subKind: 'invalidInstantiation',
            token: Token,
            type: Type
        }
    }

    export type InterpretError
        = InterpretErrors.StackEmpty
        | InterpretErrors.ConstIdxOutOfBounds
        | InterpretErrors.EndOfBytes
        | InterpretErrors.TypeError

    export namespace InterpretErrors {
        interface StackEmpty extends BaseError {
            kind: 'interpretError',
            subKind: 'stackEmpty'
        }

        interface ConstIdxOutOfBounds extends BaseError {
            kind: 'interpretError',
            subKind: 'constIdxOutOfBounds'
        }

        interface EndOfBytes extends BaseError {
            kind: 'interpretError',
            subKind: 'endOfBytes'
        }

        interface TypeError extends BaseError {
            kind: 'interpretError',
            subKind: 'typeError',
            expected: string,
            actual: string
        }
    }
}