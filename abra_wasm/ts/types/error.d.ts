import { Token } from './token'
import { Position } from './position'
import { Type } from './abra-types'
import { BinaryOp } from './binary-op'

export type Error
    = Errors.ParseError
    | Errors.LexerError
    | Errors.TypecheckerError
    | Errors.InterpretError

export namespace Errors {
    export type ParseError
        = ParseErrors.UnexpectedToken
        | ParseErrors.UnexpectedEof
        | ParseErrors.ExpectedToken
        | ParseErrors.Raw

    export namespace ParseErrors {
        interface UnexpectedToken {
            kind: 'parseError',
            subKind: 'unexpectedToken',
            token: Token
        }

        interface UnexpectedEof {
            kind: 'parseError',
            subKind: 'unexpectedEof'
        }

        interface ExpectedToken {
            kind: 'parseError',
            subKind: 'expectedToken',
            expectedType: string,
            token: Token
        }

        interface Raw {
            kind: 'parseError',
            subKind: 'raw',
            msg: string
        }
    }

    export type LexerError
        = LexerErrors.UnexpectedEof
        | LexerErrors.UnexpectedChar
        | LexerErrors.UnterminatedString

    export namespace LexerErrors {
        interface UnexpectedEof {
            kind: 'lexerError',
            subKind: 'unexpectedEof',
            pos: Position
        }

        interface UnexpectedChar {
            kind: 'lexerError',
            subKind: 'unexpectedChar',
            pos: Position,
            char: string
        }

        interface UnterminatedString {
            kind: 'lexerError',
            subKind: 'unterminatedString',
            startPos: Position,
            endPos: Position
        }
    }

    export type TypecheckerError
        = TypecheckerErrors.Mismatch
        | TypecheckerErrors.InvalidOperator
        | TypecheckerErrors.MissingRequiredAssignment
        | TypecheckerErrors.DuplicateBinding
        | TypecheckerErrors.UnknownIdentifier
        | TypecheckerErrors.InvalidAssignmentTarget
        | TypecheckerErrors.AssignmentToImmutable
        | TypecheckerErrors.UnannotatedUninitialized
        | TypecheckerErrors.UnknownType
        | TypecheckerErrors.MissingIfExprBranch
        | TypecheckerErrors.IfExprBranchMismatch
        | TypecheckerErrors.InvalidInvocationTarget
        | TypecheckerErrors.IncorrectArity
        | TypecheckerErrors.ParamNameMismatch

    export namespace TypecheckerErrors {
        interface Mismatch {
            kind: 'typecheckerError',
            subKind: 'mismatch',
            token: Token,
            expected: Type,
            actual: Type
        }

        interface InvalidOperator {
            kind: 'typecheckerError',
            subKind: 'invalidOperator',
            token: Token,
            op: BinaryOp,
            lType: Type,
            rType: Type
        }

        interface MissingRequiredAssignment {
            kind: 'typecheckerError',
            subKind: 'missingRequiredAssignment',
            ident: Token
        }

        interface DuplicateBinding {
            kind: 'typecheckerError',
            subKind: 'duplicateBinding',
            ident: Token,
            origIdent: Token
        }

        interface UnknownIdentifier {
            kind: 'typecheckerError',
            subKind: 'unknownIdentifier',
            ident: Token
        }

        interface InvalidAssignmentTarget {
            kind: 'typecheckerError',
            subKind: 'invalidAssignmentTarget',
            token: Token
        }

        interface AssignmentToImmutable {
            kind: 'typecheckerError',
            subKind: 'assignmentToImmutable',
            token: Token,
            origIdent: Token,
        }

        interface UnannotatedUninitialized {
            kind: 'typecheckerError',
            subKind: 'unannotatedUninitialized',
            ident: Token,
            isImmutable: boolean
        }

        interface UnknownType {
            kind: 'typecheckerError',
            subKind: 'unknownType',
            typeIdent: Token
        }

        interface MissingIfExprBranch {
            kind: 'typecheckerError',
            subKind: 'missingIfExprBranch',
            typeIdent: Token
        }

        interface IfExprBranchMismatch {
            kind: 'typecheckerError',
            subKind: 'ifExprBranchMismatch',
            ifToken: Token,
            isIfBranch: boolean
        }

        interface InvalidInvocationTarget {
            kind: 'typecheckerError',
            subKind: 'invalidInvocationTarget',
            token: Token
        }

        interface IncorrectArity {
            kind: 'typecheckerError',
            subKind: 'incorrectArity',
            token: Token,
            expected: number,
            actual: number
        }

        interface ParamNameMismatch {
            kind: 'typecheckerError',
            subKind: 'paramNameMismatch',
            token: Token,
            expected: string,
            actual: string
        }
    }

    export type InterpretError
        = InterpretErrors.StackEmpty
        | InterpretErrors.ConstIdxOutOfBounds
        | InterpretErrors.EndOfBytes
        | InterpretErrors.TypeError

    export namespace InterpretErrors {
        interface StackEmpty {
            kind: 'interpretError',
            subKind: 'stackEmpty'
        }

        interface ConstIdxOutOfBounds {
            kind: 'interpretError',
            subKind: 'constIdxOutOfBounds'
        }

        interface EndOfBytes {
            kind: 'interpretError',
            subKind: 'endOfBytes'
        }

        interface TypeError {
            kind: 'interpretError',
            subKind: 'typeError',
            expected: string,
            actual: string
        }
    }
}