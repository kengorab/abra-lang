import { Position } from './position'

export type Token
    = Tokens.Int
    | Tokens.Float
    | Tokens.String
    | Tokens.Bool
    | Tokens.Func
    | Tokens.Val
    | Tokens.Var
    | Tokens.If
    | Tokens.Else
    | Tokens.While
    | Tokens.Break
    | Tokens.For
    | Tokens.In
    | Tokens.Type
    | Tokens.Ident
    | Tokens.Self
    | Tokens.None
    | Tokens.Assign
    | Tokens.Plus
    | Tokens.PlusEq
    | Tokens.Minus
    | Tokens.MinusEq
    | Tokens.Star
    | Tokens.StarEq
    | Tokens.Slash
    | Tokens.SlashEq
    | Tokens.And
    | Tokens.AndEq
    | Tokens.Or
    | Tokens.OrEq
    | Tokens.Elvis
    | Tokens.ElvisEq
    | Tokens.GT
    | Tokens.GTE
    | Tokens.LT
    | Tokens.LTE
    | Tokens.Eq
    | Tokens.Neq
    | Tokens.Bang
    | Tokens.LParen
    | Tokens.RParen
    | Tokens.LBrack
    | Tokens.RBrack
    | Tokens.LBrace
    | Tokens.RBrace
    | Tokens.Colon
    | Tokens.Comma
    | Tokens.Question
    | Tokens.Dot
    | Tokens.QuestionDot
    | Tokens.Arrow

export namespace Tokens {
    interface Int {
        kind: 'int',
        pos: Position,
        val: number
    }

    interface Float {
        kind: 'float',
        pos: Position,
        val: number
    }

    interface String {
        kind: 'string',
        pos: Position,
        val: string
    }

    interface Bool {
        kind: 'bool',
        pos: Position,
        val: boolean
    }

    interface Func {
        kind: 'func',
        pos: Position,
    }

    interface Val {
        kind: 'val',
        pos: Position,
    }

    interface Var {
        kind: 'var',
        pos: Position,
    }

    interface If {
        kind: 'if',
        pos: Position,
    }

    interface Else {
        kind: 'else',
        pos: Position,
    }

    interface While {
        kind: 'while',
        pos: Position,
    }

    interface Break {
        kind: 'break',
        pos: Position,
    }

    interface For {
        kind: 'for',
        pos: Position,
    }

    interface In {
        kind: 'in',
        pos: Position,
    }

    interface Type {
        kind: 'type',
        pos: Position,
    }

    interface Ident {
        kind: 'ident',
        pos: Position,
        val: string
    }

    interface Self {
        kind: 'self',
        pos: Position
    }

    interface None {
        kind: 'none',
        pos: Position
    }

    interface Assign {
        kind: 'assign',
        pos: Position,
    }

    interface Plus {
        kind: 'plus',
        pos: Position,
    }

    interface PlusEq {
        kind: 'plusEq',
        pos: Position,
    }

    interface Minus {
        kind: 'minus',
        pos: Position,
    }

    interface MinusEq {
        kind: 'minusEq',
        pos: Position,
    }

    interface Star {
        kind: 'star',
        pos: Position,
    }

    interface StarEq {
        kind: 'starEq',
        pos: Position,
    }

    interface Slash {
        kind: 'slash',
        pos: Position,
    }

    interface SlashEq {
        kind: 'slashEq',
        pos: Position,
    }

    interface And {
        kind: 'and',
        pos: Position,
    }

    interface AndEq {
        kind: 'andEq',
        pos: Position,
    }

    interface Or {
        kind: 'or',
        pos: Position,
    }

    interface OrEq {
        kind: 'orEq',
        pos: Position,
    }

    interface Elvis {
        kind: 'elvis',
        pos: Position,
    }

    interface ElvisEq {
        kind: 'elvisEq',
        pos: Position,
    }

    interface GT {
        kind: 'gt',
        pos: Position,
    }

    interface GTE {
        kind: 'gte',
        pos: Position,
    }

    interface LT {
        kind: 'lt',
        pos: Position,
    }

    interface LTE {
        kind: 'lte',
        pos: Position,
    }

    interface Eq {
        kind: 'eq',
        pos: Position,
    }

    interface Neq {
        kind: 'neq',
        pos: Position,
    }

    interface Bang {
        kind: 'bang',
        pos: Position,
    }

    interface LParen {
        kind: 'lParen',
        pos: Position,
    }

    interface RParen {
        kind: 'rParen',
        pos: Position,
    }

    interface LBrack {
        kind: 'lBrack',
        pos: Position,
    }

    interface RBrack {
        kind: 'rBrack',
        pos: Position,
    }

    interface LBrace {
        kind: 'lBrace',
        pos: Position,
    }

    interface RBrace {
        kind: 'rBrace',
        pos: Position,
    }

    interface Colon {
        kind: 'colon',
        pos: Position,
    }

    interface Comma {
        kind: 'comma',
        pos: Position,
    }

    interface Question {
        kind: 'question',
        pos: Position,
    }

    interface Dot {
        kind: 'dot',
        pos: Position,
    }

    interface QuestionDot {
        kind: 'questionDot',
        pos: Position,
    }

    interface Arrow {
        kind: 'arrow',
        pos: Position,
    }
}
