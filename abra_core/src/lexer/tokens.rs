use std::cmp::{max, min, Ordering};

#[derive(Debug, Clone, Eq, PartialEq, Ord)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

pub const POSITION_BOGUS: Position = Position { line: 0, col: 0 };

impl Position {
    pub fn new(line: usize, col: usize) -> Self { Position { line, col } }
}

impl Default for Position {
    fn default() -> Self { Position { line: 0, col: 0 } }
}

impl PartialOrd<Self> for Position {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(if self.line < other.line {
            Ordering::Less
        } else if self.line > other.line {
            Ordering::Greater
        } else {
            if self.col < other.col {
                Ordering::Less
            } else if self.col > other.col {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub fn with_length(start: &Position, length: usize) -> Range {
        Self {
            start: start.clone(),
            end: Position { line: start.line, col: start.col + length },
        }
    }

    pub fn expand(&self, other: &Range) -> Range {
        let start = min(&self.start, &other.start).clone();
        let end = max(&self.end, &other.end).clone();

        Self { start, end }
    }
}

#[derive(Debug, Display, Clone, PartialEq, EnumString, EnumDiscriminants)]
#[strum_discriminants(name(TokenType), derive(Display))]
pub enum Token {
    // Builtin types
    #[strum(to_string = "int", serialize = "Int")] Int(Position, i64),
    #[strum(to_string = "float", serialize = "Float")] Float(Position, f64),
    #[strum(to_string = "string", serialize = "String")] String(Position, String),
    #[strum(to_string = "string", serialize = "StringInterp")] StringInterp(Position, Vec<Token>),
    #[strum(to_string = "boolean", serialize = "Bool")] Bool(Position, bool),

    // Keywords
    #[strum(to_string = "func", serialize = "Func")] Func(Position),
    #[strum(to_string = "val", serialize = "Val")] Val(Position),
    #[strum(to_string = "var", serialize = "Var")] Var(Position),
    #[strum(to_string = "if", serialize = "If")] If(Position),
    #[strum(to_string = "else", serialize = "Else")] Else(Position),
    #[strum(to_string = "while", serialize = "While")] While(Position),
    #[strum(to_string = "break", serialize = "Break")] Break(Position),
    #[strum(to_string = "continue", serialize = "Continue")] Continue(Position),
    #[strum(to_string = "for", serialize = "For")] For(Position),
    #[strum(to_string = "in", serialize = "In")] In(Position),
    #[strum(to_string = "match", serialize = "Match")] Match(Position),
    #[strum(to_string = "type", serialize = "Type")] Type(Position),
    #[strum(to_string = "enum", serialize = "Enum")] Enum(Position),
    #[strum(to_string = "return", serialize = "Return")] Return(Position, bool),
    #[strum(to_string = "readonly", serialize = "Readonly")] Readonly(Position),
    #[strum(to_string = "import", serialize = "Import")] Import(Position),
    #[strum(to_string = "export", serialize = "Export")] Export(Position),
    #[strum(to_string = "from", serialize = "From")] From(Position),
    #[strum(to_string = "as", serialize = "As")] As(Position),
    #[strum(to_string = "try", serialize = "Try")] Try(Position),

    // Identifiers
    #[strum(to_string = "identifier", serialize = "Ident")] Ident(Position, String),
    #[strum(to_string = "self", serialize = "Self")] Self_(Position),
    #[strum(to_string = "none", serialize = "None")] None(Position),

    // Operators
    #[strum(to_string = "=", serialize = "Assign")] Assign(Position),
    #[strum(to_string = "+", serialize = "Plus")] Plus(Position),
    #[strum(to_string = "+=", serialize = "PlusEq")] PlusEq(Position),
    #[strum(to_string = "-", serialize = "Minus")] Minus(Position),
    #[strum(to_string = "-=", serialize = "MinusEq")] MinusEq(Position),
    #[strum(to_string = "*", serialize = "Star")] Star(Position),
    #[strum(to_string = "*=", serialize = "StarEq")] StarEq(Position),
    #[strum(to_string = "/", serialize = "Slash")] Slash(Position),
    #[strum(to_string = "/=", serialize = "SlashEq")] SlashEq(Position),
    #[strum(to_string = "%", serialize = "Percent")] Percent(Position),
    #[strum(to_string = "%=", serialize = "PercentEq")] PercentEq(Position),
    #[strum(to_string = "&&", serialize = "And")] And(Position),
    #[strum(to_string = "&&=", serialize = "AndEq")] AndEq(Position),
    #[strum(to_string = "||", serialize = "Or")] Or(Position),
    #[strum(to_string = "||=", serialize = "OrEq")] OrEq(Position),
    #[strum(to_string = "^", serialize = "Caret")] Caret(Position),
    #[strum(to_string = "?:", serialize = "Elvis")] Elvis(Position),
    #[strum(to_string = "?:=", serialize = "ElvisEq")] ElvisEq(Position),
    #[strum(to_string = ">", serialize = "GT")] GT(Position),
    #[strum(to_string = ">=", serialize = "GTE")] GTE(Position),
    #[strum(to_string = "<", serialize = "LT")] LT(Position),
    #[strum(to_string = "<=", serialize = "LTE")] LTE(Position),
    #[strum(to_string = "==", serialize = "Eq")] Eq(Position),
    #[strum(to_string = "!=", serialize = "Neq")] Neq(Position),
    #[strum(to_string = "!", serialize = "Bang")] Bang(Position),
    #[strum(to_string = "**", serialize = "StarStar")] StarStar(Position),

    // Delimiters
    #[strum(to_string = "(", serialize = "LParen")] LParen(Position, /* is_preceded_by_newline: */ bool),
    #[strum(to_string = ")", serialize = "RParen")] RParen(Position),
    #[strum(to_string = "[", serialize = "LBrack")] LBrack(Position, /* is_preceded_by_newline: */ bool),
    #[strum(to_string = "]", serialize = "RBrack")] RBrack(Position),
    #[strum(to_string = "{", serialize = "LBrace")] LBrace(Position),
    #[strum(to_string = "}", serialize = "RBrace")] RBrace(Position),
    #[strum(to_string = "#{", serialize = "LBraceHash")] LBraceHash(Position),
    #[strum(to_string = "|", serialize = "Pipe")] Pipe(Position),
    #[strum(to_string = ":", serialize = "Colon")] Colon(Position),
    #[strum(to_string = ",", serialize = "Comma")] Comma(Position),
    #[strum(to_string = "?", serialize = "Question")] Question(Position),
    #[strum(to_string = ".", serialize = "Dot")] Dot(Position),
    #[strum(to_string = "?.", serialize = "QuestionDot")] QuestionDot(Position),
    #[strum(to_string = "=>", serialize = "Arrow")] Arrow(Position),
    #[strum(to_string = "@", serialize = "At")] At(Position),
}

impl Token {
    pub fn get_position(&self) -> Position {
        let pos = match self {
            Token::Int(pos, _) |
            Token::Float(pos, _) |
            Token::String(pos, _) |
            Token::StringInterp(pos, _) |
            Token::Bool(pos, _) |

            Token::Func(pos) |
            Token::Val(pos) |
            Token::Var(pos) |
            Token::If(pos) |
            Token::Else(pos) |
            Token::While(pos) |
            Token::Break(pos) |
            Token::Continue(pos) |
            Token::For(pos) |
            Token::In(pos) |
            Token::Match(pos) |
            Token::Type(pos) |
            Token::Enum(pos) |
            Token::Return(pos, _) |
            Token::Readonly(pos) |
            Token::Import(pos) |
            Token::Export(pos) |
            Token::From(pos) |
            Token::As(pos) |
            Token::Try(pos) |

            Token::Ident(pos, _) |
            Token::Self_(pos) |
            Token::None(pos) |

            Token::Assign(pos) |
            Token::Plus(pos) |
            Token::PlusEq(pos) |
            Token::Minus(pos) |
            Token::MinusEq(pos) |
            Token::Star(pos) |
            Token::StarEq(pos) |
            Token::Slash(pos) |
            Token::SlashEq(pos) |
            Token::Percent(pos) |
            Token::PercentEq(pos) |
            Token::And(pos) |
            Token::AndEq(pos) |
            Token::Or(pos) |
            Token::OrEq(pos) |
            Token::Caret(pos) |
            Token::Elvis(pos) |
            Token::ElvisEq(pos) |
            Token::GT(pos) |
            Token::GTE(pos) |
            Token::LT(pos) |
            Token::LTE(pos) |
            Token::Eq(pos) |
            Token::Neq(pos) |
            Token::Bang(pos) |
            Token::StarStar(pos) |

            Token::LParen(pos, _) |
            Token::RParen(pos) |
            Token::LBrack(pos, _) |
            Token::RBrack(pos) |
            Token::LBrace(pos) |
            Token::RBrace(pos) |
            Token::LBraceHash(pos) |
            Token::Pipe(pos) |
            Token::Colon(pos) |
            Token::Comma(pos) |
            Token::Question(pos) |
            Token::Dot(pos) |
            Token::QuestionDot(pos) |
            Token::Arrow(pos) |
            Token::At(pos) => pos
        };
        pos.clone()
    }

    pub fn get_range(&self) -> Range {
        match self {
            Token::Int(pos, v) => Range::with_length(pos, format!("{}", v).len() - 1),
            Token::Float(pos, v) => Range::with_length(pos, format!("{}", v).len() - 1),
            Token::String(pos, v) => Range::with_length(pos, format!("{}", v).len() + 1),
            Token::StringInterp(pos, chunks) => {
                let len_last = if let Some(Token::String(pos, v)) = chunks.last() {
                    pos.col + v.len()
                } else { unimplemented!() };
                Range::with_length(pos, pos.col + len_last - 1)
            }
            Token::Bool(pos, v) => Range::with_length(pos, format!("{}", v).len() - 1),

            Token::Func(pos) => Range::with_length(pos, 3),
            Token::Val(pos) => Range::with_length(pos, 2),
            Token::Var(pos) => Range::with_length(pos, 2),
            Token::If(pos) => Range::with_length(pos, 1),
            Token::Else(pos) => Range::with_length(pos, 3),
            Token::While(pos) => Range::with_length(pos, 4),
            Token::Break(pos) => Range::with_length(pos, 4),
            Token::Continue(pos) => Range::with_length(pos, 7),
            Token::For(pos) => Range::with_length(pos, 2),
            Token::In(pos) => Range::with_length(pos, 1),
            Token::Match(pos) => Range::with_length(pos, 4),
            Token::Type(pos) => Range::with_length(pos, 3),
            Token::Enum(pos) => Range::with_length(pos, 3),
            Token::Return(pos, _) => Range::with_length(pos, 5),
            Token::Readonly(pos) => Range::with_length(pos, 7),
            Token::Import(pos) => Range::with_length(pos, 5),
            Token::Export(pos) => Range::with_length(pos, 5),
            Token::From(pos) => Range::with_length(pos, 3),
            Token::As(pos) => Range::with_length(pos, 1),
            Token::Try(pos) => Range::with_length(pos, 2),

            Token::Ident(pos, i) => Range::with_length(pos, i.len() - 1),
            Token::Self_(pos) => Range::with_length(pos, 3),
            Token::None(pos) => Range::with_length(pos, 3),

            Token::Assign(pos) => Range::with_length(pos, 0),
            Token::Plus(pos) => Range::with_length(pos, 0),
            Token::PlusEq(pos) => Range::with_length(pos, 1),
            Token::Minus(pos) => Range::with_length(pos, 0),
            Token::MinusEq(pos) => Range::with_length(pos, 1),
            Token::Star(pos) => Range::with_length(pos, 0),
            Token::StarEq(pos) => Range::with_length(pos, 1),
            Token::Slash(pos) => Range::with_length(pos, 0),
            Token::SlashEq(pos) => Range::with_length(pos, 1),
            Token::Percent(pos) => Range::with_length(pos, 0),
            Token::PercentEq(pos) => Range::with_length(pos, 1),
            Token::And(pos) => Range::with_length(pos, 1),
            Token::AndEq(pos) => Range::with_length(pos, 2),
            Token::Or(pos) => Range::with_length(pos, 1),
            Token::OrEq(pos) => Range::with_length(pos, 2),
            Token::Caret(pos) => Range::with_length(pos, 1),
            Token::Elvis(pos) => Range::with_length(pos, 1),
            Token::ElvisEq(pos) => Range::with_length(pos, 1),
            Token::GT(pos) => Range::with_length(pos, 0),
            Token::GTE(pos) => Range::with_length(pos, 1),
            Token::LT(pos) => Range::with_length(pos, 0),
            Token::LTE(pos) => Range::with_length(pos, 1),
            Token::Eq(pos) => Range::with_length(pos, 1),
            Token::Neq(pos) => Range::with_length(pos, 1),
            Token::Bang(pos) => Range::with_length(pos, 0),
            Token::StarStar(pos) => Range::with_length(pos, 1),

            Token::LParen(pos, _) => Range::with_length(pos, 0),
            Token::RParen(pos) => Range::with_length(pos, 0),
            Token::LBrack(pos, _) => Range::with_length(pos, 0),
            Token::RBrack(pos) => Range::with_length(pos, 0),
            Token::LBrace(pos) => Range::with_length(pos, 0),
            Token::RBrace(pos) => Range::with_length(pos, 0),
            Token::LBraceHash(pos) => Range::with_length(pos, 1),
            Token::Pipe(pos) => Range::with_length(pos, 0),
            Token::Colon(pos) => Range::with_length(pos, 0),
            Token::Comma(pos) => Range::with_length(pos, 0),
            Token::Question(pos) => Range::with_length(pos, 0),
            Token::Dot(pos) => Range::with_length(pos, 0),
            Token::QuestionDot(pos) => Range::with_length(pos, 1),
            Token::Arrow(pos) => Range::with_length(pos, 1),
            Token::At(pos) => Range::with_length(pos, 0),
        }
    }

    pub fn get_ident_name(token: &Token) -> String {
        match token {
            Token::Ident(_, ident) => ident.clone(),
            Token::Self_(_) => "self".to_string(),
            Token::None(_) => "None".to_string(),
            _ => unreachable!()
        }
    }
}

impl Eq for Token {}
