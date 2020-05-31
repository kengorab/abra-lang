#[derive(Debug, Clone, PartialEq)]
pub struct Position { pub line: usize, pub col: usize }

impl Position {
    pub fn new(line: usize, col: usize) -> Self { Position { line, col } }
}

impl Default for Position {
    fn default() -> Self { Position { line: 0, col: 0 } }
}

#[derive(Debug, Display, Clone, PartialEq, EnumString, EnumDiscriminants)]
#[strum_discriminants(name(TokenType), derive(Display))]
pub enum Token {
    #[strum(to_string = "int", serialize = "Int")] Int(Position, i64),
    #[strum(to_string = "float", serialize = "Float")] Float(Position, f64),
    #[strum(to_string = "string", serialize = "String")] String(Position, String),
    #[strum(to_string = "boolean", serialize = "Bool")] Bool(Position, bool),

    #[strum(to_string = "func", serialize = "Func")] Func(Position),
    #[strum(to_string = "val", serialize = "Val")] Val(Position),
    #[strum(to_string = "var", serialize = "Var")] Var(Position),
    #[strum(to_string = "if", serialize = "If")] If(Position),
    #[strum(to_string = "else", serialize = "Else")] Else(Position),
    #[strum(to_string = "while", serialize = "While")] While(Position),
    #[strum(to_string = "break", serialize = "Break")] Break(Position),
    #[strum(to_string = "for", serialize = "For")] For(Position),
    #[strum(to_string = "in", serialize = "In")] In(Position),
    #[strum(to_string = "type", serialize = "Type")] Type(Position),

    #[strum(to_string = "identifier", serialize = "Ident")] Ident(Position, String),
    #[strum(to_string = "self", serialize = "Self")] Self_(Position),

    #[strum(to_string = "=", serialize = "Assign")] Assign(Position),
    #[strum(to_string = "+", serialize = "Plus")] Plus(Position),
    #[strum(to_string = "-", serialize = "Minus")] Minus(Position),
    #[strum(to_string = "*", serialize = "Star")] Star(Position),
    #[strum(to_string = "/", serialize = "Slash")] Slash(Position),
    #[strum(to_string = "%", serialize = "Percent")] Percent(Position),
    #[strum(to_string = "&&", serialize = "And")] And(Position),
    #[strum(to_string = "||", serialize = "Or")] Or(Position),
    #[strum(to_string = "?:", serialize = "Elvis")] Elvis(Position),
    #[strum(to_string = ">", serialize = "GT")] GT(Position),
    #[strum(to_string = ">=", serialize = "GTE")] GTE(Position),
    #[strum(to_string = "<", serialize = "LT")] LT(Position),
    #[strum(to_string = "<=", serialize = "LTE")] LTE(Position),
    #[strum(to_string = "==", serialize = "Eq")] Eq(Position),
    #[strum(to_string = "!=", serialize = "Neq")] Neq(Position),
    #[strum(to_string = "!", serialize = "Bang")] Bang(Position),

    #[strum(to_string = "(", serialize = "LParen")] LParen(Position, /* is_preceded_by_newline: */ bool),
    #[strum(to_string = ")", serialize = "RParen")] RParen(Position),
    #[strum(to_string = "[", serialize = "LBrack")] LBrack(Position, /* is_preceded_by_newline: */ bool),
    #[strum(to_string = "]", serialize = "RBrack")] RBrack(Position),
    #[strum(to_string = "{", serialize = "LBrace")] LBrace(Position),
    #[strum(to_string = "}", serialize = "RBrace")] RBrace(Position),
    #[strum(to_string = ":", serialize = "Colon")] Colon(Position),
    #[strum(to_string = ",", serialize = "Comma")] Comma(Position),
    #[strum(to_string = "?", serialize = "Question")] Question(Position),
    #[strum(to_string = ".", serialize = "Dot")] Dot(Position),
}

impl Token {
    pub fn get_position(&self) -> Position {
        let pos = match self {
            Token::Int(pos, _) |
            Token::Float(pos, _) |
            Token::String(pos, _) |
            Token::Bool(pos, _) |

            Token::Func(pos) |
            Token::Val(pos) |
            Token::Var(pos) |
            Token::If(pos) |
            Token::Else(pos) |
            Token::While(pos) |
            Token::Break(pos) |
            Token::For(pos) |
            Token::In(pos) |
            Token::Type(pos) |

            Token::Ident(pos, _) |
            Token::Self_(pos) |

            Token::Assign(pos) |
            Token::Plus(pos) |
            Token::Minus(pos) |
            Token::Star(pos) |
            Token::Slash(pos) |
            Token::Percent(pos) |
            Token::And(pos) |
            Token::Or(pos) |
            Token::Elvis(pos) |
            Token::GT(pos) |
            Token::GTE(pos) |
            Token::LT(pos) |
            Token::LTE(pos) |
            Token::Eq(pos) |
            Token::Neq(pos) |
            Token::Bang(pos) |

            Token::LParen(pos, _) |
            Token::RParen(pos) |
            Token::LBrack(pos, _) |
            Token::RBrack(pos) |
            Token::LBrace(pos) |
            Token::RBrace(pos) |
            Token::Colon(pos) |
            Token::Comma(pos) |
            Token::Question(pos) |
            Token::Dot(pos) => pos
        };
        pos.clone()
    }

    pub fn get_ident_name(token: &Token) -> String {
        match token {
            Token::Ident(_, ident) => ident.clone(),
            Token::Self_(_) => "self".to_string(),
            _ => unreachable!()
        }
    }
}
