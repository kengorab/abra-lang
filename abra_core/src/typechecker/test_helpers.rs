macro_rules! ident_token {
    ($pos: expr, $i: expr) => (
        match $pos {
            (line, col) => {
                Token::Ident(Position::new(line, col), $i.to_string())
            }
        }
    );
}

macro_rules! int_literal {
    ($pos: expr, $i: expr) => (
        match $pos {
            (line, col) => {
                TypedAstNode::Literal(
                    Token::Int(Position::new(line, col), $i),
                    TypedLiteralNode::IntLiteral($i)
                )
            }
        }
    );
}

macro_rules! float_literal {
    ($pos: expr, $i: expr) => (
        match $pos {
            (line, col) => {
                TypedAstNode::Literal(
                    Token::Float(Position::new(line, col), $i),
                    TypedLiteralNode::FloatLiteral($i)
                )
            }
        }
    );
}

macro_rules! bool_literal {
    ($pos: expr, $b: expr) => (
        match $pos {
            (line, col) => {
                TypedAstNode::Literal(
                    Token::Bool(Position::new(line, col), $b),
                    TypedLiteralNode::BoolLiteral($b)
                )
            }
        }
    );
}

macro_rules! string_literal {
    ($pos: expr, $str: expr) => (
        match $pos {
            (line, col) => {
                TypedAstNode::Literal(
                    Token::String(Position::new(line, col), $str.to_string()),
                    TypedLiteralNode::StringLiteral($str.to_string())
                )
            }
        }
    );
}
