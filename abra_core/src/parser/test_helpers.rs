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
                AstNode::Literal(
                    Token::Int(Position::new(line, col), $i),
                    AstLiteralNode::IntLiteral($i)
                )
            }
        }
    );
}

macro_rules! float_literal {
    ($pos: expr, $i: expr) => (
        match $pos {
            (line, col) => {
                AstNode::Literal(
                    Token::Float(Position::new(line, col), $i),
                    AstLiteralNode::FloatLiteral($i)
                )
            }
        }
    );
}

macro_rules! bool_literal {
    ($pos: expr, $b: expr) => (
        match $pos {
            (line, col) => {
                AstNode::Literal(
                    Token::Bool(Position::new(line, col), $b),
                    AstLiteralNode::BoolLiteral($b)
                )
            }
        }
    );
}

macro_rules! string_literal {
    ($pos: expr, $str: expr) => (
        match $pos {
            (line, col) => {
                AstNode::Literal(
                    Token::String(Position::new(line, col), $str.to_string()),
                    AstLiteralNode::StringLiteral($str.to_string())
                )
            }
        }
    );
}

macro_rules! identifier {
    ($pos: expr, $ident_name: expr) => (
        match $pos {
            (line, col) => {
                AstNode::Identifier(
                    Token::Ident(Position::new(line, col), $ident_name.to_string()),
                    None
                )
            }
        }
    );
}
