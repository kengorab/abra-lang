use crate::lexer::tokens::Token;
use std::iter::Peekable;
use std::vec::IntoIter;
use crate::parser::ast::{AstNode, AstLiteralNode, UnaryOp, BinaryOp, UnaryNode, BinaryNode, ArrayNode, BindingDeclNode};
use crate::parser::precedence::Precedence;
use crate::parser::parse_error::ParseError;

pub fn parse(tokens: Vec<Token>) -> Result<Vec<AstNode>, ParseError> {
    let mut parser = Parser::new(tokens);

    let mut nodes: Vec<AstNode> = vec![];
    loop {
        match parser.peek() {
            Some(t) => {
                match t {
                    _ => nodes.push(parser.parse_stmt()?)
                }
            }
            None => break
        }
    }

    Ok(nodes)
}

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>
}

type PrefixFn = Fn(&mut Parser, Token) -> Result<AstNode, ParseError>;
type InfixFn = Fn(&mut Parser, Token, AstNode) -> Result<AstNode, ParseError>;

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        let tokens = tokens.into_iter().peekable();
        Parser { tokens }
    }

    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn expect_next(&mut self) -> Result<Token, ParseError> {
        self.advance().ok_or(ParseError::UnexpectedEof)
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    // Begin Pratt plumbing

    fn parse_precedence(&mut self, prec: Precedence) -> Result<AstNode, ParseError> {
        let prefix_token = match self.peek() {
            Some(token) => Ok(token.clone()),
            None => Err(ParseError::UnexpectedEof),
        }?;

        match self.get_prefix_rule(&prefix_token) {
            None => Err(ParseError::UnexpectedToken(prefix_token)),
            Some(prefix_fn) => {
                // Cursor sits on token AFTER the match token; in-/prefix fns always start on token AFTER the one they match
                let prefix_token = self.expect_next()?;

                let mut left: AstNode = (*prefix_fn)(self, prefix_token)?;

                if let Some(token) = self.peek() {
                    let next_prec = Parser::get_precedence_for_token(&token);
                    let prec: u8 = prec.into();
                    let next_prec: u8 = next_prec.into();
                    while prec < next_prec {
                        if let Some(_) = self.peek() {
                            let infix_token = self.expect_next()?;

                            if let Some(infix_fn) = self.get_infix_rule(&infix_token) {
                                left = (*infix_fn)(self, infix_token, left)?
                            }
                        } else {
                            break;
                        }
                    }
                }

                Ok(left)
            }
        }
    }

    fn get_prefix_rule(&self, tok: &Token) -> Option<Box<PrefixFn>> {
        use self::Parser;

        match tok {
            Token::Int(_, _) |
            Token::Float(_, _) |
            Token::String(_, _) |
            Token::Bool(_, _) => Some(Box::new(Parser::parse_literal)),
            Token::Minus(_) | Token::Bang(_) => Some(Box::new(Parser::parse_unary)),
            Token::LBrack(_) => Some(Box::new(Parser::parse_array)),
            Token::Val(_) |
            Token::Var(_) |
            Token::Plus(_) |
            Token::Star(_) |
            Token::Slash(_) |
            Token::And(_) |
            Token::Or(_) |
            Token::GT(_) |
            Token::GTE(_) |
            Token::LT(_) |
            Token::LTE(_) |
            Token::Neq(_) |
            Token::Eq(_) |
            Token::RBrack(_) |
            Token::Comma(_) |
            Token::Assign(_) |
            Token::Ident(_, _) => None,
        }
    }

    fn get_infix_rule(&self, tok: &Token) -> Option<Box<InfixFn>> {
        match tok {
            Token::Int(_, _) |
            Token::Float(_, _) |
            Token::String(_, _) |
            Token::Bool(_, _) |
            Token::Bang(_) |
            Token::Val(_) |
            Token::Var(_) |
            Token::Ident(_, _) => None,
            Token::Assign(_) => /* TODO: Assignment statements */None,
            Token::Plus(_) |
            Token::Star(_) |
            Token::Slash(_) |
            Token::Minus(_) |
            Token::And(_) |
            Token::Or(_) |
            Token::GT(_) |
            Token::GTE(_) |
            Token::LT(_) |
            Token::LTE(_) |
            Token::Neq(_) |
            Token::Eq(_) |
            Token::LBrack(_) |
            Token::RBrack(_) |
            Token::Comma(_) => Some(Box::new(Parser::parse_binary)),
        }
    }

    fn get_precedence_for_token(tok: &Token) -> Precedence {
        match tok {
            Token::Int(_, _) |
            Token::Float(_, _) |
            Token::String(_, _) |
            Token::Bool(_, _) |
            Token::Bang(_) |
            Token::LBrack(_) |
            Token::RBrack(_) |
            Token::Comma(_) |
            Token::Val(_) |
            Token::Var(_) |
            Token::Ident(_, _) |
            Token::Assign(_) => Precedence::None,
            Token::Plus(_) | Token::Minus(_) => Precedence::Addition,
            Token::Star(_) | Token::Slash(_) => Precedence::Multiplication,
            Token::And(_) => Precedence::And,
            Token::Or(_) => Precedence::Or,
            Token::Eq(_) | Token::Neq(_) => Precedence::Equality,
            Token::GT(_) | Token::GTE(_) | Token::LT(_) | Token::LTE(_) => Precedence::Comparison,
        }
    }

    // End Pratt plumbing

    fn parse_stmt(&mut self) -> Result<AstNode, ParseError> {
        match self.peek() {
            Some(Token::Val(_)) => self.parse_binding_decl(),
            Some(Token::Var(_)) => self.parse_binding_decl(),
            Some(_) => self.parse_expr(),
            None => Err(ParseError::UnexpectedEof)
        }
    }

    fn parse_binding_decl(&mut self) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;
        let is_mutable = match &token {
            Token::Val(_) => false,
            Token::Var(_) => true,
            _ => unreachable!()
        };

        let ident = self.expect_next().and_then(|tok| {
            match tok {
                Token::Ident(_, _) => Ok(tok),
                _ => Err(ParseError::UnexpectedToken(tok))
            }
        })?;

        let expr = match self.peek() {
            Some(Token::Assign(_)) => {
                self.expect_next()?; // Consume '='
                let expr = self.parse_expr()?;
                Some(Box::new(expr))
            }
            Some(_) | None => None
        };

        Ok(AstNode::BindingDecl(token, BindingDeclNode { ident, is_mutable, expr }))
    }

    fn parse_expr(&mut self) -> Result<AstNode, ParseError> {
        self.parse_precedence(Precedence::None)
    }

    fn parse_literal(&mut self, token: Token) -> Result<AstNode, ParseError> {
        match &token {
            Token::Int(_, val) => Ok(AstNode::Literal(token.clone(), AstLiteralNode::IntLiteral(*val))),
            Token::Float(_, val) => Ok(AstNode::Literal(token.clone(), AstLiteralNode::FloatLiteral(*val))),
            Token::String(_, val) => Ok(AstNode::Literal(token.clone(), AstLiteralNode::StringLiteral(val.clone()))),
            Token::Bool(_, val) => Ok(AstNode::Literal(token.clone(), AstLiteralNode::BoolLiteral(*val))),
            _ => Err(ParseError::Raw(format!("Unknown literal: {:?}", token)))
        }
    }

    fn parse_unary(&mut self, token: Token) -> Result<AstNode, ParseError> {
        let expr = self.parse_precedence(Precedence::Unary)?;
        let op = match token {
            Token::Minus(_) => UnaryOp::Minus,
            Token::Bang(_) => UnaryOp::Negate,
            _ => unreachable!()
        };
        Ok(AstNode::Unary(token, UnaryNode { op, expr: Box::new(expr) }))
    }

    fn parse_binary(&mut self, token: Token, left: AstNode) -> Result<AstNode, ParseError> {
        let prec = Parser::get_precedence_for_token(&token);
        let right = self.parse_precedence(prec)?;
        let op = match token {
            Token::Plus(_) => BinaryOp::Add,
            Token::Minus(_) => BinaryOp::Sub,
            Token::Star(_) => BinaryOp::Mul,
            Token::Slash(_) => BinaryOp::Div,
            Token::And(_) => BinaryOp::And,
            Token::Or(_) => BinaryOp::Or,
            Token::GT(_) => BinaryOp::Gt,
            Token::GTE(_) => BinaryOp::Gte,
            Token::LT(_) => BinaryOp::Lt,
            Token::LTE(_) => BinaryOp::Lte,
            Token::Neq(_) => BinaryOp::Neq,
            Token::Eq(_) => BinaryOp::Eq,
            _ => unreachable!()
        };
        Ok(AstNode::Binary(token, BinaryNode { left: Box::new(left), op, right: Box::new(right) }))
    }

    fn parse_array(&mut self, token: Token) -> Result<AstNode, ParseError> {
        let mut item_expected = true;
        let mut items: Vec<Box<AstNode>> = vec![];
        loop {
            let token = self.peek().ok_or(ParseError::UnexpectedEof)?;
            if let Token::RBrack(_) = token {
                self.expect_next()?; // Consume ']' before ending loop
                break;
            } else if !item_expected {
                return match self.peek() {
                    Some(tok) => Err(ParseError::UnexpectedToken(tok.clone())),
                    None => Err(ParseError::UnexpectedEof)
                };
            }

            let expr = self.parse_expr()?;
            items.push(Box::new(expr));

            let token = self.peek().ok_or(ParseError::UnexpectedEof)?;
            if let Token::Comma(_) = token {
                self.expect_next()?; // Consume comma
            } else {
                item_expected = false;
            }
        }

        Ok(AstNode::Array(token, ArrayNode { items }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokens::{Token, Position};
    use crate::parser::ast::AstNode::*;
    use crate::parser::ast::AstLiteralNode::*;
    use crate::lexer::lexer::tokenize;

    type TestResult = Result<(), ParseError>;

    fn parse(input: &str) -> Result<Vec<AstNode>, ParseError> {
        let tokens = tokenize(&input.to_string()).unwrap();
        super::parse(tokens)
    }

    #[test]
    fn parse_literals() -> TestResult {
        let ast = parse("123 4.56 0.789 \"hello world\" true false")?;
        let expected = vec![
            Literal(Token::Int(Position::new(1, 1), 123), IntLiteral(123)),
            Literal(Token::Float(Position::new(1, 5), 4.56), FloatLiteral(4.56)),
            Literal(Token::Float(Position::new(1, 10), 0.789), FloatLiteral(0.789)),
            Literal(Token::String(Position::new(1, 16), "hello world".to_string()), StringLiteral("hello world".to_string())),
            Literal(Token::Bool(Position::new(1, 30), true), BoolLiteral(true)),
            Literal(Token::Bool(Position::new(1, 35), false), BoolLiteral(false)),
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_unary() -> TestResult {
        let ast = parse("-123")?;
        let expected = vec![
            Unary(
                Token::Minus(Position::new(1, 1)),
                UnaryNode {
                    op: UnaryOp::Minus,
                    expr: Box::new(
                        Literal(Token::Int(Position::new(1, 2), 123), IntLiteral(123))
                    ),
                },
            )
        ];
        assert_eq!(expected, ast);

        let ast = parse("!true")?;
        let expected = vec![
            Unary(
                Token::Bang(Position::new(1, 1)),
                UnaryNode {
                    op: UnaryOp::Negate,
                    expr: Box::new(
                        Literal(Token::Bool(Position::new(1, 2), true), BoolLiteral(true))
                    ),
                },
            )
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_unary_errors() -> TestResult {
        let err = parse("-").unwrap_err();
        assert_eq!(ParseError::UnexpectedEof, err);

        let err = parse("-   +").unwrap_err();
        assert_eq!(ParseError::UnexpectedToken(Token::Plus(Position::new(1, 5))), err);

        let err = parse("!").unwrap_err();
        assert_eq!(ParseError::UnexpectedEof, err);

        let err = parse("!   +").unwrap_err();
        Ok(assert_eq!(ParseError::UnexpectedToken(Token::Plus(Position::new(1, 5))), err))
    }

    #[test]
    fn parse_binary_numeric() -> TestResult {
        let ast = parse("1.2 + 3")?;
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 5)),
                BinaryNode {
                    left: Box::new(
                        Literal(Token::Float(Position::new(1, 1), 1.2), FloatLiteral(1.2))
                    ),
                    op: BinaryOp::Add,
                    right: Box::new(
                        Literal(Token::Int(Position::new(1, 7), 3), IntLiteral(3))
                    ),
                },
            )
        ];
        assert_eq!(expected, ast);

        let ast = parse("\"hello \" + \"world\"")?;
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 10)),
                BinaryNode {
                    left: Box::new(
                        Literal(
                            Token::String(Position::new(1, 1), "hello ".to_string()),
                            StringLiteral("hello ".to_string()),
                        )
                    ),
                    op: BinaryOp::Add,
                    right: Box::new(
                        Literal(
                            Token::String(Position::new(1, 12), "world".to_string()),
                            StringLiteral("world".to_string()),
                        )
                    ),
                },
            )
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_binary_and_unary_numeric() -> TestResult {
        let ast = parse("1 + -2")?;
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 3)),
                BinaryNode {
                    left: Box::new(
                        Literal(Token::Int(Position::new(1, 1), 1), IntLiteral(1))
                    ),
                    op: BinaryOp::Add,
                    right: Box::new(
                        Unary(
                            Token::Minus(Position::new(1, 5)),
                            UnaryNode {
                                op: UnaryOp::Minus,
                                expr: Box::new(
                                    Literal(Token::Int(Position::new(1, 6), 2), IntLiteral(2))
                                ),
                            },
                        )
                    ),
                },
            )
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_binary_and_unary_boolean() -> TestResult {
        let ast = parse("true && !false")?;
        let expected = vec![
            Binary(
                Token::And(Position::new(1, 6)),
                BinaryNode {
                    left: Box::new(
                        Literal(Token::Bool(Position::new(1, 1), true), BoolLiteral(true))
                    ),
                    op: BinaryOp::And,
                    right: Box::new(
                        Unary(
                            Token::Bang(Position::new(1, 9)),
                            UnaryNode {
                                op: UnaryOp::Negate,
                                expr: Box::new(
                                    Literal(Token::Bool(Position::new(1, 10), false), BoolLiteral(false))
                                ),
                            },
                        )
                    ),
                },
            )
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_binary_precedence_numeric() -> TestResult {
        let ast = parse("1 + 2 * 3")?;
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 3)),
                BinaryNode {
                    left: Box::new(
                        Literal(Token::Int(Position::new(1, 1), 1), IntLiteral(1))
                    ),
                    op: BinaryOp::Add,
                    right: Box::new(
                        Binary(
                            Token::Star(Position::new(1, 7)),
                            BinaryNode {
                                left: Box::new(
                                    Literal(Token::Int(Position::new(1, 5), 2), IntLiteral(2))
                                ),
                                op: BinaryOp::Mul,
                                right: Box::new(
                                    Literal(Token::Int(Position::new(1, 9), 3), IntLiteral(3))
                                ),
                            },
                        )
                    ),
                },
            )
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_binary_precedence_boolean() -> TestResult {
        let ast = parse("true && true || false && false")?;
        let expected = vec![
            Binary(
                Token::Or(Position::new(1, 14)),
                BinaryNode {
                    left: Box::new(
                        Binary(
                            Token::And(Position::new(1, 6)),
                            BinaryNode {
                                left: Box::new(
                                    Literal(Token::Bool(Position::new(1, 1), true), BoolLiteral(true))
                                ),
                                op: BinaryOp::And,
                                right: Box::new(
                                    Literal(Token::Bool(Position::new(1, 9), true), BoolLiteral(true))
                                ),
                            },
                        )
                    ),
                    op: BinaryOp::Or,
                    right: Box::new(
                        Binary(
                            Token::And(Position::new(1, 23)),
                            BinaryNode {
                                left: Box::new(
                                    Literal(Token::Bool(Position::new(1, 17), false), BoolLiteral(false))
                                ),
                                op: BinaryOp::And,
                                right: Box::new(
                                    Literal(Token::Bool(Position::new(1, 26), false), BoolLiteral(false))
                                ),
                            },
                        )
                    ),
                },
            )
        ];
        assert_eq!(expected, ast);

        let ast = parse("1 + 2 <= 3 != 11 >= 13")?;
        let expected = vec![
            Binary(
                Token::Neq(Position::new(1, 12)),
                BinaryNode {
                    left: Box::new(
                        Binary(
                            Token::LTE(Position::new(1, 7)),
                            BinaryNode {
                                left: Box::new(
                                    Binary(
                                        Token::Plus(Position::new(1, 3)),
                                        BinaryNode {
                                            left: Box::new(
                                                Literal(Token::Int(Position::new(1, 1), 1), IntLiteral(1))
                                            ),
                                            op: BinaryOp::Add,
                                            right: Box::new(
                                                Literal(Token::Int(Position::new(1, 5), 2), IntLiteral(2))
                                            ),
                                        },
                                    )
                                ),
                                op: BinaryOp::Lte,
                                right: Box::new(
                                    Literal(Token::Int(Position::new(1, 10), 3), IntLiteral(3))
                                ),
                            },
                        )
                    ),
                    op: BinaryOp::Neq,
                    right: Box::new(
                        Binary(
                            Token::GTE(Position::new(1, 18)),
                            BinaryNode {
                                left: Box::new(
                                    Literal(Token::Int(Position::new(1, 15), 11), IntLiteral(11))
                                ),
                                op: BinaryOp::Gte,
                                right: Box::new(
                                    Literal(Token::Int(Position::new(1, 21), 13), IntLiteral(13))
                                ),
                            },
                        )
                    ),
                },
            )
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_binary_errors_eof() {
        let cases = vec![
            "-5 +", "-5 -", "-5 *", "-5 /",
            "5 >", "5 >=", "5 <", "5 <=", "5 ==", "5 !=",
        ];

        for input in cases {
            let error = parse(input).unwrap_err();
            assert_eq!(ParseError::UnexpectedEof, error, "Parsing {} should have UnexpectedEof error", input);
        }
    }

    #[test]
    fn parse_binary_errors_unexpected_token() {
        let cases = vec![
            ("5 > + 4", ParseError::UnexpectedToken(Token::Plus(Position::new(1, 5)))),
            ("5 >> 4", ParseError::UnexpectedToken(Token::GT(Position::new(1, 4)))),
            ("5 < + 4", ParseError::UnexpectedToken(Token::Plus(Position::new(1, 5)))),
            ("5 << 4", ParseError::UnexpectedToken(Token::LT(Position::new(1, 4)))),
            ("5 <> 6", ParseError::UnexpectedToken(Token::GT(Position::new(1, 4)))),
        ];

        for (input, err) in cases {
            let error = parse(input).unwrap_err();
            assert_eq!(err, error, "Parsing {} should have error: {:?}", input, err);
        }
    }

    #[test]
    fn parse_array_empty() -> TestResult {
        let ast = parse("[]")?;
        let expected = AstNode::Array(Token::LBrack(Position::new(1, 1)), ArrayNode {
            items: vec![]
        });
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_array_with_items() -> TestResult {
        let ast = parse("[1, true, \"a\", 3.14]")?;
        let expected = AstNode::Array(
            Token::LBrack(Position::new(1, 1)),
            ArrayNode {
                items: vec![
                    Box::new(AstNode::Literal(
                        Token::Int(Position::new(1, 2), 1),
                        AstLiteralNode::IntLiteral(1),
                    )),
                    Box::new(AstNode::Literal(
                        Token::Bool(Position::new(1, 5), true),
                        AstLiteralNode::BoolLiteral(true),
                    )),
                    Box::new(AstNode::Literal(
                        Token::String(Position::new(1, 11), "a".to_string()),
                        AstLiteralNode::StringLiteral("a".to_string()),
                    )),
                    Box::new(AstNode::Literal(
                        Token::Float(Position::new(1, 16), 3.14),
                        AstLiteralNode::FloatLiteral(3.14),
                    ))
                ]
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_array_nested() -> TestResult {
        let ast = parse("[[1, 2], [3, 4]]")?;
        let expected = AstNode::Array(
            Token::LBrack(Position::new(1, 1)),
            ArrayNode {
                items: vec![
                    Box::new(AstNode::Array(
                        Token::LBrack(Position::new(1, 2)),
                        ArrayNode {
                            items: vec![
                                Box::new(AstNode::Literal(
                                    Token::Int(Position::new(1, 3), 1),
                                    AstLiteralNode::IntLiteral(1),
                                )),
                                Box::new(AstNode::Literal(
                                    Token::Int(Position::new(1, 6), 2),
                                    AstLiteralNode::IntLiteral(2),
                                )),
                            ]
                        },
                    )),
                    Box::new(AstNode::Array(
                        Token::LBrack(Position::new(1, 10)),
                        ArrayNode {
                            items: vec![
                                Box::new(AstNode::Literal(
                                    Token::Int(Position::new(1, 11), 3),
                                    AstLiteralNode::IntLiteral(3),
                                )),
                                Box::new(AstNode::Literal(
                                    Token::Int(Position::new(1, 14), 4),
                                    AstLiteralNode::IntLiteral(4),
                                )),
                            ]
                        },
                    ))
                ]
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_array_error() {
        let error = parse("[").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, error);

        let error = parse("[1").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, error);

        let error = parse("[1,").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, error);

        let error = parse("[,").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Comma(Position::new(1, 2)));
        assert_eq!(expected, error);

        let error = parse("[1, 2 true").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Bool(Position::new(1, 7), true));
        assert_eq!(expected, error);
    }

    #[test]
    fn parse_binding_decls_no_assignment() -> TestResult {
        let ast = parse("val abc\nvar abc")?;
        let expected = vec![
            AstNode::BindingDecl(
                Token::Val(Position::new(1, 1)),
                BindingDeclNode {
                    ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                    is_mutable: false,
                    expr: None,
                },
            ),
            AstNode::BindingDecl(
                Token::Var(Position::new(2, 1)),
                BindingDeclNode {
                    ident: Token::Ident(Position::new(2, 5), "abc".to_string()),
                    is_mutable: true,
                    expr: None,
                },
            ),
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_binding_decls_with_assignment() -> TestResult {
        let ast = parse("val abc = 1\nvar abc = 1")?;
        let expected = vec![
            AstNode::BindingDecl(
                Token::Val(Position::new(1, 1)),
                BindingDeclNode {
                    ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                    is_mutable: false,
                    expr: Some(Box::new(
                        AstNode::Literal(Token::Int(Position::new(1, 11), 1), AstLiteralNode::IntLiteral(1))
                    )),
                },
            ),
            AstNode::BindingDecl(
                Token::Var(Position::new(2, 1)),
                BindingDeclNode {
                    ident: Token::Ident(Position::new(2, 5), "abc".to_string()),
                    is_mutable: true,
                    expr: Some(Box::new(
                        AstNode::Literal(Token::Int(Position::new(2, 11), 1), AstLiteralNode::IntLiteral(1))
                    )),
                },
            ),
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_binding_decls_error() {
        let err = parse("val 123 = \"hello \" + \"world\"").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Int(Position::new(1, 5), 123));
        assert_eq!(expected, err);

        let err = parse("val _abc =").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, err);

        let err = parse("val abc = val def = 3").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Val(Position::new(1, 11)));
        assert_eq!(expected, err);
    }
}
