use crate::lexer::tokens::Token;
use std::iter::Peekable;
use std::vec::IntoIter;
use crate::parser::ast::{AstNode, AstLiteralNode, UnaryOp, BinaryOp, UnaryNode, BinaryNode};
use crate::parser::precedence::Precedence;
use crate::parser::parse_error::ParseError;

pub fn parse(tokens: Vec<Token>) -> Result<Vec<AstNode>, ParseError> {
    let mut parser = Parser::new(tokens);

    let mut nodes: Vec<AstNode> = vec![];
    loop {
        match parser.peek() {
            Some(t) => {
                match t {
                    _ => nodes.push(parser.parse_expr()?)
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
                let prefix_token = self.advance().expect("Safe, since peek above is Some");

                let mut left: AstNode = (*prefix_fn)(self, prefix_token)?;

                if let Some(token) = self.peek() {
                    let next_prec = Parser::get_precedence_for_token(&token);
                    let prec: u8 = prec.into();
                    let next_prec: u8 = next_prec.into();
                    while prec < next_prec {
                        if let Some(_) = self.peek() {
                            let infix_token = self.advance().expect("Safe, since peek above is Some");

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
            Token::Int(_, _) | Token::Float(_, _) => Some(Box::new(Parser::parse_literal)),
            Token::Plus(_) | Token::Star(_) | Token::Slash(_) => None,
            Token::Minus(_) => Some(Box::new(Parser::parse_unary)),
            _ => unimplemented!()
        }
    }

    fn get_infix_rule(&self, tok: &Token) -> Option<Box<InfixFn>> {
        use self::Parser;

        match tok {
            Token::Int(_, _) | Token::Float(_, _) => None,
            Token::Plus(_) | Token::Star(_) | Token::Slash(_) | Token::Minus(_) =>
                Some(Box::new(Parser::parse_binary)),
            _ => unimplemented!()
        }
    }

    fn get_precedence_for_token(tok: &Token) -> Precedence {
        match tok {
            Token::Int(_, _) | Token::Float(_, _) => Precedence::None,
            Token::Plus(_) | Token::Minus(_) => Precedence::Addition,
            Token::Star(_) | Token::Slash(_) => Precedence::Multiplication,
            _ => unimplemented!()
        }
    }

    // End Pratt plumbing

    fn parse_expr(&mut self) -> Result<AstNode, ParseError> {
        self.parse_precedence(Precedence::None)
    }

    fn parse_literal(&mut self, token: Token) -> Result<AstNode, ParseError> {
        match &token {
            Token::Int(_, val) => Ok(AstNode::Literal(token.clone(), AstLiteralNode::IntLiteral(*val))),
            Token::Float(_, val) => Ok(AstNode::Literal(token.clone(), AstLiteralNode::FloatLiteral(*val))),
            _ => Err(ParseError::Raw(format!("Unknown literal: {:?}", token)))
        }
    }

    fn parse_unary(&mut self, token: Token) -> Result<AstNode, ParseError> {
        let expr = self.parse_precedence(Precedence::Unary)?;
        let op = match token {
            Token::Minus(_) => UnaryOp::Minus,
            _ => unimplemented!()
        };
        Ok(AstNode::Unary(token, UnaryNode { typ: None, op, expr: Box::new(expr) }))
    }

    fn parse_binary(&mut self, token: Token, left: AstNode) -> Result<AstNode, ParseError> {
        let prec = Parser::get_precedence_for_token(&token);
        let right = self.parse_precedence(prec)?;
        let op = match token {
            Token::Plus(_) => BinaryOp::Add,
            Token::Minus(_) => BinaryOp::Sub,
            Token::Star(_) => BinaryOp::Mul,
            Token::Slash(_) => BinaryOp::Div,
            _ => unimplemented!()
        };
        Ok(AstNode::Binary(token, BinaryNode { typ: None, left: Box::new(left), op, right: Box::new(right) }))
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
        let ast = parse("123 4.56 0.789")?;
        let expected = vec![
            Literal(Token::Int(Position::new(1, 1), 123), IntLiteral(123)),
            Literal(Token::Float(Position::new(1, 5), 4.56), FloatLiteral(4.56)),
            Literal(Token::Float(Position::new(1, 10), 0.789), FloatLiteral(0.789)),
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
                    typ: None,
                    op: UnaryOp::Minus,
                    expr: Box::new(
                        Literal(Token::Int(Position::new(1, 2), 123), IntLiteral(123))
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
        Ok(assert_eq!(ParseError::UnexpectedToken(Token::Plus(Position::new(1, 5))), err))
    }

    #[test]
    fn parse_binary() -> TestResult {
        let ast = parse("1.2 + 3")?;
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 5)),
                BinaryNode {
                    typ: None,
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
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_binary_and_unary() -> TestResult {
        let ast = parse("1 + -2")?;
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 3)),
                BinaryNode {
                    typ: None,
                    left: Box::new(
                        Literal(Token::Int(Position::new(1, 1), 1), IntLiteral(1))
                    ),
                    op: BinaryOp::Add,
                    right: Box::new(
                        Unary(
                            Token::Minus(Position::new(1, 5)),
                            UnaryNode {
                                typ: None,
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
    fn parse_binary_precedence() -> TestResult {
        let ast = parse("1 + 2 * 3")?;
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 3)),
                BinaryNode {
                    typ: None,
                    left: Box::new(
                        Literal(Token::Int(Position::new(1, 1), 1), IntLiteral(1))
                    ),
                    op: BinaryOp::Add,
                    right: Box::new(
                        Binary(
                            Token::Star(Position::new(1, 7)),
                            BinaryNode {
                                typ: None,
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
    fn parse_binary_errors() -> TestResult {
        let err = parse("-5 +").unwrap_err();
        Ok(assert_eq!(ParseError::UnexpectedEof, err))
    }
}
