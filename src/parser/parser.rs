use crate::lexer::tokens::{Token, Position};
use std::iter::Peekable;
use std::vec::IntoIter;
use crate::parser::ast::{AstNode, Precedence, AstLiteralNode, UnaryOp, BinaryOp};
use crate::parser::parse_error::ParseError;
use std::collections::HashMap;
use std::path::Component::Prefix;

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

    fn parse_precedence(&mut self, prec: Precedence) -> Result<AstNode, ParseError> {
        let prefix_token = self.peek().expect("There should still be tokens by now").clone();
        let (prefix_fn, infix_fn) = self.get_rules_for_token(&prefix_token);
        match prefix_fn {
            None => Err(ParseError::Raw("Expected prefix fn".to_string())),
            Some(prefix_fn) => {
                // Cursor sits on token AFTER the match token; in-/prefix fns always start on token AFTER the one they match
                let prefix_token = self.advance().unwrap();

                let mut left: AstNode = (*prefix_fn)(self, prefix_token)?;

                if let Some(token) = self.peek() {
                    let next_prec = Parser::get_precedence_for_token(&token);
                    let prec: u8 = prec.into();
                    let next_prec: u8 = next_prec.into();
                    while prec < next_prec {
                        if let Some(_) = self.peek() {
                            let infix_token = self.advance().unwrap();

                            let (_, infix_fn) = self.get_rules_for_token(&infix_token);
                            if let Some(infix_fn) = infix_fn {
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

    fn parse_expr(&mut self) -> Result<AstNode, ParseError> {
        self.parse_precedence(Precedence::None)
    }

    fn get_rules_for_token(&mut self, tok: &Token) -> (Option<Box<PrefixFn>>, Option<Box<InfixFn>>) {
        use self::Parser;

        match tok {
            Token::Int(_, _) => (Some(Box::new(Parser::parse_literal)), None),
            Token::Float(_, _) => (Some(Box::new(Parser::parse_literal)), None),
            Token::Plus(_) | Token::Star(_) | Token::Slash(_) => (None, Some(Box::new(Parser::parse_binary))),
            Token::Minus(_) => (Some(Box::new(Parser::parse_unary)), Some(Box::new(Parser::parse_binary))),
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

    fn parse_literal(&mut self, token: Token) -> Result<AstNode, ParseError> {
        match &token {
            Token::Int(pos, val) => Ok(AstNode::Literal(token.clone(), AstLiteralNode::IntLiteral(*val))),
            Token::Float(pos, val) => Ok(AstNode::Literal(token.clone(), AstLiteralNode::FloatLiteral(*val))),
            _ => Err(ParseError::Raw(format!("Unknown literal: {:?}", token)))
        }
    }

    fn parse_unary(&mut self, token: Token) -> Result<AstNode, ParseError> {
        let expr = self.parse_precedence(Precedence::Unary)?;
        let unary_op = match token {
            Token::Minus(_) => UnaryOp::Minus,
            _ => unimplemented!()
        };
        Ok(AstNode::Unary(token, unary_op, Box::new(expr)))
    }

    fn parse_binary(&mut self, token: Token, left: AstNode) -> Result<AstNode, ParseError> {
        let prec = Parser::get_precedence_for_token(&token);
        let right = self.parse_precedence(prec)?;
        let binary_op = match token {
            Token::Plus(_) => BinaryOp::Add,
            Token::Minus(_) => BinaryOp::Sub,
            Token::Star(_) => BinaryOp::Mul,
            Token::Slash(_) => BinaryOp::Div,
            _ => unimplemented!()
        };
        Ok(AstNode::Binary(token, Box::new(left), binary_op, Box::new(right)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::AstNode::*;
    use crate::parser::ast::AstLiteralNode::*;
    use crate::lexer::lexer::tokenize;

    fn parse(input: &str) -> Vec<AstNode> {
        let tokens = tokenize(&input.to_string());
        super::parse(tokens).unwrap()
    }

    #[test]
    fn parse_literals() {
        let ast = parse("123 4.56 0.789");
        let expected = vec![
            Literal(Token::Int(Position::new(1, 1), 123), IntLiteral(123)),
            Literal(Token::Float(Position::new(1, 5), 4.56), FloatLiteral(4.56)),
            Literal(Token::Float(Position::new(1, 10), 0.789), FloatLiteral(0.789)),
        ];
        assert_eq!(expected, ast);
    }

    #[test]
    fn parse_unary() {
        let ast = parse("-123");
        let expected = vec![
            Unary(
                Token::Minus(Position::new(1, 1)),
                UnaryOp::Minus,
                Box::new(
                    Literal(Token::Int(Position::new(1, 2), 123), IntLiteral(123))
                ),
            )
        ];
        assert_eq!(expected, ast);
    }

    #[test]
    fn parse_binary() {
        let ast = parse("1.2 + 3");
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 5)),
                Box::new(
                    Literal(Token::Float(Position::new(1, 1), 1.2), FloatLiteral(1.2))
                ),
                BinaryOp::Add,
                Box::new(
                    Literal(Token::Int(Position::new(1, 7), 3), IntLiteral(3))
                ),
            )
        ];
        assert_eq!(expected, ast);
    }

    #[test]
    fn parse_binary_and_unary() {
        let ast = parse("1 + -2");
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 3)),
                Box::new(
                    Literal(Token::Int(Position::new(1, 1), 1), IntLiteral(1))
                ),
                BinaryOp::Add,
                Box::new(
                    Unary(
                        Token::Minus(Position::new(1, 5)),
                        UnaryOp::Minus,
                        Box::new(
                            Literal(Token::Int(Position::new(1, 6), 2), IntLiteral(2))
                        ),
                    )
                ),
            )
        ];
        assert_eq!(expected, ast);
    }

    #[test]
    fn parse_binary_precedence() {
        let ast = parse("1 + 2 * 3");
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 3)),
                Box::new(
                    Literal(Token::Int(Position::new(1, 1), 1), IntLiteral(1))
                ),
                BinaryOp::Add,
                Box::new(
                    Binary(
                        Token::Star(Position::new(1, 7)),
                        Box::new(
                            Literal(Token::Int(Position::new(1, 5), 2), IntLiteral(2))
                        ),
                        BinaryOp::Mul,
                        Box::new(
                            Literal(Token::Int(Position::new(1, 9), 3), IntLiteral(3))
                        ),
                    )
                ),
            )
        ];
        assert_eq!(expected, ast);
    }
}
