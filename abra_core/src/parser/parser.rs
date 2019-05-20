use std::iter::Peekable;
use std::vec::IntoIter;
use crate::lexer::tokens::Token;
use crate::parser::ast::{AstNode, AstLiteralNode, UnaryOp, BinaryOp, UnaryNode, BinaryNode, ArrayNode, BindingDeclNode, AssignmentNode, TypeIdentifier, IndexingMode, IndexingNode, GroupedNode, IfNode};
use crate::parser::precedence::Precedence;
use crate::parser::parse_error::ParseError;

pub fn parse(tokens: Vec<Token>) -> Result<Vec<AstNode>, ParseError> {
    let mut parser = Parser::new(tokens);

    let mut nodes: Vec<AstNode> = vec![];
    loop {
        match parser.peek() {
            Some(_) => nodes.push(parser.parse_stmt()?),
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

    fn expect_peek(&mut self) -> Result<&Token, ParseError> {
        self.peek().ok_or(ParseError::UnexpectedEof)
    }

    // Begin Pratt plumbing

    fn parse_precedence(&mut self, prec: Precedence) -> Result<AstNode, ParseError> {
        let prefix_token = self.expect_peek()?.clone();

        match self.get_prefix_rule(&prefix_token) {
            None => Err(ParseError::UnexpectedToken(prefix_token)),
            Some(prefix_fn) => {
                // Cursor sits on token AFTER the match token; in-/prefix fns always start on token AFTER the one they match
                let prefix_token = self.expect_next()?;

                let mut left: AstNode = (*prefix_fn)(self, prefix_token)?;

                let prec: u8 = prec.into();
                loop {
                    if let Some(infix_token) = self.peek() {
                        let next_prec = Parser::get_precedence_for_token(&infix_token);
                        let next_prec: u8 = next_prec.into();
                        if prec < next_prec {
                            let infix_token = self.expect_next()?;

                            if let Some(infix_fn) = self.get_infix_rule(&infix_token) {
                                left = (*infix_fn)(self, infix_token, left)?
                            }
                        } else {
                            break;
                        }
                    } else {
                        break;
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
            Token::LParen(_) => Some(Box::new(Parser::parse_grouped)),
            Token::LBrack(_) => Some(Box::new(Parser::parse_array)),
            Token::Ident(_, _) => Some(Box::new(Parser::parse_ident)),
            Token::If(_) => Some(Box::new(Parser::parse_if_expr)),
            _ => None,
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
            Token::RBrack(_) |
            Token::Comma(_) |
            Token::Ident(_, _) => None,
            Token::LBrack(_) => Some(Box::new(Parser::parse_index)),
            Token::Assign(_) => Some(Box::new(Parser::parse_assignment)),
            _ => Some(Box::new(Parser::parse_binary)),
        }
    }

    fn get_precedence_for_token(tok: &Token) -> Precedence {
        match tok {
            Token::Plus(_) | Token::Minus(_) => Precedence::Addition,
            Token::Star(_) | Token::Slash(_) => Precedence::Multiplication,
            Token::And(_) => Precedence::And,
            Token::Or(_) => Precedence::Or,
            Token::Elvis(_) => Precedence::Coalesce,
            Token::Eq(_) | Token::Neq(_) => Precedence::Equality,
            Token::GT(_) | Token::GTE(_) | Token::LT(_) | Token::LTE(_) => Precedence::Comparison,
            Token::Assign(_) => Precedence::Assignment,
            Token::LBrack(_) => Precedence::Call,
            _ => Precedence::None,
        }
    }

    // End Pratt plumbing

    fn parse_stmt(&mut self) -> Result<AstNode, ParseError> {
        match self.expect_peek()? {
            Token::Val(_) => self.parse_binding_decl(),
            Token::Var(_) => self.parse_binding_decl(),
            Token::If(_) => self.parse_if_statement(),
            _ => self.parse_expr(),
        }
    }

    fn parse_type_identifier(&mut self) -> Result<TypeIdentifier, ParseError> {
        let mut left = match self.expect_next()? {
            ident @ Token::Ident(_, _) => Ok(TypeIdentifier::Normal { ident }),
            t @ _ => Err(ParseError::ExpectedToken(
                // FIXME: Not great, using "identifier" here, just so it shows up in the error
                Token::Ident(t.get_position(), "identifier".to_string()),
                t,
            ))
        }?;

        let mut next_token = self.peek();
        loop {
            match next_token {
                Some(Token::LBrack(_)) => {
                    self.expect_next()?; // Consume '['
                    match self.expect_peek()? {
                        Token::RBrack(_) => self.expect_next(), // Consume ']'
                        t => Err(ParseError::ExpectedToken(
                            Token::RBrack(t.get_position()),
                            t.clone(),
                        )),
                    }?;
                    left = TypeIdentifier::Array { inner: Box::new(left) }
                }
                Some(Token::Question(_)) => {
                    self.expect_next()?; // Consume '?'
                    left = TypeIdentifier::Option { inner: Box::new(left) }
                }
                _ => break
            }
            next_token = self.peek();
        };

        Ok(left)
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

        let type_ann = match self.peek() {
            Some(Token::Colon(_)) => {
                self.expect_next()?; // Consume ':'
                let type_ident = self.parse_type_identifier()?;
                Ok(Some(type_ident))
            }
            Some(_) | None => Ok(None)
        }?;

        let expr = match self.peek() {
            Some(Token::Assign(_)) => {
                self.expect_next()?; // Consume '='
                let expr = self.parse_expr()?;
                Some(Box::new(expr))
            }
            Some(_) | None => None
        };

        Ok(AstNode::BindingDecl(token, BindingDeclNode { ident, is_mutable, type_ann, expr }))
    }

    #[inline]
    fn parse_expr_or_block(&mut self) -> Result<Vec<AstNode>, ParseError> {
        match self.peek() {
            Some(Token::LBrace(_)) => {
                self.expect_next()?; // Consume '{'
                let mut body = Vec::<AstNode>::new();
                loop {
                    match self.peek() {
                        Some(Token::RBrace(_)) => {
                            self.expect_next()?; // Consume '}'
                            break Ok(body);
                        }
                        Some(_) => body.push(self.parse_stmt()?),
                        None => break Err(ParseError::UnexpectedEof)
                    }
                }
            }
            _ => Ok(vec![self.parse_expr()?])
        }
    }

    fn parse_if_node(&mut self) -> Result<IfNode, ParseError> {
        // Consume '(', or fail
        match self.expect_next()? {
            Token::LParen(_) => Ok(()),
            t @ _ => Err(ParseError::ExpectedToken(Token::LParen(t.get_position()), t))
        }?;
        let condition = Box::new(self.parse_expr()?);
        // Consume ')', or fail
        match self.expect_next()? {
            Token::RParen(_) => Ok(()),
            t @ _ => Err(ParseError::ExpectedToken(Token::RParen(t.get_position()), t))
        }?;

        let if_block = self.parse_expr_or_block()?;

        let else_block = if let Some(Token::Else(_)) = self.peek() {
            self.expect_next()?; // Consume 'else'
            if let Some(Token::If(_)) = self.peek() {
                Some(vec![self.parse_if_statement()?])
            } else {
                Some(self.parse_expr_or_block()?)
            }
        } else {
            None
        };
        Ok(IfNode { condition, if_block, else_block })
    }

    fn parse_if_statement(&mut self) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;
        let if_node = self.parse_if_node()?;
        Ok(AstNode::IfStatement(token, if_node))
    }

    fn parse_if_expr(&mut self, token: Token) -> Result<AstNode, ParseError> {
        let if_node = self.parse_if_node()?;
        Ok(AstNode::IfExpression(token, if_node))
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

    fn parse_grouped(&mut self, token: Token) -> Result<AstNode, ParseError> {
        let expr = self.parse_expr()?;
        match self.expect_next()? {
            Token::RParen(_) => {
                Ok(AstNode::Grouped(token, GroupedNode { expr: Box::new(expr) }))
            }
            t @ _ => Err(ParseError::UnexpectedToken(t))
        }
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
            Token::Elvis(_) => BinaryOp::Coalesce,
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

    fn parse_index(&mut self, token: Token, left: AstNode) -> Result<AstNode, ParseError> {
        let mode = match self.expect_peek()? {
            Token::RBrack(_) => Err(ParseError::UnexpectedToken(self.expect_next()?)),
            Token::Colon(_) => {
                self.expect_next()?; // Consume ':'
                let range_end = self.parse_expr()?;

                match self.expect_next()? {
                    Token::RBrack(_) => Ok(IndexingMode::Range(None, Some(Box::new(range_end)))),
                    t @ _ => Err(ParseError::UnexpectedToken(t))
                }
            }
            _ => {
                let idx = self.parse_expr()?;
                match self.expect_next()? {
                    Token::RBrack(_) => Ok(IndexingMode::Index(Box::new(idx))),
                    Token::Colon(_) => {
                        match self.expect_peek()? {
                            Token::RBrack(_) => {
                                self.expect_next()?; // Consume ']'
                                Ok(IndexingMode::Range(Some(Box::new(idx)), None))
                            }
                            _ => {
                                let range_end = self.parse_expr()?;
                                match self.expect_next()? {
                                    Token::RBrack(_) => Ok(IndexingMode::Range(Some(Box::new(idx)), Some(Box::new(range_end)))),
                                    t @ _ => Err(ParseError::UnexpectedToken(t))
                                }
                            }
                        }
                    }
                    t @ _ => Err(ParseError::UnexpectedToken(t))
                }
            }
        }?;
        Ok(AstNode::Indexing(token, IndexingNode {
            target: Box::new(left),
            index: mode,
        }))
    }

    fn parse_assignment(&mut self, token: Token, left: AstNode) -> Result<AstNode, ParseError> {
        // By passing Prec::None (one lower than Prec::Assign), we achieve right-associativity
        // (i.e. a = b = c ==> (a = (b = c))
        let expr = self.parse_precedence(Precedence::None)?;
        let node = AssignmentNode { target: Box::new(left), expr: Box::new(expr) };
        Ok(AstNode::Assignment(token, node))
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

    fn parse_ident(&mut self, token: Token) -> Result<AstNode, ParseError> {
        match &token {
            Token::Ident(_, _) => Ok(AstNode::Identifier(token)),
            _ => Err(ParseError::UnexpectedToken(token)) // This should be unreachable, but just in case...
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokens::{Token, Position};
    use crate::parser::ast::AstNode::*;
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
            int_literal!((1, 1), 123),
            float_literal!((1, 5), 4.56),
            float_literal!((1, 10), 0.789),
            string_literal!((1, 16), "hello world"),
            bool_literal!((1, 30), true),
            bool_literal!((1, 35), false),
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
                    expr: Box::new(int_literal!((1, 2), 123)),
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
                    expr: Box::new(bool_literal!((1, 2), true)),
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
                    left: Box::new(float_literal!((1, 1), 1.2)),
                    op: BinaryOp::Add,
                    right: Box::new(int_literal!((1, 7), 3)),
                },
            )
        ];
        assert_eq!(expected, ast);

        let ast = parse("\"hello \" + \"world\"")?;
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 10)),
                BinaryNode {
                    left: Box::new(string_literal!((1, 1), "hello ")),
                    op: BinaryOp::Add,
                    right: Box::new(string_literal!((1, 12), "world")),
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
                    left: Box::new(int_literal!((1, 1), 1)),
                    op: BinaryOp::Add,
                    right: Box::new(
                        Unary(
                            Token::Minus(Position::new(1, 5)),
                            UnaryNode {
                                op: UnaryOp::Minus,
                                expr: Box::new(int_literal!((1, 6), 2)),
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
                    left: Box::new(bool_literal!((1, 1), true)),
                    op: BinaryOp::And,
                    right: Box::new(
                        Unary(
                            Token::Bang(Position::new(1, 9)),
                            UnaryNode {
                                op: UnaryOp::Negate,
                                expr: Box::new(bool_literal!((1, 10), false)),
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
                    left: Box::new(int_literal!((1, 1), 1)),
                    op: BinaryOp::Add,
                    right: Box::new(
                        Binary(
                            Token::Star(Position::new(1, 7)),
                            BinaryNode {
                                left: Box::new(int_literal!((1, 5), 2)),
                                op: BinaryOp::Mul,
                                right: Box::new(int_literal!((1, 9), 3)),
                            },
                        )
                    ),
                },
            )
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_binary_with_grouping() -> TestResult {
        let ast = parse("(1 + 2) * 3")?;
        let expected = vec![
            Binary(
                Token::Star(Position::new(1, 9)),
                BinaryNode {
                    left: Box::new(
                        Grouped(
                            Token::LParen(Position::new(1, 1)),
                            GroupedNode {
                                expr: Box::new(
                                    Binary(
                                        Token::Plus(Position::new(1, 4)),
                                        BinaryNode {
                                            left: Box::new(int_literal!((1, 2), 1)),
                                            op: BinaryOp::Add,
                                            right: Box::new(int_literal!((1, 6), 2)),
                                        },
                                    )
                                )
                            },
                        )
                    ),
                    op: BinaryOp::Mul,
                    right: Box::new(int_literal!((1, 11), 3)),
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
                                left: Box::new(bool_literal!((1, 1), true)),
                                op: BinaryOp::And,
                                right: Box::new(bool_literal!((1, 9), true)),
                            },
                        )
                    ),
                    op: BinaryOp::Or,
                    right: Box::new(
                        Binary(
                            Token::And(Position::new(1, 23)),
                            BinaryNode {
                                left: Box::new(bool_literal!((1, 17), false)),
                                op: BinaryOp::And,
                                right: Box::new(bool_literal!((1, 26), false)),
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
                                            left: Box::new(int_literal!((1, 1), 1)),
                                            op: BinaryOp::Add,
                                            right: Box::new(int_literal!((1, 5), 2)),
                                        },
                                    )
                                ),
                                op: BinaryOp::Lte,
                                right: Box::new(int_literal!((1, 10), 3)),
                            },
                        )
                    ),
                    op: BinaryOp::Neq,
                    right: Box::new(
                        Binary(
                            Token::GTE(Position::new(1, 18)),
                            BinaryNode {
                                left: Box::new(int_literal!((1, 15), 11)),
                                op: BinaryOp::Gte,
                                right: Box::new(int_literal!((1, 21), 13)),
                            },
                        )
                    ),
                },
            )
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_binary_precedence_coalesce() -> TestResult {
        let ast = parse("abc ?: true || false")?;
        let expected = vec![
            Binary(
                Token::Or(Position::new(1, 13)),
                BinaryNode {
                    left: Box::new(
                        Binary(
                            Token::Elvis(Position::new(1, 5)),
                            BinaryNode {
                                left: Box::new(identifier!((1, 1), "abc")),
                                op: BinaryOp::Coalesce,
                                right: Box::new(bool_literal!((1, 8), true)),
                            },
                        )
                    ),
                    op: BinaryOp::Or,
                    right: Box::new(bool_literal!((1, 16), false)),
                },
            )
        ];
        assert_eq!(expected, ast);

        let ast = parse("false || abc ?: true")?;
        let expected = vec![
            Binary(
                Token::Or(Position::new(1, 7)),
                BinaryNode {
                    left: Box::new(bool_literal!((1, 1), false)),
                    op: BinaryOp::Or,
                    right: Box::new(
                        Binary(
                            Token::Elvis(Position::new(1, 14)),
                            BinaryNode {
                                left: Box::new(identifier!((1, 10), "abc")),
                                op: BinaryOp::Coalesce,
                                right: Box::new(bool_literal!((1, 17), true)),
                            },
                        )
                    ),
                },
            )
        ];
        assert_eq!(expected, ast);

        let ast = parse("1 + abc ?: 0 != !def ?: true")?;
        let expected = vec![
            Binary(
                Token::Neq(Position::new(1, 14)),
                BinaryNode {
                    left: Box::new(
                        Binary(
                            Token::Plus(Position::new(1, 3)),
                            BinaryNode {
                                left: Box::new(int_literal!((1, 1), 1)),
                                op: BinaryOp::Add,
                                right: Box::new(
                                    Binary(
                                        Token::Elvis(Position::new(1, 9)),
                                        BinaryNode {
                                            left: Box::new(identifier!((1, 5), "abc")),
                                            op: BinaryOp::Coalesce,
                                            right: Box::new(int_literal!((1, 12), 0)),
                                        },
                                    )
                                ),
                            },
                        )
                    ),
                    op: BinaryOp::Neq,
                    right: Box::new(
                        Binary(
                            Token::Elvis(Position::new(1, 22)),
                            BinaryNode {
                                left: Box::new(
                                    Unary(
                                        Token::Bang(Position::new(1, 17)),
                                        UnaryNode {
                                            op: UnaryOp::Negate,
                                            expr: Box::new(identifier!((1, 18), "def")),
                                        },
                                    )
                                ),
                                op: BinaryOp::Coalesce,
                                right: Box::new(bool_literal!((1, 25), true)),
                            },
                        )
                    ),
                },
            ),
        ];
        assert_eq!(expected, ast);

        let ast = parse("abc ?: def[0] == 0 <= def ?: 9")?;
        let expected = vec![
            Binary(
                Token::Eq(Position::new(1, 15)),
                BinaryNode {
                    left: Box::new(
                        Binary(
                            Token::Elvis(Position::new(1, 5)),
                            BinaryNode {
                                left: Box::new(identifier!((1, 1), "abc")),
                                op: BinaryOp::Coalesce,
                                right: Box::new(
                                    Indexing(
                                        Token::LBrack(Position::new(1, 11)),
                                        IndexingNode {
                                            target: Box::new(identifier!((1, 8), "def")),
                                            index: IndexingMode::Index(Box::new(int_literal!((1, 12), 0))),
                                        },
                                    )
                                ),
                            },
                        )
                    ),
                    op: BinaryOp::Eq,
                    right: Box::new(
                        Binary(
                            Token::LTE(Position::new(1, 20)),
                            BinaryNode {
                                left: Box::new(int_literal!((1, 18), 0)),
                                op: BinaryOp::Lte,
                                right: Box::new(
                                    Binary(
                                        Token::Elvis(Position::new(1, 27)),
                                        BinaryNode {
                                            left: Box::new(identifier!((1, 23), "def")),
                                            op: BinaryOp::Coalesce,
                                            right: Box::new(int_literal!((1, 30), 9)),
                                        },
                                    )
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
                    Box::new(int_literal!((1, 2), 1)),
                    Box::new(bool_literal!((1, 5), true)),
                    Box::new(string_literal!((1, 11), "a")),
                    Box::new(float_literal!((1, 16), 3.14))
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
                                Box::new(int_literal!((1, 3), 1)),
                                Box::new(int_literal!((1, 6), 2)),
                            ]
                        },
                    )),
                    Box::new(AstNode::Array(
                        Token::LBrack(Position::new(1, 10)),
                        ArrayNode {
                            items: vec![
                                Box::new(int_literal!((1, 11), 3)),
                                Box::new(int_literal!((1, 14), 4)),
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
                    type_ann: None,
                    expr: None,
                },
            ),
            AstNode::BindingDecl(
                Token::Var(Position::new(2, 1)),
                BindingDeclNode {
                    ident: Token::Ident(Position::new(2, 5), "abc".to_string()),
                    is_mutable: true,
                    type_ann: None,
                    expr: None,
                },
            ),
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_binding_decls_with_assignment() -> TestResult {
        let ast = parse("val abc = 1 + \"a\"\nvar abc = 1")?;
        let expected = vec![
            AstNode::BindingDecl(
                Token::Val(Position::new(1, 1)),
                BindingDeclNode {
                    ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                    is_mutable: false,
                    type_ann: None,
                    expr: Some(Box::new(
                        AstNode::Binary(
                            Token::Plus(Position::new(1, 13)),
                            BinaryNode {
                                left: Box::new(int_literal!((1, 11), 1)),
                                op: BinaryOp::Add,
                                right: Box::new(string_literal!((1, 15), "a")),
                            },
                        ),
                    )),
                },
            ),
            AstNode::BindingDecl(
                Token::Var(Position::new(2, 1)),
                BindingDeclNode {
                    ident: Token::Ident(Position::new(2, 5), "abc".to_string()),
                    is_mutable: true,
                    type_ann: None,
                    expr: Some(Box::new(int_literal!((2, 11), 1))),
                },
            ),
        ];
        Ok(assert_eq!(expected, ast))
    }

    #[test]
    fn parse_type_annotations() -> TestResult {
        fn ast_to_type_ann(ast: Vec<AstNode>) -> TypeIdentifier {
            match ast.into_iter().next() {
                Some(AstNode::BindingDecl(_, BindingDeclNode { type_ann, .. })) => type_ann.unwrap(),
                _ => unreachable!()
            }
        }

        let ast = parse("var abc: Bool")?;
        let expected = TypeIdentifier::Normal {
            ident: Token::Ident(Position::new(1, 10), "Bool".to_string())
        };
        let type_ann = ast_to_type_ann(ast);
        assert_eq!(expected, type_ann);

        let ast = parse("var abc: Int[]")?;
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Normal {
                ident: Token::Ident(Position::new(1, 10), "Int".to_string())
            })
        };
        let type_ann = ast_to_type_ann(ast);
        assert_eq!(expected, type_ann);

        let ast = parse("var abc: Int[][]")?;
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Array {
                inner: Box::new(TypeIdentifier::Normal {
                    ident: Token::Ident(Position::new(1, 10), "Int".to_string())
                })
            })
        };
        let type_ann = ast_to_type_ann(ast);
        assert_eq!(expected, type_ann);

        let ast = parse("var abc: Int?")?;
        let expected = TypeIdentifier::Option {
            inner: Box::new(TypeIdentifier::Normal {
                ident: Token::Ident(Position::new(1, 10), "Int".to_string())
            })
        };
        let type_ann = ast_to_type_ann(ast);
        assert_eq!(expected, type_ann);

        let ast = parse("var abc: Int?[]")?;
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Option {
                inner: Box::new(TypeIdentifier::Normal {
                    ident: Token::Ident(Position::new(1, 10), "Int".to_string())
                })
            })
        };
        let type_ann = ast_to_type_ann(ast);
        assert_eq!(expected, type_ann);

        let ast = parse("var abc: Int?[]?")?;
        let expected = TypeIdentifier::Option {
            inner: Box::new(TypeIdentifier::Array {
                inner: Box::new(TypeIdentifier::Option {
                    inner: Box::new(TypeIdentifier::Normal {
                        ident: Token::Ident(Position::new(1, 10), "Int".to_string())
                    })
                })
            })
        };
        let type_ann = ast_to_type_ann(ast);
        assert_eq!(expected, type_ann);

        Ok(())
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

    #[test]
    fn parse_binding_decls_with_type_annotations_error() {
        let err = parse("val a: 123 = 123").unwrap_err();
        let expected = ParseError::ExpectedToken(
            Token::Ident(Position::new(1, 8), "identifier".to_string()),
            Token::Int(Position::new(1, 8), 123),
        );
        assert_eq!(expected, err);

        let err = parse("val a: Int[ = 123").unwrap_err();
        let expected = ParseError::ExpectedToken(
            Token::RBrack(Position::new(1, 13)),
            Token::Assign(Position::new(1, 13)),
        );
        assert_eq!(expected, err);

        let err = parse("val a: Int[").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, err);
    }

    #[test]
    fn parse_ident() -> TestResult {
        let ast = parse("abcd")?;
        let expected = identifier!((1, 1), "abcd");
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_assignment() -> TestResult {
        let ast = parse("abc = 123")?;
        let expected = AstNode::Assignment(
            Token::Assign(Position::new(1, 5)),
            AssignmentNode {
                target: Box::new(identifier!((1, 1), "abc")),
                expr: Box::new(int_literal!((1, 7), 123)),
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abc = def")?;
        let expected = AstNode::Assignment(
            Token::Assign(Position::new(1, 5)),
            AssignmentNode {
                target: Box::new(identifier!((1, 1), "abc")),
                expr: Box::new(identifier!((1, 7), "def")),
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("a = b = c")?;
        let expected = AstNode::Assignment(
            Token::Assign(Position::new(1, 3)),
            AssignmentNode {
                target: Box::new(identifier!((1, 1), "a")),
                expr: Box::new(
                    AstNode::Assignment(
                        Token::Assign(Position::new(1, 7)),
                        AssignmentNode {
                            target: Box::new(identifier!((1, 5), "b")),
                            expr: Box::new(identifier!((1, 9), "c")),
                        },
                    )
                ),
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_indexing() -> TestResult {
        let ast = parse("abcd[1]")?;
        let expected = AstNode::Indexing(
            Token::LBrack(Position::new(1, 5)),
            IndexingNode {
                target: Box::new(identifier!((1, 1), "abcd")),
                index: IndexingMode::Index(Box::new(int_literal!((1, 6), 1))),
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abcd[1:3]")?;
        let expected = AstNode::Indexing(
            Token::LBrack(Position::new(1, 5)),
            IndexingNode {
                target: Box::new(identifier!((1, 1), "abcd")),
                index: IndexingMode::Range(
                    Some(Box::new(int_literal!((1, 6), 1))),
                    Some(Box::new(int_literal!((1, 8), 3))),
                ),
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abcd[a:]")?;
        let expected = AstNode::Indexing(
            Token::LBrack(Position::new(1, 5)),
            IndexingNode {
                target: Box::new(identifier!((1, 1), "abcd")),
                index: IndexingMode::Range(
                    Some(Box::new(
                        AstNode::Identifier(
                            Token::Ident(Position::new(1, 6), "a".to_string()),
                        )
                    )),
                    None,
                ),
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abcd[:b]")?;
        let expected = AstNode::Indexing(
            Token::LBrack(Position::new(1, 5)),
            IndexingNode {
                target: Box::new(identifier!((1, 1), "abcd")),
                index: IndexingMode::Range(
                    None,
                    Some(Box::new(
                        AstNode::Identifier(
                            Token::Ident(Position::new(1, 7), "b".to_string()),
                        )
                    )),
                ),
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_indexing_nested() -> TestResult {
        let ast = parse("a[b[2]]")?;
        let expected = AstNode::Indexing(
            Token::LBrack(Position::new(1, 2)),
            IndexingNode {
                target: Box::new(identifier!((1, 1), "a")),
                index: IndexingMode::Index(Box::new(
                    AstNode::Indexing(
                        Token::LBrack(Position::new(1, 4)),
                        IndexingNode {
                            target: Box::new(identifier!((1, 3), "b")),
                            index: IndexingMode::Index(Box::new(
                                int_literal!((1, 5), 2)
                            )),
                        },
                    )
                )),
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("[a, b][1][2]")?;
        let expected = AstNode::Indexing(
            Token::LBrack(Position::new(1, 10)),
            IndexingNode {
                target: Box::new(AstNode::Indexing(
                    Token::LBrack(Position::new(1, 7)),
                    IndexingNode {
                        target: Box::new(
                            AstNode::Array(
                                Token::LBrack(Position::new(1, 1)),
                                ArrayNode {
                                    items: vec![
                                        Box::new(identifier!((1, 2), "a")),
                                        Box::new(identifier!((1, 5), "b")),
                                    ],
                                },
                            )
                        ),
                        index: IndexingMode::Index(Box::new(
                            int_literal!((1, 8), 1)
                        )),
                    },
                )),
                index: IndexingMode::Index(Box::new(
                    int_literal!((1, 11), 2)
                )),
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_indexing_error() {
        let err = parse("abcd[]").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::RBrack(Position::new(1, 6)));
        assert_eq!(expected, err);

        let err = parse("abcd[: val b = 3").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Val(Position::new(1, 8)));
        assert_eq!(expected, err);

        let err = parse("abcd[1a").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Ident(Position::new(1, 7), "a".to_string()));
        assert_eq!(expected, err);

        let err = parse("abcd[1:1:").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Colon(Position::new(1, 9)));
        assert_eq!(expected, err);
    }

    #[test]
    fn parse_if_statement() -> TestResult {
        // Each phase of testing tests permutations of with/without braces

        let ast = parse("if (3 < 4)   \"hello\"")?;
        let expected = AstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            IfNode {
                condition: Box::new(
                    AstNode::Binary(
                        Token::LT(Position::new(1, 7)),
                        BinaryNode {
                            left: Box::new(int_literal!((1, 5), 3)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 9), 4)),
                        },
                    )
                ),
                if_block: vec![
                    string_literal!((1, 14), "hello")
                ],
                else_block: None,
            },
        );
        assert_eq!(expected, ast[0]);
        let ast = parse("if (3 < 4) { \"hello\" }")?;
        assert_eq!(expected, ast[0]);

        let ast = parse("if (3 < 4)   \"hello\"   else   \"world\"")?;
        let expected = AstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            IfNode {
                condition: Box::new(
                    AstNode::Binary(
                        Token::LT(Position::new(1, 7)),
                        BinaryNode {
                            left: Box::new(int_literal!((1, 5), 3)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 9), 4)),
                        },
                    )
                ),
                if_block: vec![
                    string_literal!((1, 14), "hello")
                ],
                else_block: Some(vec![
                    string_literal!((1, 31), "world")
                ]),
            },
        );
        assert_eq!(expected, ast[0]);
        let ast = parse("if (3 < 4) { \"hello\" } else { \"world\" }")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if (3 < 4) { \"hello\" } else   \"world\"")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if (3 < 4)   \"hello\"   else { \"world\" }")?;
        assert_eq!(expected, ast[0]);

        let ast = parse("if (3 < 4)   \"hello\"   else if (true)   \"world\"   else   \"!\"")?;
        let expected = AstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            IfNode {
                condition: Box::new(
                    AstNode::Binary(
                        Token::LT(Position::new(1, 7)),
                        BinaryNode {
                            left: Box::new(int_literal!((1, 5), 3)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 9), 4)),
                        },
                    )
                ),
                if_block: vec![
                    string_literal!((1, 14), "hello")
                ],
                else_block: Some(vec![
                    AstNode::IfStatement(
                        Token::If(Position::new(1, 29)),
                        IfNode {
                            condition: Box::new(bool_literal!((1, 33), true)),
                            if_block: vec![
                                string_literal!((1, 41), "world")
                            ],
                            else_block: Some(vec![
                                string_literal!((1, 58), "!")
                            ]),
                        },
                    )
                ]),
            },
        );

        assert_eq!(expected, ast[0]);
        let ast = parse("if (3 < 4) { \"hello\" } else if (true)   \"world\"   else   \"!\"")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if (3 < 4) { \"hello\" } else if (true) { \"world\" } else   \"!\"")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if (3 < 4) { \"hello\" } else if (true) { \"world\" } else { \"!\" }")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if (3 < 4)   \"hello\"   else if (true) { \"world\" } else { \"!\" }")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if (3 < 4)   \"hello\"   else if (true)   \"world\"   else { \"!\" }")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if (3 < 4)   \"hello\"   else if (true) { \"world\" } else   \"!\"")?;
        assert_eq!(expected, ast[0]);

        // Test with statement inside block
        let ast = parse("if (3 < 4) { val a = \"hello\" a }")?;
        let expected = AstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            IfNode {
                condition: Box::new(
                    AstNode::Binary(
                        Token::LT(Position::new(1, 7)),
                        BinaryNode {
                            left: Box::new(int_literal!((1, 5), 3)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 9), 4)),
                        },
                    )
                ),
                if_block: vec![
                    AstNode::BindingDecl(
                        Token::Val(Position::new(1, 14)),
                        BindingDeclNode {
                            is_mutable: false,
                            ident: Token::Ident(Position::new(1, 18), "a".to_string()),
                            type_ann: None,
                            expr: Some(Box::new(string_literal!((1, 22), "hello"))),
                        },
                    ),
                    identifier!((1, 30), "a")
                ],
                else_block: None,
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_if_expression() -> TestResult {
        let ast = parse("val str = if (3 < 4) \"hello\"")?;
        let expected = AstNode::BindingDecl(
            Token::Val(Position::new(1, 1)),
            BindingDeclNode {
                ident: Token::Ident(Position::new(1, 5), "str".to_string()),
                is_mutable: false,
                type_ann: None,
                expr: Some(Box::new(
                    AstNode::IfExpression(
                        Token::If(Position::new(1, 11)),
                        IfNode {
                            condition: Box::new(
                                AstNode::Binary(
                                    Token::LT(Position::new(1, 17)),
                                    BinaryNode {
                                        left: Box::new(int_literal!((1, 15), 3)),
                                        op: BinaryOp::Lt,
                                        right: Box::new(int_literal!((1, 19), 4)),
                                    },
                                )
                            ),
                            if_block: vec![
                                string_literal!((1, 22), "hello")
                            ],
                            else_block: None,
                        },
                    )
                )),
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("\"hello\" + if (3 < 4) \"world\" else \"!\"")?;
        let expected = AstNode::Binary(
            Token::Plus(Position::new(1, 9)),
            BinaryNode {
                left: Box::new(string_literal!((1, 1), "hello")),
                op: BinaryOp::Add,
                right: Box::new(
                    AstNode::IfExpression(
                        Token::If(Position::new(1, 11)),
                        IfNode {
                            condition: Box::new(
                                AstNode::Binary(
                                    Token::LT(Position::new(1, 17)),
                                    BinaryNode {
                                        left: Box::new(int_literal!((1, 15), 3)),
                                        op: BinaryOp::Lt,
                                        right: Box::new(int_literal!((1, 19), 4)),
                                    },
                                )
                            ),
                            if_block: vec![
                                string_literal!((1, 22), "world")
                            ],
                            else_block: Some(vec![
                                string_literal!((1, 35), "!")
                            ]),
                        },
                    )
                )
            }
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }
}
