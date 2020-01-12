use std::iter::Peekable;
use std::vec::IntoIter;

use crate::lexer::tokens::{Token, TokenType};
use crate::parser::ast::{ArrayNode, AssignmentNode, AstLiteralNode, AstNode, BinaryNode, BinaryOp, BindingDeclNode, ForLoopNode, FunctionDeclNode, GroupedNode, IfNode, IndexingMode, IndexingNode, InvocationNode, TypeIdentifier, UnaryNode, UnaryOp, WhileLoopNode, TypeDeclNode, MapNode, AccessorNode};
use crate::parser::parse_error::ParseError;
use crate::parser::precedence::Precedence;

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

#[derive(PartialEq)]
enum Context {
    ParsingExpr,
}

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
    context: Vec<Context>,
}

type PrefixFn = dyn Fn(&mut Parser, Token) -> Result<AstNode, ParseError>;
type InfixFn = dyn Fn(&mut Parser, Token, AstNode) -> Result<AstNode, ParseError>;

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        let tokens = tokens.into_iter().peekable();
        Parser { tokens, context: vec![] }
    }

    fn is_context(&self, ctx: Context) -> bool {
        match self.context.last() {
            None => false,
            Some(c) => c == &ctx
        }
    }

    fn enter_context(&mut self, ctx: Context) {
        self.context.push(ctx);
    }

    fn exit_context(&mut self) {
        self.context.pop();
    }

    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn expect_next(&mut self) -> Result<Token, ParseError> {
        self.advance().ok_or(ParseError::UnexpectedEof)
    }

    fn expect_next_token(&mut self, expected_token: TokenType) -> Result<Token, ParseError> {
        self.expect_next().and_then(|tok| {
            let token_type: TokenType = tok.clone().into();
            if token_type != expected_token {
                Err(ParseError::ExpectedToken(expected_token, tok))
            } else {
                Ok(tok)
            }
        })
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
        match tok {
            Token::Int(_, _) |
            Token::Float(_, _) |
            Token::String(_, _) |
            Token::Bool(_, _) => Some(Box::new(Parser::parse_literal)),
            Token::Minus(_) | Token::Bang(_) => Some(Box::new(Parser::parse_unary)),
            Token::LParen(_) => Some(Box::new(Parser::parse_grouped)),
            Token::LBrack(_) => Some(Box::new(Parser::parse_array)),
            Token::LBrace(_) => Some(Box::new(Parser::parse_map_literal)),
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
            Token::LParen(_) => Some(Box::new(Parser::parse_invocation)),
            Token::Dot(_) => Some(Box::new(Parser::parse_accessor)),
            _ => Some(Box::new(Parser::parse_binary)),
        }
    }

    fn get_precedence_for_token(tok: &Token) -> Precedence {
        match tok {
            Token::Plus(_) | Token::Minus(_) => Precedence::Addition,
            Token::Star(_) | Token::Slash(_) | Token::Percent(_) => Precedence::Multiplication,
            Token::And(_) => Precedence::And,
            Token::Or(_) => Precedence::Or,
            Token::Elvis(_) => Precedence::Coalesce,
            Token::Eq(_) | Token::Neq(_) => Precedence::Equality,
            Token::GT(_) | Token::GTE(_) | Token::LT(_) | Token::LTE(_) => Precedence::Comparison,
            Token::Assign(_) => Precedence::Assignment,
            Token::Dot(_) | Token::LBrack(_) | Token::LParen(_) => Precedence::Call,
            _ => Precedence::None,
        }
    }

    // End Pratt plumbing

    fn parse_stmt(&mut self) -> Result<AstNode, ParseError> {
        match self.expect_peek()? {
            Token::Func(_) => self.parse_func_decl(),
            Token::Val(_) => self.parse_binding_decl(),
            Token::Var(_) => self.parse_binding_decl(),
            Token::Type(_) => self.parse_type_decl(),
            Token::If(_) => self.parse_if_statement(),
            Token::While(_) => self.parse_while_statement(),
            Token::For(_) => self.parse_for_statement(),
            Token::Break(_) => self.parse_break_statement(),
            _ => self.parse_expr(),
        }
    }

    fn parse_type_identifier(&mut self) -> Result<TypeIdentifier, ParseError> {
        let mut left = match self.expect_next_token(TokenType::Ident)? {
            ident @ Token::Ident(_, _) => TypeIdentifier::Normal { ident },
            _ => unreachable!() // Since expect_next_token verifies it's a TokenType::Ident
        };

        let mut next_token = self.peek();
        loop {
            match next_token {
                Some(Token::LBrack(_)) => {
                    self.expect_next()?; // Consume '['
                    match self.expect_peek()? {
                        Token::RBrack(_) => self.expect_next(), // Consume ']'
                        t => Err(ParseError::ExpectedToken(
                            TokenType::RBrack,//::RBrack(t.get_position()),
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

    fn parse_func_decl(&mut self) -> Result<AstNode, ParseError> {
        let func_token = self.expect_next()?;

        let func_name = self.expect_next_token(TokenType::Ident)?;

        // Parsing args
        self.expect_next_token(TokenType::LParen)?;
        let mut args: Vec<(Token, Option<TypeIdentifier>, Option<AstNode>)> = Vec::new();
        loop {
            let token = self.peek().ok_or(ParseError::UnexpectedEof)?;
            match token {
                Token::RParen(_) => {
                    self.expect_next()?; // Consume ')' before ending loop
                    break;
                }
                Token::Ident(_, _) => {
                    let arg_ident = self.expect_next()?;

                    let type_ident = if let Token::Colon(_) = self.peek().ok_or(ParseError::UnexpectedEof)? {
                        self.expect_next()?; // Consume ':'
                        Some(self.parse_type_identifier()?)
                    } else { None };

                    let default_value = match self.peek().ok_or(ParseError::UnexpectedEof)? {
                        Token::Assign(_) => {
                            self.expect_next()?; // Consume '='
                            Some(self.parse_expr()?)
                        }
                        next_tok @ _ => {
                            if type_ident.is_none() {
                                return Err(ParseError::ExpectedToken(TokenType::Colon, next_tok.clone()));
                            }
                            None
                        }
                    };

                    args.push((arg_ident, type_ident, default_value));

                    match self.peek().ok_or(ParseError::UnexpectedEof)? {
                        Token::Comma(_) => self.expect_next(),
                        Token::RParen(_) => continue,
                        tok @ _ => return Err(ParseError::UnexpectedToken(tok.clone())),
                    }?;
                }
                tok @ _ => return Err(ParseError::UnexpectedToken(tok.clone())),
            }
        }

        let ret_type = match self.peek().ok_or(ParseError::UnexpectedEof)? {
            Token::Colon(_) => {
                self.expect_next()?;
                Some(self.parse_type_identifier()?)
            }
            _ => None
        };

        let body = match self.expect_peek()? {
            Token::Assign(_) => {
                self.expect_next()?;
                Ok(vec![self.parse_expr()?])
            }
            Token::LBrace(_) => self.parse_expr_or_block(),
            t @ _ => Err(ParseError::UnexpectedToken(t.clone()))
        }?;

        Ok(AstNode::FunctionDecl(func_token, FunctionDeclNode {
            name: func_name,
            args,
            ret_type,
            body,
        }))
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

    fn parse_type_decl(&mut self) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;

        let name = self.expect_next().and_then(|tok| {
            match tok {
                Token::Ident(_, _) => Ok(tok),
                _ => Err(ParseError::UnexpectedToken(tok))
            }
        })?;

        self.expect_next_token(TokenType::LBrace)?;

        let mut fields = Vec::new();
        loop {
            let token = self.peek().ok_or(ParseError::UnexpectedEof)?;
            match token {
                Token::RBrace(_) => break,
                Token::Comma(_) => {
                    self.expect_next()?; // Consume ','
                }
                Token::Ident(_, _) => {
                    let field_name = self.expect_next()?;
                    self.expect_next_token(TokenType::Colon)?;
                    let field_type = self.parse_type_identifier()?;
                    let default_value = if let Some(Token::Assign(_)) = self.peek() {
                        self.expect_next()?; // Consume '='
                        Some(self.parse_expr()?)
                    } else { None };

                    fields.push((field_name, field_type, default_value));
                }
                _ => return Err(ParseError::UnexpectedToken(token.clone()))
            }
        }

        self.expect_next_token(TokenType::RBrace)?;

        Ok(AstNode::TypeDecl(token, TypeDeclNode { name, fields }))
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

    fn parse_for_statement(&mut self) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;

        let iteratee = self.expect_next().and_then(|tok| {
            match tok {
                Token::Ident(_, _) => Ok(tok),
                _ => Err(ParseError::ExpectedToken(TokenType::Ident, tok))
            }
        })?;

        let index_ident = if let Some(Token::Comma(_)) = self.peek() {
            self.expect_next()?; // Consume ','
            let index_ident = self.expect_next().and_then(|tok| {
                match tok {
                    Token::Ident(_, _) => Ok(tok),
                    _ => Err(ParseError::ExpectedToken(TokenType::Ident, tok))
                }
            })?;
            Some(index_ident)
        } else {
            None
        };

        self.expect_next_token(TokenType::In)?;
        let iterator = Box::new(self.parse_expr()?);
        let body = self.parse_expr_or_block()?;

        Ok(AstNode::ForLoop(token, ForLoopNode { iteratee, index_ident, iterator, body }))
    }

    fn parse_while_statement(&mut self) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;
        let condition = Box::new(self.parse_expr()?);

        let body = self.parse_expr_or_block()?;
        Ok(AstNode::WhileLoop(token, WhileLoopNode { condition, body }))
    }

    fn parse_break_statement(&mut self) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;
        Ok(AstNode::Break(token))
    }

    fn parse_if_node(&mut self) -> Result<IfNode, ParseError> {
        let condition = Box::new(self.parse_expr()?);

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
        let mut if_node = self.parse_if_node()?;

        // If we're in the context of parsing an expression (ie, not at the top-level), we need to
        // know whether to treat if-elses as statements or expressions. If, for example, there's an
        // if-else as the last item in an if expression's then-block, that should be treated as an
        // expression and will be typechecked against other branches (and will be the return value).
        //
        // This code was annoying as hell to write, especially......
        let if_node = if !self.is_context(Context::ParsingExpr) {
            if_node
        } else {
            let if_node = match if_node.if_block.last() {
                Some(AstNode::IfStatement(_, _)) => {
                    match if_node.if_block.pop() {
                        Some(AstNode::IfStatement(token, node)) => {
                            if_node.if_block.push(AstNode::IfExpression(token, node));
                            if_node
                        }
                        _ => if_node
                    }
                }
                _ => if_node
            };

            // ...... this part right here, with all of the fighting I did with the borrow checker.
            // Since the else_block is an Option type, it required the code be so much more verbose.
            // There's probably a more idiomatic way to do this, but I've sunk too much time into it
            // already.
            let IfNode { condition, if_block, else_block } = if_node;
            match else_block {
                Some(mut else_block) => match else_block.last_mut() {
                    Some(AstNode::IfStatement(_, _)) => {
                        match else_block.pop() {
                            Some(AstNode::IfStatement(token, node)) => {
                                else_block.push(AstNode::IfExpression(token, node));
                                IfNode { condition, if_block, else_block: Some(else_block) }
                            }
                            _ => IfNode { condition, if_block, else_block: Some(else_block) }
                        }
                    }
                    _ => IfNode { condition, if_block, else_block: Some(else_block) }
                }
                _ => IfNode { condition, if_block, else_block }
            }
        };

        Ok(AstNode::IfExpression(token, if_node))
    }

    fn parse_expr(&mut self) -> Result<AstNode, ParseError> {
        self.enter_context(Context::ParsingExpr);
        let result = self.parse_precedence(Precedence::None);
        self.exit_context();
        result
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
            Token::Percent(_) => BinaryOp::Mod,
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

    fn parse_invocation(&mut self, token: Token, left: AstNode) -> Result<AstNode, ParseError> {
        let lparen = token;
        let mut item_expected = true;
        let mut args = Vec::<(Option<Token>, AstNode)>::new();
        loop {
            let token = self.peek().ok_or(ParseError::UnexpectedEof)?;
            if let Token::RParen(_) = token {
                self.expect_next()?; // Consume ')' before ending loop
                break;
            } else {
                if !item_expected {
                    return Err(ParseError::UnexpectedToken(token.clone()));
                }

                match self.parse_expr()? {
                    ident @ AstNode::Identifier(_) => {
                        if let Token::Colon(_) = self.expect_peek()? {
                            self.expect_next()?; // Consume ':'
                            let arg_value = self.parse_expr()?;
                            // Kind of silly, but I can't destruct ident while also @-ing it
                            let arg_name = match ident {
                                AstNode::Identifier(arg_name) => arg_name,
                                _ => unreachable!()
                            };
                            args.push((Some(arg_name), arg_value));
                        } else {
                            args.push((None, ident));
                        }
                    }
                    expr @ _ => args.push((None, expr))
                }

                if let Token::Comma(_) = self.expect_peek()? {
                    self.expect_next()?; // Consume ','
                } else {
                    item_expected = false;
                }
            }
        }

        Ok(AstNode::Invocation(lparen, InvocationNode { target: Box::new(left), args }))
    }

    fn parse_accessor(&mut self, token: Token, left: AstNode) -> Result<AstNode, ParseError> {
        let dot = token;
        let field = self.expect_next_token(TokenType::Ident)?;
        Ok(AstNode::Accessor(dot, AccessorNode { target: Box::new(left), field }))
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

    fn parse_map_literal(&mut self, token: Token) -> Result<AstNode, ParseError> {
        let mut item_expected = true;
        let mut items = Vec::<(Token, AstNode)>::new();
        loop {
            let token = self.peek().ok_or(ParseError::UnexpectedEof)?;
            if let Token::RBrace(_) = token {
                self.expect_next()?; // Consume '}' before ending loop
                break;
            } else if !item_expected {
                return match self.peek() {
                    Some(tok) => Err(ParseError::UnexpectedToken(tok.clone())),
                    None => Err(ParseError::UnexpectedEof)
                };
            }

            let field_name = self.expect_next_token(TokenType::Ident)?;
            self.expect_next_token(TokenType::Colon)?;
            let field_value = self.parse_expr()?;
            items.push((field_name, field_value));

            let token = self.peek().ok_or(ParseError::UnexpectedEof)?;
            if let Token::Comma(_) = token {
                self.expect_next()?; // Consume comma
            } else {
                item_expected = false;
            }
        }
        Ok(AstNode::Map(token, MapNode { items }))
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
    use crate::lexer::lexer::tokenize;
    use crate::lexer::tokens::{Position, Token};
    use crate::parser::ast::AstNode::*;

    use super::*;

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
        let ast = parse("1 + 2 * 3 % 4")?;
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 3)),
                BinaryNode {
                    left: Box::new(int_literal!((1, 1), 1)),
                    op: BinaryOp::Add,
                    right: Box::new(
                        Binary(
                            Token::Percent(Position::new(1, 11)),
                            BinaryNode {
                                left: Box::new(
                                    Binary(
                                        Token::Star(Position::new(1, 7)),
                                        BinaryNode {
                                            left: Box::new(int_literal!((1, 5), 2)),
                                            op: BinaryOp::Mul,
                                            right: Box::new(int_literal!((1, 9), 3)),
                                        },
                                    )
                                ),
                                op: BinaryOp::Mod,
                                right: Box::new(int_literal!((1, 13), 4)),
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
    fn parse_map_empty() -> TestResult {
        let ast = parse("{}")?;
        let expected = AstNode::Map(Token::LBrace(Position::new(1, 1)), MapNode { items: vec![] });
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_map_with_items() -> TestResult {
        let ast = parse("{ a: 1, b: true }")?;
        let expected = AstNode::Map(
            Token::LBrace(Position::new(1, 1)),
            MapNode {
                items: vec![
                    (ident_token!((1, 3), "a"), int_literal!((1, 6), 1)),
                    (ident_token!((1, 9), "b"), bool_literal!((1, 12), true)),
                ]
            },
        );
        assert_eq!(expected, ast[0]);
        let ast = parse("{ a: 1, b: true, }")?; // Testing trailing commas
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_map_nested() -> TestResult {
        let ast = parse("{ a: { a1: 1, a2: 2 }, b: true }")?;
        let expected = AstNode::Map(
            Token::LBrace(Position::new(1, 1)),
            MapNode {
                items: vec![
                    (ident_token!((1, 3), "a"), AstNode::Map(
                        Token::LBrace(Position::new(1, 6)),
                        MapNode {
                            items: vec![
                                (ident_token!((1, 8), "a1"), int_literal!((1, 12), 1)),
                                (ident_token!((1, 15), "a2"), int_literal!((1, 19), 2)),
                            ]
                        },
                    )),
                    (ident_token!((1, 24), "b"), bool_literal!((1, 27), true)),
                ]
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_map_error() {
        let error = parse("{ 123: true }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::Int(Position::new(1, 3), 123));
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
    fn parse_type_identifier() {
        #[inline]
        fn parse_type_identifier(input: &str) -> TypeIdentifier {
            let tokens = tokenize(&input.to_string()).unwrap();
            let mut parser = Parser::new(tokens);
            parser.parse_type_identifier().unwrap()
        }

        let type_ident = parse_type_identifier("Bool");
        let expected = TypeIdentifier::Normal {
            ident: Token::Ident(Position::new(1, 1), "Bool".to_string())
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("Int[]");
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Normal {
                ident: Token::Ident(Position::new(1, 1), "Int".to_string())
            })
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("Int?");
        let expected = TypeIdentifier::Option {
            inner: Box::new(TypeIdentifier::Normal {
                ident: Token::Ident(Position::new(1, 1), "Int".to_string())
            })
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("Int[][]");
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Array {
                inner: Box::new(TypeIdentifier::Normal {
                    ident: Token::Ident(Position::new(1, 1), "Int".to_string())
                })
            })
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("Int[][]");
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Array {
                inner: Box::new(TypeIdentifier::Normal {
                    ident: Token::Ident(Position::new(1, 1), "Int".to_string())
                })
            })
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("Int?[]");
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Option {
                inner: Box::new(TypeIdentifier::Normal {
                    ident: Token::Ident(Position::new(1, 1), "Int".to_string())
                })
            })
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("Int?[]?");
        let expected = TypeIdentifier::Option {
            inner: Box::new(TypeIdentifier::Array {
                inner: Box::new(TypeIdentifier::Option {
                    inner: Box::new(TypeIdentifier::Normal {
                        ident: Token::Ident(Position::new(1, 1), "Int".to_string())
                    })
                })
            })
        };
        assert_eq!(expected, type_ident);
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
            TokenType::Ident,
            Token::Int(Position::new(1, 8), 123),
        );
        assert_eq!(expected, err);

        let err = parse("val a: Int[ = 123").unwrap_err();
        let expected = ParseError::ExpectedToken(
            TokenType::RBrack,
            Token::Assign(Position::new(1, 13)),
        );
        assert_eq!(expected, err);

        let err = parse("val a: Int[").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, err);
    }

    #[test]
    fn parse_func_decl() -> TestResult {
        let ast = parse("func abc() = 123")?;
        let expected = AstNode::FunctionDecl(
            Token::Func(Position::new(1, 1)),
            FunctionDeclNode {
                name: Token::Ident(Position::new(1, 6), "abc".to_string()),
                args: vec![],
                ret_type: None,
                body: vec![
                    int_literal!((1, 14), 123)
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("func abc() { val a = 123 a }")?;
        let expected = AstNode::FunctionDecl(
            Token::Func(Position::new(1, 1)),
            FunctionDeclNode {
                name: Token::Ident(Position::new(1, 6), "abc".to_string()),
                args: vec![],
                ret_type: None,
                body: vec![
                    AstNode::BindingDecl(
                        Token::Val(Position::new(1, 14)),
                        BindingDeclNode {
                            is_mutable: false,
                            ident: Token::Ident(Position::new(1, 18), "a".to_string()),
                            type_ann: None,
                            expr: Some(Box::new(int_literal!((1, 22), 123))),
                        },
                    ),
                    identifier!((1, 26), "a")
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("func abc(a: Int) = 123")?;
        let args = match ast.first().unwrap() {
            AstNode::FunctionDecl(_, FunctionDeclNode { args, .. }) => args,
            _ => unreachable!()
        };
        let expected = vec![
            (ident_token!((1, 10), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 13), "Int") }), None)
        ];
        assert_eq!(&expected, args);

        let ast = parse("func abc(a: Int, b: Int?) = 123")?;
        let args = match ast.first().unwrap() {
            AstNode::FunctionDecl(_, FunctionDeclNode { args, .. }) => args,
            _ => unreachable!()
        };
        let expected = vec![
            (ident_token!((1, 10), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 13), "Int") }), None),
            (ident_token!((1, 18), "b"), Some(TypeIdentifier::Option { inner: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 21), "Int") }) }), None)
        ];
        assert_eq!(&expected, args);

        // Testing trailing comma in param list
        let ast = parse("func abc(a: Int,) = 123")?;
        let args = match ast.first().unwrap() {
            AstNode::FunctionDecl(_, FunctionDeclNode { args, .. }) => args,
            _ => unreachable!()
        };
        let expected = vec![
            (ident_token!((1, 10), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 13), "Int") }), None)
        ];
        assert_eq!(&expected, args);

        // Testing default arg values
        let ast = parse("func abc(a: Int, b: Int = 2, c = 4) = 123")?;
        let args = match ast.first().unwrap() {
            AstNode::FunctionDecl(_, FunctionDeclNode { args, .. }) => args,
            _ => unreachable!()
        };
        let expected = vec![
            (ident_token!((1, 10), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 13), "Int") }), None),
            (ident_token!((1, 18), "b"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 21), "Int") }), Some(int_literal!((1, 27), 2))),
            (ident_token!((1, 30), "c"), None, Some(int_literal!((1, 34), 4))),
        ];
        assert_eq!(&expected, args);

        // Testing return type
        let ast = parse("func abc(a: Int): String = 123")?;
        let ret_type = match ast.first().unwrap() {
            AstNode::FunctionDecl(_, FunctionDeclNode { ret_type, .. }) => ret_type,
            _ => unreachable!()
        };
        let expected = Some(TypeIdentifier::Normal { ident: ident_token!((1, 19), "String") });
        assert_eq!(&expected, ret_type);

        Ok(())
    }

    #[test]
    fn parse_func_decl_error() {
        let error = parse("func (a: Int) = 123").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::LParen(Position::new(1, 6)));
        assert_eq!(expected, error);

        let error = parse("func abc) = 123").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::LParen, Token::RParen(Position::new(1, 9)));
        assert_eq!(expected, error);

        let error = parse("func abc( = 123").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Assign(Position::new(1, 11)));
        assert_eq!(expected, error);

        let error = parse("func abc(a) = 123").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Colon, Token::RParen(Position::new(1, 11)));
        assert_eq!(expected, error);

        let error = parse("func abc(a: ) = 123").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::RParen(Position::new(1, 13)));
        assert_eq!(expected, error);

        let error = parse("func abc(a: Int b: Int) = 123").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Ident(Position::new(1, 17), "b".to_string()));
        assert_eq!(expected, error);
    }

    #[test]
    fn parse_type_decl() -> TestResult {
        let ast = parse("type Person {}")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                name: ident_token!((1, 6), "Person"),
                fields: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("type Person { name: String }")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                name: ident_token!((1, 6), "Person"),
                fields: vec![(ident_token!((1, 15), "name"), TypeIdentifier::Normal { ident: ident_token!((1, 21), "String") }, None)],
            },
        );
        assert_eq!(expected, ast[0]);
        let ast = parse("type Person { name: String, }")?; // Test trailing comma
        assert_eq!(expected, ast[0]);

        let ast = parse("type Person { name: String, age: Int }")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                name: ident_token!((1, 6), "Person"),
                fields: vec![
                    (ident_token!((1, 15), "name"), TypeIdentifier::Normal { ident: ident_token!((1, 21), "String") }, None),
                    (ident_token!((1, 29), "age"), TypeIdentifier::Normal { ident: ident_token!((1, 34), "Int") }, None),
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        // Testing with default value
        let ast = parse("type Person { name: String, isHappy: Bool = true }")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                name: ident_token!((1, 6), "Person"),
                fields: vec![
                    (ident_token!((1, 15), "name"), TypeIdentifier::Normal { ident: ident_token!((1, 21), "String") }, None),
                    (ident_token!((1, 29), "isHappy"), TypeIdentifier::Normal { ident: ident_token!((1, 38), "Bool") }, Some(bool_literal!((1, 45), true)))
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_type_decl_error() {
        let error = parse("type Person }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::LBrace, Token::RBrace(Position::new(1, 13)));
        assert_eq!(expected, error);

        let error = parse("type Person {").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, error);

        let error = parse("type Person { 1234 }").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Int(Position::new(1, 15), 1234));
        assert_eq!(expected, error);

        let error = parse("type Person { name }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Colon, Token::RBrace(Position::new(1, 20)));
        assert_eq!(expected, error);

        let error = parse("type Person { name: }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::RBrace(Position::new(1, 21)));
        assert_eq!(expected, error);

        let error = parse("type Person { name: Int").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, error);

        let error = parse("type Person { name: 1234").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::Int(Position::new(1, 21), 1234));
        assert_eq!(expected, error);
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

        let ast = parse("if 3 < 4   \"hello\"")?;
        let expected = AstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            IfNode {
                condition: Box::new(
                    AstNode::Binary(
                        Token::LT(Position::new(1, 6)),
                        BinaryNode {
                            left: Box::new(int_literal!((1, 4), 3)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 8), 4)),
                        },
                    )
                ),
                if_block: vec![
                    string_literal!((1, 12), "hello")
                ],
                else_block: None,
            },
        );
        assert_eq!(expected, ast[0]);
        let ast = parse("if 3 < 4 { \"hello\" }")?;
        assert_eq!(expected, ast[0]);

        let ast = parse("if 3 < 4   \"hello\"   else   \"world\"")?;
        let expected = AstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            IfNode {
                condition: Box::new(
                    AstNode::Binary(
                        Token::LT(Position::new(1, 6)),
                        BinaryNode {
                            left: Box::new(int_literal!((1, 4), 3)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 8), 4)),
                        },
                    )
                ),
                if_block: vec![
                    string_literal!((1, 12), "hello")
                ],
                else_block: Some(vec![
                    string_literal!((1, 29), "world")
                ]),
            },
        );
        assert_eq!(expected, ast[0]);
        let ast = parse("if 3 < 4 { \"hello\" } else { \"world\" }")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if 3 < 4 { \"hello\" } else   \"world\"")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if 3 < 4   \"hello\"   else { \"world\" }")?;
        assert_eq!(expected, ast[0]);

        let ast = parse("if 3 < 4   \"hello\"   else if true   \"world\"   else   \"!\"")?;
        let expected = AstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            IfNode {
                condition: Box::new(
                    AstNode::Binary(
                        Token::LT(Position::new(1, 6)),
                        BinaryNode {
                            left: Box::new(int_literal!((1, 4), 3)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 8), 4)),
                        },
                    )
                ),
                if_block: vec![
                    string_literal!((1, 12), "hello")
                ],
                else_block: Some(vec![
                    AstNode::IfStatement(
                        Token::If(Position::new(1, 27)),
                        IfNode {
                            condition: Box::new(bool_literal!((1, 30), true)),
                            if_block: vec![
                                string_literal!((1, 37), "world")
                            ],
                            else_block: Some(vec![
                                string_literal!((1, 54), "!")
                            ]),
                        },
                    )
                ]),
            },
        );

        assert_eq!(expected, ast[0]);
        let ast = parse("if 3 < 4 { \"hello\" } else if true   \"world\"   else   \"!\"")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if 3 < 4 { \"hello\" } else if true { \"world\" } else   \"!\"")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if 3 < 4 { \"hello\" } else if true { \"world\" } else { \"!\" }")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if 3 < 4   \"hello\"   else if true { \"world\" } else { \"!\" }")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if 3 < 4   \"hello\"   else if true   \"world\"   else { \"!\" }")?;
        assert_eq!(expected, ast[0]);
        let ast = parse("if 3 < 4   \"hello\"   else if true { \"world\" } else   \"!\"")?;
        assert_eq!(expected, ast[0]);

        // Test with statement inside block
        let ast = parse("if 3 < 4 { val a = \"hello\" a }")?;
        let expected = AstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            IfNode {
                condition: Box::new(
                    AstNode::Binary(
                        Token::LT(Position::new(1, 6)),
                        BinaryNode {
                            left: Box::new(int_literal!((1, 4), 3)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 8), 4)),
                        },
                    )
                ),
                if_block: vec![
                    AstNode::BindingDecl(
                        Token::Val(Position::new(1, 12)),
                        BindingDeclNode {
                            is_mutable: false,
                            ident: Token::Ident(Position::new(1, 16), "a".to_string()),
                            type_ann: None,
                            expr: Some(Box::new(string_literal!((1, 20), "hello"))),
                        },
                    ),
                    identifier!((1, 28), "a")
                ],
                else_block: None,
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_if_expression() -> TestResult {
        let ast = parse("val str = if 3 < 4 \"hello\"")?;
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
                                    Token::LT(Position::new(1, 16)),
                                    BinaryNode {
                                        left: Box::new(int_literal!((1, 14), 3)),
                                        op: BinaryOp::Lt,
                                        right: Box::new(int_literal!((1, 18), 4)),
                                    },
                                )
                            ),
                            if_block: vec![
                                string_literal!((1, 20), "hello")
                            ],
                            else_block: None,
                        },
                    )
                )),
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("\"hello\" + if 3 < 4 \"world\" else \"!\"")?;
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
                                    Token::LT(Position::new(1, 16)),
                                    BinaryNode {
                                        left: Box::new(int_literal!((1, 14), 3)),
                                        op: BinaryOp::Lt,
                                        right: Box::new(int_literal!((1, 18), 4)),
                                    },
                                )
                            ),
                            if_block: vec![
                                string_literal!((1, 20), "world")
                            ],
                            else_block: Some(vec![
                                string_literal!((1, 33), "!")
                            ]),
                        },
                    )
                ),
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_if_expression_vs_statement() -> TestResult {
        let ast = parse("if (true) { if (true) 1 } else { 2 }")?;
        if let AstNode::IfStatement(_, node) = ast.get(0).clone().unwrap() {
            match (*node).if_block[0] {
                AstNode::IfStatement(_, _) => {}
                _ => panic!("Should be an AstNode::IfStatement, not an IfExpression")
            }
        } else {
            panic!("The first node should be an AstNode::IfStatement")
        }

        let ast = parse("val a = if (true) { if (true) 1 } else { 2 }")?;
        if let AstNode::BindingDecl(_, node) = ast.get(0).clone().unwrap() {
            let BindingDeclNode { expr, .. } = node.clone();
            let expr = expr.as_ref().unwrap().as_ref();
            match expr {
                AstNode::IfExpression(_, IfNode { if_block, .. }) => {
                    match if_block.get(0).unwrap() {
                        AstNode::IfExpression(_, _) => {}
                        _ => panic!("Should be an AstNode::IfExpression, not an IfStatement")
                    }
                }
                _ => panic!("Should be an AstNode::IfExpression, not an IfStatement")
            }
        } else {
            panic!("The first node should be an AstNode::BindingDecl")
        }

        let ast = parse("val a = if (true) { 1 } else if (true) { 2 }")?;
        if let AstNode::BindingDecl(_, node) = ast.get(0).clone().unwrap() {
            let BindingDeclNode { expr, .. } = node.clone();
            let expr = expr.as_ref().unwrap().as_ref();
            match expr {
                AstNode::IfExpression(_, IfNode { else_block, .. }) => {
                    if let Some(else_block) = else_block {
                        match else_block.get(0).unwrap() {
                            AstNode::IfExpression(_, _) => {}
                            _ => panic!("Should be an AstNode::IfExpression, not an IfStatement")
                        }
                    } else {
                        panic!("Expected else_block to be present")
                    }
                }
                _ => panic!("Should be an AstNode::IfExpression, not an IfStatement")
            }
        } else {
            panic!("The first node should be an AstNode::BindingDecl")
        }

        let ast = parse("val a = if (true) { if (true) { 1 } 2 } else { 2 }")?;
        if let AstNode::BindingDecl(_, node) = ast.get(0).clone().unwrap() {
            let BindingDeclNode { expr, .. } = node.clone();
            let expr = expr.as_ref().unwrap().as_ref();
            match expr {
                AstNode::IfExpression(_, IfNode { if_block, .. }) => {
                    match if_block.get(0).unwrap() {
                        AstNode::IfStatement(_, _) => {}
                        _ => panic!("Should be an AstNode::IfStatement, not an IfExpression")
                    }
                }
                _ => panic!("Should be an AstNode::IfExpression, not an IfStatement")
            }
        } else {
            panic!("The first node should be an AstNode::BindingDecl")
        }

        Ok(())
    }

    #[test]
    fn parse_invocation() -> TestResult {
        let ast = parse("abc()")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 4)),
            InvocationNode {
                target: Box::new(identifier!((1, 1), "abc")),
                args: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abc(4)")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 4)),
            InvocationNode {
                target: Box::new(identifier!((1, 1), "abc")),
                args: vec![
                    (None, int_literal!((1, 5), 4))
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        // Testing trailing commas
        let ast = parse("abc(4,)")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 4)),
            InvocationNode {
                target: Box::new(identifier!((1, 1), "abc")),
                args: vec![
                    (None, int_literal!((1, 5), 4))
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abc(4, def(5, 6), 7)")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 4)),
            InvocationNode {
                target: Box::new(identifier!((1, 1), "abc")),
                args: vec![
                    (None, int_literal!((1, 5), 4)),
                    (None, AstNode::Invocation(
                        Token::LParen(Position::new(1, 11)),
                        InvocationNode {
                            target: Box::new(identifier!((1, 8), "def")),
                            args: vec![
                                (None, int_literal!((1, 12), 5)),
                                (None, int_literal!((1, 15), 6)),
                            ],
                        },
                    )),
                    (None, int_literal!((1, 19), 7)),
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abc.def(4)")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 8)),
            InvocationNode {
                target: Box::new(AstNode::Accessor(
                    Token::Dot(Position::new(1, 4)),
                    AccessorNode {
                        target: Box::new(identifier!((1, 1), "abc")),
                        field: ident_token!((1, 5), "def"),
                    },
                )),
                args: vec![
                    (None, int_literal!((1, 9), 4)),
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_invocation_named_parameters() -> TestResult {
        let ast = parse("abc(a: 4)")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 4)),
            InvocationNode {
                target: Box::new(identifier!((1, 1), "abc")),
                args: vec![
                    (Some(ident_token!((1, 5), "a")), int_literal!((1, 8), 4))
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        // Testing trailing commas
        let ast = parse("abc(a: 4,)")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 4)),
            InvocationNode {
                target: Box::new(identifier!((1, 1), "abc")),
                args: vec![
                    (Some(ident_token!((1, 5), "a")), int_literal!((1, 8), 4))
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abc(a: 4, def(5, d: 6), c: 7)")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 4)),
            InvocationNode {
                target: Box::new(identifier!((1, 1), "abc")),
                args: vec![
                    (Some(ident_token!((1, 5), "a")), int_literal!((1, 8), 4)),
                    (None, AstNode::Invocation(
                        Token::LParen(Position::new(1, 14)),
                        InvocationNode {
                            target: Box::new(identifier!((1, 11), "def")),
                            args: vec![
                                (None, int_literal!((1, 15), 5)),
                                (Some(ident_token!((1, 18), "d")), int_literal!((1, 21), 6)),
                            ],
                        },
                    )),
                    (Some(ident_token!((1, 25), "c")), int_literal!((1, 28), 7)),
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_invocation_errors() {
        let error = parse("abc(").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, error);

        let error = parse("abc!()").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::RParen(Position::new(1, 6)));
        assert_eq!(expected, error);

        let error = parse("abc(1 + )").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::RParen(Position::new(1, 9)));
        assert_eq!(expected, error);

        let error = parse("abc(1, 1 1)").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Int(Position::new(1, 10), 1));
        assert_eq!(expected, error);

        let error = parse("abc(a + b: 2)").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Colon(Position::new(1, 10)));
        assert_eq!(expected, error);
    }

    #[test]
    fn parse_while_loop() -> TestResult {
        let ast = parse("while true 1 + 1")?;
        let expected = AstNode::WhileLoop(
            Token::While(Position::new(1, 1)),
            WhileLoopNode {
                condition: Box::new(bool_literal!((1, 7), true)),
                body: vec![
                    AstNode::Binary(
                        Token::Plus(Position::new(1, 14)),
                        BinaryNode {
                            left: Box::new(int_literal!((1, 12), 1)),
                            op: BinaryOp::Add,
                            right: Box::new(int_literal!((1, 16), 1)),
                        },
                    )
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("while true { break }")?;
        let expected = AstNode::WhileLoop(
            Token::While(Position::new(1, 1)),
            WhileLoopNode {
                condition: Box::new(bool_literal!((1, 7), true)),
                body: vec![
                    AstNode::Break(Token::Break(Position::new(1, 14)))
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("while a < 3 {\nval a = 1\na + 1 }")?;
        let expected = AstNode::WhileLoop(
            Token::While(Position::new(1, 1)),
            WhileLoopNode {
                condition: Box::new(
                    AstNode::Binary(
                        Token::LT(Position::new(1, 9)),
                        BinaryNode {
                            left: Box::new(AstNode::Identifier(ident_token!((1, 7), "a"))),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 11), 3)),
                        },
                    )
                ),
                body: vec![
                    AstNode::BindingDecl(
                        Token::Val(Position::new(2, 1)),
                        BindingDeclNode {
                            is_mutable: false,
                            type_ann: None,
                            ident: ident_token!((2, 5), "a"),
                            expr: Some(Box::new(int_literal!((2, 9), 1))),
                        },
                    ),
                    AstNode::Binary(
                        Token::Plus(Position::new(3, 3)),
                        BinaryNode {
                            left: Box::new(AstNode::Identifier(ident_token!((3, 1), "a"))),
                            op: BinaryOp::Add,
                            right: Box::new(int_literal!((3, 5), 1)),
                        },
                    )
                ],
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_for_loop() -> TestResult {
        let ast = parse("for a in [0, 1] { a }")?;
        let expected = AstNode::ForLoop(
            Token::For(Position::new(1, 1)),
            ForLoopNode {
                iteratee: ident_token!((1, 5), "a"),
                index_ident: None,
                iterator: Box::new(AstNode::Array(
                    Token::LBrack(Position::new(1, 10)),
                    ArrayNode {
                        items: vec![
                            Box::new(int_literal!((1, 11), 0)),
                            Box::new(int_literal!((1, 14), 1))
                        ]
                    },
                )),
                body: vec![identifier!((1, 19), "a")],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("for a, i in [0, 1] { a }")?;
        let expected = AstNode::ForLoop(
            Token::For(Position::new(1, 1)),
            ForLoopNode {
                iteratee: ident_token!((1, 5), "a"),
                index_ident: Some(ident_token!((1, 8), "i")),
                iterator: Box::new(AstNode::Array(
                    Token::LBrack(Position::new(1, 13)),
                    ArrayNode {
                        items: vec![
                            Box::new(int_literal!((1, 14), 0)),
                            Box::new(int_literal!((1, 17), 1))
                        ]
                    },
                )),
                body: vec![identifier!((1, 22), "a")],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_for_loop_error() {
        let error = parse("for 123 in [0, 1] { a }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::Int(Position::new(1, 5), 123));
        assert_eq!(expected, error);

        let error = parse("for a [0, 1] { a }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::In, Token::LBrack(Position::new(1, 7)));
        assert_eq!(expected, error);

        let error = parse("for a in { a }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Colon, Token::RBrace(Position::new(1, 14)));
        assert_eq!(expected, error);

        let error = parse("for a, in [0, 1] { a }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::In(Position::new(1, 8)));
        assert_eq!(expected, error);
    }

    #[test]
    fn parse_dot_accessor() -> TestResult {
        let ast = parse("abc.def")?;
        let expected = AstNode::Accessor(
            Token::Dot(Position::new(1, 4)),
            AccessorNode {
                target: Box::new(identifier!((1, 1), "abc")),
                field: ident_token!((1, 5), "def"),
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abc.def.ghi.jkl")?;
        let expected = AstNode::Accessor(
            Token::Dot(Position::new(1, 12)),
            AccessorNode {
                target: Box::new(AstNode::Accessor(
                    Token::Dot(Position::new(1, 8)),
                    AccessorNode {
                        target: Box::new(AstNode::Accessor(
                            Token::Dot(Position::new(1, 4)),
                            AccessorNode {
                                target: Box::new(identifier!((1, 1), "abc")),
                                field: ident_token!((1, 5), "def"),
                            },
                        )),
                        field: ident_token!((1, 9), "ghi"),
                    },
                )),
                field: ident_token!((1, 13), "jkl"),
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }
}
