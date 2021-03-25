use peekmore::{PeekMore, PeekMoreIterator};
use std::vec::IntoIter;
use crate::lexer::tokens::{Token, TokenType};
use crate::parser::ast::{ArrayNode, AssignmentNode, AstLiteralNode, AstNode, BinaryNode, BinaryOp, BindingDeclNode, ForLoopNode, FunctionDeclNode, GroupedNode, IfNode, IndexingMode, IndexingNode, InvocationNode, TypeIdentifier, UnaryNode, UnaryOp, WhileLoopNode, TypeDeclNode, MapNode, AccessorNode, LambdaNode, EnumDeclNode, MatchNode, MatchCase, MatchCaseType, SetNode, BindingPattern, TypeDeclField, ImportNode, ModuleId};
use crate::parser::parse_error::ParseError;
use crate::parser::precedence::Precedence;

pub struct ParseResult {
    pub(crate) imports: Vec<(Token, ModuleId)>,
    pub(crate) nodes: Vec<AstNode>,
}

pub fn parse(tokens: Vec<Token>) -> Result<ParseResult, ParseError> {
    let mut parser = Parser::new(tokens);

    let mut nodes = Vec::new();
    let mut imports = Vec::new();
    let mut imports_done = false;
    loop {
        match parser.peek() {
            Some(tok) => {
                let node = if let Token::Import(_) = tok {
                    let node = parser.parse_import_statement(!imports_done)?;
                    if let AstNode::ImportStatement(import_tok, import_node) = &node {
                        let module_id = import_node.get_module_id();
                        imports.push((import_tok.clone(), module_id))
                    }
                    node
                } else {
                    imports_done = true;
                    parser.parse_stmt(None)?
                };

                nodes.push(node);
            }
            None => break
        }
    }

    Ok(ParseResult { imports, nodes })
}

#[derive(PartialEq)]
enum Context {
    ParsingExpr,
}

pub struct Parser {
    tokens: PeekMoreIterator<IntoIter<Token>>,
    context: Vec<Context>,
}

type PrefixFn = dyn Fn(&mut Parser, Token) -> Result<AstNode, ParseError>;
type InfixFn = dyn Fn(&mut Parser, Token, AstNode) -> Result<AstNode, ParseError>;

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        let tokens = tokens.into_iter().peekmore();
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

    fn expect_next(&mut self) -> Result<Token, ParseError> {
        self.tokens.next().ok_or(ParseError::UnexpectedEof)
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

    fn expect_peek_token(&mut self, expected_token: TokenType) -> Result<Token, ParseError> {
        self.expect_peek().and_then(|tok| {
            let tok = tok.clone();
            let token_type: TokenType = tok.clone().into();
            if token_type != expected_token {
                Err(ParseError::ExpectedToken(expected_token, tok))
            } else {
                Ok(tok)
            }
        })
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
            Token::StringInterp(_, _) |
            Token::Bool(_, _) => Some(Box::new(Parser::parse_literal)),
            Token::Minus(_) | Token::Bang(_) => Some(Box::new(Parser::parse_unary)),
            Token::LParen(_, _) => Some(Box::new(Parser::parse_grouped)),
            Token::LBrack(_, _) => Some(Box::new(Parser::parse_array)),
            Token::LBrace(_) => Some(Box::new(Parser::parse_map_literal)),
            Token::LBraceHash(_) => Some(Box::new(Parser::parse_set_literal)),
            Token::Ident(_, _) |
            Token::Self_(_) |
            Token::None(_) => Some(Box::new(Parser::parse_ident)),
            Token::If(_) => Some(Box::new(Parser::parse_if_expr)),
            Token::Match(_) => Some(Box::new(Parser::parse_match_expr)),
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
            Token::LBrack(_, _) => Some(Box::new(Parser::parse_index)),
            Token::Assign(_) => Some(Box::new(Parser::parse_assignment)),
            Token::LParen(_, _) => Some(Box::new(Parser::parse_invocation)),
            Token::Dot(_) | Token::QuestionDot(_) => Some(Box::new(Parser::parse_accessor)),
            Token::Arrow(_) => Some(Box::new(Parser::parse_lambda)),
            _ => Some(Box::new(Parser::parse_binary)),
        }
    }

    fn get_precedence_for_token(tok: &Token) -> Precedence {
        match tok {
            Token::Plus(_) | Token::PlusEq(_) | Token::Minus(_) | Token::MinusEq(_) => Precedence::Addition,
            Token::Star(_) | Token::StarEq(_) | Token::Slash(_) | Token::SlashEq(_) | Token::Percent(_) | Token::PercentEq(_) => Precedence::Multiplication,
            Token::And(_) | Token::AndEq(_) => Precedence::And,
            Token::Or(_) | Token::OrEq(_) | Token::Caret(_) => Precedence::Or,
            Token::Elvis(_) | Token::ElvisEq(_) | Token::StarStar(_) => Precedence::Coalesce,
            Token::Eq(_) | Token::Neq(_) => Precedence::Equality,
            Token::GT(_) | Token::GTE(_) | Token::LT(_) | Token::LTE(_) => Precedence::Comparison,
            Token::Assign(_) => Precedence::Assignment,
            Token::Dot(_) | Token::QuestionDot(_) | Token::Arrow(_) => Precedence::Call,
            Token::LParen(_, is_preceded_by_newline) => {
                if *is_preceded_by_newline { Precedence::None } else { Precedence::Call }
            }
            Token::LBrack(_, is_preceded_by_newline) => {
                if *is_preceded_by_newline { Precedence::None } else { Precedence::Call }
            }
            _ => Precedence::None,
        }
    }

    // End Pratt plumbing

    fn parse_stmt(&mut self, export_token: Option<Token>) -> Result<AstNode, ParseError> {
        match self.expect_peek()? {
            Token::Func(_) => self.parse_func_decl(export_token),
            Token::Val(_) => self.parse_binding_decl(export_token),
            Token::Var(_) => self.parse_binding_decl(export_token),
            Token::Type(_) | Token::Enum(_) => self.parse_type_decl(export_token),
            Token::If(_) => self.parse_if_statement(),
            Token::Match(_) => self.parse_match_statement(),
            Token::While(_) => self.parse_while_statement(),
            Token::For(_) => self.parse_for_statement(),
            Token::Break(_) => self.parse_break_statement(),
            Token::Return(_, _) => self.parse_return_statement(),
            Token::Import(_) => self.parse_import_statement(false),
            Token::Export(_) => self.parse_exported_statement(),
            _ => self.parse_expr(),
        }
    }

    fn parse_type_identifier(&mut self, consume: bool) -> Result<TypeIdentifier, ParseError> {
        // Rather than delegate to functions, just use some handy macros to either consume the token
        // or just pass over it.
        macro_rules! expect_next {
            () => {
              if consume {
                  self.expect_next()?;
              } else {
                  self.tokens.advance_cursor();
              }
            };
        }
        macro_rules! expect_next_token {
            ($token_type:expr) => {
              if consume {
                  self.expect_next_token($token_type)?
              } else {
                  let tok = self.expect_peek_token($token_type)?;
                  self.tokens.advance_cursor();
                  tok
              }
            };
        }

        let mut left = if let Token::LParen(_, _) = self.expect_peek()? {
            expect_next!(); // Consume '('

            let mut types = Vec::new();
            loop {
                if let Token::RParen(_) = self.expect_peek()? {
                    expect_next!(); // Consume ')'
                    break;
                }
                types.push(self.parse_type_identifier(consume)?);
                if let Token::Comma(_) = self.expect_peek()? {
                    expect_next!(); // Consume ','
                }
            }

            if types.len() == 1 {
                if let Some(Token::Arrow(_)) = self.peek() {
                    expect_next!(); // Consume '=>'
                    let ret_type = self.parse_type_identifier(consume)?;
                    TypeIdentifier::Func { args: types, ret: Box::new(ret_type) }
                } else {
                    types.into_iter().next().unwrap()
                }
            } else if let Some(Token::Arrow(_)) = self.peek() {
                expect_next_token!(TokenType::Arrow);

                let ret_type = self.parse_type_identifier(consume)?;
                TypeIdentifier::Func { args: types, ret: Box::new(ret_type) }
            } else {
                TypeIdentifier::Tuple { types }
            }
        } else {
            let ident = match expect_next_token!(TokenType::Ident) {
                ident @ Token::Ident(_, _) => ident,
                _ => unreachable!() // Since expect_next_token verifies it's a TokenType::Ident
            };
            let type_args = if let Some(Token::LT(_)) = self.peek() {
                expect_next_token!(TokenType::LT);
                let mut type_args = Vec::new();
                loop {
                    let type_arg = self.parse_type_identifier(consume)?;
                    type_args.push(type_arg);
                    if let Token::Comma(_) = self.expect_peek()? {
                        expect_next!(); // Consume ','
                    } else {
                        expect_next_token!(TokenType::GT);
                        break;
                    }
                }
                Some(type_args)
            } else { None };
            TypeIdentifier::Normal { ident, type_args }
        };

        let mut next_token = self.peek();
        loop {
            match next_token {
                Some(Token::LBrack(_, _)) => {
                    expect_next!(); // Consume '['
                    match self.expect_peek()? {
                        Token::RBrack(_) => Ok(expect_next!()), // Consume ']'
                        t => Err(ParseError::ExpectedToken(TokenType::RBrack, t.clone())),
                    }?;
                    left = TypeIdentifier::Array { inner: Box::new(left) }
                }
                Some(Token::Question(_)) => {
                    expect_next!(); // Consume '?'
                    left = TypeIdentifier::Option { inner: Box::new(left) }
                }
                _ => break
            }
            next_token = self.peek();
        };

        if let Some(Token::Pipe(_)) = self.peek() {
            expect_next!(); // Consume '|'

            let right = self.parse_type_identifier(consume)?;
            left = TypeIdentifier::Union {
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_binding_pattern(&mut self) -> Result<BindingPattern, ParseError> {
        let binding = match self.expect_next()? {
            tok @ Token::Ident(_, _) => BindingPattern::Variable(tok),
            tok @ Token::LParen(_, _) => {
                let mut patterns = Vec::new();
                let mut done = false;
                loop {
                    if done {
                        let err_tok = self.expect_next()?;
                        return Err(ParseError::ExpectedToken(TokenType::RParen, err_tok));
                    }

                    let pat = self.parse_binding_pattern()?;
                    patterns.push(pat);

                    if let Token::Comma(_) = self.expect_peek()? {
                        self.expect_next()?; // Consume ','
                    } else if let Token::RParen(_) = self.expect_peek()? {
                        self.expect_next()?; // Consume ')'
                        break;
                    } else {
                        done = true;
                    }
                }
                BindingPattern::Tuple(tok, patterns)
            }
            tok @ Token::LBrack(_, _) => {
                let mut patterns = Vec::new();
                let mut done = false;
                loop {
                    if done {
                        let err_tok = self.expect_next()?;
                        return Err(ParseError::ExpectedToken(TokenType::RBrack, err_tok));
                    }

                    if let Token::Star(_) = self.expect_peek()? {
                        self.expect_next()?; // Consume '*'
                        let ident = self.expect_next_token(TokenType::Ident)?;
                        let pat = BindingPattern::Variable(ident);
                        patterns.push((pat, true));
                    } else {
                        let pat = self.parse_binding_pattern()?;
                        patterns.push((pat, false));
                    }

                    if let Token::Comma(_) = self.expect_peek()? {
                        self.expect_next()?; // Consume ','
                    } else if let Token::RBrack(_) = self.expect_peek()? {
                        self.expect_next()?; // Consume ']'
                        break;
                    } else {
                        done = true;
                    }
                }
                BindingPattern::Array(tok, patterns, false)
            }
            tok @ _ => return Err(ParseError::UnexpectedToken(tok))
        };

        Ok(binding)
    }

    fn parse_func_args(
        &mut self,
        allow_bare_arg: bool,
    ) -> Result<Vec<(Token, Option<TypeIdentifier>, bool, Option<AstNode>)>, ParseError> {
        let mut args = Vec::new();
        loop {
            let token = self.expect_peek()?;

            match token {
                Token::RParen(_) => {
                    self.expect_next()?; // Consume ')' before ending loop
                    break;
                }
                Token::Self_(_) => {
                    let self_tok = self.expect_next()?;
                    args.push((self_tok, None, false, None));

                    match self.peek().ok_or(ParseError::UnexpectedEof)? {
                        Token::Comma(_) => self.expect_next(),
                        Token::RParen(_) => continue,
                        tok @ _ => return Err(ParseError::UnexpectedToken(tok.clone())),
                    }?;
                }
                tok @ Token::Star(_) |
                tok @ Token::Ident(_, _) => {
                    let is_varargs = if let Token::Star(_) = tok {
                        self.expect_next()?; // Consume '*'
                        true
                    } else { false };
                    let arg_ident = self.expect_next_token(TokenType::Ident)?;

                    let type_ident = if let Token::Colon(_) = self.peek().ok_or(ParseError::UnexpectedEof)? {
                        self.expect_next()?; // Consume ':'
                        Some(self.parse_type_identifier(true)?)
                    } else { None };

                    let default_value = match self.peek().ok_or(ParseError::UnexpectedEof)? {
                        Token::Assign(_) => {
                            self.expect_next()?; // Consume '='
                            Some(self.parse_expr()?)
                        }
                        next_tok @ _ => {
                            if type_ident.is_none() && !allow_bare_arg {
                                return Err(ParseError::ExpectedToken(TokenType::Colon, next_tok.clone()));
                            }
                            None
                        }
                    };

                    args.push((arg_ident, type_ident, is_varargs, default_value));

                    match self.peek().ok_or(ParseError::UnexpectedEof)? {
                        Token::Comma(_) => self.expect_next(),
                        Token::RParen(_) => continue,
                        tok @ _ => return Err(ParseError::UnexpectedToken(tok.clone())),
                    }?;
                }
                tok @ _ => return Err(ParseError::UnexpectedToken(tok.clone())),
            }
        };

        Ok(args)
    }

    fn parse_exported_statement(&mut self) -> Result<AstNode, ParseError> {
        let export_token = self.expect_next()?; // Consume 'export'
        match self.expect_peek()? {
            Token::Func(_) | Token::Val(_) | Token::Var(_) | Token::Type(_) | Token::Enum(_) => {
                self.parse_stmt(Some(export_token))
            }
            tok => Err(ParseError::UnexpectedToken(tok.clone()))
        }
    }

    fn parse_type_args(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut type_args = Vec::new();
        if let Token::LT(_) = self.expect_peek()? {
            self.expect_next()?; // Consume '<'

            loop {
                let ident = self.expect_next_token(TokenType::Ident)?;
                type_args.push(ident);

                match self.expect_next()? {
                    Token::Comma(_) => {
                        if let Token::GT(_) = self.expect_peek()? {
                            self.expect_next()?; // Consume '>'
                            break;
                        } else {
                            continue;
                        }
                    }
                    Token::GT(_) => break,
                    tok @ _ => return Err(ParseError::UnexpectedToken(tok))
                }
            }
        }
        Ok(type_args)
    }

    fn parse_func_decl(&mut self, export_token: Option<Token>) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;
        let name = self.expect_next_token(TokenType::Ident)?;

        let type_args = self.parse_type_args()?;

        self.expect_next_token(TokenType::LParen)?;
        let args = self.parse_func_args(false)?;

        let ret_type = match self.peek().ok_or(ParseError::UnexpectedEof)? {
            Token::Colon(_) => {
                self.expect_next()?;
                Some(self.parse_type_identifier(true)?)
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

        Ok(AstNode::FunctionDecl(token, FunctionDeclNode { export_token, name, type_args, args, ret_type, body }))
    }

    fn parse_binding_decl(&mut self, export_token: Option<Token>) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;
        let is_mutable = match &token {
            Token::Val(_) => false,
            Token::Var(_) => true,
            _ => unreachable!()
        };

        let binding = self.parse_binding_pattern()?;

        let type_ann = match self.peek() {
            Some(Token::Colon(_)) => {
                self.expect_next()?; // Consume ':'
                let type_ident = self.parse_type_identifier(true)?;
                Ok(Some(type_ident))
            }
            Some(_) | None => Ok(None)
        }?;

        // For destructuring assignments, assignment is required
        let is_assignment_required = if let BindingPattern::Variable(_) = &binding { false } else { true };

        let should_parse_expr = if is_assignment_required {
            self.expect_next_token(TokenType::Assign)?;
            true
        } else if let Some(Token::Assign(_)) = self.peek() {
            self.expect_next()?; // Consume '='
            true
        } else {
            false
        };

        let expr = if should_parse_expr {
            let expr = self.parse_expr()?;
            Some(Box::new(expr))
        } else { None };

        Ok(AstNode::BindingDecl(token, BindingDeclNode { export_token, binding, is_mutable, type_ann, expr }))
    }

    fn parse_type_decl(&mut self, export_token: Option<Token>) -> Result<AstNode, ParseError> {
        let keyword_tok = self.expect_next()?;
        let is_enum = if let Token::Enum(_) = &keyword_tok { true } else { false };

        let name = self.expect_next().and_then(|tok| {
            match tok {
                Token::Ident(_, _) => Ok(tok),
                _ => Err(ParseError::UnexpectedToken(tok))
            }
        })?;

        let type_args = self.parse_type_args()?;

        self.expect_next_token(TokenType::LBrace)?;

        let mut fields = Vec::new();
        let mut variants = Vec::new();
        let mut methods = Vec::new();
        loop {
            let token = self.peek().ok_or(ParseError::UnexpectedEof)?;
            match token {
                Token::RBrace(_) => break,
                Token::Comma(_) => {
                    self.expect_next()?; // Consume ','
                }
                Token::Ident(_, _) => {
                    let ident = self.expect_next()?;

                    if is_enum {
                        let args = if let Token::LParen(_, _) = self.expect_peek()? {
                            self.expect_next_token(TokenType::LParen)?;
                            let args = self.parse_func_args(false)?;
                            Some(args)
                        } else { None };
                        variants.push((ident, args));
                    } else {
                        self.expect_next_token(TokenType::Colon)?;
                        let type_ident = self.parse_type_identifier(true)?;
                        let default_value = if let Some(Token::Assign(_)) = self.peek() {
                            self.expect_next()?; // Consume '='
                            Some(self.parse_expr()?)
                        } else { None };

                        let readonly = if let Token::Readonly(_) = self.expect_peek()? {
                            Some(self.expect_next()?)
                        } else { None };

                        let field = TypeDeclField { ident, type_ident, default_value, readonly };
                        fields.push(field);
                    }
                }
                Token::Func(_) => {
                    let method = self.parse_func_decl(None)?;
                    methods.push(method);
                }
                _ => return Err(ParseError::UnexpectedToken(token.clone()))
            }
        }

        self.expect_next_token(TokenType::RBrace)?;

        if is_enum {
            Ok(AstNode::EnumDecl(keyword_tok, EnumDeclNode { export_token, name, variants, methods, type_args }))
        } else {
            Ok(AstNode::TypeDecl(keyword_tok, TypeDeclNode { export_token, name, fields, methods, type_args }))
        }
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
                        Some(_) => body.push(self.parse_stmt(None)?),
                        None => break Err(ParseError::UnexpectedEof)
                    }
                }
            }
            _ => Ok(vec![self.parse_expr()?])
        }
    }

    fn parse_for_statement(&mut self) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;

        let binding = self.parse_binding_pattern()?;

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

        Ok(AstNode::ForLoop(token, ForLoopNode { binding, index_ident, iterator, body }))
    }

    fn parse_while_statement(&mut self) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;

        let condition = Box::new(self.parse_expr()?);
        let condition_binding = if let Some(Token::Pipe(_)) = self.peek() {
            self.expect_next()?; // Consume '|'
            let ident = self.expect_next_token(TokenType::Ident)?; // Expect binding ident
            self.expect_next_token(TokenType::Pipe)?; // Expect closing '|'

            Some(ident)
        } else { None };

        let body = self.parse_expr_or_block()?;
        Ok(AstNode::WhileLoop(token, WhileLoopNode { condition, condition_binding, body }))
    }

    fn parse_break_statement(&mut self) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;
        Ok(AstNode::Break(token))
    }

    fn parse_return_statement(&mut self) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;
        let has_newline = if let Token::Return(_, has_newline) = token { has_newline } else { unreachable!() };
        if has_newline {
            return Ok(AstNode::ReturnStatement(token, None));
        }

        let next_tok = self.expect_peek()?.clone();

        let token_begins_statement = match &next_tok {
            Token::Func(_) | Token::Val(_) | Token::Var(_) | Token::Type(_) | Token::Enum(_) |
            Token::While(_) | Token::For(_) | Token::Break(_) | Token::Return(_, _) => true,
            _ => false
        };
        if token_begins_statement {
            return Err(ParseError::UnexpectedToken(next_tok));
        }

        let has_expr = self.get_prefix_rule(&next_tok).is_some();
        let expr = if has_expr {
            Some(Box::new(self.parse_expr()?))
        } else { None };

        Ok(AstNode::ReturnStatement(token, expr))
    }

    fn parse_import_statement(&mut self, is_proper: bool) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?; // Consume 'import' token
        if !is_proper {
            return Err(ParseError::UnexpectedToken(token));
        }

        let mut star_token = None;
        let mut imports = Vec::new();
        if let Token::Star(_) = self.expect_peek()? {
            let token = self.expect_next()?; // Consume '*'
            star_token = Some(token);
        } else {
            loop {
                let ident = self.expect_next_token(TokenType::Ident)?;
                imports.push(ident);

                if let Token::Comma(_) = self.expect_peek()? {
                    self.expect_next()?; // Consume ','
                } else {
                    break;
                }
            }
        }

        self.expect_next_token(TokenType::From)?;

        let mut path = Vec::new();
        let dot_token = if let Token::Dot(_) = self.expect_peek()? {
            let token = self.expect_next()?; // Consume '.'
            Some(token)
        } else { None };
        loop {
            let ident = self.expect_next_token(TokenType::Ident)?;
            path.push(ident);

            if let Some(Token::Dot(_)) = self.peek() {
                self.expect_next()?; // Consume '.'
            } else {
                break;
            }
        }

        Ok(AstNode::ImportStatement(token, ImportNode { imports, star_token, leading_dot_token: dot_token, path }))
    }

    fn parse_if_node(&mut self) -> Result<IfNode, ParseError> {
        let condition = Box::new(self.parse_expr()?);

        let condition_binding = if let Some(Token::Pipe(_)) = self.peek() {
            self.expect_next()?; // Consume '|'
            let pattern = self.parse_binding_pattern()?;
            self.expect_next_token(TokenType::Pipe)?; // Expect closing '|'

            Some(pattern)
        } else { None };

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
        Ok(IfNode { condition, condition_binding, if_block, else_block })
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
            let IfNode { condition, condition_binding, if_block, else_block } = if_node;
            match else_block {
                Some(mut else_block) => match else_block.last_mut() {
                    Some(AstNode::IfStatement(_, _)) => {
                        match else_block.pop() {
                            Some(AstNode::IfStatement(token, node)) => {
                                else_block.push(AstNode::IfExpression(token, node));
                                IfNode { condition, condition_binding, if_block, else_block: Some(else_block) }
                            }
                            _ => IfNode { condition, condition_binding, if_block, else_block: Some(else_block) }
                        }
                    }
                    _ => IfNode { condition, condition_binding, if_block, else_block: Some(else_block) }
                }
                _ => IfNode { condition, condition_binding, if_block, else_block }
            }
        };

        Ok(AstNode::IfExpression(token, if_node))
    }

    fn parse_match_node(&mut self) -> Result<MatchNode, ParseError> {
        let target = Box::new(self.parse_expr()?);

        self.expect_next_token(TokenType::LBrace)?;

        let mut branches = Vec::new();
        loop {
            match self.peek() {
                Some(tok @ Token::RBrace(_)) => {
                    if branches.is_empty() {
                        // A match node must have at least 1 case
                        return Err(ParseError::UnexpectedToken(tok.clone()));
                    }

                    self.expect_next()?; // Consume '}'
                    break;
                }
                Some(_) => {
                    let case = self.parse_match_case()?;
                    self.expect_next_token(TokenType::Arrow)?;
                    let body = self.parse_expr_or_block()?;
                    branches.push((case, body));

                    if let Some(Token::Comma(_)) = self.peek() {
                        self.expect_next()?;
                    }
                }
                None => return Err(ParseError::UnexpectedEof)
            }
        }

        Ok(MatchNode { target, branches })
    }

    fn parse_match_case(&mut self) -> Result<MatchCase, ParseError> {
        let expect_next_ident_or_none = |p: &mut Parser| {
            match p.expect_next()? {
                t @ Token::Ident(_, _) | t @ Token::None(_) => Ok(t),
                t @ _ => Err(ParseError::ExpectedToken(TokenType::Ident, t))
            }
        };

        let ident = expect_next_ident_or_none(self)?;
        let mut case_token = ident.clone();
        if Token::get_ident_name(&ident) == "_" {
            let case_binding = if let Some(Token::Ident(_, _)) = self.peek() {
                Some(self.expect_next()?)
            } else { None };
            return Ok(MatchCase { token: ident.clone(), match_type: MatchCaseType::Wildcard(ident), case_binding, args: None });
        }

        let mut idents = vec![ident];
        while let Some(Token::Dot(_)) = self.peek() {
            self.expect_next()?; // Consume '.'
            let ident = expect_next_ident_or_none(self)?;
            idents.push(ident);
        }

        let args = if let Some(Token::LParen(_, _)) = self.peek() {
            case_token = self.expect_next()?; // Consume '('
            let mut args = Vec::new();
            let mut item_expected = true;
            loop {
                match self.expect_peek()? {
                    Token::RParen(_) => {
                        let tok = self.expect_next()?; // Consume ')'
                        if args.is_empty() {
                            return Err(ParseError::ExpectedToken(TokenType::Ident, tok));
                        }
                        break;
                    }
                    _ => {
                        if !item_expected {
                            let tok = self.expect_next()?;
                            return Err(ParseError::ExpectedToken(TokenType::RParen, tok));
                        }

                        let pat = self.parse_binding_pattern()?;
                        args.push(pat);
                        if let Some(Token::Comma(_)) = self.peek() {
                            self.expect_next()?; // Consume ','
                        } else {
                            item_expected = false;
                        }
                    }
                }
            }
            Some(args)
        } else { None };

        let case_binding = if let Some(Token::Ident(_, _)) = self.peek() {
            Some(self.expect_next()?)
        } else { None };

        let match_type = if idents.len() == 1 {
            MatchCaseType::Ident(idents.into_iter().next().unwrap())
        } else {
            MatchCaseType::Compound(idents.drain(..).collect())
        };
        Ok(MatchCase { token: case_token, match_type, case_binding, args })
    }

    fn parse_match_statement(&mut self) -> Result<AstNode, ParseError> {
        let token = self.expect_next()?;
        let match_node = self.parse_match_node()?;
        Ok(AstNode::MatchStatement(token, match_node))
    }

    fn parse_match_expr(&mut self, token: Token) -> Result<AstNode, ParseError> {
        let match_node = self.parse_match_node()?;
        Ok(AstNode::MatchExpression(token, match_node))
    }

    fn parse_expr(&mut self) -> Result<AstNode, ParseError> {
        self.enter_context(Context::ParsingExpr);
        let result = self.parse_precedence(Precedence::None);
        self.exit_context();
        result
    }

    fn parse_literal(&mut self, token: Token) -> Result<AstNode, ParseError> {
        match token.clone() {
            Token::Int(_, val) => Ok(AstNode::Literal(token, AstLiteralNode::IntLiteral(val))),
            Token::Float(_, val) => Ok(AstNode::Literal(token, AstLiteralNode::FloatLiteral(val))),
            Token::String(_, val) => Ok(AstNode::Literal(token, AstLiteralNode::StringLiteral(val.clone()))),
            Token::StringInterp(_, chunks) => Self::parse_string_interpolation(chunks),
            Token::Bool(_, val) => Ok(AstNode::Literal(token, AstLiteralNode::BoolLiteral(val))),
            _ => unreachable!()
        }
    }

    fn parse_string_interpolation(chunks: Vec<Token>) -> Result<AstNode, ParseError> {
        let mut chunks = chunks.into_iter();

        let first_chunk = chunks.next().expect("There should always be at least 1 chunk");
        let base_node = if let Token::String(_, val) = first_chunk.clone() {
            AstNode::Literal(first_chunk.clone(), AstLiteralNode::StringLiteral(val))
        } else { unreachable!() };

        let ParseResult { nodes: args, .. } = parse(chunks.collect())?;

        Ok(AstNode::Invocation(
            Token::LParen(first_chunk.get_position(), false),
            InvocationNode {
                target: Box::new(AstNode::Accessor(
                    Token::Dot(first_chunk.get_position()),
                    AccessorNode {
                        target: Box::new(base_node),
                        field: Box::new(AstNode::Identifier(
                            Token::Ident(first_chunk.get_position(), "concat".to_string()),
                            None,
                        )),
                        is_opt_safe: false,
                    },
                )),
                args: args.into_iter()
                    .map(|arg| (None, arg))
                    .collect(),
            },
        ))
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
        // If the next token is ')', followed by '=>', we're parsing a no-args lambda
        if let Token::RParen(_) = self.expect_peek()? {
            self.tokens.advance_cursor();
            if let Some(Token::Arrow(_)) = self.peek() {
                self.expect_next()?; // Consume ')'
                let token = self.expect_next()?; // Consume '=>'
                let body = self.parse_expr_or_block()?;
                return Ok(AstNode::Lambda(token, LambdaNode { args: vec![], body }));
            } else {
                self.tokens.reset_cursor();
            }
        }

        // If the next token is an ident, we need to determine if we're parsing a tuple or a lambda.
        let peek_tok = self.expect_peek()?;
        let should_attempt_lambda_parse = match peek_tok {
            Token::Ident(_, _) => true,
            Token::Star(_) => {
                self.tokens.advance_cursor();
                if let Token::Ident(_, _) = self.expect_peek()? {
                    true
                } else {
                    self.tokens.reset_cursor();
                    false
                }
            }
            _ => false
        };
        if should_attempt_lambda_parse {
            self.tokens.advance_cursor();

            // We do this by advancing the token cursor as long as we see [<ident>,]*. If at any point,
            // we see something other than an ident after a comma, we bail immediately.
            let mut only_saw_args_and_commas = true;
            while let Token::Comma(_) = self.expect_peek()? {
                self.tokens.advance_cursor(); // Skip over ','
                if let Token::Star(_) = self.expect_peek()? {
                    self.tokens.advance_cursor(); // Skip over * (denoting a varargs param)
                }
                if let Token::Ident(_, _) = self.expect_peek()? {
                    self.tokens.advance_cursor(); // Skip over ident
                } else {
                    only_saw_args_and_commas = false;
                    break;
                }
            }

            // If up until now, we only saw idents and commas, continue onward. Now, we look for clues
            // that we're parsing a lambda. A `:` indicates a type identifier, and an `=` indicates a
            // default value for a parameter; if we see these, we know right away that we're parsing a
            // lambda. If we see a `)` _followed by a `=>`_, then we also know we're parsing a lambda.
            // Otherwise, reset the cursor to the beginning and parse a tuple.
            if only_saw_args_and_commas {
                let is_lambda = match self.expect_peek()? {
                    Token::Colon(_) | Token::Assign(_) => true,
                    Token::RParen(_) => {
                        self.tokens.advance_cursor(); // Skip over ')'
                        if let Some(Token::Arrow(_)) = self.peek() { true } else { false }
                    }
                    _ => false
                };
                self.tokens.reset_cursor();

                if is_lambda {
                    let args = self.parse_func_args(true)?;
                    let arrow_tok = self.expect_next_token(TokenType::Arrow)?;
                    let body = self.parse_expr_or_block()?;
                    return Ok(AstNode::Lambda(arrow_tok, LambdaNode { args, body }));
                }
            } else {
                self.tokens.reset_cursor();
            }
        }

        let expr = self.parse_expr()?;

        if let Token::Comma(_) = self.expect_peek()? {
            let mut tuple_items = vec![expr];
            while let Token::Comma(_) = self.expect_peek()? {
                self.expect_next()?; // Consume ','
                tuple_items.push(self.parse_expr()?);
            }
            self.expect_next_token(TokenType::RParen)?;
            return Ok(AstNode::Tuple(token, tuple_items));
        }

        // If the next token is ')', we've finished parsing the grouped expression. We have an infix
        // parser for the '=>' token which expects either an identifier or a grouped expr.
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
            Token::PlusEq(_) => BinaryOp::AddEq,
            Token::Minus(_) => BinaryOp::Sub,
            Token::MinusEq(_) => BinaryOp::SubEq,
            Token::Star(_) => BinaryOp::Mul,
            Token::StarEq(_) => BinaryOp::MulEq,
            Token::Slash(_) => BinaryOp::Div,
            Token::SlashEq(_) => BinaryOp::DivEq,
            Token::Percent(_) => BinaryOp::Mod,
            Token::PercentEq(_) => BinaryOp::ModEq,
            Token::And(_) => BinaryOp::And,
            Token::AndEq(_) => BinaryOp::AndEq,
            Token::Or(_) => BinaryOp::Or,
            Token::OrEq(_) => BinaryOp::OrEq,
            Token::Elvis(_) => BinaryOp::Coalesce,
            Token::ElvisEq(_) => BinaryOp::CoalesceEq,
            Token::GT(_) => BinaryOp::Gt,
            Token::GTE(_) => BinaryOp::Gte,
            Token::LT(_) => BinaryOp::Lt,
            Token::LTE(_) => BinaryOp::Lte,
            Token::Neq(_) => BinaryOp::Neq,
            Token::Eq(_) => BinaryOp::Eq,
            Token::Caret(_) => BinaryOp::Xor,
            Token::StarStar(_) => BinaryOp::Pow,
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
                    ident @ AstNode::Identifier(_, _) => {
                        if let Token::Colon(_) = self.expect_peek()? {
                            self.expect_next()?; // Consume ':'
                            let arg_value = self.parse_expr()?;
                            // Kind of silly, but I can't destruct ident while also @-ing it
                            let arg_name = match ident {
                                AstNode::Identifier(arg_name, None) => arg_name,
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
        let is_opt_safe = match &dot {
            Token::QuestionDot(_) => true,
            _ => false
        };
        let field = self.expect_next_token(TokenType::Ident)?;
        let field = self.parse_ident(field)?;
        Ok(AstNode::Accessor(dot, AccessorNode { target: Box::new(left), field: Box::new(field), is_opt_safe }))
    }

    fn parse_lambda(&mut self, token: Token, left: AstNode) -> Result<AstNode, ParseError> {
        // Since this function will be called as the infix operator resolver for the => token,
        // we need to assert that the `left` node is either an Identifier, or a Grouped that wraps
        // an Identifier.
        let args = match left {
            AstNode::Identifier(ident, _) => vec![(ident, None, false, None)],
            AstNode::Grouped(_, GroupedNode { expr }) => {
                match *expr {
                    AstNode::Identifier(ident, _) => vec![(ident, None, false, None)],
                    _ => return Err(ParseError::UnexpectedToken(token))
                }
            }
            _ => return Err(ParseError::UnexpectedToken(token))
        };

        let body = self.parse_expr_or_block()?;
        Ok(AstNode::Lambda(token, LambdaNode { args, body }))
    }

    fn parse_array(&mut self, token: Token) -> Result<AstNode, ParseError> {
        let mut item_expected = true;
        let mut items = vec![];
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
            items.push(expr);

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

    fn parse_set_literal(&mut self, token: Token) -> Result<AstNode, ParseError> {
        let mut item_expected = true;
        let mut items = vec![];
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

            let expr = self.parse_expr()?;
            items.push(expr);

            let token = self.peek().ok_or(ParseError::UnexpectedEof)?;
            if let Token::Comma(_) = token {
                self.expect_next()?; // Consume comma
            } else {
                item_expected = false;
            }
        }

        Ok(AstNode::Set(token, SetNode { items }))
    }

    fn parse_ident(&mut self, token: Token) -> Result<AstNode, ParseError> {
        let ident_token = match &token {
            Token::Ident(_, _) | Token::Self_(_) | Token::None(_) => token,
            _ => return Err(ParseError::UnexpectedToken(token)) // This should be unreachable, but just in case...
        };
        let mut types = Vec::new();
        if let Some(Token::LT(_)) = self.peek() {
            self.tokens.advance_cursor();
            loop {
                if self.peek().is_none() {
                    return Err(ParseError::UnexpectedEof);
                }

                if let Some(Token::GT(_)) = self.peek() {
                    if types.is_empty() {
                        self.tokens.reset_cursor();
                    } else {
                        self.tokens.advance_cursor();
                    }
                    break;
                }
                let type_ident_res = self.parse_type_identifier(false);
                match type_ident_res {
                    Err(_) => {
                        self.tokens.reset_cursor();
                        break;
                    }
                    Ok(type_ident) => {
                        types.push(type_ident);
                        if let Some(Token::Comma(_)) = self.peek() {
                            self.tokens.advance_cursor();
                        }
                    }
                }
            }
            if let Some(Token::LParen(_, _)) = self.peek() {
                self.tokens.truncate_iterator_to_cursor();
            } else {
                self.tokens.reset_cursor();
            }
        }
        let types = if types.is_empty() { None } else { Some(types) };
        Ok(AstNode::Identifier(ident_token, types))
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
        super::parse(tokens).map(|ParseResult { nodes, .. }| nodes)
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
    fn parse_string_interpolation() -> TestResult {
        let ast = parse("\"abc $def ghi\"")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 1), false),
            InvocationNode {
                target: Box::new(AstNode::Accessor(
                    Token::Dot(Position::new(1, 1)),
                    AccessorNode {
                        target: Box::new(AstNode::Literal(
                            Token::String(Position::new(1, 1), "abc ".to_string()),
                            AstLiteralNode::StringLiteral("abc ".to_string()),
                        )),
                        field: Box::new(AstNode::Identifier(
                            Token::Ident(Position::new(1, 1), "concat".to_string()),
                            None,
                        )),
                        is_opt_safe: false,
                    },
                )),
                args: vec![
                    (None, AstNode::Identifier(
                        Token::Ident(Position::new(1, 7), "def".to_string()),
                        None,
                    )),
                    (None, AstNode::Literal(
                        Token::String(Position::new(1, 10), " ghi".to_string()),
                        AstLiteralNode::StringLiteral(" ghi".to_string()),
                    ))
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("\"abc ${1 + 2} ghi\"")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 1), false),
            InvocationNode {
                target: Box::new(AstNode::Accessor(
                    Token::Dot(Position::new(1, 1)),
                    AccessorNode {
                        target: Box::new(AstNode::Literal(
                            Token::String(Position::new(1, 1), "abc ".to_string()),
                            AstLiteralNode::StringLiteral("abc ".to_string()),
                        )),
                        field: Box::new(AstNode::Identifier(
                            Token::Ident(Position::new(1, 1), "concat".to_string()),
                            None,
                        )),
                        is_opt_safe: false,
                    },
                )),
                args: vec![
                    (None, AstNode::Binary(
                        Token::Plus(Position::new(1, 10)),
                        BinaryNode {
                            left: Box::new(int_literal!((1, 8), 1)),
                            op: BinaryOp::Add,
                            right: Box::new(int_literal!((1, 12), 2)),
                        },
                    )),
                    (None, AstNode::Literal(
                        Token::String(Position::new(1, 14), " ghi".to_string()),
                        AstLiteralNode::StringLiteral(" ghi".to_string()),
                    ))
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
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
        let ast = parse("1 ** 5 + 2 * 3 % 4")?;
        let expected = vec![
            Binary(
                Token::Plus(Position::new(1, 8)),
                BinaryNode {
                    left: Box::new(
                        Binary(
                            Token::StarStar(Position::new(1, 3)),
                            BinaryNode {
                                left: Box::new(int_literal!((1, 1), 1)),
                                op: BinaryOp::Pow,
                                right: Box::new(int_literal!((1, 6), 5)),
                            },
                        )
                    ),
                    op: BinaryOp::Add,
                    right: Box::new(
                        Binary(
                            Token::Percent(Position::new(1, 16)),
                            BinaryNode {
                                left: Box::new(
                                    Binary(
                                        Token::Star(Position::new(1, 12)),
                                        BinaryNode {
                                            left: Box::new(int_literal!((1, 10), 2)),
                                            op: BinaryOp::Mul,
                                            right: Box::new(int_literal!((1, 14), 3)),
                                        },
                                    )
                                ),
                                op: BinaryOp::Mod,
                                right: Box::new(int_literal!((1, 18), 4)),
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
                            Token::LParen(Position::new(1, 1), false),
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
                                        Token::LBrack(Position::new(1, 11), false),
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
    fn parse_binary_assignment_operators() -> TestResult {
        let test_cases = vec![
            ("a += 3", Token::PlusEq(Position::new(1, 3)), BinaryOp::AddEq, int_literal!((1, 6), 3)),
            ("a -= 3", Token::MinusEq(Position::new(1, 3)), BinaryOp::SubEq, int_literal!((1, 6), 3)),
            ("a *= 3", Token::StarEq(Position::new(1, 3)), BinaryOp::MulEq, int_literal!((1, 6), 3)),
            ("a /= 3", Token::SlashEq(Position::new(1, 3)), BinaryOp::DivEq, int_literal!((1, 6), 3)),
            ("a %= 3", Token::PercentEq(Position::new(1, 3)), BinaryOp::ModEq, int_literal!((1, 6), 3)),
            ("a &&= true", Token::AndEq(Position::new(1, 3)), BinaryOp::AndEq, bool_literal!((1, 7), true)),
            ("a ||= false", Token::OrEq(Position::new(1, 3)), BinaryOp::OrEq, bool_literal!((1, 7), false)),
            ("a ?:= false", Token::ElvisEq(Position::new(1, 3)), BinaryOp::CoalesceEq, bool_literal!((1, 7), false)),
        ];

        for (input, tok, op, right) in test_cases {
            let ast = parse(input)?;
            let left = Box::new(identifier!((1, 1), "a"));
            let expected = vec![
                Binary(tok, BinaryNode { left, op, right: Box::new(right) })
            ];
            assert_eq!(expected, ast);
        }

        Ok(())
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
        let expected = AstNode::Array(Token::LBrack(Position::new(1, 1), false), ArrayNode {
            items: vec![]
        });
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_array_with_items() -> TestResult {
        let ast = parse("[1, true, \"a\", 3.14]")?;
        let expected = AstNode::Array(
            Token::LBrack(Position::new(1, 1), false),
            ArrayNode {
                items: vec![
                    int_literal!((1, 2), 1),
                    bool_literal!((1, 5), true),
                    string_literal!((1, 11), "a"),
                    float_literal!((1, 16), 3.14),
                ]
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_array_nested() -> TestResult {
        let ast = parse("[[1, 2], [3, 4]]")?;
        let expected = AstNode::Array(
            Token::LBrack(Position::new(1, 1), false),
            ArrayNode {
                items: vec![
                    AstNode::Array(
                        Token::LBrack(Position::new(1, 2), false),
                        ArrayNode {
                            items: vec![
                                int_literal!((1, 3), 1),
                                int_literal!((1, 6), 2),
                            ]
                        },
                    ),
                    AstNode::Array(
                        Token::LBrack(Position::new(1, 10), false),
                        ArrayNode {
                            items: vec![
                                int_literal!((1, 11), 3),
                                int_literal!((1, 14), 4),
                            ]
                        },
                    )
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
    fn parse_set_empty() -> TestResult {
        let ast = parse("#{}")?;
        let expected = AstNode::Set(Token::LBraceHash(Position::new(1, 1)), SetNode { items: vec![] });
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_set_with_items() -> TestResult {
        let ast = parse("#{1, 2, 3}")?;
        let expected = AstNode::Set(
            Token::LBraceHash(Position::new(1, 1)),
            SetNode { items: vec![int_literal!((1, 3), 1), int_literal!((1, 6), 2), int_literal!((1, 9), 3)] },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("#{1, 2, 3, }")?; // Test with trailing comma
        assert_eq!(expected, ast[0]); // Test with same expectation

        let ast = parse("#{#{1, 2}, #{3, 4}}")?;
        let expected = AstNode::Set(
            Token::LBraceHash(Position::new(1, 1)),
            SetNode {
                items: vec![
                    AstNode::Set(
                        Token::LBraceHash(Position::new(1, 3)),
                        SetNode {
                            items: vec![int_literal!((1, 5), 1), int_literal!((1, 8), 2)]
                        },
                    ),
                    AstNode::Set(
                        Token::LBraceHash(Position::new(1, 12)),
                        SetNode {
                            items: vec![int_literal!((1, 14), 3), int_literal!((1, 17), 4)]
                        },
                    )
                ]
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_binding_decls_no_assignment() -> TestResult {
        let ast = parse("val abc\nvar abc")?;
        let expected = vec![
            AstNode::BindingDecl(
                Token::Val(Position::new(1, 1)),
                BindingDeclNode {
                    export_token: None,
                    binding: BindingPattern::Variable(ident_token!((1, 5), "abc")),
                    is_mutable: false,
                    type_ann: None,
                    expr: None,
                },
            ),
            AstNode::BindingDecl(
                Token::Var(Position::new(2, 1)),
                BindingDeclNode {
                    export_token: None,
                    binding: BindingPattern::Variable(ident_token!((2, 5), "abc")),
                    is_mutable: true,
                    type_ann: None,
                    expr: None,
                },
            ),
        ];
        assert_eq!(expected, ast);

        Ok(())
    }

    #[test]
    fn parse_binding_decls_with_assignment() -> TestResult {
        let ast = parse("val abc = 1 + \"a\"\nvar abc = 1")?;
        let expected = vec![
            AstNode::BindingDecl(
                Token::Val(Position::new(1, 1)),
                BindingDeclNode {
                    export_token: None,
                    binding: BindingPattern::Variable(ident_token!((1, 5), "abc")),
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
                    export_token: None,
                    binding: BindingPattern::Variable(ident_token!((2, 5), "abc")),
                    is_mutable: true,
                    type_ann: None,
                    expr: Some(Box::new(int_literal!((2, 11), 1))),
                },
            ),
        ];
        assert_eq!(expected, ast);

        Ok(())
    }

    #[test]
    fn parse_binding_decls_destructuring() -> TestResult {
        let cases = vec![
            // Variable (aka non-destructuring)
            ("val a", BindingPattern::Variable(ident_token!((1, 5), "a"))),
            // Tuples
            (
                "val (a, b) = (1, 2)",
                BindingPattern::Tuple(
                    Token::LParen(Position::new(1, 5), false),
                    vec![
                        BindingPattern::Variable(ident_token!((1, 6), "a")),
                        BindingPattern::Variable(ident_token!((1, 9), "b")),
                    ],
                )
            ),
            (
                "val ((a, b), c) = ((1, 2), 3)",
                BindingPattern::Tuple(
                    Token::LParen(Position::new(1, 5), false),
                    vec![
                        BindingPattern::Tuple(
                            Token::LParen(Position::new(1, 6), false),
                            vec![
                                BindingPattern::Variable(ident_token!((1, 7), "a")),
                                BindingPattern::Variable(ident_token!((1, 10), "b")),
                            ],
                        ),
                        BindingPattern::Variable(ident_token!((1, 14), "c")),
                    ],
                )
            ),
            // Arrays
            (
                "val [a, b] = [1, 2]",
                BindingPattern::Array(
                    Token::LBrack(Position::new(1, 5), false),
                    vec![
                        (BindingPattern::Variable(ident_token!((1, 6), "a")), false),
                        (BindingPattern::Variable(ident_token!((1, 9), "b")), false),
                    ],
                    false,
                )
            ),
            (
                "val [[a], b] = [[1], 2]",
                BindingPattern::Array(
                    Token::LBrack(Position::new(1, 5), false),
                    vec![
                        (
                            BindingPattern::Array(
                                Token::LBrack(Position::new(1, 6), false),
                                vec![
                                    (BindingPattern::Variable(ident_token!((1, 7), "a")), false),
                                ],
                                false,
                            ),
                            false
                        ),
                        (BindingPattern::Variable(ident_token!((1, 11), "b")), false),
                    ],
                    false,
                )
            ),
            // Nested
            (
                "val ([a, b], [(c, d)]) = ([1, 2], [(3, 4)])",
                BindingPattern::Tuple(
                    Token::LParen(Position::new(1, 5), false),
                    vec![
                        BindingPattern::Array(
                            Token::LBrack(Position::new(1, 6), false),
                            vec![
                                (BindingPattern::Variable(ident_token!((1, 7), "a")), false, ),
                                (BindingPattern::Variable(ident_token!((1, 10), "b")), false, ),
                            ],
                            false,
                        ),
                        BindingPattern::Array(
                            Token::LBrack(Position::new(1, 14), false),
                            vec![
                                (
                                    BindingPattern::Tuple(
                                        Token::LParen(Position::new(1, 15), false),
                                        vec![
                                            BindingPattern::Variable(ident_token!((1, 16), "c")),
                                            BindingPattern::Variable(ident_token!((1, 19), "d")),
                                        ],
                                    ),
                                    false
                                ),
                            ],
                            false,
                        ),
                    ],
                )
            ),
        ];

        for (input, pattern) in cases {
            let ast = parse(input)?;
            let actual = if let AstNode::BindingDecl(_, BindingDeclNode { binding, .. }) = ast.first().unwrap().clone() { binding } else { unreachable!() };
            assert_eq!(pattern, actual);
        }

        Ok(())
    }

    #[test]
    fn parse_binding_decls_errors() {
        let err = parse("val (a b) = (1, 2)").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::RParen, ident_token!((1, 8), "b"));
        assert_eq!(expected, err);

        let err = parse("val (1, b) = (1, 2)").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Int(Position::new(1, 6), 1));
        assert_eq!(expected, err);

        let err = parse("val (a, b = (1, 2)").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::RParen, Token::Assign(Position::new(1, 11)));
        assert_eq!(expected, err);

        let err = parse("val (a, b)\na + b").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Assign, ident_token!((2, 1), "a"));
        assert_eq!(expected, err);
    }

    #[test]
    fn parse_type_identifier() {
        #[inline]
        fn parse_type_identifier(input: &str) -> TypeIdentifier {
            let tokens = tokenize(&input.to_string()).unwrap();
            let mut parser = Parser::new(tokens);
            parser.parse_type_identifier(true).unwrap()
        }

        // Plain idents, optionals, and arrays
        let type_ident = parse_type_identifier("Bool");
        let expected = TypeIdentifier::Normal {
            ident: Token::Ident(Position::new(1, 1), "Bool".to_string()),
            type_args: None,
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("Int[]");
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Normal {
                ident: Token::Ident(Position::new(1, 1), "Int".to_string()),
                type_args: None,
            })
        };
        assert_eq!(expected, type_ident);

        // Verify that the parens are ignored
        let type_ident = parse_type_identifier("Int?");
        let expected = TypeIdentifier::Option {
            inner: Box::new(TypeIdentifier::Normal {
                ident: Token::Ident(Position::new(1, 1), "Int".to_string()),
                type_args: None,
            })
        };
        assert_eq!(expected, type_ident);
        let type_ident = parse_type_identifier("(Int)?");
        let expected = TypeIdentifier::Option {
            inner: Box::new(TypeIdentifier::Normal {
                ident: Token::Ident(Position::new(1, 2), "Int".to_string()),
                type_args: None,
            })
        };
        assert_eq!(expected, type_ident);
        let type_ident = parse_type_identifier("(((Int)?))");
        let expected = TypeIdentifier::Option {
            inner: Box::new(TypeIdentifier::Normal {
                ident: Token::Ident(Position::new(1, 4), "Int".to_string()),
                type_args: None,
            })
        };
        assert_eq!(expected, type_ident);

        // Complex arrays
        let type_ident = parse_type_identifier("Int[][]");
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Array {
                inner: Box::new(TypeIdentifier::Normal {
                    ident: Token::Ident(Position::new(1, 1), "Int".to_string()),
                    type_args: None,
                })
            })
        };
        assert_eq!(expected, type_ident);
        let type_ident = parse_type_identifier("(Int[])[]");
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Array {
                inner: Box::new(TypeIdentifier::Normal {
                    ident: Token::Ident(Position::new(1, 2), "Int".to_string()),
                    type_args: None,
                })
            })
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("Int?[]");
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Option {
                inner: Box::new(TypeIdentifier::Normal {
                    ident: Token::Ident(Position::new(1, 1), "Int".to_string()),
                    type_args: None,
                })
            })
        };
        assert_eq!(expected, type_ident);
        let type_ident = parse_type_identifier("(Int?)[]");
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Option {
                inner: Box::new(TypeIdentifier::Normal {
                    ident: Token::Ident(Position::new(1, 2), "Int".to_string()),
                    type_args: None,
                })
            })
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("Int?[]?");
        let expected = TypeIdentifier::Option {
            inner: Box::new(TypeIdentifier::Array {
                inner: Box::new(TypeIdentifier::Option {
                    inner: Box::new(TypeIdentifier::Normal {
                        ident: Token::Ident(Position::new(1, 1), "Int".to_string()),
                        type_args: None,
                    })
                })
            })
        };
        assert_eq!(expected, type_ident);

        // Tuple types
        let type_ident = parse_type_identifier("(Int, Int)");
        let expected = TypeIdentifier::Tuple {
            types: vec![
                TypeIdentifier::Normal { ident: ident_token!((1, 2), "Int"), type_args: None },
                TypeIdentifier::Normal { ident: ident_token!((1, 7), "Int"), type_args: None },
            ]
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("(Int, (Bool, Int), String[])?");
        let expected = TypeIdentifier::Option {
            inner: Box::new(TypeIdentifier::Tuple {
                types: vec![
                    TypeIdentifier::Normal { ident: ident_token!((1, 2), "Int"), type_args: None },
                    TypeIdentifier::Tuple {
                        types: vec![
                            TypeIdentifier::Normal { ident: ident_token!((1, 8), "Bool"), type_args: None },
                            TypeIdentifier::Normal { ident: ident_token!((1, 14), "Int"), type_args: None },
                        ]
                    },
                    TypeIdentifier::Array {
                        inner: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 20), "String"), type_args: None })
                    }
                ]
            })
        };
        assert_eq!(expected, type_ident);

        // Function types
        let type_ident = parse_type_identifier("() => Int");
        let expected = TypeIdentifier::Func {
            args: vec![],
            ret: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 7), "Int"), type_args: None }),
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("() => Int?");
        let expected = TypeIdentifier::Func {
            args: vec![],
            ret: Box::new(TypeIdentifier::Option {
                inner: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 7), "Int"), type_args: None })
            }),
        };
        assert_eq!(expected, type_ident);
        let type_ident = parse_type_identifier("(() => Int)?");
        let expected = TypeIdentifier::Option {
            inner: Box::new(
                TypeIdentifier::Func {
                    args: vec![],
                    ret: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 8), "Int"), type_args: None }),
                }
            ),
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("(String, Int?) => Int");
        let expected = TypeIdentifier::Func {
            args: vec![
                TypeIdentifier::Normal { ident: ident_token!((1, 2), "String"), type_args: None },
                TypeIdentifier::Option {
                    inner: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 10), "Int"), type_args: None })
                },
            ],
            ret: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 19), "Int"), type_args: None }),
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("((String, Int[]) => Int)[]");
        let expected = TypeIdentifier::Array {
            inner: Box::new(TypeIdentifier::Func {
                args: vec![
                    TypeIdentifier::Normal { ident: ident_token!((1, 3), "String"), type_args: None },
                    TypeIdentifier::Array {
                        inner: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 11), "Int"), type_args: None })
                    },
                ],
                ret: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 21), "Int"), type_args: None }),
            })
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("((String) => Int, Int[]) => (String) => Int");
        let expected = TypeIdentifier::Func {
            args: vec![
                TypeIdentifier::Func {
                    args: vec![TypeIdentifier::Normal { ident: ident_token!((1, 3), "String"), type_args: None }],
                    ret: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 14), "Int"), type_args: None }),
                },
                TypeIdentifier::Array {
                    inner: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 19), "Int"), type_args: None })
                }
            ],
            ret: Box::new(TypeIdentifier::Func {
                args: vec![TypeIdentifier::Normal { ident: ident_token!((1, 30), "String"), type_args: None }],
                ret: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 41), "Int"), type_args: None }),
            }),
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("List<Int>");
        let expected = TypeIdentifier::Normal {
            ident: ident_token!((1, 1), "List"),
            type_args: Some(vec![
                TypeIdentifier::Normal { ident: ident_token!((1, 6), "Int"), type_args: None },
            ]),
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("Map<String, Int>");
        let expected = TypeIdentifier::Normal {
            ident: ident_token!((1, 1), "Map"),
            type_args: Some(vec![
                TypeIdentifier::Normal { ident: ident_token!((1, 5), "String"), type_args: None },
                TypeIdentifier::Normal { ident: ident_token!((1, 13), "Int"), type_args: None },
            ]),
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("Map<String, Map<String, List<Int>>>");
        let expected = TypeIdentifier::Normal {
            ident: ident_token!((1, 1), "Map"),
            type_args: Some(vec![
                TypeIdentifier::Normal { ident: ident_token!((1, 5), "String"), type_args: None },
                TypeIdentifier::Normal {
                    ident: ident_token!((1, 13), "Map"),
                    type_args: Some(vec![
                        TypeIdentifier::Normal { ident: ident_token!((1, 17), "String"), type_args: None },
                        TypeIdentifier::Normal {
                            ident: ident_token!((1, 25), "List"),
                            type_args: Some(vec![
                                TypeIdentifier::Normal { ident: ident_token!((1, 30), "Int"), type_args: None },
                            ]),
                        },
                    ]),
                },
            ]),
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("String | Int");
        let expected = TypeIdentifier::Union {
            left: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 1), "String"), type_args: None }),
            right: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 10), "Int"), type_args: None }),
        };
        assert_eq!(expected, type_ident);

        let type_ident = parse_type_identifier("String | Int | Float");
        let expected = TypeIdentifier::Union {
            left: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 1), "String"), type_args: None }),
            right: Box::new(TypeIdentifier::Union {
                left: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 10), "Int"), type_args: None }),
                right: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 16), "Float"), type_args: None }),
            }),
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
                export_token: None,
                name: Token::Ident(Position::new(1, 6), "abc".to_string()),
                type_args: vec![],
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
                export_token: None,
                name: Token::Ident(Position::new(1, 6), "abc".to_string()),
                type_args: vec![],
                args: vec![],
                ret_type: None,
                body: vec![
                    AstNode::BindingDecl(
                        Token::Val(Position::new(1, 14)),
                        BindingDeclNode {
                            export_token: None,
                            binding: BindingPattern::Variable(ident_token!((1, 18), "a")),
                            is_mutable: false,
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
            (ident_token!((1, 10), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 13), "Int"), type_args: None }), false, None)
        ];
        assert_eq!(&expected, args);

        let ast = parse("func abc(a: Int, b: Int?) = 123")?;
        let args = match ast.first().unwrap() {
            AstNode::FunctionDecl(_, FunctionDeclNode { args, .. }) => args,
            _ => unreachable!()
        };
        let expected = vec![
            (ident_token!((1, 10), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 13), "Int"), type_args: None }), false, None),
            (ident_token!((1, 18), "b"), Some(TypeIdentifier::Option { inner: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 21), "Int"), type_args: None }) }), false, None)
        ];
        assert_eq!(&expected, args);

        // Testing trailing comma in param list
        let ast = parse("func abc(a: Int,) = 123")?;
        let args = match ast.first().unwrap() {
            AstNode::FunctionDecl(_, FunctionDeclNode { args, .. }) => args,
            _ => unreachable!()
        };
        let expected = vec![
            (ident_token!((1, 10), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 13), "Int"), type_args: None }), false, None)
        ];
        assert_eq!(&expected, args);

        // Testing default arg values
        let ast = parse("func abc(a: Int, b: Int = 2, c = 4) = 123")?;
        let args = match ast.first().unwrap() {
            AstNode::FunctionDecl(_, FunctionDeclNode { args, .. }) => args,
            _ => unreachable!()
        };
        let expected = vec![
            (ident_token!((1, 10), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 13), "Int"), type_args: None }), false, None),
            (ident_token!((1, 18), "b"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 21), "Int"), type_args: None }), false, Some(int_literal!((1, 27), 2))),
            (ident_token!((1, 30), "c"), None, false, Some(int_literal!((1, 34), 4))),
        ];
        assert_eq!(&expected, args);

        // Testing return type
        let ast = parse("func abc(a: Int): String = 123")?;
        let ret_type = match ast.first().unwrap() {
            AstNode::FunctionDecl(_, FunctionDeclNode { ret_type, .. }) => ret_type,
            _ => unreachable!()
        };
        let expected = Some(TypeIdentifier::Normal { ident: ident_token!((1, 19), "String"), type_args: None });
        assert_eq!(&expected, ret_type);

        // Testing varargs param
        let ast = parse("func abc(*a: Int[]) = 123")?;
        let args = match ast.first().unwrap() {
            AstNode::FunctionDecl(_, FunctionDeclNode { args, .. }) => args,
            _ => unreachable!()
        };
        let expected = vec![
            (ident_token!((1, 11), "a"), Some(TypeIdentifier::Array { inner: Box::new(TypeIdentifier::Normal { ident: ident_token!((1, 14), "Int"), type_args: None }) }), true, None)
        ];
        assert_eq!(&expected, args);

        Ok(())
    }

    #[test]
    fn parse_func_decl_type_args() -> TestResult {
        let ast = parse("func abc<T>(a: T, b: T?) = 123")?;
        let type_args = match ast.first().unwrap() {
            AstNode::FunctionDecl(_, FunctionDeclNode { type_args, .. }) => type_args,
            _ => unreachable!()
        };
        let expected = vec![
            ident_token!((1, 10), "T")
        ];
        assert_eq!(&expected, type_args);

        Ok(())
    }

    #[test]
    fn parse_func_decl_error() {
        let error = parse("func (a: Int) = 123").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::LParen(Position::new(1, 6), false));
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
    fn parse_lambda_no_args() -> TestResult {
        let ast = parse("() => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 4)),
            LambdaNode {
                args: vec![],
                body: vec![int_literal!((1, 7), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("() => {\nval a = 123\na }")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 4)),
            LambdaNode {
                args: vec![],
                body: vec![
                    AstNode::BindingDecl(
                        Token::Val(Position::new(2, 1)),
                        BindingDeclNode {
                            export_token: None,
                            binding: BindingPattern::Variable(ident_token!((2, 5), "a")),
                            type_ann: None,
                            expr: Some(Box::new(int_literal!((2, 9), 123))),
                            is_mutable: false,
                        },
                    ),
                    identifier!((3, 1), "a"),
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_lambda_single_arg() -> TestResult {
        let ast = parse("a => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 3)),
            LambdaNode {
                args: vec![(ident_token!((1, 1), "a"), None, false, None)],
                body: vec![int_literal!((1, 6), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("(a) => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 5)),
            LambdaNode {
                args: vec![(ident_token!((1, 2), "a"), None, false, None)],
                body: vec![int_literal!((1, 8), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("(a: String) => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 13)),
            LambdaNode {
                args: vec![(ident_token!((1, 2), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 5), "String"), type_args: None }), false, None)],
                body: vec![int_literal!((1, 16), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("(a = 2) => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 9)),
            LambdaNode {
                args: vec![(ident_token!((1, 2), "a"), None, false, Some(int_literal!((1, 6), 2)))],
                body: vec![int_literal!((1, 12), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("(a: String,) => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 14)),
            LambdaNode {
                args: vec![(ident_token!((1, 2), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 5), "String"), type_args: None }), false, None)],
                body: vec![int_literal!((1, 17), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        // Test with vararg param
        let ast = parse("(*a) => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 6)),
            LambdaNode {
                args: vec![(ident_token!((1, 3), "a"), None, true, None)],
                body: vec![int_literal!((1, 9), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_lambda_multi_arg() -> TestResult {
        let ast = parse("(a, b: Int) => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 13)),
            LambdaNode {
                args: vec![
                    (ident_token!((1, 2), "a"), None, false, None),
                    (ident_token!((1, 5), "b"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 8), "Int"), type_args: None }), false, None),
                ],
                body: vec![int_literal!((1, 16), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("(a, b) => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 8)),
            LambdaNode {
                args: vec![
                    (ident_token!((1, 2), "a"), None, false, None),
                    (ident_token!((1, 5), "b"), None, false, None),
                ],
                body: vec![int_literal!((1, 11), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("(a: Int, b: Int) => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 18)),
            LambdaNode {
                args: vec![
                    (ident_token!((1, 2), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 5), "Int"), type_args: None }), false, None),
                    (ident_token!((1, 10), "b"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 13), "Int"), type_args: None }), false, None),
                ],
                body: vec![int_literal!((1, 21), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("(a = 3, b: Int) => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 17)),
            LambdaNode {
                args: vec![
                    (ident_token!((1, 2), "a"), None, false, Some(int_literal!((1, 6), 3))),
                    (ident_token!((1, 9), "b"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 12), "Int"), type_args: None }), false, None),
                ],
                body: vec![int_literal!((1, 20), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("(a: Int, b = 3) => 123")?;
        let expected = AstNode::Lambda(
            Token::Arrow(Position::new(1, 17)),
            LambdaNode {
                args: vec![
                    (ident_token!((1, 2), "a"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 5), "Int"), type_args: None }), false, None),
                    (ident_token!((1, 10), "b"), None, false, Some(int_literal!((1, 14), 3))),
                ],
                body: vec![int_literal!((1, 20), 123)],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_tuple() -> TestResult {
        let ast = parse("(a, b)")?;
        let expected = AstNode::Tuple(
            Token::LParen(Position::new(1, 1), false),
            vec![identifier!((1, 2), "a"), identifier!((1, 5), "b")],
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("(a, 1)")?;
        let expected = AstNode::Tuple(
            Token::LParen(Position::new(1, 1), false),
            vec![identifier!((1, 2), "a"), int_literal!((1, 5), 1)],
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("(1, a, \"abc\")")?;
        let expected = AstNode::Tuple(
            Token::LParen(Position::new(1, 1), false),
            vec![int_literal!((1, 2), 1), identifier!((1, 5), "a"), string_literal!((1, 8), "abc")],
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("((1, a), \"abc\")")?;
        let expected = AstNode::Tuple(
            Token::LParen(Position::new(1, 1), false),
            vec![
                AstNode::Tuple(
                    Token::LParen(Position::new(1, 2), false),
                    vec![
                        int_literal!((1, 3), 1),
                        identifier!((1, 6), "a"),
                    ]),
                string_literal!((1, 10), "abc")
            ],
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_tuple_errors() {
        let error = parse("(a, b: Int)").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, error);

        let error = parse("(a, b = 1)").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, error);

        let error = parse("(1, b: Int)").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::RParen, Token::Colon(Position::new(1, 6)));
        assert_eq!(expected, error);
    }

    #[test]
    fn parse_type_decl() -> TestResult {
        let ast = parse("type Person {}")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                export_token: None,
                name: ident_token!((1, 6), "Person"),
                type_args: vec![],
                fields: vec![],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("type Person { name: String }")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                export_token: None,
                name: ident_token!((1, 6), "Person"),
                type_args: vec![],
                fields: vec![
                    TypeDeclField {
                        ident: ident_token!((1, 15), "name"),
                        type_ident: TypeIdentifier::Normal { ident: ident_token!((1, 21), "String"), type_args: None },
                        default_value: None,
                        readonly: None,
                    },
                ],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);
        let ast = parse("type Person { name: String, }")?; // Test trailing comma
        assert_eq!(expected, ast[0]);

        let ast = parse("type Person { name: String, age: Int }")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                export_token: None,
                name: ident_token!((1, 6), "Person"),
                type_args: vec![],
                fields: vec![
                    TypeDeclField {
                        ident: ident_token!((1, 15), "name"),
                        type_ident: TypeIdentifier::Normal { ident: ident_token!((1, 21), "String"), type_args: None },
                        default_value: None,
                        readonly: None,
                    },
                    TypeDeclField {
                        ident: ident_token!((1, 29), "age"),
                        type_ident: TypeIdentifier::Normal { ident: ident_token!((1, 34), "Int"), type_args: None },
                        default_value: None,
                        readonly: None,
                    },
                ],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        // Testing with default value
        let ast = parse("type Person { name: String, isHappy: Bool = true }")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                export_token: None,
                name: ident_token!((1, 6), "Person"),
                type_args: vec![],
                fields: vec![
                    TypeDeclField {
                        ident: ident_token!((1, 15), "name"),
                        type_ident: TypeIdentifier::Normal { ident: ident_token!((1, 21), "String"), type_args: None },
                        default_value: None,
                        readonly: None,
                    },
                    TypeDeclField {
                        ident: ident_token!((1, 29), "isHappy"),
                        type_ident: TypeIdentifier::Normal { ident: ident_token!((1, 38), "Bool"), type_args: None },
                        default_value: Some(bool_literal!((1, 45), true)),
                        readonly: None,
                    }
                ],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        // Test with field specs
        let ast = parse("type Person { name: String readonly }")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                export_token: None,
                name: ident_token!((1, 6), "Person"),
                type_args: vec![],
                fields: vec![
                    TypeDeclField {
                        ident: ident_token!((1, 15), "name"),
                        type_ident: TypeIdentifier::Normal { ident: ident_token!((1, 21), "String"), type_args: None },
                        default_value: None,
                        readonly: Some(Token::Readonly(Position::new(1, 28))),
                    },
                ],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_type_decl_type_args() -> TestResult {
        // Testing with a type argument
        let ast = parse("type List<T> { }")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                export_token: None,
                name: ident_token!((1, 6), "List"),
                type_args: vec![ident_token!((1, 11), "T")],
                fields: vec![],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        // Testing with multiple type arguments (with and without trailing commas)
        let ast = parse("type List<T, U> { }")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                export_token: None,
                name: ident_token!((1, 6), "List"),
                type_args: vec![
                    ident_token!((1, 11), "T"),
                    ident_token!((1, 14), "U"),
                ],
                fields: vec![],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);
        let ast = parse("type List<T, U,> { }")?;
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_type_decl_type_args_error() {
        let error = parse("type List<> { }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::GT(Position::new(1, 11)));
        assert_eq!(expected, error);

        let error = parse("type List<1> { }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::Int(Position::new(1, 11), 1));
        assert_eq!(expected, error);

        let error = parse("type List<[]> { }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::LBrack(Position::new(1, 11), false));
        assert_eq!(expected, error);

        let error = parse("type List<T, ,> { }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::Comma(Position::new(1, 14)));
        assert_eq!(expected, error);
    }

    #[test]
    fn parse_type_decl_methods() -> TestResult {
        let input = "\
          type Person {\n\
            func hello(self) = \"hello\"\n\
          }\
        ";
        let ast = parse(input)?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypeDeclNode {
                export_token: None,
                name: ident_token!((1, 6), "Person"),
                type_args: vec![],
                fields: vec![],
                methods: vec![
                    AstNode::FunctionDecl(
                        Token::Func(Position::new(2, 1)),
                        FunctionDeclNode {
                            export_token: None,
                            name: Token::Ident(Position::new(2, 6), "hello".to_string()),
                            type_args: vec![],
                            args: vec![
                                (Token::Self_(Position::new(2, 12)), None, false, None),
                            ],
                            ret_type: None,
                            body: vec![
                                string_literal!((2, 20), "hello"),
                            ],
                        },
                    ),
                ],
            },
        );
        Ok(assert_eq!(expected, ast[0]))
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
    fn parse_enum_decl() -> TestResult {
        let input = "\
          enum Color {\n\
            Red\n\
            Blue\n\
          }\
        ";
        let ast = parse(input)?;
        let expected = AstNode::EnumDecl(
            Token::Enum(Position::new(1, 1)),
            EnumDeclNode {
                export_token: None,
                name: ident_token!((1, 6), "Color"),
                type_args: vec![],
                variants: vec![
                    (ident_token!((2, 1), "Red"), None),
                    (ident_token!((3, 1), "Blue"), None),
                ],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        let input = "enum Direction { Red, Blue }";
        let ast = parse(input)?;
        let expected = AstNode::EnumDecl(
            Token::Enum(Position::new(1, 1)),
            EnumDeclNode {
                export_token: None,
                name: ident_token!((1, 6), "Direction"),
                type_args: vec![],
                variants: vec![
                    (ident_token!((1, 18), "Red"), None),
                    (ident_token!((1, 23), "Blue"), None),
                ],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        let input = "enum Direction { Red, Blue, Rgb(r: Int, g: Int, b: Int) }";
        let ast = parse(input)?;
        let expected = AstNode::EnumDecl(
            Token::Enum(Position::new(1, 1)),
            EnumDeclNode {
                export_token: None,
                name: ident_token!((1, 6), "Direction"),
                type_args: vec![],
                variants: vec![
                    (ident_token!((1, 18), "Red"), None),
                    (ident_token!((1, 23), "Blue"), None),
                    (ident_token!((1, 29), "Rgb"), Some(vec![
                        (ident_token!((1, 33), "r"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 36), "Int"), type_args: None }), false, None),
                        (ident_token!((1, 41), "g"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 44), "Int"), type_args: None }), false, None),
                        (ident_token!((1, 49), "b"), Some(TypeIdentifier::Normal { ident: ident_token!((1, 52), "Int"), type_args: None }), false, None),
                    ])),
                ],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_ident() -> TestResult {
        let ast = parse("abcd")?;
        let expected = identifier!((1, 1), "abcd");
        assert_eq!(expected, ast[0]);

        let ast = parse("self")?;
        let expected = AstNode::Identifier(Token::Self_(Position::new(1, 1)), None);
        assert_eq!(expected, ast[0]);

        let ast = parse("None")?;
        let expected = AstNode::Identifier(Token::None(Position::new(1, 1)), None);
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
            Token::LBrack(Position::new(1, 5), false),
            IndexingNode {
                target: Box::new(identifier!((1, 1), "abcd")),
                index: IndexingMode::Index(Box::new(int_literal!((1, 6), 1))),
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abcd[1:3]")?;
        let expected = AstNode::Indexing(
            Token::LBrack(Position::new(1, 5), false),
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
            Token::LBrack(Position::new(1, 5), false),
            IndexingNode {
                target: Box::new(identifier!((1, 1), "abcd")),
                index: IndexingMode::Range(
                    Some(Box::new(identifier!((1, 6), "a"))),
                    None,
                ),
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abcd[:b]")?;
        let expected = AstNode::Indexing(
            Token::LBrack(Position::new(1, 5), false),
            IndexingNode {
                target: Box::new(identifier!((1, 1), "abcd")),
                index: IndexingMode::Range(
                    None,
                    Some(Box::new(identifier!((1, 7), "b"))),
                ),
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_indexing_nested() -> TestResult {
        let ast = parse("a[b[2]]")?;
        let expected = AstNode::Indexing(
            Token::LBrack(Position::new(1, 2), false),
            IndexingNode {
                target: Box::new(identifier!((1, 1), "a")),
                index: IndexingMode::Index(Box::new(
                    AstNode::Indexing(
                        Token::LBrack(Position::new(1, 4), false),
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
            Token::LBrack(Position::new(1, 10), false),
            IndexingNode {
                target: Box::new(AstNode::Indexing(
                    Token::LBrack(Position::new(1, 7), false),
                    IndexingNode {
                        target: Box::new(
                            AstNode::Array(
                                Token::LBrack(Position::new(1, 1), false),
                                ArrayNode {
                                    items: vec![
                                        identifier!((1, 2), "a"),
                                        identifier!((1, 5), "b"),
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
    fn parse_indexing_separate_expressions() -> TestResult {
        let ast = parse("val a = 1\n+\na\n[a]\nprintln(a)\n[a]")?;
        let expected = vec![
            AstNode::BindingDecl(Token::Val(Position::new(1, 1)), BindingDeclNode {
                export_token: None,
                binding: BindingPattern::Variable(ident_token!((1, 5), "a")),
                type_ann: None,
                expr: Some(Box::new(AstNode::Binary(Token::Plus(Position::new(2, 1)), BinaryNode {
                    left: Box::new(int_literal!((1, 9), 1)),
                    op: BinaryOp::Add,
                    right: Box::new(identifier!((3, 1), "a")),
                }))),
                is_mutable: false,
            }),
            AstNode::Array(
                Token::LBrack(Position::new(4, 1), true),
                ArrayNode {
                    items: vec![identifier!((4, 2), "a")]
                },
            ),
            AstNode::Invocation(Token::LParen(Position { line: 5, col: 8 }, false), InvocationNode {
                target: Box::new(identifier!((5, 1), "println")),
                args: vec![
                    (None, identifier!((5, 9), "a"))
                ],
            }),
            AstNode::Array(
                Token::LBrack(Position::new(6, 1), true),
                ArrayNode {
                    items: vec![identifier!((6, 2), "a")]
                },
            )
        ];
        Ok(assert_eq!(expected, ast))
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
                condition_binding: None,
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
                condition_binding: None,
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
                condition_binding: None,
                if_block: vec![
                    string_literal!((1, 12), "hello")
                ],
                else_block: Some(vec![
                    AstNode::IfStatement(
                        Token::If(Position::new(1, 27)),
                        IfNode {
                            condition: Box::new(bool_literal!((1, 30), true)),
                            condition_binding: None,
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
                condition_binding: None,
                if_block: vec![
                    AstNode::BindingDecl(
                        Token::Val(Position::new(1, 12)),
                        BindingDeclNode {
                            export_token: None,
                            binding: BindingPattern::Variable(ident_token!((1, 16), "a")),
                            is_mutable: false,
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
                export_token: None,
                binding: BindingPattern::Variable(ident_token!((1, 5), "str")),
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
                            condition_binding: None,
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
                            condition_binding: None,
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
    fn parse_if_statement_with_condition_binding() -> TestResult {
        let ast = parse("if a |item| \"hello\"")?;
        let expected = AstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            IfNode {
                condition: Box::new(identifier!((1, 4), "a")),
                condition_binding: Some(BindingPattern::Variable(ident_token!((1, 7), "item"))),
                if_block: vec![
                    string_literal!((1, 13), "hello")
                ],
                else_block: None,
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn parse_invocation() -> TestResult {
        let ast = parse("abc()")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 4), false),
            InvocationNode {
                target: Box::new(identifier!((1, 1), "abc")),
                args: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abc(4)")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 4), false),
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
            Token::LParen(Position::new(1, 4), false),
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
            Token::LParen(Position::new(1, 4), false),
            InvocationNode {
                target: Box::new(identifier!((1, 1), "abc")),
                args: vec![
                    (None, int_literal!((1, 5), 4)),
                    (None, AstNode::Invocation(
                        Token::LParen(Position::new(1, 11), false),
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
            Token::LParen(Position::new(1, 8), false),
            InvocationNode {
                target: Box::new(AstNode::Accessor(
                    Token::Dot(Position::new(1, 4)),
                    AccessorNode {
                        target: Box::new(identifier!((1, 1), "abc")),
                        field: Box::new(identifier!((1, 5), "def")),
                        is_opt_safe: false,
                    },
                )),
                args: vec![
                    (None, int_literal!((1, 9), 4)),
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abc<Int>(4)")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 9), false),
            InvocationNode {
                target: Box::new(
                    AstNode::Identifier(
                        ident_token!((1, 1), "abc"),
                        Some(vec![TypeIdentifier::Normal { ident: ident_token!((1, 5), "Int"), type_args: None }]),
                    )
                ),
                args: vec![
                    (None, int_literal!((1, 10), 4))
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abc<Int, List<Int>>(4)")?;
        let expected = AstNode::Invocation(
            Token::LParen(Position::new(1, 20), false),
            InvocationNode {
                target: Box::new(
                    AstNode::Identifier(
                        ident_token!((1, 1), "abc"),
                        Some(vec![
                            TypeIdentifier::Normal { ident: ident_token!((1, 5), "Int"), type_args: None },
                            TypeIdentifier::Normal {
                                ident: ident_token!((1, 10), "List"),
                                type_args: Some(vec![
                                    TypeIdentifier::Normal { ident: ident_token!((1, 15), "Int"), type_args: None },
                                ]),
                            },
                        ]),
                    )
                ),
                args: vec![
                    (None, int_literal!((1, 21), 4))
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
            Token::LParen(Position::new(1, 4), false),
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
            Token::LParen(Position::new(1, 4), false),
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
            Token::LParen(Position::new(1, 4), false),
            InvocationNode {
                target: Box::new(identifier!((1, 1), "abc")),
                args: vec![
                    (Some(ident_token!((1, 5), "a")), int_literal!((1, 8), 4)),
                    (None, AstNode::Invocation(
                        Token::LParen(Position::new(1, 14), false),
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
    fn parse_array_invocation_separate_expressions() -> TestResult {
        let ast = parse("[a, b]\n(a + b)")?;
        let expected = vec![
            AstNode::Array(
                Token::LBrack(Position::new(1, 1), false),
                ArrayNode {
                    items: vec![
                        identifier!((1, 2), "a"),
                        identifier!((1, 5), "b"),
                    ]
                },
            ),
            AstNode::Grouped(Token::LParen(Position::new(2, 1), true), GroupedNode {
                expr: Box::new(AstNode::Binary(Token::Plus(Position::new(2, 4)), BinaryNode {
                    left: Box::new(identifier!((2, 2), "a")),
                    op: BinaryOp::Add,
                    right: Box::new(identifier!((2, 6), "b")),
                }))
            }),
        ];
        Ok(assert_eq!(expected, ast))
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
                condition_binding: None,
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
                condition_binding: None,
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
                            left: Box::new(identifier!((1, 7), "a")),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 11), 3)),
                        },
                    )
                ),
                condition_binding: None,
                body: vec![
                    AstNode::BindingDecl(
                        Token::Val(Position::new(2, 1)),
                        BindingDeclNode {
                            export_token: None,
                            binding: BindingPattern::Variable(ident_token!((2, 5), "a")),
                            is_mutable: false,
                            type_ann: None,
                            expr: Some(Box::new(int_literal!((2, 9), 1))),
                        },
                    ),
                    AstNode::Binary(
                        Token::Plus(Position::new(3, 3)),
                        BinaryNode {
                            left: Box::new(identifier!((3, 1), "a")),
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
    fn parse_while_loop_with_condition_binding() -> TestResult {
        let ast = parse("while a |item| { \"hello\" }")?;
        let expected = AstNode::WhileLoop(
            Token::While(Position::new(1, 1)),
            WhileLoopNode {
                condition: Box::new(identifier!((1, 7), "a")),
                condition_binding: Some(ident_token!((1, 10), "item")),
                body: vec![
                    string_literal!((1, 18), "hello")
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
                binding: BindingPattern::Variable(ident_token!((1, 5), "a")),
                index_ident: None,
                iterator: Box::new(AstNode::Array(
                    Token::LBrack(Position::new(1, 10), false),
                    ArrayNode {
                        items: vec![
                            int_literal!((1, 11), 0),
                            int_literal!((1, 14), 1),
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
                binding: BindingPattern::Variable(ident_token!((1, 5), "a")),
                index_ident: Some(ident_token!((1, 8), "i")),
                iterator: Box::new(AstNode::Array(
                    Token::LBrack(Position::new(1, 13), false),
                    ArrayNode {
                        items: vec![
                            int_literal!((1, 14), 0),
                            int_literal!((1, 17), 1),
                        ]
                    },
                )),
                body: vec![identifier!((1, 22), "a")],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("for (x, y), i in [a, b] { x }")?;
        let expected = AstNode::ForLoop(
            Token::For(Position::new(1, 1)),
            ForLoopNode {
                binding: BindingPattern::Tuple(
                    Token::LParen(Position::new(1, 5), false),
                    vec![
                        BindingPattern::Variable(ident_token!((1, 6), "x")),
                        BindingPattern::Variable(ident_token!((1, 9), "y"))
                    ],
                ),
                index_ident: Some(ident_token!((1, 13), "i")),
                iterator: Box::new(AstNode::Array(
                    Token::LBrack(Position::new(1, 18), false),
                    ArrayNode {
                        items: vec![
                            identifier!((1, 19), "a"),
                            identifier!((1, 22), "b")
                        ]
                    },
                )),
                body: vec![identifier!((1, 27), "x")],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_for_loop_error() {
        let error = parse("for 123 in [0, 1] { a }").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Int(Position::new(1, 5), 123));
        assert_eq!(expected, error);

        let error = parse("for a [0, 1] { a }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::In, Token::LBrack(Position::new(1, 7), false));
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
                field: Box::new(identifier!((1, 5), "def")),
                is_opt_safe: false,
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
                                field: Box::new(identifier!((1, 5), "def")),
                                is_opt_safe: false,
                            },
                        )),
                        field: Box::new(identifier!((1, 9), "ghi")),
                        is_opt_safe: false,
                    },
                )),
                field: Box::new(identifier!((1, 13), "jkl")),
                is_opt_safe: false,
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_optional_dot_accessor() -> TestResult {
        let ast = parse("abc?.def")?;
        let expected = AstNode::Accessor(
            Token::QuestionDot(Position::new(1, 4)),
            AccessorNode {
                target: Box::new(identifier!((1, 1), "abc")),
                field: Box::new(identifier!((1, 6), "def")),
                is_opt_safe: true,
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = parse("abc.def?.ghi?.jkl")?;
        let expected = AstNode::Accessor(
            Token::QuestionDot(Position::new(1, 13)),
            AccessorNode {
                target: Box::new(AstNode::Accessor(
                    Token::QuestionDot(Position::new(1, 8)),
                    AccessorNode {
                        target: Box::new(AstNode::Accessor(
                            Token::Dot(Position::new(1, 4)),
                            AccessorNode {
                                target: Box::new(identifier!((1, 1), "abc")),
                                field: Box::new(identifier!((1, 5), "def")),
                                is_opt_safe: false,
                            },
                        )),
                        field: Box::new(identifier!((1, 10), "ghi")),
                        is_opt_safe: true,
                    },
                )),
                field: Box::new(identifier!((1, 15), "jkl")),
                is_opt_safe: true,
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_match_statement() -> TestResult {
        let ast = parse("\
          match a {\n\
            Int => 123\n\
            A.String a => a\n\
            Abc(a, b) abc => a\n\
            A.Bcd(a) => a\n\
            _ => 0\n\
            _ x => {\n\
              0\n\
              x\n\
            }\n\
          }
        ")?;
        let expected = AstNode::MatchStatement(
            Token::Match(Position::new(1, 1)),
            MatchNode {
                target: Box::new(identifier!((1, 7), "a")),
                branches: vec![
                    (
                        MatchCase { token: ident_token!((2, 1), "Int"), match_type: MatchCaseType::Ident(ident_token!((2, 1), "Int")), case_binding: None, args: None },
                        vec![int_literal!((2, 8), 123)]
                    ),
                    (
                        MatchCase {
                            token: ident_token!((3, 1), "A"),
                            match_type: MatchCaseType::Compound(vec![ident_token!((3, 1), "A"), ident_token!((3, 3), "String")]),
                            case_binding: Some(ident_token!((3, 10), "a")),
                            args: None,
                        },
                        vec![identifier!((3, 15), "a")]
                    ),
                    (
                        MatchCase {
                            token: Token::LParen(Position::new(4, 4), false),
                            match_type: MatchCaseType::Ident(ident_token!((4, 1), "Abc")),
                            case_binding: Some(ident_token!((4, 11), "abc")),
                            args: Some(vec![
                                BindingPattern::Variable(ident_token!((4, 5), "a")),
                                BindingPattern::Variable(ident_token!((4, 8), "b")),
                            ]),
                        },
                        vec![identifier!((4, 18), "a")]
                    ),
                    (
                        MatchCase {
                            token: Token::LParen(Position::new(5, 6), false),
                            match_type: MatchCaseType::Compound(vec![ident_token!((5, 1), "A"), ident_token!((5, 3), "Bcd")]),
                            case_binding: None,
                            args: Some(vec![
                                BindingPattern::Variable(ident_token!((5, 7), "a")),
                            ]),
                        },
                        vec![identifier!((5, 13), "a")]
                    ),
                    (
                        MatchCase { token: ident_token!((6, 1), "_"), match_type: MatchCaseType::Wildcard(ident_token!((6, 1), "_")), case_binding: None, args: None },
                        vec![int_literal!((6, 6), 0)]
                    ),
                    (
                        MatchCase { token: ident_token!((7, 1), "_"), match_type: MatchCaseType::Wildcard(ident_token!((7, 1), "_")), case_binding: Some(ident_token!((7, 3), "x")), args: None },
                        vec![int_literal!((8, 1), 0), identifier!((9, 1), "x")]
                    )
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_match_statement_errors() {
        let error = parse("match {}").unwrap_err();
        let expected = ParseError::UnexpectedEof;
        assert_eq!(expected, error);

        let error = parse("match a }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::LBrace, Token::RBrace(Position::new(1, 9)));
        assert_eq!(expected, error);

        let error = parse("match a {}").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::RBrace(Position::new(1, 10)));
        assert_eq!(expected, error);

        let error = parse("match a { 123 => 456 }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::Int(Position::new(1, 11), 123));
        assert_eq!(expected, error);

        let error = parse("match a { Int 123 }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Arrow, Token::Int(Position::new(1, 15), 123));
        assert_eq!(expected, error);

        let error = parse("match a { Int i x => 123 }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Arrow, ident_token!((1, 17), "x"));
        assert_eq!(expected, error);

        let error = parse("match a { Int() x => 123 }").unwrap_err();
        let expected = ParseError::ExpectedToken(TokenType::Ident, Token::RParen(Position::new(1, 15)));
        assert_eq!(expected, error);

        let error = parse("match a { Int(123) x => 123 }").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Int(Position::new(1, 15), 123));
        assert_eq!(expected, error);
    }

    #[test]
    fn parse_return_statement() -> TestResult {
        let ast = parse("func abc() { return }")?;
        let body = if let AstNode::FunctionDecl(_, FunctionDeclNode { body, .. }) = &ast[0] { body } else { unreachable!() };
        let expected = vec![
            AstNode::ReturnStatement(
                Token::Return(Position::new(1, 14), false),
                None,
            )
        ];
        assert_eq!(&expected, body);

        let ast = parse("\
          func abc() {\n\
            return\n\
          }\
        ")?;
        let body = if let AstNode::FunctionDecl(_, FunctionDeclNode { body, .. }) = &ast[0] { body } else { unreachable!() };
        let expected = vec![
            AstNode::ReturnStatement(
                Token::Return(Position::new(2, 1), true),
                None,
            )
        ];
        assert_eq!(&expected, body);

        let ast = parse("\
          func abc() {\n\
            return   }\
        ")?;
        let body = if let AstNode::FunctionDecl(_, FunctionDeclNode { body, .. }) = &ast[0] { body } else { unreachable!() };
        let expected = vec![
            AstNode::ReturnStatement(
                Token::Return(Position::new(2, 1), false),
                None,
            )
        ];
        assert_eq!(&expected, body);

        let ast = parse("func abc() { return 123 }")?;
        let body = if let AstNode::FunctionDecl(_, FunctionDeclNode { body, .. }) = &ast[0] { body } else { unreachable!() };
        let expected = vec![
            AstNode::ReturnStatement(
                Token::Return(Position::new(1, 14), false),
                Some(Box::new(int_literal!((1, 21), 123))),
            )
        ];
        assert_eq!(&expected, body);
        let ast = parse("\
          func abc() {\n\
            return \n\
            123 }\
        ")?;
        let body = if let AstNode::FunctionDecl(_, FunctionDeclNode { body, .. }) = &ast[0] { body } else { unreachable!() };
        let expected = vec![
            AstNode::ReturnStatement(
                Token::Return(Position::new(2, 1), true),
                None,
            ),
            int_literal!((3, 1), 123)
        ];
        assert_eq!(&expected, body);

        Ok(())
    }

    #[test]
    fn parse_match_return_statement_errors() {
        let error = parse("return func abc() {}").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Func(Position::new(1, 8)));
        assert_eq!(expected, error);

        let error = parse("return val a = 3").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Val(Position::new(1, 8)));
        assert_eq!(expected, error);

        let error = parse("return var a").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Var(Position::new(1, 8)));
        assert_eq!(expected, error);

        let error = parse("return type Person { name: String }").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Type(Position::new(1, 8)));
        assert_eq!(expected, error);

        let error = parse("return enum Direction { Up, Down }").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Enum(Position::new(1, 8)));
        assert_eq!(expected, error);

        let error = parse("return while true {}").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::While(Position::new(1, 8)));
        assert_eq!(expected, error);

        let error = parse("return for x in range(0, 1) {}").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::For(Position::new(1, 8)));
        assert_eq!(expected, error);

        let error = parse("return break").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Break(Position::new(1, 8)));
        assert_eq!(expected, error);

        let error = parse("return return").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Return(Position::new(1, 8), true));
        assert_eq!(expected, error);
    }

    #[test]
    fn parse_import_statement() -> TestResult {
        let ast = parse("import * from abc.def")?;
        let expected = vec![
            AstNode::ImportStatement(
                Token::Import(Position::new(1, 1)),
                ImportNode {
                    imports: vec![],
                    star_token: Some(Token::Star(Position::new(1, 8))),
                    leading_dot_token: None,
                    path: vec![ident_token!((1, 15), "abc"), ident_token!((1, 19), "def")],
                },
            )
        ];
        assert_eq!(expected, ast);

        let ast = parse("import Date from time.date")?;
        let expected = vec![
            AstNode::ImportStatement(
                Token::Import(Position::new(1, 1)),
                ImportNode {
                    imports: vec![ident_token!((1, 8), "Date")],
                    star_token: None,
                    leading_dot_token: None,
                    path: vec![ident_token!((1, 18), "time"), ident_token!((1, 23), "date")],
                },
            )
        ];
        assert_eq!(expected, ast);

        let ast = parse("import SomeType, someFunc from .local.module")?;
        let expected = vec![
            AstNode::ImportStatement(
                Token::Import(Position::new(1, 1)),
                ImportNode {
                    imports: vec![
                        ident_token!((1, 8), "SomeType"),
                        ident_token!((1, 18), "someFunc"),
                    ],
                    star_token: None,
                    leading_dot_token: Some(Token::Dot(Position::new(1, 32))),
                    path: vec![ident_token!((1, 33), "local"), ident_token!((1, 39), "module")],
                },
            )
        ];
        assert_eq!(expected, ast);

        Ok(())
    }

    #[test]
    fn parse_exported_statements() -> TestResult {
        // Val binding
        let ast = parse("export val x = 14")?;
        let expected = AstNode::BindingDecl(
            Token::Val(Position::new(1, 8)),
            BindingDeclNode {
                export_token: Some(Token::Export(Position::new(1, 1))),
                binding: BindingPattern::Variable(ident_token!((1, 12), "x")),
                type_ann: None,
                expr: Some(Box::new(int_literal!((1, 16), 14))),
                is_mutable: false,
            },
        );
        assert_eq!(expected, ast[0]);

        // Var binding
        let ast = parse("export var x = 14")?;
        let expected = AstNode::BindingDecl(
            Token::Var(Position::new(1, 8)),
            BindingDeclNode {
                export_token: Some(Token::Export(Position::new(1, 1))),
                binding: BindingPattern::Variable(ident_token!((1, 12), "x")),
                type_ann: None,
                expr: Some(Box::new(int_literal!((1, 16), 14))),
                is_mutable: true,
            },
        );
        assert_eq!(expected, ast[0]);

        // Function declaration
        let ast = parse("export func abc() {}")?;
        let expected = AstNode::FunctionDecl(
            Token::Func(Position::new(1, 8)),
            FunctionDeclNode {
                export_token: Some(Token::Export(Position::new(1, 1))),
                name: ident_token!((1, 13), "abc"),
                type_args: vec![],
                args: vec![],
                ret_type: None,
                body: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        // Type declaration
        let ast = parse("export type Person {}")?;
        let expected = AstNode::TypeDecl(
            Token::Type(Position::new(1, 8)),
            TypeDeclNode {
                export_token: Some(Token::Export(Position::new(1, 1))),
                name: ident_token!((1, 13), "Person"),
                type_args: vec![],
                fields: vec![],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        // Enum declaration
        let ast = parse("export enum Direction {}")?;
        let expected = AstNode::EnumDecl(
            Token::Enum(Position::new(1, 8)),
            EnumDeclNode {
                export_token: Some(Token::Export(Position::new(1, 1))),
                name: ident_token!((1, 13), "Direction"),
                type_args: vec![],
                variants: vec![],
                methods: vec![],
            },
        );
        assert_eq!(expected, ast[0]);

        Ok(())
    }

    #[test]
    fn parse_exported_statements_errors() {
        let error = parse("export 123").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Int(Position::new(1, 8), 123));
        assert_eq!(expected, error);

        let error = parse("export if true {} else {}").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::If(Position::new(1, 8)));
        assert_eq!(expected, error);
    }

    #[test]
    fn parse_imports_errors() {
        let err = parse("\
          func abc() {}\n\
          import abc from def.ghi\
        ").unwrap_err();
        let expected = ParseError::UnexpectedToken(Token::Import(Position::new(2, 1)));
        assert_eq!(expected, err);
    }
}
