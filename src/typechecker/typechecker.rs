use crate::parser::ast::{AstNode, AstLiteralNode, UnaryNode, BinaryNode, BinaryOp};
use crate::common::visitor::AstVisitor;
use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode};
use crate::typechecker::typechecker_error::TypecheckerError;

pub struct Typechecker;

impl Typechecker {
    pub fn typecheck(&self, ast: Vec<AstNode>) -> Result<Vec<TypedAstNode>, TypecheckerError> {
        let results: Result<Vec<TypedAstNode>, TypecheckerError> = ast.into_iter()
            .map(|node| self.visit(node))
            .collect();
        results
    }
}

impl AstVisitor<TypedAstNode, TypecheckerError> for Typechecker {
    fn visit_literal(&self, token: Token, node: AstLiteralNode) -> Result<TypedAstNode, TypecheckerError> {
        match node {
            AstLiteralNode::IntLiteral(val) =>
                Ok(TypedAstNode::Literal(token, TypedLiteralNode::IntLiteral(val))),
            AstLiteralNode::FloatLiteral(val) =>
                Ok(TypedAstNode::Literal(token, TypedLiteralNode::FloatLiteral(val))),
        }
    }

    fn visit_unary(&self, token: Token, node: UnaryNode) -> Result<TypedAstNode, TypecheckerError> {
        let expr = *node.expr;
        let typed_expr = self.visit(expr)?;

        let node = TypedUnaryNode {
            typ: typed_expr.get_type(),
            op: node.op,
            expr: Box::new(typed_expr),
        };
        Ok(TypedAstNode::Unary(token, node))
    }

    fn visit_binary(&self, token: Token, node: BinaryNode) -> Result<TypedAstNode, TypecheckerError> {
        let left = *node.left;
        let left_token = left.get_token();
        let typed_left = self.visit(left)?;

        match node.op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                let ltype = typed_left.get_type();
                match ltype {
                    Type::Int | Type::Float => {
                        let right = *node.right;
                        let right_token = right.get_token();
                        let typed_right = self.visit(right)?;
                        let rtype = typed_right.get_type();
                        match rtype {
                            Type::Int | Type::Float => {
                                let typ = if let BinaryOp::Div = node.op {
                                    Type::Float
                                } else {
                                    match (ltype, rtype) {
                                        (Type::Int, Type::Int) => Type::Int,
                                        (Type::Float, _) => Type::Float,
                                        (_, Type::Float) => Type::Float,
                                        _ => unreachable!(), // TODO: Readdress this?
                                    }
                                };

                                let node = TypedBinaryNode {
                                    typ,
                                    left: Box::new(typed_left),
                                    op: node.op,
                                    right: Box::new(typed_right),
                                };
                                Ok(TypedAstNode::Binary(token, node))
                            }
                            _ => {
                                let expected = Type::Or(vec![Type::Int, Type::Float]);
                                Err(TypecheckerError::Mismatch { token: right_token, expected, actual: rtype })
                            }
                        }
                    }
                    _ => {
                        let expected = Type::Or(vec![Type::Int, Type::Float]);
                        Err(TypecheckerError::Mismatch { token: left_token, expected, actual: ltype })
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::lexer::tokens::Position;
    use crate::parser::ast::UnaryOp;

    type TestResult = Result<(), TypecheckerError>;

    fn typecheck(input: &str) -> Result<Vec<TypedAstNode>, TypecheckerError> {
        let tokens = tokenize(&input.to_string());
        let ast = parse(tokens).unwrap();

        let typechecker = Typechecker {};
        typechecker.typecheck(ast)
    }

    #[test]
    fn typecheck_literals() -> TestResult {
        let typed_ast = typecheck("1 2.34")?;
        let expected = vec![
            TypedAstNode::Literal(
                Token::Int(Position::new(1, 1), 1),
                TypedLiteralNode::IntLiteral(1),
            ),
            TypedAstNode::Literal(
                Token::Float(Position::new(1, 3), 2.34),
                TypedLiteralNode::FloatLiteral(2.34),
            )
        ];
        Ok(assert_eq!(expected, typed_ast))
    }

    #[test]
    fn typecheck_unary() -> TestResult {
        let typed_ast = typecheck("-1")?;
        let expected = vec![
            TypedAstNode::Unary(
                Token::Minus(Position::new(1, 1)),
                TypedUnaryNode {
                    typ: Type::Int,
                    op: UnaryOp::Minus,
                    expr: Box::new(
                        TypedAstNode::Literal(
                            Token::Int(Position::new(1, 2), 1),
                            TypedLiteralNode::IntLiteral(1),
                        ),
                    ),
                },
            ),
        ];
        assert_eq!(expected, typed_ast);

        let typed_ast = typecheck("-2.34")?;
        let expected = vec![
            TypedAstNode::Unary(
                Token::Minus(Position::new(1, 1)),
                TypedUnaryNode {
                    typ: Type::Float,
                    op: UnaryOp::Minus,
                    expr: Box::new(
                        TypedAstNode::Literal(
                            Token::Float(Position::new(1, 2), 2.34),
                            TypedLiteralNode::FloatLiteral(2.34),
                        ),
                    ),
                },
            ),
        ];
        Ok(assert_eq!(expected, typed_ast))
    }

    #[test]
    fn typecheck_binary_arithmetic() -> TestResult {
        let typed_ast = typecheck("1 + 2")?;
        let expected = TypedAstNode::Binary(
            Token::Plus(Position::new(1, 3)),
            TypedBinaryNode {
                typ: Type::Int,
                left: Box::new(
                    TypedAstNode::Literal(
                        Token::Int(Position::new(1, 1), 1),
                        TypedLiteralNode::IntLiteral(1),
                    ),
                ),
                op: BinaryOp::Add,
                right: Box::new(
                    TypedAstNode::Literal(
                        Token::Int(Position::new(1, 5), 2),
                        TypedLiteralNode::IntLiteral(2),
                    ),
                ),
            },
        );
        Ok(assert_eq!(expected, typed_ast[0]))
    }

    #[test]
    fn typecheck_binary_arithmetic_nested() -> TestResult {
        let typed_ast = typecheck("1 + 2.3 - -4.5")?;
        let expected = TypedAstNode::Binary(
            Token::Minus(Position::new(1, 9)),
            TypedBinaryNode {
                typ: Type::Float,
                left: Box::new(
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(1, 3)),
                        TypedBinaryNode {
                            typ: Type::Float,
                            left: Box::new(
                                TypedAstNode::Literal(
                                    Token::Int(Position::new(1, 1), 1),
                                    TypedLiteralNode::IntLiteral(1),
                                ),
                            ),
                            op: BinaryOp::Add,
                            right: Box::new(
                                TypedAstNode::Literal(
                                    Token::Float(Position::new(1, 5), 2.3),
                                    TypedLiteralNode::FloatLiteral(2.3),
                                ),
                            ),
                        },
                    )
                ),
                op: BinaryOp::Sub,
                right: Box::new(
                    TypedAstNode::Unary(
                        Token::Minus(Position::new(1, 11)),
                        TypedUnaryNode {
                            typ: Type::Float,
                            op: UnaryOp::Minus,
                            expr: Box::new(
                                TypedAstNode::Literal(
                                    Token::Float(Position::new(1, 12), 4.5),
                                    TypedLiteralNode::FloatLiteral(4.5),
                                ),
                            ),
                        },
                    )
                ),
            },
        );
        Ok(assert_eq!(expected, typed_ast[0]))
    }

    #[test]
    #[ignore]
    fn typecheck_binary_arithmetic_failure() -> TestResult {
        // TODO: Add when there are non-numeric types
        unimplemented!()
    }
}
