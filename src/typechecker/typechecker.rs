use crate::parser::ast::{AstNode, AstLiteralNode, UnaryNode, BinaryNode, BinaryOp};
use crate::common::ast_visitor::AstVisitor;
use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode};
use crate::typechecker::typechecker_error::TypecheckerError;

pub struct Typechecker;

pub fn typecheck(ast: Vec<AstNode>) -> Result<Vec<TypedAstNode>, TypecheckerError> {
    let typechecker = Typechecker {};

    let results: Result<Vec<TypedAstNode>, TypecheckerError> = ast.into_iter()
        .map(|node| typechecker.visit(node))
        .collect();
    results
}

impl AstVisitor<TypedAstNode, TypecheckerError> for Typechecker {
    fn visit_literal(&self, token: Token, node: AstLiteralNode) -> Result<TypedAstNode, TypecheckerError> {
        match node {
            AstLiteralNode::IntLiteral(val) =>
                Ok(TypedAstNode::Literal(token, TypedLiteralNode::IntLiteral(val))),
            AstLiteralNode::FloatLiteral(val) =>
                Ok(TypedAstNode::Literal(token, TypedLiteralNode::FloatLiteral(val))),
            AstLiteralNode::StringLiteral(val) =>
                Ok(TypedAstNode::Literal(token, TypedLiteralNode::StringLiteral(val))),
            AstLiteralNode::BoolLiteral(val) =>
                Ok(TypedAstNode::Literal(token, TypedLiteralNode::BoolLiteral(val)))
        }
    }

    fn visit_unary(&self, token: Token, node: UnaryNode) -> Result<TypedAstNode, TypecheckerError> {
        let expr = *node.expr;
        let typed_expr = self.visit(expr)?;
        let expr_type = typed_expr.get_type();
        match expr_type {
            Type::Int | Type::Float => {
                let node = TypedUnaryNode {
                    typ: expr_type,
                    op: node.op,
                    expr: Box::new(typed_expr),
                };
                Ok(TypedAstNode::Unary(token, node))
            }
            _ => Err(TypecheckerError::Mismatch {
                token,
                expected: Type::Or(vec![Type::Int, Type::Float]),
                actual: expr_type,
            })
        }
    }

    fn visit_binary(&self, token: Token, node: BinaryNode) -> Result<TypedAstNode, TypecheckerError> {
        let left = *node.left;
        let typed_left = self.visit(left)?;
        let ltype = typed_left.get_type();

        let right = *node.right;
        let typed_right = self.visit(right)?;
        let rtype = typed_right.get_type();

        if ltype == Type::String || rtype == Type::String {
            return if node.op != BinaryOp::Add {
                Err(TypecheckerError::InvalidOperator { token, op: node.op, ltype, rtype })
            } else {
                Ok(TypedAstNode::Binary(token, TypedBinaryNode {
                    typ: Type::String,
                    left: Box::new(typed_left),
                    op: node.op,
                    right: Box::new(typed_right),
                }))
            };
        }

        let typ = match node.op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => {
                match (&ltype, &rtype) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                    (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: node.op.clone(), ltype, rtype })
                }
            }
            BinaryOp::Div => {
                match (&ltype, &rtype) {
                    (Type::Int, Type::Int) | (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                    (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: node.op.clone(), ltype, rtype })
                }
            }
            _ => unimplemented!()
        };

        Ok(TypedAstNode::Binary(token.clone(), TypedBinaryNode {
            typ: typ?,
            left: Box::new(typed_left),
            op: node.op,
            right: Box::new(typed_right),
        }))
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
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();

        super::typecheck(ast)
    }

    #[test]
    fn typecheck_literals() -> TestResult {
        let typed_ast = typecheck("1 2.34 \"hello\"")?;
        let expected = vec![
            TypedAstNode::Literal(
                Token::Int(Position::new(1, 1), 1),
                TypedLiteralNode::IntLiteral(1),
            ),
            TypedAstNode::Literal(
                Token::Float(Position::new(1, 3), 2.34),
                TypedLiteralNode::FloatLiteral(2.34),
            ),
            TypedAstNode::Literal(
                Token::String(Position::new(1, 8), "hello".to_string()),
                TypedLiteralNode::StringLiteral("hello".to_string()),
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
    fn typecheck_unary_failure() -> TestResult {
        let err = typecheck("-\"bad\"").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Minus(Position::new(1, 1)),
            expected: Type::Or(vec![Type::Int, Type::Float]),
            actual: Type::String,
        };
        assert_eq!(expected, err);

        let err = typecheck("-false").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Minus(Position::new(1, 1)),
            expected: Type::Or(vec![Type::Int, Type::Float]),
            actual: Type::Bool,
        };
        Ok(assert_eq!(expected, err))
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
    fn typecheck_binary_str_concat() -> TestResult {
        let typed_ast = typecheck("\"hello \" + \"world\"")?;
        let expected = TypedAstNode::Binary(
            Token::Plus(Position::new(1, 10)),
            TypedBinaryNode {
                typ: Type::String,
                left: Box::new(
                    TypedAstNode::Literal(
                        Token::String(Position::new(1, 1), "hello ".to_string()),
                        TypedLiteralNode::StringLiteral("hello ".to_string()),
                    )
                ),
                op: BinaryOp::Add,
                right: Box::new(
                    TypedAstNode::Literal(
                        Token::String(Position::new(1, 12), "world".to_string()),
                        TypedLiteralNode::StringLiteral("world".to_string()),
                    )
                ),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("\"hello \" + 3")?;
        let expected = TypedAstNode::Binary(
            Token::Plus(Position::new(1, 10)),
            TypedBinaryNode {
                typ: Type::String,
                left: Box::new(
                    TypedAstNode::Literal(
                        Token::String(Position::new(1, 1), "hello ".to_string()),
                        TypedLiteralNode::StringLiteral("hello ".to_string()),
                    )
                ),
                op: BinaryOp::Add,
                right: Box::new(
                    TypedAstNode::Literal(Token::Int(Position::new(1, 12), 3), TypedLiteralNode::IntLiteral(3))
                ),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("3.14 + \"world\"")?;
        let expected = TypedAstNode::Binary(
            Token::Plus(Position::new(1, 6)),
            TypedBinaryNode {
                typ: Type::String,
                left: Box::new(
                    TypedAstNode::Literal(Token::Float(Position::new(1, 1), 3.14), TypedLiteralNode::FloatLiteral(3.14))
                ),
                op: BinaryOp::Add,
                right: Box::new(
                    TypedAstNode::Literal(
                        Token::String(Position::new(1, 8), "world".to_string()),
                        TypedLiteralNode::StringLiteral("world".to_string()),
                    )
                ),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("false + \" world\"")?;
        let expected = TypedAstNode::Binary(
            Token::Plus(Position::new(1, 7)),
            TypedBinaryNode {
                typ: Type::String,
                left: Box::new(
                    TypedAstNode::Literal(Token::Bool(Position::new(1, 1), false), TypedLiteralNode::BoolLiteral(false))
                ),
                op: BinaryOp::Add,
                right: Box::new(
                    TypedAstNode::Literal(
                        Token::String(Position::new(1, 9), " world".to_string()),
                        TypedLiteralNode::StringLiteral(" world".to_string()),
                    )
                ),
            },
        );
        Ok(assert_eq!(expected, typed_ast[0]))
    }

    #[test]
    fn typecheck_binary_arithmetic_failures() {
        let cases = vec![
            ("3 - \"str\"", Token::Minus(Position::new(1, 3)), BinaryOp::Sub, Type::Int, Type::String),
            ("3.2 - \"str\"", Token::Minus(Position::new(1, 5)), BinaryOp::Sub, Type::Float, Type::String),
            ("3 * \"str\"", Token::Star(Position::new(1, 3)), BinaryOp::Mul, Type::Int, Type::String),
            ("3.2 * \"str\"", Token::Star(Position::new(1, 5)), BinaryOp::Mul, Type::Float, Type::String),
            ("3 / \"str\"", Token::Slash(Position::new(1, 3)), BinaryOp::Div, Type::Int, Type::String),
            ("3.2 / \"str\"", Token::Slash(Position::new(1, 5)), BinaryOp::Div, Type::Float, Type::String),
            //
            ("\"str\" - 3", Token::Minus(Position::new(1, 7)), BinaryOp::Sub, Type::String, Type::Int),
            ("\"str\" - 3.2", Token::Minus(Position::new(1, 7)), BinaryOp::Sub, Type::String, Type::Float),
            ("\"str\" * 3", Token::Star(Position::new(1, 7)), BinaryOp::Mul, Type::String, Type::Int),
            ("\"str\" * 3.2", Token::Star(Position::new(1, 7)), BinaryOp::Mul, Type::String, Type::Float),
            ("\"str\" / 3", Token::Slash(Position::new(1, 7)), BinaryOp::Div, Type::String, Type::Int),
            ("\"str\" / 3.2", Token::Slash(Position::new(1, 7)), BinaryOp::Div, Type::String, Type::Float),
            //
            ("true + 1", Token::Plus(Position::new(1, 6)), BinaryOp::Add, Type::Bool, Type::Int),
            ("true + 1.0", Token::Plus(Position::new(1, 6)), BinaryOp::Add, Type::Bool, Type::Float),
            ("true + false", Token::Plus(Position::new(1, 6)), BinaryOp::Add, Type::Bool, Type::Bool),
            ("true - 1", Token::Minus(Position::new(1, 6)), BinaryOp::Sub, Type::Bool, Type::Int),
            ("true - 1.0", Token::Minus(Position::new(1, 6)), BinaryOp::Sub, Type::Bool, Type::Float),
            ("true - \"str\"", Token::Minus(Position::new(1, 6)), BinaryOp::Sub, Type::Bool, Type::String),
            ("true - false", Token::Minus(Position::new(1, 6)), BinaryOp::Sub, Type::Bool, Type::Bool),
            ("true * 1", Token::Star(Position::new(1, 6)), BinaryOp::Mul, Type::Bool, Type::Int),
            ("true * 1.0", Token::Star(Position::new(1, 6)), BinaryOp::Mul, Type::Bool, Type::Float),
            ("true * \"str\"", Token::Star(Position::new(1, 6)), BinaryOp::Mul, Type::Bool, Type::String),
            ("true * false", Token::Star(Position::new(1, 6)), BinaryOp::Mul, Type::Bool, Type::Bool),
            ("true / 1", Token::Slash(Position::new(1, 6)), BinaryOp::Div, Type::Bool, Type::Int),
            ("true / 1.0", Token::Slash(Position::new(1, 6)), BinaryOp::Div, Type::Bool, Type::Float),
            ("true / \"str\"", Token::Slash(Position::new(1, 6)), BinaryOp::Div, Type::Bool, Type::String),
            ("true / false", Token::Slash(Position::new(1, 6)), BinaryOp::Div, Type::Bool, Type::Bool),
        ];

        for (input, token, op, ltype, rtype) in cases {
            let expected = TypecheckerError::InvalidOperator { token, op, ltype, rtype };
            let msg = format!("Typechecking `{}` should result in the error {:?}", input, expected);

            let err = typecheck(input).expect_err(&*msg);
            assert_eq!(expected, err, "{}", msg);
        }
    }
}
