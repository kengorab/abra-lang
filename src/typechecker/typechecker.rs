use crate::parser::ast::{AstNode, AstLiteralNode, UnaryNode, BinaryNode, BinaryOp, UnaryOp, ArrayNode, BindingDeclNode};
use crate::common::ast_visitor::AstVisitor;
use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode};
use crate::typechecker::typechecker_error::TypecheckerError;
use std::collections::{HashSet, HashMap};
use std::iter::FromIterator;

pub(crate) struct Scope {
    //    parent: Option<Box<&'a Scope<'a>>>,
    pub(crate) bindings: HashMap<String, (Token, Option<Type>)>,
}

impl Scope {
    fn new() -> Self {
        Scope { /*parent: None, */bindings: HashMap::new() }
    }

//    fn child(&'a self) -> Self {
//        Scope { parent: Some(Box::new(self)), bindings: HashMap::new() }
//    }
}

pub struct Typechecker {
    pub(crate) scope: Scope
}

pub fn typecheck(ast: Vec<AstNode>) -> Result<(Typechecker, Vec<TypedAstNode>), TypecheckerError> {
    let scope = Scope::new();

    let mut typechecker = Typechecker { scope };

    let results: Result<Vec<TypedAstNode>, TypecheckerError> = ast.into_iter()
        .map(|node| typechecker.visit(node))
        .collect();
    Ok((typechecker, results?))
}

impl AstVisitor<TypedAstNode, TypecheckerError> for Typechecker {
    fn visit_literal(&mut self, token: Token, node: AstLiteralNode) -> Result<TypedAstNode, TypecheckerError> {
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

    fn visit_unary(&mut self, token: Token, node: UnaryNode) -> Result<TypedAstNode, TypecheckerError> {
        let expr = *node.expr;
        let typed_expr = self.visit(expr)?;
        let expr_type = typed_expr.get_type();
        match (&node.op, &expr_type) {
            (UnaryOp::Minus, Type::Int) | (UnaryOp::Minus, Type::Float) |
            (UnaryOp::Negate, Type::Bool) => {
                let node = TypedUnaryNode { typ: expr_type, op: node.op, expr: Box::new(typed_expr) };
                Ok(TypedAstNode::Unary(token, node))
            }
            (op @ UnaryOp::Minus, _) | (op @ UnaryOp::Negate, _) => {
                let expected = if op == &UnaryOp::Minus {
                    Type::Or(vec![Type::Int, Type::Float])
                } else {
                    Type::Bool
                };
                Err(TypecheckerError::Mismatch { token, expected, actual: expr_type })
            }
        }
    }

    fn visit_binary(&mut self, token: Token, node: BinaryNode) -> Result<TypedAstNode, TypecheckerError> {
        let left = *node.left;
        let typed_left = self.visit(left)?;
        let ltype = typed_left.get_type();

        let right = *node.right;
        let typed_right = self.visit(right)?;
        let rtype = typed_right.get_type();

        let typ = match node.op {
            BinaryOp::Add =>
                match (&ltype, &rtype) {
                    (Type::String, _) | (_, Type::String) => Ok(Type::String),
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                    (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: node.op.clone(), ltype, rtype })
                }
            BinaryOp::Sub | BinaryOp::Mul =>
                match (&ltype, &rtype) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                    (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: node.op.clone(), ltype, rtype })
                }
            BinaryOp::Div =>
                match (&ltype, &rtype) {
                    (Type::Int, Type::Int) | (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                    (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: node.op.clone(), ltype, rtype })
                }
            BinaryOp::And | BinaryOp::Or =>
                match (&ltype, &rtype) {
                    (Type::Bool, Type::Bool) => Ok(Type::Bool),
                    (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: node.op.clone(), ltype, rtype })
                }
            BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte =>
                match (&ltype, &rtype) {
                    (Type::String, Type::String) => Ok(Type::Bool),
                    (Type::Int, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) | (Type::Float, Type::Int) => Ok(Type::Bool),
                    (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: node.op.clone(), ltype, rtype })
                }
            BinaryOp::Neq | BinaryOp::Eq => Ok(Type::Bool),
        };

        Ok(TypedAstNode::Binary(token.clone(), TypedBinaryNode {
            typ: typ?,
            left: Box::new(typed_left),
            op: node.op,
            right: Box::new(typed_right),
        }))
    }

    fn visit_array(&mut self, token: Token, node: ArrayNode) -> Result<TypedAstNode, TypecheckerError> {
        let items: Result<Vec<TypedAstNode>, TypecheckerError> = node.items.into_iter()
            .map(|n| self.visit(*n))
            .collect();
        let items = items?;

        let item_types: HashSet<Type> = HashSet::from_iter(
            items.iter().map(|node| node.get_type())
        );
        let typ = if item_types.len() == 1 {
            let typ = item_types.into_iter()
                .nth(0)
                .expect("We know the size is 1");
            Some(Box::new(typ))
        } else if !item_types.is_empty() {
            Some(Box::new(Type::Or(item_types.into_iter().collect())))
        } else {
            None
        };

        let items = items.into_iter()
            .map(Box::new)
            .collect();

        Ok(TypedAstNode::Array(token.clone(), TypedArrayNode { typ: Type::Array(typ), items }))
    }

    fn visit_binding_decl(&mut self, token: Token, node: BindingDeclNode) -> Result<TypedAstNode, TypecheckerError> {
        let BindingDeclNode { is_mutable, ident, expr } = node;

        if !is_mutable && expr == None {
            return Err(TypecheckerError::MissingRequiredAssignment { ident });
        }

        let name = match &ident {
            Token::Ident(_, ident_name) => ident_name,
            _ => unreachable!()
        };

        if let Some((orig_ident, _)) = self.scope.bindings.get(name) {
            let orig_ident = orig_ident.clone();
            return Err(TypecheckerError::DuplicateBinding { ident, orig_ident });
        }

        let typed_expr = match expr {
            Some(e) => Some(self.visit(*e)?),
            None => None
        };
        let typ = match &typed_expr {
            Some(e) => Some(e.get_type()),
            None => None
        };

        self.scope.bindings.insert(name.clone(), (ident.clone(), typ));

        let node = TypedBindingDeclNode {
            is_mutable,
            ident,
            expr: typed_expr.map(Box::new),
        };
        Ok(TypedAstNode::BindingDecl(token, node))
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

        let (_, nodes) = super::typecheck(ast)?;
        Ok(nodes)
    }

    fn typecheck_get_typechecker(input: &str) -> (Typechecker, Vec<TypedAstNode>) {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();

        super::typecheck(ast).unwrap()
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
        assert_eq!(expected, typed_ast);

        let typed_ast = typecheck("!true")?;
        let expected = vec![
            TypedAstNode::Unary(
                Token::Bang(Position::new(1, 1)),
                TypedUnaryNode {
                    typ: Type::Bool,
                    op: UnaryOp::Negate,
                    expr: Box::new(
                        TypedAstNode::Literal(
                            Token::Bool(Position::new(1, 2), true),
                            TypedLiteralNode::BoolLiteral(true),
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
        assert_eq!(expected, err);

        let err = typecheck("!4.5").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Bang(Position::new(1, 1)),
            expected: Type::Bool,
            actual: Type::Float,
        };
        assert_eq!(expected, err);

        let err = typecheck("!\"abc\"").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Bang(Position::new(1, 1)),
            expected: Type::Bool,
            actual: Type::String,
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

    #[test]
    fn typecheck_binary_boolean() -> TestResult {
        let typed_ast = typecheck("true && true || false")?;
        let expected = TypedAstNode::Binary(
            Token::Or(Position::new(1, 14)),
            TypedBinaryNode {
                typ: Type::Bool,
                left: Box::new(
                    TypedAstNode::Binary(
                        Token::And(Position::new(1, 6)),
                        TypedBinaryNode {
                            typ: Type::Bool,
                            left: Box::new(
                                TypedAstNode::Literal(
                                    Token::Bool(Position::new(1, 1), true),
                                    TypedLiteralNode::BoolLiteral(true))
                            ),
                            op: BinaryOp::And,
                            right: Box::new(
                                TypedAstNode::Literal(
                                    Token::Bool(Position::new(1, 9), true),
                                    TypedLiteralNode::BoolLiteral(true))
                            ),
                        },
                    ),
                ),
                op: BinaryOp::Or,
                right: Box::new(
                    TypedAstNode::Literal(
                        Token::Bool(Position::new(1, 17), false),
                        TypedLiteralNode::BoolLiteral(false),
                    ),
                ),
            },
        );
        Ok(assert_eq!(expected, typed_ast[0]))
    }

    #[test]
    fn typecheck_binary_boolean_errors() {
        let cases = vec![
            ("true && 1", Token::And(Position::new(1, 6)), BinaryOp::And, Type::Bool, Type::Int),
            ("true && 3.14", Token::And(Position::new(1, 6)), BinaryOp::And, Type::Bool, Type::Float),
            ("true && \"str\"", Token::And(Position::new(1, 6)), BinaryOp::And, Type::Bool, Type::String),
            ("false && 1", Token::And(Position::new(1, 7)), BinaryOp::And, Type::Bool, Type::Int),
            ("false && 3.14", Token::And(Position::new(1, 7)), BinaryOp::And, Type::Bool, Type::Float),
            ("false && \"str\"", Token::And(Position::new(1, 7)), BinaryOp::And, Type::Bool, Type::String),
            //
            ("true || 1", Token::Or(Position::new(1, 6)), BinaryOp::Or, Type::Bool, Type::Int),
            ("true || 3.14", Token::Or(Position::new(1, 6)), BinaryOp::Or, Type::Bool, Type::Float),
            ("true || \"str\"", Token::Or(Position::new(1, 6)), BinaryOp::Or, Type::Bool, Type::String),
            ("false || 1", Token::Or(Position::new(1, 7)), BinaryOp::Or, Type::Bool, Type::Int),
            ("false || 3.14", Token::Or(Position::new(1, 7)), BinaryOp::Or, Type::Bool, Type::Float),
            ("false || \"str\"", Token::Or(Position::new(1, 7)), BinaryOp::Or, Type::Bool, Type::String),
        ];

        for (input, token, op, ltype, rtype) in cases {
            let expected = TypecheckerError::InvalidOperator { token, op, ltype, rtype };
            let msg = format!("Typechecking `{}` should result in the error {:?}", input, expected);

            let err = typecheck(input).expect_err(&*msg);
            assert_eq!(expected, err, "{}", msg);
        }
    }

    #[test]
    fn typecheck_binary_comparisons() -> TestResult {
        let cases: Vec<(&str, Box<Fn(Position) -> Token>, BinaryOp)> = vec![
            ("1 <  2", Box::new(Token::LT), BinaryOp::Lt),
            ("1 <= 2", Box::new(Token::LTE), BinaryOp::Lte),
            ("1 >  2", Box::new(Token::GT), BinaryOp::Gt),
            ("1 >= 2", Box::new(Token::GTE), BinaryOp::Gte),
            ("1 != 2", Box::new(Token::Neq), BinaryOp::Neq),
            ("1 == 2", Box::new(Token::Eq), BinaryOp::Eq),
        ];
        for (input, token, op) in cases {
            let typed_ast = typecheck(input)?;
            let expected = TypedAstNode::Binary(
                token(Position::new(1, 3)),
                TypedBinaryNode {
                    typ: Type::Bool,
                    left: Box::new(
                        TypedAstNode::Literal(
                            Token::Int(Position::new(1, 1), 1),
                            TypedLiteralNode::IntLiteral(1))
                    ),
                    op,
                    right: Box::new(
                        TypedAstNode::Literal(
                            Token::Int(Position::new(1, 6), 2),
                            TypedLiteralNode::IntLiteral(2))
                    ),
                },
            );
            assert_eq!(expected, typed_ast[0]);
        };

        let cases: Vec<(&str, Box<Fn(Position) -> Token>, BinaryOp)> = vec![
            ("\"abc\" <  \"def\"", Box::new(Token::LT), BinaryOp::Lt),
            ("\"abc\" <= \"def\"", Box::new(Token::LTE), BinaryOp::Lte),
            ("\"abc\" >  \"def\"", Box::new(Token::GT), BinaryOp::Gt),
            ("\"abc\" >= \"def\"", Box::new(Token::GTE), BinaryOp::Gte),
            ("\"abc\" == \"def\"", Box::new(Token::Eq), BinaryOp::Eq),
            ("\"abc\" != \"def\"", Box::new(Token::Neq), BinaryOp::Neq),
        ];
        for (input, token, op) in cases {
            let typed_ast = typecheck(input)?;
            let expected = TypedAstNode::Binary(
                token(Position::new(1, 7)),
                TypedBinaryNode {
                    typ: Type::Bool,
                    left: Box::new(
                        TypedAstNode::Literal(
                            Token::String(Position::new(1, 1), "abc".to_string()),
                            TypedLiteralNode::StringLiteral("abc".to_string()))
                    ),
                    op,
                    right: Box::new(
                        TypedAstNode::Literal(
                            Token::String(Position::new(1, 10), "def".to_string()),
                            TypedLiteralNode::StringLiteral("def".to_string()))
                    ),
                },
            );
            assert_eq!(expected, typed_ast[0]);
        }

        let cases: Vec<(&str, Box<Fn(Position) -> Token>, BinaryOp)> = vec![
            ("\"abc\" == 3", Box::new(Token::Eq), BinaryOp::Eq),
            ("\"abc\" != 3", Box::new(Token::Neq), BinaryOp::Neq),
        ];
        for (input, token, op) in cases {
            let typed_ast = typecheck(input)?;
            let expected = TypedAstNode::Binary(
                token(Position::new(1, 7)),
                TypedBinaryNode {
                    typ: Type::Bool,
                    left: Box::new(
                        TypedAstNode::Literal(
                            Token::String(Position::new(1, 1), "abc".to_string()),
                            TypedLiteralNode::StringLiteral("abc".to_string()))
                    ),
                    op,
                    right: Box::new(
                        TypedAstNode::Literal(
                            Token::Int(Position::new(1, 10), 3),
                            TypedLiteralNode::IntLiteral(3))
                    ),
                },
            );
            assert_eq!(expected, typed_ast[0]);
        }

        Ok(())
    }

    #[test]
    fn typecheck_binary_comparison_errors() {
        let cases = vec![
            ("\"str\" <  3", Token::LT(Position::new(1, 7)), BinaryOp::Lt, Type::String, Type::Int),
            ("\"str\" <  3.0", Token::LT(Position::new(1, 7)), BinaryOp::Lt, Type::String, Type::Float),
            ("\"str\" <= 3", Token::LTE(Position::new(1, 7)), BinaryOp::Lte, Type::String, Type::Int),
            ("\"str\" <= 3.0", Token::LTE(Position::new(1, 7)), BinaryOp::Lte, Type::String, Type::Float),
            ("\"str\" >  3", Token::GT(Position::new(1, 7)), BinaryOp::Gt, Type::String, Type::Int),
            ("\"str\" >  3.0", Token::GT(Position::new(1, 7)), BinaryOp::Gt, Type::String, Type::Float),
            ("\"str\" >= 3", Token::GTE(Position::new(1, 7)), BinaryOp::Gte, Type::String, Type::Int),
            ("\"str\" >= 3.0", Token::GTE(Position::new(1, 7)), BinaryOp::Gte, Type::String, Type::Float),
            //
            ("[1, 2] < 3", Token::LT(Position::new(1, 8)), BinaryOp::Lt, Type::Array(Some(Box::new(Type::Int))), Type::Int),
            ("[1, 2] <= 3", Token::LTE(Position::new(1, 8)), BinaryOp::Lte, Type::Array(Some(Box::new(Type::Int))), Type::Int),
            ("[1, 2] > 3", Token::GT(Position::new(1, 8)), BinaryOp::Gt, Type::Array(Some(Box::new(Type::Int))), Type::Int),
            ("[1, 2] >= 3", Token::GTE(Position::new(1, 8)), BinaryOp::Gte, Type::Array(Some(Box::new(Type::Int))), Type::Int),
        ];

        for (input, token, op, ltype, rtype) in cases {
            let expected = TypecheckerError::InvalidOperator { token, op, ltype, rtype };
            let msg = format!("Typechecking `{}` should result in the error {:?}", input, expected);

            let err = typecheck(input).expect_err(&*msg);
            assert_eq!(expected, err, "{}", msg);
        }
    }

    #[test]
    fn typecheck_array_empty() -> TestResult {
        let typed_ast = typecheck("[]")?;
        let expected = TypedAstNode::Array(
            Token::LBrack(Position::new(1, 1)),
            TypedArrayNode { typ: Type::Array(None), items: vec![] },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_array_homogeneous() -> TestResult {
        let typed_ast = typecheck("[1, 2, 3]")?;
        let expected = TypedAstNode::Array(
            Token::LBrack(Position::new(1, 1)),
            TypedArrayNode {
                typ: Type::Array(Some(Box::new(Type::Int))),
                items: vec![
                    Box::new(
                        TypedAstNode::Literal(
                            Token::Int(Position::new(1, 2), 1),
                            TypedLiteralNode::IntLiteral(1))
                    ),
                    Box::new(
                        TypedAstNode::Literal(
                            Token::Int(Position::new(1, 5), 2),
                            TypedLiteralNode::IntLiteral(2))
                    ),
                    Box::new(
                        TypedAstNode::Literal(
                            Token::Int(Position::new(1, 8), 3),
                            TypedLiteralNode::IntLiteral(3))
                    )
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("[\"a\", \"b\"]")?;
        let expected = TypedAstNode::Array(
            Token::LBrack(Position::new(1, 1)),
            TypedArrayNode {
                typ: Type::Array(Some(Box::new(Type::String))),
                items: vec![
                    Box::new(
                        TypedAstNode::Literal(
                            Token::String(Position::new(1, 2), "a".to_string()),
                            TypedLiteralNode::StringLiteral("a".to_string()))
                    ),
                    Box::new(
                        TypedAstNode::Literal(
                            Token::String(Position::new(1, 7), "b".to_string()),
                            TypedLiteralNode::StringLiteral("b".to_string()))
                    )
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("[true, false]")?;
        let expected = TypedAstNode::Array(
            Token::LBrack(Position::new(1, 1)),
            TypedArrayNode {
                typ: Type::Array(Some(Box::new(Type::Bool))),
                items: vec![
                    Box::new(
                        TypedAstNode::Literal(
                            Token::Bool(Position::new(1, 2), true),
                            TypedLiteralNode::BoolLiteral(true))
                    ),
                    Box::new(
                        TypedAstNode::Literal(
                            Token::Bool(Position::new(1, 8), false),
                            TypedLiteralNode::BoolLiteral(false))
                    )
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_array_nested() -> TestResult {
        let typed_ast = typecheck("[[1, 2], [3, 4]]")?;
        let expected_type = Type::Array(Some(Box::new(Type::Array(Some(Box::new(Type::Int))))));
        assert_eq!(expected_type, typed_ast[0].get_type());

        // TODO: Handle edge cases, like [[1, 2.3], [3.4, 5]]

        Ok(())
    }

    #[test]
    fn typecheck_binding_decl() -> TestResult {
        let (typechecker, typed_ast) = typecheck_get_typechecker("val abc = 123");
        let expected = TypedAstNode::BindingDecl(
            Token::Val(Position::new(1, 1)),
            TypedBindingDeclNode {
                is_mutable: false,
                ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                expr: Some(Box::new(
                    TypedAstNode::Literal(
                        Token::Int(Position::new(1, 11), 123),
                        TypedLiteralNode::IntLiteral(123),
                    )
                )),
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let binding = typechecker.scope.bindings.get("abc").unwrap();
        let expected_binding = (
            Token::Ident(Position::new(1, 5), "abc".to_string()),
            Some(Type::Int)
        );
        assert_eq!(&expected_binding, binding);

        let (typechecker, typed_ast) = typecheck_get_typechecker("var abc");
        let expected = TypedAstNode::BindingDecl(
            Token::Var(Position::new(1, 1)),
            TypedBindingDeclNode {
                is_mutable: true,
                ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                expr: None,
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let binding = typechecker.scope.bindings.get("abc").unwrap();
        let expected_binding = (
            Token::Ident(Position::new(1, 5), "abc".to_string()),
            None
        );
        assert_eq!(&expected_binding, binding);

        Ok(())
    }

    #[test]
    fn typecheck_binding_decl_errors() {
        let err = typecheck("val abc").unwrap_err();
        let expected = TypecheckerError::MissingRequiredAssignment {
            ident: Token::Ident(Position::new(1, 5), "abc".to_string())
        };
        assert_eq!(expected, err);

        let err = typecheck("val abc = 1\nval abc = 2").unwrap_err();
        let expected = TypecheckerError::DuplicateBinding {
            ident: Token::Ident(Position::new(2, 5), "abc".to_string()),
            orig_ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
        };
        assert_eq!(expected, err);
    }
}
