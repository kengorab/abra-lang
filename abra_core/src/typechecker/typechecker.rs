use crate::parser::ast::{AstNode, AstLiteralNode, UnaryNode, BinaryNode, BinaryOp, UnaryOp, ArrayNode, BindingDeclNode, AssignmentNode, IndexingNode, IndexingMode};
use crate::common::ast_visitor::AstVisitor;
use crate::lexer::tokens::Token;
use crate::typechecker::types::Type;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode};
use crate::typechecker::typechecker_error::TypecheckerError;
use std::collections::{HashSet, HashMap};
use std::iter::FromIterator;

#[derive(Debug, PartialEq)]
pub(crate) struct ScopeBinding(Token, Type, bool);

pub(crate) struct Scope {
    //    parent: Option<Box<&'a Scope<'a>>>,
    pub(crate) bindings: HashMap<String, ScopeBinding>,
    pub(crate) types: HashMap<String, Type>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            /*parent: None, */
            bindings: HashMap::new(),
            types: {
                let mut types = HashMap::new();
                types.insert("Int".to_string(), Type::Int);
                types.insert("Float".to_string(), Type::Float);
                types.insert("Bool".to_string(), Type::Bool);
                types.insert("String".to_string(), Type::String);
                types
            },
        }
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
        #[inline]
        fn type_for_op(token: &Token, op: &BinaryOp, ltype: Type, rtype: Type) -> Result<Type, TypecheckerError> {
            match op {
                BinaryOp::Add =>
                    match (&ltype, &rtype) {
                        (Type::String, _) | (_, Type::String) => Ok(Type::String),
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Sub | BinaryOp::Mul =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Div =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) | (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::And | BinaryOp::Or =>
                    match (&ltype, &rtype) {
                        (Type::Bool, Type::Bool) => Ok(Type::Bool),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte =>
                    match (&ltype, &rtype) {
                        (Type::String, Type::String) => Ok(Type::Bool),
                        (Type::Int, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) | (Type::Float, Type::Int) => Ok(Type::Bool),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Neq | BinaryOp::Eq => Ok(Type::Bool),
                _ => unimplemented!()
            }
        }

        let left = *node.left;
        let typed_left = self.visit(left)?;
        let ltype = typed_left.get_type();

        let right = *node.right;
        let typed_right = self.visit(right)?;
        let rtype = typed_right.get_type();

        let typ = match (ltype, rtype) {
            (Type::Option(ltype), Type::Option(rtype)) => {
                // Catch inner errors and rethrow, ensuring types contained in error are wrapped in Type::Option
                let inner_type = type_for_op(&token, &node.op, *ltype, *rtype).map_err(|err| match err {
                    TypecheckerError::InvalidOperator { token, op, ltype, rtype } =>
                        TypecheckerError::InvalidOperator {
                            token,
                            op,
                            ltype: Type::Option(Box::new(ltype)),
                            rtype: Type::Option(Box::new(rtype)),
                        },
                    _ => unreachable!()
                })?;
                Ok(Type::Option(Box::new(inner_type)))
            }
            (ltype @ _, rtype @ _) => type_for_op(&token, &node.op, ltype, rtype),
        }?;

        Ok(TypedAstNode::Binary(token.clone(), TypedBinaryNode {
            typ,
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
            item_types.into_iter()
                .nth(0)
                .expect("We know the size is 1")
        } else if !item_types.is_empty() {
            Type::Or(item_types.into_iter().collect())
        } else {
            Type::Any
        };

        let items = items.into_iter()
            .map(Box::new)
            .collect();

        Ok(TypedAstNode::Array(token.clone(), TypedArrayNode { typ: Type::Array(Box::new(typ)), items }))
    }

    fn visit_binding_decl(&mut self, token: Token, node: BindingDeclNode) -> Result<TypedAstNode, TypecheckerError> {
        let BindingDeclNode { is_mutable, ident, type_ann, expr } = node;

        if !is_mutable && expr == None {
            return Err(TypecheckerError::MissingRequiredAssignment { ident });
        }

        let name = Token::get_ident_name(&ident);

        if let Some(ScopeBinding(orig_ident, _, _)) = self.scope.bindings.get(name) {
            let orig_ident = orig_ident.clone();
            return Err(TypecheckerError::DuplicateBinding { ident, orig_ident });
        }

        let typed_expr = match expr {
            Some(e) => Some(self.visit(*e)?),
            None => None
        };

        let typ = match (&typed_expr, &type_ann) {
            (Some(e), None) => Ok(e.get_type()),
            (typed_expr @ _, Some(ann)) => {
                let typ = Type::from_type_ident(ann, &self.scope.types)
                    .ok_or(TypecheckerError::UnknownType { type_ident: ann.get_ident() })?;

                match typed_expr {
                    None => Ok(typ),
                    Some(e) => {
                        if typ == e.get_type() {
                            Ok(e.get_type())
                        } else {
                            Err(TypecheckerError::Mismatch {
                                token: e.get_token().clone(),
                                expected: typ.clone(),
                                actual: e.get_type(),
                            })
                        }
                    }
                }
            }
            (None, None) => Err(TypecheckerError::UnannotatedUninitialized {
                ident: ident.clone(),
                is_mutable,
            })
        }?;

        self.scope.bindings.insert(name.clone(), ScopeBinding(ident.clone(), typ, is_mutable));

        let node = TypedBindingDeclNode {
            is_mutable,
            ident,
            expr: typed_expr.map(Box::new),
        };
        Ok(TypedAstNode::BindingDecl(token, node))
    }

    fn visit_ident(&mut self, token: Token) -> Result<TypedAstNode, TypecheckerError> {
        let name = Token::get_ident_name(&token);

        match self.scope.bindings.get(name) {
            None => Err(TypecheckerError::UnknownIdentifier { ident: token }),
            Some(ScopeBinding(_, typ, is_mutable)) => Ok(TypedAstNode::Identifier(token, typ.clone(), is_mutable.clone())),
        }
    }

    fn visit_assignment(&mut self, token: Token, node: AssignmentNode) -> Result<TypedAstNode, TypecheckerError> {
        let AssignmentNode { target, expr } = node;
        if let AstNode::Identifier(ident_tok) = *target {
            let ident = self.visit_ident(ident_tok.clone())?;
            let (typ, is_mutable) = match &ident {
                TypedAstNode::Identifier(_, typ, is_mutable) => (typ, is_mutable),
                _ => unreachable!()
            };
            if !is_mutable {
                let name = Token::get_ident_name(&ident_tok);
                let orig_ident = match self.scope.bindings.get(name) {
                    Some(ScopeBinding(orig_ident, _, _)) => orig_ident.clone(),
                    None => unreachable!()
                };
                return Err(TypecheckerError::AssignmentToImmutable { token, orig_ident });
            }

            let expr = self.visit(*expr)?;
            let expr_type = expr.get_type();
            if typ != &expr_type {
                Err(TypecheckerError::Mismatch {
                    token: expr.get_token().clone(),
                    expected: typ.clone(),
                    actual: expr_type,
                })
            } else {
                let node = TypedAssignmentNode {
                    typ: expr_type,
                    target: Box::new(ident),
                    expr: Box::new(expr),
                };
                Ok(TypedAstNode::Assignment(token, node))
            }
        } else {
            Err(TypecheckerError::InvalidAssignmentTarget { token })
        }
    }

    fn visit_indexing(&mut self, token: Token, node: IndexingNode) -> Result<TypedAstNode, TypecheckerError> {
        let IndexingNode { target, index } = node;

        let target = self.visit(*target)?;
        let target_type = target.get_type();

        let typ = match (target_type, &index) {
            (Type::Array(inner_type), IndexingMode::Index(_)) => Ok(Type::Option(inner_type)),
            (Type::Array(inner_type), IndexingMode::Range(_, _)) => Ok(Type::Array(inner_type)),
            (Type::String, _) => Ok(Type::String),
            (typ, _) => Err(TypecheckerError::Mismatch {
                token: token.clone(),
                expected: Type::Or(vec![Type::Array(Box::new(Type::Any)), Type::String]),
                actual: typ,
            })
        }?;

        let index = match index {
            IndexingMode::Index(idx) => {
                let idx = self.visit(*idx)?;
                match idx.get_type() {
                    Type::Int => Ok(IndexingMode::Index(Box::new(idx))),
                    typ @ _ => Err(TypecheckerError::Mismatch {
                        token: idx.get_token().clone(),
                        expected: Type::Int,
                        actual: typ,
                    })
                }
            }
            IndexingMode::Range(start, end) => {
                #[inline]
                fn visit_endpoint(tc: &mut Typechecker, node: Option<Box<AstNode>>) -> Result<Option<Box<TypedAstNode>>, TypecheckerError> {
                    match node {
                        None => Ok(None),
                        Some(node) => {
                            let typed_node = tc.visit(*node)?;
                            let token = typed_node.get_token().clone();
                            match typed_node.get_type() {
                                Type::Int => Ok(Some(Box::new(typed_node))),
                                typ @ _ => Err(TypecheckerError::Mismatch { token, expected: Type::Int, actual: typ })
                            }
                        }
                    }
                }

                let start = visit_endpoint(self, start)?;
                let end = visit_endpoint(self, end)?;
                Ok(IndexingMode::Range(start, end))
            }
        }?;

        Ok(TypedAstNode::Indexing(token, TypedIndexingNode {
            typ,
            target: Box::new(target),
            index,
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
    fn typecheck_binary_arithmetic_optionals() -> TestResult {
        let typed_ast = typecheck("[0][1] + [2][3]")?;
        let expected = TypedAstNode::Binary(
            Token::Plus(Position::new(1, 8)),
            TypedBinaryNode {
                typ: Type::Option(Box::new(Type::Int)),
                left: Box::new(
                    TypedAstNode::Indexing(
                        Token::LBrack(Position::new(1, 4)),
                        TypedIndexingNode {
                            typ: Type::Option(Box::new(Type::Int)),
                            index: IndexingMode::Index(Box::new(
                                TypedAstNode::Literal(
                                    Token::Int(Position::new(1, 5), 1),
                                    TypedLiteralNode::IntLiteral(1),
                                ),
                            )),
                            target: Box::new(
                                TypedAstNode::Array(
                                    Token::LBrack(Position::new(1, 1)),
                                    TypedArrayNode {
                                        typ: Type::Array(Box::new(Type::Int)),
                                        items: vec![
                                            Box::new(
                                                TypedAstNode::Literal(
                                                    Token::Int(Position::new(1, 2), 0),
                                                    TypedLiteralNode::IntLiteral(0),
                                                ),
                                            )
                                        ],
                                    },
                                )
                            ),
                        },
                    )
                ),
                op: BinaryOp::Add,
                right: Box::new(
                    TypedAstNode::Indexing(
                        Token::LBrack(Position::new(1, 13)),
                        TypedIndexingNode {
                            typ: Type::Option(Box::new(Type::Int)),
                            index: IndexingMode::Index(Box::new(
                                TypedAstNode::Literal(
                                    Token::Int(Position::new(1, 14), 3),
                                    TypedLiteralNode::IntLiteral(3),
                                ),
                            )),
                            target: Box::new(
                                TypedAstNode::Array(
                                    Token::LBrack(Position::new(1, 10)),
                                    TypedArrayNode {
                                        typ: Type::Array(Box::new(Type::Int)),
                                        items: vec![
                                            Box::new(
                                                TypedAstNode::Literal(
                                                    Token::Int(Position::new(1, 11), 2),
                                                    TypedLiteralNode::IntLiteral(2),
                                                ),
                                            )
                                        ],
                                    },
                                )
                            ),
                        },
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
            //
            ("[1, 2][0] + 1", Token::Plus(Position::new(1, 11)), BinaryOp::Add, Type::Option(Box::new(Type::Int)), Type::Int),
            ("[\"a\", \"b\"][0] - [\"c\"][0]", Token::Minus(Position::new(1, 15)), BinaryOp::Sub, Type::Option(Box::new(Type::String)), Type::Option(Box::new(Type::String))),
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
            ("[1, 2] < 3", Token::LT(Position::new(1, 8)), BinaryOp::Lt, Type::Array(Box::new(Type::Int)), Type::Int),
            ("[1, 2] <= 3", Token::LTE(Position::new(1, 8)), BinaryOp::Lte, Type::Array(Box::new(Type::Int)), Type::Int),
            ("[1, 2] > 3", Token::GT(Position::new(1, 8)), BinaryOp::Gt, Type::Array(Box::new(Type::Int)), Type::Int),
            ("[1, 2] >= 3", Token::GTE(Position::new(1, 8)), BinaryOp::Gte, Type::Array(Box::new(Type::Int)), Type::Int),
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
            TypedArrayNode { typ: Type::Array(Box::new(Type::Any)), items: vec![] },
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
                typ: Type::Array(Box::new(Type::Int)),
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
                typ: Type::Array(Box::new(Type::String)),
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
                typ: Type::Array(Box::new(Type::Bool)),
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
        let expected_type = Type::Array(Box::new(Type::Array(Box::new(Type::Int))));
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
        let expected_binding = ScopeBinding(
            Token::Ident(Position::new(1, 5), "abc".to_string()),
            Type::Int,
            false,
        );
        assert_eq!(&expected_binding, binding);

        let (typechecker, typed_ast) = typecheck_get_typechecker("var abc: Int[] = [1]");
        let expected = TypedAstNode::BindingDecl(
            Token::Var(Position::new(1, 1)),
            TypedBindingDeclNode {
                is_mutable: true,
                ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                expr: Some(Box::new(
                    TypedAstNode::Array(
                        Token::LBrack(Position::new(1, 18)),
                        TypedArrayNode {
                            typ: Type::Array(Box::new(Type::Int)),
                            items: vec![
                                Box::new(TypedAstNode::Literal(
                                    Token::Int(Position::new(1, 19), 1),
                                    TypedLiteralNode::IntLiteral(1),
                                ))
                            ],
                        },
                    )
                )),
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let binding = typechecker.scope.bindings.get("abc").unwrap();
        let expected_binding = ScopeBinding(
            Token::Ident(Position::new(1, 5), "abc".to_string()),
            Type::Array(Box::new(Type::Int)),
            true,
        );
        assert_eq!(&expected_binding, binding);

        let (typechecker, typed_ast) = typecheck_get_typechecker("var abc: Int");
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
        let expected_binding = ScopeBinding(
            Token::Ident(Position::new(1, 5), "abc".to_string()),
            Type::Int,
            true,
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

        let err = typecheck("val abc: Int[] = 1").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Int(Position::new(1, 18), 1),
            expected: Type::Array(Box::new(Type::Int)),
            actual: Type::Int,
        };
        assert_eq!(expected, err);

        let err = typecheck("val abc: NonExistentType = true").unwrap_err();
        let expected = TypecheckerError::UnknownType {
            type_ident: Token::Ident(Position::new(1, 10), "NonExistentType".to_string())
        };
        assert_eq!(expected, err);

        let err = typecheck("var abc: NonExistentType").unwrap_err();
        let expected = TypecheckerError::UnknownType {
            type_ident: Token::Ident(Position::new(1, 10), "NonExistentType".to_string())
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn typecheck_ident() -> TestResult {
        let typed_ast = typecheck("val abc = 123\nabc")?;
        let expected = vec![
            TypedAstNode::BindingDecl(
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
            ),
            TypedAstNode::Identifier(
                Token::Ident(Position::new(2, 1), "abc".to_string()),
                Type::Int,
                false,
            )
        ];
        assert_eq!(expected, typed_ast);
        Ok(())
    }

    #[test]
    fn typecheck_ident_errors() {
        let err = typecheck("abc").unwrap_err();
        let expected = TypecheckerError::UnknownIdentifier {
            ident: Token::Ident(Position::new(1, 1), "abc".to_string())
        };
        assert_eq!(expected, err);

        let err = typecheck("var abc\nabc").unwrap_err();
        let expected = TypecheckerError::UnannotatedUninitialized {
            ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
            is_mutable: true,
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn typecheck_assignment() -> TestResult {
        let typed_ast = typecheck("var abc = 123\nabc = 456")?;
        let expected = vec![
            TypedAstNode::BindingDecl(
                Token::Var(Position::new(1, 1)),
                TypedBindingDeclNode {
                    is_mutable: true,
                    ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                    expr: Some(Box::new(
                        TypedAstNode::Literal(
                            Token::Int(Position::new(1, 11), 123),
                            TypedLiteralNode::IntLiteral(123),
                        )
                    )),
                },
            ),
            TypedAstNode::Assignment(
                Token::Assign(Position::new(2, 5)),
                TypedAssignmentNode {
                    typ: Type::Int,
                    target: Box::new(TypedAstNode::Identifier(
                        Token::Ident(Position::new(2, 1), "abc".to_string()),
                        Type::Int,
                        true,
                    )),
                    expr: Box::new(
                        TypedAstNode::Literal(
                            Token::Int(Position::new(2, 7), 456),
                            TypedLiteralNode::IntLiteral(456),
                        )
                    ),
                },
            )
        ];
        assert_eq!(expected, typed_ast);
        Ok(())
    }

    #[test]
    fn typecheck_assignment_errors() {
        let err = typecheck("true = 345").unwrap_err();
        let expected = TypecheckerError::InvalidAssignmentTarget { token: Token::Assign(Position::new(1, 6)) };
        assert_eq!(expected, err);

        let err = typecheck("val abc = 345\nabc = 67").unwrap_err();
        let expected = TypecheckerError::AssignmentToImmutable {
            token: Token::Assign(Position::new(2, 5)),
            orig_ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
        };
        assert_eq!(expected, err);

        let err = typecheck("var abc = 345\nabc = \"str\"").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(2, 7), "str".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn typecheck_indexing() -> TestResult {
        let typed_ast = typecheck("val abc = [1, 2, 3]\nabc[1]")?;
        let expected = TypedAstNode::Indexing(
            Token::LBrack(Position::new(2, 4)),
            TypedIndexingNode {
                typ: Type::Option(Box::new(Type::Int)),
                target: Box::new(
                    TypedAstNode::Identifier(
                        Token::Ident(Position::new(2, 1), "abc".to_string()),
                        Type::Array(Box::new(Type::Int)),
                        false,
                    )
                ),
                index: IndexingMode::Index(Box::new(
                    TypedAstNode::Literal(
                        Token::Int(Position::new(2, 5), 1),
                        TypedLiteralNode::IntLiteral(1),
                    )
                )),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        let typed_ast = typecheck("val idx = 1\nval abc = [1, 2, 3]\nabc[idx:]")?;
        let expected = TypedAstNode::Indexing(
            Token::LBrack(Position::new(3, 4)),
            TypedIndexingNode {
                typ: Type::Array(Box::new(Type::Int)),
                target: Box::new(
                    TypedAstNode::Identifier(
                        Token::Ident(Position::new(3, 1), "abc".to_string()),
                        Type::Array(Box::new(Type::Int)),
                        false,
                    )
                ),
                index: IndexingMode::Range(
                    Some(Box::new(
                        TypedAstNode::Identifier(
                            Token::Ident(Position::new(3, 5), "idx".to_string()),
                            Type::Int,
                            false,
                        )
                    )),
                    None,
                ),
            },
        );
        assert_eq!(expected, typed_ast[2]);

        let typed_ast = typecheck("val idx = 1\n\"abc\"[:idx * 2]")?;
        let expected = TypedAstNode::Indexing(
            Token::LBrack(Position::new(2, 6)),
            TypedIndexingNode {
                typ: Type::String,
                target: Box::new(
                    TypedAstNode::Literal(
                        Token::String(Position::new(2, 1), "abc".to_string()),
                        TypedLiteralNode::StringLiteral("abc".to_string()),
                    )
                ),
                index: IndexingMode::Range(
                    None,
                    Some(Box::new(
                        TypedAstNode::Binary(
                            Token::Star(Position::new(2, 12)),
                            TypedBinaryNode {
                                typ: Type::Int,
                                op: BinaryOp::Mul,
                                left: Box::new(
                                    TypedAstNode::Identifier(
                                        Token::Ident(Position::new(2, 8), "idx".to_string()),
                                        Type::Int,
                                        false,
                                    )
                                ),
                                right: Box::new(
                                    TypedAstNode::Literal(
                                        Token::Int(Position::new(2, 14), 2),
                                        TypedLiteralNode::IntLiteral(2),
                                    )
                                ),
                            },
                        ),
                    )),
                ),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        Ok(())
    }

    #[test]
    fn typecheck_indexing_errors() {
        let err = typecheck("[1, 2, 3][\"a\"]").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(1, 11), "a".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, err);

        let err = typecheck("\"abcd\"[[1, 2]]").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::LBrack(Position::new(1, 8)),
            expected: Type::Int,
            actual: Type::Array(Box::new(Type::Int)),
        };
        assert_eq!(expected, err);

        let err = typecheck("[1, 2, 3][\"a\":]").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(1, 11), "a".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, err);

        let err = typecheck("[1, 2, 3][:\"a\"]").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(1, 12), "a".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, err);

        let err = typecheck("123[0]").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::LBrack(Position::new(1, 4)),
            expected: Type::Or(vec![Type::Array(Box::new(Type::Any)), Type::String]),
            actual: Type::Int,
        };
        assert_eq!(expected, err);

        let err = typecheck("val a: Int = [1, 2, 3][0]").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::LBrack(Position::new(1, 23)),
            expected: Type::Int,
            actual: Type::Option(Box::new(Type::Int)),
        };
        assert_eq!(expected, err);
    }
}
