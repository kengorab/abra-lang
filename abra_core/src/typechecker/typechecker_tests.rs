use crate::lexer::tokens::{Token, Position};
use crate::parser::ast::{AstNode, AstLiteralNode, BinaryOp, UnaryOp, IndexingMode, LambdaNode, BindingPattern, ModuleId};
use crate::typechecker::types::{Type, StructType, FnType, StructTypeField};
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode, TypedInvocationNode, TypedWhileLoopNode, TypedForLoopNode, TypedTypeDeclNode, TypedMapNode, TypedAccessorNode, TypedInstantiationNode, AssignmentTargetKind, TypedLambdaNode, TypedEnumDeclNode, TypedMatchNode, TypedTupleNode, TypedSetNode, TypedTypeDeclField, TypedMatchKind};
use crate::typechecker::typechecker_error::{TypecheckerErrorKind, InvalidAssignmentTargetReason};
use crate::ModuleLoader;
use std::collections::HashSet;
use crate::typechecker::typechecker::{TypedModule, ScopeBinding};
use crate::common::test_utils::MockModuleReader;

type TestResult = Result<(), TypecheckerErrorKind>;

fn test_typecheck(input: &str) -> Result<TypedModule, TypecheckerErrorKind> {
    test_typecheck_with_modules(input, vec![])
}

fn test_typecheck_with_modules(input: &str, modules: Vec<(&str, &str)>) -> Result<TypedModule, TypecheckerErrorKind> {
    let mock_reader = MockModuleReader::new(modules);
    let mut mock_loader = ModuleLoader::new(&mock_reader);
    let module_id = ModuleId::from_name("_test");
    let module = crate::typecheck(module_id, &input.to_string(), &mut mock_loader)
        .map_err(|e| if let crate::Error::TypecheckerError(e) = e { e.kind } else { unreachable!() })?;
    Ok(module)
}

fn to_string_method_type() -> (String, Type) {
    ("toString".to_string(), Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }))
}

#[test]
fn typecheck_literals() -> TestResult {
    let module = test_typecheck("1 2.34 \"hello\"")?;
    let expected = vec![
        int_literal!((1, 1), 1),
        float_literal!((1, 3), 2.34),
        string_literal!((1, 8), "hello"),
    ];
    Ok(assert_eq!(expected, module.typed_nodes))
}

#[test]
fn typecheck_unary() -> TestResult {
    let module = test_typecheck("-1")?;
    let expected = vec![
        TypedAstNode::Unary(
            Token::Minus(Position::new(1, 1)),
            TypedUnaryNode {
                typ: Type::Int,
                op: UnaryOp::Minus,
                expr: Box::new(int_literal!((1, 2), 1)),
            },
        ),
    ];
    assert_eq!(expected, module.typed_nodes);

    let module = test_typecheck("-2.34")?;
    let expected = vec![
        TypedAstNode::Unary(
            Token::Minus(Position::new(1, 1)),
            TypedUnaryNode {
                typ: Type::Float,
                op: UnaryOp::Minus,
                expr: Box::new(float_literal!((1, 2), 2.34)),
            },
        ),
    ];
    assert_eq!(expected, module.typed_nodes);

    let module = test_typecheck("!true")?;
    let expected = vec![
        TypedAstNode::Unary(
            Token::Bang(Position::new(1, 1)),
            TypedUnaryNode {
                typ: Type::Bool,
                op: UnaryOp::Negate,
                expr: Box::new(bool_literal!((1, 2), true)),
            },
        ),
    ];
    assert_eq!(expected, module.typed_nodes);

    let module = test_typecheck("val item = [1, 2][0]\n!item")?;
    let expected = TypedAstNode::Unary(
        Token::Bang(Position::new(2, 1)),
        TypedUnaryNode {
            typ: Type::Option(Box::new(Type::Int)),
            op: UnaryOp::Negate,
            expr: Box::new(identifier!((2, 2), "item", Type::Option(Box::new(Type::Int)), 0)),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    Ok(())
}

#[test]
fn typecheck_unary_failure() -> TestResult {
    let err = test_typecheck("-\"bad\"").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Minus(Position::new(1, 1)),
        expected: Type::Union(vec![Type::Int, Type::Float]),
        actual: Type::String,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("-false").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Minus(Position::new(1, 1)),
        expected: Type::Union(vec![Type::Int, Type::Float]),
        actual: Type::Bool,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("!4.5").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Bang(Position::new(1, 1)),
        expected: Type::Union(vec![Type::Bool, Type::Option(Box::new(Type::Any))]),
        actual: Type::Float,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("!\"abc\"").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Bang(Position::new(1, 1)),
        expected: Type::Union(vec![Type::Bool, Type::Option(Box::new(Type::Any))]),
        actual: Type::String,
    };
    Ok(assert_eq!(expected, err))
}

#[test]
fn typecheck_binary_numeric_operators() -> TestResult {
    let module = test_typecheck("1 + 2")?;
    let expected = TypedAstNode::Binary(
        Token::Plus(Position::new(1, 3)),
        TypedBinaryNode {
            typ: Type::Int,
            left: Box::new(int_literal!((1, 1), 1)),
            op: BinaryOp::Add,
            right: Box::new(int_literal!((1, 5), 2)),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("1 % 2")?;
    let expected = TypedAstNode::Binary(
        Token::Percent(Position::new(1, 3)),
        TypedBinaryNode {
            typ: Type::Int,
            left: Box::new(int_literal!((1, 1), 1)),
            op: BinaryOp::Mod,
            right: Box::new(int_literal!((1, 5), 2)),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("1.1 % 2")?;
    let expected = TypedAstNode::Binary(
        Token::Percent(Position::new(1, 5)),
        TypedBinaryNode {
            typ: Type::Float,
            left: Box::new(float_literal!((1, 1), 1.1)),
            op: BinaryOp::Mod,
            right: Box::new(int_literal!((1, 7), 2)),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("1 % 2.3")?;
    let expected = TypedAstNode::Binary(
        Token::Percent(Position::new(1, 3)),
        TypedBinaryNode {
            typ: Type::Float,
            left: Box::new(int_literal!((1, 1), 1)),
            op: BinaryOp::Mod,
            right: Box::new(float_literal!((1, 5), 2.3)),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("2 ** 3")?;
    let expected = TypedAstNode::Binary(
        Token::StarStar(Position::new(1, 3)),
        TypedBinaryNode {
            typ: Type::Float,
            left: Box::new(int_literal!((1, 1), 2)),
            op: BinaryOp::Pow,
            right: Box::new(int_literal!((1, 6), 3)),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("1 ** 2.3")?;
    let expected = TypedAstNode::Binary(
        Token::StarStar(Position::new(1, 3)),
        TypedBinaryNode {
            typ: Type::Float,
            left: Box::new(int_literal!((1, 1), 1)),
            op: BinaryOp::Pow,
            right: Box::new(float_literal!((1, 6), 2.3)),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    Ok(())
}

#[test]
fn typecheck_binary_numeric_operators_nested() -> TestResult {
    let module = test_typecheck("1 + 2.3 - -4.5")?;
    let expected = TypedAstNode::Binary(
        Token::Minus(Position::new(1, 9)),
        TypedBinaryNode {
            typ: Type::Float,
            left: Box::new(
                TypedAstNode::Binary(
                    Token::Plus(Position::new(1, 3)),
                    TypedBinaryNode {
                        typ: Type::Float,
                        left: Box::new(int_literal!((1, 1), 1)),
                        op: BinaryOp::Add,
                        right: Box::new(float_literal!((1, 5), 2.3)),
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
                        expr: Box::new(float_literal!((1, 12), 4.5)),
                    },
                )
            ),
        },
    );
    Ok(assert_eq!(expected, module.typed_nodes[0]))
}

#[test]
fn typecheck_binary_str_concat() -> TestResult {
    let module = test_typecheck("\"hello \" + \"world\"")?;
    let expected = TypedAstNode::Binary(
        Token::Plus(Position::new(1, 10)),
        TypedBinaryNode {
            typ: Type::String,
            left: Box::new(string_literal!((1, 1), "hello ")),
            op: BinaryOp::Add,
            right: Box::new(string_literal!((1, 12), "world")),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("\"hello \" + 3")?;
    let expected = TypedAstNode::Binary(
        Token::Plus(Position::new(1, 10)),
        TypedBinaryNode {
            typ: Type::String,
            left: Box::new(string_literal!((1, 1), "hello ")),
            op: BinaryOp::Add,
            right: Box::new(int_literal!((1, 12), 3)),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("3.14 + \"world\"")?;
    let expected = TypedAstNode::Binary(
        Token::Plus(Position::new(1, 6)),
        TypedBinaryNode {
            typ: Type::String,
            left: Box::new(float_literal!((1, 1), 3.14)),
            op: BinaryOp::Add,
            right: Box::new(string_literal!((1, 8), "world")),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("false + \" world\"")?;
    let expected = TypedAstNode::Binary(
        Token::Plus(Position::new(1, 7)),
        TypedBinaryNode {
            typ: Type::String,
            left: Box::new(bool_literal!((1, 1), false)),
            op: BinaryOp::Add,
            right: Box::new(string_literal!((1, 9), " world")),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    Ok(())
}

#[test]
fn typecheck_binary_assignment_operator() {
    assert!(test_typecheck("var a = 1\na += 3").is_ok());
    assert!(test_typecheck("var a = \"abc\"\na += 123").is_ok());
    assert!(test_typecheck("var a = \"abc\"\na += \"def\"").is_ok());

    assert!(test_typecheck("var a = 1\na -= 3").is_ok());
    assert!(test_typecheck("var a = 1\na *= 3").is_ok());
    assert!(test_typecheck("var a = 1.0\na /= 3").is_ok());
    assert!(test_typecheck("var a = 1\na %= 3").is_ok());
    assert!(test_typecheck("var a = true\na ||= false").is_ok());
    assert!(test_typecheck("var a = true\na &&= false").is_ok());

    assert!(test_typecheck("var a = None\na ?:= false").is_ok());
}

#[test]
fn typecheck_binary_assignment_operator_errors() {
    assert!(test_typecheck("var a = 123\na += \"def\"").is_err());
    assert!(test_typecheck("val a = 1\na *= 3").is_err());

    assert!(test_typecheck("var a = true\na ||= 123").is_err());
    assert!(test_typecheck("var a = \"asdf\"\na &&= false").is_err());

    assert!(test_typecheck("true &&= false").is_err());
}

#[test]
fn typecheck_binary_numeric_failures() {
    let cases = vec![
        ("3 - \"str\"", Token::Minus(Position::new(1, 3)), BinaryOp::Sub, Type::Int, Type::String),
        ("3.2 - \"str\"", Token::Minus(Position::new(1, 5)), BinaryOp::Sub, Type::Float, Type::String),
        ("3 * \"str\"", Token::Star(Position::new(1, 3)), BinaryOp::Mul, Type::Int, Type::String),
        ("3.2 * \"str\"", Token::Star(Position::new(1, 5)), BinaryOp::Mul, Type::Float, Type::String),
        ("3 / \"str\"", Token::Slash(Position::new(1, 3)), BinaryOp::Div, Type::Int, Type::String),
        ("3.2 / \"str\"", Token::Slash(Position::new(1, 5)), BinaryOp::Div, Type::Float, Type::String),
        ("3.2 % \"str\"", Token::Percent(Position::new(1, 5)), BinaryOp::Mod, Type::Float, Type::String),
        ("3.2 ** \"str\"", Token::StarStar(Position::new(1, 5)), BinaryOp::Pow, Type::Float, Type::String),
        //
        ("\"str\" - 3", Token::Minus(Position::new(1, 7)), BinaryOp::Sub, Type::String, Type::Int),
        ("\"str\" - 3.2", Token::Minus(Position::new(1, 7)), BinaryOp::Sub, Type::String, Type::Float),
        ("\"str\" * 3", Token::Star(Position::new(1, 7)), BinaryOp::Mul, Type::String, Type::Int),
        ("\"str\" * 3.2", Token::Star(Position::new(1, 7)), BinaryOp::Mul, Type::String, Type::Float),
        ("\"str\" / 3", Token::Slash(Position::new(1, 7)), BinaryOp::Div, Type::String, Type::Int),
        ("\"str\" / 3.2", Token::Slash(Position::new(1, 7)), BinaryOp::Div, Type::String, Type::Float),
        ("\"str\" % 3.2", Token::Percent(Position::new(1, 7)), BinaryOp::Mod, Type::String, Type::Float),
        ("\"str\" ** 3.2", Token::StarStar(Position::new(1, 7)), BinaryOp::Pow, Type::String, Type::Float),
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
        ("true % false", Token::Percent(Position::new(1, 6)), BinaryOp::Mod, Type::Bool, Type::Bool),
        //
        ("[1, 2][0] + 1", Token::Plus(Position::new(1, 11)), BinaryOp::Add, Type::Option(Box::new(Type::Int)), Type::Int),
        ("[0][1] + [2][3]", Token::Plus(Position::new(1, 8)), BinaryOp::Add, Type::Option(Box::new(Type::Int)), Type::Option(Box::new(Type::Int))),
        ("[\"a\", \"b\"][0] - [\"c\"][0]", Token::Minus(Position::new(1, 15)), BinaryOp::Sub, Type::Option(Box::new(Type::String)), Type::Option(Box::new(Type::String))),
    ];

    for (input, token, op, ltype, rtype) in cases {
        let expected = TypecheckerErrorKind::InvalidOperator { token, op, ltype, rtype };
        let msg = format!("Typechecking `{}` should result in the error {:?}", input, expected);

        let err = test_typecheck(input).expect_err(&*msg);
        assert_eq!(expected, err, "{}", msg);
    }
}

#[test]
fn typecheck_binary_boolean() -> TestResult {
    let module = test_typecheck("true && true || false")?;
    let expected = TypedAstNode::IfExpression(Token::If(Position::new(1, 14)), TypedIfNode {
        typ: Type::Bool,
        condition: Box::new(
            TypedAstNode::IfExpression(Token::If(Position::new(1, 6)), TypedIfNode {
                typ: Type::Bool,
                condition: Box::new(bool_literal!((1, 1), true)),
                condition_binding: None,
                if_block: vec![
                    bool_literal!((1, 9), true),
                ],
                else_block: Some(vec![
                    bool_literal!((1, 6), false), // <- pos is derived from && position
                ]),
            }),
        ),
        condition_binding: None,
        if_block: vec![
            bool_literal!((1, 14), true), // <- pos is derived from || position
        ],
        else_block: Some(vec![
            bool_literal!((1, 17), false),
        ]),
    });
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("true ^ true")?;
    let expected = TypedAstNode::Binary(
        Token::Caret(Position::new(1, 6)),
        TypedBinaryNode {
            typ: Type::Bool,
            left: Box::new(bool_literal!((1, 1), true)),
            op: BinaryOp::Xor,
            right: Box::new(bool_literal!((1, 8), true)),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    // Testing boolean operators with Option types
    let module = test_typecheck("val item = [1, 2][0]\nitem && true")?;
    let expected = TypedAstNode::IfExpression(
        Token::If(Position::new(2, 6)),
        TypedIfNode {
            typ: Type::Bool,
            condition: Box::new(identifier!((2, 1), "item", Type::Option(Box::new(Type::Int)), 0)),
            condition_binding: None,
            if_block: vec![
                bool_literal!((2, 9), true),
            ],
            else_block: Some(vec![
                bool_literal!((2, 6), false), // <- pos is derived from && position
            ]),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);
    let res = test_typecheck("val item = [1, 2][0]\ntrue && item");
    assert!(res.is_ok());
    let res = test_typecheck("val item = [1, 2][0]\nitem && item");
    assert!(res.is_ok());

    let module = test_typecheck("val item = [1, 2][0]\nitem || false")?;
    let expected = TypedAstNode::IfExpression(
        Token::If(Position::new(2, 6)),
        TypedIfNode {
            typ: Type::Bool,
            condition: Box::new(identifier!((2, 1), "item", Type::Option(Box::new(Type::Int)), 0)),
            condition_binding: None,
            if_block: vec![
                bool_literal!((2, 6), true), // <- pos is derived from || position
            ],
            else_block: Some(vec![
                bool_literal!((2, 9), false),
            ]),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);
    let res = test_typecheck("val item = [1, 2][0]\nfalse || item");
    assert!(res.is_ok());
    let res = test_typecheck("val item = [1, 2][0]\nitem || item");
    assert!(res.is_ok());

    let module = test_typecheck("val item = [1, 2][0]\nitem ^ false")?;
    let expected = TypedAstNode::Binary(
        Token::Caret(Position::new(2, 6)),
        TypedBinaryNode {
            typ: Type::Bool,
            left: Box::new(identifier!((2, 1), "item", Type::Option(Box::new(Type::Int)), 0)),
            op: BinaryOp::Xor,
            right: Box::new(bool_literal!((2, 8), false)),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);
    let res = test_typecheck("val item = [1, 2][0]\nfalse ^ item");
    assert!(res.is_ok());
    let res = test_typecheck("val item = [1, 2][0]\nitem ^ item");
    assert!(res.is_ok());

    Ok(())
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
        //
        ("true ^ 1", Token::Caret(Position::new(1, 6)), BinaryOp::Xor, Type::Bool, Type::Int),
        ("true ^ 3.14", Token::Caret(Position::new(1, 6)), BinaryOp::Xor, Type::Bool, Type::Float),
        ("true ^ \"str\"", Token::Caret(Position::new(1, 6)), BinaryOp::Xor, Type::Bool, Type::String),
        ("false ^ 1", Token::Caret(Position::new(1, 7)), BinaryOp::Xor, Type::Bool, Type::Int),
        ("false ^ 3.14", Token::Caret(Position::new(1, 7)), BinaryOp::Xor, Type::Bool, Type::Float),
        ("false ^ \"str\"", Token::Caret(Position::new(1, 7)), BinaryOp::Xor, Type::Bool, Type::String),
    ];

    for (input, token, op, ltype, rtype) in cases {
        let expected = TypecheckerErrorKind::InvalidOperator { token, op, ltype, rtype };
        let msg = format!("Typechecking `{}` should result in the error {:?}", input, expected);

        let err = test_typecheck(input).expect_err(&*msg);
        assert_eq!(expected, err, "{}", msg);
    }
}

#[test]
fn typecheck_binary_comparisons() -> TestResult {
    let cases: Vec<(&str, Box<dyn Fn(Position) -> Token>, BinaryOp)> = vec![
        ("1 <  2", Box::new(Token::LT), BinaryOp::Lt),
        ("1 <= 2", Box::new(Token::LTE), BinaryOp::Lte),
        ("1 >  2", Box::new(Token::GT), BinaryOp::Gt),
        ("1 >= 2", Box::new(Token::GTE), BinaryOp::Gte),
        ("1 != 2", Box::new(Token::Neq), BinaryOp::Neq),
        ("1 == 2", Box::new(Token::Eq), BinaryOp::Eq),
    ];
    for (input, token, op) in cases {
        let module = test_typecheck(input)?;
        let expected = TypedAstNode::Binary(
            token(Position::new(1, 3)),
            TypedBinaryNode {
                typ: Type::Bool,
                left: Box::new(int_literal!((1, 1), 1)),
                op,
                right: Box::new(int_literal!((1, 6), 2)),
            },
        );
        assert_eq!(expected, module.typed_nodes[0]);
    };

    let cases: Vec<(&str, Box<dyn Fn(Position) -> Token>, BinaryOp)> = vec![
        ("\"abc\" <  \"def\"", Box::new(Token::LT), BinaryOp::Lt),
        ("\"abc\" <= \"def\"", Box::new(Token::LTE), BinaryOp::Lte),
        ("\"abc\" >  \"def\"", Box::new(Token::GT), BinaryOp::Gt),
        ("\"abc\" >= \"def\"", Box::new(Token::GTE), BinaryOp::Gte),
        ("\"abc\" == \"def\"", Box::new(Token::Eq), BinaryOp::Eq),
        ("\"abc\" != \"def\"", Box::new(Token::Neq), BinaryOp::Neq),
    ];
    for (input, token, op) in cases {
        let module = test_typecheck(input)?;
        let expected = TypedAstNode::Binary(
            token(Position::new(1, 7)),
            TypedBinaryNode {
                typ: Type::Bool,
                left: Box::new(string_literal!((1, 1), "abc")),
                op,
                right: Box::new(string_literal!((1, 10), "def")),
            },
        );
        assert_eq!(expected, module.typed_nodes[0]);
    }

    let cases: Vec<(&str, Box<dyn Fn(Position) -> Token>, BinaryOp)> = vec![
        ("\"abc\" == 3", Box::new(Token::Eq), BinaryOp::Eq),
        ("\"abc\" != 3", Box::new(Token::Neq), BinaryOp::Neq),
    ];
    for (input, token, op) in cases {
        let module = test_typecheck(input)?;
        let expected = TypedAstNode::Binary(
            token(Position::new(1, 7)),
            TypedBinaryNode {
                typ: Type::Bool,
                left: Box::new(string_literal!((1, 1), "abc")),
                op,
                right: Box::new(int_literal!((1, 10), 3)),
            },
        );
        assert_eq!(expected, module.typed_nodes[0]);
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
        let expected = TypecheckerErrorKind::InvalidOperator { token, op, ltype, rtype };
        let msg = format!("Typechecking `{}` should result in the error {:?}", input, expected);

        let err = test_typecheck(input).expect_err(&*msg);
        assert_eq!(expected, err, "{}", msg);
    }
}

#[test]
fn typecheck_binary_coalesce_operation() -> TestResult {
    let cases = vec![
        ("[1][0] ?: 2", Type::Int),
        ("[[0, 1]][0] ?: [1, 2]", Type::Array(Box::new(Type::Int))),
        ("[[0, 1][0]][0] ?: [1, 2][1]", Type::Option(Box::new(Type::Int))),
        ("[][0] ?: 0", Type::Int),
        ("None ?: 0", Type::Int),
    ];

    for (input, expected_type) in cases {
        let module = test_typecheck(input)?;
        assert_eq!(expected_type, module.typed_nodes[0].get_type());
    };

    Ok(())
}

#[test]
fn typecheck_binary_coalesce_errors() {
    let cases = vec![
        ("[1][0] ?: \"a\"", Token::String(Position::new(1, 11), "a".to_string()), Type::Int, Type::String),
        ("[1][0] ?: 1.0", Token::Float(Position::new(1, 11), 1.0), Type::Int, Type::Float),
    ];
    for (input, token, expected, actual) in cases {
        let expected = TypecheckerErrorKind::Mismatch { token, expected, actual };
        let msg = format!("Typechecking `{}` should result in the error {:?}", input, expected);

        let err = test_typecheck(input).expect_err(&*msg);
        assert_eq!(expected, err, "{}", msg);
    }

    let input = "\"abc\" ?: 12";
    let err = test_typecheck(input).expect_err("Error expected");
    let expected = TypecheckerErrorKind::InvalidOperator {
        token: Token::Elvis(Position::new(1, 7)),
        op: BinaryOp::Coalesce,
        ltype: Type::String,
        rtype: Type::Int,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_grouped() -> TestResult {
    let module = test_typecheck("(1 + 2)")?;
    let expected = TypedAstNode::Grouped(
        Token::LParen(Position::new(1, 1), false),
        TypedGroupedNode {
            typ: Type::Int,
            expr: Box::new(
                TypedAstNode::Binary(
                    Token::Plus(Position::new(1, 4)),
                    TypedBinaryNode {
                        typ: Type::Int,
                        left: Box::new(int_literal!((1, 2), 1)),
                        op: BinaryOp::Add,
                        right: Box::new(int_literal!((1, 6), 2)),
                    },
                )
            ),
        },
    );
    Ok(assert_eq!(expected, module.typed_nodes[0]))
}

#[test]
fn typecheck_array_empty() -> TestResult {
    let module = test_typecheck("[]")?;
    let expected = TypedAstNode::Array(
        Token::LBrack(Position::new(1, 1), false),
        TypedArrayNode { typ: Type::Array(Box::new(Type::Unknown)), items: vec![] },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    // Verify explicit type annotations works
    let res = test_typecheck("val a: Int[] = []");
    assert!(res.is_ok());
    let res = test_typecheck("val a: Array<Int> = []");
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_array_nonempty() -> TestResult {
    let module = test_typecheck("[1, 2, 3]")?;
    let expected = TypedAstNode::Array(
        Token::LBrack(Position::new(1, 1), false),
        TypedArrayNode {
            typ: Type::Array(Box::new(Type::Int)),
            items: vec![
                int_literal!((1, 2), 1),
                int_literal!((1, 5), 2),
                int_literal!((1, 8), 3),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("[\"a\", true]")?;
    let expected = TypedAstNode::Array(
        Token::LBrack(Position::new(1, 1), false),
        TypedArrayNode {
            typ: Type::Array(Box::new(Type::Union(vec![Type::String, Type::Bool]))),
            items: vec![
                string_literal!((1, 2), "a"),
                bool_literal!((1, 7), true),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    // Verify explicit type annotations works
    let res = test_typecheck("val a: Bool[] = [true, false]");
    assert!(res.is_ok());
    let res = test_typecheck("val a: Array<Bool> = [true]");
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_array_nested() -> TestResult {
    let module = test_typecheck("[[1, 2], [3, 4]]")?;
    let expected_type = Type::Array(Box::new(Type::Array(Box::new(Type::Int))));
    assert_eq!(expected_type, module.typed_nodes[0].get_type());

    // TODO: Handle edge cases, like [[1, 2.3], [3.4, 5]], which should be (Int | Float)[][]

    Ok(())
}

#[test]
fn typecheck_set_empty() -> TestResult {
    let module = test_typecheck("#{}")?;
    let expected = TypedAstNode::Set(
        Token::LBraceHash(Position::new(1, 1)),
        TypedSetNode { typ: Type::Set(Box::new(Type::Unknown)), items: vec![] },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    // Verify explicit type annotations works
    let res = test_typecheck("val a: Set<Int> = #{}");
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_set_nonempty() -> TestResult {
    let module = test_typecheck("#{1, 2, 3}")?;
    let expected = TypedAstNode::Set(
        Token::LBraceHash(Position::new(1, 1)),
        TypedSetNode {
            typ: Type::Set(Box::new(Type::Int)),
            items: vec![
                int_literal!((1, 3), 1),
                int_literal!((1, 6), 2),
                int_literal!((1, 9), 3),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("#{\"a\", 12}")?;
    let expected = TypedAstNode::Set(
        Token::LBraceHash(Position::new(1, 1)),
        TypedSetNode {
            typ: Type::Set(Box::new(Type::Union(vec![Type::String, Type::Int]))),
            items: vec![
                string_literal!((1, 3), "a"),
                int_literal!((1, 8), 12),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    // Verify explicit type annotations works
    let res = test_typecheck("val a: Set<Int> = #{1, 2, 3}");
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_set_nested() -> TestResult {
    let module = test_typecheck("#{#{1, 2}, #{3, 4}}")?;
    let expected_type = Type::Set(Box::new(Type::Set(Box::new(Type::Int))));
    assert_eq!(expected_type, module.typed_nodes[0].get_type());

    // Verify explicit type annotations works
    let res = test_typecheck("val a: Set<Set<Int>> = #{#{1, 2}, #{3, 4}}");
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_tuple() -> TestResult {
    let module = test_typecheck("(1, \"hello\")")?;
    let expected_type = Type::Tuple(vec![Type::Int, Type::String]);
    assert_eq!(expected_type, module.typed_nodes[0].get_type());

    let module = test_typecheck("(1, (\"hello\", \"world\"), 3)")?;
    let expected_type = Type::Tuple(vec![
        Type::Int,
        Type::Tuple(vec![Type::String, Type::String]),
        Type::Int,
    ]);
    assert_eq!(expected_type, module.typed_nodes[0].get_type());

    let res = test_typecheck("val t: (Int, Int, String) = (1, 2, \"three\")");
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_tuple_generics() -> TestResult {
    let module = test_typecheck("\
          func abc<T>(t: T): (T, T[]) = (t, [t])\
          abc(1)
        ")?;
    let expected_type = Type::Tuple(vec![Type::Int, Type::Array(Box::new(Type::Int))]);
    assert_eq!(expected_type, module.typed_nodes[1].get_type());

    let module = test_typecheck("\
          func abc<T, U, V>(t: T, u: U, v: V): (T, U, V) = (t, u, v)\
          abc(1, \"2\", [3])
        ")?;
    let expected_type = Type::Tuple(vec![
        Type::Int,
        Type::String,
        Type::Array(Box::new(Type::Int)),
    ]);
    assert_eq!(expected_type, module.typed_nodes[1].get_type());

    Ok(())
}

#[test]
fn typecheck_map_empty() -> TestResult {
    let module = test_typecheck("{}")?;
    let expected = TypedAstNode::Map(
        Token::LBrace(Position::new(1, 1)),
        TypedMapNode {
            typ: Type::Map(Box::new(Type::Unknown), Box::new(Type::Unknown)),
            items: vec![],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    Ok(())
}

#[test]
fn typecheck_map_nonempty() -> TestResult {
    let module = test_typecheck("{ a: 1, b: 2 }")?;
    let expected = TypedAstNode::Map(
        Token::LBrace(Position::new(1, 1)),
        TypedMapNode {
            typ: Type::Map(Box::new(Type::String), Box::new(Type::Int)),
            items: vec![
                (string_literal!((1, 3), "a"), int_literal!((1, 6), 1)),
                (string_literal!((1, 9), "b"), int_literal!((1, 12), 2)),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("{ a: { c: true }, b: { c: false } }")?;
    let expected = TypedAstNode::Map(
        Token::LBrace(Position::new(1, 1)),
        TypedMapNode {
            typ: Type::Map(
                Box::new(Type::String),
                Box::new(Type::Map(Box::new(Type::String), Box::new(Type::Bool))),
            ),
            items: vec![
                (string_literal!((1, 3), "a"), TypedAstNode::Map(
                    Token::LBrace(Position::new(1, 6)),
                    TypedMapNode {
                        typ: Type::Map(Box::new(Type::String), Box::new(Type::Bool)),
                        items: vec![(string_literal!((1, 8), "c"), bool_literal!((1, 11), true))],
                    },
                )),
                (string_literal!((1, 19), "b"), TypedAstNode::Map(
                    Token::LBrace(Position::new(1, 22)),
                    TypedMapNode {
                        typ: Type::Map(Box::new(Type::String), Box::new(Type::Bool)),
                        items: vec![(string_literal!((1, 24), "c"), bool_literal!((1, 27), false))],
                    },
                )),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("{ a: 1, b: true, c: \"hello\" }")?;
    let expected_type = Type::Map(
        Box::new(Type::String),
        Box::new(Type::Union(vec![Type::Int, Type::Bool, Type::String])),
    );
    assert_eq!(expected_type, module.typed_nodes[0].get_type());

    Ok(())
}

#[test]
fn typecheck_type_annotation() -> TestResult {
    let cases = vec![
        // Simple cases
        ("var abc: Int", Type::Int),
        ("var abc: Int[]", Type::Array(Box::new(Type::Int))),
        ("var abc: Int?", Type::Option(Box::new(Type::Int))),
        // Complex cases
        ("var abc: Int[][]", Type::Array(Box::new(Type::Array(Box::new(Type::Int))))),
        ("var abc: Int?[]", Type::Array(Box::new(Type::Option(Box::new(Type::Int))))),
        ("var abc: Int[]?", Type::Option(Box::new(Type::Array(Box::new(Type::Int))))),
    ];

    for (input, expected_binding_type) in cases {
        let module = test_typecheck(input)?;
        assert_eq!(expected_binding_type, module.global_bindings["abc"].1);
    }
    Ok(())
}

#[test]
fn typecheck_binding_decl() -> TestResult {
    let module = test_typecheck("val abc = 123")?;
    let expected = TypedAstNode::BindingDecl(
        Token::Val(Position::new(1, 1)),
        TypedBindingDeclNode {
            binding: BindingPattern::Variable(ident_token!((1, 5), "abc")),
            is_mutable: false,
            expr: Some(Box::new(int_literal!((1, 11), 123))),
            scope_depth: 0,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    let binding = &module.global_bindings["abc"];
    let expected_binding = ScopeBinding(
        Token::Ident(Position::new(1, 5), "abc".to_string()),
        Type::Int,
        false,
    );
    assert_eq!(&expected_binding, binding);

    let module = test_typecheck("var abc: Int")?;
    let expected = TypedAstNode::BindingDecl(
        Token::Var(Position::new(1, 1)),
        TypedBindingDeclNode {
            binding: BindingPattern::Variable(ident_token!((1, 5), "abc")),
            is_mutable: true,
            expr: None,
            scope_depth: 0,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    let binding = &module.global_bindings["abc"];
    let expected_binding = ScopeBinding(
        Token::Ident(Position::new(1, 5), "abc".to_string()),
        Type::Int,
        true,
    );
    assert_eq!(&expected_binding, binding);

    assert!(test_typecheck("val arr: Int[] = []").is_ok());

    Ok(())
}

#[test]
fn typecheck_binding_decl_errors() {
    let err = test_typecheck("val abc").unwrap_err();
    let expected = TypecheckerErrorKind::MissingRequiredAssignment {
        ident: ident_token!((1, 5), "abc"),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val abc = 1\nval abc = 2").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateBinding {
        ident: Token::Ident(Position::new(2, 5), "abc".to_string()),
        orig_ident: Some(Token::Ident(Position::new(1, 5), "abc".to_string())),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val abc: Int[] = 1").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Int(Position::new(1, 18), 1),
        expected: Type::Array(Box::new(Type::Int)),
        actual: Type::Int,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val abc: NonExistentType = true").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownType {
        type_ident: Token::Ident(Position::new(1, 10), "NonExistentType".to_string())
    };
    assert_eq!(expected, err);

    let err = test_typecheck("var abc: NonExistentType").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownType {
        type_ident: Token::Ident(Position::new(1, 10), "NonExistentType".to_string())
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val abc = println()").unwrap_err();
    let expected = TypecheckerErrorKind::ForbiddenVariableType {
        binding: BindingPattern::Variable(ident_token!((1, 5), "abc")),
        typ: Type::Unit,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_binding_decl_destructuring() -> TestResult {
    // Tuples
    let module = test_typecheck("val (a, b, c) = (1, \"2\", [1, 2, 3])")?;
    let expected = TypedAstNode::BindingDecl(
        Token::Val(Position::new(1, 1)),
        TypedBindingDeclNode {
            binding: BindingPattern::Tuple(
                Token::LParen(Position::new(1, 5), false),
                vec![
                    BindingPattern::Variable(ident_token!((1, 6), "a")),
                    BindingPattern::Variable(ident_token!((1, 9), "b")),
                    BindingPattern::Variable(ident_token!((1, 12), "c")),
                ],
            ),
            is_mutable: false,
            expr: Some(Box::new(TypedAstNode::Tuple(
                Token::LParen(Position::new(1, 17), false),
                TypedTupleNode {
                    typ: Type::Tuple(vec![Type::Int, Type::String, Type::Array(Box::new(Type::Int))]),
                    items: vec![
                        int_literal!((1, 18), 1),
                        string_literal!((1, 21), "2"),
                        TypedAstNode::Array(
                            Token::LBrack(Position::new(1, 26), false),
                            TypedArrayNode {
                                typ: Type::Array(Box::new(Type::Int)),
                                items: vec![
                                    int_literal!((1, 27), 1),
                                    int_literal!((1, 30), 2),
                                    int_literal!((1, 33), 3),
                                ],
                            },
                        ),
                    ],
                },
            ))),
            scope_depth: 0,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    let binding = &module.global_bindings["a"];
    let expected_binding = ScopeBinding(ident_token!((1, 6), "a"), Type::Int, false);
    assert_eq!(&expected_binding, binding);
    let binding = &module.global_bindings["b"];
    let expected_binding = ScopeBinding(ident_token!((1, 9), "b"), Type::String, false);
    assert_eq!(&expected_binding, binding);
    let binding = &module.global_bindings["c"];
    let expected_binding = ScopeBinding(ident_token!((1, 12), "c"), Type::Array(Box::new(Type::Int)), false);
    assert_eq!(&expected_binding, binding);

    // Arrays
    let module = test_typecheck("val [a, *b, c] = [1, 2, 3]")?;
    let expected = TypedAstNode::BindingDecl(
        Token::Val(Position::new(1, 1)),
        TypedBindingDeclNode {
            binding: BindingPattern::Array(
                Token::LBrack(Position::new(1, 5), false),
                vec![
                    (BindingPattern::Variable(ident_token!((1, 6), "a")), false),
                    (BindingPattern::Variable(ident_token!((1, 10), "b")), true),
                    (BindingPattern::Variable(ident_token!((1, 13), "c")), false),
                ],
                false,
            ),
            is_mutable: false,
            expr: Some(Box::new(TypedAstNode::Array(
                Token::LBrack(Position::new(1, 18), false),
                TypedArrayNode {
                    typ: Type::Array(Box::new(Type::Int)),
                    items: vec![
                        int_literal!((1, 19), 1),
                        int_literal!((1, 22), 2),
                        int_literal!((1, 25), 3),
                    ],
                },
            ))),
            scope_depth: 0,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    let binding = &module.global_bindings["a"];
    let expected_binding = ScopeBinding(ident_token!((1, 6), "a"), Type::Option(Box::new(Type::Int)), false);
    assert_eq!(&expected_binding, binding);
    let binding = &module.global_bindings["b"];
    let expected_binding = ScopeBinding(ident_token!((1, 10), "b"), Type::Array(Box::new(Type::Int)), false);
    assert_eq!(&expected_binding, binding);
    let binding = &module.global_bindings["c"];
    let expected_binding = ScopeBinding(ident_token!((1, 13), "c"), Type::Option(Box::new(Type::Int)), false);
    assert_eq!(&expected_binding, binding);

    // Strings
    let module = test_typecheck("val [a, *b, c] = \"hello\"")?;
    let expected = TypedAstNode::BindingDecl(
        Token::Val(Position::new(1, 1)),
        TypedBindingDeclNode {
            binding: BindingPattern::Array(
                Token::LBrack(Position::new(1, 5), false),
                vec![
                    (BindingPattern::Variable(ident_token!((1, 6), "a")), false),
                    (BindingPattern::Variable(ident_token!((1, 10), "b")), true),
                    (BindingPattern::Variable(ident_token!((1, 13), "c")), false),
                ],
                true,
            ),
            is_mutable: false,
            expr: Some(Box::new(string_literal!((1, 18), "hello"))),
            scope_depth: 0,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    let binding = &module.global_bindings["a"];
    let expected_binding = ScopeBinding(ident_token!((1, 6), "a"), Type::String, false);
    assert_eq!(&expected_binding, binding);
    let binding = &module.global_bindings["b"];
    let expected_binding = ScopeBinding(ident_token!((1, 10), "b"), Type::String, false);
    assert_eq!(&expected_binding, binding);
    let binding = &module.global_bindings["c"];
    let expected_binding = ScopeBinding(ident_token!((1, 13), "c"), Type::String, false);
    assert_eq!(&expected_binding, binding);

    // Nested
    let module = test_typecheck("val [(x1, y1), (x2, y2)] = [(1, 2), (3, 4)]")?;
    let expected = TypedAstNode::BindingDecl(
        Token::Val(Position::new(1, 1)),
        TypedBindingDeclNode {
            binding: BindingPattern::Array(
                Token::LBrack(Position::new(1, 5), false),
                vec![
                    (
                        BindingPattern::Tuple(
                            Token::LParen(Position::new(1, 6), false),
                            vec![
                                BindingPattern::Variable(ident_token!((1, 7), "x1")),
                                BindingPattern::Variable(ident_token!((1, 11), "y1")),
                            ],
                        ),
                        false
                    ),
                    (
                        BindingPattern::Tuple(
                            Token::LParen(Position::new(1, 16), false),
                            vec![
                                BindingPattern::Variable(ident_token!((1, 17), "x2")),
                                BindingPattern::Variable(ident_token!((1, 21), "y2")),
                            ],
                        ),
                        false
                    ),
                ],
                false,
            ),
            is_mutable: false,
            expr: Some(Box::new(TypedAstNode::Array(
                Token::LBrack(Position::new(1, 28), false),
                TypedArrayNode {
                    typ: Type::Array(Box::new(Type::Tuple(vec![Type::Int, Type::Int]))),
                    items: vec![
                        TypedAstNode::Tuple(
                            Token::LParen(Position::new(1, 29), false),
                            TypedTupleNode {
                                typ: Type::Tuple(vec![Type::Int, Type::Int]),
                                items: vec![
                                    int_literal!((1, 30), 1),
                                    int_literal!((1, 33), 2),
                                ],
                            },
                        ),
                        TypedAstNode::Tuple(
                            Token::LParen(Position::new(1, 37), false),
                            TypedTupleNode {
                                typ: Type::Tuple(vec![Type::Int, Type::Int]),
                                items: vec![
                                    int_literal!((1, 38), 3),
                                    int_literal!((1, 41), 4),
                                ],
                            },
                        ),
                    ],
                },
            ))),
            scope_depth: 0,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    let binding = &module.global_bindings["x1"];
    let expected_binding = ScopeBinding(ident_token!((1, 7), "x1"), Type::Option(Box::new(Type::Int)), false);
    assert_eq!(&expected_binding, binding);
    let binding = &module.global_bindings["y1"];
    let expected_binding = ScopeBinding(ident_token!((1, 11), "y1"), Type::Option(Box::new(Type::Int)), false);
    assert_eq!(&expected_binding, binding);
    let binding = &module.global_bindings["x2"];
    let expected_binding = ScopeBinding(ident_token!((1, 17), "x2"), Type::Option(Box::new(Type::Int)), false);
    assert_eq!(&expected_binding, binding);
    let binding = &module.global_bindings["y2"];
    let expected_binding = ScopeBinding(ident_token!((1, 21), "y2"), Type::Option(Box::new(Type::Int)), false);
    assert_eq!(&expected_binding, binding);

    Ok(())
}

#[test]
fn typecheck_binding_decl_destructuring_errors() {
    let err = test_typecheck("val (a, b) = [1, 2, 3]").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidAssignmentDestructuring {
        binding: BindingPattern::Tuple(
            Token::LParen(Position::new(1, 5), false),
            vec![
                BindingPattern::Variable(ident_token!((1, 6), "a")),
                BindingPattern::Variable(ident_token!((1, 9), "b")),
            ],
        ),
        typ: Type::Array(Box::new(Type::Int)),
    };
    assert_eq!(expected, err);
    let err = test_typecheck("val [a, b] = (1, 2, 3)").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidAssignmentDestructuring {
        binding: BindingPattern::Array(
            Token::LBrack(Position::new(1, 5), false),
            vec![
                (BindingPattern::Variable(ident_token!((1, 6), "a")), false),
                (BindingPattern::Variable(ident_token!((1, 9), "b")), false),
            ],
            false,
        ),
        typ: Type::Tuple(vec![Type::Int, Type::Int, Type::Int]),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val (a, b, c) = (1, 2)").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidAssignmentDestructuring {
        binding: BindingPattern::Tuple(
            Token::LParen(Position::new(1, 5), false),
            vec![
                BindingPattern::Variable(ident_token!((1, 6), "a")),
                BindingPattern::Variable(ident_token!((1, 9), "b")),
                BindingPattern::Variable(ident_token!((1, 12), "c")),
            ],
        ),
        typ: Type::Tuple(vec![Type::Int, Type::Int]),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val [a, *b, *c] = [1, 2, 3]").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateSplatDestructuring {
        token: ident_token!((1, 14), "c")
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val (a, b, a) = (1, 2, 1)").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateBinding {
        ident: ident_token!((1, 12), "a"),
        orig_ident: Some(ident_token!((1, 6), "a")),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          val a = 4\n\
          val (a, b) = (1, 2)\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateBinding {
        ident: ident_token!((2, 6), "a"),
        orig_ident: Some(ident_token!((1, 5), "a")),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          type List<T> { items: T[] }\n\
          val (l1, l2) = (List(items: []), List(items: []))\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnboundGeneric(
        Token::LParen(Position::new(2, 5), false),
        "T".to_string(),
    );
    assert_eq!(expected, err);
}

#[test]
fn typecheck_function_decl() -> TestResult {
    let module = test_typecheck("func abc(): Int = 123")?;
    let expected = TypedAstNode::FunctionDecl(
        Token::Func(Position::new(1, 1)),
        TypedFunctionDeclNode {
            name: Token::Ident(Position::new(1, 6), "abc".to_string()),
            args: vec![],
            ret_type: Type::Int,
            body: vec![
                int_literal!((1, 19), 123)
            ],
            scope_depth: 0,
            is_recursive: false,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    let ScopeBinding(_, typ, _) = &module.global_bindings["abc"];
    let expected_type = Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::Int), is_variadic: false, is_enum_constructor: false });
    assert_eq!(&expected_type, typ);

    let module = test_typecheck("func abc(a: Int): Int = a + 1")?;
    let expected = TypedAstNode::FunctionDecl(
        Token::Func(Position::new(1, 1)),
        TypedFunctionDeclNode {
            name: Token::Ident(Position::new(1, 6), "abc".to_string()),
            args: vec![(ident_token!((1, 10), "a"), Type::Int, false, None)],
            ret_type: Type::Int,
            body: vec![
                TypedAstNode::Binary(
                    Token::Plus(Position::new(1, 27)),
                    TypedBinaryNode {
                        typ: Type::Int,
                        left: Box::new(identifier!((1, 25), "a", Type::Int, 1)),
                        op: BinaryOp::Add,
                        right: Box::new(int_literal!((1, 29), 1)),
                    })
            ],
            scope_depth: 0,
            is_recursive: false,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("func abc(): Int[] { val a = [1, 2] a }")?;
    let expected = TypedAstNode::FunctionDecl(
        Token::Func(Position::new(1, 1)),
        TypedFunctionDeclNode {
            name: Token::Ident(Position::new(1, 6), "abc".to_string()),
            args: vec![],
            ret_type: Type::Array(Box::new(Type::Int)),
            body: vec![
                TypedAstNode::BindingDecl(
                    Token::Val(Position::new(1, 21)),
                    TypedBindingDeclNode {
                        binding: BindingPattern::Variable(ident_token!((1, 25), "a")),
                        is_mutable: false,
                        expr: Some(Box::new(
                            TypedAstNode::Array(
                                Token::LBrack(Position::new(1, 29), false),
                                TypedArrayNode {
                                    typ: Type::Array(Box::new(Type::Int)),
                                    items: vec![
                                        int_literal!((1, 30), 1),
                                        int_literal!((1, 33), 2),
                                    ],
                                },
                            )
                        )),
                        scope_depth: 1,
                    },
                ),
                identifier!((1, 36), "a", Type::Array(Box::new(Type::Int)), 1),
            ],
            scope_depth: 0,
            is_recursive: false,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    let ScopeBinding(_, typ, _) = &module.global_bindings["abc"];
    let expected_type = Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::Array(Box::new(Type::Int))), is_variadic: false, is_enum_constructor: false });
    assert_eq!(&expected_type, typ);

    let module = test_typecheck("func abc(): Int = 123")?;
    let ret_type = match module.typed_nodes.first().unwrap() {
        TypedAstNode::FunctionDecl(_, TypedFunctionDeclNode { ret_type, .. }) => ret_type,
        _ => panic!("Node must be a FunctionDecl")
    };
    assert_eq!(&Type::Int, ret_type);

    // Test that bindings assigned to functions have the proper type
    let module = test_typecheck("func abc(a: Int): Bool = a == 1\nval def = abc")?;
    let ScopeBinding(_, typ, _) = &module.global_bindings["def"];
    assert_eq!(&Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::Int, false)], type_args: vec![], ret_type: Box::new(Type::Bool), is_variadic: false, is_enum_constructor: false }), typ);

    let module = test_typecheck("func abc(): Bool[] = []")?;
    let ScopeBinding(_, typ, _) = &module.global_bindings["abc"];
    let expected_type = Type::Fn(FnType {
        arg_types: vec![],
        type_args: vec![],
        ret_type: Box::new(Type::Array(Box::new(Type::Bool))),
        is_variadic: false,
        is_enum_constructor: false,
    });
    assert_eq!(&expected_type, typ);

    let module = test_typecheck("func abc(): Bool[]? = None")?;
    let ScopeBinding(_, typ, _) = &module.global_bindings["abc"];
    let expected_type = Type::Fn(FnType {
        arg_types: vec![],
        type_args: vec![],
        ret_type: Box::new(Type::Option(Box::new(Type::Array(Box::new(Type::Bool))))),
        is_variadic: false,
        is_enum_constructor: false,
    });
    assert_eq!(&expected_type, typ);

    Ok(())
}

#[test]
fn typecheck_function_decl_args() -> TestResult {
    let module = test_typecheck("func abc(a: Int): Int = 123")?;
    let args = match module.typed_nodes.first().unwrap() {
        TypedAstNode::FunctionDecl(_, TypedFunctionDeclNode { args, .. }) => args,
        _ => panic!("Node must be a FunctionDecl")
    };
    let expected = vec![
        (ident_token!((1, 10), "a"), Type::Int, false, None)
    ];
    assert_eq!(&expected, args);

    let module = test_typecheck("func abc(a: Int, b: Bool?, c: Int[]): Int = 123")?;
    let args = match module.typed_nodes.first().unwrap() {
        TypedAstNode::FunctionDecl(_, TypedFunctionDeclNode { args, .. }) => args,
        _ => panic!("Node must be a FunctionDecl")
    };
    let expected = vec![
        (ident_token!((1, 10), "a"), Type::Int, false, None),
        (ident_token!((1, 18), "b"), Type::Option(Box::new(Type::Bool)), false, None),
        (ident_token!((1, 28), "c"), Type::Array(Box::new(Type::Int)), false, None),
    ];
    assert_eq!(&expected, args);

    let module = test_typecheck("func abc(a: Int = 1, b = [1, 2, 3]): Int = 123")?;
    let args = match module.typed_nodes.first().unwrap() {
        TypedAstNode::FunctionDecl(_, TypedFunctionDeclNode { args, .. }) => args,
        _ => panic!("Node must be a FunctionDecl")
    };
    let expected = vec![
        (ident_token!((1, 10), "a"), Type::Int, false, Some(int_literal!((1, 19), 1))),
        (ident_token!((1, 22), "b"), Type::Array(Box::new(Type::Int)), false, Some(
            TypedAstNode::Array(
                Token::LBrack(Position::new(1, 26), false),
                TypedArrayNode {
                    typ: Type::Array(Box::new(Type::Int)),
                    items: vec![
                        int_literal!((1, 27), 1),
                        int_literal!((1, 30), 2),
                        int_literal!((1, 33), 3),
                    ],
                },
            )
        )),
    ];
    assert_eq!(&expected, args);

    // A function with default-valued arguments beyond those required should still be acceptable
    let result = test_typecheck(r#"
          func call(fn: (Int) => Int, value: Int): Int = fn(value)
          func incr(v: Int, incBy = 3): Int = v + incBy
          call(incr, 21)
        "#);
    assert!(result.is_ok());

    Ok(())
}

#[test]
fn typecheck_function_decl_args_error() {
    let error = test_typecheck("func abc(a: Int, a: Bool) = 123").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateBinding {
        ident: ident_token!((1, 18), "a"),
        orig_ident: Some(ident_token!((1, 10), "a")),
    };
    assert_eq!(expected, error);

    let error = test_typecheck("func abc(a: Int, b: Bool = \"hello\") = 123").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::String(Position::new(1, 28), "hello".to_string()),
        expected: Type::Bool,
        actual: Type::String,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("func abc(a: Int, b = 1, c: Int) = 123").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidRequiredArgPosition(ident_token!((1, 25), "c"));
    assert_eq!(expected, error);

    let error = test_typecheck("func abc(self, a: Int, b = 1, c: Int) = 123").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidSelfParam { token: Token::Self_(Position::new(1, 10)) };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_function_decl_varargs() -> TestResult {
    let module = test_typecheck("func abc(*a: Int[]): Int = 123")?;
    let args = match module.typed_nodes.first().unwrap() {
        TypedAstNode::FunctionDecl(_, TypedFunctionDeclNode { args, .. }) => args,
        _ => panic!("Node must be a FunctionDecl")
    };
    let expected = vec![
        (
            ident_token!((1, 11), "a"),
            Type::Array(Box::new(Type::Int)),
            true,
            Some(TypedAstNode::Array(Token::LBrack(Position::new(1, 11), false), TypedArrayNode { typ: Type::Array(Box::new(Type::Int)), items: vec![] }))
        )
    ];
    assert_eq!(&expected, args);
    let ScopeBinding(_, typ, _) = &module.global_bindings["abc"];
    let expected_type = Type::Fn(FnType {
        arg_types: vec![("a".to_string(), Type::Array(Box::new(Type::Int)), true)],
        type_args: vec![],
        ret_type: Box::new(Type::Int),
        is_variadic: true,
        is_enum_constructor: false,
    });
    assert_eq!(&expected_type, typ);

    Ok(())
}

#[test]
fn typecheck_function_decl_varargs_errors() {
    let err = test_typecheck("func abc(*a: Int): Int = 123").unwrap_err();
    let expected = TypecheckerErrorKind::VarargMismatch {
        token: ident_token!((1, 11), "a"),
        typ: Type::Int,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("func abc(*a: Int[], b: Int): Int = 123").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidVarargPosition(ident_token!((1, 11), "a"));
    assert_eq!(expected, err);
}

#[test]
fn typecheck_function_decl_errors() {
    let err = test_typecheck("func println() = 123").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateBinding {
        ident: Token::Ident(Position::new(1, 6), "println".to_string()),
        orig_ident: None,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("func myFunc() = 123 + true").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidOperator {
        token: Token::Plus(Position::new(1, 21)),
        ltype: Type::Int,
        op: BinaryOp::Add,
        rtype: Type::Bool,
    };
    assert_eq!(expected, err);

    let error = test_typecheck("func abc(a: Int): Bool = 123").unwrap_err();
    let expected = TypecheckerErrorKind::ReturnTypeMismatch {
        token: Token::Int(Position::new(1, 26), 123),
        fn_name: "abc".to_string(),
        bare_return: false,
        expected: Type::Bool,
        actual: Type::Int,
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_function_decl_recursion() -> TestResult {
    let module = test_typecheck("func abc(): Int {\nabc()\n}")?;
    let ScopeBinding(_, typ, _) = &module.global_bindings["abc"];
    let expected_type = Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::Int), is_variadic: false, is_enum_constructor: false });
    assert_eq!(&expected_type, typ);

    let is_recursive = match module.typed_nodes.first().unwrap() {
        TypedAstNode::FunctionDecl(_, TypedFunctionDeclNode { is_recursive, .. }) => is_recursive,
        _ => panic!("Node must be a FunctionDecl")
    };
    assert_eq!(&true, is_recursive);

    Ok(())
}

#[test]
fn typecheck_function_decl_inner_function() -> TestResult {
    let module = test_typecheck("func a(): Int {\nfunc b(): Int { 1 }\n b()\n}")?;
    let func = match module.typed_nodes.first().unwrap() {
        TypedAstNode::FunctionDecl(_, func) => func,
        _ => panic!("Node must be a FunctionDecl")
    };
    assert_eq!(Type::Int, func.ret_type);

    Ok(())
}

#[test]
fn typecheck_function_decl_inner_function_err() {
    let error = test_typecheck("func a(): Int {\nfunc b(): Int { 1 }\n b + 1\n}").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidOperator {
        token: Token::Plus(Position::new(3, 4)),
        op: BinaryOp::Add,
        ltype: Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::Int), is_variadic: false, is_enum_constructor: false }),
        rtype: Type::Int,
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_function_decl_ordering() {
    // Test root level
    let res = test_typecheck(r#"
          func abc(): Int = def()
          func def(): Int = 5
        "#);
    assert!(res.is_ok());

    // Test nested within function scope
    let res = test_typecheck(r#"
          func abc() {
            func def(): Int = ghi()
            func ghi(): Int = 5
          }
        "#);
    assert!(res.is_ok());

    // Test nested within if-block/else-block
    let res = test_typecheck(r#"
          if true {
            func abc(): Int = def()
            func def(): Int = 5
          } else {
            func abc(): Int = def()
            func def(): Int = 5
          }
        "#);
    assert!(res.is_ok());

    let err = test_typecheck("\
          if true {\n\
            if true {\n\
              abc()\n\
            }\n\
            func abc() = println(\"\")\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownIdentifier {
        ident: ident_token!((3, 1), "abc")
    };
    assert_eq!(expected, err);

    // Test nested within match block
    let res = test_typecheck(r#"
          match "asdf" {
            String => {
              func abc(): Int = def()
              func def(): Int = 5
            }
          }
        "#);
    assert!(res.is_ok());

    // Test nested within for-loop
    let res = test_typecheck(r#"
          for i in [1, 2] {
            func def(): Int = ghi()
            func ghi(): Int = 5
          }
        "#);
    assert!(res.is_ok());

    // Test nested within while-loop
    let res = test_typecheck(r#"
          while true {
            func def(): Int = ghi()
            func ghi(): Int = 5
          }
        "#);
    assert!(res.is_ok());
}

#[test]
fn typecheck_function_decl_generics() -> TestResult {
    let module = test_typecheck(r#"
          func abc<T>(t: T): T { t }
          val a = abc(123)
          a
        "#)?;
    let typ = module.typed_nodes.last().unwrap().get_type();
    assert_eq!(Type::Int, typ);

    let module = test_typecheck(r#"
          func abc<T, U>(t: T, u: U): U { u }
          val a = abc(123, "asdf")
          a
        "#)?;
    let typ = module.typed_nodes.last().unwrap().get_type();
    assert_eq!(Type::String, typ);

    let module = test_typecheck(r#"
          func map<T, U>(arr: T[], fn: (T) => U): U[] { [] }
          val a = map([0, 1, 2], x => x + "!")
          a
        "#)?;
    let typ = module.typed_nodes.last().unwrap().get_type();
    assert_eq!(Type::Array(Box::new(Type::String)), typ);

    let module = test_typecheck(r#"
          func map<T, U>(arr: T[], fn: (T) => U): U[] { [] }
          val a = map([[0, 1], [2, 3]], a => a.length)
          a
        "#)?;
    let typ = module.typed_nodes.last().unwrap().get_type();
    assert_eq!(Type::Array(Box::new(Type::Int)), typ);

    let module = test_typecheck(r#"
          func map<T, U>(arr: T[], fn: (T) => U): (T) => U[] { t => [] }
          val a = map([[0, 1], [2, 3]], a => a.length)
          a
        "#)?;
    let typ = module.typed_nodes.last().unwrap().get_type();
    let expected = Type::Fn(FnType {
        arg_types: vec![("_".to_string(), Type::Array(Box::new(Type::Int)), false)],
        type_args: vec![],
        ret_type: Box::new(Type::Array(Box::new(Type::Int))),
        is_variadic: false,
        is_enum_constructor: false,
    });
    assert_eq!(expected, typ);

    // Verify generic resolution works for maps
    let module = test_typecheck(r#"
          func abc<T>(item: T): Map<String, T> = { a: item }
          val a = abc("hello")["a"]
          a
        "#)?;
    let typ = module.typed_nodes.last().unwrap().get_type();
    assert_eq!(Type::Option(Box::new(Type::String)), typ);

    // Verify generic resolution works for named arguments too
    let module = test_typecheck(r#"
          func map<T, U>(arr: T[], fn: (T) => U): (T) => U[] { t => [] }
          val a = map(fn: a => a.length, arr: [[0, 1], [2, 3]])
          a
        "#)?;
    let typ = module.typed_nodes.last().unwrap().get_type();
    let expected = Type::Fn(FnType {
        arg_types: vec![("_".to_string(), Type::Array(Box::new(Type::Int)), false)],
        type_args: vec![],
        ret_type: Box::new(Type::Array(Box::new(Type::Int))),
        is_variadic: false,
        is_enum_constructor: false,
    });
    assert_eq!(expected, typ);

    // Verify generic resolution works for union types
    let module = test_typecheck(r#"
          func cos<T>(angle: T | Float): T | Float = angle
          cos(1.23)
        "#)?;
    let typ = module.typed_nodes.last().unwrap().get_type();
    let expected = Type::Union(vec![Type::Float, Type::Float]); // <- Redundant, I know
    assert_eq!(expected, typ);
    let module = test_typecheck(r#"
          func cos<T>(angle: T | Float): T | Float = angle
          cos(12)
        "#)?;
    let typ = module.typed_nodes.last().unwrap().get_type();
    let expected = Type::Union(vec![Type::Int, Type::Float]);
    assert_eq!(expected, typ);

    Ok(())
}

#[test]
fn typecheck_function_decl_generics_errors() {
    let error = test_typecheck("\
          func map<T, U, T>(arr: T[], fn: (T) => U): U[] { [] }\n\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateTypeArgument {
        ident: ident_token!((1, 16), "T"),
        orig_ident: ident_token!((1, 10), "T"),
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          func map<T, U, V>(arr: T[], fn: (T) => U): V[] { [] }\n\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnboundGeneric(ident_token!((1, 44), "V"), "V".to_string());
    assert_eq!(expected, error);

    let error = test_typecheck("\
          func map<T, U>(arr: T[], fn: (T) => U): (T) => U[] { () => [] }\n\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::IncorrectArity {
        token: Token::Arrow(Position::new(1, 57)),
        expected: 1,
        actual: 0,
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_type_decl() -> TestResult {
    let module = test_typecheck("type Person { name: String }")?;
    let expected = TypedAstNode::TypeDecl(
        Token::Type(Position::new(1, 1)),
        TypedTypeDeclNode {
            name: ident_token!((1, 6), "Person"),
            fields: vec![
                TypedTypeDeclField {
                    ident: ident_token!((1, 15), "name"),
                    typ: Type::String,
                    default_value: None,
                }
            ],
            static_fields: vec![],
            methods: vec![],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    assert_eq!(Type::Reference("_test/Person".to_string(), vec![]), module.types["Person"]);
    let expected_type = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![StructTypeField { name: "name".to_string(), typ: Type::String, has_default_value: false, readonly: false }],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/Person"]);

    let module = test_typecheck("type Person { name: String, age: Int = 0 }")?;
    let expected = TypedAstNode::TypeDecl(
        Token::Type(Position::new(1, 1)),
        TypedTypeDeclNode {
            name: ident_token!((1, 6), "Person"),
            fields: vec![
                TypedTypeDeclField {
                    ident: ident_token!((1, 15), "name"),
                    typ: Type::String,
                    default_value: None,
                },
                TypedTypeDeclField {
                    ident: ident_token!((1, 29), "age"),
                    typ: Type::Int,
                    default_value: Some(int_literal!((1, 40), 0)),
                },
            ],
            static_fields: vec![],
            methods: vec![],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    assert_eq!(Type::Reference("_test/Person".to_string(), vec![]), module.types["Person"]);
    let expected_type = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "name".to_string(), typ: Type::String, has_default_value: false, readonly: false },
            StructTypeField { name: "age".to_string(), typ: Type::Int, has_default_value: true, readonly: false },
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/Person"]);

    Ok(())
}

#[test]
fn typecheck_type_decl_self_referencing() -> TestResult {
    let module = test_typecheck("\
          type Node {\n\
            value: Int\n\
            next: Node? = None\n\
          }\n\
          val node = Node(value: 1, next: Node(value: 2))\n\
          node\n\
        ")?;

    let expected = identifier!((6, 1), "node", Type::Reference("_test/Node".to_string(), vec![]), 0);
    assert_eq!(expected, module.typed_nodes[2]);
    let expected_type = Type::Struct(StructType {
        name: "Node".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "value".to_string(), typ: Type::Int, has_default_value: false, readonly: false },
            StructTypeField {
                name: "next".to_string(),
                typ: Type::Option(Box::new(Type::Reference("_test/Node".to_string(), vec![]))),
                has_default_value: true,
                readonly: false,
            },
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/Node"]);

    Ok(())
}

#[test]
fn typecheck_type_decl_readonly_fields() -> TestResult {
    let module = test_typecheck("\
          type Foo {\n\
            a: Int\n\
            b: Int readonly \n\
          }\
        ")?;
    let expected_type = Type::Struct(StructType {
        name: "Foo".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "a".to_string(), typ: Type::Int, has_default_value: false, readonly: false },
            StructTypeField { name: "b".to_string(), typ: Type::Int, has_default_value: false, readonly: true },
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/Foo"]);

    Ok(())
}

#[test]
fn typecheck_type_decl_errors() {
    let error = test_typecheck("type Person { name: Huh }").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownType { type_ident: ident_token!((1, 21), "Huh") };
    assert_eq!(expected, error);

    let error = test_typecheck("type Person { age: Int, age: String }").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateField { orig_ident: ident_token!((1, 15), "age"), ident: ident_token!((1, 25), "age"), orig_is_field: true, orig_is_enum_variant: false };
    assert_eq!(expected, error);

    let error = test_typecheck("type Person { age: String = true }").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch { token: Token::Bool(Position::new(1, 29), true), expected: Type::String, actual: Type::Bool };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          func abc() {\n\
            type Person { age: String }\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidTypeDeclDepth { token: Token::Type(Position::new(2, 1)) };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          type Person {\n\
            age: String\n\
            func toString(self): Int = 16\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidProtocolMethod {
        token: ident_token!((3, 6), "toString"),
        fn_name: "toString".to_string(),
        expected: Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }),
        actual: Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::Int), is_variadic: false, is_enum_constructor: false }),
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_type_decl_ordering() {
    let res = test_typecheck(r#"
          type Person { name: Name }
          type Name { first: String }
        "#);
    assert!(res.is_ok());
}

#[test]
fn typecheck_type_decl_methods() -> TestResult {
    let input = "\
          type Person {\n\
            name: String\n\
            func getName(self): String = self.name\n\
            func getName2(self): String = self.getName()\n\
          }
        ";
    let module = test_typecheck(input)?;
    let person_type = Type::Reference("_test/Person".to_string(), vec![]);
    let expected = TypedAstNode::TypeDecl(
        Token::Type(Position::new(1, 1)),
        TypedTypeDeclNode {
            name: ident_token!((1, 6), "Person"),
            fields: vec![
                TypedTypeDeclField {
                    ident: ident_token!((2, 1), "name"),
                    typ: Type::String,
                    default_value: None,
                },
            ],
            static_fields: vec![],
            methods: vec![
                (
                    "getName".to_string(),
                    TypedAstNode::FunctionDecl(
                        Token::Func(Position::new(3, 1)),
                        TypedFunctionDeclNode {
                            name: Token::Ident(Position::new(3, 6), "getName".to_string()),
                            args: vec![
                                (Token::Self_(Position { line: 3, col: 14 }), person_type.clone(), false, None)
                            ],
                            ret_type: Type::String,
                            body: vec![
                                TypedAstNode::Accessor(
                                    Token::Dot(Position::new(3, 34)),
                                    TypedAccessorNode {
                                        typ: Type::String,
                                        target: Box::new(TypedAstNode::Identifier(
                                            Token::Self_(Position::new(3, 30)),
                                            TypedIdentifierNode {
                                                typ: person_type.clone(),
                                                name: "self".to_string(),
                                                scope_depth: 2,
                                                is_mutable: false,
                                            },
                                        )),
                                        field_ident: ident_token!((3, 35), "name"),
                                        field_idx: 0,
                                        is_opt_safe: false,
                                        is_method: false,
                                        is_readonly: false,
                                    },
                                )
                            ],
                            scope_depth: 1,
                            is_recursive: false,
                        },
                    ),
                ),
                (
                    "getName2".to_string(),
                    TypedAstNode::FunctionDecl(
                        Token::Func(Position::new(4, 1)),
                        TypedFunctionDeclNode {
                            name: Token::Ident(Position::new(4, 6), "getName2".to_string()),
                            args: vec![
                                (Token::Self_(Position { line: 4, col: 15 }), person_type.clone(), false, None)
                            ],
                            ret_type: Type::String,
                            body: vec![
                                TypedAstNode::Invocation(
                                    Token::LParen(Position::new(4, 43), false),
                                    TypedInvocationNode {
                                        typ: Type::String,
                                        args: vec![],
                                        target: Box::new(
                                            TypedAstNode::Accessor(
                                                Token::Dot(Position::new(4, 35)),
                                                TypedAccessorNode {
                                                    typ: Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }),
                                                    target: Box::new(TypedAstNode::Identifier(
                                                        Token::Self_(Position::new(4, 31)),
                                                        TypedIdentifierNode {
                                                            typ: person_type.clone(),
                                                            name: "self".to_string(),
                                                            scope_depth: 2,
                                                            is_mutable: false,
                                                        },
                                                    )),
                                                    field_ident: ident_token!((4, 36), "getName"),
                                                    field_idx: 1,
                                                    is_opt_safe: false,
                                                    is_method: true,
                                                    is_readonly: true,
                                                },
                                            )
                                        ),
                                    },
                                )
                            ],
                            scope_depth: 1,
                            is_recursive: false,
                        },
                    ),
                ),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    let expected_type = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "name".to_string(), typ: Type::String, has_default_value: false, readonly: false }
        ],
        static_fields: vec![],
        methods: vec![
            to_string_method_type(),
            ("getName".to_string(), Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false })),
            ("getName2".to_string(), Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false })),
        ],
    });
    Ok(assert_eq!(expected_type, module.referencable_types["_test/Person"]))
}

#[test]
fn typecheck_type_decl_static_methods() -> TestResult {
    let input = "\
          type Person {\n\
            name: String\n\
            func getName(): String = \"hello\"\n\
          }
        ";
    let module = test_typecheck(input)?;
    let expected = TypedAstNode::TypeDecl(
        Token::Type(Position::new(1, 1)),
        TypedTypeDeclNode {
            name: ident_token!((1, 6), "Person"),
            fields: vec![
                TypedTypeDeclField {
                    ident: ident_token!((2, 1), "name"),
                    typ: Type::String,
                    default_value: None,
                },
            ],
            static_fields: vec![
                (
                    Token::Func(Position::new(3, 1)),
                    Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }),
                    Some(TypedAstNode::FunctionDecl(
                        Token::Func(Position::new(3, 1)),
                        TypedFunctionDeclNode {
                            name: Token::Ident(Position::new(3, 6), "getName".to_string()),
                            args: vec![],
                            ret_type: Type::String,
                            body: vec![
                                string_literal!((3, 26), "hello")
                            ],
                            scope_depth: 1,
                            is_recursive: false,
                        },
                    )),
                ),
            ],
            methods: vec![],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);
    let expected_type = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "name".to_string(), typ: Type::String, has_default_value: false, readonly: false }
        ],
        static_fields: vec![
            ("getName".to_string(), Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }), true),
        ],
        methods: vec![to_string_method_type()],
    });
    Ok(assert_eq!(expected_type, module.referencable_types["_test/Person"]))
}

#[test]
fn typecheck_type_decl_methods_errors() {
    let input = "\
          type Person {\n\
            func hello(self): String = \"hello\"\n\
            func hello(self): String = \"hello\"\n\
          }
        ";
    let error = test_typecheck(input).unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateField {
        orig_ident: ident_token!((2, 6), "hello"),
        ident: ident_token!((3, 6), "hello"),
        orig_is_field: false,
        orig_is_enum_variant: false,
    };
    assert_eq!(expected, error);

    let input = "\
          type Person {\n\
            func hello(self, self): String = \"hello\"\n\
          }
        ";
    let error = test_typecheck(input).unwrap_err();
    let expected = TypecheckerErrorKind::InvalidSelfParamPosition { token: Token::Self_(Position::new(2, 18)) };
    assert_eq!(expected, error);

    // TODO: This test passes, since it's _technically_ ok to have a bare expression in a Unit-returning function. There should maybe be a warning about this though, because it's pretty misleading
    // let input = "\
    //   type Person {\n\
    //     func hello(self) = \"hello\"\n\
    //   }
    // ";
    // let error = test_typecheck(input).unwrap_err();
    // let expected = TypecheckerError::ReturnTypeMismatch {
    //     token: Token::String(Position::new(2, 20), "hello".to_string()),
    //     fn_name: "hello".to_string(),
    //     fn_missing_ret_ann: true,
    //     bare_return: false,
    //     expected: Type::Unit,
    //     actual: Type::String,
    // };
    // assert_eq!(expected, error);
}

#[test]
fn typecheck_type_decl_generics() -> TestResult {
    let input = "type List<T> { items: T[] }";
    let module = test_typecheck(input)?;
    let expected_type = Type::Struct(StructType {
        name: "List".to_string(),
        type_args: vec![("T".to_string(), Type::Generic("T".to_string()))],
        constructable: true,
        fields: vec![
            StructTypeField { name: "items".to_string(), typ: Type::Array(Box::new(Type::Generic("T".to_string()))), has_default_value: false, readonly: false }
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/List"]);

    let input = r#"
          type List<T> { items: T[] }
          val l = List(items: [1, 2, 3])
          l
        "#;
    let module = test_typecheck(input)?;
    let actual_type = module.typed_nodes.last().unwrap().get_type();
    let expected_type = Type::Reference("_test/List".to_string(), vec![Type::Int]);
    assert_eq!(expected_type, actual_type);

    let input = r#"
          type List<T> { items: T[] }
          val l: List<Int> = List(items: [1, 2, 3])
        "#;
    let res = test_typecheck(input);
    assert!(res.is_ok());

    let input = r#"
          type List<T> { items: T[] }
          val l = List<Int>(items: [1, 2, 3])
        "#;
    let res = test_typecheck(input);
    assert!(res.is_ok());

    let input = r#"
          type List<T> { items: T[] }
          val l: List<Int> = List(items: [])
        "#;
    let res = test_typecheck(input);
    assert!(res.is_ok());

    let input = r#"
          type List<T> { items: T[] }
          val l = List<Int>(items: [])
        "#;
    let res = test_typecheck(input);
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_type_decl_generics_errors() {
    let input = "type List<T, U, T> { items: T[] }";
    let error = test_typecheck(input).unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateTypeArgument {
        ident: ident_token!((1, 17), "T"),
        orig_ident: ident_token!((1, 11), "T"),
    };
    assert_eq!(expected, error);

    let input = "\
          type List<T> { items: T[] }\n\
          val l = List(items: [])\
        ";
    let error = test_typecheck(input).unwrap_err();
    let expected = TypecheckerErrorKind::UnboundGeneric(ident_token!((2, 5), "l"), "T".to_string());
    assert_eq!(expected, error);

    let input = "\
          type List<T> { items: T[] }\n\
          val l: List<String> = List(items: [1, 2, 3])\
        ";
    let error = test_typecheck(input).unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::LParen(Position::new(2, 27), false),
        expected: Type::Reference("_test/List".to_string(), vec![Type::String]),
        actual: Type::Reference("_test/List".to_string(), vec![Type::Int]),
    };
    assert_eq!(expected, error);

    let input = "\
          type List<T> { items: T[] }\n\
          val l = List<String>(items: [1, 2, 3])\
        ";
    let error = test_typecheck(input).unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::LBrack(Position::new(2, 29), false),
        expected: Type::Array(Box::new(Type::String)),
        actual: Type::Array(Box::new(Type::Int)),
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_type_decl_generics_fields_and_methods() -> TestResult {
    let type_def = r#"
          type List<T> {
            items: T[] = []
            func push(self, item: T): List<T> {
              self.items.push(item)
              self
            }
            func map<U>(self, fn: (T) => U): List<U> {
              List(items: self.items.map(fn))
            }
            func concat(self, other: List<T>): List<T> {
              List(items: self.items.concat(other.items))
            }
            func reduce<U>(self, initialValue: U, fn: (U, T) => U): U {
              var acc = initialValue
              for item in self.items { acc = fn(acc, item) }
              acc
            }
          }
        "#;

    let input = format!(r#"{}
          val l: List<Int> = List(items: [1, 2, 3])
          val items: Int[] = l.items
        "#, type_def);
    let res = test_typecheck(&input);
    assert!(res.is_ok());

    let input = format!(r#"{}
          val l: List<Int> = List(items: [1, 2, 3])
          l.push(4)
        "#, type_def);
    let res = test_typecheck(&input);
    assert!(res.is_ok());

    let input = format!(r#"{}
          val l: List<String> = List(items: [])
          l.push("abc")
        "#, type_def);
    let res = test_typecheck(&input);
    assert!(res.is_ok());

    let input = format!(r#"{}
          val l: List<String> = List()
          l.push("abc")
        "#, type_def);
    let res = test_typecheck(&input);
    assert!(res.is_ok());

    let input = format!(r#"{}
          val l: List<String> = List(items: [])
          val lengths: List<Int> = l.map(s => s.length)
        "#, type_def);
    let res = test_typecheck(&input);
    assert!(res.is_ok());

    let input = format!(r#"{}
          List(items: [1, 2, 3]).concat(List(items: [4, 5, 6]))
        "#, type_def);
    let res = test_typecheck(&input);
    assert!(res.is_ok());

    let input = format!(r#"{}
          List(items: [1, 2, 3]).reduce<String[]>([], (acc, i) => {{
            acc.push(i + "!")
            acc
          }})
        "#, type_def);
    let res = test_typecheck(&input);
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_type_decl_generics_fields_and_methods_errors() {
    let type_def = "\
          type List<T> {\n\
            items: T[]\n\
            func push(self, item: T): T[] {\n\
              self.items.push(item)\n\
              self.items\n\
            }\n\
            func reduce<U>(self, initialValue: U, fn: (U, T) => U): U {\n\
              initialValue\n\
            }\n\
          }\
        ";

    let input = format!("{}\n\
          val l: List<Int> = List(items: [1, 2, 3])\n\
          l.items.push(\"abcd\")\
        ", type_def);
    let error = test_typecheck(&input).unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::String(Position::new(12, 14), "abcd".to_string()),
        expected: Type::Int,
        actual: Type::String,
    };
    assert_eq!(expected, error);

    let input = format!("{}\n\
          val l: List<Int> = List(items: [1, 2, 3])\n\
          l.push(\"abcd\")\
        ", type_def);
    let error = test_typecheck(&input).unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::String(Position::new(12, 8), "abcd".to_string()),
        expected: Type::Int,
        actual: Type::String,
    };
    assert_eq!(expected, error);

    let input = format!("{}\n\
          val l = List(items: [1, 2, 3])\n\
          l.reduce<Int>(\"\", (acc, i) => acc + i)\
        ", type_def);
    let error = test_typecheck(&input).unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::String(Position::new(12, 15), "".to_string()),
        expected: Type::Int,
        actual: Type::String,
    };
    assert_eq!(expected, error);

    let list_struct_type = Type::Struct(StructType {
        name: "List".to_string(),
        type_args: vec![("T".to_string(), Type::Generic("T".to_string()))],
        constructable: true,
        fields: vec![
            StructTypeField { name: "items".to_string(), typ: Type::Array(Box::new(Type::Generic("T".to_string()))), has_default_value: false, readonly: false }
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    let input = "\
          type List<T> { items: T[] }\n\
          val l: List = List(items: [1, 2, 3])\
        ";
    let error = test_typecheck(&input).unwrap_err();
    let expected = TypecheckerErrorKind::InvalidTypeArgumentArity {
        token: ident_token!((2, 8), "List"),
        actual_type: list_struct_type.clone(),
        expected: 1,
        actual: 0,
    };
    assert_eq!(expected, error);

    let input = "\
          type List<T> { items: T[] }\n\
          val l: List<Int, Int> = List(items: [1, 2, 3])\
        ";
    let error = test_typecheck(&input).unwrap_err();
    let expected = TypecheckerErrorKind::InvalidTypeArgumentArity {
        token: ident_token!((2, 8), "List"),
        actual_type: list_struct_type,
        expected: 1,
        actual: 2,
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_enum_decl() -> TestResult {
    let module = test_typecheck("enum Temp { Hot, Cold }")?;
    let expected = TypedAstNode::EnumDecl(
        Token::Enum(Position::new(1, 1)),
        TypedEnumDeclNode {
            name: ident_token!((1, 6), "Temp"),
            static_fields: vec![],
            methods: vec![],
            variants: vec![
                (
                    ident_token!((1, 13), "Hot"),
                    (Type::Reference("_test/Temp".to_string(), vec![]), None)
                ),
                (
                    ident_token!((1, 18), "Cold"),
                    (Type::Reference("_test/Temp".to_string(), vec![]), None)
                ),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("\
          enum AngleMode { Radians(rads: Float), Degrees(degs: Float) }\
        ")?;
    let expected = TypedAstNode::EnumDecl(
        Token::Enum(Position::new(1, 1)),
        TypedEnumDeclNode {
            name: ident_token!((1, 6), "AngleMode"),
            static_fields: vec![],
            methods: vec![],
            variants: vec![
                (
                    ident_token!((1, 18), "Radians"),
                    (
                        Type::Fn(FnType {
                            arg_types: vec![("rads".to_string(), Type::Float, false)],
                            type_args: vec![],
                            ret_type: Box::new(Type::Reference("_test/AngleMode".to_string(), vec![])),
                            is_variadic: false,
                            is_enum_constructor: true,
                        }),
                        Some(vec![None])
                    )
                ),
                (
                    ident_token!((1, 40), "Degrees"),
                    (
                        Type::Fn(FnType {
                            arg_types: vec![("degs".to_string(), Type::Float, false)],
                            type_args: vec![],
                            ret_type: Box::new(Type::Reference("_test/AngleMode".to_string(), vec![])),
                            is_variadic: false,
                            is_enum_constructor: true,
                        }),
                        Some(vec![None])
                    )
                ),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    // Verify that constructor variants can have default arg values that are themselves enum variants
    let module = test_typecheck("\
          enum Color { Red, Darken(base: Color = Color.Red, amount: Float = 10.0) }\
        ")?;
    let expected = TypedAstNode::EnumDecl(
        Token::Enum(Position::new(1, 1)),
        TypedEnumDeclNode {
            name: ident_token!((1, 6), "Color"),
            methods: vec![],
            static_fields: vec![],
            variants: vec![
                (
                    ident_token!((1, 14), "Red"),
                    (Type::Reference("_test/Color".to_string(), vec![]), None)
                ),
                (
                    ident_token!((1, 19), "Darken"),
                    (
                        Type::Fn(FnType {
                            arg_types: vec![
                                ("base".to_string(), Type::Reference("_test/Color".to_string(), vec![]), false),
                                ("amount".to_string(), Type::Float, false),
                            ],
                            type_args: vec![],
                            ret_type: Box::new(Type::Reference("_test/Color".to_string(), vec![])),
                            is_variadic: false,
                            is_enum_constructor: true,
                        }),
                        Some(vec![
                            Some(TypedAstNode::Accessor(
                                Token::Dot(Position::new(1, 45)),
                                TypedAccessorNode {
                                    typ: Type::Reference("_test/Color".to_string(), vec![]),
                                    target: Box::new(identifier!(
                                            (1, 40),
                                            "Color",
                                            Type::Type(
                                                "_test/Color".to_string(),
                                                Box::new(Type::Reference("_test/Color".to_string(), vec![])),
                                                true,
                                            ),
                                            0
                                        )),
                                    field_idx: 0,
                                    field_ident: ident_token!((1, 46), "Red"),
                                    is_opt_safe: false,
                                    is_method: false,
                                    is_readonly: true,
                                },
                            )),
                            Some(float_literal!((1, 67), 10.0)),
                        ])
                    )
                ),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    Ok(())
}

#[test]
fn typecheck_enum_decl_errors() {
    let error = test_typecheck("enum Temp { Hot, Hot }").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateField {
        ident: ident_token!((1, 18), "Hot"),
        orig_ident: ident_token!((1, 13), "Hot"),
        orig_is_field: false,
        orig_is_enum_variant: true,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          enum Temp { Hot, Cold }\n\
          enum Temp { Hot, Cold, Tepid }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateType {
        ident: ident_token!((2, 6), "Temp"),
        orig_ident: Some(ident_token!((1, 6), "Temp")),
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          enum Temp { Hot(temp: Int, temp: Int), Cold }\n\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateBinding {
        ident: ident_token!((1, 28), "temp"),
        orig_ident: Some(ident_token!((1, 17), "temp")),
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          enum Temp { Hot(self), Cold }\n\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidSelfParam {
        token: Token::Self_(Position::new(1, 17))
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          enum Temp { Hot(*degs: Int[]), Cold }\n\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidVarargUsage(ident_token!((1, 18), "degs"));
    assert_eq!(expected, error);
}

#[test]
fn typecheck_enum_decl_ordering() {
    let res = test_typecheck(r#"
          type Qux { foo1: Foo1, foo2: Foo2 }
          enum Foo1 { Bar(f: Foo2), Baz(qux: Qux) }
          enum Foo2 { Asdf, Qwer }
        "#);
    assert!(res.is_ok());
}

#[test]
fn typecheck_enum_decl_variants() {
    let is_ok = test_typecheck("\
          enum Scale { Fahrenheit, Celsius }\n\
          func isBoiling(amount: Float, scale: Scale): Float = amount\n\
          isBoiling(212.0, Scale.Fahrenheit)\
        ").is_ok();
    assert!(is_ok);

    let is_ok = test_typecheck("\
          enum Scale { Fahrenheit(degs: Float), Celsius(degs: Float) }\n\
          func isBoiling(scale: Scale): Bool = true\n\
          isBoiling(Scale.Fahrenheit(degs: 212.0))\
        ").is_ok();
    assert!(is_ok);
}

#[test]
fn typecheck_enum_decl_variants_errors() {
    let error = test_typecheck("\
          enum Scale { Fahrenheit, Celsius }\n\
          func isBoiling(amount: Float, scale: Scale = 123): Float = amount\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Int(Position::new(2, 46), 123),
        actual: Type::Int,
        expected: Type::Reference("_test/Scale".to_string(), vec![]),
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          enum Scale { Fahrenheit(degs: Float), Celsius(degs: Float) }\n\
          func isBoiling(scale: Scale): Bool = true\n\
          isBoiling(Scale.Fahrenheit(degs: \"212.0\"))\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::String(Position::new(3, 34), "212.0".to_string()),
        actual: Type::String,
        expected: Type::Float,
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_enum_decl_variants_generics() {
    let is_ok = test_typecheck("\
          enum LL<T> {\n\
            Cons(item: T, next: LL<T>)\n\
            Empty\n\
          }\n\
          LL.Cons(1, LL.Cons(2, LL.Empty))\n\
        ").is_ok();
    assert!(is_ok);
}

#[test]
fn typecheck_enum_decl_variants_generics_errors() {
    let err = test_typecheck("\
          enum LL<T> {\n\
            Cons(item: T, next: LL<T>)\n\
            Empty\n\
          }\n\
          LL.Cons(1, LL.Cons(\"2\", LL.Empty))\n\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::LParen(Position::new(5, 19), false),
        expected: Type::Reference("_test/LL".to_string(), vec![Type::Int]),
        actual: Type::Reference("_test/LL".to_string(), vec![Type::String]),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_enum_decl_methods() {
    let is_ok = test_typecheck("\
          enum Color {\n\
            Red\n\
            Green\n\
            func hexCode(self): String = \"0xffffff\"\n\
          }\n\
          val hex: String = Color.Red.hexCode()\
        ").is_ok();
    assert!(is_ok);

    // Test static methods
    let is_ok = test_typecheck("\
          enum Color {\n\
            Red\n\
            Green\n\
            func gray(): String = \"0x777\"\n\
          }\n\
          val hex: String = Color.gray()\
        ").is_ok();
    assert!(is_ok);
}

#[test]
fn typecheck_enum_decl_methods_errors() {
    let error = test_typecheck("\
          enum Color {\n\
            Red\n\
            Green\n\
            func hexCode(self): String = \"0xffffff\"\n\
          }\n\
          Color.Red.hex()\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownMember {
        token: ident_token!((6, 11), "hex"),
        target_type: Type::Reference("_test/Color".to_string(), vec![]),
        module_id: None,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          enum Color {\n\
            Red\n\
            Green\n\
            func black(self): String = \"0x000000\"\n\
          }\n\
          Color.white()\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownMember {
        token: ident_token!((6, 7), "white"),
        target_type: Type::Type(
            "_test/Color".to_string(),
            Box::new(Type::Reference("_test/Color".to_string(), vec![])),
            true,
        ),
        module_id: None,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          enum Color {\n\
            Red\n\
            func Red(): String = \"0xFF0000\"\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateField {
        ident: ident_token!((3, 6), "Red"),
        orig_ident: ident_token!((2, 1), "Red"),
        orig_is_field: false,
        orig_is_enum_variant: true,
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_enum_decl_variant_returns() {
    let is_ok = test_typecheck(r#"
          enum Foo { Bar }
          func a(): Foo = if true { b() } else { Foo.Bar }
          func b(): Foo = Foo.Bar
        "#).is_ok();
    assert!(is_ok);
}

#[test]
fn typecheck_ident() -> TestResult {
    let module = test_typecheck("val abc = 123\nabc")?;
    let expected = vec![
        TypedAstNode::BindingDecl(
            Token::Val(Position::new(1, 1)),
            TypedBindingDeclNode {
                binding: BindingPattern::Variable(ident_token!((1, 5), "abc")),
                is_mutable: false,
                expr: Some(Box::new(int_literal!((1, 11), 123))),
                scope_depth: 0,
            },
        ),
        identifier!((2, 1), "abc", Type::Int, 0),
    ];
    assert_eq!(expected, module.typed_nodes);
    Ok(())
}

#[test]
fn typecheck_ident_errors() {
    let err = test_typecheck("abc").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownIdentifier {
        ident: Token::Ident(Position::new(1, 1), "abc".to_string())
    };
    assert_eq!(expected, err);

    let err = test_typecheck("println(_)").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownIdentifier {
        ident: Token::Ident(Position::new(1, 9), "_".to_string())
    };
    assert_eq!(expected, err);

    let err = test_typecheck("var abc\nabc").unwrap_err();
    let expected = TypecheckerErrorKind::UnannotatedUninitialized {
        ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
        is_mutable: true,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_assignment_identifier() -> TestResult {
    let module = test_typecheck("var abc = 123\nabc = 456")?;
    let expected = vec![
        TypedAstNode::BindingDecl(
            Token::Var(Position::new(1, 1)),
            TypedBindingDeclNode {
                binding: BindingPattern::Variable(ident_token!((1, 5), "abc")),
                is_mutable: true,
                expr: Some(Box::new(int_literal!((1, 11), 123))),
                scope_depth: 0,
            },
        ),
        TypedAstNode::Assignment(
            Token::Assign(Position::new(2, 5)),
            TypedAssignmentNode {
                kind: AssignmentTargetKind::Identifier,
                typ: Type::Int,
                target: Box::new(identifier_mut!((2, 1), "abc", Type::Int, 0)),
                expr: Box::new(int_literal!((2, 7), 456)),
            },
        ),
    ];
    assert_eq!(expected, module.typed_nodes);
    Ok(())
}

#[test]
fn typecheck_assignment_indexing() -> TestResult {
    let module = test_typecheck("var abc = [1, 2]\nabc[0] = 2")?;
    let expected = TypedAstNode::Assignment(
        Token::Assign(Position::new(2, 8)),
        TypedAssignmentNode {
            kind: AssignmentTargetKind::ArrayIndex,
            typ: Type::Int,
            target: Box::new(TypedAstNode::Indexing(
                Token::LBrack(Position::new(2, 4), false),
                TypedIndexingNode {
                    typ: Type::Option(Box::new(Type::Int)),
                    target: Box::new(identifier_mut!((2, 1), "abc", Type::Array(Box::new(Type::Int)), 0)),
                    index: IndexingMode::Index(Box::new(int_literal!((2, 5), 0))),
                },
            )),
            expr: Box::new(int_literal!((2, 10), 2)),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("var abc = {a: 2}\nabc[\"a\"] = 3")?;
    let expected = TypedAstNode::Assignment(
        Token::Assign(Position::new(2, 10)),
        TypedAssignmentNode {
            kind: AssignmentTargetKind::MapIndex,
            typ: Type::Int,
            target: Box::new(TypedAstNode::Indexing(
                Token::LBrack(Position::new(2, 4), false),
                TypedIndexingNode {
                    typ: Type::Option(Box::new(Type::Int)),
                    target: Box::new(identifier_mut!((2, 1), "abc", Type::Map(Box::new(Type::String), Box::new(Type::Int)), 0)),
                    index: IndexingMode::Index(Box::new(string_literal!((2, 5), "a"))),
                },
            )),
            expr: Box::new(int_literal!((2, 12), 3)),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let res = test_typecheck("var abc = {a: 2, b: true}\nabc[\"c\"] = 3");
    assert!(res.is_ok());

    let res = test_typecheck("val abc = (2, true)\nabc[1] = false");
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_assignment_field() -> TestResult {
    let module = test_typecheck("\
          type Person { name: String }\n\
          val a = Person(name: \"abc\")\n\
          a.name = \"qwer\"\n\
        ")?;
    let expected = TypedAstNode::Assignment(
        Token::Assign(Position::new(3, 8)),
        TypedAssignmentNode {
            kind: AssignmentTargetKind::Field,
            typ: Type::String,
            target: Box::new(TypedAstNode::Accessor(
                Token::Dot(Position::new(3, 2)),
                TypedAccessorNode {
                    typ: Type::String,
                    target: Box::new(identifier!((3, 1), "a", Type::Reference("_test/Person".to_string(), vec![]), 0)),
                    field_idx: 0,
                    field_ident: ident_token!((3, 3), "name"),
                    is_opt_safe: false,
                    is_method: false,
                    is_readonly: false,
                },
            )),
            expr: Box::new(string_literal!((3, 10), "qwer")),
        },
    );
    assert_eq!(expected, module.typed_nodes[2]);
    let expected_type = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "name".to_string(), typ: Type::String, has_default_value: false, readonly: false }
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/Person"]);

    // Test setting inner property of readonly field
    let res = test_typecheck("\
          type Name { first: String }\n\
          type Person { name: Name readonly }\n\
          val p = Person(name: Name(first: \"Ken\"))\n\
          p.name.first = \"Meg\"\
        ").is_ok();
    assert!(res);

    Ok(())
}

#[test]
fn typecheck_assignment_errors_with_target() {
    let err = test_typecheck("true = 345").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidAssignmentTarget { token: Token::Assign(Position::new(1, 6)), typ: None, reason: InvalidAssignmentTargetReason::IllegalTarget };
    assert_eq!(expected, err);

    let err = test_typecheck("val abc = 345\nabc = 67").unwrap_err();
    let expected = TypecheckerErrorKind::AssignmentToImmutable {
        token: Token::Assign(Position::new(2, 5)),
        orig_ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("var abc = 345\nabc = \"str\"").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::String(Position::new(2, 7), "str".to_string()),
        expected: Type::Int,
        actual: Type::String,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val a = [1, 2]\na[0:1] = \"str\"").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidAssignmentTarget {
        token: Token::Assign(Position::new(2, 8)),
        typ: None,
        reason: InvalidAssignmentTargetReason::IndexingMode,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val a = \"abc\"\na[0] = \"qwer\"").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidAssignmentTarget {
        token: Token::Assign(Position::new(2, 6)),
        typ: Some(Type::String),
        reason: InvalidAssignmentTargetReason::StringTarget,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          type Person { name: String }\n\
          val a = Person(name: \"abc\")\n\
          a.bogusField = \"qwer\"\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownMember {
        token: ident_token!((3, 3), "bogusField"),
        target_type: Type::Reference("_test/Person".to_string(), vec![]),
        module_id: None,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          type Person { name: String }\n\
          val a = Person(name: \"abc\")\n\
          a.name = 123\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Int(Position::new(3, 10), 123),
        expected: Type::String,
        actual: Type::Int,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          type Person {\n\
            name: String\n\
            func foo(self): String = \"hello\"
          }\n\
          val a = Person(name: \"abc\")\n\
          a.foo = () => \"ahoy\"\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidAssignmentTarget {
        token: Token::Assign(Position::new(6, 7)),
        typ: None,
        reason: InvalidAssignmentTargetReason::MethodTarget,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          type Person {\n\
            name: String readonly\n\
          }\n\
          val a = Person(name: \"abc\")\n\
          a.name = \"hello\"\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidAccess {
        token: ident_token!((5, 3), "name"),
        is_field: true,
        is_get: false,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_assignment_errors_with_type() {
    let err = test_typecheck("val abc = [1, 2]\nabc[2] = \"7\"").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::String(Position::new(2, 10), "7".to_string()),
        expected: Type::Int,
        actual: Type::String,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val abc = {a: 2, b: 3}\nabc[\"b\"] = \"7\"").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::String(Position::new(2, 12), "7".to_string()),
        expected: Type::Int,
        actual: Type::String,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val abc = {a: 2, b: true}\nabc[\"b\"] = \"7\"").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::String(Position::new(2, 12), "7".to_string()),
        expected: Type::Union(vec![Type::Int, Type::Bool]),
        actual: Type::String,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_indexing() -> TestResult {
    let module = test_typecheck("val abc = [1, 2, 3]\nabc[1]")?;
    let expected = TypedAstNode::Indexing(
        Token::LBrack(Position::new(2, 4), false),
        TypedIndexingNode {
            typ: Type::Option(Box::new(Type::Int)),
            target: Box::new(identifier!((2, 1), "abc", Type::Array(Box::new(Type::Int)), 0)),
            index: IndexingMode::Index(Box::new(int_literal!((2, 5), 1))),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("val idx = 1\nval abc = [1, 2, 3]\nabc[idx:]")?;
    let expected = TypedAstNode::Indexing(
        Token::LBrack(Position::new(3, 4), false),
        TypedIndexingNode {
            typ: Type::Array(Box::new(Type::Int)),
            target: Box::new(identifier!((3, 1), "abc", Type::Array(Box::new(Type::Int)), 0)),
            index: IndexingMode::Range(
                Some(Box::new(identifier!((3, 5), "idx", Type::Int, 0))),
                None,
            ),
        },
    );
    assert_eq!(expected, module.typed_nodes[2]);

    let module = test_typecheck("val idx = 1\n\"abc\"[:idx * 2]")?;
    let expected = TypedAstNode::Indexing(
        Token::LBrack(Position::new(2, 6), false),
        TypedIndexingNode {
            typ: Type::String,
            target: Box::new(string_literal!((2, 1), "abc")),
            index: IndexingMode::Range(
                None,
                Some(Box::new(
                    TypedAstNode::Binary(
                        Token::Star(Position::new(2, 12)),
                        TypedBinaryNode {
                            typ: Type::Int,
                            op: BinaryOp::Mul,
                            left: Box::new(identifier!((2, 8), "idx", Type::Int, 0)),
                            right: Box::new(int_literal!((2, 14), 2)),
                        },
                    ),
                )),
            ),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("val map = { a: 1, b: 2 }\nmap[\"a\"]")?;
    let expected = TypedAstNode::Indexing(
        Token::LBrack(Position::new(2, 4), false),
        TypedIndexingNode {
            typ: Type::Option(Box::new(Type::Int)),
            target: Box::new(identifier!((2, 1), "map", Type::Map(Box::new(Type::String), Box::new(Type::Int)), 0)),
            index: IndexingMode::Index(Box::new(string_literal!((2, 5), "a"))),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("val map = { a: 1, b: true }\nmap[\"a\"]")?;
    let expected = TypedAstNode::Indexing(
        Token::LBrack(Position::new(2, 4), false),
        TypedIndexingNode {
            typ: Type::Option(Box::new(Type::Union(vec![Type::Int, Type::Bool]))),
            target: Box::new(identifier!(
                    (2, 1),
                    "map",
                    Type::Map(Box::new(Type::String), Box::new(Type::Union(vec![Type::Int, Type::Bool]))),
                    0
                )),
            index: IndexingMode::Index(Box::new(string_literal!((2, 5), "a"))),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("(1, 2)[0]")?;
    let expected = TypedAstNode::Indexing(
        Token::LBrack(Position::new(1, 7), false),
        TypedIndexingNode {
            typ: Type::Int,
            target: Box::new(TypedAstNode::Tuple(
                Token::LParen(Position::new(1, 1), false),
                TypedTupleNode {
                    typ: Type::Tuple(vec![Type::Int, Type::Int]),
                    items: vec![int_literal!((1, 2), 1), int_literal!((1, 5), 2)],
                },
            )),
            index: IndexingMode::Index(Box::new(int_literal!((1, 8), 0))),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    Ok(())
}

#[test]
fn typecheck_indexing_errors() {
    let err = test_typecheck("[1, 2, 3][\"a\"]").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidIndexingSelector {
        token: Token::String(Position::new(1, 11), "a".to_string()),
        target_type: Type::Array(Box::new(Type::Int)),
        selector_type: Type::String,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\"abcd\"[[1, 2]]").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidIndexingSelector {
        token: Token::LBrack(Position::new(1, 8), false),
        target_type: Type::String,
        selector_type: Type::Array(Box::new(Type::Int)),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("[1, 2, 3][\"a\":]").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::String(Position::new(1, 11), "a".to_string()),
        expected: Type::Int,
        actual: Type::String,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("[1, 2, 3][:\"a\"]").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::String(Position::new(1, 12), "a".to_string()),
        expected: Type::Int,
        actual: Type::String,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("123[0]").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidIndexingTarget {
        token: Token::LBrack(Position::new(1, 4), false),
        target_type: Type::Int,
        index_mode: IndexingMode::Index(Box::new(
            AstNode::Literal(
                Token::Int(Position::new(1, 5), 0),
                AstLiteralNode::IntLiteral(0),
            )
        )),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("val a: Int = [1, 2, 3][0]").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::LBrack(Position::new(1, 23), false),
        expected: Type::Int,
        actual: Type::Option(Box::new(Type::Int)),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("{ a: 1, b: 2 }[3]").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidIndexingSelector {
        token: Token::Int(Position::new(1, 16), 3),
        target_type: Type::Map(Box::new(Type::String), Box::new(Type::Int)),
        selector_type: Type::Int,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("{ a: true, b: 2 }[123]").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidIndexingSelector {
        token: Token::Int(Position::new(1, 19), 123),
        selector_type: Type::Int,
        target_type: Type::Map(Box::new(Type::String), Box::new(Type::Union(vec![Type::Bool, Type::Int]))),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("(1, 2)[2]").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidTupleIndexingSelector {
        token: Token::Int(Position::new(1, 8), 2),
        types: vec![Type::Int, Type::Int],
        non_constant: false,
        index: 2,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("(1, 2)[0 + 1]").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidTupleIndexingSelector {
        token: Token::Plus(Position::new(1, 10)),
        types: vec![Type::Int, Type::Int],
        non_constant: true,
        index: -1,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_if_statement() -> TestResult {
    let module = test_typecheck("if 1 < 2 1234")?;
    let expected = TypedAstNode::IfStatement(
        Token::If(Position::new(1, 1)),
        TypedIfNode {
            typ: Type::Unit,
            condition: Box::new(
                TypedAstNode::Binary(
                    Token::LT(Position::new(1, 6)),
                    TypedBinaryNode {
                        typ: Type::Bool,
                        left: Box::new(int_literal!((1, 4), 1)),
                        op: BinaryOp::Lt,
                        right: Box::new(int_literal!((1, 8), 2)),
                    },
                )
            ),
            condition_binding: None,
            if_block: vec![int_literal!((1, 10), 1234)],
            else_block: None,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("if 1 < 2 1234 else 1 + 2")?;
    let expected = TypedAstNode::IfStatement(
        Token::If(Position::new(1, 1)),
        TypedIfNode {
            typ: Type::Unit,
            condition: Box::new(
                TypedAstNode::Binary(
                    Token::LT(Position::new(1, 6)),
                    TypedBinaryNode {
                        typ: Type::Bool,
                        left: Box::new(int_literal!((1, 4), 1)),
                        op: BinaryOp::Lt,
                        right: Box::new(int_literal!((1, 8), 2)),
                    },
                )
            ),
            condition_binding: None,
            if_block: vec![int_literal!((1, 10), 1234)],
            else_block: Some(vec![
                TypedAstNode::Binary(
                    Token::Plus(Position::new(1, 22)),
                    TypedBinaryNode {
                        typ: Type::Int,
                        left: Box::new(int_literal!((1, 20), 1)),
                        op: BinaryOp::Add,
                        right: Box::new(int_literal!((1, 24), 2)),
                    },
                )
            ]),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("\
          val i: Int? = 0\n\
          if i 1 else 2\
        ")?;
    let expected = TypedAstNode::IfStatement(
        Token::If(Position::new(2, 1)),
        TypedIfNode {
            typ: Type::Unit,
            condition: Box::new(identifier!((2, 4), "i", Type::Option(Box::new(Type::Int)), 0)),
            condition_binding: None,
            if_block: vec![int_literal!((2, 6), 1)],
            else_block: Some(vec![int_literal!((2, 13), 2)]),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    Ok(())
}

#[test]
fn typecheck_if_statement_condition_binding() -> TestResult {
    let module = test_typecheck("\
          val i: Int? = 0\n\
          if i |i| i else 2\
        ")?;
    let expected = TypedAstNode::IfStatement(
        Token::If(Position::new(2, 1)),
        TypedIfNode {
            typ: Type::Unit,
            condition: Box::new(identifier!((2, 4), "i", Type::Option(Box::new(Type::Int)), 0)),
            condition_binding: Some(BindingPattern::Variable(ident_token!((2, 7), "i"))),
            if_block: vec![
                identifier!((2, 10), "i", Type::Int, 1)
            ],
            else_block: Some(vec![int_literal!((2, 17), 2)]),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("if true |v| v else false")?;
    let expected = TypedAstNode::IfStatement(
        Token::If(Position::new(1, 1)),
        TypedIfNode {
            typ: Type::Unit,
            condition: Box::new(bool_literal!((1, 4), true)),
            condition_binding: Some(BindingPattern::Variable(ident_token!((1, 10), "v"))),
            if_block: vec![
                identifier!((1, 13), "v", Type::Bool, 1)
            ],
            else_block: Some(vec![bool_literal!((1, 20), false)]),
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    Ok(())
}

#[test]
fn typecheck_if_statement_errors() {
    let error = test_typecheck("if 4 { val a = \"hello\" a }").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidIfConditionType {
        token: Token::Int(Position::new(1, 4), 4),
        actual: Type::Int,
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_if_statement_scopes() -> TestResult {
    let module = test_typecheck("if 1 < 2 { val a = \"hello\" a }")?;
    let expected = TypedAstNode::IfStatement(
        Token::If(Position::new(1, 1)),
        TypedIfNode {
            typ: Type::Unit,
            condition: Box::new(
                TypedAstNode::Binary(
                    Token::LT(Position::new(1, 6)),
                    TypedBinaryNode {
                        typ: Type::Bool,
                        left: Box::new(int_literal!((1, 4), 1)),
                        op: BinaryOp::Lt,
                        right: Box::new(int_literal!((1, 8), 2)),
                    },
                )
            ),
            condition_binding: None,
            if_block: vec![
                TypedAstNode::BindingDecl(
                    Token::Val(Position::new(1, 12)),
                    TypedBindingDeclNode {
                        binding: BindingPattern::Variable(ident_token!((1, 16), "a")),
                        is_mutable: false,
                        expr: Some(Box::new(string_literal!((1, 20), "hello"))),
                        scope_depth: 1,
                    },
                ),
                identifier!((1, 28), "a", Type::String, 1),
            ],
            else_block: None,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck(
        "val a = \"hello\"\nif 1 < 2 { val b = \"world\" a + b } else { a + \"!\" }"
    )?;
    let expected = TypedAstNode::IfStatement(
        Token::If(Position::new(2, 1)),
        TypedIfNode {
            typ: Type::Unit,
            condition: Box::new(
                TypedAstNode::Binary(
                    Token::LT(Position::new(2, 6)),
                    TypedBinaryNode {
                        typ: Type::Bool,
                        left: Box::new(int_literal!((2, 4), 1)),
                        op: BinaryOp::Lt,
                        right: Box::new(int_literal!((2, 8), 2)),
                    },
                )
            ),
            condition_binding: None,
            if_block: vec![
                TypedAstNode::BindingDecl(
                    Token::Val(Position::new(2, 12)),
                    TypedBindingDeclNode {
                        binding: BindingPattern::Variable(ident_token!((2, 16), "b")),
                        is_mutable: false,
                        expr: Some(Box::new(string_literal!((2, 20), "world"))),
                        scope_depth: 1,
                    },
                ),
                TypedAstNode::Binary(
                    Token::Plus(Position::new(2, 30)),
                    TypedBinaryNode {
                        typ: Type::String,
                        left: Box::new(identifier!((2, 28), "a", Type::String, 0)),
                        op: BinaryOp::Add,
                        right: Box::new(identifier!((2, 32), "b", Type::String, 1)),
                    },
                ),
            ],
            else_block: Some(vec![
                TypedAstNode::Binary(
                    Token::Plus(Position::new(2, 45)),
                    TypedBinaryNode {
                        typ: Type::String,
                        left: Box::new(identifier!((2, 43), "a", Type::String, 0)),
                        op: BinaryOp::Add,
                        right: Box::new(string_literal!((2, 47), "!")),
                    },
                ),
            ]),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    Ok(())
}

#[test]
fn typecheck_if_statement_scopes_errors() {
    let error = test_typecheck("if (1 < 2) { a }").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownIdentifier { ident: Token::Ident(Position::new(1, 14), "a".to_string()) };
    assert_eq!(expected, error);

    let error = test_typecheck("val num = 1\nif (1 < 2) { num + true }").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidOperator {
        token: Token::Plus(Position::new(2, 18)),
        ltype: Type::Int,
        op: BinaryOp::Add,
        rtype: Type::Bool,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("if (1 < 2) { val num = 1 }\nnum + 2").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownIdentifier { ident: Token::Ident(Position::new(2, 1), "num".to_string()) };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_if_expression_conversion_to_statement() -> TestResult {
    let module = test_typecheck("\
          func abc() {\n\
            if true { println(\"hello\") }\n\
          }\
        ")?;
    let expected = TypedAstNode::FunctionDecl(
        Token::Func(Position::new(1, 1)),
        TypedFunctionDeclNode {
            name: ident_token!((1, 6), "abc"),
            args: vec![],
            body: vec![
                TypedAstNode::IfStatement(
                    Token::If(Position::new(2, 1)),
                    TypedIfNode {
                        typ: Type::Unit,
                        condition: Box::new(bool_literal!((2, 4), true)),
                        condition_binding: None,
                        if_block: vec![
                            TypedAstNode::Invocation(
                                Token::LParen(Position::new(2, 18), false),
                                TypedInvocationNode {
                                    typ: Type::Unit,
                                    target: Box::new(identifier!((2, 11), "println", Type::Fn(FnType { arg_types: vec![("items".to_string(), Type::Array(Box::new(Type::Any)), true)], type_args: vec![], ret_type: Box::new(Type::Unit), is_variadic: true, is_enum_constructor: false }), 0)),
                                    args: vec![
                                        Some(TypedAstNode::Array(
                                            Token::LBrack(Position::new(2, 19), false),
                                            TypedArrayNode {
                                                typ: Type::Array(Box::new(Type::String)),
                                                items: vec![
                                                    string_literal!((2, 19), "hello")
                                                ],
                                            },
                                        ))
                                    ],
                                },
                            )
                        ],
                        else_block: None,
                    },
                ),
                TypedAstNode::_Nil(Token::None(Position::new(0, 0))),
            ],
            ret_type: Type::Unit,
            scope_depth: 0,
            is_recursive: false,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    Ok(())
}

#[test]
fn typecheck_if_expression() -> TestResult {
    let module = test_typecheck("val a = if (1 < 2) 123 else 456")?;
    assert_eq!(Type::Int, module.global_bindings["a"].1);

    let module = test_typecheck("val a = if (1 < 2) 123")?;
    assert_eq!(Type::Option(Box::new(Type::Int)), module.global_bindings["a"].1);

    let module = test_typecheck("val a = if (1 < 2) { if (true) 123 } else if (false) 456")?;
    assert_eq!(Type::Option(Box::new(Type::Int)), module.global_bindings["a"].1);

    Ok(())
}

#[test]
fn typecheck_if_expression_errors() {
    let error = test_typecheck("val a = if (1 < 2) {} else 456").unwrap_err();
    let expected = TypecheckerErrorKind::MissingIfExprBranch { if_token: Token::If(Position::new(1, 9)), is_if_branch: true };
    assert_eq!(expected, error);

    let error = test_typecheck("val a = if (1 < 2) 123 else {}").unwrap_err();
    let expected = TypecheckerErrorKind::MissingIfExprBranch { if_token: Token::If(Position::new(1, 9)), is_if_branch: false };
    assert_eq!(expected, error);

    let error = test_typecheck("val a = if (1 < 2) 123 else true").unwrap_err();
    let expected = TypecheckerErrorKind::IfExprBranchMismatch {
        if_token: Token::If(Position::new(1, 9)),
        if_type: Type::Int,
        else_type: Type::Bool,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("val a = if (1 < 2) { if (true) 123 } else 456").unwrap_err();
    let expected = TypecheckerErrorKind::IfExprBranchMismatch {
        if_token: Token::If(Position::new(1, 9)),
        if_type: Type::Option(Box::new(Type::Int)),
        else_type: Type::Int,
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_invocation() -> TestResult {
    let module = test_typecheck("func abc() {}\nabc()")?;
    let expected = TypedAstNode::Invocation(
        Token::LParen(Position::new(2, 4), false),
        TypedInvocationNode {
            typ: Type::Unit,
            target: Box::new(
                identifier!((2, 1), "abc", Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::Unit), is_variadic: false, is_enum_constructor: false }), 0)
            ),
            args: vec![],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("\
          func abc(a: Int, b: String): String { b }\n\
          abc(1, \"2\")\
        ")?;
    let expected = TypedAstNode::Invocation(
        Token::LParen(Position::new(2, 4), false),
        TypedInvocationNode {
            typ: Type::String,
            target: Box::new(identifier!(
                    (2, 1),
                    "abc",
                    Type::Fn(FnType {
                        arg_types: vec![("a".to_string(), Type::Int, false), ("b".to_string(), Type::String, false)],
                        type_args: vec![],
                        ret_type: Box::new(Type::String),
                        is_variadic: false,
                        is_enum_constructor: false
                    }),
                    0
                )),
            args: vec![
                Some(int_literal!((2, 5), 1)),
                Some(string_literal!((2, 8), "2")),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let res = test_typecheck(r#"
          func abc(a: Int, b = "hello"): String { b }
          abc(1)
        "#);
    assert_eq!(res.is_ok(), true);

    let res = test_typecheck(r#"
          func abc(*a: Int[]) {}
          abc()
          abc(1)
          abc(1, 2, 3, 4)
          abc(a: [1, 2, 3])
        "#);
    assert_eq!(res.is_ok(), true);

    let res = test_typecheck(r#"
          func abc(a: Int, *b: Int[]) {}
          abc(1)
          abc(1)
          abc(1, 2, 3, 4)
          abc(a: 1, b: [2, 3, 4])
        "#);
    assert_eq!(res.is_ok(), true);

    Ok(())
}

#[test]
fn typecheck_invocation_instantiation() -> TestResult {
    let module = test_typecheck("\
          type Person { name: String }\n\
          Person(name: \"Ken\")\
        ")?;
    let expected = TypedAstNode::Instantiation(
        Token::LParen(Position::new(2, 7), false),
        TypedInstantiationNode {
            typ: Type::Reference("_test/Person".to_string(), vec![]),
            target: Box::new(
                identifier!((2, 1), "Person", Type::Type("_test/Person".to_string(), Box::new(Type::Reference("_test/Person".to_string(), vec![])), false), 0)
            ),
            fields: vec![
                ("name".to_string(), string_literal!((2, 14), "Ken"))
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);
    let expected_type = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "name".to_string(), typ: Type::String, has_default_value: false, readonly: false }
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/Person"]);

    // Test with default parameters
    let module = test_typecheck("\
          type Person { name: String, age: Int = 0 }\n\
          Person(name: \"Ken\")\
        ")?;
    let expected = TypedAstNode::Instantiation(
        Token::LParen(Position::new(2, 7), false),
        TypedInstantiationNode {
            typ: Type::Reference("_test/Person".to_string(), vec![]),
            target: Box::new(
                identifier!((2, 1), "Person", Type::Type("_test/Person".to_string(), Box::new(Type::Reference("_test/Person".to_string(), vec![])), false), 0)
            ),
            fields: vec![
                ("name".to_string(), string_literal!((2, 14), "Ken")),
                ("age".to_string(), int_literal!((1, 40), 0)),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);
    let expected_type = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "name".to_string(), typ: Type::String, has_default_value: false, readonly: false },
            StructTypeField { name: "age".to_string(), typ: Type::Int, has_default_value: true, readonly: false },
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/Person"]);

    Ok(())
}

#[test]
fn typecheck_invocation_errors() {
    let error = test_typecheck("func abc() {}\nabc(1, 2)").unwrap_err();
    let expected = TypecheckerErrorKind::IncorrectArity {
        token: ident_token!((2, 1), "abc"),
        expected: 0,
        actual: 2,
    };
    assert_eq!(error, expected);

    let error = test_typecheck("func abc(a: Int) {}\nabc(z: false)").unwrap_err();
    let expected = TypecheckerErrorKind::UnexpectedParamName {
        token: ident_token!((2, 5), "z"),
    };
    assert_eq!(error, expected);

    let error = test_typecheck("func abc(a: Int, b: Int) {}\nabc()").unwrap_err();
    let expected = TypecheckerErrorKind::IncorrectArity {
        token: ident_token!((2, 1), "abc"),
        expected: 2,
        actual: 0,
    };
    assert_eq!(error, expected);

    let error = test_typecheck("func abc(a: Int, b: Int) {}\nabc(a: 3)").unwrap_err();
    let expected = TypecheckerErrorKind::MissingRequiredParams {
        token: ident_token!((2, 1), "abc"),
        missing_params: vec!["b".to_string()],
    };
    assert_eq!(error, expected);

    let error = test_typecheck("func abc(a: Int) {}\nabc(false)").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Bool(Position::new(2, 5), false),
        expected: Type::Int,
        actual: Type::Bool,
    };
    assert_eq!(error, expected);

    let error = test_typecheck("func abc(a: Int) {}\nabc(a: false)").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Bool(Position::new(2, 8), false),
        expected: Type::Int,
        actual: Type::Bool,
    };
    assert_eq!(error, expected);

    let error = test_typecheck("val abc = [1, 2]\nabc()").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidInvocationTarget {
        token: ident_token!((2, 1), "abc"),
        target_type: Type::Array(Box::new(Type::Int)),
    };
    assert_eq!(error, expected);

    let error = test_typecheck("func abc(*a: Int[]) {}\nabc(\"a\")").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::LBrack(Position::new(2, 5), false),
        expected: Type::Array(Box::new(Type::Int)),
        actual: Type::Array(Box::new(Type::String)),
    };
    assert_eq!(error, expected);

    let error = test_typecheck("func abc(*a: Int[]) {}\nabc(1, \"a\")").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::LBrack(Position::new(2, 5), false),
        expected: Type::Array(Box::new(Type::Int)),
        actual: Type::Array(Box::new(Type::Union(vec![Type::Int, Type::String]))),
    };
    assert_eq!(error, expected);
}

#[test]
fn typecheck_invocation_struct_instantiation_error() {
    let error = test_typecheck("\
          type Person { name: String }\n\
          Person()\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::MissingRequiredParams {
        token: ident_token!((2, 1), "Person"),
        missing_params: vec!["name".to_string()],
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          type Person { name: String }\n\
          Person(args: 1)\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnexpectedParamName {
        token: ident_token!((2, 8), "args"),
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          type Person { name: String }\n\
          Person(1)\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidTypeFuncInvocation {
        token: ident_token!((2, 1), "Person"),
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          type Person { name: String }\n\
          Person(name: 123)\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Int(Position::new(2, 14), 123),
        expected: Type::String,
        actual: Type::Int,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          type Person { name: String }\n\
          Person(age: 123, name: \"Ken\")\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnexpectedParamName {
        token: ident_token!((2, 8), "age"),
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          type Person { name: String }\n\
          Person(name: \"Meg\", name: \"Ken\")\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateParamName {
        token: ident_token!((2, 21), "name"),
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          type Person { name: String }\n\
          Person()\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::MissingRequiredParams {
        token: ident_token!((2, 1), "Person"),
        missing_params: vec!["name".to_string()],
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_invocation_primitive_instantiation_error() {
    let cases = vec![
        (Type::Unit, "Unit"),
        (Type::Any, "Any"),
        (Type::Int, "Int"),
        (Type::Float, "Float"),
        (Type::String, "String"),
        (Type::Bool, "Bool"),
        (Type::Reference("Array".to_string(), vec![Type::Generic("T".to_string())]), "Array"),
        (Type::Reference("Set".to_string(), vec![Type::Generic("T".to_string())]), "Set"),
        (Type::Reference("Map".to_string(), vec![Type::Generic("K".to_string()), Type::Generic("V".to_string())]), "Map"),
    ];
    for (typ, input) in cases {
        let error = test_typecheck(&format!("{}(1.2)", input)).unwrap_err();
        let expected = TypecheckerErrorKind::InvalidInstantiation { token: ident_token!((1, 1), input), typ };
        assert_eq!(expected, error);
    }

    let error = test_typecheck("\
          enum Foo { Bar }\n\
          Foo(1.2)\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidInstantiation {
        token: ident_token!((2, 1), "Foo"),
        typ: Type::Reference("_test/Foo".to_string(), vec![]),
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_while_loop() -> TestResult {
    let module = test_typecheck("while true 1 + 1")?;
    let expected = TypedAstNode::WhileLoop(
        Token::While(Position::new(1, 1)),
        TypedWhileLoopNode {
            condition: Box::new(bool_literal!((1, 7), true)),
            condition_binding: None,
            body: vec![
                TypedAstNode::Binary(
                    Token::Plus(Position::new(1, 14)),
                    TypedBinaryNode {
                        typ: Type::Int,
                        left: Box::new(int_literal!((1, 12), 1)),
                        op: BinaryOp::Add,
                        right: Box::new(int_literal!((1, 16), 1)),
                    },
                )
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("while true { break }")?;
    let expected = TypedAstNode::WhileLoop(
        Token::While(Position::new(1, 1)),
        TypedWhileLoopNode {
            condition: Box::new(bool_literal!((1, 7), true)),
            condition_binding: None,
            body: vec![
                TypedAstNode::Break(Token::Break(Position::new(1, 14)))
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("while true { continue }")?;
    let expected = TypedAstNode::WhileLoop(
        Token::While(Position::new(1, 1)),
        TypedWhileLoopNode {
            condition: Box::new(bool_literal!((1, 7), true)),
            condition_binding: None,
            body: vec![
                TypedAstNode::Continue(Token::Continue(Position::new(1, 14)))
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("while true {\nval a = 1\na + 1 }")?;
    let expected = TypedAstNode::WhileLoop(
        Token::While(Position::new(1, 1)),
        TypedWhileLoopNode {
            condition: Box::new(bool_literal!((1, 7), true)),
            condition_binding: None,
            body: vec![
                TypedAstNode::BindingDecl(
                    Token::Val(Position::new(2, 1)),
                    TypedBindingDeclNode {
                        binding: BindingPattern::Variable(ident_token!((2, 5), "a")),
                        scope_depth: 1,
                        is_mutable: false,
                        expr: Some(Box::new(int_literal!((2, 9), 1))),
                    },
                ),
                TypedAstNode::Binary(
                    Token::Plus(Position::new(3, 3)),
                    TypedBinaryNode {
                        typ: Type::Int,
                        left: Box::new(identifier!((3, 1), "a", Type::Int, 1)),
                        op: BinaryOp::Add,
                        right: Box::new(int_literal!((3, 5), 1)),
                    },
                ),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("while [1, 2][0] { break }")?;
    let expected = TypedAstNode::WhileLoop(
        Token::While(Position::new(1, 1)),
        TypedWhileLoopNode {
            condition: Box::new(
                TypedAstNode::Indexing(
                    Token::LBrack(Position::new(1, 13), false),
                    TypedIndexingNode {
                        typ: Type::Option(Box::new(Type::Int)),
                        index: IndexingMode::Index(Box::new(int_literal!((1, 14), 0))),
                        target: Box::new(
                            TypedAstNode::Array(
                                Token::LBrack(Position::new(1, 7), false),
                                TypedArrayNode {
                                    typ: Type::Array(Box::new(Type::Int)),
                                    items: vec![
                                        int_literal!((1, 8), 1),
                                        int_literal!((1, 11), 2),
                                    ],
                                },
                            )
                        ),
                    },
                )
            ),
            condition_binding: None,
            body: vec![
                TypedAstNode::Break(Token::Break(Position::new(1, 19)))
            ],
        },
    );
    Ok(assert_eq!(expected, module.typed_nodes[0]))
}

#[test]
fn typecheck_while_loop_condition_binding() -> TestResult {
    let module = test_typecheck("while true |v| { v }")?;
    let expected = TypedAstNode::WhileLoop(
        Token::While(Position::new(1, 1)),
        TypedWhileLoopNode {
            condition: Box::new(bool_literal!((1, 7), true)),
            condition_binding: Some(ident_token!((1, 13), "v")),
            body: vec![
                identifier!((1, 18), "v", Type::Bool, 2)
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("\
          val item = [1, 2, 3][0]\n\
          while item |item| { item }\
        ")?;
    let expected = TypedAstNode::WhileLoop(
        Token::While(Position::new(2, 1)),
        TypedWhileLoopNode {
            condition: Box::new(
                identifier!((2, 7), "item", Type::Option(Box::new(Type::Int)), 0)
            ),
            condition_binding: Some(ident_token!((2, 13), "item")),
            body: vec![
                identifier!((2, 21), "item", Type::Int, 2)
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    Ok(())
}

#[test]
fn typecheck_while_loop_error() {
    let error = test_typecheck("while 1 + 1 { println(123) }").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidIfConditionType {
        token: Token::Plus(Position::new(1, 9)),
        actual: Type::Int,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("while \"asdf\" |str| { println(123) }").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidIfConditionType {
        token: Token::String(Position::new(1, 7), "asdf".to_string()),
        actual: Type::String,
    };
    assert_eq!(expected, error)
}

#[test]
fn typecheck_break_statement_error() {
    let error = test_typecheck("if true { break }").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidTerminatorPlacement(Token::Break(Position::new(1, 11)));
    assert_eq!(expected, error);

    let error = test_typecheck("func abc() { break }").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidTerminatorPlacement(Token::Break(Position::new(1, 14)));
    assert_eq!(expected, error)
}

#[test]
fn typecheck_continue_statement_error() {
    let error = test_typecheck("if true { continue }").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidTerminatorPlacement(Token::Continue(Position::new(1, 11)));
    assert_eq!(expected, error);

    let error = test_typecheck("func abc() { continue }").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidTerminatorPlacement(Token::Continue(Position::new(1, 14)));
    assert_eq!(expected, error)
}

#[test]
fn typecheck_for_loop() -> TestResult {
    let module = test_typecheck("val arr = [1, 2, 3]\nfor a in arr {\na + 1 }")?;
    let expected = TypedAstNode::ForLoop(
        Token::For(Position::new(2, 1)),
        TypedForLoopNode {
            binding: BindingPattern::Variable(ident_token!((2, 5), "a")),
            index_ident: None,
            iterator: Box::new(identifier!((2, 10), "arr", Type::Array(Box::new(Type::Int)), 0)),
            body: vec![
                TypedAstNode::Binary(
                    Token::Plus(Position::new(3, 3)),
                    TypedBinaryNode {
                        typ: Type::Int,
                        left: Box::new(identifier!((3, 1), "a", Type::Int, 2)),
                        op: BinaryOp::Add,
                        right: Box::new(int_literal!((3, 5), 1)),
                    },
                )
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("val arr = [1, 2, 3]\nfor a, i in arr {\na + i }")?;
    let expected = TypedAstNode::ForLoop(
        Token::For(Position::new(2, 1)),
        TypedForLoopNode {
            binding: BindingPattern::Variable(ident_token!((2, 5), "a")),
            index_ident: Some(ident_token!((2, 8), "i")),
            iterator: Box::new(identifier!((2, 13), "arr", Type::Array(Box::new(Type::Int)), 0)),
            body: vec![
                TypedAstNode::Binary(
                    Token::Plus(Position::new(3, 3)),
                    TypedBinaryNode {
                        typ: Type::Int,
                        left: Box::new(identifier!((3, 1), "a", Type::Int, 2)),
                        op: BinaryOp::Add,
                        right: Box::new(identifier!((3, 5), "i", Type::Int, 2)),
                    },
                )
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("val set = #{1, 2, 3}\nfor a, i in set {\na + i }")?;
    let expected = TypedAstNode::ForLoop(
        Token::For(Position::new(2, 1)),
        TypedForLoopNode {
            binding: BindingPattern::Variable(ident_token!((2, 5), "a")),
            index_ident: Some(ident_token!((2, 8), "i")),
            iterator: Box::new(identifier!((2, 13), "set", Type::Set(Box::new(Type::Int)), 0)),
            body: vec![
                TypedAstNode::Binary(
                    Token::Plus(Position::new(3, 3)),
                    TypedBinaryNode {
                        typ: Type::Int,
                        left: Box::new(identifier!((3, 1), "a", Type::Int, 2)),
                        op: BinaryOp::Add,
                        right: Box::new(identifier!((3, 5), "i", Type::Int, 2)),
                    },
                )
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("val map = {a:1, b:2}\nfor k, v in map {\nk + v }")?;
    let expected = TypedAstNode::ForLoop(
        Token::For(Position::new(2, 1)),
        TypedForLoopNode {
            binding: BindingPattern::Variable(ident_token!((2, 5), "k")),
            index_ident: Some(ident_token!((2, 8), "v")),
            iterator: Box::new(identifier!((2, 13), "map", Type::Map(Box::new(Type::String),Box::new(Type::Int)), 0)),
            body: vec![
                TypedAstNode::Binary(
                    Token::Plus(Position::new(3, 3)),
                    TypedBinaryNode {
                        typ: Type::String,
                        left: Box::new(identifier!((3, 1), "k", Type::String, 2)),
                        op: BinaryOp::Add,
                        right: Box::new(identifier!((3, 5), "v", Type::Int, 2)),
                    },
                )
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("\
          val coords = [(1, 2), (3, 4)]\n\
          for (x, y), i in coords {\n\
            x + y\n\
          }\
        ")?;
    let expected = TypedAstNode::ForLoop(
        Token::For(Position::new(2, 1)),
        TypedForLoopNode {
            binding: BindingPattern::Tuple(
                Token::LParen(Position::new(2, 5), false),
                vec![
                    BindingPattern::Variable(ident_token!((2, 6), "x")),
                    BindingPattern::Variable(ident_token!((2, 9), "y")),
                ],
            ),
            index_ident: Some(ident_token!((2, 13), "i")),
            iterator: Box::new(identifier!((2, 18), "coords", Type::Array(Box::new(Type::Tuple(vec![Type::Int, Type::Int]))), 0)),
            body: vec![
                TypedAstNode::Binary(
                    Token::Plus(Position::new(3, 3)),
                    TypedBinaryNode {
                        typ: Type::Int,
                        left: Box::new(identifier!((3, 1), "x", Type::Int, 2)),
                        op: BinaryOp::Add,
                        right: Box::new(identifier!((3, 5), "y", Type::Int, 2)),
                    },
                )
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("\
          val coords = [(1, 2), (3, 4)]\n\
          for (x, y), i in coords {\n\
            continue\n\
          }\
        ")?;
    let expected = TypedAstNode::ForLoop(
        Token::For(Position::new(2, 1)),
        TypedForLoopNode {
            binding: BindingPattern::Tuple(
                Token::LParen(Position::new(2, 5), false),
                vec![
                    BindingPattern::Variable(ident_token!((2, 6), "x")),
                    BindingPattern::Variable(ident_token!((2, 9), "y")),
                ],
            ),
            index_ident: Some(ident_token!((2, 13), "i")),
            iterator: Box::new(identifier!((2, 18), "coords", Type::Array(Box::new(Type::Tuple(vec![Type::Int, Type::Int]))), 0)),
            body: vec![
                TypedAstNode::Continue(Token::Continue(Position::new(3, 1)))
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    Ok(())
}

#[test]
fn typecheck_for_loop_error() {
    let error = test_typecheck("for a in 123 { a }").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidLoopTarget {
        token: Token::Int(Position::new(1, 10), 123),
        target_type: Type::Int,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("for a in (1, 2) { a }").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidLoopTarget {
        token: Token::LParen(Position::new(1, 10), false),
        target_type: Type::Tuple(vec![Type::Int, Type::Int]),
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_terminated_expressions() {
    // Testing returns
    let res = test_typecheck(r#"
          func a(): String {
            val s = if true { return "asdf" } else "zxcv"
            s
          }
        "#);
    assert!(res.is_ok());
    let res = test_typecheck(r#"
          func a(): String {
            val s = match [1, 2][0] {
              Int i => "$i"
              None => { return "asdf" }
            }
            s
          }
        "#);
    assert!(res.is_ok());
    let res = test_typecheck(r#"
          func a(): String {
            while true { return "asdf" }
          }
          func b(): String {
            for i in range(0, 1) { return "asdf" }
          }
        "#);
    assert!(res.is_ok());

    // Testing break
    let res = test_typecheck(r#"
          func a(): String {
            while true {
              if true { break }
              else { return "asdf" }
            }
            "zxcv"
          }
        "#);
    assert!(res.is_ok());

    // Testing continue
    let res = test_typecheck(r#"
          func a(): String {
            while true {
              if true { continue }
              else { return "asdf" }
            }
            "zxcv"
          }
        "#);
    assert!(res.is_ok());
    let res = test_typecheck(r#"
          func a(): String {
            for i in range(0, 1) {
              match [1][0] {
                Int i => { continue }
                None => { return "asdf" }
              }
            }
            "zxcv"
          }
        "#);
    assert!(res.is_ok());
}

#[test]
fn typecheck_accessor_instance() -> TestResult {
    // Getting fields off structs
    let module = test_typecheck("\
          type Person { name: String }\n\
          val p = Person(name: \"Sam\")\n\
          p.name\n\
        ")?;
    let expected = TypedAstNode::Accessor(
        Token::Dot(Position::new(3, 2)),
        TypedAccessorNode {
            typ: Type::String,
            target: Box::new(identifier!((3, 1), "p", Type::Reference("_test/Person".to_string(), vec![]), 0)),
            field_ident: ident_token!((3, 3), "name"),
            field_idx: 0,
            is_opt_safe: false,
            is_method: false,
            is_readonly: false,
        },
    );
    assert_eq!(expected, module.typed_nodes[2]);
    let expected_typ = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "name".to_string(), typ: Type::String, has_default_value: false, readonly: false }
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_typ, module.referencable_types["_test/Person"]);

    // Getting fields off structs with default field values
    let module = test_typecheck("\
          type Person { name: String, age: Int = 0 }\n\
          val p = Person(name: \"Sam\")\n\
          p.age\n\
        ")?;
    let expected = TypedAstNode::Accessor(
        Token::Dot(Position::new(3, 2)),
        TypedAccessorNode {
            typ: Type::Int,
            target: Box::new(identifier!((3, 1), "p", Type::Reference("_test/Person".to_string(), vec![]), 0)),
            field_ident: ident_token!((3, 3), "age"),
            field_idx: 1,
            is_opt_safe: false,
            is_method: false,
            is_readonly: false,
        },
    );
    assert_eq!(expected, module.typed_nodes[2]);
    let expected_type = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "name".to_string(), typ: Type::String, has_default_value: false, readonly: false },
            StructTypeField { name: "age".to_string(), typ: Type::Int, has_default_value: true, readonly: false },
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/Person"]);

    // Getting field of builtin Array type
    let module = test_typecheck("[1, 2, 3].length")?;
    let expected = TypedAstNode::Accessor(
        Token::Dot(Position::new(1, 10)),
        TypedAccessorNode {
            typ: Type::Int,
            target: Box::new(TypedAstNode::Array(
                Token::LBrack(Position::new(1, 1), false),
                TypedArrayNode {
                    typ: Type::Array(Box::new(Type::Int)),
                    items: vec![
                        int_literal!((1, 2), 1),
                        int_literal!((1, 5), 2),
                        int_literal!((1, 8), 3),
                    ],
                },
            )),
            field_ident: ident_token!((1, 11), "length"),
            field_idx: 0,
            is_opt_safe: false,
            is_method: false,
            is_readonly: true,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    // Getting field of builtin String type
    let module = test_typecheck("\"hello\".length")?;
    let expected = TypedAstNode::Accessor(
        Token::Dot(Position::new(1, 8)),
        TypedAccessorNode {
            typ: Type::Int,
            target: Box::new(string_literal!((1, 1), "hello")),
            field_ident: ident_token!((1, 9), "length"),
            field_idx: 0,
            is_opt_safe: false,
            is_method: false,
            is_readonly: true,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    // Test getting readonly field
    let res = test_typecheck("\
          type Person { name: String = \"Jim\" readonly }\n\
          val p = Person(name: \"Ken\")\n\
          p.name\
        ").is_ok();
    assert!(res);

    Ok(())
}

#[test]
fn typecheck_accessor_static() -> TestResult {
    // Getting static fields off structs
    let module = test_typecheck("\
          type Person { func getName(): String = \"Sam\" }\n\
          Person.getName\n\
        ")?;
    let expected = TypedAstNode::Accessor(
        Token::Dot(Position::new(2, 7)),
        TypedAccessorNode {
            typ: Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }),
            target: Box::new(identifier!((2, 1), "Person", Type::Type("_test/Person".to_string(), Box::new(Type::Reference("_test/Person".to_string(), vec![])), false), 0)),
            field_ident: ident_token!((2, 8), "getName"),
            field_idx: 0,
            is_opt_safe: false,
            is_method: true,
            is_readonly: true,
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);
    let expected_type = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![],
        static_fields: vec![("getName".to_string(), Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }), true)],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/Person"]);

    Ok(())
}

#[test]
fn typecheck_accessor_optional_safe() -> TestResult {
    let module = test_typecheck("\
          type Person { name: String? = None }\n\
          val p = Person()\n\
          p.name?.length\n\
        ")?;
    let expected = TypedAstNode::Accessor(
        Token::QuestionDot(Position::new(3, 7)),
        TypedAccessorNode {
            typ: Type::Option(Box::new(Type::Int)),
            target: Box::new(TypedAstNode::Accessor(
                Token::Dot(Position::new(3, 2)),
                TypedAccessorNode {
                    typ: Type::Option(Box::new(Type::String)),
                    target: Box::new(identifier!((3, 1), "p", Type::Reference("_test/Person".to_string(), vec![]), 0)),
                    field_ident: ident_token!((3, 3), "name"),
                    field_idx: 0,
                    is_opt_safe: false,
                    is_method: false,
                    is_readonly: false,
                },
            )),
            field_ident: ident_token!((3, 9), "length"),
            field_idx: 0,
            is_opt_safe: true,
            is_method: false,
            is_readonly: true,
        },
    );
    assert_eq!(expected, module.typed_nodes[2]);
    let expected_type = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "name".to_string(), typ: Type::Option(Box::new(Type::String)), has_default_value: true, readonly: false }
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/Person"]);

    // Verify that it also works for non-optional fields, converting QuestionDot to just Dot
    let module = test_typecheck("\
          type Person { name: String = \"\" }\n\
          val p = Person()\n\
          p?.name\n\
        ")?;
    let expected = TypedAstNode::Accessor(
        Token::Dot(Position::new(3, 2)),
        TypedAccessorNode {
            typ: Type::String,
            target: Box::new(identifier!((3, 1), "p", Type::Reference("_test/Person".to_string(), vec![]), 0)),
            field_ident: ident_token!((3, 4), "name"),
            field_idx: 0,
            is_opt_safe: false,
            is_method: false,
            is_readonly: false,
        },
    );
    assert_eq!(expected, module.typed_nodes[2]);
    let expected_type = Type::Struct(StructType {
        name: "Person".to_string(),
        type_args: vec![],
        constructable: true,
        fields: vec![
            StructTypeField { name: "name".to_string(), typ: Type::String, has_default_value: true, readonly: false }
        ],
        static_fields: vec![],
        methods: vec![to_string_method_type()],
    });
    assert_eq!(expected_type, module.referencable_types["_test/Person"]);

    Ok(())
}

#[test]
fn typecheck_accessor_error() {
    let error = test_typecheck("\
          type Person { name: String }\n\
          val p = Person(name: \"Sam\")\n\
          p.firstName\n\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownMember {
        token: ident_token!((3, 3), "firstName"),
        target_type: Type::Reference("_test/Person".to_string(), vec![]),
        module_id: None,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("true.value").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownMember {
        token: ident_token!((1, 6), "value"),
        target_type: Type::Bool,
        module_id: None,
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_lambda() -> TestResult {
    let module = test_typecheck("() => \"hello\"")?;
    let expected = TypedAstNode::Lambda(
        Token::Arrow(Position::new(1, 4)),
        TypedLambdaNode {
            typ: Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }),
            args: vec![],
            typed_body: Some(vec![string_literal!((1, 7), "hello")]),
            orig_node: None,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    let module = test_typecheck("a => \"hello\"")?;
    // Not great: this performs no actual assertions on the original scopes. But the original scopes are hard to construct for testing purposes so this'll do for now
    let orig_scopes = if let TypedAstNode::Lambda(_, TypedLambdaNode { orig_node: Some((_, orig_scopes)), .. }) = &module.typed_nodes[0] {
        orig_scopes.clone()
    } else { unreachable!() };

    let expected = TypedAstNode::Lambda(
        Token::Arrow(Position::new(1, 3)),
        TypedLambdaNode {
            typ: Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::Unknown, false)], type_args: vec![], ret_type: Box::new(Type::Unknown), is_variadic: false, is_enum_constructor: false }),
            args: vec![(ident_token!((1, 1), "a"), Type::Unknown, None)],
            typed_body: None,
            orig_node: Some((
                LambdaNode {
                    args: vec![(ident_token!((1, 1), "a"), None, false, None)],
                    body: vec![
                        AstNode::Literal(
                            Token::String(Position::new(1, 6), "hello".to_string()),
                            AstLiteralNode::StringLiteral("hello".to_string()),
                        )
                    ],
                },
                orig_scopes
            )),
        },
    );
    assert_eq!(&expected, &module.typed_nodes[0]);

    let module = test_typecheck("(a, b = \"b\") => \"hello\"")?;
    // Not great: this performs no actual assertions on the original scopes. But the original scopes are hard to construct for testing purposes so this'll do for now
    let orig_scopes = if let TypedAstNode::Lambda(_, TypedLambdaNode { orig_node: Some((_, orig_scopes)), .. }) = &module.typed_nodes[0] {
        orig_scopes.clone()
    } else { unreachable!() };

    let expected = TypedAstNode::Lambda(
        Token::Arrow(Position::new(1, 14)),
        TypedLambdaNode {
            typ: Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::Unknown, false), ("b".to_string(), Type::String, true)], type_args: vec![], ret_type: Box::new(Type::Unknown), is_variadic: false, is_enum_constructor: false }),
            args: vec![
                (ident_token!((1, 2), "a"), Type::Unknown, None),
                (ident_token!((1, 5), "b"), Type::String, Some(string_literal!((1, 9), "b"))),
            ],
            orig_node: Some((
                LambdaNode {
                    args: vec![
                        (ident_token!((1, 2), "a"), None, false, None),
                        (
                            ident_token!((1, 5), "b"),
                            None,
                            false,
                            Some(AstNode::Literal(
                                Token::String(Position::new(1, 9), "b".to_string()),
                                AstLiteralNode::StringLiteral("b".to_string()),
                            ))
                        ),
                    ],
                    body: vec![
                        AstNode::Literal(
                            Token::String(Position::new(1, 17), "hello".to_string()),
                            AstLiteralNode::StringLiteral("hello".to_string()),
                        )
                    ],
                },
                orig_scopes
            )),
            typed_body: None,
        },
    );
    assert_eq!(&expected, &module.typed_nodes[0]);

    let module = test_typecheck("(a: String) => \"hello\"")?;
    let expected = TypedAstNode::Lambda(
        Token::Arrow(Position::new(1, 13)),
        TypedLambdaNode {
            typ: Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::String, false)], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }),
            args: vec![
                (ident_token!((1, 2), "a"), Type::String, None),
            ],
            typed_body: Some(vec![
                string_literal!((1, 16), "hello")
            ]),
            orig_node: None,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    Ok(())
}

#[test]
fn typecheck_lambda_closure() -> TestResult {
    let module = test_typecheck("\
          func getAdder(x: Int): (Int) => Int {\n\
            y => x + y\n\
          }\
        ")?;
    let expected = TypedAstNode::FunctionDecl(
        Token::Func(Position::new(1, 1)),
        TypedFunctionDeclNode {
            name: ident_token!((1, 6), "getAdder"),
            args: vec![
                (ident_token!((1, 15), "x"), Type::Int, false, None)
            ],
            ret_type: Type::Fn(FnType { arg_types: vec![("y".to_string(), Type::Int, false)], type_args: vec![], ret_type: Box::new(Type::Int), is_variadic: false, is_enum_constructor: false }),
            body: vec![
                TypedAstNode::Lambda(
                    Token::Arrow(Position::new(2, 3)),
                    TypedLambdaNode {
                        typ: Type::Fn(FnType {
                            arg_types: vec![("y".to_string(), Type::Int, false)],
                            type_args: vec![],
                            ret_type: Box::new(Type::Int),
                            is_variadic: false,
                            is_enum_constructor: false,
                        }),
                        args: vec![
                            (ident_token!((2, 1), "y"), Type::Int, None)
                        ],
                        orig_node: None,
                        typed_body: Some(vec![
                            TypedAstNode::Binary(
                                Token::Plus(Position::new(2, 8)),
                                TypedBinaryNode {
                                    typ: Type::Int,
                                    op: BinaryOp::Add,
                                    left: Box::new(identifier!((2, 6), "x", Type::Int, 1)),
                                    right: Box::new(identifier!((2, 10), "y", Type::Int, 2)),
                                },
                            )
                        ]),
                    },
                )
            ],
            scope_depth: 0,
            is_recursive: false,
        },
    );
    assert_eq!(expected, module.typed_nodes[0]);

    Ok(())
}

#[test]
fn typecheck_lambda_errors() {
    let error = test_typecheck("(a: Int) => a.toUpper()").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownMember {
        token: ident_token!((1, 15), "toUpper"),
        target_type: Type::Int,
        module_id: None,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("(*a: Int[]) => a.toUpper()").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidVarargUsage(ident_token!((1, 3), "a"));
    assert_eq!(expected, error);
}

#[test]
fn typecheck_lambda_inference() -> TestResult {
    let module = test_typecheck("\
          var fn = (a: String) => a\n\
          fn = a => a\n\
        ")?;
    let expected = TypedAstNode::Assignment(
        Token::Assign(Position::new(2, 4)),
        TypedAssignmentNode {
            kind: AssignmentTargetKind::Identifier,
            typ: Type::Fn(FnType {
                arg_types: vec![("a".to_string(), Type::String, false)],
                type_args: vec![],
                ret_type: Box::new(Type::String),
                is_variadic: false,
                is_enum_constructor: false,
            }),
            target: Box::new(identifier_mut!((2, 1), "fn", Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::String, false)], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }), 0)),
            expr: Box::new(TypedAstNode::Lambda(
                Token::Arrow(Position::new(2, 8)),
                TypedLambdaNode {
                    typ: Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::String, false)], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }),
                    args: vec![
                        (ident_token!((2, 6), "a"), Type::String, None)
                    ],
                    typed_body: Some(vec![
                        identifier!((2, 11), "a", Type::String, 1)
                    ]),
                    orig_node: None,
                },
            )),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let module = test_typecheck("\
          var fn = (a: String) => a\n\
          fn = (a, b = \"a\") => \"hello\"\n\
        ")?;
    let expected = TypedAstNode::Assignment(
        Token::Assign(Position::new(2, 4)),
        TypedAssignmentNode {
            kind: AssignmentTargetKind::Identifier,
            typ: Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::String, false)], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }),
            target: Box::new(identifier_mut!((2, 1), "fn", Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::String, false)], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false }), 0)),
            expr: Box::new(TypedAstNode::Lambda(
                Token::Arrow(Position::new(2, 19)),
                TypedLambdaNode {
                    typ: Type::Fn(FnType {
                        arg_types: vec![("a".to_string(), Type::String, false), ("b".to_string(), Type::String, true)],
                        type_args: vec![],
                        ret_type: Box::new(Type::String),
                        is_variadic: false,
                        is_enum_constructor: false,
                    }),
                    args: vec![
                        (ident_token!((2, 7), "a"), Type::String, None),
                        (ident_token!((2, 10), "b"), Type::String, Some(string_literal!((2, 14), "a"))),
                    ],
                    typed_body: Some(vec![string_literal!((2, 22), "hello")]),
                    orig_node: None,
                },
            )),
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    let res = test_typecheck(r#"
          var fn = (a: String) => a
          func abc(str: String): String = str
          fn = abc
          fn("abc")
        "#);
    assert!(res.is_ok());

    let res = test_typecheck(r#"
          var fn: (String) => String = a => a
          type Person {
            name: String
            func greet(self, greeting: String): String = greeting + ", " + self.name
          }
          fn = Person(name: "Ken").greet
          fn("Hello")
        "#);
    assert!(res.is_ok());

    let res = test_typecheck(r#"
          func call(fn: (String) => String, value: String): String = fn(value)
          call((x, b = "hello") => b, "hello")
        "#);
    assert!(res.is_ok());

    // A lambda which returns Unit can accept anything as a response (which will just be thrown out)
    let res = test_typecheck(r#"
          func call(fn: (String) => Unit, value: String) = fn(value)
          call((x, b = "hello") => b, "hello")
        "#);
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_lambda_inference_errors() {
    let error = test_typecheck("\
          var fn: (String) => Int = a => a\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Arrow(Position::new(1, 29)),
        expected: Type::Int,
        actual: Type::String,
    };
    assert_eq!(expected, error);

    let error = test_typecheck("\
          val fns: ((Int) => Int)[] = [\n\
            x => x * 2,\n\
            x => x + \"!\",\n\
          ]\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Arrow(Position::new(3, 3)),
        expected: Type::Int,
        actual: Type::String,
    };
    assert_eq!(expected, error);
}

#[test]
fn typecheck_match_statements() -> TestResult {
    // Verify branches for Int? type
    let module = test_typecheck("\
          val i = [1, 2][2]\n\
          match i {\n\
            Int i => i\n\
            None => 0\n\
          }
        ")?;
    let expected = TypedAstNode::MatchStatement(
        Token::Match(Position::new(2, 1)),
        TypedMatchNode {
            typ: Type::Unit,
            target: Box::new(identifier!((2, 7), "i", Type::Option(Box::new(Type::Int)), 0)),
            branches: vec![
                (
                    TypedMatchKind::Type { type_name: "Int".to_string(), args: None },
                    Some("i".to_string()),
                    vec![identifier!((3, 10), "i", Type::Int, 1)]
                ),
                (TypedMatchKind::None, None, vec![int_literal!((4, 9), 0)]),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    // Verify branches for (Int | String)? type
    let module = test_typecheck("\
          val i: (Int | String)? = [1, 2][2]\n\
          match i {\n\
            Int i => i\n\
            _ v => v\n\
          }
        ")?;
    let expected = TypedAstNode::MatchStatement(
        Token::Match(Position::new(2, 1)),
        TypedMatchNode {
            typ: Type::Unit,
            target: Box::new(
                identifier!((2, 7), "i", Type::Option(Box::new(Type::Union(vec![Type::Int, Type::String]))), 0)
            ),
            branches: vec![
                (
                    TypedMatchKind::Type { type_name: "Int".to_string(), args: None },
                    Some("i".to_string()),
                    vec![identifier!((3, 10), "i", Type::Int, 1)]
                ),
                (
                    TypedMatchKind::Wildcard,
                    Some("v".to_string()),
                    vec![identifier!((4, 8), "v", Type::Union(vec![Type::String, Type::Unknown]), 1)]
                ),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[1]);

    // Verify branches for enum type
    let module = test_typecheck("\
          enum Direction { Left, Right, Up, Down }\n\
          val d: Direction = Direction.Left\n\
          match d {\n\
            Direction.Left => 0\n\
            Direction.Right => 1\n\
            Direction.Up => 2\n\
            Direction.Down => 3\n\
          }
        ")?;
    let enum_ref_type = Type::Reference("_test/Direction".to_string(), vec![]);
    let expected = TypedAstNode::MatchStatement(
        Token::Match(Position::new(3, 1)),
        TypedMatchNode {
            typ: Type::Unit,
            target: Box::new(identifier!((3, 7), "d", enum_ref_type.clone(), 0)),
            branches: vec![
                (
                    TypedMatchKind::EnumVariant { variant_idx: 0, args: None },
                    None,
                    vec![int_literal!((4, 19), 0)],
                ),
                (
                    TypedMatchKind::EnumVariant { variant_idx: 1, args: None },
                    None,
                    vec![int_literal!((5, 20), 1)],
                ),
                (
                    TypedMatchKind::EnumVariant { variant_idx: 2, args: None },
                    None,
                    vec![int_literal!((6, 17), 2)],
                ),
                (
                    TypedMatchKind::EnumVariant { variant_idx: 3, args: None },
                    None,
                    vec![int_literal!((7, 19), 3)],
                ),
            ],
        },
    );
    assert_eq!(expected, module.typed_nodes[2]);

    let res = test_typecheck("\
          enum Foo { Bar(baz: Int) }\n\
          val f: Foo = Foo.Bar(baz: 24)\n\
          val i: Int = match f {\n\
            Foo.Bar(z) => z\n\
          }
        ");
    assert!(res.is_ok());

    let res = test_typecheck("\
          val s = \"asdf\"\n\
          val i: Int = match s {\n\
            \"asdf\" s => s.length\n\
            _ s => s.length\n\
          }
        ");
    assert!(res.is_ok());

    let res = test_typecheck("\
          val s = (\"asdf\", 123)\n\
          val i: Int = match s {\n\
            (\"asdf\", 123) s => 1\n\
            _ s => s[0].length\n\
          }
        ");
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_match_expressions() -> TestResult {
    let module = test_typecheck("\
          val i = [1, 2][2]\n\
          val j = match i {\n\
            Int i => i\n\
            None => {\n\
              println(\"Got nothing!\")\n\
              0\n\
            }\n\
          }\n\
          j
        ")?;
    let expected = identifier!((9, 1), "j", Type::Int, 0);
    assert_eq!(expected, module.typed_nodes[2]);

    let module = test_typecheck("\
          val i: String | Int = 123\n\
          val j = match i {\n\
            Int i => i\n\
            String s => s.length\n\
          }\n\
          j
        ")?;
    let expected = identifier!((6, 1), "j", Type::Int, 0);
    assert_eq!(expected, module.typed_nodes[2]);

    // Verify matches with generic types
    let module = test_typecheck("\
          type Foo<T> { bar: T }\n\
          val f = if true { Foo(bar: 12) }\n\
          val j = match f {\n\
            Foo f => f.bar\n\
            None => 0\n\
          }\n\
          j
        ")?;
    let expected = identifier!((7, 1), "j", Type::Int, 0);
    assert_eq!(expected, module.typed_nodes[3]);

    // Verify matches with multiple generic types
    let module = test_typecheck("\
          type Foo<T> { bar: T }\n\
          type Baz<T> { qux: T }\n\
          val f: Foo<Int> | Baz<String> = Baz(qux: \"asdf\")\n\
          val j = match f {\n\
            Foo f => f.bar\n\
            Baz b => b.qux.length\n\
          }\n\
          j
        ")?;
    let expected = identifier!((8, 1), "j", Type::Int, 0);
    assert_eq!(expected, module.typed_nodes[4]);

    // Verify matches with generic enums
    let module = test_typecheck("\
          enum LL<T> { Cons(item: T, next: LL<T>), Empty }\n\
          val l = LL.Cons(1, LL.Cons(2, LL.Empty))\n\
          val j = match l {\n\
            LL.Empty => 0\n\
            LL.Cons(item, _) => item\n\
          }\n\
          j
        ")?;
    let expected = identifier!((7, 1), "j", Type::Int, 0);
    assert_eq!(expected, module.typed_nodes[3]);

    // Verify matches with mixed generic enums and generic types
    let module = test_typecheck("\
          type Foo<T> { bar: T }\n\
          enum LL<T> { Cons(item: T, next: LL<T>), Empty }\n\
          val l: Foo<Int> | LL<Int> = LL.Cons(1, LL.Cons(2, LL.Empty))\n\
          val j = match l {\n\
            Foo f => f.bar\n\
            LL.Empty => 0\n\
            LL.Cons(item, _) => item\n\
          }\n\
          j
        ")?;
    let expected = identifier!((9, 1), "j", Type::Int, 0);
    assert_eq!(expected, module.typed_nodes[4]);

    Ok(())
}

#[test]
fn typecheck_match_statements_errors() {
    // Verify branches for (Int | String)? type
    let err = test_typecheck("\
          val i: (Int | String)? = [1, 2][2]\n\
          match i {\n\
            Int i => i\n\
            None => 0\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::NonExhaustiveMatch { token: Token::Match(Position::new(2, 1)) };
    assert_eq!(expected, err);

    // Verify branches for (Int? | String?) type
    let err = test_typecheck("\
          val i: Int? | String? = [1, 2][2]\n\
          match i {\n\
            Int i => i\n\
            None => 0\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::NonExhaustiveMatch { token: Token::Match(Position::new(2, 1)) };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          val i = 0\n\
          match i {\n\
            _ i => i\n\
            Int => 0\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableMatchCase {
        token: ident_token!((4, 1), "Int"),
        typ: None,
        is_unreachable_none: false,
        prior_covering_case_tok: None,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          val i = 0\n\
          match i {\n\
            Int i => i\n\
            String => 0\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableMatchCase {
        token: ident_token!((4, 1), "String"),
        typ: Some(Type::String),
        is_unreachable_none: false,
        prior_covering_case_tok: None,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          val i = 0\n\
          match i {\n\
            _ i => i\n\
            _ x => x\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateMatchCase { token: ident_token!((4, 1), "_") };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          val i = 0\n\
          match i {\n\
            BogusType i => i\n\
            _ x => x\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownType { type_ident: ident_token!((3, 1), "BogusType") };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          enum Direction { Left, Right, Up, Down }\n\
          val d = Direction.Left\n\
          match d {\n\
            Direction.Sideways => 0
            _ x => x\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownMember {
        token: ident_token!((4, 11), "Sideways"),
        target_type: Type::Reference("_test/Direction".to_string(), vec![]),
        module_id: None,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          enum Direction { Left, Right, Up, Down }\n\
          val d = Direction.Left\n\
          match d {\n\
            Direction.Left.A => 0
            _ x => x\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnknownMember {
        token: ident_token!((4, 16), "A"),
        target_type: Type::Reference("_test/Direction".to_string(), vec![]),
        module_id: None,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          enum Direction { Left, Right, Up, Down }\n\
          val d = Direction.Left\n\
          match d {\n\
            Direction.Left => 0
            Direction.Left => 1
            _ x => x\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateMatchCase {
        token: ident_token!((5, 23), "Left"),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          enum Foo { Bar(baz: Int) }\n\
          val f: Foo = Foo.Bar(baz: 24)\n\
          val i: Int = match f {\n\
            Foo.Bar(z, x) => z\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidMatchCaseDestructuringArity {
        token: Token::LParen(Position::new(4, 8), false),
        typ: Type::Reference("_test/Foo".to_string(), vec![]),
        enum_variant: Some("Bar".to_string()),
        expected: 1,
        actual: 2,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          enum Foo { Bar(baz: Int, qux: Int) }\n\
          val f: Foo = Foo.Bar(baz: 6, qux: 24)\n\
          val i: Int = match f {\n\
            Foo.Bar(z) => z\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidMatchCaseDestructuringArity {
        token: Token::LParen(Position::new(4, 8), false),
        typ: Type::Reference("_test/Foo".to_string(), vec![]),
        enum_variant: Some("Bar".to_string()),
        expected: 2,
        actual: 1,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          enum Foo { Bar }\n\
          val f: Foo = Foo.Bar\n\
          val i: Int = match f {\n\
            Foo.Bar(a, b) => 0\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidMatchCaseDestructuring {
        token: ident_token!((4, 5), "Bar"),
        typ: Some(Type::Reference("_test/Foo".to_string(), vec![])),
        enum_variant: Some("Bar".to_string()),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          val x: Int? = 0\n\
          val i = match x {\n\
            Int(z) => 0\n\
            _ => 1\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidMatchCaseDestructuring {
        token: ident_token!((3, 1), "Int"),
        typ: Some(Type::Int),
        enum_variant: None,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          val x = 0\n\
          match x {\n\
            \"asdf\" s => {}
            Int(z) => {}\n\
            _ => {}\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableMatchCase {
        token: Token::String(Position::new(3, 1), "asdf".to_string()),
        typ: Some(Type::String),
        is_unreachable_none: false,
        prior_covering_case_tok: None,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          match (0, \"abc\") {\n\
            (\"asdf\", 123) s => {}
            Int(z) => {}\n\
            _ => {}\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableMatchCase {
        token: Token::LParen(Position::new(2, 1), true),
        typ: Some(Type::Tuple(vec![Type::String, Type::Int])),
        is_unreachable_none: false,
        prior_covering_case_tok: None,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          enum Foo { Bar(baz: Int, quux: Int) }\n\
          match Foo.Bar(0, 0) {\n\
            Foo.Bar(a, 1) => {}\n\
            Foo.Bar(1, 1) => {}\n\
            _ => {}\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableMatchCase {
        token: ident_token!((4, 5), "Bar"),
        typ: None,
        is_unreachable_none: false,
        prior_covering_case_tok: Some(ident_token!((3, 5), "Bar")),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          enum Foo { Bar(baz: Int, quux: Int) }\n\
          match Foo.Bar(0, 0) {\n\
            Foo.Bar(a, b) => {}\n\
            Foo.Bar(1, 1) => {}\n\
            _ => {}\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableMatchCase {
        token: ident_token!((4, 5), "Bar"),
        typ: Some(Type::Reference("_test/Foo".to_string(), vec![])),
        is_unreachable_none: false,
        prior_covering_case_tok: None,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          enum Foo { Bar(baz: Int, quux: Int) }\n\
          match Foo.Bar(0, 0) {\n\
            Foo.Bar(1, 1) => {}\n\
            Foo.Bar(1, 1) => {}\n\
            _ => {}\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableMatchCase {
        token: ident_token!((4, 5), "Bar"),
        typ: None,
        is_unreachable_none: false,
        prior_covering_case_tok: Some(ident_token!((3, 5), "Bar")),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_match_expressions_errors() {
    let err = test_typecheck("\
          val i = [1, 2][2]\n\
          val j = match i {\n\
            Int i => i\n\
            None => false\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::MatchBranchMismatch {
        token: Token::Bool(Position::new(4, 9), false),
        expected: Type::Int,
        actual: Type::Bool,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          val i = [1, 2][2]\n\
          val j = match i {\n\
            Int i => println(\"\")\n\
            None => println(\"\")\n\
          }
        ").unwrap_err();
    let expected = TypecheckerErrorKind::ForbiddenVariableType {
        binding: BindingPattern::Variable(ident_token!((2, 5), "j")),
        typ: Type::Unit,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_return_statements() {
    let input = r#"
          func f(): Int {
            if true { return 1 }
            return 2
          }
        "#;
    assert!(test_typecheck(input).is_ok());

    let input = r#"
          func f(): Int {
            if true { return 1 }
            2
          }
        "#;
    assert!(test_typecheck(input).is_ok());

    let input = r#"
          func f(): Int {
            if true { return 1 } else { return 2 }
          }
        "#;
    assert!(test_typecheck(input).is_ok());

    let input = r#"
          func f(i: Int | String): Int {
            match i {
              String => { return 3 }
              Int => { return 3 }
            }
          }
        "#;
    assert!(test_typecheck(input).is_ok());

    let input = r#"
          func f(): String {
            while true {
              if true { return "a" }
            }
            val s = if true { return "1" } else { "a" }
            s
          }
        "#;
    assert!(test_typecheck(input).is_ok());

    let input = r#"
          func f(i: Int | String): String {
            while true {
              if true { return "a" }
            }
            val s = match i {
              Int => { return "a" }
              String => "st"
            }
            s
          }
        "#;
    assert!(test_typecheck(input).is_ok());

    let input = r#"
          func f(i: Int | String): String[] {
            for _ in [1, 2] {
              if true { return [] }
            }
            val s = match i {
              Int => { return ["a"] }
              String => "st"
            }
            [s]
          }
        "#;
    assert!(test_typecheck(input).is_ok());

    let input = r#"
          func f(): String[] {
            val d = if true {
              if true { return [] } else { "d" }
            } else if false {
              if true { return [] } else { "d" }
            } else {
              if true { return [] } else { "d" }
            }
            [d]
          }
        "#;
    assert!(test_typecheck(input).is_ok());

    let input = r#"
          func f(): (Int) => Int {
            if true {
              return (i) => 5
            }
            (i: Int) => 6
          }
        "#;
    assert!(test_typecheck(input).is_ok());
}

#[test]
fn typecheck_return_statements_unreachable_code_errors() {
    let err = test_typecheck("\
          func f(): Int {\n\
            return 3\n\
            return 6\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableCode {
        token: Token::Return(Position::new(3, 1), false)
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          func f(): Int {\n\
            if true { return 1 } else { return 2 }\n\
            return 3\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableCode {
        token: Token::Return(Position::new(3, 1), false)
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          func f(): Int {\n\
            if true {\
              return 1\n\
              val x = 5 + 4\n\
            }\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableCode {
        token: Token::Val(Position::new(3, 1))
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          func f(): Int {\n\
            while true {\
              return 1\n\
              val x = 5 + 4\n\
            }\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableCode {
        token: Token::Val(Position::new(3, 1))
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          func f(): Int {\n\
            for _ in [1, 2] {\
              return 1\n\
              val x = 5 + 4\n\
            }\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::UnreachableCode {
        token: Token::Val(Position::new(3, 1))
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_return_statements_type_errors() {
    let err = test_typecheck("func f(): String { return 5 }").unwrap_err();
    let expected = TypecheckerErrorKind::ReturnTypeMismatch {
        token: Token::Int(Position::new(1, 27), 5),
        fn_name: "f".to_string(),
        bare_return: false,
        actual: Type::Int,
        expected: Type::String,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          func f(): String {\n\
            if true { return \"\" }\n\
            return 5\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::ReturnTypeMismatch {
        token: Token::Int(Position::new(3, 8), 5),
        fn_name: "f".to_string(),
        bare_return: false,
        actual: Type::Int,
        expected: Type::String,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          func f() {\n\
            if true { return [] }\n\
            return {}\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::ReturnTypeMismatch {
        token: Token::LBrack(Position::new(2, 18), false),
        fn_name: "f".to_string(),
        bare_return: false,
        actual: Type::Array(Box::new(Type::Unknown)),
        expected: Type::Unit,
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          func f(): Map<String, Int> {\n\
            if true { return [] }\n\
            return {}\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::ReturnTypeMismatch {
        token: Token::LBrack(Position::new(2, 18), false),
        fn_name: "f".to_string(),
        bare_return: false,
        actual: Type::Array(Box::new(Type::Unknown)),
        expected: Type::Map(Box::new(Type::String), Box::new(Type::Int)),
    };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          func f(): Map<String, Int> {\n\
            return\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::ReturnTypeMismatch {
        token: Token::Return(Position::new(2, 1), true),
        fn_name: "f".to_string(),
        bare_return: true,
        actual: Type::Unit,
        expected: Type::Map(Box::new(Type::String), Box::new(Type::Int)),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_exports() -> TestResult {
    let module = test_typecheck("\
          export val a = 123
          val b = 123
          export var c = 456
          var d = 456
          export func abc() {}
          func def() {}
          export type Foo {}
          type Bar {}
          export enum Baz {}
          enum Quuz {}
        ")?;
    let expected = vec!["a", "c", "abc", "Foo", "Baz"].into_iter().map(|s| s.to_string()).collect::<HashSet<_>>();
    assert_eq!(expected, module.exports.keys().map(|s| s.to_string()).collect());

    Ok(())
}

#[test]
fn typecheck_exports_errors() {
    let err = test_typecheck("\
          func abc() {\n\
            export val d = 123\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidExportDepth { token: Token::Export(Position::new(2, 1)) };
    assert_eq!(expected, err);

    let err = test_typecheck("\
          func abc() {\n\
            export func d() = 123\n\
          }\
        ").unwrap_err();
    let expected = TypecheckerErrorKind::InvalidExportDepth { token: Token::Export(Position::new(2, 1)) };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_imports() -> TestResult {
    // Verify all import kinds
    let mod1 = "\
          import a, b, Foo, Baz from .mod2\n\
          Baz(foo1: b(a: a), foo2: Foo.Bar)
        ";
    let modules = vec![(
        ".mod2",
        "\
              export val a = 1\n\
              export enum Foo { Bar }
              export type Baz { foo1: Foo, foo2: Foo }
              export func b(a: Int): Foo = Foo.Bar
            "
    )];
    let res = test_typecheck_with_modules(mod1, modules);
    assert!(res.is_ok());

    // Verify all import kinds, with import-all wildcard
    let mod1 = "\
          import * from .mod2\n\
          Baz(foo1: b(a: a), foo2: Foo.Bar)
        ";
    let modules = vec![(
        ".mod2",
        "\
              export val a = 1\n\
              export enum Foo { Bar }\n\
              export type Baz { foo1: Foo, foo2: Foo }\n\
              export func b(a: Int): Foo = Foo.Bar\
            "
    )];
    let res = test_typecheck_with_modules(mod1, modules);
    assert!(res.is_ok());

    // Verify loading from multiple modules
    let mod1 = "\
          import a from .mod2\n\
          import b from .mod3\n\
          val _: Int = a + b\
        ";
    let modules = vec![
        (".mod2", "export val a = 1"),
        (".mod3", "export val b = 2"),
    ];
    let res = test_typecheck_with_modules(mod1, modules);
    assert!(res.is_ok());

    // Verify nested module loading
    let mod1 = "\
          import a from .mod2\n\
          val _: Int = a\
        ";
    let modules = vec![
        (".mod2", "\
              import b from .mod3\n\
              export val a = b + 1\
            "),
        (".mod3", "export val b = 2"),
    ];
    let res = test_typecheck_with_modules(mod1, modules);
    assert!(res.is_ok());

    // Verify working with (non-imported) type from another module
    let mod1 = "\
          import me from .me\n\
          me.name\
        ";
    let modules = vec![
        (".me", "\
              type Person { name: String }\n\
              export val me = Person(name: \"Ken\")\
            ")
    ];
    let res = test_typecheck_with_modules(mod1, modules);
    assert!(res.is_ok());

    Ok(())
}

#[test]
fn typecheck_imports_errors() {
    // Verify invalid import value
    let mod1 = "\
          import a from .mod2\n\
          a + 4
        ";
    let mod2 = "export val b = 6";
    let modules = vec![(".mod2", mod2)];
    let err = test_typecheck_with_modules(mod1, modules).unwrap_err();
    let expected = TypecheckerErrorKind::InvalidImportValue { ident: ident_token!((1, 8), "a") };
    assert_eq!(expected, err);

    // Verify error when imported module doesn't exist
    let mod1 = "\
          import a from .mod2.some_mod\n\
          a + 4
        ";
    let err = test_typecheck_with_modules(mod1, vec![]).unwrap_err();
    let expected = TypecheckerErrorKind::InvalidModuleImport {
        token: Token::Import(Position::new(1, 1)),
        module_name: ".mod2.some_mod".to_string(),
    };
    assert_eq!(expected, err);

    // Verify invalid circular import
    let mod1 = "\
          import b from .mod2\n\
          b + 4
        ";
    let modules = vec![
        (".mod2", "\
              import c from .mod3\n\
              export val b = 6\
            "),
        (".mod3", "\
              import b from .mod2\n\
              export val c = 6\
            "),
    ];
    let err = test_typecheck_with_modules(mod1, modules).unwrap_err();
    let expected = TypecheckerErrorKind::CircularModuleImport {
        token: Token::Import(Position::new(1, 1)),
        module_name: ".mod2".to_string(),
    };
    assert_eq!(expected, err);

    // Verify typechecking of functions across modules
    let mod1 = "\
          import fn from .mod2\n\
          fn(a: 4)
        ";
    let mod2 = "export func fn(a: String) {}";
    let modules = vec![(".mod2", mod2)];
    let err = test_typecheck_with_modules(mod1, modules).unwrap_err();
    let expected = TypecheckerErrorKind::Mismatch {
        token: Token::Int(Position::new(2, 7), 4),
        expected: Type::String,
        actual: Type::Int,
    };
    assert_eq!(expected, err);

    // Verify duplicate function declarations
    let mod1 = "\
          import fn from .mod2\n\
          func fn() {}
        ";
    let mod2 = "export func fn() {}";
    let modules = vec![(".mod2", mod2)];
    let err = test_typecheck_with_modules(mod1, modules).unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateBinding {
        ident: ident_token!((2, 6), "fn"),
        orig_ident: Some(ident_token!((1, 8), "fn")),
    };
    assert_eq!(expected, err);

    // Verify duplicate type declarations
    let mod1 = "\
          import A from .mod2\n\
          type A {}
        ";
    let mod2 = "export type A {}";
    let modules = vec![(".mod2", mod2)];
    let err = test_typecheck_with_modules(mod1, modules).unwrap_err();
    let expected = TypecheckerErrorKind::DuplicateType {
        ident: ident_token!((2, 6), "A"),
        orig_ident: Some(ident_token!((1, 8), "A")),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_import_dependency_order() {
    let mod1 = "import A from .mod2";
    let modules = vec![
        (
            ".mod2", "\
                  import B from .mod3\n\
                  import C from .mod4\n\
                  export val A = 4\
                "
        ),
        (
            ".mod3", "\
                  import C from .mod4\n\
                  export val B = 4\
                "
        ),
        (
            ".mod4", "\
                  export val C = 12\
                "
        ),
    ];
    let reader = MockModuleReader::new(modules);
    let mut loader = ModuleLoader::new(&reader);
    crate::typecheck(ModuleId::from_name("_test"), &mod1.to_string(), &mut loader).unwrap();
    let expected = vec![
        ModuleId::from_name("prelude"),
        ModuleId::from_name(".mod4"),
        ModuleId::from_name(".mod3"),
        ModuleId::from_name(".mod2"),
    ];
    assert_eq!(expected, loader.ordering);
}

#[test]
fn typecheck_import_alias() {
    let mod1 = "\
          import .mod2 as mod2\n\
          println(mod2.a)
        ";
    let modules = vec![(".mod2", "export val a = 1")];
    let res = test_typecheck_with_modules(mod1, modules);
    assert!(res.is_ok());

    let res = test_typecheck("\
          import io\n\
          println(io.prompt)
        ");
    assert!(res.is_ok());

    let mod1 = "\
          import .mod2 as mod2\n\
          println(mod2.A(a: 123))
        ";
    let modules = vec![(".mod2", "export type A { a: Int }")];
    let res = test_typecheck_with_modules(mod1, modules);
    assert!(res.is_ok());
}

#[test]
fn typecheck_import_alias_errors() {
    let mod1 = "\
          import io\n\
          import .mod2 as io
        ";
    let modules = vec![(".mod2", "export val a = 1")];
    let res = test_typecheck_with_modules(mod1, modules);
    let expected = TypecheckerErrorKind::DuplicateBinding {
        ident: ident_token!((2, 17), "io"),
        orig_ident: Some(ident_token!((1, 8), "io")),
    };
    assert_eq!(expected, res.unwrap_err());

    let mod1 = "\
          import .mod2 as foo\n\
          import .mod3 as foo
        ";
    let modules = vec![(".mod2", "export val a = 1"), (".mod3", "export val b = 2")];
    let res = test_typecheck_with_modules(mod1, modules);
    let expected = TypecheckerErrorKind::DuplicateBinding {
        ident: ident_token!((2, 17), "foo"),
        orig_ident: Some(ident_token!((1, 17), "foo")),
    };
    assert_eq!(expected, res.unwrap_err());

    let mod1 = "import .mod2";
    let modules = vec![(".mod2", "export val a = 1")];
    let res = test_typecheck_with_modules(mod1, modules);
    let expected = TypecheckerErrorKind::ForbiddenImportAliasing {
        import_token: Token::Import(Position::new(1, 1)),
        module_id: ModuleId::from_name(".mod2"),
    };
    assert_eq!(expected, res.unwrap_err());

    let mod1 = "\
          import .mod2 as mod2\n\
          println(mod2.z)
        ";
    let modules = vec![(".mod2", "export val a = 1")];
    let res = test_typecheck_with_modules(mod1, modules);
    let expected = TypecheckerErrorKind::UnknownMember {
        token: ident_token!((2, 14), "z"),
        target_type: Type::Module(ModuleId::from_name(".mod2")),
        module_id: Some(ModuleId::from_name(".mod2")),
    };
    assert_eq!(expected, res.unwrap_err());
}

#[test]
fn typecheck_try_expression() {
    let res = test_typecheck("\
          func a(): Result<Int, String> {\n\
            val a = try Result.Ok(123)\n\
            Result.Ok(a + 1)\n\
          }\
        ");
    assert!(res.is_ok());

    let res = test_typecheck("\
          func r(): Result<Int, String> = Result.Err(\"asdf\")\n\
          func a(): Result<Float, String> {\n\
            val a = try r()\n\
            Result.Ok(a + 0.1)\n\
          }\
        ");
    assert!(res.is_ok());
}

#[test]
fn typecheck_try_expression_errors() {
    // Try outside of a function
    let res = test_typecheck("val a = try Result.Ok(123)");
    let expected = TypecheckerErrorKind::InvalidTryPlacement {
        try_token: Token::Try(Position::new(1, 9)),
        fn_ctx: None,
    };
    assert_eq!(expected, res.unwrap_err());

    // Try within non-Tryable-returning function
    let res = test_typecheck("\
          func a(): Int {\n\
            val a = try Result.Ok(123)\n\
            Result.Ok(a + 1)\n\
          }\
        ");
    let expected = TypecheckerErrorKind::InvalidTryPlacement {
        try_token: Token::Try(Position::new(2, 9)),
        fn_ctx: Some((ident_token!((1, 6), "a"), Type::Int)),
    };
    assert_eq!(expected, res.unwrap_err());

    // Try on non-tryable type
    let res = test_typecheck("\
          func a(): Result<Int, Int> {\n\
            val a = try [1, 2, 3]\n\
            Result.Ok(a + 1)\n\
          }\
        ");
    let expected = TypecheckerErrorKind::InvalidTryType {
        try_token: Token::Try(Position::new(2, 9)),
        typ: Type::Array(Box::new(Type::Int)),
    };
    assert_eq!(expected, res.unwrap_err());

    // Try expression's error type does not match function's return type
    let res = test_typecheck("\
          func a(): Result<Int, Int> {\n\
            val a = try Result.Err(\"asdf\")
            Result.Ok(a + 1)\n\
          }\
        ");
    let expected = TypecheckerErrorKind::TryMismatch {
        try_token: Token::Try(Position::new(2, 9)),
        try_type: Type::Reference("Result".to_string(), vec![Type::Placeholder, Type::String]),
        return_type: Type::Reference("Result".to_string(), vec![Type::Int, Type::Int]),
    };
    assert_eq!(expected, res.unwrap_err());
}
