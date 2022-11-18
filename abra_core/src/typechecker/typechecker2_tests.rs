use std::collections::HashMap;
use itertools::Either;
use crate::lexer::tokens::{Position, Range, Token};
use crate::parser;
use crate::parser::ast::UnaryOp;
use crate::typechecker::typechecker2::{TypedModule, LoadModule, ModuleId, Project, Typechecker2, TypecheckError, PRELUDE_MODULE_ID, Type, PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_BOOL_TYPE_ID, PRELUDE_STRING_TYPE_ID, TypedNode, TypedLiteral, TypeError, Variable, VarId, ScopeId};

struct TestModuleLoader {
    files: HashMap<String, String>,
}

impl TestModuleLoader {
    pub fn new(mod_contents: Vec<(&str, &str)>) -> Self {
        Self {
            files: mod_contents.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect()
        }
    }
}

impl LoadModule for TestModuleLoader {
    fn resolve_path(&self, module_id: &parser::ast::ModuleId) -> String {
        module_id.get_path(".")
    }

    fn load_file(&self, file_name: &String) -> String {
        self.files.get(file_name)
            .expect(&format!("Internal error: missing file {} from test module loader", file_name))
            .clone()
    }
}

const TEST_MODULE_NAME: &str = "test";

fn test_typecheck(input: &str) -> Result<Project, TypecheckError> {
    let module_id = parser::ast::ModuleId::parse_module_path(&format!("{}", TEST_MODULE_NAME)).unwrap();

    let loader = TestModuleLoader::new(vec![
        (module_id.get_path(".").as_str(), input)
    ]);

    let mut project = Project::default();
    let mut tc = Typechecker2::new(&loader, &mut project);
    tc.typecheck_prelude();

    tc.typecheck_module(&module_id).map(|_| project)
}

#[test]
fn typecheck_prelude() {
    let project = test_typecheck("").unwrap();
    let prelude_module = &project.modules[0];

    let expected = TypedModule {
        id: PRELUDE_MODULE_ID,
        name: "prelude".to_string(),
        types: vec![
            Type::Builtin(PRELUDE_INT_TYPE_ID.id),
            Type::Builtin(PRELUDE_FLOAT_TYPE_ID.id),
            Type::Builtin(PRELUDE_BOOL_TYPE_ID.id),
            Type::Builtin(PRELUDE_STRING_TYPE_ID.id),
        ],
        code: vec![],
        scopes: vec![],
    };
    assert_eq!(&expected, prelude_module);
}

#[test]
fn typecheck_literals() {
    let project = test_typecheck("1 2.34\ntrue \"hello\"").unwrap();
    let module = &project.modules[1];
    assert_eq!(ModuleId { id: 1 }, module.id);
    assert_eq!(format!("./{}", TEST_MODULE_NAME), module.name);
    assert!(module.types.is_empty());

    let expected: Vec<TypedNode> = vec![
        TypedNode::Literal { token: Token::Int(Position::new(1, 1), 1), value: TypedLiteral::Int(1), type_id: PRELUDE_INT_TYPE_ID },
        TypedNode::Literal { token: Token::Float(Position::new(1, 3), 2.34), value: TypedLiteral::Float(2.34), type_id: PRELUDE_FLOAT_TYPE_ID },
        TypedNode::Literal { token: Token::Bool(Position::new(2, 1), true), value: TypedLiteral::Bool(true), type_id: PRELUDE_BOOL_TYPE_ID },
        TypedNode::Literal { token: Token::String(Position::new(2, 6), "hello".to_string()), value: TypedLiteral::String("hello".to_string()), type_id: PRELUDE_STRING_TYPE_ID },
    ];
    assert_eq!(expected, module.code);
}

#[test]
fn typecheck_unary() {
    let project = test_typecheck("-1").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        TypedNode::Unary {
            token: Token::Minus(Position::new(1, 1)),
            op: UnaryOp::Minus,
            expr: Box::new(TypedNode::Literal { token: Token::Int(Position::new(1, 2), 1), value: TypedLiteral::Int(1), type_id: PRELUDE_INT_TYPE_ID }),
        },
    ];
    assert_eq!(expected, module.code);

    let project = test_typecheck("-2.34\n!true").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        TypedNode::Unary {
            token: Token::Minus(Position::new(1, 1)),
            op: UnaryOp::Minus,
            expr: Box::new(TypedNode::Literal { token: Token::Float(Position::new(1, 2), 2.34), value: TypedLiteral::Float(2.34), type_id: PRELUDE_FLOAT_TYPE_ID }),
        },
        TypedNode::Unary {
            token: Token::Bang(Position::new(2, 1)),
            op: UnaryOp::Negate,
            expr: Box::new(TypedNode::Literal { token: Token::Bool(Position::new(2, 2), true), value: TypedLiteral::Bool(true), type_id: PRELUDE_BOOL_TYPE_ID }),
        },
    ];
    assert_eq!(expected, module.code);
}

#[test]
fn typecheck_failure_unary() {
    let Either::Right(err) = test_typecheck("-true").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 1), end: Position::new(1, 5) },
        expected: vec![PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let Either::Right(err) = test_typecheck("!1").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 1), end: Position::new(1, 2) },
        expected: vec![PRELUDE_BOOL_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_binding_declaration() {
    let project = test_typecheck(r#"
      val x = 24
      var y: Bool
      var z: String = "hello"
    "#).unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 0 },
            name: "x".to_string(),
            type_id: PRELUDE_INT_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Range { start: Position::new(2, 11), end: Position::new(2, 11) },
        },
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 1 },
            name: "y".to_string(),
            type_id: PRELUDE_BOOL_TYPE_ID,
            is_mutable: true,
            is_initialized: false,
            defined_span: Range { start: Position::new(3, 11), end: Position::new(3, 11) },
        },
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 2 },
            name: "z".to_string(),
            type_id: PRELUDE_STRING_TYPE_ID,
            is_mutable: true,
            is_initialized: true,
            defined_span: Range { start: Position::new(4, 11), end: Position::new(4, 11) },
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);
}

#[test]
fn typecheck_failure_binding_declaration() {
    let Either::Right(err) = test_typecheck("val x: Bogus = 123").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownType { span: Range { start: Position::new(1, 8), end: Position::new(1, 12) } };
    assert_eq!(expected, err);

    let Either::Right(err) = test_typecheck("val x: Bogus = 123").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownType { span: Range { start: Position::new(1, 8), end: Position::new(1, 12) } };
    assert_eq!(expected, err);

    let Either::Right(err) = test_typecheck("val x = 1\nval x = 4").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateBinding {
        span: Range { start: Position::new(2, 5), end: Position::new(2, 5) },
        original_span: Range { start: Position::new(1, 5), end: Position::new(1, 5) },
    };
    assert_eq!(expected, err);

    let Either::Right(err) = test_typecheck("val x").unwrap_err() else { unreachable!() };
    let expected = TypeError::MissingBindingInitializer {
        span: Range { start: Position::new(1, 5), end: Position::new(1, 5) },
        is_also_missing_type_hint: true,
    };
    assert_eq!(expected, err);
    let Either::Right(err) = test_typecheck("val x: Int").unwrap_err() else { unreachable!() };
    let expected = TypeError::MissingBindingInitializer {
        span: Range { start: Position::new(1, 5), end: Position::new(1, 5) },
        is_also_missing_type_hint: false,
    };
    assert_eq!(expected, err);
    let Either::Right(err) = test_typecheck("var x").unwrap_err() else { unreachable!() };
    let expected = TypeError::MissingBindingInitializer {
        span: Range { start: Position::new(1, 5), end: Position::new(1, 5) },
        is_also_missing_type_hint: true,
    };
    assert_eq!(expected, err);
}
