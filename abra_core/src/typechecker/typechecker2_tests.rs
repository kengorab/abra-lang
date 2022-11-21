use std::collections::HashMap;
use itertools::Either;
use crate::lexer::tokens::{Position, Range, Token};
use crate::parser;
use crate::parser::ast::{BindingPattern, UnaryOp};
use crate::typechecker::typechecker2::{TypedModule, LoadModule, ModuleId, Project, Typechecker2, TypecheckError, PRELUDE_MODULE_ID, Type, PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_BOOL_TYPE_ID, PRELUDE_STRING_TYPE_ID, TypedNode, TypedLiteral, TypeError, Variable, VarId, ScopeId, Struct, StructId, PRELUDE_UNIT_TYPE_ID, PRELUDE_UNKNOWN_TYPE_ID, Scope, TypeId, Function, FuncId, FunctionParam};

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

fn test_typecheck(input: &str) -> Result<Project, (Project, TypecheckError)> {
    let module_id = parser::ast::ModuleId::parse_module_path(&format!("{}", TEST_MODULE_NAME)).unwrap();

    let loader = TestModuleLoader::new(vec![
        (module_id.get_path(".").as_str(), input)
    ]);

    let mut project = Project::default();
    let mut tc = Typechecker2::new(&loader, &mut project);
    tc.typecheck_prelude();

    match tc.typecheck_module(&module_id) {
        Ok(_) => Ok(project),
        Err(e) => Err((project, e))
    }
}

#[test]
fn typecheck_prelude() {
    let project = test_typecheck("").unwrap();
    let prelude_module = &project.modules[0];

    let prelude_scope_id = ScopeId { module_id: PRELUDE_MODULE_ID, id: 0 };
    let expected = TypedModule {
        id: PRELUDE_MODULE_ID,
        name: "prelude".to_string(),
        types: vec![
            Type::Builtin(PRELUDE_UNIT_TYPE_ID.id),
            Type::Builtin(PRELUDE_INT_TYPE_ID.id),
            Type::Builtin(PRELUDE_FLOAT_TYPE_ID.id),
            Type::Builtin(PRELUDE_BOOL_TYPE_ID.id),
            Type::Builtin(PRELUDE_STRING_TYPE_ID.id),
            Type::Generic("T".to_string()), // From `None` definition
            Type::GenericInstance(
                StructId { module_id: PRELUDE_MODULE_ID, id: 0 },
                vec![project.find_type_id(&PRELUDE_MODULE_ID, &Type::Generic("T".to_string())).unwrap()],
            ),
        ],
        functions: vec![],
        structs: vec![
            Struct {
                id: StructId { module_id: PRELUDE_MODULE_ID, id: 0 },
                name: "Option".to_string(),
            },
            Struct {
                id: StructId { module_id: PRELUDE_MODULE_ID, id: 1 },
                name: "Array".to_string(),
            },
            Struct {
                id: StructId { module_id: PRELUDE_MODULE_ID, id: 2 },
                name: "Tuple".to_string(),
            },
            Struct {
                id: StructId { module_id: PRELUDE_MODULE_ID, id: 3 },
                name: "Set".to_string(),
            },
        ],
        code: vec![],
        scopes: vec![
            Scope {
                id: prelude_scope_id,
                parent: None,
                vars: vec![
                    Variable {
                        id: VarId { scope_id: prelude_scope_id, id: 0 },
                        name: "None".to_string(),
                        type_id: TypeId { module_id: PRELUDE_MODULE_ID, id: 6 },
                        is_mutable: false,
                        is_initialized: true,
                        defined_span: None,
                    }
                ],
                funcs: vec![],
            }
        ],
    };
    assert_eq!(&expected, prelude_module);
}

#[test]
fn typecheck_literal() {
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
    let (_, Either::Right(err)) = test_typecheck("-true").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 1), end: Position::new(1, 5) },
        expected: vec![PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("!1").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 1), end: Position::new(1, 2) },
        expected: vec![PRELUDE_BOOL_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_array() {
    let project = test_typecheck("[1, 2, 3]").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let expected = project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("[[1, 2], [3]]").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ModuleId { id: 1 }, &project.array_type(int_array_type_id)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: Int[] = [1, 2, 3]").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: Int[] = []").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: Int[][] = [[]]").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let inner_type_id = project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ModuleId { id: 1 }, &project.array_type(inner_type_id)).unwrap();
    assert_eq!(expected, type_id);
}

#[test]
fn typecheck_failure_array() {
    let (_, Either::Right(err)) = test_typecheck("[1, true, 3]").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 5), end: Position::new(1, 8) },
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val a: Int[] = [true, false]").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 17), end: Position::new(1, 20) },
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val a: Int[][] = [[true]]").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 20), end: Position::new(1, 23) },
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val a = []").unwrap_err() else { unreachable!() };
    let expected = TypeError::ForbiddenAssignment {
        span: Range { start: Position::new(1, 9), end: Position::new(1, 9) },
        type_id: PRELUDE_UNKNOWN_TYPE_ID,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_tuple() {
    let project = test_typecheck("(1, 2, 3)").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let expected = project.find_type_id(&ModuleId { id: 1 }, &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("([1, 2], 3, \"hello\")").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ModuleId { id: 1 }, &project.tuple_type(vec![int_array_type_id, PRELUDE_INT_TYPE_ID, PRELUDE_STRING_TYPE_ID])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: (Int, Bool, String[]) = (1, true, [\"3\"])").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let string_array_type_id = project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_STRING_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ModuleId { id: 1 }, &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_BOOL_TYPE_ID, string_array_type_id])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: ((Int, Int), (Int, Int)) = ((1, 2), (3, 4))").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let inner_type_id = project.find_type_id(&ModuleId { id: 1 }, &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap();
    let expected = project.find_type_id(&ModuleId { id: 1 }, &project.tuple_type(vec![inner_type_id, inner_type_id])).unwrap();
    assert_eq!(expected, type_id);
}

#[test]
fn typecheck_failure_tuple() {
    let (project, Either::Right(err)) = test_typecheck("val a: Int[] = (true, 43)").unwrap_err() else { unreachable!() };
    let int_array_type_id = project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let bool_int_tuple_type_id = project.find_type_id(&ModuleId { id: 1 }, &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap();
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 16), end: Position::new(1, 24) },
        expected: vec![int_array_type_id],
        received: bool_int_tuple_type_id,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val a: (Bool, Float) = (true, 43)").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 31), end: Position::new(1, 32) },
        expected: vec![PRELUDE_FLOAT_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("val a: (Bool, Float, Bool) = (true, 4.3)").unwrap_err() else { unreachable!() };
    let bool_float_bool_tuple_type_id = project.find_type_id(&ModuleId { id: 1 }, &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_BOOL_TYPE_ID])).unwrap();
    let bool_float_tuple_type_id = project.find_type_id(&ModuleId { id: 1 }, &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID])).unwrap();
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 30), end: Position::new(1, 39) },
        expected: vec![bool_float_bool_tuple_type_id],
        received: bool_float_tuple_type_id,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_set() {
    let project = test_typecheck("#{1, 2, 3}").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let expected = project.find_type_id(&ModuleId { id: 1 }, &Type::GenericInstance(project.prelude_set_struct_id, vec![PRELUDE_INT_TYPE_ID])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("#{[1, 2], [3]}").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ModuleId { id: 1 }, &Type::GenericInstance(project.prelude_set_struct_id, vec![int_array_type_id])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val s: Set<Int> = #{1, 2, 3}").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ModuleId { id: 1 }, &Type::GenericInstance(project.prelude_set_struct_id, vec![PRELUDE_INT_TYPE_ID])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val s: Set<Int> = #{}").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ModuleId { id: 1 }, &Type::GenericInstance(project.prelude_set_struct_id, vec![PRELUDE_INT_TYPE_ID])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val s: Set<Set<Int>> = #{#{}}").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let inner_type_id = project.find_type_id(&ModuleId { id: 1 }, &Type::GenericInstance(project.prelude_set_struct_id, vec![PRELUDE_INT_TYPE_ID])).unwrap();
    let expected = project.find_type_id(&ModuleId { id: 1 }, &Type::GenericInstance(project.prelude_set_struct_id, vec![inner_type_id])).unwrap();
    assert_eq!(expected, type_id);
}

#[test]
fn typecheck_failure_set() {
    let (_, Either::Right(err)) = test_typecheck("#{1, true, 3}").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 6), end: Position::new(1, 9) },
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val s: Set<Int> = #{true, false}").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 21), end: Position::new(1, 24) },
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val s: Set<Set<Int>> = #{#{true}}").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 28), end: Position::new(1, 31) },
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val s = #{}").unwrap_err() else { unreachable!() };
    let expected = TypeError::ForbiddenAssignment {
        span: Range { start: Position::new(1, 9), end: Position::new(1, 10) },
        type_id: PRELUDE_UNKNOWN_TYPE_ID,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_none() {
    let project = test_typecheck("val x: Int? = None").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 0 },
            name: "x".to_string(),
            type_id: project.find_type_id(&ModuleId { id: 1 }, &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(1, 5), end: Position::new(1, 5) }),
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck("val x: Int[]? = None").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 0 },
            name: "x".to_string(),
            type_id: project.find_type_id(&ModuleId { id: 1 }, &project.option_type(project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap())).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(1, 5), end: Position::new(1, 5) }),
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck("val x: (Int?)[] = [None]").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 0 },
            name: "x".to_string(),
            type_id: project.find_type_id(
                &ModuleId { id: 1 },
                &project.array_type(
                    project.find_type_id(
                        &ModuleId { id: 1 },
                        &project.option_type(PRELUDE_INT_TYPE_ID),
                    ).unwrap()
                ),
            ).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(1, 5), end: Position::new(1, 5) }),
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);
}

#[test]
fn typecheck_failure_none() {
    let (project, Either::Right(err)) = test_typecheck("val x = None").unwrap_err() else { unreachable!() };
    let none_var = &project.prelude_module().scopes[0].vars[0];
    assert_eq!("None", none_var.name);
    let expected = TypeError::ForbiddenAssignment {
        span: Range { start: Position::new(1, 9), end: Position::new(1, 12) },
        type_id: none_var.type_id,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("val x = [None]").unwrap_err() else { unreachable!() };
    let none_var = &project.prelude_module().scopes[0].vars[0];
    assert_eq!("None", none_var.name);
    let expected = TypeError::ForbiddenAssignment {
        span: Range { start: Position::new(1, 9), end: Position::new(1, 13) },
        type_id: project.find_type_id(&ModuleId { id: 1 }, &project.array_type(none_var.type_id)).unwrap(),
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
            defined_span: Some(Range { start: Position::new(2, 11), end: Position::new(2, 11) }),
        },
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 1 },
            name: "y".to_string(),
            type_id: PRELUDE_BOOL_TYPE_ID,
            is_mutable: true,
            is_initialized: false,
            defined_span: Some(Range { start: Position::new(3, 11), end: Position::new(3, 11) }),
        },
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 2 },
            name: "z".to_string(),
            type_id: PRELUDE_STRING_TYPE_ID,
            is_mutable: true,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(4, 11), end: Position::new(4, 11) }),
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck(r#"
      val (a, b, [c, *d], (e, f)) = (1, 2.3, [true, false], ("a", ("b", "c")))
    "#).unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 0 },
            name: "a".to_string(),
            type_id: PRELUDE_INT_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 12), end: Position::new(2, 12) }),
        },
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 1 },
            name: "b".to_string(),
            type_id: PRELUDE_FLOAT_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 15), end: Position::new(2, 15) }),
        },
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 2 },
            name: "c".to_string(),
            type_id: project.find_type_id(&ModuleId { id: 1 }, &project.option_type(PRELUDE_BOOL_TYPE_ID)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 19), end: Position::new(2, 19) }),
        },
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 3 },
            name: "d".to_string(),
            type_id: project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_BOOL_TYPE_ID)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 23), end: Position::new(2, 23) }),
        },
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 4 },
            name: "e".to_string(),
            type_id: PRELUDE_STRING_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 28), end: Position::new(2, 28) }),
        },
        Variable {
            id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 5 },
            name: "f".to_string(),
            type_id: project.find_type_id(&ModuleId { id: 1 }, &project.tuple_type(vec![PRELUDE_STRING_TYPE_ID, PRELUDE_STRING_TYPE_ID])).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 31), end: Position::new(2, 31) }),
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);
}

#[test]
fn typecheck_failure_binding_declaration() {
    let (_, Either::Right(err)) = test_typecheck("val x: Bogus = 123").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownType {
        span: Range { start: Position::new(1, 8), end: Position::new(1, 12) },
        name: "Bogus".to_string(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val x = 1\nval x = 4").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateBinding {
        span: Range { start: Position::new(2, 5), end: Position::new(2, 5) },
        name: "x".to_string(),
        original_span: Some(Range { start: Position::new(1, 5), end: Position::new(1, 5) }),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val x").unwrap_err() else { unreachable!() };
    let expected = TypeError::MissingBindingInitializer {
        span: Range { start: Position::new(1, 5), end: Position::new(1, 5) },
        is_mutable: false,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("val x: Int").unwrap_err() else { unreachable!() };
    let expected = TypeError::MissingBindingInitializer {
        span: Range { start: Position::new(1, 5), end: Position::new(1, 5) },
        is_mutable: false,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("var x").unwrap_err() else { unreachable!() };
    let expected = TypeError::MissingBindingInitializer {
        span: Range { start: Position::new(1, 5), end: Position::new(1, 5) },
        is_mutable: true,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_function_declaration() {
    let project = test_typecheck("func foo(): Int = 24").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Function {
            id: FuncId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 0 },
            name: "foo".to_string(),
            params: vec![],
            return_type_id: PRELUDE_INT_TYPE_ID,
            defined_span: Some(Range { start: Position::new(1, 6), end: Position::new(1, 8) }),
            body: vec![
                TypedNode::Literal {
                    token: Token::Int(Position::new(1, 19), 24),
                    value: TypedLiteral::Int(24),
                    type_id: PRELUDE_INT_TYPE_ID,
                }
            ],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);

    let project = test_typecheck("\
      val x = 24\n\
      func foo(x: Bool) {\n\
        val y = !x\n\
      }\
    ").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Function {
            id: FuncId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 0 },
            name: "foo".to_string(),
            params: vec![
                FunctionParam {
                    name: "x".to_string(),
                    type_id: PRELUDE_BOOL_TYPE_ID,
                    defined_span: Some(Range { start: Position::new(2, 10), end: Position::new(2, 10) }),
                    default_value: None,
                }
            ],
            return_type_id: PRELUDE_UNIT_TYPE_ID,
            defined_span: Some(Range { start: Position::new(2, 6), end: Position::new(2, 8) }),
            body: vec![
                TypedNode::BindingDeclaration {
                    token: Token::Val(Position::new(3, 1)),
                    pattern: BindingPattern::Variable(Token::Ident(Position::new(3, 5), "y".to_string())),
                    vars: vec![
                        VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 1 }, id: 1 }
                    ],
                    expr: Some(Box::new(TypedNode::Unary {
                        token: Token::Bang(Position::new(3, 9)),
                        op: UnaryOp::Negate,
                        expr: Box::new(TypedNode::Identifier {
                            token: Token::Ident(Position::new(3, 10), "x".to_string()),
                            var_id: VarId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 1 }, id: 0 },
                            type_id: PRELUDE_BOOL_TYPE_ID,
                        }),
                    })),
                },
            ],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);

    let project = test_typecheck("func foo(): (Bool, Bool)[] = []").unwrap();
    let module = &project.modules[1];
    let bool_bool_tuple_array_type_id = project.find_type_id(
        &ModuleId { id: 1 },
        &project.array_type(
            project.find_type_id(
                &ModuleId { id: 1 },
                &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_BOOL_TYPE_ID]),
            ).unwrap()
        ),
    ).unwrap();
    let expected = vec![
        Function {
            id: FuncId { scope_id: ScopeId { module_id: ModuleId { id: 1 }, id: 0 }, id: 0 },
            name: "foo".to_string(),
            params: vec![],
            return_type_id: bool_bool_tuple_array_type_id,
            defined_span: Some(Range { start: Position::new(1, 6), end: Position::new(1, 8) }),
            body: vec![
                TypedNode::Array {
                    token: Token::LBrack(Position::new(1, 30), false),
                    items: vec![],
                    type_id: bool_bool_tuple_array_type_id,
                },
            ],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);

    assert!(test_typecheck("func foo(x: Bool[] = []) {}").is_ok());
    assert!(test_typecheck("func foo(x = 12): Int = x").is_ok());
}

#[test]
fn typecheck_failure_function_declaration() {
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int): Int = a\n\
      func foo(b: Bool): Bool = b\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateBinding {
        span: Range { start: Position::new(2, 6), end: Position::new(2, 8) },
        name: "foo".to_string(),
        original_span: Some(Range { start: Position::new(1, 6), end: Position::new(1, 8) }),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("func foo(a: Int, a: Bool) = a").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateParameter {
        span: Range { start: Position::new(1, 18), end: Position::new(1, 18) },
        name: "a".to_string(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("func foo(a: Bool): Int = a").unwrap_err() else { unreachable!() };
    let expected = TypeError::ReturnTypeMismatch {
        span: Range { start: Position::new(1, 26), end: Position::new(1, 26) },
        func_name: "foo".to_string(),
        expected: PRELUDE_INT_TYPE_ID,
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    // Default-valued parameter tests

    let (_, Either::Right(err)) = test_typecheck("func foo(a = x) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownIdentifier {
        span: Range { start: Position::new(1, 14), end: Position::new(1, 14) },
        token: Token::Ident(Position::new(1, 14), "x".to_string()),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("func foo(a: Bool[] = [1, 2]) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 23), end: Position::new(1, 23) },
        expected: vec![PRELUDE_BOOL_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("func foo(a: Bool = [1, 2]) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 20), end: Position::new(1, 24) },
        expected: vec![PRELUDE_BOOL_TYPE_ID],
        received: project.find_type_id(&ModuleId { id: 1 }, &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);
}
