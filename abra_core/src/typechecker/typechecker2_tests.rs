use std::collections::HashMap;
use itertools::Either;
use crate::lexer::tokens::{Position, Range, Token};
use crate::parser;
use crate::parser::ast::{BindingPattern, UnaryOp};
use crate::typechecker::typechecker2::{TypedModule, LoadModule, ModuleId, Project, Typechecker2, TypecheckError, PRELUDE_MODULE_ID, Type, PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_BOOL_TYPE_ID, PRELUDE_STRING_TYPE_ID, TypedNode, TypedLiteral, TypeError, Variable, VarId, ScopeId, Struct, StructId, PRELUDE_UNIT_TYPE_ID, Scope, TypeId, Function, FuncId, FunctionParam, PRELUDE_SCOPE_ID, StructField, VariableAlias, DuplicateNameKind, AccessorKind};

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

    let expected = TypedModule {
        id: PRELUDE_MODULE_ID,
        name: "prelude".to_string(),
        type_ids: vec![
            TypeId(PRELUDE_SCOPE_ID, PRELUDE_UNIT_TYPE_ID.1),
            TypeId(PRELUDE_SCOPE_ID, PRELUDE_INT_TYPE_ID.1),
            TypeId(PRELUDE_SCOPE_ID, PRELUDE_FLOAT_TYPE_ID.1),
            TypeId(PRELUDE_SCOPE_ID, PRELUDE_BOOL_TYPE_ID.1),
            TypeId(PRELUDE_SCOPE_ID, PRELUDE_STRING_TYPE_ID.1),
            TypeId(ScopeId(PRELUDE_MODULE_ID, 1), 0), // `T` generic, from `Option<T>` definition
            TypeId(ScopeId(PRELUDE_MODULE_ID, 2), 0), // `T` generic, from `Array<T>` definition
            TypeId(ScopeId(PRELUDE_MODULE_ID, 3), 0), // `T` generic, from `Set<T>` definition
            TypeId(ScopeId(PRELUDE_MODULE_ID, 4), 0), // `K` generic, from `Map<K, V>` definition
            TypeId(ScopeId(PRELUDE_MODULE_ID, 4), 1), // `V` generic, from `Map<K, V>` definition
            TypeId(ScopeId(PRELUDE_MODULE_ID, 5), 0), // `T` generic, from `None` definition
            TypeId(PRELUDE_SCOPE_ID, 5), // `T?` type, for `None` builtin
        ],
        functions: vec![],
        structs: vec![
            Struct {
                id: StructId(PRELUDE_MODULE_ID, 0),
                name: "Option".to_string(),
                defined_span: None,
                generics: Some(vec![
                    TypeId(ScopeId(PRELUDE_MODULE_ID, 1), 0),
                ]),
                fields: vec![],
                methods: vec![],
                static_methods: vec![],
            },
            Struct {
                id: StructId(PRELUDE_MODULE_ID, 1),
                name: "Array".to_string(),
                defined_span: None,
                generics: Some(vec![
                    TypeId(ScopeId(PRELUDE_MODULE_ID, 2), 0),
                ]),
                fields: vec![
                    StructField { name: "length".to_string(), type_id: PRELUDE_INT_TYPE_ID },
                ],
                methods: vec![],
                static_methods: vec![],
            },
            Struct {
                id: StructId(PRELUDE_MODULE_ID, 2),
                name: "Tuple".to_string(),
                defined_span: None,
                generics: None,
                fields: vec![],
                methods: vec![],
                static_methods: vec![],
            },
            Struct {
                id: StructId(PRELUDE_MODULE_ID, 3),
                name: "Set".to_string(),
                defined_span: None,
                generics: Some(vec![
                    TypeId(ScopeId(PRELUDE_MODULE_ID, 3), 0),
                ]),
                fields: vec![],
                methods: vec![],
                static_methods: vec![],
            },
            Struct {
                id: StructId(PRELUDE_MODULE_ID, 4),
                name: "Map".to_string(),
                defined_span: None,
                generics: Some(vec![
                    TypeId(ScopeId(PRELUDE_MODULE_ID, 4), 0),
                    TypeId(ScopeId(PRELUDE_MODULE_ID, 4), 1),
                ]),
                fields: vec![],
                methods: vec![],
                static_methods: vec![],
            },
        ],
        code: vec![],
        scopes: vec![
            Scope {
                label: "prelude.root".to_string(),
                id: PRELUDE_SCOPE_ID,
                parent: None,
                types: vec![
                    Type::Builtin(PRELUDE_UNIT_TYPE_ID.1),
                    Type::Builtin(PRELUDE_INT_TYPE_ID.1),
                    Type::Builtin(PRELUDE_FLOAT_TYPE_ID.1),
                    Type::Builtin(PRELUDE_BOOL_TYPE_ID.1),
                    Type::Builtin(PRELUDE_STRING_TYPE_ID.1),
                    Type::GenericInstance(
                        StructId(PRELUDE_MODULE_ID, 0),
                        vec![TypeId(ScopeId(PRELUDE_MODULE_ID, 5), 0)],
                    ),
                ],
                vars: vec![
                    Variable {
                        id: VarId(PRELUDE_SCOPE_ID, 0),
                        name: "None".to_string(),
                        type_id: TypeId(PRELUDE_SCOPE_ID, 5),
                        is_mutable: false,
                        is_initialized: true,
                        defined_span: None,
                        is_captured: false,
                        alias: VariableAlias::None,
                    }
                ],
                funcs: vec![],
            },
            Scope {
                label: "prelude.Option".to_string(),
                id: ScopeId(PRELUDE_MODULE_ID, 1),
                parent: Some(PRELUDE_SCOPE_ID),
                types: vec![Type::Generic(None, "T".to_string())],
                vars: vec![],
                funcs: vec![],
            },
            Scope {
                label: "prelude.Array".to_string(),
                id: ScopeId(PRELUDE_MODULE_ID, 2),
                parent: Some(PRELUDE_SCOPE_ID),
                types: vec![Type::Generic(None, "T".to_string())],
                vars: vec![],
                funcs: vec![],
            },
            Scope {
                label: "prelude.Set".to_string(),
                id: ScopeId(PRELUDE_MODULE_ID, 3),
                parent: Some(PRELUDE_SCOPE_ID),
                types: vec![Type::Generic(None, "T".to_string())],
                vars: vec![],
                funcs: vec![],
            },
            Scope {
                label: "prelude.Map".to_string(),
                id: ScopeId(PRELUDE_MODULE_ID, 4),
                parent: Some(PRELUDE_SCOPE_ID),
                types: vec![Type::Generic(None, "K".to_string()), Type::Generic(None, "V".to_string())],
                vars: vec![],
                funcs: vec![],
            },
            Scope {
                label: "prelude.None".to_string(),
                id: ScopeId(PRELUDE_MODULE_ID, 5),
                parent: Some(PRELUDE_SCOPE_ID),
                types: vec![Type::Generic(None, "T".to_string())],
                vars: vec![],
                funcs: vec![],
            },
        ],
    };
    assert_eq!(&expected, prelude_module);
}

#[test]
fn typecheck_literal() {
    let project = test_typecheck("1 2.34\ntrue \"hello\"").unwrap();
    let module = &project.modules[1];
    assert_eq!(ModuleId(1), module.id);
    assert_eq!(format!("./{}", TEST_MODULE_NAME), module.name);
    assert!(module.type_ids.is_empty());

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
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("[[1, 2], [3]]").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(int_array_type_id)).unwrap();
    assert_eq!(expected, type_id);

    // Test deferred generic inference
    let project = test_typecheck("[[], [1, 2, 3]]").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(int_array_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("[None, 123]").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(int_opt_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("[123, None]").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(int_opt_type_id)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: Int[] = [1, 2, 3]").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: Int[] = []").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: Int[][] = [[]]").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let inner_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(inner_type_id)).unwrap();
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

    let (project, Either::Right(err)) = test_typecheck("val a = []").unwrap_err() else { unreachable!() };
    let expected = TypeError::ForbiddenAssignment {
        span: Range { start: Position::new(1, 9), end: Position::new(1, 9) },
        type_id: {
            let array_t_type_id = project.prelude_module().structs[project.prelude_array_struct_id.1].generics.as_ref().unwrap()[0];
            project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(array_t_type_id)).unwrap()
        },
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_tuple() {
    let project = test_typecheck("(1, 2, 3)").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("([1, 2], 3, \"hello\")").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.tuple_type(vec![int_array_type_id, PRELUDE_INT_TYPE_ID, PRELUDE_STRING_TYPE_ID])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: (Int, Bool, String[]) = (1, true, [\"3\"])").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let string_array_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_STRING_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_BOOL_TYPE_ID, string_array_type_id])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: ((Int, Int), (Int, Int)) = ((1, 2), (3, 4))").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let inner_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.tuple_type(vec![inner_type_id, inner_type_id])).unwrap();
    assert_eq!(expected, type_id);
}

#[test]
fn typecheck_failure_tuple() {
    let (project, Either::Right(err)) = test_typecheck("val a: Int[] = (true, 43)").unwrap_err() else { unreachable!() };
    let int_array_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let bool_int_tuple_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap();
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
    let bool_float_bool_tuple_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_BOOL_TYPE_ID])).unwrap();
    let bool_float_tuple_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID])).unwrap();
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
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("#{[1, 2], [3]}").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.set_type(int_array_type_id)).unwrap();
    assert_eq!(expected, type_id);

    // Test deferred generic inference
    let project = test_typecheck("#{#{}, #{1, 2, 3}}").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_set_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.set_type(int_set_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("#{None, 123}").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.set_type(int_opt_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("#{123, None}").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.set_type(int_opt_type_id)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val s: Set<Int> = #{1, 2, 3}").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val s: Set<Int> = #{}").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val s: Set<Set<Int>> = #{#{}}").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let inner_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.set_type(inner_type_id)).unwrap();
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

    let (project, Either::Right(err)) = test_typecheck("val s = #{}").unwrap_err() else { unreachable!() };
    let expected = TypeError::ForbiddenAssignment {
        span: Range { start: Position::new(1, 9), end: Position::new(1, 10) },
        type_id: {
            let set_t_type_id = project.prelude_module().structs[project.prelude_set_struct_id.1].generics.as_ref().unwrap()[0];
            project.find_type_id(&ScopeId(ModuleId(1), 0), &project.set_type(set_t_type_id)).unwrap()
        },
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_map() {
    let project = test_typecheck("{ a: true, b: !false }").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(PRELUDE_STRING_TYPE_ID, PRELUDE_BOOL_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("{ (true): 1, (false): 2 }").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(PRELUDE_BOOL_TYPE_ID, PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("{ ([1, 2, 3]): 1, ([3, 4, 5]): 2 }").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(int_array_type_id, PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    // Test generic inference in values
    let project = test_typecheck("{ a: 123, b: None }").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(PRELUDE_STRING_TYPE_ID, int_opt_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("{ a: None, b: 123 }").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    assert_eq!(expected, type_id);
    let project = test_typecheck("{ a: [], b: [1, 2, 3] }").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(PRELUDE_STRING_TYPE_ID, int_array_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("{ a: [1, 2, 3], b: [] }").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(PRELUDE_STRING_TYPE_ID, int_array_type_id)).unwrap();
    assert_eq!(expected, type_id);

    // Test option inference in keys
    let project = test_typecheck("{ (None): 123, 456: 789 }").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(int_opt_type_id, PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("{ 123: 123, (None): 456 }").unwrap();
    let type_id = *project.modules[1].code[0].type_id();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val m: Map<Int, Int> = { 1: 2, 3: 4 }").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val m: Map<Bool, Int> = {}").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(PRELUDE_BOOL_TYPE_ID, PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val m: Map<Int, Map<Int, Int>> = {}").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    let inner_type_id = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(PRELUDE_INT_TYPE_ID, inner_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("val m: Map<Int, Map<Int, Int>> = { 1: {} }").unwrap();
    let type_id = project.modules[1].scopes[0].vars[0].type_id;
    assert_eq!(expected, type_id);
}

#[test]
fn typecheck_failure_map() {
    let (_, Either::Right(err)) = test_typecheck("{ a: 1, b: true }").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 12), end: Position::new(1, 15) },
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val m: Map<String, Int> = { (true): 1 }").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 30), end: Position::new(1, 33) },
        expected: vec![PRELUDE_STRING_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val m: Map<Int, Map<Int, Int>> = { 1: { 2: true } }").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 44), end: Position::new(1, 47) },
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("val m = {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::ForbiddenAssignment {
        span: Range { start: Position::new(1, 9), end: Position::new(1, 9) },
        type_id: {
            let map_generics = project.prelude_module().structs[project.prelude_map_struct_id.1].generics.as_ref().unwrap();
            let map_k_type_id = map_generics[0];
            let map_v_type_id = map_generics[1];
            project.find_type_id(&ScopeId(ModuleId(1), 0), &project.map_type(map_k_type_id, map_v_type_id)).unwrap()
        },
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_none() {
    let project = test_typecheck("val x: Int? = None").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 0),
            name: "x".to_string(),
            type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(1, 5), end: Position::new(1, 5) }),
            is_captured: false,
            alias: VariableAlias::None,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck("val x: Int? = 12").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 0),
            name: "x".to_string(),
            type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(1, 5), end: Position::new(1, 5) }),
            is_captured: false,
            alias: VariableAlias::None,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);
    let project = test_typecheck("val x: Int?? = 12").unwrap(); // Verify multiple layers of Option shouldn't matter
    let module = &project.modules[1];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck("val x: Int[]? = None").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 0),
            name: "x".to_string(),
            type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap())).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(1, 5), end: Position::new(1, 5) }),
            is_captured: false,
            alias: VariableAlias::None,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck("val x: (Int?)[] = [None]").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 0),
            name: "x".to_string(),
            type_id: project.find_type_id(
                &ScopeId(ModuleId(1), 0),
                &project.array_type(
                    project.find_type_id(
                        &ScopeId(ModuleId(1), 0),
                        &project.option_type(PRELUDE_INT_TYPE_ID),
                    ).unwrap()
                ),
            ).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(1, 5), end: Position::new(1, 5) }),
            is_captured: false,
            alias: VariableAlias::None,
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
        type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(none_var.type_id)).unwrap(),
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("val x: Int = None").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 14), end: Position::new(1, 17) },
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap(),
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
            id: VarId(ScopeId(ModuleId(1), 0), 0),
            name: "x".to_string(),
            type_id: PRELUDE_INT_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 11), end: Position::new(2, 11) }),
            is_captured: false,
            alias: VariableAlias::None,
        },
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 1),
            name: "y".to_string(),
            type_id: PRELUDE_BOOL_TYPE_ID,
            is_mutable: true,
            is_initialized: false,
            defined_span: Some(Range { start: Position::new(3, 11), end: Position::new(3, 11) }),
            is_captured: false,
            alias: VariableAlias::None,
        },
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 2),
            name: "z".to_string(),
            type_id: PRELUDE_STRING_TYPE_ID,
            is_mutable: true,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(4, 11), end: Position::new(4, 11) }),
            is_captured: false,
            alias: VariableAlias::None,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck(r#"
      val (a, b, [c, *d], (e, f)) = (1, 2.3, [true, false], ("a", ("b", "c")))
    "#).unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 0),
            name: "a".to_string(),
            type_id: PRELUDE_INT_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 12), end: Position::new(2, 12) }),
            is_captured: false,
            alias: VariableAlias::None,
        },
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 1),
            name: "b".to_string(),
            type_id: PRELUDE_FLOAT_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 15), end: Position::new(2, 15) }),
            is_captured: false,
            alias: VariableAlias::None,
        },
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 2),
            name: "c".to_string(),
            type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(PRELUDE_BOOL_TYPE_ID)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 19), end: Position::new(2, 19) }),
            is_captured: false,
            alias: VariableAlias::None,
        },
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 3),
            name: "d".to_string(),
            type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_BOOL_TYPE_ID)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 23), end: Position::new(2, 23) }),
            is_captured: false,
            alias: VariableAlias::None,
        },
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 4),
            name: "e".to_string(),
            type_id: PRELUDE_STRING_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 28), end: Position::new(2, 28) }),
            is_captured: false,
            alias: VariableAlias::None,
        },
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 5),
            name: "f".to_string(),
            type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.tuple_type(vec![PRELUDE_STRING_TYPE_ID, PRELUDE_STRING_TYPE_ID])).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 31), end: Position::new(2, 31) }),
            is_captured: false,
            alias: VariableAlias::None,
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
    let expected = TypeError::DuplicateName {
        span: Range { start: Position::new(2, 5), end: Position::new(2, 5) },
        name: "x".to_string(),
        original_span: Some(Range { start: Position::new(1, 5), end: Position::new(1, 5) }),
        kind: DuplicateNameKind::Variable,
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
fn typecheck_type_declaration() {
    let project = test_typecheck("\
      type Foo {\n\
        a: String\n\
        b: Int\n\
      }\
    ").unwrap();
    let module = &project.modules[1];
    let struct_id = StructId(ModuleId(1), 0);
    let expected = vec![
        Struct {
            id: StructId(ModuleId(1), 0),
            name: "Foo".to_string(),
            defined_span: Some(Range { start: Position::new(1, 6), end: Position::new(1, 8) }),
            generics: None,
            fields: vec![
                StructField { name: "a".to_string(), type_id: PRELUDE_STRING_TYPE_ID },
                StructField { name: "b".to_string(), type_id: PRELUDE_INT_TYPE_ID },
            ],
            methods: vec![],
            static_methods: vec![],
        }
    ];
    assert_eq!(expected, module.structs);
    // Verify that the alias variable is inserted for the new type
    let expected = vec![
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 0),
            name: "Foo".to_string(),
            type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.struct_type(struct_id)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(1, 6), end: Position::new(1, 8) }),
            is_captured: false,
            alias: VariableAlias::Struct(struct_id),
        }
    ];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck("\
      type Foo {\n\
        a: String\n\
        func foo(self): Int = 12\n\
        func fooStatic(): Int = 24\n\
      }\
    ").unwrap();
    let module = &project.modules[1];
    let struct_id = StructId(ModuleId(1), 0);
    let expected = vec![
        Struct {
            id: struct_id,
            name: "Foo".to_string(),
            defined_span: Some(Range { start: Position::new(1, 6), end: Position::new(1, 8) }),
            generics: None,
            fields: vec![
                StructField { name: "a".to_string(), type_id: PRELUDE_STRING_TYPE_ID },
            ],
            methods: vec![FuncId(ScopeId(ModuleId(1), 1), 0)],
            static_methods: vec![FuncId(ScopeId(ModuleId(1), 1), 1)],
        }
    ];
    assert_eq!(expected, module.structs);
    let expected = vec![
        Function {
            id: FuncId(ScopeId(ModuleId(1), 1), 0),
            fn_scope_id: ScopeId(ModuleId(1), 2),
            name: "foo".to_string(),
            generic_ids: vec![],
            params: vec![
                FunctionParam {
                    name: "self".to_string(),
                    type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &Type::GenericInstance(struct_id, vec![])).unwrap(),
                    defined_span: Some(Range { start: Position::new(3, 10), end: Position::new(3, 13) }),
                    default_value: None,
                }
            ],
            return_type_id: PRELUDE_INT_TYPE_ID,
            defined_span: Some(Range { start: Position::new(3, 6), end: Position::new(3, 8) }),
            body: vec![
                TypedNode::Literal {
                    token: Token::Int(Position::new(3, 23), 12),
                    value: TypedLiteral::Int(12),
                    type_id: PRELUDE_INT_TYPE_ID,
                }
            ],
            captured_vars: vec![],
        },
        Function {
            id: FuncId(ScopeId(ModuleId(1), 1), 1),
            fn_scope_id: ScopeId(ModuleId(1), 3),
            name: "fooStatic".to_string(),
            generic_ids: vec![],
            params: vec![],
            return_type_id: PRELUDE_INT_TYPE_ID,
            defined_span: Some(Range { start: Position::new(4, 6), end: Position::new(4, 14) }),
            body: vec![
                TypedNode::Literal {
                    token: Token::Int(Position::new(4, 25), 24),
                    value: TypedLiteral::Int(24),
                    type_id: PRELUDE_INT_TYPE_ID,
                }
            ],
            captured_vars: vec![],
        },
    ];
    assert_eq!(expected, module.scopes[1].funcs);
}

#[test]
fn typecheck_failure_type_declaration() {
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {}\n\
      type Foo {}\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Range { start: Position::new(2, 6), end: Position::new(2, 8) },
        name: "Foo".to_string(),
        original_span: Some(Range { start: Position::new(1, 6), end: Position::new(1, 8) }),
        kind: DuplicateNameKind::Type,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {\n\
        a: Bogus\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownType {
        span: Range { start: Position::new(2, 4), end: Position::new(2, 8) },
        name: "Bogus".to_string(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {\n\
        func foo(self) = 12\n\
        func foo(self) = 12\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Range { start: Position::new(3, 6), end: Position::new(3, 8) },
        name: "foo".to_string(),
        original_span: Some(Range { start: Position::new(2, 6), end: Position::new(2, 8) }),
        kind: DuplicateNameKind::Method,
    };
    assert_eq!(expected, err);

    // Self-parameter position tests
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {\n\
        func foo(self, self) {}\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidSelfParamPosition {
        span: Range { start: Position::new(2, 16), end: Position::new(2, 19) },
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {\n\
        func foo(a: Int, self) {}\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidSelfParamPosition {
        span: Range { start: Position::new(2, 18), end: Position::new(2, 21) },
    };
    assert_eq!(expected, err);

    // Errors in method body
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {\n\
        func foo(self) = -true\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(2, 18), end: Position::new(2, 22) },
        expected: vec![PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_function_declaration() {
    // Simple example
    let project = test_typecheck("func foo(): Int = 24").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Function {
            id: FuncId(ScopeId(ModuleId(1), 0), 0),
            fn_scope_id: ScopeId(ModuleId(1), 1),
            name: "foo".to_string(),
            generic_ids: vec![],
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
            captured_vars: vec![],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);

    // Test referencing parameters and having inner expressions
    let project = test_typecheck("\
      val x = 24\n\
      func foo(x: Bool) {\n\
        val y = !x\n\
      }\
    ").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Function {
            id: FuncId(ScopeId(ModuleId(1), 0), 0),
            fn_scope_id: ScopeId(ModuleId(1), 1),
            name: "foo".to_string(),
            generic_ids: vec![],
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
                        VarId(ScopeId(ModuleId(1), 1), 1)
                    ],
                    expr: Some(Box::new(TypedNode::Unary {
                        token: Token::Bang(Position::new(3, 9)),
                        op: UnaryOp::Negate,
                        expr: Box::new(TypedNode::Identifier {
                            token: Token::Ident(Position::new(3, 10), "x".to_string()),
                            var_id: VarId(ScopeId(ModuleId(1), 1), 0),
                            type_arg_ids: vec![],
                            type_id: PRELUDE_BOOL_TYPE_ID,
                        }),
                    })),
                },
            ],
            captured_vars: vec![],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);

    // Test using return type as hint
    let project = test_typecheck("func foo(): (Bool, Bool)[] = []").unwrap();
    let module = &project.modules[1];
    let bool_bool_tuple_array_type_id = project.find_type_id(
        &ScopeId(ModuleId(1), 1),
        &project.array_type(
            project.find_type_id(
                &ScopeId(ModuleId(1), 1),
                &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_BOOL_TYPE_ID]),
            ).unwrap()
        ),
    ).unwrap();
    let expected = vec![
        Function {
            id: FuncId(ScopeId(ModuleId(1), 0), 0),
            fn_scope_id: ScopeId(ModuleId(1), 1),
            name: "foo".to_string(),
            generic_ids: vec![],
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
            captured_vars: vec![],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);

    // Test captured variables
    let project = test_typecheck("\
      val x = 24\n\
      func foo(): Int = x\
    ").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        Function {
            id: FuncId(ScopeId(ModuleId(1), 0), 0),
            fn_scope_id: ScopeId(ModuleId(1), 1),
            name: "foo".to_string(),
            generic_ids: vec![],
            params: vec![],
            return_type_id: PRELUDE_INT_TYPE_ID,
            defined_span: Some(Range { start: Position::new(2, 6), end: Position::new(2, 8) }),
            body: vec![
                TypedNode::Identifier {
                    token: Token::Ident(Position::new(2, 19), "x".to_string()),
                    var_id: VarId(ScopeId(ModuleId(1), 0), 1),
                    type_arg_ids: vec![],
                    type_id: PRELUDE_INT_TYPE_ID,
                }
            ],
            captured_vars: vec![
                VarId(ScopeId(ModuleId(1), 0), 1),
            ],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);
    let expected = vec![
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 0),
            name: "foo".to_string(),
            type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.function_type(vec![], PRELUDE_INT_TYPE_ID)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(2, 6), end: Position::new(2, 8) }),
            is_captured: false,
            alias: VariableAlias::Function(FuncId(ScopeId(ModuleId(1), 0), 0)),
        },
        Variable {
            id: VarId(ScopeId(ModuleId(1), 0), 1),
            name: "x".to_string(),
            type_id: PRELUDE_INT_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Range { start: Position::new(1, 5), end: Position::new(1, 5) }),
            is_captured: true,
            alias: VariableAlias::None,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    // Test type arguments
    let project = test_typecheck("func foo<T>(a: T): T[] = [a]").unwrap();
    let module = &project.modules[1];
    let fn_scope_id = ScopeId(ModuleId(1), 1);
    let t_type_id = project.find_type_id_for_generic(&fn_scope_id, &"T".to_string()).unwrap();
    let expected = vec![
        Function {
            id: FuncId(ScopeId(ModuleId(1), 0), 0),
            fn_scope_id,
            name: "foo".to_string(),
            generic_ids: vec![t_type_id],
            params: vec![
                FunctionParam {
                    name: "a".to_string(),
                    type_id: t_type_id,
                    defined_span: Some(Range { start: Position::new(1, 13), end: Position::new(1, 13) }),
                    default_value: None,
                }
            ],
            return_type_id: project.find_type_id(&fn_scope_id, &project.array_type(t_type_id)).unwrap(),
            defined_span: Some(Range { start: Position::new(1, 6), end: Position::new(1, 8) }),
            body: vec![
                TypedNode::Array {
                    token: Token::LBrack(Position::new(1, 26), false),
                    items: vec![
                        TypedNode::Identifier {
                            token: Token::Ident(Position::new(1, 27), "a".to_string()),
                            var_id: VarId(fn_scope_id, 0),
                            type_arg_ids: vec![],
                            type_id: t_type_id,
                        }
                    ],
                    type_id: project.find_type_id(&fn_scope_id, &project.array_type(t_type_id)).unwrap(),
                }
            ],
            captured_vars: vec![],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);

    // Misc other tests
    assert!(test_typecheck("func foo(x: Bool[] = []) {}").is_ok());
    assert!(test_typecheck("func foo(x = 12): Int = x").is_ok());
}

#[test]
fn typecheck_failure_function_declaration() {
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int): Int = a\n\
      func foo(b: Bool): Bool = b\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Range { start: Position::new(2, 6), end: Position::new(2, 8) },
        name: "foo".to_string(),
        original_span: Some(Range { start: Position::new(1, 6), end: Position::new(1, 8) }),
        kind: DuplicateNameKind::Function,
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
        received: project.find_type_id(&ScopeId(ModuleId(1), 1), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);

    // Self-parameter tests

    let (_, Either::Right(err)) = test_typecheck("func foo(self) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidSelfParam {
        span: Range { start: Position::new(1, 10), end: Position::new(1, 13) },
    };
    assert_eq!(expected, err);

    // Generics tests

    let (_, Either::Right(err)) = test_typecheck("func foo<T, T>(a: T): T = a").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Range { start: Position::new(1, 13), end: Position::new(1, 13) },
        name: "T".to_string(),
        original_span: Some(Range { start: Position::new(1, 10), end: Position::new(1, 10) }),
        kind: DuplicateNameKind::TypeArgument,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("func foo<T, U>(a: T, b: U): (T, U) = (b, a)").unwrap_err() else { unreachable!() };
    let t_type_id = project.find_type_id_by(&ScopeId(ModuleId(1), 1), |ty| if let Type::Generic(_, name) = ty { name == "T" } else { false }).unwrap();
    let u_type_id = project.find_type_id_by(&ScopeId(ModuleId(1), 1), |ty| if let Type::Generic(_, name) = ty { name == "U" } else { false }).unwrap();
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(1, 39), end: Position::new(1, 39) },
        expected: vec![t_type_id],
        received: u_type_id,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_invocation() {
    // Test simple cases
    assert!(test_typecheck("\
      func foo(a: Int, b: Int): Int[] = [a, b]\n\
      foo(1, 2)\n\
      foo(a: 1, b: 2)\n\
      foo(b: 1, a: 2)\
    ").is_ok());

    // Test invocation with optional argument
    let project = test_typecheck("\
      func foo(a: Int[], b = true): Int = 24\n\
      foo([])\
    ").unwrap();
    let module = &project.modules[1];
    let expected = vec![
        TypedNode::Invocation {
            target: Box::new(TypedNode::Identifier {
                token: Token::Ident(Position::new(2, 1), "foo".to_string()),
                var_id: VarId(ScopeId(ModuleId(1), 0), 0),
                type_arg_ids: vec![],
                type_id: project.find_type_id(
                    &ScopeId(ModuleId(1), 1),
                    &project.function_type(
                        vec![
                            project.find_type_id(&ScopeId(ModuleId(1), 1), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap(),
                            PRELUDE_BOOL_TYPE_ID,
                        ],
                        PRELUDE_INT_TYPE_ID,
                    ),
                ).unwrap(),
            }),
            arguments: vec![
                Some(TypedNode::Array {
                    token: Token::LBrack(Position::new(2, 5), false),
                    items: vec![],
                    type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap(),
                }),
                None,
            ],
            type_id: PRELUDE_INT_TYPE_ID,
        }
    ];
    assert_eq!(expected, module.code);

    // Test invocation with generics
    let project = test_typecheck("\
      func foo3<T, U>(a: T, b: U): (T, U) = (a, b)\n\
      foo3(\"a\", true)\
    ").unwrap();
    let module = &project.modules[1];
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.tuple_type(vec![PRELUDE_STRING_TYPE_ID, PRELUDE_BOOL_TYPE_ID])).unwrap();
    assert_eq!(expected, *module.code[0].type_id());

    let project = test_typecheck("\
      func foo<T>(a: T[]): T[] = a\n\
      val x = foo(foo([1]))\
    ").unwrap();
    let module = &project.modules[1];
    let var = &module.scopes[0].vars[1];
    let expected = project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!("x", var.name);
    assert_eq!(expected, var.type_id);

    let project = test_typecheck("\
      func foo<T>(a: T, b: T): T[] = [a, b]\n\
      foo<Int?>(1, None)\
    ").unwrap();
    let module = &project.modules[1];
    let type_id = module.code.last().unwrap().type_id();
    let expected = project.find_type_id(
        &ScopeId(ModuleId(1), 0),
        &project.array_type(
            project.find_type_id(
                &ScopeId(ModuleId(1), 0),
                &project.option_type(PRELUDE_INT_TYPE_ID),
            ).unwrap()
        ),
    ).unwrap();
    assert_eq!(expected, *type_id);
}

#[test]
fn typecheck_failure_invocation() {
    let (_, Either::Right(err)) = test_typecheck("\
      val a = 1\n\
      a()\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::IllegalInvocation {
        span: Range { start: Position::new(2, 1), end: Position::new(2, 1) },
        type_id: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(1, b: true)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::MixedArgumentType {
        span: Range { start: Position::new(2, 8), end: Position::new(2, 8) },
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(a: 1, true)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::MixedArgumentType {
        span: Range { start: Position::new(2, 11), end: Position::new(2, 14) },
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(a: 1, a: 2)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateArgumentLabel {
        span: Range { start: Position::new(2, 11), end: Position::new(2, 11) },
        name: "a".to_string(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(a: 1, d: 2)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnexpectedArgumentName {
        span: Range { start: Position::new(2, 11), end: Position::new(2, 11) },
        arg_name: "d".to_string(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(123, true, 456, false)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidArity {
        span: Range { start: Position::new(2, 1), end: Position::new(2, 4) },
        num_required_args: 3,
        num_provided_args: 4,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(123)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidArity {
        span: Range { start: Position::new(2, 1), end: Position::new(2, 4) },
        num_required_args: 3,
        num_provided_args: 1,
    };
    assert_eq!(expected, err);
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(a: 123)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidArity {
        span: Range { start: Position::new(2, 1), end: Position::new(2, 4) },
        num_required_args: 3,
        num_provided_args: 1,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("\
      func foo<T>(a: T[]): T[] = a\n\
      val x = foo(foo([]))\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::ForbiddenAssignment {
        span: Range { start: Position::new(2, 9), end: Position::new(2, 17) },
        type_id: project.find_type_id(
            &ScopeId(ModuleId(1), 0),
            &project.array_type(project.find_type_id_for_generic(&ScopeId(ModuleId(1), 1), &"T".to_string()).unwrap()),
        ).unwrap(),
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("\
      func foo<T>(a: T, b: T): T[] = [a, b]\n\
      foo(1, None)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(2, 8), end: Position::new(2, 11) },
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);

    // Test passing type arguments explicitly

    let (_, Either::Right(err)) = test_typecheck("\
      func foo<T>(a: T, b: T): T[] = [a, b]\n\
      foo<Bogus>(1, None)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownType {
        span: Range { start: Position::new(2, 5), end: Position::new(2, 9) },
        name: "Bogus".to_string(),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo<T, U>(a: T): T[] = [a]\n\
      foo<Int>(1)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Range { start: Position::new(2, 1), end: Position::new(2, 3) },
        num_required_args: 2,
        num_provided_args: 1,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo<T, U>(a: T): T[] = [a]\n\
      foo<Int, String, Bool>(1)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Range { start: Position::new(2, 18), end: Position::new(2, 21) },
        num_required_args: 2,
        num_provided_args: 3,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo2<T>(a: T, b: T): T[] = [a, b]\n\
      val a = foo2<String>(1, 2)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Range { start: Position::new(2, 22), end: Position::new(2, 22) },
        expected: vec![PRELUDE_STRING_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_accessor() {
    // Test simple case
    let project = test_typecheck("val a = [1, 2, 3]\na.length").unwrap();
    let module = &project.modules[1];
    let expected = TypedNode::Accessor {
        target: Box::new(TypedNode::Identifier {
            token: Token::Ident(Position::new(2, 1), "a".to_string()),
            var_id: VarId(ScopeId(ModuleId(1), 0), 0),
            type_arg_ids: vec![],
            type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap(),
        }),
        kind: AccessorKind::Field,
        member_idx: 0,
        member_span: Range { start: Position::new(2, 3), end: Position::new(2, 8) },
        type_id: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, module.code[1]);
}

#[test]
fn typecheck_failure_accessor() {
    let (project, Either::Right(err)) = test_typecheck("[1].size").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownMember {
        span: Range { start: Position::new(1, 5), end: Position::new(1, 8) },
        field_name: "size".to_string(),
        type_id: project.find_type_id(&ScopeId(ModuleId(1), 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);
}
