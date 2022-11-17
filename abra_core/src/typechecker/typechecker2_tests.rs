use std::collections::HashMap;
use crate::lexer::tokens::{Position, Token};
use crate::parser;
use crate::typechecker::typechecker2::{TypedModule, LoadModule, ModuleId, Project, Typechecker2, TypecheckError, PRELUDE_MODULE_ID, Type, PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_BOOL_TYPE_ID, PRELUDE_STRING_TYPE_ID, TypedNode, TypedLiteral};

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

fn test_typecheck(input: &str) -> Result<Project, TypecheckError> {
    let module_name = "test";
    let module_id = parser::ast::ModuleId::parse_module_path(&format!("{}", module_name)).unwrap();

    let loader = TestModuleLoader::new(vec![
        (module_id.get_path(".").as_str(), input)
    ]);

    let mut project = Project::default();
    let mut tc = Typechecker2::new(&loader, &mut project);
    tc.typecheck_prelude();

    tc.typecheck_module(&module_id).map(|_| project)
}

type TestResult = Result<(), TypecheckError>;

#[test]
fn typecheck_prelude() -> TestResult {
    let project = test_typecheck("")?;
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
    };
    assert_eq!(&expected, prelude_module);

    Ok(())
}

#[test]
fn typecheck_literals() -> TestResult {
    let project = test_typecheck("1 2.34\ntrue \"hello\"")?;
    let module = &project.modules[1];
    assert_eq!(ModuleId { id: 1 }, module.id);
    assert_eq!("./test", module.name);
    assert!(module.types.is_empty());

    let expected: Vec<TypedNode> = vec![
        TypedNode::Literal { token: Token::Int(Position::new(1, 1), 1), value: TypedLiteral::Int(1), type_id: PRELUDE_INT_TYPE_ID },
        TypedNode::Literal { token: Token::Float(Position::new(1, 3), 2.34), value: TypedLiteral::Float(2.34), type_id: PRELUDE_FLOAT_TYPE_ID },
        TypedNode::Literal { token: Token::Bool(Position::new(2, 1), true), value: TypedLiteral::Bool(true), type_id: PRELUDE_BOOL_TYPE_ID },
        TypedNode::Literal { token: Token::String(Position::new(2, 6), "hello".to_string()), value: TypedLiteral::String("hello".to_string()), type_id: PRELUDE_STRING_TYPE_ID },
    ];
    assert_eq!(expected, module.code);

    Ok(())
}
