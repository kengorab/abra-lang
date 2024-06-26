use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use itertools::{Either, Itertools};
use crate::lexer::tokens::{Position, POSITION_BOGUS, Range, Token};
use crate::parser;
use crate::parser::ast::{BinaryOp, BindingPattern, UnaryOp};
use crate::typechecker::typechecker2::{LoadModule, ModuleId, Project, Typechecker2, TypecheckError, PRELUDE_MODULE_ID, Type, PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_BOOL_TYPE_ID, PRELUDE_STRING_TYPE_ID, TypedNode, TypedLiteral, TypeError, Variable, VarId, ScopeId, Struct, StructId, PRELUDE_UNIT_TYPE_ID, TypeId, Function, FuncId, FunctionParam, StructField, VariableAlias, DuplicateNameKind, AccessorKind, AssignmentKind, ImmutableAssignmentKind, InvalidTupleIndexKind, InvalidAssignmentTargetKind, Enum, EnumId, EnumVariant, EnumVariantKind, Span, UnreachableMatchCaseKind, InvalidControlFlowTargetKind, ControlFlowTerminator, TerminatorKind, ExportedValue, TypeKind, DecoratorInstance, FunctionKind, DestructuringMismatchKind};

const PRELUDE_STR: &str = include_str!("../../std/prelude.abra");
const INTRINSICS_STR: &str = include_str!("../../std/_intrinsics.abra");
const LIBC_STR: &str = include_str!("../../std/libc.abra");

struct TestModuleLoader {
    files: HashMap<String, String>,
    module_id_map: HashMap<ModuleId, parser::ast::ModuleId>,
    module_id_map_rev: HashMap<parser::ast::ModuleId, ModuleId>,
    module_id_paths: HashMap<ModuleId, String>,
}

impl TestModuleLoader {
    pub fn new(mod_contents: Vec<(String, &str)>) -> Self {
        Self {
            files: mod_contents.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
            module_id_map: HashMap::new(),
            module_id_map_rev: HashMap::new(),
            module_id_paths: HashMap::new(),
        }
    }
}

impl LoadModule for TestModuleLoader {
    fn get_path(&self, _module_id: &ModuleId) -> Option<String> { None }

    fn calculate_path_wrt_other(&self, m_id: &parser::ast::ModuleId, other: Option<&ModuleId>) -> String {
        let path = if let Some(wrt) = other {
            let wrt_path = self.module_id_paths.get(wrt).expect("Attempting to register a module with respect to other which has not yet been registered");
            m_id.get_path(PathBuf::from(wrt_path).parent().unwrap())
        } else {
            if let parser::ast::ModuleId::External(_) = &m_id {
                m_id.get_path("std")
            } else {
                m_id.get_path("")
            }
        };
        let path = path.replace("/./", "/");
        format!("{path}.abra")
    }

    fn register(&mut self, m_id: &parser::ast::ModuleId, module_id: &ModuleId, with_respect_to: Option<&ModuleId>) {
        self.module_id_map.insert(*module_id, m_id.clone());
        self.module_id_map_rev.insert(m_id.clone(), *module_id);

        let path = self.calculate_path_wrt_other(m_id, with_respect_to);
        self.module_id_paths.insert(*module_id, path);
    }

    fn get_module_id(&self, m_id: &parser::ast::ModuleId) -> Option<&ModuleId> {
        self.module_id_map_rev.get(m_id)
    }

    fn module_exists(&self, m_id: &parser::ast::ModuleId, with_respect_to: Option<&ModuleId>) -> bool {
        let path = self.calculate_path_wrt_other(m_id, with_respect_to);
        self.files.contains_key(&path)
    }

    fn load_file(&self, file_name: &String) -> Option<String> {
        self.files.get(file_name).map(|contents| contents.clone())
    }
}

const TEST_MODULE_NAME: &str = "test";
const TEST_MODULE_IDX: usize = 3;
const TEST_MODULE_ID: ModuleId = ModuleId(TEST_MODULE_IDX);

fn test_typecheck(input: &str) -> Result<Project, (Project, TypecheckError)> {
    test_typecheck_with_modules(input, &[])
}

fn assert_typecheck_ok(input: &str) {
    assert_typecheck_ok_modules(input, &[]);
}

fn test_typecheck_with_modules(entry_module: &str, other_modules: &[(&str, &str)]) -> Result<Project, (Project, TypecheckError)> {
    let mut modules = other_modules.into_iter()
        .map(|(path, contents)| {
            let module_id = parser::ast::ModuleId::parse_module_path(path).unwrap();
            (format!("{}.abra", module_id.get_path("")), *contents)
        })
        .collect_vec();
    let entry_module_id = parser::ast::ModuleId::parse_module_path(&format!("./{TEST_MODULE_NAME}")).unwrap();
    modules.push((format!("{}.abra", entry_module_id.get_path("")), entry_module));
    modules.push(("std/prelude.abra".to_string(), PRELUDE_STR));
    modules.push(("std/_intrinsics.abra".to_string(), INTRINSICS_STR));
    modules.push(("std/libc.abra".to_string(), LIBC_STR));

    let mut loader = TestModuleLoader::new(modules);
    let mut project = Project::default();
    let mut tc = Typechecker2::new(&mut loader, &mut project);
    tc.typecheck_prelude().unwrap();

    match tc.typecheck_module(&entry_module_id, None) {
        Ok(_) => Ok(project),
        Err(e) => Err((project, e))
    }
}

fn assert_typecheck_ok_modules(entry_module: &str, other_modules: &[(&str, &str)]) {
    let res = test_typecheck_with_modules(entry_module, other_modules);
    if res.is_err() { dbg!(&res.as_ref().unwrap_err().1); }
    assert!(res.is_ok());
}

fn option_type(project: &Project, inner_type_id: TypeId, variant_idx: Option<usize>) -> Type {
    Type::GenericEnumInstance(project.prelude_option_enum_id, vec![inner_type_id], variant_idx)
}

#[test]
fn test_type_assignability() {
    let cases = [
        ("val x: Any = 1", true),
        ("val x: Any = \"a\"", true),
        ("val x: Any = []", true),
        ("val x: Any = None", true),
        ("val x: Int? = 1", false),
        ("val x: Int = None", false),
        ("val x: Bool[] = []", true),
        ("val x: Bool[] = [true]", true),
        ("val x: Bool[] = [None]", false),
        ("val x: Bool?[] = [None]", true),
        ("val x: Bool[]? = None", true),
        ("val x: Bool[]? = Option.Some([true])", true),
        ("val x: Map<Int, String> = { 1: \"asdf\" }", true),
        ("val x: Map<Int, String> = { 1: None }", false),
        (
            "func f(): Int = 1\n\
             val x: () => Int = f",
            true
        ),
        (
            "func f(): Float = 1.0\n\
             val x: () => Int = f",
            false
        ),
        (
            "func f(a: Int): Int = a\n\
             val x: (Int) => Int = f",
            true
        ),
        (
            "func f(a: Int, b = 3): Int = a\n\
             val x: (Int) => Int = f",
            true
        ),
        (
            "func f(a: Int, b = 3): Int = a\n\
             val x: (Int, Int) => Int = f",
            true
        ),
        (
            "func f(a: Int, b = 3): Int = a\n\
             val x: (Int, Float) => Int = f",
            false
        ),
        (
            "func f(a: Int, b: Int): Int = a\n\
             val x: (Int) => Int = f",
            false
        ),
        (
            "func f<T>(a: T): T[] = [a]\n\
             val x: (Int) => Int[] = f",
            true
        ),
        (
            "func a<X>(x: X): X = x\n\
             func b<T>(fn: (T) => T, t: T): T = t\n\
             b(a, 12)",
            true
        ),
        (
            "type Foo<T> {\n\
               t: T\n\
               func foo<U>(self, u: U): (T, U) = (self.t, u)\n\
             }\n\
             val f = Foo(t: 12.34)\n\
             val x: (Int) => (Float, Int) = f.foo",
            true
        ),
        (
            "type List<T> {\n\
               items: T[]\n\
               func map<U>(self, fn: (T) => U): U[] = []\n\
             }\n\
             val l = List(items: [1, 2])\n\
             val map: ((Int) => Float) => Float[] = l.map",
            true
        ),
        (
            "enum Foo { Bar, Baz }\n\
             val f: Foo = Foo.Bar",
            true,
        ),
        (
            "enum Foo<T> { Bar, Baz }\n\
             val f = Foo.Bar",
            false,
        ),
        (
            "enum Foo { Bar(x: Int) }\n\
             val f: (Int) => Foo = Foo.Bar",
            true,
        ),
        (
            "enum Foo { Bar(x: Int) }\n\
             val f: (Int, Float) => Foo = Foo.Bar",
            true,
        ),
        (
            "enum Foo<T> { Bar }\n\
             func foo(f: Foo<Int>) {}\n\
             foo(Foo.Bar)",
            true,
        ),
        (
            "enum Foo<T> { Bar }\n\
             val f: Foo<Int> = Foo.Bar\n\
             func fn<T>(foo: Foo<T>) {}\n\
             fn(f)",
            true
        ),
        (
            "enum Foo<T> {\n\
               Bar\n\
               func foo(self, t: T) {}\n\
             }\n\
             val f: Foo<Int> = Foo.Bar\n\
             f.foo(123)",
            true,
        ),
        (
            "enum Foo<T> { Bar(x: T), Baz }\n\
             func makeFoo<T>(t: T, fn: (T) => Foo<T>): Foo<T> = Foo.Baz\n\
             val f1 = makeFoo(12, Foo.Bar)\n\
             val _: Foo<Int> = f1",
            true
        ),
        (
            "enum Foo<T> { Bar(x: T), Baz }\n\
             func makeFoo<T>(fn: (T) => Foo<T>): Foo<T> = Foo.Baz\n\
             val f = makeFoo(Foo.Bar)",
            false // Fails because of unbound generic T
        ),
        (
            "enum Foo<T> { Bar(x: T), Baz }\n\
             func makeFoo<T>(fn: (T) => Foo<T>): Foo<T> = Foo.Baz\n\
             val f: Foo<Int> = makeFoo(Foo.Bar)",
            true
        ),
    ];
    for (code, should_be_assignable) in cases {
        if should_be_assignable {
            assert!(test_typecheck(code).is_ok(), "Expected code to typecheck successfully:\n{}", code);
        } else {
            assert!(!test_typecheck(code).is_ok(), "Expected code to fail to typecheck:\n{}", code);
        }
    }
}

#[test]
fn typecheck_failure_type_identifier() {
    let (_, Either::Right(err)) = test_typecheck("var _: Int<Int>").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (1, 12), (1, 14)),
        num_required_args: 0,
        num_provided_args: 1,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("var _: Float<Int>").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (1, 14), (1, 16)),
        num_required_args: 0,
        num_provided_args: 1,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("var _: Bool<Int>").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (1, 13), (1, 15)),
        num_required_args: 0,
        num_provided_args: 1,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("var _: String<Int>").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (1, 15), (1, 17)),
        num_required_args: 0,
        num_provided_args: 1,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("var _: Map<Int>").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (1, 8), (1, 10)),
        num_required_args: 2,
        num_provided_args: 1,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("var _: Map<Int, String, Bool>").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (1, 25), (1, 28)),
        num_required_args: 2,
        num_provided_args: 3,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("var _: Set").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (1, 8), (1, 10)),
        num_required_args: 1,
        num_provided_args: 0,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("var _: Set<Int, String, Bool>").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (1, 17), (1, 28)),
        num_required_args: 1,
        num_provided_args: 3,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_prelude_int() {
    let result = test_typecheck(r#"
      val i = 24

      // Methods
      val abs: Int = i.abs()
      val asBase: String? = i.asBase(base: 16)
      val isEven: Bool = i.isEven()
      val isOdd: Bool = i.isOdd()
      val isBetween1: Bool = i.isBetween(lower: 1, upper: 36)
      val isBetween2: Bool = i.isBetween(lower: 1, upper: 36, inclusive: true)
    "#);
    if let Err((_, e)) = &result { dbg!(e); }
    assert!(result.is_ok());
}

#[test]
fn typecheck_prelude_float() {
    let result = test_typecheck(r#"
      val f = 1.23

      // Methods
      val abs: Float = f.abs()
      val floor: Int = f.floor()
      val ceil: Int = f.ceil()
      val round: Int = f.round()
      val withPrecision: Float = f.withPrecision(precision: 3)
    "#);
    if let Err((_, e)) = &result { dbg!(e); }
    assert!(result.is_ok());
}

#[test]
fn typecheck_prelude_string() {
    let result = test_typecheck(r#"
      val str = "foo"

      // Fields
      val length: Int = str.length

      // Methods
      val isEmpty: Bool = str.isEmpty()
      val toLower: String = str.toLower()
      val toUpper: String = str.toUpper()
      val padLeft1: String = str.padLeft(totalSize: 10)
      val padLeft2: String = str.padLeft(totalSize: 10, padding: ".")
      val trim: String = str.trim()
      val trimStart1: String = str.trimStart()
      val trimStart2: String = str.trimStart(pattern: " ")
      val trimEnd1: String = str.trimEnd()
      val trimEnd2: String = str.trimEnd(pattern: " ")
      val split1: String[] = str.split()
      val split2: String[] = str.split(by: ",")
      val splitAt: (String, String) = str.splitAt(index: 4)
      val lines: String[] = str.lines()
      val parseInt1: Int? = str.parseInt()
      val parseInt2: Int? = str.parseInt(radix: 16)
      val parseFloat: Float? = str.parseFloat()
      val concat1: String = str.concat("other")
      val concat2: String = str.concat([1, 2])
      val concat3: String = str.concat(1, 2.3, true, [1, 2, 3], ({ a: 1 }, false))
      val concat4: String = str.concat(suffix: 1, others: [2.3, true, [1, 2, 3], ({ a: 1 }, false)])
      val replaceAll: String = str.replaceAll("_", "-")
    "#);
    if let Err((_, e)) = &result { dbg!(e); }
    assert!(result.is_ok());
}

#[test]
fn typecheck_prelude_array() {
    let result = test_typecheck(r#"
      val arr = ["a", "b", "c", "d"]

      // Fields
      val length: Int = arr.length

      // Methods
      val isEmpty: Bool = arr.isEmpty()
      arr.push("e")
      arr.push(item: "e")
      val pop: String? = arr.pop()
      val popFront: String? = arr.popFront()
      val splitAt: (String[], String[]) = arr.splitAt(4)
      val concat: String[] = arr.concat(["e", "f", "g"])
      val map: Int[] = arr.map(s => s.length)
      val filter: String[] = arr.filter(s => s.length > 3)
      val reduce: Int = arr.reduce(initialValue: 0, fn: (acc, s) => acc + s.length)
      arr.forEach(s => {})
      val join1: String = arr.join()
      val join2: String = arr.join(joiner: ", ")
      val contains: Bool = arr.contains("f")
      val find: String? = arr.find(s => s == "a")
      val findIndex: (String, Int)? = arr.findIndex(s => s == "a")
      val any: Bool = arr.any(s => s == "a")
      val all: Bool = arr.all(s => s == "a")
      val none: Bool = arr.none(s => s == "a")
      val sortBy1: String[] = arr.sortBy(s => s.length)
      val sortBy2: String[] = arr.sortBy(fn: s => s.length, reverse: true)
      val dedupe: String[] = arr.dedupe()
      val dedupeBy: String[] = arr.dedupeBy(s => s.length)
      val partition: Map<Int, String[]> = arr.partition(s => s.length)
      val tally: Map<String, Int> = arr.tally()
      val tallyBy: Map<Int, Int> = arr.tallyBy(s => s.length)
      val asSet: Set<String> = arr.asSet()
      val getOr: String = arr.getOr(index: 0, default: "foo")
      val getOrElse: String = arr.getOrElse(index: 0, getDefault: () => "foo")
      arr.update(index: 4, updater: s => s.toUpper())
      val reversed: String[] = arr.reversed()
    "#);
    if let Err((_, e)) = &result { dbg!(e); }
    assert!(result.is_ok());
}

#[test]
fn typecheck_prelude_set() {
    let result = test_typecheck(r#"
      val set = #{"a", "b", "c"}

      // Fields
      val size: Int = set.size

      // Methods
      val isEmpty: Bool = set.isEmpty()
      val contains: Bool = set.contains(item: "b")
      set.insert(item: "d")
      val remove: String? = set.remove(item: "d")
      val map: Int[] = set.map(s => s.length)
      val filter: Set<String> = set.filter(s => s.length > 3)
      val reduce: Int = set.reduce(initialValue: 0, fn: (acc, s) => acc + s.length)
      val asArray: String[] = set.asArray()
      val union: Set<String> = set.union(#{"c", "d", "e"})
      val difference: Set<String> = set.difference(#{"c", "d", "e"})
      val intersection: Set<String> = set.intersection(#{"c", "d", "e"})
    "#);
    if let Err((_, e)) = &result { dbg!(e); }
    assert!(result.is_ok());
}

#[test]
fn typecheck_prelude_map() {
    let result = test_typecheck(r#"
      val map = { a: 12, b: 24, c: 48, d: 96 }

      // Fields
      val size: Int = map.size

      // Methods
      val isEmpty: Bool = map.isEmpty()
      val keys: Set<String> = map.keys()
      val values: Int[] = map.values()
      val entries: Set<(String, Int)> = map.entries()
      val containsKey: Bool = map.containsKey(key: "e")
      val mapValues1: Map<String, Bool> = map.mapValues((_, value) => value > 4)
      val mapValues2: Map<String, Bool> = map.mapValues(key => key.length > 4)
      val getOr: Int = map.getOr(key: "e", default: 192)
      val getOrElse: Int = map.getOrElse(key: "e", getDefault: () => 4)
      map.update(key: "a", updater: value => value * 2)
      val remove: Int? = map.remove(key: "a")
    "#);
    if let Err((_, e)) = &result { dbg!(e); }
    assert!(result.is_ok());
}

#[test]
fn typecheck_prelude() {
    let project = test_typecheck("").unwrap();
    let prelude_module = &project.modules[0];
    assert_eq!(PRELUDE_MODULE_ID, prelude_module.id);
    assert_eq!("prelude", prelude_module.name);

    let struct_cases = ["Tuple", "Int", "Float", "Bool", "String", "Array", "Set", "Map"];
    let struct_names = &prelude_module.structs.iter().map(|s| &s.name).collect::<HashSet<_>>();
    for name in struct_cases {
        assert!(struct_names.contains(&name.to_string()));
    }

    let enum_cases = ["Option"];
    let enum_names = &prelude_module.enums.iter().map(|s| &s.name).collect::<HashSet<_>>();
    for name in enum_cases {
        assert!(enum_names.contains(&name.to_string()));
    }
}

#[test]
fn typecheck_exports() {
    let project = test_typecheck(r#"
      export func a() {}
      export type B {}
      export enum C {}
      export val d = 1
      export val (e, f) = (true, false)
    "#).unwrap();
    let exports = &project.modules[TEST_MODULE_IDX].exports;
    let expected = HashMap::from([
        ("a".to_string(), ExportedValue::Function(FuncId(ScopeId(TEST_MODULE_ID, 0), 0))),
        ("B".to_string(), ExportedValue::Type(TypeKind::Struct(StructId(TEST_MODULE_ID, 0)))),
        ("C".to_string(), ExportedValue::Type(TypeKind::Enum(EnumId(TEST_MODULE_ID, 0)))),
        ("d".to_string(), ExportedValue::Variable(VarId(ScopeId(TEST_MODULE_ID, 0), 3))),
        ("e".to_string(), ExportedValue::Variable(VarId(ScopeId(TEST_MODULE_ID, 0), 4))),
        ("f".to_string(), ExportedValue::Variable(VarId(ScopeId(TEST_MODULE_ID, 0), 5))),
    ]);
    assert_eq!(&expected, exports);
}

#[test]
fn typecheck_failure_exports() {
    let (_, Either::Right(err)) = test_typecheck("\
      if true {\n\
        export val x = 12\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidExportScope {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 6)),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_imports() {
    assert_typecheck_ok_modules(
        r#"
          import a, B, C, d from "./2"

          d(a, B(b: a), C.C3)
        "#,
        &[
            (
                "./2",
                r#"
                  export val a = "abc"
                  export type B { b: String }
                  export enum C { C1, C2, C3 }
                  export func d(a: String, b: B, c: C) {}
                "#
            )
        ],
    );
    assert_typecheck_ok_modules(
        r#"
          import a from "./2"
          import b from "./3"

          val _: ((String, String), String) = (a, b)
        "#,
        &[
            (
                "./2",
                r#"
                  import b from "./3"
                  export val a = ("a", b)
                "#
            ),
            (
                "./3",
                r#"
                  export val b = "b"
                "#
            ),
        ],
    );
    assert_typecheck_ok_modules(
        r#"
          import * from "./2"
          import * from "./3"

          val _: (String, String) = (a, b)
        "#,
        &[
            ("./2", "export val a = \"a\""),
            ("./3", "export val b = \"b\""),
        ],
    );
    assert_typecheck_ok_modules(
        r#"
          import "./2" as two

          two.f(two.Foo(s: two.s))
          two.b(two.Bar.Bar1, two.Bar.Bar2(i: two.i))
        "#,
        &[
            (
                "./2",
                r#"
                  export type Foo { s: String }
                  export enum Bar { Bar1, Bar2(i: Int) }
                  export val s = "String"
                  export val i = 24
                  export func f(foo: Foo) {}
                  export func b(bar1: Bar, bar2: Bar) {}
                "#
            ),
        ],
    );
    assert_typecheck_ok_modules(
        r#"
          import "./2" as two

          val _: Int[] = two.f<Int[]>([])
        "#,
        &[
            ("./2", "export func f<T>(t: T): T = t"),
        ],
    );
}

#[test]
fn typecheck_failure_imports() {
    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "import a from \"./3\"",
        &[
            ("./2", "export val a = \"a\""),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownModule {
        span: Span::new(TEST_MODULE_ID, (1, 15), (1, 19)),
        module_path: "./3.abra".to_string(),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "import a from \"./2\"",
        &[
            ("./2", "import b from \"./3\"\nexport val a = (\"a\", b)"),
            ("./3", "import x from \"./bogus\"\nexport val b = \"b\""),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownModule {
        span: Span::new(ModuleId(5), (1, 15), (1, 23)),
        module_path: "./bogus.abra".to_string(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "import x from \"./2\"",
        &[
            ("./2", "export val a = \"a\""),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownExport {
        span: Span::new(TEST_MODULE_ID, (1, 8), (1, 8)),
        module_id: ModuleId(4),
        import_name: "x".to_string(),
        is_aliased: false,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "import a from \"./2\"",
        &[
            ("./2", "import x from \"./3\"\nexport val a = \"a\""),
            ("./3", "export val b = \"b\""),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownExport {
        span: Span::new(ModuleId(4), (1, 8), (1, 8)),
        module_id: ModuleId(5),
        import_name: "x".to_string(),
        is_aliased: false,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "import a from \"./2\"",
        &[
            ("./2", "import b from \"./3\"\nexport val a = \"a\""),
            ("./3", "import a from \"./2\"\nexport val b = \"b\""),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::CircularModuleImport {
        span: Span::new(ModuleId(5), (1, 15), (1, 19)),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "\
          import * from \"./2\"\n\
          import * from \"./3\"\
        ",
        &[
            ("./2", "export val a = \"a\""),
            ("./3", "export val a = \"a\""),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Span::new(TEST_MODULE_ID, (2, 8), (2, 8)),
        name: "a".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (1, 8), (1, 8))),
        kind: DuplicateNameKind::Variable,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "\
          import \"./2\" as two\n\
          println(two.xyz)\
        ",
        &[
            ("./2", "export val a = \"a\""),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownExport {
        span: Span::new(TEST_MODULE_ID, (2, 13), (2, 15)),
        module_id: ModuleId(4),
        import_name: "xyz".to_string(),
        is_aliased: true,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "\
          import \"./2\" as two\n\
          val x = (two, 1)[0].xyz\
        ",
        &[
            ("./2", "export val a = \"a\""),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownExport {
        span: Span::new(TEST_MODULE_ID, (2, 21), (2, 23)),
        module_id: ModuleId(4),
        import_name: "xyz".to_string(),
        is_aliased: true,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_literal() {
    let project = test_typecheck("1 2.34\ntrue \"hello\"").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    assert_eq!(TEST_MODULE_ID, module.id);
    assert_eq!(format!("./{TEST_MODULE_NAME}.abra"), module.name);
    assert!(module.type_ids.is_empty());

    let expected: Vec<TypedNode> = vec![
        TypedNode::Literal { token: Token::Int(Position::new(1, 1), 1), value: TypedLiteral::Int(1), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID },
        TypedNode::Literal { token: Token::Float(Position::new(1, 3), 2.34), value: TypedLiteral::Float(2.34), type_id: PRELUDE_FLOAT_TYPE_ID, resolved_type_id: PRELUDE_FLOAT_TYPE_ID },
        TypedNode::Literal { token: Token::Bool(Position::new(2, 1), true), value: TypedLiteral::Bool(true), type_id: PRELUDE_BOOL_TYPE_ID, resolved_type_id: PRELUDE_BOOL_TYPE_ID },
        TypedNode::Literal { token: Token::String(Position::new(2, 6), "hello".to_string()), value: TypedLiteral::String("hello".to_string()), type_id: PRELUDE_STRING_TYPE_ID, resolved_type_id: PRELUDE_STRING_TYPE_ID },
    ];
    assert_eq!(expected, module.code);
}

#[test]
fn typecheck_unary() {
    let project = test_typecheck("-1").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        TypedNode::Unary {
            token: Token::Minus(Position::new(1, 1)),
            op: UnaryOp::Minus,
            expr: Box::new(TypedNode::Literal { token: Token::Int(Position::new(1, 2), 1), value: TypedLiteral::Int(1), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID }),
            resolved_type_id: PRELUDE_INT_TYPE_ID,
        },
    ];
    assert_eq!(expected, module.code);

    let project = test_typecheck("-2.34\n!true").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        TypedNode::Unary {
            token: Token::Minus(Position::new(1, 1)),
            op: UnaryOp::Minus,
            expr: Box::new(TypedNode::Literal { token: Token::Float(Position::new(1, 2), 2.34), value: TypedLiteral::Float(2.34), type_id: PRELUDE_FLOAT_TYPE_ID, resolved_type_id: PRELUDE_FLOAT_TYPE_ID }),
            resolved_type_id: PRELUDE_FLOAT_TYPE_ID,
        },
        TypedNode::Unary {
            token: Token::Bang(Position::new(2, 1)),
            op: UnaryOp::Negate,
            expr: Box::new(TypedNode::Literal { token: Token::Bool(Position::new(2, 2), true), value: TypedLiteral::Bool(true), type_id: PRELUDE_BOOL_TYPE_ID, resolved_type_id: PRELUDE_BOOL_TYPE_ID }),
            resolved_type_id: PRELUDE_BOOL_TYPE_ID,
        },
    ];
    assert_eq!(expected, module.code);
}

#[test]
fn typecheck_failure_unary() {
    let (_, Either::Right(err)) = test_typecheck("-true").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 5)),
        expected: vec![PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("!1").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 2)),
        expected: vec![PRELUDE_BOOL_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_binary() {
    let cases = [
        // +
        ("1 + 2", PRELUDE_INT_TYPE_ID),
        ("1.1 + 2", PRELUDE_FLOAT_TYPE_ID),
        ("1 + 2.1", PRELUDE_FLOAT_TYPE_ID),
        ("1.1 + 2.1", PRELUDE_FLOAT_TYPE_ID),
        ("1 + \"abc\"", PRELUDE_STRING_TYPE_ID),
        ("1.1 + \"abc\"", PRELUDE_STRING_TYPE_ID),
        ("true + \"abc\"", PRELUDE_STRING_TYPE_ID),
        ("\"abc\" + ([1, 2], [3, 4])", PRELUDE_STRING_TYPE_ID),
        ("\"abc\" + \"def\"", PRELUDE_STRING_TYPE_ID),
        ("\"abc\" + 1", PRELUDE_STRING_TYPE_ID),
        ("\"abc\" + 1.1", PRELUDE_STRING_TYPE_ID),
        ("\"abc\" + false", PRELUDE_STRING_TYPE_ID),
        ("([1, 2], [3, 4]) + \"abc\"", PRELUDE_STRING_TYPE_ID),
        // -
        ("1 - 2", PRELUDE_INT_TYPE_ID),
        ("1.1 - 2", PRELUDE_FLOAT_TYPE_ID),
        ("1 - 2.1", PRELUDE_FLOAT_TYPE_ID),
        ("1.1 - 2.1", PRELUDE_FLOAT_TYPE_ID),
        // *
        ("1 * 2", PRELUDE_INT_TYPE_ID),
        ("1.1 * 2", PRELUDE_FLOAT_TYPE_ID),
        ("1 * 2.1", PRELUDE_FLOAT_TYPE_ID),
        ("1.1 * 2.1", PRELUDE_FLOAT_TYPE_ID),
        // %
        ("1 % 2", PRELUDE_INT_TYPE_ID),
        ("1.1 % 2", PRELUDE_FLOAT_TYPE_ID),
        ("1 % 2.1", PRELUDE_FLOAT_TYPE_ID),
        ("1.1 % 2.1", PRELUDE_FLOAT_TYPE_ID),
        // /
        ("1 / 2", PRELUDE_FLOAT_TYPE_ID),
        ("1.1 / 2", PRELUDE_FLOAT_TYPE_ID),
        ("1 / 2.1", PRELUDE_FLOAT_TYPE_ID),
        ("1.1 / 2.1", PRELUDE_FLOAT_TYPE_ID),
        // <
        ("1 < 2", PRELUDE_BOOL_TYPE_ID),
        ("1.1 < 2", PRELUDE_BOOL_TYPE_ID),
        ("1 < 2.1", PRELUDE_BOOL_TYPE_ID),
        ("1.1 < 2.1", PRELUDE_BOOL_TYPE_ID),
        ("\"abc\" < \"def\"", PRELUDE_BOOL_TYPE_ID),
        // <=
        ("1 <= 2", PRELUDE_BOOL_TYPE_ID),
        ("1.1 <= 2", PRELUDE_BOOL_TYPE_ID),
        ("1 <= 2.1", PRELUDE_BOOL_TYPE_ID),
        ("1.1 <= 2.1", PRELUDE_BOOL_TYPE_ID),
        ("\"abc\" <= \"def\"", PRELUDE_BOOL_TYPE_ID),
        ("12 << 34", PRELUDE_INT_TYPE_ID),
        // >
        ("1 > 2", PRELUDE_BOOL_TYPE_ID),
        ("1.1 > 2", PRELUDE_BOOL_TYPE_ID),
        ("1 > 2.1", PRELUDE_BOOL_TYPE_ID),
        ("1.1 > 2.1", PRELUDE_BOOL_TYPE_ID),
        ("\"abc\" > \"def\"", PRELUDE_BOOL_TYPE_ID),
        // >=
        ("1 >= 2", PRELUDE_BOOL_TYPE_ID),
        ("1.1 >= 2", PRELUDE_BOOL_TYPE_ID),
        ("1 >= 2.1", PRELUDE_BOOL_TYPE_ID),
        ("1.1 >= 2.1", PRELUDE_BOOL_TYPE_ID),
        ("\"abc\" >= \"def\"", PRELUDE_BOOL_TYPE_ID),
        ("12 >> 34", PRELUDE_INT_TYPE_ID),
        // ==
        ("1 == \"2\"", PRELUDE_BOOL_TYPE_ID),
        ("[1, 2] == (2, 3)", PRELUDE_BOOL_TYPE_ID),
        // !=
        ("1 != \"2\"", PRELUDE_BOOL_TYPE_ID),
        ("[1, 2] != (2, 3)", PRELUDE_BOOL_TYPE_ID),
        // ?:
        ("None ?: 123", PRELUDE_INT_TYPE_ID),
        ("1.23 ?: 0.0", PRELUDE_FLOAT_TYPE_ID),
        // &&
        ("true && false", PRELUDE_BOOL_TYPE_ID),
        ("(1 < 3) && false", PRELUDE_BOOL_TYPE_ID),
        ("0xff && 0x1", PRELUDE_INT_TYPE_ID),
        // ||
        ("true || false", PRELUDE_BOOL_TYPE_ID),
        ("(1 < 3) || false", PRELUDE_BOOL_TYPE_ID),
        ("0xff || 0x1", PRELUDE_INT_TYPE_ID),
        // ^
        ("true ^ false", PRELUDE_BOOL_TYPE_ID),
        ("(1 < 3) ^ false", PRELUDE_BOOL_TYPE_ID),
        ("0xff ^ 0x1", PRELUDE_INT_TYPE_ID),
    ];

    for (input, expected) in cases {
        let project = test_typecheck(input).unwrap();
        assert_eq!(
            expected,
            *project.modules[TEST_MODULE_IDX].code.last().unwrap().type_id(),
            "Expected `{}` to be of type {}", input, project.type_repr(&expected),
        );
    }
}

#[test]
fn typecheck_binary_assignment() {
    let cases = [
        // +=
        ("var a = 1\na += 2", PRELUDE_INT_TYPE_ID),
        ("var a = 1.0\na += 2", PRELUDE_FLOAT_TYPE_ID),
        ("var a = 1.0\na += 2.0", PRELUDE_FLOAT_TYPE_ID),
        ("var a = \"abc\"\na += 2", PRELUDE_STRING_TYPE_ID),
        // -=
        ("var a = 1\na -= 2", PRELUDE_INT_TYPE_ID),
        ("var a = 1.0\na -= 2", PRELUDE_FLOAT_TYPE_ID),
        ("var a = 1.0\na -= 2.0", PRELUDE_FLOAT_TYPE_ID),
        // *=
        ("var a = 1\na *= 2", PRELUDE_INT_TYPE_ID),
        ("var a = 1.0\na *= 2", PRELUDE_FLOAT_TYPE_ID),
        ("var a = 1.0\na *= 2.0", PRELUDE_FLOAT_TYPE_ID),
        // %=
        ("var a = 1\na %= 2", PRELUDE_INT_TYPE_ID),
        ("var a = 1.0\na %= 2", PRELUDE_FLOAT_TYPE_ID),
        ("var a = 1.0\na %= 2.0", PRELUDE_FLOAT_TYPE_ID),
        // /=
        ("var a = 1.0\na /= 2", PRELUDE_FLOAT_TYPE_ID),
        ("var a = 1.0\na /= 2.0", PRELUDE_FLOAT_TYPE_ID),
        // &&=
        ("var a = true\na &&= false", PRELUDE_BOOL_TYPE_ID),
        // ||=
        ("var a = true\na ||= false", PRELUDE_BOOL_TYPE_ID),
    ];

    for (input, expected) in cases {
        let project = test_typecheck(input).unwrap();
        assert_eq!(
            expected,
            *project.modules[TEST_MODULE_IDX].code.last().unwrap().type_id(),
            "Expected `{}` to be of type {}", input, project.type_repr(&expected),
        );
    }
}

#[test]
fn typecheck_array() {
    let project = test_typecheck("[1, 2, 3]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("[[1, 2], [3]]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(int_array_type_id)).unwrap();
    assert_eq!(expected, type_id);

    // Test deferred generic inference
    let project = test_typecheck("[[], [1, 2, 3]]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(int_array_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("[None, Option.Some(123)]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, PRELUDE_INT_TYPE_ID, Some(1))).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(int_opt_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("[Option.Some(123), None]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, PRELUDE_INT_TYPE_ID, Some(0))).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(int_opt_type_id)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: Int[] = [1, 2, 3]").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: Int[] = []").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: Int[][] = [[]]").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    let inner_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(inner_type_id)).unwrap();
    assert_eq!(expected, type_id);
}

#[test]
fn typecheck_failure_array() {
    let (_, Either::Right(err)) = test_typecheck("[1, true, 3]").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 5), (1, 8)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val a: Int[] = [true, false]").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 17), (1, 20)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val a: Int[][] = [[true]]").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 20), (1, 23)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("val a = []").unwrap_err() else { unreachable!() };
    let array_struct = &project.prelude_module().structs[project.prelude_array_struct_id.1];
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (1, 9), (1, 9)),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(array_struct.generic_ids[0])).unwrap(),
        purpose: "assignment",
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_tuple() {
    let project = test_typecheck("(1, 2, 3)").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("([1, 2], 3, \"hello\")").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![int_array_type_id, PRELUDE_INT_TYPE_ID, PRELUDE_STRING_TYPE_ID])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: (Int, Bool, String[]) = (1, true, [\"3\"])").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    let string_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_STRING_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_BOOL_TYPE_ID, string_array_type_id])).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a: ((Int, Int), (Int, Int)) = ((1, 2), (3, 4))").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    let inner_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![inner_type_id, inner_type_id])).unwrap();
    assert_eq!(expected, type_id);
}

#[test]
fn typecheck_failure_tuple() {
    let (project, Either::Right(err)) = test_typecheck("val a: Int[] = (true, 43)").unwrap_err() else { unreachable!() };
    let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let bool_int_tuple_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap();
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 16), (1, 24)),
        expected: vec![int_array_type_id],
        received: bool_int_tuple_type_id,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val a: (Bool, Float) = (true, 43)").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 31), (1, 32)),
        expected: vec![PRELUDE_FLOAT_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("val a: (Bool, Float, Bool) = (true, 4.3)").unwrap_err() else { unreachable!() };
    let bool_float_bool_tuple_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_BOOL_TYPE_ID])).unwrap();
    let bool_float_tuple_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID])).unwrap();
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 30), (1, 39)),
        expected: vec![bool_float_bool_tuple_type_id],
        received: bool_float_tuple_type_id,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_set() {
    let project = test_typecheck("#{1, 2, 3}").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("#{[1, 2], [3]}").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(int_array_type_id)).unwrap();
    assert_eq!(expected, type_id);

    // Test deferred generic inference
    let project = test_typecheck("#{#{}, #{1, 2, 3}}").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_set_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(int_set_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("#{None, Option.Some(value: 123)}").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, PRELUDE_INT_TYPE_ID, Some(1))).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(int_opt_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("#{Option.Some(value: 123), None}").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, PRELUDE_INT_TYPE_ID, Some(0))).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(int_opt_type_id)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val s: Set<Int> = #{1, 2, 3}").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val s: Set<Int> = #{}").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val s: Set<Set<Int>> = #{#{}}").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    let inner_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(inner_type_id)).unwrap();
    assert_eq!(expected, type_id);
}

#[test]
fn typecheck_failure_set() {
    let (_, Either::Right(err)) = test_typecheck("#{1, true, 3}").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 6), (1, 9)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val s: Set<Int> = #{true, false}").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 21), (1, 24)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val s: Set<Set<Int>> = #{#{true}}").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 28), (1, 31)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("val s = #{}").unwrap_err() else { unreachable!() };
    let set_struct_ = &project.prelude_module().structs[project.prelude_set_struct_id.1];
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (1, 9), (1, 10)),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(set_struct_.generic_ids[0])).unwrap(),
        purpose: "assignment",
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_map() {
    let project = test_typecheck("{ a: true, b: !false }").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(PRELUDE_STRING_TYPE_ID, PRELUDE_BOOL_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("{ (true): 1, (false): 2 }").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(PRELUDE_BOOL_TYPE_ID, PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("{ ([1, 2, 3]): 1, ([3, 4, 5]): 2 }").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(int_array_type_id, PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    // Test generic inference in values
    let project = test_typecheck("{ a: Option.Some(value: 123), b: None }").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, PRELUDE_INT_TYPE_ID, Some(0))).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(PRELUDE_STRING_TYPE_ID, int_opt_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("{ a: None, b: Option.Some(value: 123) }").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, PRELUDE_INT_TYPE_ID, Some(1))).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(PRELUDE_STRING_TYPE_ID, int_opt_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("{ a: [], b: [1, 2, 3] }").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(PRELUDE_STRING_TYPE_ID, int_array_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("{ a: [1, 2, 3], b: [] }").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(PRELUDE_STRING_TYPE_ID, int_array_type_id)).unwrap();
    assert_eq!(expected, type_id);

    // Test option inference in keys
    let project = test_typecheck("{ (None): 123, (Option.Some(value: 456)): 789 }").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, PRELUDE_INT_TYPE_ID, Some(1))).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(int_opt_type_id, PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("{ (Option.Some(value: 123)): 123, (None): 456 }").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let int_opt_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, PRELUDE_INT_TYPE_ID, Some(0))).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(int_opt_type_id, PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val m: Map<Int, Int> = { 1: 2, 3: 4 }").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val m: Map<Bool, Int> = {}").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(PRELUDE_BOOL_TYPE_ID, PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val m: Map<Int, Map<Int, Int>> = {}").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    let inner_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(PRELUDE_INT_TYPE_ID, inner_type_id)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("val m: Map<Int, Map<Int, Int>> = { 1: {} }").unwrap();
    let type_id = project.modules[TEST_MODULE_IDX].scopes[0].vars[0].type_id;
    assert_eq!(expected, type_id);
}

#[test]
fn typecheck_failure_map() {
    let (_, Either::Right(err)) = test_typecheck("{ a: 1, b: true }").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 12), (1, 15)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val m: Map<String, Int> = { (true): 1 }").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 30), (1, 33)),
        expected: vec![PRELUDE_STRING_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val m: Map<Int, Map<Int, Int>> = { 1: { 2: true } }").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 44), (1, 47)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("val m = {}").unwrap_err() else { unreachable!() };
    let map_struct = &project.prelude_module().structs[project.prelude_map_struct_id.1];
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (1, 9), (1, 9)),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(map_struct.generic_ids[0], map_struct.generic_ids[1])).unwrap(),
        purpose: "assignment",
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_none() {
    let project = test_typecheck("val x: Int? = None").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
            name: "x".to_string(),
            type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, PRELUDE_INT_TYPE_ID, None)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 5), (1, 5))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck("val x: Int? = Option.Some(value: 12)").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
            name: "x".to_string(),
            type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, PRELUDE_INT_TYPE_ID, None)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 5), (1, 5))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck("val x: Int[]? = None").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
            name: "x".to_string(),
            type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap(), None)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 5), (1, 5))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck("val x: (Int?)[] = [None]").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
            name: "x".to_string(),
            type_id: project.find_type_id(
                &ScopeId(TEST_MODULE_ID, 0),
                &project.array_type(
                    project.find_type_id(
                        &ScopeId(TEST_MODULE_ID, 0),
                        &option_type(&project, PRELUDE_INT_TYPE_ID, None),
                    ).unwrap()
                ),
            ).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 5), (1, 5))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);
}

#[test]
fn typecheck_failure_none() {
    let (project, Either::Right(err)) = test_typecheck("val x = None").unwrap_err() else { unreachable!() };
    let option_generic_t_type_id = project.get_enum_by_id(&project.prelude_option_enum_id).generic_ids[0];
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (1, 9), (1, 12)),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, option_generic_t_type_id, Some(1))).unwrap(),
        purpose: "assignment",
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("val x = [None]").unwrap_err() else { unreachable!() };
    let option_generic_t_type_id = project.get_enum_by_id(&project.prelude_option_enum_id).generic_ids[0];
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (1, 9), (1, 13)),
        type_id: project.find_type_id(
            &ScopeId(TEST_MODULE_ID, 0),
            &project.array_type(
                project.find_type_id(
                    &ScopeId(TEST_MODULE_ID, 0),
                    &option_type(&project, option_generic_t_type_id, Some(1))).unwrap()
            )).unwrap(),
        purpose: "assignment",
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("val x: Int = None").unwrap_err() else { unreachable!() };
    let option_generic_t_type_id = project.get_enum_by_id(&project.prelude_option_enum_id).generic_ids[0];
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 14), (1, 17)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &option_type(&project, option_generic_t_type_id, Some(1))).unwrap(),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_for_loop() {
    let project = test_typecheck("\
      val arr = [1, 2, 3]\n\
      for i in arr { val a = i }\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = TypedNode::ForLoop {
        token: Token::For(Position::new(2, 1)),
        binding: BindingPattern::Variable(Token::Ident(Position::new(2, 5), "i".to_string())),
        binding_var_ids: vec![VarId(ScopeId(TEST_MODULE_ID, 1), 0)],
        index_var_id: None,
        iterator: Box::new({
            let type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
            TypedNode::Identifier {
                token: Token::Ident(Position::new(2, 10), "arr".to_string()),
                var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
                type_arg_ids: vec![],
                type_id,
                resolved_type_id: type_id,
            }
        }),
        body: vec![
            TypedNode::BindingDeclaration {
                token: Token::Val(Position::new(2, 16)),
                is_exported: false,
                pattern: BindingPattern::Variable(Token::Ident(Position::new(2, 20), "a".to_string())),
                vars: vec![VarId(ScopeId(TEST_MODULE_ID, 1), 1)],
                expr: Some(Box::new(TypedNode::Identifier {
                    token: Token::Ident(Position::new(2, 24), "i".to_string()),
                    var_id: VarId(ScopeId(TEST_MODULE_ID, 1), 0),
                    type_arg_ids: vec![],
                    type_id: PRELUDE_INT_TYPE_ID,
                    resolved_type_id: PRELUDE_INT_TYPE_ID,
                })),
            }
        ],
        block_terminator: None,
    };
    assert_eq!(expected, module.code[1]);

    assert_typecheck_ok(r#"
      for str, idx in ["a", "b", "c"] {
        val _: String = str
        val _: Int = idx
      }
    "#);
    assert_typecheck_ok(r#"
      for str, idx in #{"a", "b", "c"} {
        val _: String = str
        val _: Int = idx
      }
    "#);
    assert_typecheck_ok(r#"
      for k, v in { a: 1, b: 2 } {
        val _: (String, Int) = k
        val _: Int = v
      }
    "#);
    assert_typecheck_ok(r#"
      for (k, v), i in { a: 1, b: 2 } {
        val _: String = k
        val _: Int = v
        val _: Int = i
      }
    "#);

    assert_typecheck_ok(r#"
      for (x, y), idx in [(0, 0), (1, 1)] {
        val _: Int = x
        val _: Int = y
        val _: Int = idx
      }
    "#);
}

#[test]
fn typecheck_failure_for_loop() {
    let (_, Either::Right(err)) = test_typecheck("\
      for _ in \"abc\" {}
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidControlFlowTarget {
        span: Span::new(TEST_MODULE_ID, (1, 10), (1, 14)),
        type_id: PRELUDE_STRING_TYPE_ID,
        kind: InvalidControlFlowTargetKind::ForLoop,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_while_loop() {
    let project = test_typecheck("\
      val cond = 1 < 2\n\
      while cond |c| { val a = c }\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = TypedNode::WhileLoop {
        token: Token::While(Position::new(2, 1)),
        condition: Box::new(
            TypedNode::Identifier {
                token: Token::Ident(Position::new(2, 7), "cond".to_string()),
                var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
                type_arg_ids: vec![],
                type_id: PRELUDE_BOOL_TYPE_ID,
                resolved_type_id: PRELUDE_BOOL_TYPE_ID,
            }
        ),
        condition_var_id: Some(VarId(ScopeId(TEST_MODULE_ID, 1), 0)),
        body: vec![
            TypedNode::BindingDeclaration {
                token: Token::Val(Position::new(2, 18)),
                is_exported: false,
                pattern: BindingPattern::Variable(Token::Ident(Position::new(2, 22), "a".to_string())),
                vars: vec![VarId(ScopeId(TEST_MODULE_ID, 1), 1)],
                expr: Some(Box::new(TypedNode::Identifier {
                    token: Token::Ident(Position::new(2, 26), "c".to_string()),
                    var_id: VarId(ScopeId(TEST_MODULE_ID, 1), 0),
                    type_arg_ids: vec![],
                    type_id: PRELUDE_BOOL_TYPE_ID,
                    resolved_type_id: PRELUDE_BOOL_TYPE_ID,
                })),
            }
        ],
        block_terminator: None,
    };
    assert_eq!(expected, module.code[1]);

    assert_typecheck_ok(r#"
      while [1, 2][0] |num| {
        val _: Int = num
      }
    "#);
}

#[test]
fn typecheck_failure_while_loop() {
    let (_, Either::Right(err)) = test_typecheck("\
      while \"abc\" { }
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidControlFlowTarget {
        span: Span::new(TEST_MODULE_ID, (1, 7), (1, 11)),
        type_id: PRELUDE_STRING_TYPE_ID,
        kind: InvalidControlFlowTargetKind::WhileLoop,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_break() {
    let project = test_typecheck("while true { break }").unwrap();
    assert_eq!(None, project.modules[TEST_MODULE_IDX].scopes[0].terminator);
    assert_eq!(Some(TerminatorKind::NonReturning), project.modules[TEST_MODULE_IDX].scopes[1].terminator);

    let project = test_typecheck("for i in [1, 2] { break }").unwrap();
    assert_eq!(None, project.modules[TEST_MODULE_IDX].scopes[0].terminator);
    assert_eq!(Some(TerminatorKind::NonReturning), project.modules[TEST_MODULE_IDX].scopes[1].terminator);

    assert_typecheck_ok(r#"
      while true {
        if true break
        val a = 3
      }
    "#);
    assert_typecheck_ok(r#"
      for i in [1, 2] {
        if true break
        val a = 3
      }
    "#);

    assert_typecheck_ok(r#"
      while true {
        val b = match [1, 2][0] { Int => break, _ => 123 }
        val _: Int = b
      }
    "#);
    assert_typecheck_ok(r#"
      while true {
        val b = match [1, 2][0] { Int => 123, _ => break }
        val _: Int = b
      }
    "#);
    assert_typecheck_ok(r#"
      while true {
        val b = if true break else 123
        val _: Int = b
      }
    "#);
    assert_typecheck_ok(r#"
      while true {
        val b = if true 123 else break
        val _: Int = b
      }
    "#);
}

#[test]
fn typecheck_failure_break() {
    let (_, Either::Right(err)) = test_typecheck("break").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidControlFlowTerminator {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 5)),
        terminator: ControlFlowTerminator::Break,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      while true {\n\
        break\n\
        val a = 1\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableCode {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      while true {\n\
        if true { break } else { break }\n\
        val a = 1\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableCode {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      while true {\n\
        match [1, 2, 3][0] { Int => break, _ => break }\n\
        val a = 1\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableCode {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      while true {\n\
        val a = match [1, 2, 3][0] { Int => break, _ => break }\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (2, 9), (2, 25)),
        type_id: PRELUDE_UNIT_TYPE_ID,
        purpose: "assignment",
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_continue() {
    let project = test_typecheck("while true { continue }").unwrap();
    assert_eq!(None, project.modules[TEST_MODULE_IDX].scopes[0].terminator);
    assert_eq!(Some(TerminatorKind::NonReturning), project.modules[TEST_MODULE_IDX].scopes[1].terminator);

    let project = test_typecheck("for i in [1, 2] { continue }").unwrap();
    assert_eq!(None, project.modules[TEST_MODULE_IDX].scopes[0].terminator);
    assert_eq!(Some(TerminatorKind::NonReturning), project.modules[TEST_MODULE_IDX].scopes[1].terminator);

    assert_typecheck_ok(r#"
      while true {
        if true continue
        val a = 3
      }
    "#);
    assert_typecheck_ok(r#"
      for i in [1, 2] {
        if true continue
        val a = 3
      }
    "#);

    assert_typecheck_ok(r#"
      while true {
        val b = match [1, 2][0] { Int => 123, _ => continue }
        val _: Int = b
      }
    "#);
    assert_typecheck_ok(r#"
      while true {
        val b = match [1, 2][0] { Int => continue, _ => 123 }
        val _: Int = b
      }
    "#);
    assert_typecheck_ok(r#"
      while true {
        val b = if true continue else 123
        val _: Int = b
      }
    "#);
    assert_typecheck_ok(r#"
      while true {
        val b = if true 123 else continue
        val _: Int = b
      }
    "#);
}

#[test]
fn typecheck_failure_continue() {
    let (_, Either::Right(err)) = test_typecheck("continue").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidControlFlowTerminator {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 8)),
        terminator: ControlFlowTerminator::Continue,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      while true {\n\
        continue\n\
        val a = 1\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableCode {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      while true {\n\
        if true { continue } else { continue }\n\
        val a = 1\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableCode {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      while true {\n\
        match [1, 2, 3][0] { Int => continue, _ => continue }\n\
        val a = 1\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableCode {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      while true {\n\
        val a = match [1, 2, 3][0] { Int => continue, _ => continue }\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (2, 9), (2, 25)),
        type_id: PRELUDE_UNIT_TYPE_ID,
        purpose: "assignment",
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_return() {
    assert_typecheck_ok(r#"
      func foo(): Int {
        return 123
      }
    "#);
    assert_typecheck_ok(r#"
      func foo(): Int? {
        if true return None
        else return Option.Some(value: 123)
      }
    "#);
    assert_typecheck_ok(r#"
      func foo(): Int[] {
        return []
      }
    "#);
    assert_typecheck_ok(r#"
      val _: Int[] = [1, 2, 3].map(i => {
        return i * 2
      })
    "#);

    assert_typecheck_ok(r#"
      func foo(): Int {
        val a = if true { return 6 } else 12
        return a + 1
      }
    "#);
    assert_typecheck_ok(r#"
      func foo(): Int {
        var a = 1
        while true {
          a += if true { return 6 } else 12
        }
        return a
      }
    "#);
    assert_typecheck_ok(r#"
      func foo(): Int {
        var a = 1
        for i in [1, 2] {
          a += if true { return 6 } else i
        }
        return a
      }
    "#);

    assert_typecheck_ok(r#"
      func foo() {
        while true {
          if true break else return
        }
        val a = 1
      }
    "#);
    assert_typecheck_ok(r#"
      func foo() {
        for i in [1, 2] {
          if true break else return
        }
        val a = 1
      }
    "#);
    assert_typecheck_ok(r#"
      func foo() {
        while true {
          if true continue else return
        }
        val a = 1
      }
    "#);
    assert_typecheck_ok(r#"
      func foo() {
        for i in [1, 2] {
          if true continue else return
        }
        val a = 1
      }
    "#);
}

#[test]
fn typecheck_failure_return() {
    let (_, Either::Right(err)) = test_typecheck("return 123").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidControlFlowTerminator {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 6)),
        terminator: ControlFlowTerminator::Return,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(): String {\n\
        return 24\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::ReturnTypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 8), (2, 9)),
        func_name: "foo".to_string(),
        expected: PRELUDE_STRING_TYPE_ID,
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo() {\n\
        return 24\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::ReturnTypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 8), (2, 9)),
        func_name: "foo".to_string(),
        expected: PRELUDE_UNIT_TYPE_ID,
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(): Int {\n\
        return\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::ReturnTypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 6)),
        func_name: "foo".to_string(),
        expected: PRELUDE_INT_TYPE_ID,
        received: PRELUDE_UNIT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      val _: Int[] = [1, 2, 3].map(i => {\n\
        if true { return i } else { return true }\n\
      })\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::ReturnTypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 36), (2, 39)),
        func_name: "lambda_3_0_0_1_0".to_string(),
        expected: PRELUDE_INT_TYPE_ID,
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo() {\n\
        return\n\
        val a = 1\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableCode {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo() {\n\
        while true { return }\n\
        val a = 1\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableCode {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo() {\n\
        while true {\n\
          if true return else return\n\
        }\n\
        val a = 1\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableCode {
        span: Span::new(TEST_MODULE_ID, (5, 1), (5, 3)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo() {\n\
        for i in [1, 2] { return }\n\
        val a = 1\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableCode {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo() {\n\
        for i in [1, 2] {\n\
          if true return else return\n\
        }\n\
        val a = 1\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableCode {
        span: Span::new(TEST_MODULE_ID, (5, 1), (5, 3)),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(): Int {\n\
        val a = if true return 2 else return 1\n\
        return 21\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (2, 9), (2, 38)),
        type_id: PRELUDE_UNIT_TYPE_ID,
        purpose: "assignment",
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
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
            name: "x".to_string(),
            type_id: PRELUDE_INT_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (2, 11), (2, 11))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 1),
            name: "y".to_string(),
            type_id: PRELUDE_BOOL_TYPE_ID,
            is_mutable: true,
            is_initialized: false,
            defined_span: Some(Span::new(TEST_MODULE_ID, (3, 11), (3, 11))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 2),
            name: "z".to_string(),
            type_id: PRELUDE_STRING_TYPE_ID,
            is_mutable: true,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (4, 11), (4, 11))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    let project = test_typecheck(r#"
      val (a, b, [(c1, c2), *d], (e, f)) = (1, 2.3, [(true, false), (false, true)], ("a", ("b", "c")))
    "#).unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
            name: "a".to_string(),
            type_id: PRELUDE_INT_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (2, 12), (2, 12))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 1),
            name: "b".to_string(),
            type_id: PRELUDE_FLOAT_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (2, 15), (2, 15))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 2),
            name: "c1".to_string(),
            type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.option_type(PRELUDE_BOOL_TYPE_ID)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (2, 20), (2, 21))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 3),
            name: "c2".to_string(),
            type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.option_type(PRELUDE_BOOL_TYPE_ID)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (2, 24), (2, 25))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 4),
            name: "d".to_string(),
            type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_BOOL_TYPE_ID])).unwrap())).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (2, 30), (2, 30))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 5),
            name: "e".to_string(),
            type_id: PRELUDE_STRING_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (2, 35), (2, 35))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 6),
            name: "f".to_string(),
            type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_STRING_TYPE_ID, PRELUDE_STRING_TYPE_ID])).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (2, 38), (2, 38))),
            is_captured: false,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);
}

#[test]
fn typecheck_failure_binding_declaration() {
    let (_, Either::Right(err)) = test_typecheck("val x: Bogus = 123").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownType {
        span: Span::new(TEST_MODULE_ID, (1, 8), (1, 12)),
        name: "Bogus".to_string(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val x = 1\nval x = 4").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Span::new(TEST_MODULE_ID, (2, 5), (2, 5)),
        name: "x".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (1, 5), (1, 5))),
        kind: DuplicateNameKind::Variable,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val x").unwrap_err() else { unreachable!() };
    let expected = TypeError::MissingBindingInitializer {
        span: Span::new(TEST_MODULE_ID, (1, 5), (1, 5)),
        is_mutable: false,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("val x: Int").unwrap_err() else { unreachable!() };
    let expected = TypeError::MissingBindingInitializer {
        span: Span::new(TEST_MODULE_ID, (1, 5), (1, 5)),
        is_mutable: false,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("var x").unwrap_err() else { unreachable!() };
    let expected = TypeError::MissingBindingInitializer {
        span: Span::new(TEST_MODULE_ID, (1, 5), (1, 5)),
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
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_id = StructId(TEST_MODULE_ID, 0);
    let self_instance_type_id = TypeId(ScopeId(TEST_MODULE_ID, 0), 0);
    let tostring_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 0);
    let hash_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 1);
    let eq_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 2);
    let expected = vec![
        Struct {
            id: StructId(TEST_MODULE_ID, 0),
            struct_scope_id: ScopeId(TEST_MODULE_ID, 1),
            name: "Foo".to_string(),
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
            generic_ids: vec![],
            self_type_id: self_instance_type_id,
            fields: vec![
                StructField { name: "a".to_string(), type_id: PRELUDE_STRING_TYPE_ID, is_readonly: false, defined_span: Span::new(TEST_MODULE_ID, (2, 1), (2, 1)), default_value: None },
                StructField { name: "b".to_string(), type_id: PRELUDE_INT_TYPE_ID, is_readonly: false, defined_span: Span::new(TEST_MODULE_ID, (3, 1), (3, 1)), default_value: None },
            ],
            methods: vec![tostring_func_id, hash_func_id, eq_func_id],
            static_methods: vec![],
        }
    ];
    assert_eq!(expected, module.structs);
    // Verify that the alias variable is inserted for the new type
    let expected = vec![
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
            name: "Foo".to_string(),
            type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.struct_type(struct_id)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
            is_captured: false,
            alias: VariableAlias::Type(TypeKind::Struct(struct_id)),
            is_parameter: false,
            is_exported: false,
        }
    ];
    assert_eq!(expected, module.scopes[0].vars);
    // Verify tostring method
    let expected = Function {
        id: tostring_func_id,
        fn_scope_id: ScopeId::BOGUS,
        fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.function_type(vec![], 0, false, PRELUDE_STRING_TYPE_ID)).unwrap(),
        decorators: vec![],
        name: "toString".to_string(),
        generic_ids: vec![],
        kind: FunctionKind::Method(self_instance_type_id),
        params: vec![
            FunctionParam {
                name: "self".to_string(),
                type_id: self_instance_type_id,
                var_id: VarId::BOGUS,
                defined_span: None,
                default_value: None,
                is_variadic: false,
                is_incomplete: false,
            }
        ],
        return_type_id: PRELUDE_STRING_TYPE_ID,
        defined_span: None,
        body: vec![],
        captured_vars: vec![],
        captured_closures: vec![],
    };
    assert_eq!(&expected, project.get_func_by_id(&tostring_func_id));

    // Verify that the custom toString method becomes method idx 0 regardless of where it's defined
    let project = test_typecheck("\
      type Foo {\n\
        func foo(self): Int = 12\n\
        func fooStatic(): Int = 24\n\
        func toString(self): String = \"\"\n\
      }\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_id = StructId(TEST_MODULE_ID, 0);
    let self_instance_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericInstance(struct_id, vec![])).unwrap();
    let foo_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 0);
    assert_eq!("foo", project.get_func_by_id(&foo_func_id).name);
    let foostatic_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 1);
    assert_eq!("fooStatic", project.get_func_by_id(&foostatic_func_id).name);
    let tostring_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 2);
    assert_eq!("toString", project.get_func_by_id(&tostring_func_id).name);
    let hash_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 3);
    assert_eq!("hash", project.get_func_by_id(&hash_func_id).name);
    let eq_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 4);
    assert_eq!("eq", project.get_func_by_id(&eq_func_id).name);
    let expected = vec![
        Struct {
            id: struct_id,
            struct_scope_id: ScopeId(TEST_MODULE_ID, 1),
            name: "Foo".to_string(),
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
            generic_ids: vec![],
            self_type_id: self_instance_type_id,
            fields: vec![],
            methods: vec![tostring_func_id, hash_func_id, eq_func_id, foo_func_id],
            static_methods: vec![foostatic_func_id],
        }
    ];
    assert_eq!(expected, module.structs);

    let project = test_typecheck("\
      type Foo {\n\
        a: String readonly\n\
        @Dec func foo(self): Int = 12\n\
        @Dec(\"foo\") func fooStatic(): Int = 24\n\
      }\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_id = StructId(TEST_MODULE_ID, 0);
    let self_instance_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericInstance(struct_id, vec![])).unwrap();
    let tostring_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 2);
    assert_eq!("toString", project.get_func_by_id(&tostring_func_id).name);
    let hash_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 3);
    assert_eq!("hash", project.get_func_by_id(&hash_func_id).name);
    let eq_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 4);
    assert_eq!("eq", project.get_func_by_id(&eq_func_id).name);
    let foo_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 0);
    assert_eq!("foo", project.get_func_by_id(&foo_func_id).name);
    let foostatic_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 1);
    assert_eq!("fooStatic", project.get_func_by_id(&foostatic_func_id).name);
    let expected = vec![
        Struct {
            id: struct_id,
            struct_scope_id: ScopeId(TEST_MODULE_ID, 1),
            name: "Foo".to_string(),
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
            generic_ids: vec![],
            self_type_id: self_instance_type_id,
            fields: vec![
                StructField { name: "a".to_string(), type_id: PRELUDE_STRING_TYPE_ID, is_readonly: true, defined_span: Span::new(TEST_MODULE_ID, (2, 1), (2, 1)), default_value: None },
            ],
            methods: vec![tostring_func_id, hash_func_id, eq_func_id, foo_func_id],
            static_methods: vec![foostatic_func_id],
        }
    ];
    assert_eq!(expected, module.structs);
    let expected = vec![
        Function {
            id: FuncId(ScopeId(TEST_MODULE_ID, 1), 0),
            fn_scope_id: ScopeId(TEST_MODULE_ID, 2),
            fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.function_type(vec![], 0, false, PRELUDE_INT_TYPE_ID)).unwrap(),
            decorators: vec![DecoratorInstance { name: "Dec".to_string(), args: vec![] }],
            name: "foo".to_string(),
            generic_ids: vec![],
            kind: FunctionKind::Method(self_instance_type_id),
            params: vec![
                FunctionParam {
                    name: "self".to_string(),
                    type_id: self_instance_type_id,
                    var_id: VarId(ScopeId(TEST_MODULE_ID, 2), 0),
                    defined_span: Some(Span::new(TEST_MODULE_ID, (3, 15), (3, 18))),
                    default_value: None,
                    is_variadic: false,
                    is_incomplete: false,
                }
            ],
            return_type_id: PRELUDE_INT_TYPE_ID,
            defined_span: Some(Span::new(TEST_MODULE_ID, (3, 11), (3, 13))),
            body: vec![
                TypedNode::Literal {
                    token: Token::Int(Position::new(3, 28), 12),
                    value: TypedLiteral::Int(12),
                    type_id: PRELUDE_INT_TYPE_ID,
                    resolved_type_id: PRELUDE_INT_TYPE_ID,
                }
            ],
            captured_vars: vec![],
            captured_closures: vec![],
        },
        Function {
            id: FuncId(ScopeId(TEST_MODULE_ID, 1), 1),
            fn_scope_id: ScopeId(TEST_MODULE_ID, 3),
            fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.function_type(vec![], 0, false, PRELUDE_INT_TYPE_ID)).unwrap(),
            decorators: vec![DecoratorInstance { name: "Dec".to_string(), args: vec![TypedNode::Literal { type_id: PRELUDE_STRING_TYPE_ID, resolved_type_id: PRELUDE_STRING_TYPE_ID, token: Token::String(Position::new(4, 6), "foo".to_string()), value: TypedLiteral::String("foo".to_string()) }] }],
            name: "fooStatic".to_string(),
            generic_ids: vec![],
            kind: FunctionKind::StaticMethod(self_instance_type_id), // TODO: This isn't right, it shouldn't be the instance type
            params: vec![],
            return_type_id: PRELUDE_INT_TYPE_ID,
            defined_span: Some(Span::new(TEST_MODULE_ID, (4, 18), (4, 26))),
            body: vec![
                TypedNode::Literal {
                    token: Token::Int(Position::new(4, 37), 24),
                    value: TypedLiteral::Int(24),
                    type_id: PRELUDE_INT_TYPE_ID,
                    resolved_type_id: PRELUDE_INT_TYPE_ID,
                }
            ],
            captured_vars: vec![],
            captured_closures: vec![],
        },
        Function {
            id: tostring_func_id,
            fn_scope_id: ScopeId::BOGUS,
            fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.function_type(vec![], 0, false, PRELUDE_STRING_TYPE_ID)).unwrap(),
            name: "toString".to_string(),
            decorators: vec![],
            generic_ids: vec![],
            kind: FunctionKind::Method(self_instance_type_id),
            params: vec![
                FunctionParam {
                    name: "self".to_string(),
                    type_id: self_instance_type_id,
                    var_id: VarId::BOGUS,
                    defined_span: None,
                    default_value: None,
                    is_variadic: false,
                    is_incomplete: false,
                }
            ],
            return_type_id: PRELUDE_STRING_TYPE_ID,
            defined_span: None,
            body: vec![],
            captured_vars: vec![],
            captured_closures: vec![],
        },
        Function {
            id: hash_func_id,
            fn_scope_id: ScopeId::BOGUS,
            fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.function_type(vec![], 0, false, PRELUDE_INT_TYPE_ID)).unwrap(),
            name: "hash".to_string(),
            decorators: vec![],
            generic_ids: vec![],
            kind: FunctionKind::Method(self_instance_type_id),
            params: vec![
                FunctionParam {
                    name: "self".to_string(),
                    type_id: self_instance_type_id,
                    var_id: VarId::BOGUS,
                    defined_span: None,
                    default_value: None,
                    is_variadic: false,
                    is_incomplete: false,
                }
            ],
            return_type_id: PRELUDE_INT_TYPE_ID,
            defined_span: None,
            body: vec![],
            captured_vars: vec![],
            captured_closures: vec![],
        },
        Function {
            id: eq_func_id,
            fn_scope_id: ScopeId::BOGUS,
            fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.function_type(vec![self_instance_type_id], 1, false, PRELUDE_BOOL_TYPE_ID)).unwrap(),
            name: "eq".to_string(),
            decorators: vec![],
            generic_ids: vec![],
            kind: FunctionKind::Method(self_instance_type_id),
            params: vec![
                FunctionParam {
                    name: "self".to_string(),
                    type_id: self_instance_type_id,
                    var_id: VarId::BOGUS,
                    defined_span: None,
                    default_value: None,
                    is_variadic: false,
                    is_incomplete: false,
                },
                FunctionParam {
                    name: "other".to_string(),
                    type_id: self_instance_type_id,
                    var_id: VarId::BOGUS,
                    defined_span: None,
                    default_value: None,
                    is_variadic: false,
                    is_incomplete: false,
                },
            ],
            return_type_id: PRELUDE_BOOL_TYPE_ID,
            defined_span: None,
            body: vec![],
            captured_vars: vec![],
            captured_closures: vec![],
        },
    ];
    assert_eq!(expected, module.scopes[1].funcs);

    // Test type with generics
    let project = test_typecheck("\
      type Node<T> {\n\
        value: T\n\
        func tuple<U>(self, u: U): (T, U) = (self.value, u)\n\
      }\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_id = StructId(TEST_MODULE_ID, 0);
    let struct_scope_id = ScopeId(TEST_MODULE_ID, 1);
    let tostring_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 1);
    assert_eq!("toString", project.get_func_by_id(&tostring_func_id).name);
    let hash_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 2);
    assert_eq!("hash", project.get_func_by_id(&hash_func_id).name);
    let eq_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 3);
    assert_eq!("eq", project.get_func_by_id(&eq_func_id).name);
    let tuple_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 0);
    assert_eq!("tuple", project.get_func_by_id(&tuple_func_id).name);
    let expected = vec![
        Struct {
            id: struct_id,
            struct_scope_id,
            name: "Node".to_string(),
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 9))),
            generic_ids: vec![TypeId(struct_scope_id, 0)],
            self_type_id: TypeId(ScopeId(TEST_MODULE_ID, 0), 0),
            fields: vec![
                StructField { name: "value".to_string(), type_id: TypeId(struct_scope_id, 0), is_readonly: false, defined_span: Span::new(TEST_MODULE_ID, (2, 1), (2, 5)), default_value: None },
            ],
            methods: vec![tostring_func_id, hash_func_id, eq_func_id, tuple_func_id],
            static_methods: vec![],
        }
    ];
    assert_eq!(expected, module.structs);

    // Default-valued fields
    let project = test_typecheck("\
      type Foo {\n\
        idx: Int = 0
      }\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_id = StructId(TEST_MODULE_ID, 0);
    let struct_scope_id = ScopeId(TEST_MODULE_ID, 1);
    let tostring_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 0);
    assert_eq!("toString", project.get_func_by_id(&tostring_func_id).name);
    let hash_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 1);
    assert_eq!("hash", project.get_func_by_id(&hash_func_id).name);
    let eq_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 2);
    assert_eq!("eq", project.get_func_by_id(&eq_func_id).name);
    let expected = vec![
        Struct {
            id: struct_id,
            struct_scope_id,
            name: "Foo".to_string(),
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
            generic_ids: vec![],
            self_type_id: TypeId(ScopeId(TEST_MODULE_ID, 0), 0),
            fields: vec![
                StructField {
                    name: "idx".to_string(),
                    type_id: PRELUDE_INT_TYPE_ID,
                    is_readonly: false,
                    defined_span: Span::new(TEST_MODULE_ID, (2, 1), (2, 3)),
                    default_value: Some(TypedNode::Literal {
                        token: Token::Int(Position::new(2, 12), 0),
                        value: TypedLiteral::Int(0),
                        type_id: PRELUDE_INT_TYPE_ID,
                        resolved_type_id: PRELUDE_INT_TYPE_ID,
                    }),
                },
            ],
            methods: vec![tostring_func_id, hash_func_id, eq_func_id],
            static_methods: vec![],
        }
    ];
    assert_eq!(expected, module.structs);
    assert_typecheck_ok("\
      type Foo<T> {\n\
        a: T[] = []\n\
      }\
    ");
}

#[test]
fn typecheck_failure_type_declaration() {
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {}\n\
      type Foo {}\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Span::new(TEST_MODULE_ID, (2, 6), (2, 8)),
        name: "Foo".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
        kind: DuplicateNameKind::Type,
    };
    assert_eq!(expected, err);
    // TODO: swapping the order of these two lines should result in the same error, but it doesn't since we process all `type`s' pass_0 _before_ all `enum`s'
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {}\n\
      enum Foo {}\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Span::new(TEST_MODULE_ID, (2, 6), (2, 8)),
        name: "Foo".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
        kind: DuplicateNameKind::Type,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {\n\
        a: Bogus\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownType {
        span: Span::new(TEST_MODULE_ID, (2, 4), (2, 8)),
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
        span: Span::new(TEST_MODULE_ID, (3, 6), (3, 8)),
        name: "foo".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (2, 6), (2, 8))),
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
        span: Span::new(TEST_MODULE_ID, (2, 16), (2, 19)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {\n\
        func foo(a: Int, self) {}\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidSelfParamPosition {
        span: Span::new(TEST_MODULE_ID, (2, 18), (2, 21)),
    };
    assert_eq!(expected, err);

    // Errors in method body
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {\n\
        func foo(self) = -true\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 18), (2, 22)),
        expected: vec![PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    // Duplicate generic
    let (_, Either::Right(err)) = test_typecheck("\
      type Node<T> {\n\
        func foo<T>(self, t: T) {}\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Span::new(TEST_MODULE_ID, (2, 10), (2, 10)),
        name: "T".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (1, 11), (1, 11))),
        kind: DuplicateNameKind::TypeArgument,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      type Node<T> {\n\
        func foo<T, T>(t: T) {}\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Span::new(TEST_MODULE_ID, (2, 13), (2, 13)),
        name: "T".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (2, 10), (2, 10))),
        kind: DuplicateNameKind::TypeArgument,
    };
    assert_eq!(expected, err);

    // Default-valued fields
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo {\n\
        idx: Int = true\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 12), (2, 15)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);
    let res = test_typecheck("\
      type Foo<T> {\n\
        idx: T[] = [1, 2]\n\
      }\
    ");
    assert!(res.is_err());
}

#[test]
fn typecheck_enum_declaration() {
    let project = test_typecheck("\
      enum Foo {\n\
        Bar\n\
        Baz(x: Int)\n\
      }\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let enum_id = EnumId(TEST_MODULE_ID, 0);
    let baz_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 0);
    let tostring_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 1);
    assert_eq!("toString", project.get_func_by_id(&tostring_func_id).name);
    let hash_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 2);
    assert_eq!("hash", project.get_func_by_id(&hash_func_id).name);
    let eq_func_id = FuncId(ScopeId(TEST_MODULE_ID, 1), 3);
    assert_eq!("eq", project.get_func_by_id(&eq_func_id).name);
    let self_type_id= TypeId(ScopeId(TEST_MODULE_ID, 0), 0);
    let expected = vec![
        Enum {
            id: enum_id,
            enum_scope_id: ScopeId(TEST_MODULE_ID, 1),
            name: "Foo".to_string(),
            defined_span: Span::new(TEST_MODULE_ID, (1, 6), (1, 8)),
            generic_ids: vec![],
            self_type_id,
            variants: vec![
                EnumVariant { name: "Bar".to_string(), defined_span: Span::new(TEST_MODULE_ID, (2, 1), (2, 3)), kind: EnumVariantKind::Constant },
                EnumVariant { name: "Baz".to_string(), defined_span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)), kind: EnumVariantKind::Container(baz_func_id) },
            ],
            all_variants_constant: false,
            methods: vec![tostring_func_id, hash_func_id, eq_func_id],
            static_methods: vec![],
        }
    ];
    assert_eq!(expected, module.enums);
    // Verify that the alias variable is inserted for the new type
    let expected = vec![
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
            name: "Foo".to_string(),
            type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.enum_type(enum_id)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
            is_captured: false,
            alias: VariableAlias::Type(TypeKind::Enum(enum_id)),
            is_parameter: false,
            is_exported: false,
        }
    ];
    assert_eq!(expected, module.scopes[0].vars);
    // Verify that the `Baz` variant has a function definition
    let return_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 2), &Type::GenericEnumInstance(enum_id, vec![], Some(1))).unwrap();
    let baz_variant_func = Function {
        id: baz_func_id,
        fn_scope_id: ScopeId(TEST_MODULE_ID, 2),
        fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.function_type(vec![PRELUDE_INT_TYPE_ID], 1, false, return_type_id)).unwrap(),
        decorators: vec![],
        name: "Baz".to_string(),
        generic_ids: vec![],
        kind: FunctionKind::StaticMethod(self_type_id),
        params: vec![
            FunctionParam {
                name: "x".to_string(),
                type_id: PRELUDE_INT_TYPE_ID,
                var_id: VarId(ScopeId(TEST_MODULE_ID, 2), 0),
                defined_span: Some(Span::new(TEST_MODULE_ID, (3, 5), (3, 5))),
                default_value: None,
                is_variadic: false,
                is_incomplete: false,
            }
        ],
        return_type_id,
        defined_span: Some(Span::new(TEST_MODULE_ID, (3, 1), (3, 3))),
        body: vec![],
        captured_vars: vec![],
        captured_closures: vec![],
    };
    assert_eq!(baz_variant_func, module.scopes[1].funcs[0]);
}

#[test]
fn typecheck_failure_enum_declaration() {
    let (_, Either::Right(err)) = test_typecheck("\
      enum Foo {}\n\
      enum Foo {}\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Span::new(TEST_MODULE_ID, (2, 6), (2, 8)),
        name: "Foo".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
        kind: DuplicateNameKind::Enum,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("enum Foo { Bar, Bar }").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Span::new(TEST_MODULE_ID, (1, 17), (1, 19)),
        name: "Bar".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (1, 12), (1, 14))),
        kind: DuplicateNameKind::EnumVariant,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      enum Foo {\n\
        Bar\n\
        func Bar() {}\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Span::new(TEST_MODULE_ID, (3, 6), (3, 8)),
        name: "Bar".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (2, 1), (2, 3))),
        kind: DuplicateNameKind::StaticMethodOrVariant,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      enum Foo {\n\
        Bar(x: Int, x: Int)\n\
      }\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateParameter {
        span: Span::new(TEST_MODULE_ID, (2, 13), (2, 13)),
        name: "x".to_string(),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_function_declaration() {
    // Simple example
    let project = test_typecheck("func foo(): Int = 24").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        Function {
            id: FuncId(ScopeId(TEST_MODULE_ID, 0), 0),
            fn_scope_id: ScopeId(TEST_MODULE_ID, 1),
            fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.function_type(vec![], 0, false, PRELUDE_INT_TYPE_ID)).unwrap(),
            decorators: vec![],
            name: "foo".to_string(),
            generic_ids: vec![],
            kind: FunctionKind::Freestanding,
            params: vec![],
            return_type_id: PRELUDE_INT_TYPE_ID,
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
            body: vec![
                TypedNode::Literal {
                    token: Token::Int(Position::new(1, 19), 24),
                    value: TypedLiteral::Int(24),
                    type_id: PRELUDE_INT_TYPE_ID,
                    resolved_type_id: PRELUDE_INT_TYPE_ID,
                }
            ],
            captured_vars: vec![],
            captured_closures: vec![],
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
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        Function {
            id: FuncId(ScopeId(TEST_MODULE_ID, 0), 0),
            fn_scope_id: ScopeId(TEST_MODULE_ID, 1),
            fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.function_type(vec![PRELUDE_BOOL_TYPE_ID], 1, false, PRELUDE_UNIT_TYPE_ID)).unwrap(),
            decorators: vec![],
            name: "foo".to_string(),
            generic_ids: vec![],
            kind: FunctionKind::Freestanding,
            params: vec![
                FunctionParam {
                    name: "x".to_string(),
                    type_id: PRELUDE_BOOL_TYPE_ID,
                    var_id: VarId(ScopeId(TEST_MODULE_ID, 1), 0),
                    defined_span: Some(Span::new(TEST_MODULE_ID, (2, 10), (2, 10))),
                    default_value: None,
                    is_variadic: false,
                    is_incomplete: false,
                }
            ],
            return_type_id: PRELUDE_UNIT_TYPE_ID,
            defined_span: Some(Span::new(TEST_MODULE_ID, (2, 6), (2, 8))),
            body: vec![
                TypedNode::BindingDeclaration {
                    token: Token::Val(Position::new(3, 1)),
                    is_exported: false,
                    pattern: BindingPattern::Variable(Token::Ident(Position::new(3, 5), "y".to_string())),
                    vars: vec![
                        VarId(ScopeId(TEST_MODULE_ID, 1), 1)
                    ],
                    expr: Some(Box::new(TypedNode::Unary {
                        token: Token::Bang(Position::new(3, 9)),
                        op: UnaryOp::Negate,
                        expr: Box::new(TypedNode::Identifier {
                            token: Token::Ident(Position::new(3, 10), "x".to_string()),
                            var_id: VarId(ScopeId(TEST_MODULE_ID, 1), 0),
                            type_arg_ids: vec![],
                            type_id: PRELUDE_BOOL_TYPE_ID,
                            resolved_type_id: PRELUDE_BOOL_TYPE_ID,
                        }),
                        resolved_type_id: PRELUDE_BOOL_TYPE_ID,
                    })),
                },
            ],
            captured_vars: vec![],
            captured_closures: vec![],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);

    // Test using return type as hint
    let project = test_typecheck("func foo(): (Bool, Bool)[] = []").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let bool_bool_tuple_array_type_id = project.find_type_id(
        &ScopeId(TEST_MODULE_ID, 1),
        &project.array_type(
            project.find_type_id(
                &ScopeId(TEST_MODULE_ID, 1),
                &project.tuple_type(vec![PRELUDE_BOOL_TYPE_ID, PRELUDE_BOOL_TYPE_ID]),
            ).unwrap()
        ),
    ).unwrap();
    let expected = vec![
        Function {
            id: FuncId(ScopeId(TEST_MODULE_ID, 0), 0),
            fn_scope_id: ScopeId(TEST_MODULE_ID, 1),
            fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.function_type(vec![], 0, false, bool_bool_tuple_array_type_id)).unwrap(),
            decorators: vec![],
            name: "foo".to_string(),
            generic_ids: vec![],
            kind: FunctionKind::Freestanding,
            params: vec![],
            return_type_id: bool_bool_tuple_array_type_id,
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
            body: vec![
                TypedNode::Array {
                    token: Token::LBrack(Position::new(1, 30), false),
                    items: vec![],
                    type_id: bool_bool_tuple_array_type_id,
                    resolved_type_id: bool_bool_tuple_array_type_id,
                },
            ],
            captured_vars: vec![],
            captured_closures: vec![],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);

    // Test captured variables
    let project = test_typecheck("\
      val x = 24\n\
      func foo(): Int = x\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        Function {
            id: FuncId(ScopeId(TEST_MODULE_ID, 0), 0),
            fn_scope_id: ScopeId(TEST_MODULE_ID, 1),
            fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.function_type(vec![], 0, false, PRELUDE_INT_TYPE_ID)).unwrap(),
            decorators: vec![],
            name: "foo".to_string(),
            generic_ids: vec![],
            kind: FunctionKind::Freestanding,
            params: vec![],
            return_type_id: PRELUDE_INT_TYPE_ID,
            defined_span: Some(Span::new(TEST_MODULE_ID, (2, 6), (2, 8))),
            body: vec![
                TypedNode::Identifier {
                    token: Token::Ident(Position::new(2, 19), "x".to_string()),
                    var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 1),
                    type_arg_ids: vec![],
                    type_id: PRELUDE_INT_TYPE_ID,
                    resolved_type_id: PRELUDE_INT_TYPE_ID,
                }
            ],
            captured_vars: vec![
                VarId(ScopeId(TEST_MODULE_ID, 0), 1),
            ],
            captured_closures: vec![],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);
    let expected = vec![
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
            name: "foo".to_string(),
            type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.function_type(vec![], 0, false, PRELUDE_INT_TYPE_ID)).unwrap(),
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (2, 6), (2, 8))),
            is_captured: false,
            alias: VariableAlias::Function(FuncId(ScopeId(TEST_MODULE_ID, 0), 0)),
            is_parameter: false,
            is_exported: false,
        },
        Variable {
            id: VarId(ScopeId(TEST_MODULE_ID, 0), 1),
            name: "x".to_string(),
            type_id: PRELUDE_INT_TYPE_ID,
            is_mutable: false,
            is_initialized: true,
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 5), (1, 5))),
            is_captured: true,
            alias: VariableAlias::None,
            is_parameter: false,
            is_exported: false,
        },
    ];
    assert_eq!(expected, module.scopes[0].vars);

    // Test type arguments
    let project = test_typecheck("func foo<T>(a: T): T[] = [a]").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let fn_scope_id = ScopeId(TEST_MODULE_ID, 1);
    let t_type_id = project.find_type_id_for_generic(&fn_scope_id, "T").unwrap();
    let t_array_type_id = project.find_type_id(&fn_scope_id, &project.array_type(t_type_id)).unwrap();
    let expected = vec![
        Function {
            id: FuncId(ScopeId(TEST_MODULE_ID, 0), 0),
            fn_scope_id,
            fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.function_type(vec![t_type_id], 1, false, t_array_type_id)).unwrap(),
            decorators: vec![],
            name: "foo".to_string(),
            generic_ids: vec![t_type_id],
            kind: FunctionKind::Freestanding,
            params: vec![
                FunctionParam {
                    name: "a".to_string(),
                    type_id: t_type_id,
                    var_id: VarId(ScopeId(TEST_MODULE_ID, 1), 0),
                    defined_span: Some(Span::new(TEST_MODULE_ID, (1, 13), (1, 13))),
                    default_value: None,
                    is_variadic: false,
                    is_incomplete: false,
                }
            ],
            return_type_id: t_array_type_id,
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
            body: vec![
                TypedNode::Array {
                    token: Token::LBrack(Position::new(1, 26), false),
                    items: vec![
                        TypedNode::Identifier {
                            token: Token::Ident(Position::new(1, 27), "a".to_string()),
                            var_id: VarId(fn_scope_id, 0),
                            type_arg_ids: vec![],
                            type_id: t_type_id,
                            resolved_type_id: t_type_id,
                        }
                    ],
                    type_id: t_array_type_id,
                    resolved_type_id: t_array_type_id,
                }
            ],
            captured_vars: vec![],
            captured_closures: vec![],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);

    // Test varargs
    let project = test_typecheck("func foo(*a: Int[]) {}").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let fn_scope_id = ScopeId(TEST_MODULE_ID, 1);
    let expected = vec![
        Function {
            id: FuncId(ScopeId(TEST_MODULE_ID, 0), 0),
            fn_scope_id,
            fn_type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.function_type(vec![PRELUDE_INT_TYPE_ID], 0, true, PRELUDE_UNIT_TYPE_ID)).unwrap(),
            decorators: vec![],
            name: "foo".to_string(),
            generic_ids: vec![],
            kind: FunctionKind::Freestanding,
            params: vec![
                FunctionParam {
                    name: "a".to_string(),
                    type_id: PRELUDE_INT_TYPE_ID,
                    var_id: VarId(ScopeId(TEST_MODULE_ID, 1), 0),
                    defined_span: Some(Span::new(TEST_MODULE_ID, (1, 11), (1, 11))),
                    default_value: None,
                    is_variadic: true,
                    is_incomplete: false,
                }
            ],
            return_type_id: PRELUDE_UNIT_TYPE_ID,
            defined_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
            body: vec![],
            captured_vars: vec![],
            captured_closures: vec![],
        }
    ];
    assert_eq!(expected, module.scopes[0].funcs);

    // Misc other tests
    assert!(test_typecheck("func foo(x: Bool[] = []) {}").is_ok());
    assert!(test_typecheck("func foo(x = 12): Int = x").is_ok());
    assert!(test_typecheck(r#"
      type Foo<T> { t: T }
      func f<T1>(t1: T1): Foo<T1> = Foo(t: t1)
    "#).is_ok());
}

#[test]
fn typecheck_failure_function_declaration() {
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int): Int = a\n\
      func foo(b: Bool): Bool = b\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Span::new(TEST_MODULE_ID, (2, 6), (2, 8)),
        name: "foo".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (1, 6), (1, 8))),
        kind: DuplicateNameKind::Function,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("func foo(a: Int, a: Bool) = a").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateParameter {
        span: Span::new(TEST_MODULE_ID, (1, 18), (1, 18)),
        name: "a".to_string(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("func foo(a: Bool): Int = a").unwrap_err() else { unreachable!() };
    let expected = TypeError::ReturnTypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 26), (1, 26)),
        func_name: "foo".to_string(),
        expected: PRELUDE_INT_TYPE_ID,
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    // Default-valued parameter tests

    let (_, Either::Right(err)) = test_typecheck("func foo(a = x) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownIdentifier {
        span: Span::new(TEST_MODULE_ID, (1, 14), (1, 14)),
        token: Token::Ident(Position::new(1, 14), "x".to_string()),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("func foo(a: Bool[] = [1, 2]) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 23), (1, 23)),
        expected: vec![PRELUDE_BOOL_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("func foo(a: Bool = [1, 2]) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 20), (1, 24)),
        expected: vec![PRELUDE_BOOL_TYPE_ID],
        received: project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);

    // Self-parameter tests

    let (_, Either::Right(err)) = test_typecheck("func foo(self) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidSelfParam {
        span: Span::new(TEST_MODULE_ID, (1, 10), (1, 13)),
    };
    assert_eq!(expected, err);

    // Generics tests

    let (_, Either::Right(err)) = test_typecheck("func foo<T, T>(a: T): T = a").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateName {
        span: Span::new(TEST_MODULE_ID, (1, 13), (1, 13)),
        name: "T".to_string(),
        original_span: Some(Span::new(TEST_MODULE_ID, (1, 10), (1, 10))),
        kind: DuplicateNameKind::TypeArgument,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("func foo<T, U>(a: T, b: U): (T, U) = (b, a)").unwrap_err() else { unreachable!() };
    let t_type_id = project.find_type_id_by(&ScopeId(TEST_MODULE_ID, 1), |ty| if let Type::Generic(_, name) = ty { name == "T" } else { false }).unwrap();
    let u_type_id = project.find_type_id_by(&ScopeId(TEST_MODULE_ID, 1), |ty| if let Type::Generic(_, name) = ty { name == "U" } else { false }).unwrap();
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 39), (1, 39)),
        expected: vec![t_type_id],
        received: u_type_id,
    };
    assert_eq!(expected, err);

    // Varargs tests

    let (_, Either::Right(err)) = test_typecheck("func foo(*a: Int) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidVarargType {
        span: Span::new(TEST_MODULE_ID, (1, 11), (1, 11)),
        type_id: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("func foo(*a: Int[], b: Int) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidVarargPosition {
        span: Span::new(TEST_MODULE_ID, (1, 11), (1, 11)),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("func foo(*a: Int[], b = 6) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidVarargPosition {
        span: Span::new(TEST_MODULE_ID, (1, 11), (1, 11)),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("func foo(b = 6, *a: Int[]) {}").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidRequiredParamPosition {
        span: Span::new(TEST_MODULE_ID, (1, 18), (1, 18)),
        is_variadic: true,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_function_default_param_values() {
    // Case 1, `foo` can discover function `bar`
    assert_typecheck_ok(r#"
      func foo(a: Int, b = bar()): Int = 1
      func bar(): Int = 1
    "#);
    assert_typecheck_ok(r#"
      func foo(a: Int, b = bar): Int {
        val _: () => Int = b
        1
      }
      func bar(): Int = 1
    "#);
    assert_typecheck_ok(r#"
      func foo(a: Int, b: () => Int = bar): Int = 1
      func bar(): Int = 1
    "#);

    // Case 2, `foo` can discover method `bar`
    assert_typecheck_ok(r#"
      type Foo { func bar(self): Int = 12 }
      func foo(f: Foo, b = f.bar()): Int = b
    "#);
    assert_typecheck_ok(r#"
      type Foo { func bar(self): Int = 12 }
      func foo(f: Foo, b = f.bar): Int = b()
    "#);
    assert_typecheck_ok(r#"
      enum Foo { func bar(self): Int = 12 }
      func foo(f: Foo, b = f.bar()): Int = b
    "#);
    assert_typecheck_ok(r#"
      enum Foo { func bar(self): Int = 12 }
      func foo(f: Foo, b = f.bar): Int = b()
    "#);

    // Case 3, `bar` has required arguments
    assert_typecheck_ok(r#"
      func foo(a: Int, b = bar(123)): Int = -1
      func bar(i: Int): Int = i
    "#);
    assert_typecheck_ok(r#"
      func foo(a: Int, b = bar(i: a)): Int = -1 // Referencing prior arg in expression
      func bar(i: Int): Int = i
    "#);
    assert_typecheck_ok(r#"
      func foo(a: Int, c: (Int, Int) => Int = bar): Int = 1
      func bar(a: Int): Int = a
    "#);

    // Case 4, invocation of `bar` in sub-expression
    assert_typecheck_ok(r#"
      func foo(a: Int, b = 1 + bar(123)): Int = -1
      func bar(i: Int): Int = i
    "#);

    // Case 5, `bar` contains generics
    assert_typecheck_ok(r#"
      func foo(a: Int, b = bar(123)): Int = a + b
      func bar<T>(t: T): T = t
    "#);
    assert_typecheck_ok(r#"
      func foo(a: Int, b = 1 + bar(1)): Int = a + b
      func bar<T>(t: T): T = t
    "#);

    // Case 6, recursive references
    assert_typecheck_ok(r#"
      func foo(a: Int, b = foo(1)): Int = -1
    "#);
    assert_typecheck_ok(r#"
      func foo(a: Int, b = bar()): Int = -1
      func bar(a = foo(1)): Int = 1
    "#);
}

#[test]
fn typecheck_failure_function_default_param_values() {
    // Case 1, `foo` wants function `bar` but it doesn't exist
    let (_, Either::Right(err)) = test_typecheck("func foo(a: Int, b = bar()): Int = 1").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownIdentifier {
        span: Span::new(TEST_MODULE_ID, (1, 22), (1, 24)),
        token: Token::Ident(Position::new(1, 22), "bar".to_string()),
    };
    assert_eq!(expected, err);

    // Case 2, `foo` wants method `bar` but it doesn't exist
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo { }\n\
      func foo(f: Foo, b = f.bar()): Int = b\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownMember {
        span: Span::new(TEST_MODULE_ID, (2, 24), (2, 26)),
        field_name: "bar".to_string(),
        type_id: TypeId(ScopeId(TEST_MODULE_ID, 0), 0),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      enum Foo { }\n\
      func foo(f: Foo, b = f.bar()): Int = b\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownMember {
        span: Span::new(TEST_MODULE_ID, (2, 24), (2, 26)),
        field_name: "bar".to_string(),
        type_id: TypeId(ScopeId(TEST_MODULE_ID, 0), 0),
    };
    assert_eq!(expected, err);

    // Case 3, `bar` has required arguments
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b = bar()): Int = -1\n\
      func bar(i: Int): Int = i\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidArity {
        span: Span::new(TEST_MODULE_ID, (1, 22), (1, 25)),
        num_possible_args: 1,
        num_required_args: 1,
        num_provided_args: 0,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b = bar(false)): Int = -1\n\
      func bar(i: Int): Int = i\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 26), (1, 30)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b = bar(z: 123)): Int = -1\n\
      func bar(i: Int): Int = i\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnexpectedArgumentName {
        span: Span::new(TEST_MODULE_ID, (1, 26), (1, 26)),
        arg_name: "z".to_string(),
        is_instantiation: false,
    };
    assert_eq!(expected, err);

    // Case 4, invocation of `bar` in sub-expression
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b = true && bar()): Int = -1\n\
      func bar(): Int = 1\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::IllegalOperator {
        span: Span::new(TEST_MODULE_ID, (1, 22), (1, 32)),
        op: BinaryOp::And,
        left: PRELUDE_BOOL_TYPE_ID,
        right: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    // Case 5, `bar` contains generics
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b = bar()): Int = 1\n\
      func bar<T>(): T? = None\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (1, 22), (1, 24)),
        type_id: TypeId(ScopeId(TEST_MODULE_ID, 1), 0),
        purpose: "parameter",
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b = 1 + bar(true)): Int = 1\n\
      func bar<T>(t: T): T = t\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::IllegalOperator {
        span: Span::new(TEST_MODULE_ID, (1, 22), (1, 33)),
        op: BinaryOp::Add,
        left: PRELUDE_INT_TYPE_ID,
        right: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    // Case 6, recursive/mutually-recursive references
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b = foo(1, true)): Int = -1\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 29), (1, 32)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b = bar()): Int = -1\n\
      func bar(a = foo(true)): Int = 1\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 18), (2, 21)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo<T>(a: T, b = 1 + baz(1)): T = a\n\
      func baz<T>(t: T, f = 1 + foo(1, \"b\")): T = t\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 34), (2, 36)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_STRING_TYPE_ID,
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
      func foo(a: Int[], b = true, c = 5): Int = 24\n\
      foo(a: [], c: 6)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = vec![
        TypedNode::FuncDeclaration(FuncId(ScopeId(TEST_MODULE_ID, 0), 0)),
        TypedNode::Invocation {
            target: Box::new({
                let type_id = project.find_type_id(
                    &ScopeId(TEST_MODULE_ID, 1),
                    &project.function_type(
                        vec![
                            project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap(),
                            PRELUDE_BOOL_TYPE_ID,
                            PRELUDE_INT_TYPE_ID,
                        ],
                        1,
                        false,
                        PRELUDE_INT_TYPE_ID,
                    ),
                ).unwrap();
                TypedNode::Identifier {
                    token: Token::Ident(Position::new(2, 1), "foo".to_string()),
                    var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
                    type_arg_ids: vec![],
                    type_id,
                    resolved_type_id: type_id,
                }
            }),
            arguments: vec![
                Some({
                    let type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
                    let resolved_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
                    TypedNode::Array {
                        token: Token::LBrack(Position::new(2, 8), false),
                        items: vec![],
                        type_id,
                        resolved_type_id,
                    }
                }),
                None,
                Some(TypedNode::Literal {
                    token: Token::Int(Position::new(2, 15), 6),
                    value: TypedLiteral::Int(6),
                    type_id: PRELUDE_INT_TYPE_ID,
                    resolved_type_id: PRELUDE_INT_TYPE_ID,
                }),
            ],
            type_arg_ids: vec![],
            type_id: PRELUDE_INT_TYPE_ID,
            resolved_type_id: PRELUDE_INT_TYPE_ID,
        },
    ];
    assert_eq!(expected, module.code);

    // Invoking field of type
    let project = test_typecheck("\
      type Foo { foo: (Int) => Int }\n\
      var f: Foo // This shortcut may fail in the future, using an uninitialized `var`\n\
      val foo = f.foo\n\
      foo(1)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let foo_var = &module.scopes[0].vars[2];
    let expected = Variable {
        id: VarId(ScopeId(TEST_MODULE_ID, 0), 2),
        name: "foo".to_string(),
        type_id: project.find_type_id(
            &ScopeId(TEST_MODULE_ID, 0),
            &project.function_type(vec![PRELUDE_INT_TYPE_ID], 1, false, PRELUDE_INT_TYPE_ID),
        ).unwrap(),
        is_mutable: false,
        is_initialized: true,
        defined_span: Some(Span::new(TEST_MODULE_ID, (3, 5), (3, 7))),
        is_captured: false,
        alias: VariableAlias::None,
        is_parameter: false,
        is_exported: false,
    };
    assert_eq!(&expected, foo_var);
    let accessor_invocation = &module.code[3];
    assert_eq!(PRELUDE_INT_TYPE_ID, *accessor_invocation.type_id());

    // Invoking enum variant constructor
    let project = test_typecheck("\
      enum Foo { Bar(x: Int, y: Float) }\n\
      val f = Foo.Bar\n\
      val foo = Foo.Bar(1, 2.3)\n\
      f(1, 2.3)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let enum_id = EnumId(TEST_MODULE_ID, 0);
    let f_var = &module.scopes[0].vars[1];
    let expected = Variable {
        id: VarId(ScopeId(TEST_MODULE_ID, 0), 1),
        name: "f".to_string(),
        type_id: project.find_type_id(
            &ScopeId(TEST_MODULE_ID, 1),
            &project.function_type(
                vec![PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID],
                2,
                false,
                project.find_type_id(&ScopeId(TEST_MODULE_ID, 2), &Type::GenericEnumInstance(enum_id, vec![], Some(0))).unwrap(),
            ),
        ).unwrap(),
        is_mutable: false,
        is_initialized: true,
        defined_span: Some(Span::new(TEST_MODULE_ID, (2, 5), (2, 5))),
        is_captured: false,
        alias: VariableAlias::None,
        is_parameter: false,
        is_exported: false,
    };
    assert_eq!(&expected, f_var);
    let foo_var = &module.scopes[0].vars[2];
    let expected = Variable {
        id: VarId(ScopeId(TEST_MODULE_ID, 0), 2),
        name: "foo".to_string(),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 2), &Type::GenericEnumInstance(enum_id, vec![], Some(0))).unwrap(),
        is_mutable: false,
        is_initialized: true,
        defined_span: Some(Span::new(TEST_MODULE_ID, (3, 5), (3, 7))),
        is_captured: false,
        alias: VariableAlias::None,
        is_parameter: false,
        is_exported: false,
    };
    assert_eq!(&expected, foo_var);
    let f_invocation = &module.code[3];
    assert_eq!(project.find_type_id(&ScopeId(TEST_MODULE_ID, 2), &Type::GenericEnumInstance(enum_id, vec![], Some(0))).unwrap(), *f_invocation.type_id());

    // Invoking variadic functions
    // The interesting thing here is in the arguments, so let's pull out the function declaration to cut down on noise
    let foo_fn = "func foo(a: Int, *args: Int[]) {}";
    let foo_fn_ident = |project: &Project| {
        let type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.function_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID], 1, true, PRELUDE_UNIT_TYPE_ID)).unwrap();
        TypedNode::Identifier {
            token: Token::Ident(Position::new(2, 1), "foo".to_string()),
            var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
            type_arg_ids: vec![],
            type_id,
            resolved_type_id: type_id,
        }
    };

    let project = test_typecheck(&format!("{}\nfoo(1)", &foo_fn)).unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = TypedNode::Invocation {
        target: Box::new(foo_fn_ident(&project)),
        arguments: vec![
            Some(TypedNode::Literal { token: Token::Int(Position::new(2, 5), 1), value: TypedLiteral::Int(1), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID }),
            Some({
                let type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
                TypedNode::Array {
                    token: Token::LBrack(POSITION_BOGUS, false),
                    items: vec![],
                    type_id,
                    resolved_type_id: type_id,
                }
            }),
        ],
        type_arg_ids: vec![],
        type_id: PRELUDE_UNIT_TYPE_ID,
        resolved_type_id: PRELUDE_UNIT_TYPE_ID,
    };
    assert_eq!(&expected, &module.code[1]);
    let project = test_typecheck(&format!("{}\nfoo(1, 2)", &foo_fn)).unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = TypedNode::Invocation {
        target: Box::new(foo_fn_ident(&project)),
        arguments: vec![
            Some(TypedNode::Literal { token: Token::Int(Position::new(2, 5), 1), value: TypedLiteral::Int(1), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID }),
            Some({
                let type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
                TypedNode::Array {
                    token: Token::LBrack(Position::new(2, 8), false),
                    items: vec![
                        TypedNode::Literal { token: Token::Int(Position::new(2, 8), 2), value: TypedLiteral::Int(2), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID }
                    ],
                    type_id,
                    resolved_type_id: type_id,
                }
            }),
        ],
        type_arg_ids: vec![],
        type_id: PRELUDE_UNIT_TYPE_ID,
        resolved_type_id: PRELUDE_UNIT_TYPE_ID,
    };
    assert_eq!(&expected, &module.code[1]);
    let project = test_typecheck(&format!("{}\nfoo(1, 2, 3)", &foo_fn)).unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = TypedNode::Invocation {
        target: Box::new(foo_fn_ident(&project)),
        arguments: vec![
            Some(TypedNode::Literal { token: Token::Int(Position::new(2, 5), 1), value: TypedLiteral::Int(1), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID }),
            Some({
                let type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
                TypedNode::Array {
                    token: Token::LBrack(Position::new(2, 8), false),
                    items: vec![
                        TypedNode::Literal { token: Token::Int(Position::new(2, 8), 2), value: TypedLiteral::Int(2), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID },
                        TypedNode::Literal { token: Token::Int(Position::new(2, 11), 3), value: TypedLiteral::Int(3), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID },
                    ],
                    type_id,
                    resolved_type_id: type_id,
                }
            }),
        ],
        type_arg_ids: vec![],
        type_id: PRELUDE_UNIT_TYPE_ID,
        resolved_type_id: PRELUDE_UNIT_TYPE_ID,
    };
    assert_eq!(&expected, &module.code[1]);
    // When an array literal is passed
    let project = test_typecheck(&format!("{}\nfoo(args: [2, 3], a: 1)", &foo_fn)).unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = TypedNode::Invocation {
        target: Box::new(foo_fn_ident(&project)),
        arguments: vec![
            Some(TypedNode::Literal { token: Token::Int(Position::new(2, 22), 1), value: TypedLiteral::Int(1), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID }),
            Some({
                let type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
                TypedNode::Array {
                    token: Token::LBrack(Position::new(2, 11), false),
                    items: vec![
                        TypedNode::Literal { token: Token::Int(Position::new(2, 12), 2), value: TypedLiteral::Int(2), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID },
                        TypedNode::Literal { token: Token::Int(Position::new(2, 15), 3), value: TypedLiteral::Int(3), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID },
                    ],
                    type_id,
                    resolved_type_id: type_id,
                }
            }),
        ],
        type_arg_ids: vec![],
        type_id: PRELUDE_UNIT_TYPE_ID,
        resolved_type_id: PRELUDE_UNIT_TYPE_ID,
    };
    assert_eq!(&expected, &module.code[1]);
    // When an array is passed
    assert!(test_typecheck(&format!("{}\nval a = [1, 2, 3]\nfoo(args: a, a: 1)", &foo_fn)).is_ok());

    // Option-chaining accessor
    assert_typecheck_ok(r#"
      val _: String = "foo"?.toLower()
    "#);
    assert_typecheck_ok(r#"
      type Foo { name: String }
      val arr: Foo[] = []
      val _: String? = arr[0]?.name?.toLower()
    "#);
}

#[test]
fn typecheck_invocation_generics() {
    // Test invocation with generics
    let project = test_typecheck("\
      func foo3<T, U>(a: T, b: U): (T, U) = (a, b)\n\
      foo3(\"a\", true)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_STRING_TYPE_ID, PRELUDE_BOOL_TYPE_ID])).unwrap();
    assert_eq!(expected, *module.code[1].type_id());

    let project = test_typecheck("\
      func foo<T>(a: T[]): T[] = a\n\
      val x = foo(foo([1]))\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let var = &module.scopes[0].vars[1];
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!("x", var.name);
    assert_eq!(expected, var.type_id);

    let project = test_typecheck("\
      func foo<T>(a: T, b: T): T[] = [a, b]\n\
      foo<Int?>(Option.Some(value: 1), None)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let type_id = module.code.last().unwrap().type_id();
    let expected = project.find_type_id(
        &ScopeId(TEST_MODULE_ID, 0),
        &project.array_type(
            project.find_type_id(
                &ScopeId(TEST_MODULE_ID, 0),
                &project.option_type(PRELUDE_INT_TYPE_ID),
            ).unwrap()
        ),
    ).unwrap();
    assert_eq!(expected, *type_id);

    let project = test_typecheck("\
      func makeArray<T>(): T[] = []\n\
      val arr = makeArray<Int>()\n\
      arr\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let type_id = module.code.last().unwrap().type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, *type_id);

    assert_typecheck_ok("\
      func makeArray<U>(): U[] = []\n\
      type Foo {\n\
        a: Int[] = makeArray()\n\
      }\
    ");
    assert_typecheck_ok("\
      func makeArray<U>(): U[] = []\n\
      type Foo<T> {\n\
        a: T[] = makeArray()\n\
      }\
    ");
    assert_typecheck_ok("\
      type Foo<T> { func makeFoo<T>(): Foo<T> = Foo() }\n\
      type Bar<T> {\n\
        a: Foo<T> = Foo.makeFoo()\n\
      }\
    ");
    assert_typecheck_ok("\
      type Foo<T> {\n\
        func make<T>(): Foo<T> = Foo()\n\
      }\n\
      val f = Foo.make<Int>()\n\
      val _: Foo<Int> = f\
    ");
}

#[test]
fn typecheck_failure_invocation_generics() {
    let (project, Either::Right(err)) = test_typecheck("\
      func foo<T>(a: T[]): T[] = a\n\
      val x = foo(foo([]))\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (2, 9), (2, 17)),
        type_id: project.find_type_id(
            &ScopeId(TEST_MODULE_ID, 0),
            &project.array_type(project.find_type_id_for_generic(&ScopeId(TEST_MODULE_ID, 1), "T").unwrap()),
        ).unwrap(),
        purpose: "assignment",
    };
    assert_eq!(expected, err);

    // InvalidTypeArgumentArity

    let (_, Either::Right(err)) = test_typecheck("\
      func foo<T, U>(a: T): T[] = [a]\n\
      foo<Int>(1)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 3)),
        num_required_args: 2,
        num_provided_args: 1,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo<T, U>(a: T): T[] = [a]\n\
      foo<Int, String, Bool>(1)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (2, 18), (2, 21)),
        num_required_args: 2,
        num_provided_args: 3,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      type Foo { func foo<T, U>(a: T): T[] = [a] }\n\
      Foo.foo<Int, String, Bool>(1)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (2, 22), (2, 25)),
        num_required_args: 2,
        num_provided_args: 3,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      type Foo { func foo<T, U>(self, a: T): T[] = [a] }\n\
      Foo().foo<Int, String, Bool>(1)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTypeArgumentArity {
        span: Span::new(TEST_MODULE_ID, (2, 24), (2, 27)),
        num_required_args: 2,
        num_provided_args: 3,
    };
    assert_eq!(expected, err);

    // TypeMismatch
    let (_, Either::Right(err)) = test_typecheck("\
      func foo2<T>(a: T, b: T): T[] = [a, b]\n\
      val a = foo2<String>(1, 2)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 22), (2, 22)),
        expected: vec![PRELUDE_STRING_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo2<T>(a: T, b: T): T[] = [a, b]\n\
      val a = foo2<String>(1, 2)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 22), (2, 22)),
        expected: vec![PRELUDE_STRING_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("\
      func make<T>(): T[] = []\n\
      val arr: Float[] = make<Int>()\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 20), (2, 23)),
        expected: vec![project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_FLOAT_TYPE_ID)).unwrap()],
        received: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("\
    type Foo<T> {\n\
        func make<T>(): Foo<T> = Foo()\n\
    }\n\
    val f: Foo<Float> = Foo.make<Int>()\
    ").unwrap_err() else { unreachable!() };
    let foo_struct_id = project.find_struct_by_name(&TEST_MODULE_ID, &"Foo".to_string()).unwrap().id;
    let foo_int_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericInstance(foo_struct_id, vec![PRELUDE_INT_TYPE_ID])).unwrap();
    let foo_float_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericInstance(foo_struct_id, vec![PRELUDE_FLOAT_TYPE_ID])).unwrap();
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (4, 21), (4, 28)),
        expected: vec![foo_float_type_id],
        received: foo_int_type_id,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_method_invocation() {
    // Invoking method of type
    let project = test_typecheck("\
      type Foo {\n\
        func foo(self, a: Int, b = 4): Int = a\n\
        func bar(self, a: Int): Int = a\n\
      }\n\
      val f = Foo()\n\
      val bar = f.bar\n\
      bar(1)\n\
      f.foo(2)\n\
      f.foo(a: 2)\n\
      f.foo(b: 2, a: 6)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let bar_var = &module.scopes[0].vars[2];
    let expected = Variable {
        id: VarId(ScopeId(TEST_MODULE_ID, 0), 2),
        name: "bar".to_string(),
        type_id: project.find_type_id(
            &ScopeId(TEST_MODULE_ID, 0),
            &project.function_type(vec![PRELUDE_INT_TYPE_ID], 1, false, PRELUDE_INT_TYPE_ID),
        ).unwrap(),
        is_mutable: false,
        is_initialized: true,
        defined_span: Some(Span::new(TEST_MODULE_ID, (6, 5), (6, 7))),
        is_captured: false,
        alias: VariableAlias::None,
        is_parameter: false,
        is_exported: false,
    };
    assert_eq!(&expected, bar_var);
    let var_invocation = &module.code[3];
    assert_eq!(PRELUDE_INT_TYPE_ID, *var_invocation.type_id());
    let accessor_invocation = &module.code[4];
    assert_eq!(PRELUDE_INT_TYPE_ID, *accessor_invocation.type_id());
    let accessor_invocation_arg_label = &module.code[5];
    assert_eq!(PRELUDE_INT_TYPE_ID, *accessor_invocation_arg_label.type_id());
    let accessor_invocation_arg_labels = &module.code[6];
    assert_eq!(PRELUDE_INT_TYPE_ID, *accessor_invocation_arg_labels.type_id());

    // Invoking static method of type
    let project = test_typecheck("\
      type Foo {\n\
        func foo(a: Int, b = 4): Int = a\n\
        func bar(a: Int): Int = a\n\
      }\n\
      val bar = Foo.bar\n\
      bar(1)\n\
      Foo.foo(2)\n\
      Foo.foo(a: 2)\n\
      Foo.foo(b: 2, a: 6)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let bar_var = &module.scopes[0].vars[1];
    let expected = Variable {
        id: VarId(ScopeId(TEST_MODULE_ID, 0), 1),
        name: "bar".to_string(),
        type_id: project.find_type_id(
            &ScopeId(TEST_MODULE_ID, 1),
            &project.function_type(vec![PRELUDE_INT_TYPE_ID], 1, false, PRELUDE_INT_TYPE_ID),
        ).unwrap(),
        is_mutable: false,
        is_initialized: true,
        defined_span: Some(Span::new(TEST_MODULE_ID, (5, 5), (5, 7))),
        is_captured: false,
        alias: VariableAlias::None,
        is_parameter: false,
        is_exported: false,
    };
    assert_eq!(&expected, bar_var);
    let var_invocation = &module.code[2];
    assert_eq!(PRELUDE_INT_TYPE_ID, *var_invocation.type_id());
    let accessor_invocation = &module.code[3];
    assert_eq!(PRELUDE_INT_TYPE_ID, *accessor_invocation.type_id());
    let accessor_invocation_arg_label = &module.code[4];
    assert_eq!(PRELUDE_INT_TYPE_ID, *accessor_invocation_arg_label.type_id());
    let accessor_invocation_arg_labels = &module.code[5];
    assert_eq!(PRELUDE_INT_TYPE_ID, *accessor_invocation_arg_labels.type_id());

    // Invoking static method of type (with generics)
    let project = test_typecheck("\
      type Foo<T> { func foo<T>(t: T): T = t }\n\
      Foo.foo(2)\n\
      Foo.foo(t: true)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    assert_eq!(PRELUDE_INT_TYPE_ID, *module.code[1].type_id());
    assert_eq!(PRELUDE_BOOL_TYPE_ID, *module.code[2].type_id());

    // Invoking method of enum variant
    let project = test_typecheck("\
      enum Foo {\n\
        Bar\n\
        func foo(self, a: Int, b = 4): Int = a\n\
        func bar(self, a: Int): Int = a\n\
      }\n\
      val f = Foo.Bar\n\
      val bar = f.bar\n\
      bar(1)\n\
      f.foo(2)\n\
      f.foo(a: 2)\n\
      f.foo(b: 2, a: 6)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let bar_var = &module.scopes[0].vars[2];
    let expected = Variable {
        id: VarId(ScopeId(TEST_MODULE_ID, 0), 2),
        name: "bar".to_string(),
        type_id: project.find_type_id(
            &ScopeId(TEST_MODULE_ID, 0),
            &project.function_type(vec![PRELUDE_INT_TYPE_ID], 1, false, PRELUDE_INT_TYPE_ID),
        ).unwrap(),
        is_mutable: false,
        is_initialized: true,
        defined_span: Some(Span::new(TEST_MODULE_ID, (7, 5), (7, 7))),
        is_captured: false,
        alias: VariableAlias::None,
        is_parameter: false,
        is_exported: false,
    };
    assert_eq!(&expected, bar_var);
    let var_invocation = &module.code[3];
    assert_eq!(PRELUDE_INT_TYPE_ID, *var_invocation.type_id());
    let accessor_invocation = &module.code[3];
    assert_eq!(PRELUDE_INT_TYPE_ID, *accessor_invocation.type_id());
    let accessor_invocation_arg_label = &module.code[4];
    assert_eq!(PRELUDE_INT_TYPE_ID, *accessor_invocation_arg_label.type_id());
    let accessor_invocation_arg_labels = &module.code[5];
    assert_eq!(PRELUDE_INT_TYPE_ID, *accessor_invocation_arg_labels.type_id());
}

#[test]
fn typecheck_failure_invocation() {
    let (_, Either::Right(err)) = test_typecheck("\
      val a = 1\n\
      a()\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::IllegalInvocation {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 1)),
        type_id: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(1, b: true)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::MixedArgumentType {
        span: Span::new(TEST_MODULE_ID, (2, 8), (2, 8)),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(a: 1, true)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::MixedArgumentType {
        span: Span::new(TEST_MODULE_ID, (2, 11), (2, 14)),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(a: 1, a: 2)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateArgumentLabel {
        span: Span::new(TEST_MODULE_ID, (2, 11), (2, 11)),
        name: "a".to_string(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(a: 1, d: 2)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnexpectedArgumentName {
        span: Span::new(TEST_MODULE_ID, (2, 11), (2, 11)),
        arg_name: "d".to_string(),
        is_instantiation: false,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(123, true, 456, false)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidArity {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 4)),
        num_possible_args: 3,
        num_required_args: 2,
        num_provided_args: 4,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(123)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidArity {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 4)),
        num_possible_args: 3,
        num_required_args: 2,
        num_provided_args: 1,
    };
    assert_eq!(expected, err);
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      func foo(a: Int, b: Bool, c = 123): Int = a\n\
      foo(a: 123)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidArity {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 4)),
        num_possible_args: 3,
        num_required_args: 2,
        num_provided_args: 1,
    };
    assert_eq!(expected, err);

    // Test invocation of fields
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo { foo: (Int) => Int }\n\
      var f: Foo\n\
      f.foo(a: 1)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnexpectedArgumentName {
        span: Span::new(TEST_MODULE_ID, (3, 7), (3, 7)),
        arg_name: "a".to_string(),
        is_instantiation: false,
    };
    assert_eq!(expected, err);

    // Test invocation of identifier
    let (_, Either::Right(err)) = test_typecheck("\
      var fn: (Int) => Int\n\
      fn(a: 4)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnexpectedArgumentName {
        span: Span::new(TEST_MODULE_ID, (2, 4), (2, 4)),
        arg_name: "a".to_string(),
        is_instantiation: false,
    };
    assert_eq!(expected, err);

    // Test invocation of constant enum variant
    let (_, Either::Right(err)) = test_typecheck("\
      enum Foo { Bar }\n\
      Foo.Bar()
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::IllegalEnumVariantConstruction {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 7)),
        enum_id: EnumId(TEST_MODULE_ID, 0),
        variant_idx: 0,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck(r#"["a"][0].toLower()"#).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownMember {
        span: Span::new(TEST_MODULE_ID, (1, 10), (1, 16)),
        field_name: "toLower".to_string(),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.option_type(PRELUDE_STRING_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_invocation_instantiation() {
    // Test simple case
    let project = test_typecheck("\
      type Foo { a: Int, b: Bool }\n\
      Foo(a: 12, b: true)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_ = &module.structs[0];
    let expected = TypedNode::Invocation {
        target: Box::new({
            let type_id = project.find_type_id_by(&ScopeId(TEST_MODULE_ID, 0), |ty| if let Type::Type(TypeKind::Struct(s_id)) = ty { *s_id == struct_.id } else { false }).unwrap();
            TypedNode::Identifier {
                token: Token::Ident(Position::new(2, 1), "Foo".to_string()),
                var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
                type_arg_ids: vec![],
                type_id,
                resolved_type_id: type_id,
            }
        }),
        arguments: vec![
            Some(TypedNode::Literal {
                token: Token::Int(Position::new(2, 8), 12),
                value: TypedLiteral::Int(12),
                type_id: PRELUDE_INT_TYPE_ID,
                resolved_type_id: PRELUDE_INT_TYPE_ID,
            }),
            Some(TypedNode::Literal {
                token: Token::Bool(Position::new(2, 15), true),
                value: TypedLiteral::Bool(true),
                type_id: PRELUDE_BOOL_TYPE_ID,
                resolved_type_id: PRELUDE_BOOL_TYPE_ID,
            }),
        ],
        type_arg_ids: vec![],
        type_id: struct_.self_type_id,
        resolved_type_id: struct_.self_type_id,
    };
    assert_eq!(expected, module.code[1]);

    // Test generics
    let project = test_typecheck("\
      type Node<T> { v: T }\n\
      val n = Node(v: 12)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_ = &module.structs[0];
    let var_n = &module.scopes[0].vars[1];
    assert_eq!("n", var_n.name);
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericInstance(struct_.id, vec![PRELUDE_INT_TYPE_ID])).unwrap();
    assert_eq!(expected, var_n.type_id);

    // Test with ambiguous generics, using type annotation
    let project = test_typecheck("\
      type Node<T> { v: T }\n\
      val n: Node<Int[]> = Node(v: [])\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_ = &module.structs[0];
    let var_n = &module.scopes[0].vars[1];
    assert_eq!("n", var_n.name);
    let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericInstance(struct_.id, vec![int_array_type_id])).unwrap();
    assert_eq!(expected, var_n.type_id);

    // Test with ambiguous generics, using type argument
    let project = test_typecheck("\
      type Node<T> { v: T }\n\
      val n = Node<Int[]>(v: [])\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_ = &module.structs[0];
    let var_n = &module.scopes[0].vars[1];
    assert_eq!("n", var_n.name);
    let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericInstance(struct_.id, vec![int_array_type_id])).unwrap();
    assert_eq!(expected, var_n.type_id);

    // Test default-valued fields
    let project = test_typecheck("\
      type Foo { a: Int, b: Int = 123 }\n\
      Foo(a: 24)\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_ = &module.structs[0];
    let expected = TypedNode::Invocation {
        target: Box::new({
            let type_id = project.find_type_id_by(&ScopeId(TEST_MODULE_ID, 0), |ty| if let Type::Type(TypeKind::Struct(s_id)) = ty { *s_id == struct_.id } else { false }).unwrap();
            TypedNode::Identifier {
                token: Token::Ident(Position::new(2, 1), "Foo".to_string()),
                var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
                type_arg_ids: vec![],
                type_id,
                resolved_type_id: type_id,
            }
        }),
        arguments: vec![
            Some(TypedNode::Literal {
                token: Token::Int(Position::new(2, 8), 24),
                value: TypedLiteral::Int(24),
                type_id: PRELUDE_INT_TYPE_ID,
                resolved_type_id: PRELUDE_INT_TYPE_ID,
            }),
            None,
        ],
        type_arg_ids: vec![],
        type_id: struct_.self_type_id,
        resolved_type_id: struct_.self_type_id,
    };
    assert_eq!(expected, module.code[1]);
}

#[test]
fn typecheck_failure_invocation_instantiation() {
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo { a: String }\n\
      val f = Foo(\"asdf\")\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::MissingRequiredArgumentLabels {
        span: Span::new(TEST_MODULE_ID, (2, 13), (2, 18)),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      type Foo { a: String }\n\
      Foo(b: 12)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnexpectedArgumentName {
        span: Span::new(TEST_MODULE_ID, (2, 5), (2, 5)),
        arg_name: "b".to_string(),
        is_instantiation: true,
    };
    assert_eq!(expected, err);

    // Test with ambiguous generics
    let (project, Either::Right(err)) = test_typecheck("\
      type Node<T> { v: T }\n\
      val n = Node(v: [])\
    ").unwrap_err() else { unreachable!() };
    let node_struct = project.find_struct_by_name(&TEST_MODULE_ID, &"Node".to_string()).unwrap();
    let array_struct = &project.prelude_module().structs[project.prelude_array_struct_id.1];
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (2, 9), (2, 17)),
        type_id: project.find_type_id(
            &ScopeId(TEST_MODULE_ID, 0),
            &Type::GenericInstance(
                node_struct.id,
                vec![project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(array_struct.generic_ids[0])).unwrap()],
            ),
        ).unwrap(),
        purpose: "assignment",
    };
    assert_eq!(expected, err);

    // Test with mismatched type arguments
    let (_, Either::Right(err)) = test_typecheck("\
      type Node<T> { v: T }\n\
      val n = Node<Int>(v: true)\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 22), (2, 25)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);

    // Test instantiating enum
    let (project, Either::Right(err)) = test_typecheck("\
      enum Foo { Bar }\n\
      Foo()\
    ").unwrap_err() else { unreachable!() };
    let type_id = project.find_enum_by_name(&TEST_MODULE_ID, &"Foo".to_string()).unwrap().self_type_id;
    let expected = TypeError::IllegalInvocation {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 3)),
        type_id,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_accessor() {
    // Test simple case
    let project = test_typecheck("val a = [1, 2, 3]\na.length").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let expected = TypedNode::Accessor {
        target: Box::new({
            let type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
            TypedNode::Identifier {
                token: Token::Ident(Position::new(2, 1), "a".to_string()),
                var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
                type_arg_ids: vec![],
                type_id,
                resolved_type_id: type_id,
            }
        }),
        kind: AccessorKind::Field,
        is_opt_safe: false,
        member_idx: 0,
        member_span: Range { start: Position::new(2, 3), end: Position::new(2, 8) },
        type_id: PRELUDE_INT_TYPE_ID,
        type_arg_ids: vec![],
        resolved_type_id: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, module.code[1]);

    // Defined type
    let project = test_typecheck("\
      type Foo { a: Int }\n\
      val f = Foo(a: 12)\n\
      f.a\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_ = &module.structs[0];
    let expected = TypedNode::Accessor {
        target: Box::new({
            let type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericInstance(struct_.id, vec![])).unwrap();
            TypedNode::Identifier {
                token: Token::Ident(Position::new(3, 1), "f".to_string()),
                var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 1),
                type_arg_ids: vec![],
                type_id,
                resolved_type_id: type_id,
            }
        }),
        kind: AccessorKind::Field,
        is_opt_safe: false,
        member_idx: 0,
        member_span: Range { start: Position::new(3, 3), end: Position::new(3, 3) },
        type_id: PRELUDE_INT_TYPE_ID,
        type_arg_ids: vec![],
        resolved_type_id: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, module.code[2]);

    // Accessing method
    let project = test_typecheck("\
      type Foo {\n\
        a: Int\n\
        func b(self, c: Int): Int[] = [self.a, c]\n\
      }\n\
      val f = Foo(a: 12)\n\
      f.b\
    ").unwrap();
    let module = &project.modules[TEST_MODULE_IDX];
    let struct_ = &module.structs[0];
    let expected = TypedNode::Accessor {
        target: Box::new({
            let type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericInstance(struct_.id, vec![])).unwrap();
            TypedNode::Identifier {
                token: Token::Ident(Position::new(6, 1), "f".to_string()),
                var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 1),
                type_arg_ids: vec![],
                type_id,
                resolved_type_id: type_id,
            }
        }),
        kind: AccessorKind::Method,
        is_opt_safe: false,
        member_idx: 3,
        member_span: Range { start: Position::new(6, 3), end: Position::new(6, 3) },
        type_id: {
            let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
            project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.function_type(vec![PRELUDE_INT_TYPE_ID], 1, false, int_array_type_id)).unwrap()
        },
        type_arg_ids: vec![],
        resolved_type_id: {
            let int_array_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
            project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.function_type(vec![PRELUDE_INT_TYPE_ID], 1, false, int_array_type_id)).unwrap()
        },
    };
    assert_eq!(expected, module.code[2]);

    // Option-chaining accessor
    assert_typecheck_ok(r#"
      val _: Int = "foo"?.length
    "#);
    assert_typecheck_ok(r#"
      type Foo { name: String }
      val arr: Foo[] = []
      val _: Int? = arr[0]?.name?.length
    "#);
}

#[test]
fn typecheck_failure_accessor() {
    let (project, Either::Right(err)) = test_typecheck("[1].size").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownMember {
        span: Span::new(TEST_MODULE_ID, (1, 5), (1, 8)),
        field_name: "size".to_string(),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("\
      type Foo { a: Int }\n\
      val f = Foo(a: 12)\n\
      f.b\
    ").unwrap_err() else { unreachable!() };
    let struct_ = &project.modules[TEST_MODULE_IDX].structs[0];
    let expected = TypeError::UnknownMember {
        span: Span::new(TEST_MODULE_ID, (3, 3), (3, 3)),
        field_name: "b".to_string(),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericInstance(struct_.id, vec![])).unwrap(),
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("\
      type List<T> {\n\
        items: T[]\n\
        func map<U>(self, fn: (T) => U): U[] = []\n\
      }\n\
      val l = List(items: [1, 2, 3])\n\
      val map = l.map\
    ").unwrap_err() else { unreachable!() };
    let u_type_id = project.find_type_id_for_generic(&ScopeId(TEST_MODULE_ID, 2), "U").unwrap();
    let expected = TypeError::ForbiddenAssignment {
        span: Span::new(TEST_MODULE_ID, (6, 11), (6, 15)),
        type_id: project.find_type_id(
            &ScopeId(TEST_MODULE_ID, 0),
            &project.function_type(
                vec![
                    project.find_type_id(
                        &ScopeId(TEST_MODULE_ID, 0),
                        &project.function_type(vec![PRELUDE_INT_TYPE_ID], 1, false, u_type_id),
                    ).unwrap()
                ],
                1,
                false,
                project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.array_type(u_type_id)).unwrap(),
            ),
        ).unwrap(),
        purpose: "assignment",
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck(r#"["a"][0].length"#).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownMember {
        span: Span::new(TEST_MODULE_ID, (1, 10), (1, 15)),
        field_name: "length".to_string(),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.option_type(PRELUDE_STRING_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_lambda() {
    // Test simple case
    let project = test_typecheck("val f = (a: Int, b: Float) => a + b").unwrap();
    let f_var = &project.modules[TEST_MODULE_IDX].scopes[0].vars[0];
    let expected = Variable {
        id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
        name: "f".to_string(),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 1), &project.function_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID], 2, false, PRELUDE_FLOAT_TYPE_ID)).unwrap(),
        is_mutable: false,
        is_initialized: true,
        defined_span: Some(Span::new(TEST_MODULE_ID, (1, 5), (1, 5))),
        is_captured: false,
        alias: VariableAlias::None,
        is_parameter: false,
        is_exported: false,
    };
    assert_eq!(&expected, f_var);

    // Test inferred from type hints
    let project = test_typecheck("val f: (Int) => Float = (a) => a + 0.1").unwrap();
    let f_var = &project.modules[TEST_MODULE_IDX].scopes[0].vars[0];
    let expected = Variable {
        id: VarId(ScopeId(TEST_MODULE_ID, 0), 0),
        name: "f".to_string(),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.function_type(vec![PRELUDE_INT_TYPE_ID], 1, false, PRELUDE_FLOAT_TYPE_ID)).unwrap(),
        is_mutable: false,
        is_initialized: true,
        defined_span: Some(Span::new(TEST_MODULE_ID, (1, 5), (1, 5))),
        is_captured: false,
        alias: VariableAlias::None,
        is_parameter: false,
        is_exported: false,
    };
    assert_eq!(&expected, f_var);

    // Misc others
    assert!(test_typecheck("val f: (Int) => Int = (a = 4) => a + 1").is_ok());
    assert!(test_typecheck("val f: (Int) => Int = (a, b = 4) => a + b").is_ok());
}

#[test]
fn typecheck_failure_lambda() {
    let (_, Either::Right(err)) = test_typecheck("a => 123").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownTypeForParameter {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 1)),
        param_name: "a".to_string(),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("(a: Int, b) => 123").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownTypeForParameter {
        span: Span::new(TEST_MODULE_ID, (1, 10), (1, 10)),
        param_name: "b".to_string(),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("val f: (Int) => Int = (a, b) => 123").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownTypeForParameter {
        span: Span::new(TEST_MODULE_ID, (1, 27), (1, 27)),
        param_name: "b".to_string(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val f: (Int) => Int = (a: Float) => 123").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 24), (1, 24)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_FLOAT_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("val f: (Int) => Int = (a) => a + 0.1").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 30), (1, 36)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_FLOAT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val _: String[] = [1, 2, 3].map(x => x + 1)").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 38), (1, 42)),
        expected: vec![PRELUDE_STRING_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("[1, 2, 3].map((x: String) => x)").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 16), (1, 16)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_STRING_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("[1, 2, 3].map((x: Int, y: String) => x)").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 24), (1, 24)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_STRING_TYPE_ID,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_indexing() {
    // Arrays
    let project = test_typecheck("[1, 2, 3][0]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("[1, 2, 3][0:1]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    let project = test_typecheck("val a = 2\n[1, 2, 3, 4][a:]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[1].type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("val a = 2\n[1, 2, 3, 4][:a]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[1].type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.array_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);

    // Strings
    let project = test_typecheck("\"asdf\"[0]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    assert_eq!(PRELUDE_STRING_TYPE_ID, type_id);
    let project = test_typecheck("\"asdf\"[1:2]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    assert_eq!(PRELUDE_STRING_TYPE_ID, type_id);
    let project = test_typecheck("\"asdf\"[:2]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    assert_eq!(PRELUDE_STRING_TYPE_ID, type_id);
    let project = test_typecheck("\"asdf\"[1:]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    assert_eq!(PRELUDE_STRING_TYPE_ID, type_id);

    // Tuples
    let project = test_typecheck("(0, \"a\", true)[0]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    assert_eq!(PRELUDE_INT_TYPE_ID, type_id);
    let project = test_typecheck("(0, \"a\", true)[1]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    assert_eq!(PRELUDE_STRING_TYPE_ID, type_id);
    let project = test_typecheck("(0, \"a\", true)[2]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    assert_eq!(PRELUDE_BOOL_TYPE_ID, type_id);

    // Maps
    let project = test_typecheck("{ a: 1, b: 2 }[\"a\"]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);
    let project = test_typecheck("{ ((1, 2)): true, ((2, 3)): false }[(1, 2)]").unwrap();
    let type_id = *project.modules[TEST_MODULE_IDX].code[0].type_id();
    let expected = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.option_type(PRELUDE_BOOL_TYPE_ID)).unwrap();
    assert_eq!(expected, type_id);
}

#[test]
fn typechecking_failure_indexing() {
    let (project, Either::Right(err)) = test_typecheck("#{1, 2}[4]").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidIndexableType {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 6)),
        is_range: false,
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);
    let (project, Either::Right(err)) = test_typecheck("#{1, 2}[1:4]").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidIndexableType {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 6)),
        is_range: true,
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.set_type(PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("[1, 2][\"a\"]").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 8), (1, 10)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_STRING_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("[1, 2][1:\"a\"]").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 10), (1, 12)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_STRING_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\"asdf\"[\"a\"]").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 8), (1, 10)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_STRING_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("(1, 2)[-1]").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTupleIndex {
        span: Span::new(TEST_MODULE_ID, (1, 8), (1, 9)),
        kind: InvalidTupleIndexKind::NonConstant,
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap(),
    };
    assert_eq!(expected, err);
    let (project, Either::Right(err)) = test_typecheck("(1, 2)[3]").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidTupleIndex {
        span: Span::new(TEST_MODULE_ID, (1, 8), (1, 8)),
        kind: InvalidTupleIndexKind::OutOfBounds(3),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap(),
    };
    assert_eq!(expected, err);
    let (project, Either::Right(err)) = test_typecheck("(1, 2)[1:3]").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidIndexableType {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 5)),
        is_range: true,
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.tuple_type(vec![PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID])).unwrap(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("{ a: 1 }[1]").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 10), (1, 10)),
        expected: vec![PRELUDE_STRING_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (project, Either::Right(err)) = test_typecheck("{ a: 1 }[1:2]").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidIndexableType {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 6)),
        is_range: true,
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.map_type(PRELUDE_STRING_TYPE_ID, PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_assignment() {
    // Test assign to variable
    let project = test_typecheck("var a = 12\na = 34").unwrap();
    let node = &project.modules[TEST_MODULE_IDX].code[1];
    let expected = TypedNode::Assignment {
        span: Range { start: Position::new(2, 1), end: Position::new(2, 6) },
        kind: AssignmentKind::Identifier { var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 0) },
        type_id: PRELUDE_INT_TYPE_ID,
        expr: Box::new(TypedNode::Literal {
            token: Token::Int(Position::new(2, 5), 34),
            value: TypedLiteral::Int(34),
            type_id: PRELUDE_INT_TYPE_ID,
            resolved_type_id: PRELUDE_INT_TYPE_ID,
        }),
    };
    assert_eq!(&expected, node);

    // Test assign to field
    let project = test_typecheck("\
      type Foo { foo: Int }\n\
      val f = Foo(foo: 12)\n\
      f.foo = 24\
    ").unwrap();
    let foo_struct = project.find_struct_by_name(&TEST_MODULE_ID, &"Foo".to_string()).unwrap();
    let foo_type_id = project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericInstance(foo_struct.id, vec![])).unwrap();
    let node = &project.modules[TEST_MODULE_IDX].code[2];
    let expected = TypedNode::Assignment {
        span: Range { start: Position::new(3, 1), end: Position::new(3, 10) },
        kind: AssignmentKind::Accessor {
            target: Box::new(TypedNode::Identifier {
                token: Token::Ident(Position::new(3, 1), "f".to_string()),
                var_id: VarId(ScopeId(TEST_MODULE_ID, 0), 1),
                type_arg_ids: vec![],
                type_id: foo_type_id,
                resolved_type_id: foo_type_id,
            }),
            kind: AccessorKind::Field,
            member_idx: 0,
        },
        type_id: PRELUDE_INT_TYPE_ID,
        expr: Box::new(TypedNode::Literal {
            token: Token::Int(Position::new(3, 9), 24),
            value: TypedLiteral::Int(24),
            type_id: PRELUDE_INT_TYPE_ID,
            resolved_type_id: PRELUDE_INT_TYPE_ID,
        }),
    };
    assert_eq!(&expected, node);

    // Test nested field assignment (with generics!)
    let result = test_typecheck("\
      type A<T> { x: T }\n\
      type B<T> { a: A<T> }\n\
      val b = B(a: A(x: 12))\n\
      val _: Int = b.a.x = 16\
    ");
    if result.is_err() { dbg!(&result.as_ref().unwrap_err().1); }
    assert!(result.is_ok());

    // Test indexing assignment
    let result = test_typecheck("\
      val arr = [1, 2, 3]\n\
      val _: Int = arr[3] = 5\
    ");
    if result.is_err() { dbg!(&result.as_ref().unwrap_err().1); }
    assert!(result.is_ok());
    let result = test_typecheck("\
      val map = { a: 1, b: 2 }\n\
      val _: Int = map[\"a\"] = 5\
    ");
    if result.is_err() { dbg!(&result.as_ref().unwrap_err().1); }
    assert!(result.is_ok());
}

#[test]
fn typecheck_failure_assignment() {
    // Mutability
    let (_, Either::Right(err)) = test_typecheck("val a = 12\na = 34").unwrap_err() else { unreachable!() };
    let expected = TypeError::AssignmentToImmutable {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 1)),
        var_name: "a".to_string(),
        defined_span: Some(Span::new(TEST_MODULE_ID, (1, 5), (1, 5))),
        kind: ImmutableAssignmentKind::Variable,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("func foo(a: Int) { a = 12 }").unwrap_err() else { unreachable!() };
    let expected = TypeError::AssignmentToImmutable {
        span: Span::new(TEST_MODULE_ID, (1, 20), (1, 20)),
        var_name: "a".to_string(),
        defined_span: Some(Span::new(TEST_MODULE_ID, (1, 10), (1, 10))),
        kind: ImmutableAssignmentKind::Parameter,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo { foo: Int readonly }\n\
      val f = Foo(foo: 12)\n\
      f.foo = 16\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::AssignmentToImmutable {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 5)),
        var_name: "foo".to_string(),
        defined_span: Some(Span::new(TEST_MODULE_ID, (1, 12), (1, 14))),
        kind: ImmutableAssignmentKind::Field("Foo".to_string()),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo { func foo(self) {} }\n\
      val f = Foo()\n\
      f.foo = 16\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::AssignmentToImmutable {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 5)),
        var_name: "foo".to_string(),
        defined_span: Some(Span::new(TEST_MODULE_ID, (1, 17), (1, 19))),
        kind: ImmutableAssignmentKind::Method("Foo".to_string()),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      type A<T> { x: T readonly }\n\
      type B<T> { a: A<T> }\n\
      val b = B(a: A(x: 12))\n\
      b.a.x = 16\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::AssignmentToImmutable {
        span: Span::new(TEST_MODULE_ID, (4, 1), (4, 5)),
        var_name: "x".to_string(),
        defined_span: Some(Span::new(TEST_MODULE_ID, (1, 13), (1, 13))),
        kind: ImmutableAssignmentKind::Field("A".to_string()),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      enum Foo { Bar }\n\
      Foo.Bar = 12\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::AssignmentToImmutable {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 7)),
        var_name: "Bar".to_string(),
        defined_span: Some(Span::new(TEST_MODULE_ID, (1, 12), (1, 14))),
        kind: ImmutableAssignmentKind::EnumVariant("Foo".to_string()),
    };
    assert_eq!(expected, err);

    // Mismatches in assignment type
    let (_, Either::Right(err)) = test_typecheck("var a = 12\na = false").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 5), (2, 9)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      type Foo { foo: Int }\n\
      val f = Foo(foo: 12)\n\
      f.foo = true\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (3, 9), (3, 12)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      val arr = [1, 2, 3]\n\
      arr[4] = true\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 10), (2, 13)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      val map = { a: 1, b: 2 }\n\
      map[\"a\"] = true\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 12), (2, 15)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);
    let result = test_typecheck("[1, 2, 3][0] = None");
    assert!(result.is_err());
    let result = test_typecheck("{ a: 1, b: 2 }[\"a\"] = None");
    assert!(result.is_err());

    // Invalid assignment targets
    let (_, Either::Right(err)) = test_typecheck("(0, 1)[0] = 1").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidAssignmentTarget {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 8)),
        kind: InvalidAssignmentTargetKind::IndexingTuple,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\"asdf\"[0] = \"A\"").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidAssignmentTarget {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 8)),
        kind: InvalidAssignmentTargetKind::IndexingString,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\"asdf\"[0:3] = \"A\"").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidAssignmentTarget {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 10)),
        kind: InvalidAssignmentTargetKind::IndexingRange,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("[1, 2, 3][0:3] = 1").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidAssignmentTarget {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 13)),
        kind: InvalidAssignmentTargetKind::IndexingRange,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("1 + 2 = 3").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidAssignmentTarget {
        span: Span::new(TEST_MODULE_ID, (1, 1), (1, 5)),
        kind: InvalidAssignmentTargetKind::UnsupportedAssignmentTarget,
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_if_statement() {
    let project = test_typecheck("if true { 1 }").unwrap();
    let node = &project.modules[TEST_MODULE_IDX].code[0];
    let expected = TypedNode::If {
        if_token: Token::If(Position::new(1, 1)),
        condition: Box::new(TypedNode::Literal {
            token: Token::Bool(Position::new(1, 4), true),
            value: TypedLiteral::Bool(true),
            type_id: PRELUDE_BOOL_TYPE_ID,
            resolved_type_id: PRELUDE_BOOL_TYPE_ID,
        }),
        condition_binding: None,
        if_block: vec![
            TypedNode::Literal { token: Token::Int(Position::new(1, 11), 1), value: TypedLiteral::Int(1), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID }
        ],
        if_block_terminator: None,
        else_block: vec![],
        else_block_terminator: None,
        is_statement: true,
        type_id: PRELUDE_UNIT_TYPE_ID,
        resolved_type_id: PRELUDE_UNIT_TYPE_ID,
    };
    assert_eq!(&expected, node);

    let project = test_typecheck("if true |v| { v }").unwrap();
    let node = &project.modules[TEST_MODULE_IDX].code[0];
    let condition_binding_var_id = VarId(ScopeId(TEST_MODULE_ID, 1), 0);
    let expected = TypedNode::If {
        if_token: Token::If(Position::new(1, 1)),
        condition: Box::new(TypedNode::Literal {
            token: Token::Bool(Position::new(1, 4), true),
            value: TypedLiteral::Bool(true),
            type_id: PRELUDE_BOOL_TYPE_ID,
            resolved_type_id: PRELUDE_BOOL_TYPE_ID,
        }),
        condition_binding: Some(
            (
                BindingPattern::Variable(Token::Ident(Position::new(1, 10), "v".to_string())),
                vec![condition_binding_var_id]
            )
        ),
        if_block: vec![
            TypedNode::Identifier {
                token: Token::Ident(Position::new(1, 15), "v".to_string()),
                var_id: condition_binding_var_id,
                type_arg_ids: vec![],
                type_id: PRELUDE_BOOL_TYPE_ID,
                resolved_type_id: PRELUDE_BOOL_TYPE_ID,
            }
        ],
        if_block_terminator: None,
        else_block: vec![],
        else_block_terminator: None,
        is_statement: true,
        type_id: PRELUDE_UNIT_TYPE_ID,
        resolved_type_id: PRELUDE_UNIT_TYPE_ID,
    };
    assert_eq!(&expected, node);

    let project = test_typecheck("if true { 1 } else { false }").unwrap();
    let node = &project.modules[TEST_MODULE_IDX].code[0];
    let expected = TypedNode::If {
        if_token: Token::If(Position::new(1, 1)),
        condition: Box::new(TypedNode::Literal {
            token: Token::Bool(Position::new(1, 4), true),
            value: TypedLiteral::Bool(true),
            type_id: PRELUDE_BOOL_TYPE_ID,
            resolved_type_id: PRELUDE_BOOL_TYPE_ID,
        }),
        condition_binding: None,
        if_block: vec![
            TypedNode::Literal { token: Token::Int(Position::new(1, 11), 1), value: TypedLiteral::Int(1), type_id: PRELUDE_INT_TYPE_ID, resolved_type_id: PRELUDE_INT_TYPE_ID }
        ],
        if_block_terminator: None,
        else_block: vec![
            TypedNode::Literal { token: Token::Bool(Position::new(1, 22), false), value: TypedLiteral::Bool(false), type_id: PRELUDE_BOOL_TYPE_ID, resolved_type_id: PRELUDE_BOOL_TYPE_ID }
        ],
        else_block_terminator: None,
        is_statement: true,
        type_id: PRELUDE_UNIT_TYPE_ID,
        resolved_type_id: PRELUDE_UNIT_TYPE_ID,
    };
    assert_eq!(&expected, node);

    let project = test_typecheck("func f() = if true 1 else 2").unwrap();
    let node = &project.modules[TEST_MODULE_IDX].scopes[0].funcs[0].body[0];
    assert!(matches!(node, TypedNode::If { is_statement: true, .. }));

    let project = test_typecheck("if true { if true { 1 } else { 2 } }").unwrap();
    let node = &project.modules[TEST_MODULE_IDX].code[0];
    assert!(matches!(node, TypedNode::If { is_statement: true, .. }));
    let TypedNode::If { if_block, .. } = node else { unreachable!() };
    assert!(matches!(if_block[0], TypedNode::If { is_statement: true, .. }));
}

#[test]
fn typecheck_failure_if_statement() {
    let (_, Either::Right(err)) = test_typecheck("if 123 { }").unwrap_err() else { unreachable!() };
    let expected = TypeError::InvalidControlFlowTarget {
        span: Span::new(TEST_MODULE_ID, (1, 4), (1, 6)),
        type_id: PRELUDE_INT_TYPE_ID,
        kind: InvalidControlFlowTargetKind::IfCondition,
    };
    assert_eq!(expected, err);

    // Verify variables in block scopes don't leak to outside
    let (_, Either::Right(err)) = test_typecheck("if true { val a = 1 }\na + 1").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownIdentifier {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 1)),
        token: Token::Ident(Position::new(2, 1), "a".to_string()),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("if true { } else { val a = 1 }\na + 1").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownIdentifier {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 1)),
        token: Token::Ident(Position::new(2, 1), "a".to_string()),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_if_expression() {
    assert!(test_typecheck("val x = if true { [1, 2][0] } else { Option.Some(value: 1) }\nval _: Int? = x").is_ok());
    assert!(test_typecheck("val x = if true { Option.Some(value: 1) } else { [1, 2][0] }\nval _: Int? = x").is_ok());
}

#[test]
fn typecheck_match_statement_and_expression() {
    assert_typecheck_ok(r#"
      match [1, 2][3] {
        None => 123
        _ => "abc"
      }
    "#);

    let project = test_typecheck("val x = match true { _ => 1 }").unwrap();
    let node = &project.modules[TEST_MODULE_IDX].code[0];
    let TypedNode::BindingDeclaration { expr, .. } = node.clone() else { panic!() };
    assert!(matches!(**expr.as_ref().unwrap(), TypedNode::Match { is_statement: false, .. }));
    let var_x = &project.modules[TEST_MODULE_IDX].scopes[0].vars[0];
    assert_eq!("x", var_x.name);
    assert_eq!(PRELUDE_INT_TYPE_ID, var_x.type_id);

    assert_typecheck_ok(r#"
      val x = match 123 { _ x => x + 1 }
      val _: Int = x
    "#);
    assert_typecheck_ok(r#"
      val x: Int[] = match 123 { _ x => [x] }
    "#);
    assert_typecheck_ok(r#"
      val x: Int[] = match 123 { _ => [] }
    "#);
    assert_typecheck_ok(r#"
      val x: Int? = match 123 { _ x => Option.Some(value: x + 1) }
    "#);
    assert_typecheck_ok(r#"
      val x: Int? = match 123 { _ => None }
    "#);

    assert_typecheck_ok(r#"
      val x: Int? = match [1, 2][0] {
        None => Option.Some(value: 123)
        _ => None
      }
    "#);

    assert_typecheck_ok(r#"
      val x = match [1, 2][0] {
        None => Option.Some(value: 1)
        1 => Option.Some(value: 2)
        2 => Option.Some(value: 3)
        _ => None
      }
    "#);
    assert_typecheck_ok(r#"
      val x = match 1 > 2 {
        false => 0
        true => 1
      }
    "#);
    assert_typecheck_ok(r#"
      val x = match 123 {
        Int i => i * 2
      }
    "#);
    assert_typecheck_ok(r#"
      val x = match [1, 2][0] { Int i => 0, None => 1 }
    "#);
    assert_typecheck_ok(r#"
      val x = match [1, 2][0] { None => 1, Int i => 0 }
    "#);

    assert_typecheck_ok(r#"
      enum Color { Red, Green, Blue }
      match [Color.Red][0] {
        Color.Red => 1
        Color.Green => 2
        Color.Blue => 3
        None => 4
      }
    "#);
    assert_typecheck_ok(r#"
      enum Color { Red, Green, Blue, RGB(r: Int, g: Int, b: Int) }
      match Color.Red {
        Color.RGB c => c.r + c.b + c.b
        _ => 0
      }
    "#);

    // Types referenced from import aliases
    assert_typecheck_ok_modules(
        r#"
          import C from "./2"
          import "./2" as two

          val c = C.C2
          match c {
            two.C.C1 => {}
            two.C.C2 => {}
            two.C.C3 => {}
          }
        "#,
        &[
            (
                "./2",
                r#"
                  export enum C { C1, C2, C3 }
                "#
            )
        ],
    );
    assert_typecheck_ok_modules(
        r#"
          import B from "./2"
          import "./2" as two

          val arr = [B(b: "foo"), B(b: "bar")]
          match arr[0] {
            two.B => {}
            None => {}
          }
        "#,
        &[
            (
                "./2",
                r#"
                  export type B { b: String }
                "#
            )
        ],
    );
}

#[test]
fn typecheck_failure_match_expression() {
    let (_, Either::Right(err)) = test_typecheck("\
      val x = match 123 {\n\
        _ => 1\n\
        Int i => 2\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableMatchCase {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)),
        kind: UnreachableMatchCaseKind::AlreadyCovered,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      val x = match 123 {\n\
        1 => 1\n\
        1 => 2\n\
        _ => 3\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateMatchCase {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 1)),
        orig_span: Span::new(TEST_MODULE_ID, (2, 1), (2, 1)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      val x = match 123 {\n\
        Int => 1\n\
        Int => 2\n\
        _ => 3\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateMatchCase {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 3)),
        orig_span: Span::new(TEST_MODULE_ID, (2, 1), (2, 3)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      val x = match [1, 2][0] {\n\
        None => 1\n\
        None => 2\n\
        _ => 3\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateMatchCase {
        span: Span::new(TEST_MODULE_ID, (3, 1), (3, 4)),
        orig_span: Span::new(TEST_MODULE_ID, (2, 1), (2, 4)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      enum Color { Red, Green, Blue }\n\
      match [Color.Red][0] {\n\
        Color.Red => 1\n\
        Color.Green => 2\n\
        Color.Red => 3\n\
        None => 4\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateMatchCase {
        span: Span::new(TEST_MODULE_ID, (5, 1), (5, 9)),
        orig_span: Span::new(TEST_MODULE_ID, (3, 1), (3, 9)),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      val x = match 123 {\n\
        _ => {}\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::EmptyMatchBlock {
        span: Span::new(TEST_MODULE_ID, (2, 1), (2, 1)),
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("\
      val x = match [1, 2][0] { None n => n, _ => Option.Some(value: 123) }\n\
      val _: Int = x\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 14), (2, 14)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      val _ = match 123 { None => 123, _ => 123 }\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableMatchCase {
        span: Span::new(TEST_MODULE_ID, (1, 21), (1, 24)),
        kind: UnreachableMatchCaseKind::NoTypeOverlap {
            case_type: None,
            target_type: PRELUDE_INT_TYPE_ID,
            target_span: Span::new(TEST_MODULE_ID, (1, 15), (1, 17)),
        },
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      val _ = match 123 { \"foo\" => 123, _ => 123 }\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableMatchCase {
        span: Span::new(TEST_MODULE_ID, (1, 21), (1, 25)),
        kind: UnreachableMatchCaseKind::NoTypeOverlap {
            case_type: Some(PRELUDE_STRING_TYPE_ID),
            target_type: PRELUDE_INT_TYPE_ID,
            target_span: Span::new(TEST_MODULE_ID, (1, 15), (1, 17)),
        },
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      val _ = match 123 { String => 123, _ => 123 }\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableMatchCase {
        span: Span::new(TEST_MODULE_ID, (1, 21), (1, 26)),
        kind: UnreachableMatchCaseKind::NoTypeOverlap {
            case_type: Some(PRELUDE_STRING_TYPE_ID),
            target_type: PRELUDE_INT_TYPE_ID,
            target_span: Span::new(TEST_MODULE_ID, (1, 15), (1, 17)),
        },
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      enum Color { Red, Green, Blue }\n\
      val _ = match 123 { Color.Red => 123, _ => 123 }\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnreachableMatchCase {
        span: Span::new(TEST_MODULE_ID, (2, 21), (2, 29)),
        kind: UnreachableMatchCaseKind::NoTypeOverlap {
            case_type: Some(TypeId(ScopeId(TEST_MODULE_ID, 0), 0)),
            target_type: PRELUDE_INT_TYPE_ID,
            target_span: Span::new(TEST_MODULE_ID, (2, 15), (2, 17)),
        },
    };
    assert_eq!(expected, err);
    let (project, Either::Right(err)) = test_typecheck("\
      enum Color { Red, Green, Blue }\n\
      enum Color2 { Purple, Pink, Orange }\n\
      val _ = match Color.Green { Color2.Purple => 123, _ => 123 }\n\
    ").unwrap_err() else { unreachable!() };
    let color_enum = project.find_enum_by_name(&TEST_MODULE_ID, &"Color".to_string()).unwrap();
    let color2_enum = project.find_enum_by_name(&TEST_MODULE_ID, &"Color2".to_string()).unwrap();
    let expected = TypeError::UnreachableMatchCase {
        span: Span::new(TEST_MODULE_ID, (3, 29), (3, 41)),
        kind: UnreachableMatchCaseKind::NoTypeOverlap {
            case_type: Some(project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericEnumInstance(color2_enum.id, vec![], None)).unwrap()),
            target_type: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericEnumInstance(color_enum.id, vec![], Some(1))).unwrap(),
            target_span: Span::new(TEST_MODULE_ID, (3, 15), (3, 25)),
        },
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      val x = match [1, 2][0] {\n\
        None => \"asd\"\n\
        _ => 123\n\
      }\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::BranchTypeMismatch {
        span: Span::new(TEST_MODULE_ID, (3, 6), (3, 8)),
        orig_span: Span::new(TEST_MODULE_ID, (2, 9), (2, 13)),
        expected: PRELUDE_STRING_TYPE_ID,
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("\
      val _ = match 123 { String(s) => 123, _ => 123 }\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::UnimplementedFeature {
        span: Span::new(TEST_MODULE_ID, (1, 28), (1, 28)),
        desc: "destructuring of non-enum type in match case",
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("\
      val _ = match [0, 1][0] { Int => 123 }\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::NonExhaustiveMatch {
        span: Span::new(TEST_MODULE_ID, (1, 9), (1, 13)),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &project.option_type(PRELUDE_INT_TYPE_ID)).unwrap(),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      val _ = match 123 { 1 => 1, 2 => 2 }\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::NonExhaustiveMatch {
        span: Span::new(TEST_MODULE_ID, (1, 9), (1, 13)),
        type_id: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (project, Either::Right(err)) = test_typecheck("\
      enum Color { Red, Green, Blue }\n\
      val _ = match Color.Red { Color.Red => 0, Color.Blue => 1 }\n\
    ").unwrap_err() else { unreachable!() };
    let color_enum = project.find_enum_by_name(&TEST_MODULE_ID, &"Color".to_string()).unwrap();
    let expected = TypeError::NonExhaustiveMatch {
        span: Span::new(TEST_MODULE_ID, (2, 9), (2, 13)),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericEnumInstance(color_enum.id, vec![], Some(0))).unwrap(),
    };
    assert_eq!(expected, err);

    let (project, Either::Right(err)) = test_typecheck("\
      enum Color { Red }\n\
      val _ = match Color.Red { Color.Red(r) => 0 }\n\
    ").unwrap_err() else { unreachable!() };
    let color_enum = project.find_enum_by_name(&TEST_MODULE_ID, &"Color".to_string()).unwrap();
    let expected = TypeError::DestructuringMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 37), (2, 37)),
        kind: DestructuringMismatchKind::InvalidDestructureTarget,
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 0), &Type::GenericEnumInstance(color_enum.id, vec![], Some(0))).unwrap(),
    };
    assert_eq!(expected, err);
    let (project, Either::Right(err)) = test_typecheck("\
      enum Color { Red(r: Int), Blue }\n\
      val _ = match Color.Blue { Color.Red(r1, r2) => 0, Color.Blue => 1 }\n\
    ").unwrap_err() else { unreachable!() };
    let color_enum = project.find_enum_by_name(&TEST_MODULE_ID, &"Color".to_string()).unwrap();
    let expected = TypeError::DestructuringMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 42), (2, 43)),
        kind: DestructuringMismatchKind::InvalidEnumVariantArity(1, 2),
        type_id: project.find_type_id(&ScopeId(TEST_MODULE_ID, 3), &Type::GenericEnumInstance(color_enum.id, vec![], Some(0))).unwrap(),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("\
      enum Color { Red(r: Int), Blue }\n\
      val _ = match Color.Blue { Color.Red(\"foo\") => 0, Color.Blue => 1 }\n\
    ").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (2, 38), (2, 42)),
        expected: vec![PRELUDE_INT_TYPE_ID],
        received: PRELUDE_STRING_TYPE_ID,
    };
    assert_eq!(expected, err);

    // Aliased imported types
    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "\
        import B from \"./2\"\n\
        import \"./2\" as two\n\
        var b: B\n\
        match b {\n\
          B => {}\n\
          two.B => {}\n\
        }",
        &[
            ("./2", "export type B { b: String }"),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateMatchCase {
        span: Span::new(TEST_MODULE_ID, (6, 5), (6, 5)),
        orig_span: Span::new(TEST_MODULE_ID, (5, 1), (5, 1)),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "\
        import B from \"./2\"\n\
        import \"./2\" as two\n\
        var b: B\n\
        match b {\n\
          two.Z => {}\n\
        }",
        &[
            ("./2", "export type B { b: String }"),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownExport {
        span: Span::new(TEST_MODULE_ID, (5, 5), (5, 5)),
        module_id: ModuleId(4),
        import_name: "Z".to_string(),
        is_aliased: true,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "\
        import B from \"./2\"\n\
        import \"./2\" as two\n\
        var b: B\n\
        match b {\n\
          two.hello => {}\n\
        }",
        &[
            ("./2", "export type B { b: String }\nexport func hello() {}"),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownType {
        span: Span::new(TEST_MODULE_ID, (5, 5), (5, 9)),
        name: "hello".to_string(),
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "\
        import B from \"./2\"\n\
        import \"./2\" as two\n\
        var b: B\n\
        match b {\n\
          b.B => {}\n\
        }",
        &[
            ("./2", "export type B { b: String }"),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownType {
        span: Span::new(TEST_MODULE_ID, (5, 1), (5, 1)),
        name: "b".to_string(),
    };
    assert_eq!(expected, err);

    // Aliased imported enums
    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "\
        import B from \"./2\"\n\
        import \"./2\" as two\n\
        var b: B\n\
        match b {\n\
          B.B1 => {}\n\
          two.B.B1 => {}\n\
        }",
        &[
            ("./2", "export enum B { B1 }"),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::DuplicateMatchCase {
        span: Span::new(TEST_MODULE_ID, (6, 5), (6, 8)),
        orig_span: Span::new(TEST_MODULE_ID, (5, 1), (5, 4)),
    };
    assert_eq!(expected, err);
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck_with_modules(
        "\
        import B from \"./2\"\n\
        import \"./2\" as two\n\
        var b: B\n\
        match b {\n\
          b.B.B1 => {}\n\
        }",
        &[
            ("./2", "export enum B { B1 }"),
        ],
    ).unwrap_err() else { unreachable!() };
    let expected = TypeError::UnknownType {
        span: Span::new(TEST_MODULE_ID, (5, 1), (5, 1)),
        name: "b".to_string(),
    };
    assert_eq!(expected, err);
}

#[test]
fn typecheck_failure_if_expression() {
    let (_, Either::Right(err)) = test_typecheck("val _ = if true { }").unwrap_err() else { unreachable!() };
    let expected = TypeError::EmptyIfElseBlock {
        span: Span::new(TEST_MODULE_ID, (1, 9), (1, 10)),
        kind: "if",
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val _ = if true { [][0] }").unwrap_err() else { unreachable!() };
    let expected = TypeError::EmptyIfElseBlock {
        span: Span::new(TEST_MODULE_ID, (1, 9), (1, 10)),
        kind: "else",
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val _: Bool = if true { 1 }").unwrap_err() else { unreachable!() };
    let expected = TypeError::TypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 25), (1, 25)),
        expected: vec![PRELUDE_BOOL_TYPE_ID],
        received: PRELUDE_INT_TYPE_ID,
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val _: Int = if true { 1 }").unwrap_err() else { unreachable!() };
    let expected = TypeError::EmptyIfElseBlock {
        span: Span::new(TEST_MODULE_ID, (1, 14), (1, 15)),
        kind: "else",
    };
    assert_eq!(expected, err);

    let (_, Either::Right(err)) = test_typecheck("val _ = if true { 1 } else { true }").unwrap_err() else { unreachable!() };
    let expected = TypeError::BranchTypeMismatch {
        span: Span::new(TEST_MODULE_ID, (1, 30), (1, 33)),
        orig_span: Span::new(TEST_MODULE_ID, (1, 19), (1, 19)),
        expected: PRELUDE_INT_TYPE_ID,
        received: PRELUDE_BOOL_TYPE_ID,
    };
    assert_eq!(expected, err);
    let (_, Either::Right(err)) = test_typecheck("val _: Int? = if true { } else { true }").unwrap_err() else { unreachable!() };
    let expected = TypeError::EmptyIfElseBlock {
        span: Span::new(TEST_MODULE_ID, (1, 15), (1, 16)),
        kind: "if",
    };
    assert_eq!(expected, err);
}
