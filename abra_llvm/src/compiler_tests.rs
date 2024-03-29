use inkwell::context::Context;
use itertools::{EitherOrBoth, Itertools};
use abra_core::common::test_utils::MockModuleReader;
use abra_core::module_loader::ModuleLoader;
use abra_core::parser::ast::ModuleId;
use crate::compile_to_llvm_and_run;

#[cfg(test)]
fn test_run_with_modules(input: &str, modules: Vec<(&str, &str)>) -> String {
    let mut mock_reader = MockModuleReader::new(modules);
    let mut mock_loader = ModuleLoader::new(&mut mock_reader);
    let module_id = ModuleId::parse_module_path("./test").unwrap();
    let module = crate::typecheck(module_id.clone(), &input.to_string(), &mut mock_loader)
        .map_err(|e| if let crate::Error::TypecheckerError(e) = e { e.kind } else { unreachable!("Error: {:#?}", e); })
        .unwrap();
    mock_loader.add_typed_module(module);

    let context = Context::create();
    let result = compile_to_llvm_and_run(module_id, &input.to_string(), &mut mock_reader, &context).unwrap();
    result.trim().to_string()
}

struct TestCase {
    setup: &'static str,
    input: &'static str,
    expected: &'static str,
}

impl From<(&'static str, &'static str)> for TestCase {
    fn from((input, expected): (&'static str, &'static str)) -> Self {
        TestCase { setup: "", input, expected }
    }
}

impl From<(&'static str, &'static str, &'static str)> for TestCase {
    fn from((setup, input, expected): (&'static str, &'static str, &'static str)) -> Self {
        TestCase { setup, input, expected }
    }
}

fn run_test_cases<T: Into<TestCase>>(cases: Vec<T>) {
    run_test_cases_with_setup_and_teardown("", cases, "", false);
}

fn run_test_cases_isolated<T: Into<TestCase>>(cases: Vec<T>) {
    run_test_cases_with_setup_and_teardown("", cases, "", true);
}

fn run_test_cases_with_setup<T: Into<TestCase>>(setup: &str, cases: Vec<T>) {
    run_test_cases_with_setup_and_teardown(setup, cases, "", false);
}

fn run_test_cases_with_setup_and_teardown<T: Into<TestCase>>(global_setup: &str, cases: Vec<T>, teardown: &str, isolated: bool) {
    let mut inputs = vec![];
    let mut expecteds = vec![];
    for case in cases {
        let TestCase { setup, input, expected } = case.into();
        inputs.push((setup, input));
        expecteds.push(expected);
    }

    let contents = if isolated {
        inputs.iter().enumerate()
            .map(|(idx, (setup, line))| {
                let setup = setup.lines().map(|l| format!("  {}", l)).join("\n");
                let setup = if setup.is_empty() { "".to_string() } else { format!("{}\n", setup) };
                let teardown = teardown.lines().map(|l| format!("  {}", l)).join("\n");
                let teardown = if teardown.is_empty() { "".to_string() } else { format!("{}\n", teardown) };
                format!("func testCase{}() {{\n{}  println({})\n{}}}\ntestCase{}()\n", idx, setup, line, teardown, idx)
            })
            .join("\n")
    } else {
        inputs.iter().map(|(setup, line)| format!("{}\nprintln({})\n{}", setup, line, teardown)).join("\n")
    };
    let input = format!("{}{}\nprintln()", global_setup, contents);
    // println!("{}", &input);

    let res = test_run_with_modules(&input, vec![]);
    // println!("{}", &res);
    let res_lines = res.lines();

    for (line_num, r) in res_lines.zip_longest(expecteds).enumerate() {
        match r {
            EitherOrBoth::Both(output, expected) => {
                let output = output.trim();
                assert_eq!(expected, output, "Expected '{}' but saw '{}' (test case {})", expected, output, line_num + 1);
            }
            EitherOrBoth::Left(output) => {
                panic!("Unexpected output '{}' (test case {})", output, line_num + 1);
            }
            EitherOrBoth::Right(expected) => {
                panic!("Expected '{}' but saw nothing (test case {})", expected, line_num + 1);
            }
        }
    }
}

fn run_and_verify_output_lines(input: &str, output_lines: Vec<&str>) {
    let res = test_run_with_modules(&input, vec![]);
    let expected = output_lines.join("\n");
    assert_eq!(expected, res, "Expected output '{}' to equal '{}'", expected, res);
}

#[test]
fn test_literals() {
    let cases = vec![
        ("24", "24"),
        ("24.6", "24.600000"),
        ("true", "true"),
        ("false", "false"),
        ("\"hello world\"", "hello world"),
        ("\"hello 👋\"", "hello 👋"),
    ];

    run_test_cases(cases);
}

#[test]
fn test_array_literals() {
    let cases = vec![
        ("[]", "[]"),
        ("[1]", "[1]"),
        ("[7, 10 - 18, 9]", "[7, -8, 9]"),
        ("[1.2, 3.4, -5.0]", "[1.200000, 3.400000, -5.000000]"),
        ("[\"a\", \"b\"]", "[a, b]"),
        ("[true, false]", "[true, false]"),
        ("[[1, 2], [3, 4], [5, 6]]", "[[1, 2], [3, 4], [5, 6]]"),
    ];

    run_test_cases(cases);
}

#[test]
fn test_tuple_literals() {
    let cases = vec![
        ("(1, 2)", "(1, 2)"),
        ("(\"a\", 3, [3, 4, (true, -1)])", "(a, 3, [3, 4, (true, -1)])")
    ];

    run_test_cases(cases);
}

#[test]
fn test_map_literals() {
    let cases = vec![
        ("", "{}", "{}"),
        ("", "{ a: 1 }", "{ a: 1 }"),
        (
            "",
            "{ a: 1, b: 1.2, c: (true, false), d: [{ a1: false }], e: { a2: true } }",
            "{ e: { a2: true }, d: [{ a1: false }], a: 1, c: (true, false), b: 1.200000 }"
        ),
        (
            "val x = 16",
            "{ (1 + 2): 3, (x): x*2, ((1, 2)): 4, \"abc\": 1, ({ a: 1 }): 8, (false): -1 }",
            "{ (1, 2): 4, { a: 1 }: 8, abc: 1, false: -1, 16: 32, 3: 3 }",
        )
    ];

    run_test_cases(cases);
}

#[test]
fn test_set_literals() {
    let cases = vec![
        ("#{}", "#{}"),
        ("#{1, 2, 3}", "#{3, 2, 1}"),
        ("#{1, 2, 3, 2, 1}", "#{3, 2, 1}"),
    ];
    run_test_cases(cases);
}

#[test]
fn test_unary_operations() {
    let cases = vec![
        ("-24", "-24"),
        ("-24.6", "-24.600000"),
        ("!true", "false"),
        ("!!true", "true"),
        ("!false", "true"),
        ("!!false", "false"),
    ];

    run_test_cases(cases);
}

#[test]
fn test_binary_operations_arithmetic() {
    let cases = vec![
        // Integer arithmetic
        ("1 + 1", "2"),
        ("24 + -6", "18"),
        ("24 - 6", "18"),
        ("128 - 64", "64"),
        ("128 * 64", "8192"),
        ("-12 * 6", "-72"),
        ("12 / 6", "2.000000"),
        ("6 / 12", "0.500000"),
        ("9 % 5", "4"),
        ("9 % -5", "4"),
        ("-9 % 5", "-4"),
        ("-9 % -5", "-4"),
        ("2 ** 5", "32.000000"),
        ("2 ** -5", "0.031250"),
        ("-2 ** 5", "-32.000000"),
        ("-2 ** -5", "-0.031250"),
        // Float arithmetic
        ("1 + 1.1", "2.100000"),
        ("1.2 + 1.1", "2.300000"),
        ("1.2 + 1", "2.200000"),
        ("1 - 1.1", "-0.100000"),
        ("1.2 - 1.1", "0.100000"),
        ("1.2 - 1", "0.200000"),
        ("2 * 2.2", "4.400000"),
        ("1.2 * 1.1", "1.320000"),
        ("-6.2 * 2", "-12.400000"),
        ("2 / 2.2", "0.909091"),
        ("4.5 / 1.5", "3.000000"),
        ("-6.2 / 2", "-3.100000"),
        ("9.5 % 5", "4.500000"),
        ("9 % -5.5", "3.500000"),
        ("-9.5 % 5", "-4.500000"),
        ("-9.5 % -5", "-4.500000"),
        ("2 ** 5.1", "34.296751"),
        ("2.1 ** 5.1", "43.986398"),
        ("2.1 ** 5", "40.841010"),
        ("-2 ** 5.1", "-34.296751"),
        ("-2.1 ** 5.1", "-43.986398"),
        ("-2.1 ** 5", "-40.841010"),
        ("2 ** -5.1", "0.029157"),
        ("2.1 ** -5.1", "0.022734"),
        ("2.1 ** -5", "0.024485"),
    ];

    run_test_cases(cases);
}

#[test]
fn test_binary_operations_comparisons() {
    let cases = vec![
        // Integer comparisons
        ("1 < 1", "false"),
        ("1 < 2", "true"),
        ("1 > 1", "false"),
        ("1 > 2", "false"),
        ("1 <= 1", "true"),
        ("1 <= 2", "true"),
        ("1 <= 0", "false"),
        ("1 >= 1", "true"),
        ("1 >= 0", "true"),
        ("1 >= 2", "false"),
        ("1 == 1", "true"),
        ("1 == 2", "false"),
        ("1 != 1", "false"),
        ("1 != 2", "true"),
        // Float comparisons
        ("1.1 < 1", "false"),
        ("1 < 0.9", "false"),
        ("1.1 < 0.9", "false"),
        ("0.1 < 1", "true"),
        ("0.1 < 0.9", "true"),
        ("0.8 < 0.9", "true"),
        ("0.1 > 1", "false"),
        ("0.1 > 0.9", "false"),
        ("0.8 > 0.9", "false"),
        ("1.1 > 1", "true"),
        ("1 > 0.9", "true"),
        ("1.1 > 0.9", "true"),
        ("1.1 <= 1", "false"),
        ("1 <= 0.9", "false"),
        ("1.1 <= 0.9", "false"),
        ("1.0 <= 1", "true"),
        ("0.1 <= 0.9", "true"),
        ("0.8 <= 0.9", "true"),
        ("1.0 >= 1", "true"),
        ("0.1 >= 0.9", "false"),
        ("0.8 >= 0.9", "false"),
        ("1.1 >= 1", "true"),
        ("1 >= 0.9", "true"),
        ("1.1 >= 0.9", "true"),
        ("1.0 == 1", "true"),
        ("1.0 == 1.0", "true"),
        ("1 == 1.0", "true"),
        ("2.0 == 1", "false"),
        ("1.0 == 2.1", "false"),
        ("1 == 2.0", "false"),
        ("1.0 != 1", "false"),
        ("1.0 != 1.0", "false"),
        ("1 != 1.0", "false"),
        ("2.0 != 1", "true"),
        ("1.0 != 2.1", "true"),
        ("1 != 2.0", "true"),
        // Non-numeric equality
        ("\"hello\" == \"world\"", "false"),
        ("\"hello\" != \"world\"", "true"),
        ("\"hello\" == \"hello\"", "true"),
        ("\"hello\" != \"hello\"", "false"),
        ("\"hello\" != 16", "true"),
        ("(\"hello\", 12) == \"hello\"", "false"),
        ("\"hello\" != (\"hello\", 12)", "true"),
        ("(\"hello\", 12) != (\"hello\", 12)", "false"),
        ("(\"hello\", 12.0) == (\"hello\", 12)", "true"),
        ("(\"hello\", 12) == (\"hello\", 12.0)", "true"),
        ("[(\"hello\", \"world\")] == [(\"hello\", \"world\")]", "true"),
        ("[(\"hello\", true)] != (\"hello\", false)", "true"),
    ];

    run_test_cases(cases);
}

#[test]
fn test_binary_operations_booleans() {
    let cases = vec![
        // and/or
        ("true && true", "true"),
        ("false && true", "false"),
        ("true && false", "false"),
        ("false && false", "false"),
        ("true || true", "true"),
        ("false || true", "true"),
        ("true || false", "true"),
        ("false || false", "false"),
        // exclusive-or
        ("true ^ true", "false"),
        ("false ^ true", "true"),
        ("true ^ false", "true"),
        ("false ^ false", "false"),
        // boolean equality
        ("(1 > 2) == (3 > 4)", "true"),
        ("(1 >= 2) != (3 < 4)", "true")
    ];
    run_test_cases(cases);

    let setup = r#"
      var a = 1
      func sideEffect(ret: Bool): Bool {
        a += 1
        ret
      }
    "#;
    let cases = vec![
        (setup, "if true || sideEffect(true) { a } else { a }", "1"),
        (setup, "if false || sideEffect(true) { a } else { a }", "2"),
        (setup, "if true && sideEffect(true) { a } else { a }", "2"),
        (setup, "if false && sideEffect(true) { a } else { a }", "1"),
    ];
    run_test_cases_isolated(cases);
}

#[test]
fn test_binary_operations_string_concat() {
    let cases = vec![
        (r#""hello " + "world""#, "hello world"),
        (r#""a" + 1"#, "a1"),
        (r#""a" + 1.2"#, "a1.200000"),
        (r#""a" + true"#, "atrue"),
        (r#"1 + "b""#, "1b"),
        (r#"1.2 + "b""#, "1.200000b"),
        (r#"false + "b""#, "falseb"),
    ];

    run_test_cases(cases);
}

#[test]
fn test_binary_operations_coalesce() {
    let cases = vec![
        ("[1, 2, 3][0] ?: 17", "1"),
        ("[1, 2, 3][-3] ?: 17", "1"),
        ("[1, 2, 3][10] ?: 17", "17"),
        ("[1, 2, 3][-8] ?: 17", "17"),
    ];

    run_test_cases(cases);
}

#[test]
fn test_functions() {
    let cases = vec![
        ("func a(): Int = 6 + 24", "a()", "30"),
        ("func b(): String = \"hello\"", "b()", "hello"),
        ("func c(): Int[] = [6, 24]", "c()", "[6, 24]"),
        ("func d(): String[] = [\"a\", \"b\"]", "d()", "[a, b]"),
        ("func e(): Int = 4\nfunc f(i: Int): Int = i + i * 5\nfunc g(): Int = f(e())", "g()", "24"),
        ("val s = \"foo\"\nfunc h(a: Int): String { val s = \"hello\"\ns + a }", "h(7)", "hello7"),
        (
            "func fib(i: Int): Int = if i < 2 { i } else { fib(i - 1) + fib(i - 2) }",
            "(fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6), fib(7), fib(8))",
            "(0, 1, 1, 2, 3, 5, 8, 13, 21)"
        )
    ];

    run_test_cases(cases);
}

#[test]
fn test_functions_default_valued_parameters() {
    let global_setup = r#"
      func abc(): Int {
        print("[abc] ")
        6
      }

      func foo(a = abc(), b = "asdf"): String = a.toString() + b
    "#;
    let cases = vec![
        ("", "foo(1, \"a\")", "1a"),
        ("", "foo()", "[abc] 6asdf"),
        ("", "foo(a: 2)", "2asdf"),
        ("", "foo(b: \"qwer\")", "[abc] 6qwer"),
        (
            "func a(x = abc, y = abc): Int = x() + y()",
            "(a, a)[0](a, a)",
            "[abc] [abc] [abc] [abc] 24"
        )
    ];

    run_test_cases_with_setup(global_setup, cases);
}

#[test]
fn test_functions_vararg_parameters() {
    let global_setup = r#"
      func foo(head: Int, *tail: Int[]): Int[][] = [[head], tail]
    "#;
    let cases = vec![
        ("foo(1, 2, 3)", "[[1], [2, 3]]"),
        ("foo(1, 2)", "[[1], [2]]"),
        ("foo(1)", "[[1], []]"),
    ];

    run_test_cases_with_setup(global_setup, cases);
}

#[test]
fn test_method_calls() {
    let cases = vec![
        ("\"ASDF\".toLower() + \"!\"", "asdf!"),
        ("\"aSdF\".toUpper() + \"!\"", "ASDF!"),
        ("(123).toString() + (45.6).toString()", "12345.600000"),
    ];

    run_test_cases(cases);
}

#[test]
fn test_variables() {
    let cases = vec![
        ("val a = 1\nval b = 2", "a + b", "3"),
        ("val c = \"1\"\nval d = 2", "[c, d.toString(), c + d.toString()]", "[1, 2, 12]"),
        ("var e: Int", "e", "None"), // <- I don't love this, but it's the current behavior
    ];

    run_test_cases(cases);
}

#[test]
fn test_assignment() {
    let global_setup = "var a = 2\nval b = 3\nvar c = 2.0\nval d = 3.0\nvar e: Int? = 6\nvar f: Int? = None\nvar g = true\nval h = false";
    let global_teardown = "a = 2\nc = 2.0\ne = 6\nf = None\ng = true";
    let cases = vec![
        ("a = a + b", "5"),
        ("a += b", "5"),
        ("a = a - b", "-1"),
        ("a -= b", "-1"),
        ("a = a * b", "6"),
        ("a *= b", "6"),
        ("a = a % b", "2"),
        ("a %= b", "2"),
        ("c = c / d", "0.666667"),
        ("c /= d", "0.666667"),
        ("[a += b, a -= b, a *= b, a %= b, c /= d]", "[5, 2, 6, 0, 0.666667]"),
        ("e ?:= 7", "6"),
        ("f ?:= 7", "7"),
        ("g = g || h", "true"),
        ("g ||= h", "true"),
        ("g = g && h", "false"),
        ("g &&= h", "false"),
    ];
    run_test_cases_with_setup_and_teardown(global_setup, cases, global_teardown, false);
}

#[test]
fn test_indexing() {
    let cases = vec![
        // Arrays
        ("[1, 2, 3][1]", "2"),
        ("[1, 2, 3][-1]", "3"),
        ("[][0]", "None"),
        ("[1, 2][5]", "None"),
        ("[1, 2][-3]", "None"),
        // Tuples
        ("(\"a\", 3)[0]", "a"),
        ("(\"a\", 3)[1]", "3"),
        // Strings
        ("\"asdf\"[0]", "a"),
        ("\"asdf\"[-2]", "d"),
        ("\"asdf\"[-5]", "None"),
        ("\"asdf\"[14]", "None"),
        // Maps
        ("{ a: 1 }[\"a\"]", "1"),
        ("{ a: 1, (0): 18 }[1 - 1]", "18"),
        ("{ a: 1, (0): 18 }[1]", "None"),
    ];

    run_test_cases(cases);
}

#[test]
fn test_range_indexing() {
    let global_setup = "val arr = [1, 2, 3]\nval str = \"some string\"";
    let cases = vec![
        // Arrays
        ("arr[1:2]", "[2]"),
        ("arr[-2:-1]", "[2]"),
        ("arr[:1]", "[1]"),
        ("arr[1:]", "[2, 3]"),
        ("arr[-3:]", "[1, 2, 3]"),
        ("arr[-3:]", "[1, 2, 3]"),
        // Starting and ending ranges outside of bounds
        ("arr[3:]", "[]"),
        ("arr[:4]", "[1, 2, 3]"),
        // Strings
        ("str[1:2]", "o"),
        ("str[-2:-1]", "n"),
        ("str[:4]", "some"),
        ("str[5:]", "string"),
        ("str[-6:]", "string"),
    ];

    run_test_cases_with_setup(global_setup, cases);
}

#[test]
fn test_index_assignment() {
    let array_setup = "val arr = [1, 2, 3]";
    let map_setup = "val map = { a: 1, b: 2 }";
    let tuple_setup = "val tuple = (1, 2)";
    let cases = vec![
        // Array index assignment
        (array_setup, "(arr[0] = 4, arr)[1]", "[4, 2, 3]"),
        (array_setup, "(arr[-1] = 4, arr)[1]", "[1, 2, 4]"),
        (array_setup, "(arr[3] = 4, arr)[1]", "[1, 2, 3, 4]"),
        (array_setup, "(arr[6] = 4, arr)[1]", "[1, 2, 3, None, None, None, 4]"),
        // Map index assignment
        (map_setup, "(map[\"b\"] = 6, map)[1]", "{ a: 1, b: 6 }"),
        (map_setup, "(map[\"a\" + \"b\"] = 6, map)[1]", "{ a: 1, ab: 6, b: 2 }"),
        // Tuple index assignment
        (tuple_setup, "(tuple[0] = 0, tuple)[1]", "(0, 2)"),
        (tuple_setup, "(tuple[1] = 0, tuple)[1]", "(1, 0)"),
    ];
    run_test_cases_isolated(cases);
}

#[test]
fn test_field_assignment() {
    let setup = r#"
      type Person {
        name: String

        func intro(self): String = "I'm " + self.name
      }

      val me = Person(name: "Kenny")
    "#;
    let cases = vec![
        ("(me, me.intro())", "(Person(name: Kenny), I'm Kenny)"),
        ("me.name = \"Ken\"", "Ken"),
        ("(me, me.intro())", "(Person(name: Ken), I'm Ken)"),
    ];
    run_test_cases_with_setup(setup, cases);
}

#[test]
fn test_destructuring_assignment() {
    let cases = vec![
        ("val (a, b, c) = (1, 2, 3)", "a + b + c", "6"),
        ("val ((a, b), c) = ((1, 2), (3, 4))", "a + b + c[0] + c[1]", "10"),
        ("val [(x1, y1), (x2, y2), (x3, y3)] = [(1, 2), (3, 4)]", "[x1, y1, x2, y2, x3, y3]", "[1, 2, 3, 4, None, None]"),
        ("val [(x1, y1), *mid, (x2, y2)] = [(1, 2), (3, 4), (5, 6), (7, 8)]", "(x1, y1, mid, x2, y2)", "(1, 2, [(3, 4), (5, 6)], 7, 8)"),
        ("val [h1, h2, *mid, t1, t2, t3, t4, t5] = [1, 2, 3, 4, 5, 6]", "(h1, h2, mid, t1, t2, t3, t4, t5)", "(1, 2, [], 3, 4, 5, 6, None)"),
        ("val [head, *tail] = [1, 2, 3]", "tail", "[2, 3]"),
        ("val [[a], [d], [g, *h]] = [\"abc\", \"def\", \"ghi\"]", "(a, d, g, h)", "(a, d, g, hi)"),
        (
            r#"
              func wrapper(): Int {
                val ((a, b), c) = ((1, 2), (3, 4))
                a + b + c[0] + c[1]
              }
            "#,
            "wrapper()",
            "10"
        )
    ];
    run_test_cases_isolated(cases);
}

#[test]
fn test_if_statements_and_expressions() {
    let cases = vec![
        ("var a = 0\nif a >= 0 { a += 1 } else { a += 2 }", "a", "1"),
        ("var a = 0\nif a == 0 { a = 1 }", "a", "1"),
        ("val a = if true { 0 } else { 1 }", "a", "0"),
        ("val a = if true { 0 }", "a", "0"),
        ("val a = if false { 0 }", "a", "None"),
        ("val a = if false { 0 }", "a", "None"),
        // Conditional binding
        ("val t: (Int, Int)? = None", "if t |(a, b)| { a + b } else { 10 }", "10"),
        ("val t: (Int, Int)? = (1, 2)", "if t |(a, b)| { a + b } else { 10 }", "3"),
    ];
    run_test_cases_isolated(cases);
}

#[test]
fn test_to_string_representation() {
    let cases = vec![
        ("", "1", "1"),
        ("", "-1", "-1"),
        ("", "0.123", "0.123000"),
        ("", "true", "true"),
        ("", "false", "false"),
        ("", "None", "None"),
        ("", "\"asdf\"", "asdf"),
        ("", "(1, (2, 3))", "(1, (2, 3))"),
        ("", "[[\"a\"], [\"b\", \"c\"]]", "[[a], [b, c]]"),
        ("func foo() {}", "foo", "<func foo>"),
    ];
    run_test_cases(cases);
}

#[test]
fn test_higher_order_functions() {
    let cases = vec![
        (
            r#"
              func container(): (String) => String {
                func greet(s: String): String {
                  print("hello", s)
                  "!"
                }
                greet
              }
              func foo(fn: (String) => String): String = fn("world")
            "#,
            "foo(container())",
            "hello world!"
        )
    ];
    run_test_cases_isolated(cases);
}

#[test]
fn test_closures() {
    let cases = vec![
        (
            r#"
              var z = 0
              func counter(): (() => Unit, () => Unit) {
                var a = 0
                print(z, "")

                func inc() {
                  a += 1
                  print(a, "")
                }
                func dec() {
                  a -= 1
                  print(a, "")
                }
                (inc, dec)
              }

              val (inc, dec) = counter()
              inc()
              inc()
              dec()
              dec()
            "#,
            "",
            "0 1 2 1 0"
        ),
        (
            r#"
              func container(cond: Bool): () => Unit {
                var a = 0

                func inc() {
                  a += 1
                  print(a, "")
                }
                if cond {
                  a = 7
                  print(a, "")
                }
                inc
              }

              val fn1 = container(cond: false)
              fn1()
              print("|", "")
              val fn2 = container(cond: true)
              fn2()
            "#,
            "",
            "1 | 7 8"
        ),
        // Test closing over global variables
        (
            r#"
              val a = 1
              var b = 2
              func getSum(): Int = a + b
              b = 17
            "#,
            "getSum()",
            "18"
        ),
        // Test deeply nested upvalue access
        (
            r#"
              func getCounter(): () => Int {
                var count = 100
                func unnecessaryLayer1(): () => () => Int {
                  func unnecessaryLayer2(): () => Int {
                    func tick(): Int { count += 1 }
                    tick
                  }
                  unnecessaryLayer2
                }
                count = 0
                unnecessaryLayer1()()
              }
              val tick = getCounter()
            "#,
            "[tick(), tick(), tick(), tick(), tick()]",
            "[1, 2, 3, 4, 5]"
        )
    ];
    run_test_cases(cases);
}

#[test]
fn test_types_base_functionality() {
    let setup = r#"
      type Person {
        name: String
        age: Int = 30
      }
    "#;
    let cases = vec![
        ("Person(name: \"Human\", age: 30)", "Person(name: Human, age: 30)"),
        ("Person(name: \"Human\", age: 30).toString()", "Person(name: Human, age: 30)"),
        ("Person(name: \"Human\")", "Person(name: Human, age: 30)"),
        ("Person(name: \"Human\") == Person(name: \"Other Human\")", "false"),
        ("Person(name: \"Human\") == \"Human\"", "false"),
        ("Person(name: \"Human\", age: 30) == Person(name: \"Human\")", "true"),
        (
            "#{Person(name: \"Human\", age: 30), Person(name: \"Human\"), Person(name: \"Other Human\", age: 31)}",
            "#{Person(name: Other Human, age: 31), Person(name: Human, age: 30)}"
        ),
    ];
    run_test_cases_with_setup(setup, cases);
}

#[test]
fn test_types_methods() {
    let setup = r#"
      val message = "Hello!"

      type Person {
        name: String
        age: Int = 30

        func toString(self): String {
          print(message)
          "<from explicit toString implementation>"
        }
        func printSelf(self) = print(self)
        func returnSix(self): Int = 6
        func sayHello(self) = print("Hello!")
        func sayHelloClosure(self) = print(message)
        func sayHelloIndirect(self) = self.sayHello()
      }

      val p = Person(name: "Human", age: 30)
    "#;

    let cases = vec![
        ("", "p.toString()", "Hello!<from explicit toString implementation>"),
        ("p.printSelf()", "", "Hello!<from explicit toString implementation>"),
        ("", "p.returnSix()", "6"),
        ("p.sayHello()", "", "Hello!"),
        ("p.sayHelloClosure()", "", "Hello!"),
        ("p.sayHelloIndirect()", "", "Hello!"),
    ];
    run_test_cases_with_setup(setup, cases);
}

#[test]
fn test_types_fields() {
    let setup = r#"
      val nameSuffix = "!"

      type Person {
        name: String
        age: Int = 30
        nameFormatter: (String) => String

        func formatName(self): String = self.nameFormatter(self.name)
      }
      func toUpper(s: String): String = s.toUpper() + nameSuffix
      val p = Person(name: "Human", age: 30, nameFormatter: toUpper)
    "#;
    let cases = vec![
        // Builtin types' fields
        ("\"\".length", "0"),
        ("\"asdf\".length", "4"),
        ("[].length", "0"),
        ("[1, 2, 3].length", "3"),
        ("#{}.size", "0"),
        ("#{1, 2, 3, 2, 1}.size", "3"),
        ("{}.size", "0"),
        ("{ a: 1, b: 3 }.size", "2"),
        // User-defined types' fields
        ("p.name", "Human"),
        ("p.name.length + p.age", "35"),
        ("p.nameFormatter", "<func toUpper>"),
        ("p.formatName()", "HUMAN!"),
    ];
    run_test_cases_with_setup(setup, cases);
}

#[test]
fn test_types_static_methods() {
    let setup = r#"
      val message = "Hello World"

      type A {
        func abc(): String = "Hello!"
        func def(): String = message + "!"
      }
    "#;
    let cases = vec![
        ("A.abc", "<func abc>"),
        ("A.abc()", "Hello!"),
        ("A.def", "<func def>"),
        ("A.def()", "Hello World!"),
        // Test builtin types
        ("Map.fromPairs", "<func fromPairs>"),
        ("Map.fromPairs([])", "{}"),
    ];
    run_test_cases_with_setup(setup, cases);
}

#[test]
fn test_enums_base_functionality() {
    let setup = r#"
      val defaultGray = 0.5

      enum Color {
        Red
        Green
        Blue
        Gray(percent: Float = defaultGray)
        RGB(r: Int, g: Int, b: Int)
      }
    "#;

    let cases = vec![
        // toString
        ("Color.Red", "Color.Red"),
        ("Color.Green", "Color.Green"),
        ("Color.Blue", "Color.Blue"),
        ("Color.Gray()", "Color.Gray(percent: 0.500000)"),
        ("Color.Gray(percent: 0.12)", "Color.Gray(percent: 0.120000)"),
        ("Color.RGB(r: 1, g: 2, b: 3)", "Color.RGB(r: 1, g: 2, b: 3)"),
        // eq
        ("Color.Red == Color.Red", "true"),
        ("Color.Red == Color.Blue", "false"),
        ("Color.Red == Color.Green", "false"),
        ("Color.Blue == Color.Red", "false"),
        ("Color.Blue == Color.Blue", "true"),
        ("Color.Blue == Color.Green", "false"),
        ("Color.Green == Color.Red", "false"),
        ("Color.Green == Color.Blue", "false"),
        ("Color.Green == Color.Green", "true"),
        ("Color.Gray() == Color.Gray(percent: 0.1)", "false"),
        ("Color.Gray(percent: 0.25) == Color.Gray(percent: 0.25)", "true"),
        ("Color.RGB(r: 1, g: 2, b: 3) == Color.RGB(r: 1, g: 2, b: 3)", "true"),
        ("Color.RGB(r: 1, g: 2, b: 3) == Color.RGB(r: 1, g: 2, b: 2)", "false"),
        // hash (simple)
        (
            "#{Color.Red, Color.Green, Color.Blue, Color.Blue, Color.Green, Color.Red}",
            "#{Color.Green, Color.Blue, Color.Red}"
        ),
        // hash (complex)
        (
            "#{Color.Gray(), Color.Gray(percent: 0.25), Color.Gray(percent: 0.5)}",
            "#{Color.Gray(percent: 0.500000), Color.Gray(percent: 0.250000)}"
        ),
        (
            "#{Color.RGB(r: 1, g: 2, b: 3), Color.RGB(r: 11, g: 12, b: 13), Color.RGB(r: 1, g: 2, b: 3), Color.RGB(r: 2, g: 1, b: 3)}",
            "#{Color.RGB(r: 1, g: 2, b: 3), Color.RGB(r: 2, g: 1, b: 3), Color.RGB(r: 11, g: 12, b: 13)}"
        ),
    ];
    run_test_cases_with_setup(setup, cases);
}

#[test]
fn test_enums_variant_constructors() {
    let setup = r#"
      enum Color {
        Red
        Blue
        Green
        RGB(r: Int, g: Int = 0, b: Int = 0)
      }

      func foo(fn: (Int) => Color): Color = fn(16)
    "#;
    let cases = vec![
        // toString
        ("", "Color.RGB", "<func Color.RGB>"),
        // Passing as function values
        ("", "foo(Color.RGB)", "Color.RGB(r: 16, g: 0, b: 0)"),
        // Calling as function
        (
            "val fn = Color.RGB",
            "(fn(1), fn(1, 2), fn(1, 2, 3))",
            "(Color.RGB(r: 1, g: 0, b: 0), Color.RGB(r: 1, g: 2, b: 0), Color.RGB(r: 1, g: 2, b: 3))"
        ),
    ];
    run_test_cases_with_setup(setup, cases);
}

#[test]
fn test_enums_methods() {
    let setup = r#"
      enum Color {
        Red
        Green
        Blue

        func toString(self): String = "Color(...)"

        func toHex(self): String {
          if self == Color.Red {
            "0xFF0000"
          } else if self == Color.Green {
            "0x00FF00"
          } else if self == Color.Blue {
            "0x0000FF"
          } else {
            "unreachable"
          }
        }
      }
    "#;

    let cases = vec![
        ("Color.Blue", "Color(...)"),
        ("Color.Blue.toString()", "Color(...)"),
        ("Color.Red.toHex()", "0xFF0000"),
        ("Color.Green.toHex()", "0x00FF00"),
        ("Color.Blue.toHex()", "0x0000FF"),
    ];
    run_test_cases_with_setup(setup, cases);
}

#[test]
fn test_opt_safe_accessor() {
    let setup = r#"
      type Person {
        name: String
        func intro(self): String = "Hi, I'm " + self.name
      }
      val arr = [Person(name: "Ken"), Person(name: "Meg")]
    "#;
    let cases = vec![
        ("arr[0]?.name", "Ken"),
        ("arr[1]?.name", "Meg"),
        ("arr[2]?.name", "None"),
        ("arr[0]?.intro", "<func intro>"),
        ("arr[1]?.intro", "<func intro>"),
        ("arr[2]?.intro", "None"),
        ("arr[0]?.intro()", "Hi, I'm Ken"),
        ("arr[1]?.intro()", "Hi, I'm Meg"),
        ("arr[2]?.intro()", "None"),
    ];
    run_test_cases_with_setup(setup, cases);
}

#[test]
fn test_types_bound_methods() {
    let setup = r#"
      val hello = "Hello"

      type Person {
        name: String

        func greet(self, suffix = "!"): String = hello + " from " + self.name + suffix
      }

      val i = -1
      val f = 1.23
      val str = "asdf"
      val a = [1, 2, 3]
      val set = #{1, 2, 3}
      val m = { a: 1 }

      val me = Person(name: "Ken")
      val greet = me.greet
    "#;

    let cases = vec![
        // Built-in types
        ("val intAbs = i.abs", "(i.abs(), intAbs())", "(1, 1)"),
        ("val intToString = i.toString", "(i.toString(), intToString())", "(-1, -1)"),
        ("val floatFloor = f.floor", "(f.floor(), floatFloor())", "(1, 1)"),
        ("val floatToString = f.toString", "(f.toString(), floatToString())", "(1.230000, 1.230000)"),
        ("val strToUpper = str.toUpper", "(str.toUpper(), strToUpper())", "(ASDF, ASDF)"),
        ("val strToString = str.toString", "(str.toString(), strToString())", "(asdf, asdf)"),
        ("val arrIsEmpty = a.isEmpty", "(a.isEmpty(), arrIsEmpty())", "(false, false)"),
        ("val arrToString = a.toString", "(a.toString(), arrToString())", "([1, 2, 3], [1, 2, 3])"),
        ("val setIsEmpty = set.isEmpty", "(set.isEmpty(), setIsEmpty())", "(false, false)"),
        ("val setToString = set.toString", "(set.toString(), setToString())", "(#{3, 2, 1}, #{3, 2, 1})"),
        ("val mapIsEmpty = m.isEmpty", "(m.isEmpty(), mapIsEmpty())", "(false, false)"),
        ("val mapToString = m.toString", "(m.toString(), mapToString())", "({ a: 1 }, { a: 1 })"),
        // User-defined types
        ("", "me.greet", "<func greet>"),
        ("", "greet", "<func greet>"),
        ("", "greet()", "Hello from Ken!"),
        ("", "greet(\" 🙂\")", "Hello from Ken 🙂"),
        ("", "me.greet()", "Hello from Ken!"),
        ("", "me.greet(\" 🙂\")", "Hello from Ken 🙂"),
    ];
    run_test_cases_with_setup(setup, cases);
}

#[test]
fn test_calling_bound_closures() {
    let setup = r#"
      var a = 1

      type X {
        x: Int = 6

        func foo0(self): Int {
          a += 1
          self.x
        }
        func foo1(self, a1: Int): Int {
          a += 1
          self.x + a1
        }
        func foo1_(self, a1: Int = 18): Int {
          a += 1
          self.x + a1
        }
        func foo2(self, a1: Int, a2: Int): Int {
          a += 1
          self.x + a1 + a2
        }
        func foo2_(self, a1: Int = 18, a2: Int = 19): Int {
          a += 1
          self.x + a1 + a2
        }
      }

      func foo0(): Int {
        a += 1
        6
      }
      func foo1(a1: Int): Int {
        a += 1
        6 + a1
      }
      func foo1_(a1: Int = 18): Int {
        a += 1
        6 + a1
      }
      func foo2(a1: Int, a2: Int): Int {
        a += 1
        6 + a1 + a2
      }
      func foo2_(a1: Int = 18, a2: Int = 19): Int {
        a += 1
        6 + a1 + a2
      }

      val x = X()
      val xFoo0 = x.foo0
      val xFoo1 = x.foo1
      val xFoo1_ = x.foo1_
      val xFoo2 = x.foo2
      val xFoo2_ = x.foo2_
    "#;
    let cases = vec![
        // Testing calling standalone closure functions
        ("foo0()", "6"),
        ("foo1(1)", "7"),
        ("foo1_()", "24"),
        ("foo1_(1)", "7"),
        ("foo2(1, 2)", "9"),
        ("foo2_()", "43"),
        ("foo2_(1)", "26"),
        ("foo2_(1, 2)", "9"),
        // Testing calling bound closure methods
        ("xFoo0()", "6"),
        ("xFoo1(1)", "7"),
        ("xFoo1_()", "24"),
        ("xFoo1_(1)", "7"),
        ("xFoo2(1, 2)", "9"),
        ("xFoo2_()", "43"),
        ("xFoo2_(1)", "26"),
        ("xFoo2_(1, 2)", "9"),
    ];
    run_test_cases_with_setup(setup, cases);
}

#[test]
fn test_lambdas() {
    let setup = r#"
      val fn0 = (i: Int) => i + 1

      val fn1 = (i: Int, x: Int) => i + x

      val fn2 = (i: Int, x = 2) => i + x

      val a = 16
      val fn3 = (i: Int, x = 4) => i + x + a
    "#;
    let cases = vec![
        ("fn0(1)", "2"),
        ("fn1(1, 3)", "4"),
        ("fn2(1)", "3"),
        ("fn2(1, 3)", "4"),
        ("fn3(1)", "21"),
        ("fn3(1, 3)", "20"),
    ];
    run_test_cases_with_setup(setup, cases);

    let input = r#"
      func makeCounter(start = 0): ((Int) => Int, (Int) => Int) {
        var counter = start

        val incr = (by: Int) => counter += by
        val decr = (by: Int) => counter -= by

        (incr, decr)
      }

      val (incr, decr) = makeCounter()
      println(incr(1), incr(1), decr(2), incr(1), incr(2), decr(1), decr(2))
    "#;
    run_and_verify_output_lines(input, vec!["1 2 0 1 3 2 0"]);

    let input = r#"
      type Counter {
        count: Int = 0
        incr: (Int) => Int

        func up(self): Int {
          self.count = self.incr(self.count)
        }
      }

      var incrAmount = 1
      val c = Counter(incr: x => x + incrAmount)
      println(c.up(), c.up(), c.up())
      incrAmount = 5
      println(c.up(), c.up(), c.up())
    "#;
    run_and_verify_output_lines(input, vec!["1 2 3", "8 13 18"]);

    let input = r#"
      func makeIterator<T>(arr: T[]): () => (T, Int)? {
        var idx = 0

        () => if arr[idx] |item| {
          idx += 1
          (item, idx - 1)
        } else {
          None
        }
      }

      val arr = ["a", "b", "c"]
      val iter = makeIterator(arr)
      println(iter(), iter(), iter())
      arr[3] = "d"
      arr[4] = "e"
      println(iter(), iter(), iter(), iter(), iter())
    "#;
    run_and_verify_output_lines(input, vec![
        "(a, 0) (b, 1) (c, 2)",
        "(d, 3) (e, 4) None None None",
    ]);

    // Lambdas closing over block-scoped variables
    let input = r#"
      var f: () => Int

      if true {
        val x = 6
        f = () => x
      } else {
        val y = 12
        f = () => y
      }
      println(f())

      for i in [24, 25, 26, 27, 28, 29] {
        f = () => i
        break
      }
      println(f())

      while true {
        val x = 29
        f = () => x
        break
      }
      println(f())
    "#;
    run_and_verify_output_lines(input, vec!["6", "24", "29"]);
}

#[test]
fn test_for_loops() {
    let input = r#"
      val arr = [2, 4, 6, 8, 10]
      for i in arr print(i, "")
      println()
      for i, idx in arr print(idx, "=>", i + ", ")
      println()

      val set = #{"a", "b", "a", "c", "a", "d"}
      for i in set print(i, "")
      println()
      for i, idx in set print(idx, "=>", i + ", ")
      println()

      val map = { a: 1, b: 2, c: 3, d: 4 }
      for i in map print(i, "")
      println()
      for k, v in map print(k, "=>", v + ", ")
      println()

      var count1 = 0
      for item, i in [1, 2, 3] {
        val inc = if item == 2 { break } else 1
        count1 += inc
      }
      println(count1)

      var a = 0
      for _ in [1, 2, 3, 4, 5] {
        a = a + 1
        for _ in [1, 2, 3, 4, 5] {
          a = a + 1
          if a >= 3 break
        }
        if a > 3 break
      }
      println(a) // If this printed 3, we'd know that `break` destroyed the outer loop too, but it doesn't

      var count2 = 0
      for item, i in [1, 2, 3] {
        val inc = if item == 2 { continue } else 1
        count2 += inc
      }
      println(count2)
    "#;
    run_and_verify_output_lines(input, vec![
        "2 4 6 8 10 ",
        "0 => 2, 1 => 4, 2 => 6, 3 => 8, 4 => 10, ",
        "c b d a ",
        "0 => c, 1 => b, 2 => d, 3 => a, ",
        "a d c b ",
        "a => 1, d => 4, c => 3, b => 2, ",
        "1",
        "5",
        "2",
    ]);
}

#[test]
fn test_while_loops() {
    let input = r#"
      var i = 0
      while i < 3 {
        print(i, "")
        i += 1
      }
      println()

      var a = 0
      while true {
        a = a + 1
        while true {
          a = a + 1
          if a >= 3 break
        }
        if a > 3 break
      }
      println(a) // If this printed 3, we'd know that `break` destroyed the outer loop too, but it doesn't

      val arr = [1, 2, 3]
      var idx = 0
      var count = 0
      while arr[idx] |item| {
        idx += 1
        val inc = if item == 2 { continue } else 1
        count += inc
      }
      println(count)

      // Condition binding
      (() => {
        var str = ""
        var idx = 0
        val arr = [1, 2, 3]
        while arr[idx] |item| {
          str = str + item
          idx = idx + 1
        }
        println(str)
      })()

      // Fizzbuzz-ish
      (() => {
        var i = 1
        var output = ""
        while i <= 20 {
          val msg = if i % 15 == 0 {
            "Fb"
          } else if i % 3 == 0 {
            "F"
          } else if i % 5 == 0 {
            "B"
          } else {
            "" + i
          }
          output = output + msg + ","
          i = i + 1
        }
        println(output)
      })()
    "#;
    run_and_verify_output_lines(input, vec![
        "0 1 2 ",
        "5",
        "2",
        "123",
        "1,2,F,4,B,F,7,8,F,B,11,F,13,14,Fb,16,17,F,19,B,",
    ]);
}

#[test]
fn test_return_statements() {
    let input = r#"
      func contains(arr: Int[], item: Int): Bool {
        for i in arr {
          if item == i {
            return true
          }
        }
        false
      }
      val arr = [1, 2, 3, 4]
      println(contains(arr, 4))

      val contains_ = (arr: Int[], item: Int) => {
        for i in arr {
          if item == i {
            return true
          }
        }
        false
      }
      println(contains_(arr, 4))

      func f1(): Int {
        if true {
          val x = 12
          return 24
        }
        return 6
      }
      println(f1())
      val f1_ = () => {
        if true {
          val x = 12
          return 24
        }
        // todo: `return 6` should work here but it doesn't
        6
      }
      println(f1_())

      func f2(): Int {
        while true {
          val x = 12
          return 24
        }
      }
      println(f2())

      func f3(): Int {
        for _ in [1, 2, 3] {
          val x = 12
          return 24
        }
      }
      println(f3())

      func f4(): Int {
        if true {
          return 24
        } else {
          return 6
        }
      }
      println(f4())

      func f5(i = 5): Int {
        val s = match i {
          4 => "asdf"
          _ => return 24
        }
        s.length
      }
      println(f5())
    "#;
    run_and_verify_output_lines(input, vec![
        "true",
        "true",
        "24",
        "24",
        "24",
        "24",
        "24",
        "24",
    ]);
}

#[test]
fn test_imports() {
    let input = r#"
      import Person from "./person"
      import Direction, radsToDegs from "./direction"
      import names, addName from "./names"

      val p = Person(name: "Ken")
      println(p.name)

      val up = Direction.Up
      val angle = Direction.Angled(degs: radsToDegs(0.25 * 3.1415))
      println(up, angle)

      print(names, "")
      addName("Meg")
      addName("Ken")
      println(names)
    "#;
    let modules = vec![
        ("./person", "export type Person { name: String }"),
        (
            "./direction",
            r#"
              export enum Direction {
                Up
                Angled(degs: Float)
              }

              export func radsToDegs(rads: Float): Float = rads * 180 / 3.1415
            "#
        ),
        (
            "./names",
            r#"
              export val names: String[] = []
              var idx = 0
              export func addName(name: String) {
                names[idx] = name
                idx += 1
              }
              export func getNames(): String[] = names
            "#
        ),
    ];
    let output = test_run_with_modules(input, modules);
    assert_eq!("Ken\nDirection.Up Direction.Angled(degs: 45.000000)\n[] [Meg, Ken]", output);

    let input = r#"
      import addName, getNames from "./names"
      val names: String[] = []

      addName("Ken")
      addName("Meg")

      println(getNames(), names)
    "#;
    let output = test_run_with_modules(input, vec![
        (
            "./names",
            r#"
              export val names: String[] = []
              var idx = 0
              export func addName(name: String) {
                names[idx] = name
                idx += 1
              }
              export func getNames(): String[] = names
            "#
        ),
    ]);
    assert_eq!("[Ken, Meg] []", output);
}

#[test]
fn test_match_statements_and_expressions() {
    let setup = r#"
      type Person { name: String }
      enum Color { Red, RGB(r: Int, g: Int, b: Int) }

      func test(value: Int | String | Float? | (Bool, Int) | Person | Color): (String, String) {
        match value {
          None v => ("case 0", v + "")
          1 v => ("case 1", v + "")
          2 v => ("case 2", v + "")
          "three" v => ("case 3", v + "")
          "four" v => ("case 4", v + "")
          (true, 1) v => ("case 5", v + "")
          (false, 2) v => ("case 6", v + "")
          Person v => ("case 7", v + "")
          Color v => ("case 8", v + "")
          _ v => ("wildcard", v + "")
        }
      }

      var flag = false
      func testWithReturn(value: Int?): String {
        flag = false
        val s = match value {
          None => return "early return"
          _ i => "number: " + i
        }
        flag = true

        s
      }
    "#;
    let cases = vec![
        ("test(None)", "(case 0, None)"),
        ("test(0)", "(wildcard, 0)"),
        ("test(1)", "(case 1, 1)"),
        ("test(2)", "(case 2, 2)"),
        ("test(\"three\")", "(case 3, three)"),
        ("test(\"four\")", "(case 4, four)"),
        ("test(\"five\")", "(wildcard, five)"),
        ("test((true, 1))", "(case 5, (true, 1))"),
        ("test((false, 2))", "(case 6, (false, 2))"),
        ("test(Person(name: \"Meg\"))", "(case 7, Person(name: Meg))"),
        ("test(Color.Red)", "(case 8, Color.Red)"),
        ("test(Color.RGB(r: 1, g: 2, b: 3))", "(case 8, Color.RGB(r: 1, g: 2, b: 3))"),
        ("test((true, 2))", "(wildcard, (true, 2))"),
        //
        ("(testWithReturn(None), flag)", "(early return, false)"),
        ("(testWithReturn(123), flag)", "(number: 123, true)"),
    ];
    run_test_cases_with_setup(setup, cases);

    let input = r#"
      enum Foo {
        Bar(baz: String, qux: Int)
      }
      func abc(foo: Foo): Int {
        match foo {
          Foo.Bar("asdf", 12) => 1
          Foo.Bar("asdf", q) => 2
          Foo.Bar(b, 12) => 3
          Foo.Bar(b, q) => b.length + q
        }
      }
      println(
        abc(Foo.Bar("asdf", 24)), // => 2
        abc(Foo.Bar("zxcv", 12)), // => 3
        abc(Foo.Bar("asdf", 12)), // => 1
        abc(Foo.Bar("zxcv", 0)),  // => 4
      )

      enum Point {
        TwoD(coord: (Int, Int)),
        ThreeD(coord: (Int, Int, Int))
      }
      val p = Point.ThreeD(coord: (1, 2, 3))
      val s = match p {
        Point.TwoD((x, y)) => x + y
        Point.ThreeD((x, y, z)) => x + y + z
      }
      println(s)
    "#;
    run_and_verify_output_lines(input, vec![
        "2 3 1 4",
        "6"
    ]);
}
