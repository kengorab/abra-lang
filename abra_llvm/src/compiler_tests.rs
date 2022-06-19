use inkwell::context::Context;
use itertools::Itertools;
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
        .map_err(|e| if let crate::Error::TypecheckerError(e) = e { e.kind } else { unreachable!() })
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
    let input = format!("{}{}", global_setup, contents);
    // println!("{}", &input);

    let res = test_run_with_modules(&input, vec![]);

    for (line_num, (output, expected)) in res.lines().zip(expecteds).enumerate() {
        assert_eq!(expected, output, "Expected '{}' but saw '{}' (test case {})", expected, output, line_num + 1);
    }
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
    ];

    run_test_cases(cases);
}

#[test]
fn test_binary_operations_booleans() {
    let cases = vec![
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
        ("foo(1, \"a\")", "1a"),
        ("foo()", "[abc] 6asdf"),
        ("foo(a: 2)", "2asdf"),
        ("foo(b: \"qwer\")", "[abc] 6qwer"),
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
    let global_setup = "var a = 2\nval b = 3\nvar c = 2.0\nval d = 3.0\nvar e: Int? = 6\nvar f: Int? = None";
    let global_teardown = "a = 2\nc = 2.0\ne = 6\nf = None";
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
        ("f ?:= 7", "7")
    ];
    run_test_cases_with_setup_and_teardown(global_setup, cases, global_teardown, false);

    // let global_setup = "var a = true\nval b = false";
    // let global_teardown = "a = true";
    // let cases = vec![
    //     ("a = a || b", "true"),
    //     ("a ||= b", "true")
    // ];
    // run_test_cases_with_setup_and_teardown(global_setup, cases, global_teardown);
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
