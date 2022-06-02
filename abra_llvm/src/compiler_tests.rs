use inkwell::context::Context;
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

#[test]
fn test_literals() {
    let res = test_run_with_modules("24", vec![]);
    assert_eq!(res, "24");

    let res = test_run_with_modules("24.6", vec![]);
    assert_eq!(res, "24.600000");

    let res = test_run_with_modules("true", vec![]);
    assert_eq!(res, "true");

    let res = test_run_with_modules("false", vec![]);
    assert_eq!(res, "false");

    let res = test_run_with_modules("\"hello world\"", vec![]);
    assert_eq!(res, "hello world");

    let res = test_run_with_modules("\"hello 👋\"", vec![]);
    assert_eq!(res, "hello 👋");
}

#[test]
fn test_array_literals() {
    let cases = [
        ("[]", "[]"),
        ("[1]", "[1]"),
        ("[7, 8, 9]", "[7, 8, 9]"),
        ("[1.2, 3.4, -5.0]", "[1.200000, 3.400000, -5.000000]"),
        ("[\"a\", \"b\"]", "[a, b]"),
        ("[true, false]", "[true, false]"),
        ("[[1, 2], [3, 4], [5, 6]]", "[[1, 2], [3, 4], [5, 6]]"),
    ];

    for (input, expected) in cases {
        let res = test_run_with_modules(input, vec![]);
        assert_eq!(res, expected, "expected '{}' to output '{}'", input, expected);
    }
}

#[test]
fn test_unary_operations() {
    let res = test_run_with_modules("-24", vec![]);
    assert_eq!(res, "-24");

    let res = test_run_with_modules("-24.6", vec![]);
    assert_eq!(res, "-24.600000");

    let res = test_run_with_modules("!true", vec![]);
    assert_eq!(res, "false");
    let res = test_run_with_modules("!!true", vec![]);
    assert_eq!(res, "true");

    let res = test_run_with_modules("!false", vec![]);
    assert_eq!(res, "true");
    let res = test_run_with_modules("!!false", vec![]);
    assert_eq!(res, "false");
}

#[test]
fn test_binary_operations_arithmetic() {
    let cases = [
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

    for (input, expected) in cases {
        let res = test_run_with_modules(input, vec![]);
        assert_eq!(res, expected, "expected '{}' to output '{}'", input, expected);
    }
}

#[test]
fn test_binary_operations_comparisons() {
    let cases = [
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

    for (input, expected) in cases {
        let res = test_run_with_modules(input, vec![]);
        assert_eq!(res, expected, "expected '{}' to output '{}'", input, expected);
    }
}

#[test]
fn test_binary_operations_booleans() {
    let cases = [
        // exclusive-or
        ("true ^ true", "false"),
        ("false ^ true", "true"),
        ("true ^ false", "true"),
        ("false ^ false", "false"),
        // boolean equality
        ("(1 > 2) == (3 > 4)", "true"),
        ("(1 >= 2) != (3 < 4)", "true")
    ];

    for (input, expected) in cases {
        let res = test_run_with_modules(input, vec![]);
        assert_eq!(res, expected, "expected '{}' to output '{}'", input, expected);
    }
}

#[test]
fn test_binary_operations_string_concat() {
    let cases = [
        (r#""hello " + "world""#, "hello world"),
        (r#""a" + 1"#, "a1"),
        (r#""a" + 1.2"#, "a1.200000"),
        (r#""a" + true"#, "atrue"),
        (r#"1 + "b""#, "1b"),
        (r#"1.2 + "b""#, "1.200000b"),
        (r#"false + "b""#, "falseb"),
    ];

    for (input, expected) in cases {
        let res = test_run_with_modules(input, vec![]);
        assert_eq!(res, expected, "expected '{}' to output '{}'", input, expected);
    }
}

#[test]
fn test_functions() {
    let cases = [
        ("func abc(): Int = 6 + 24", "30"),
        ("func abc(): String = \"hello\"", "hello"),
        ("func abc(): Int[] = [6, 24]", "[6, 24]"),
        ("func abc(): String[] = [\"a\", \"b\"]", "[a, b]")
    ];

    for (decl, expected) in cases {
        let input = format!("{}\nabc()", decl);
        let res = test_run_with_modules(&input, vec![]);
        assert_eq!(res, expected, "expected '{}' to output '{}'", input, expected);
    }
}

#[test]
fn test_method_calls() {
    let cases = [
        ("\"asdf\".toUpper() + \"!\"", "ASDF!"),
        ("(123).toString() + (45.6).toString()", "12345.600000"),
    ];

    for (input, expected) in cases {
        let res = test_run_with_modules(input, vec![]);
        assert_eq!(res, expected, "expected '{}' to output '{}'", input, expected);
    }
}
