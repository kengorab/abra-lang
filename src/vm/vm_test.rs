// Tests for the vm are in this file rather than in the vm file; that file is going to get massive
// and as will the tests, so it made sense I thought to split them out into a separate file.

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::typechecker::typechecker::typecheck;
    use crate::vm::compiler::compile;
    use crate::vm::value::{Value, Obj};
    use crate::vm::vm::VM;

    fn interpret(input: &str) -> Option<Value> {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let (_, typed_ast) = typecheck(ast).unwrap();
        let chunk = compile(typed_ast).unwrap();

        let mut vm = VM::new(&chunk);
        vm.run().unwrap()
    }

    #[test]
    fn interpret_nothing() {
        assert!(interpret("").is_none());
    }

    #[test]
    fn interpret_constants() {
        let result = interpret("1").unwrap();
        let expected = Value::Int(1);
        assert_eq!(expected, result);

        let result = interpret("1.23").unwrap();
        let expected = Value::Float(1.23);
        assert_eq!(expected, result);

        let result = interpret("true").unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);

        let result = interpret("false").unwrap();
        let expected = Value::Bool(false);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_unary() {
        let result = interpret("-1").unwrap();
        let expected = Value::Int(-1);
        assert_eq!(expected, result);

        let result = interpret("-1.23").unwrap();
        let expected = Value::Float(-1.23);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_binary() {
        let result = interpret("1 + 2 * 3.4 / 5").unwrap();
        let expected = Value::Float(2.36);
        assert_eq!(expected, result);

        let result = interpret("\"hello\" +  \" \"+24  + \" world\"").unwrap();
        let expected = Value::Obj(Obj::StringObj { value: Box::new("hello 24 world".to_string()) });
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_binary_boolean() {
        let result = interpret("true || false").unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);

        let result = interpret("true && false").unwrap();
        let expected = Value::Bool(false);
        assert_eq!(expected, result);

        let result = interpret("true && false || true && true").unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_binary_comparisons() {
        let cases = vec![
            // Testing <
            ("1 < 3", true), ("1 < 1", false),
            ("1 < 3.0", true), ("1 < 1.0", false),
            ("1.0 < 3.0", true), ("1.0 < 1.0", false),
            ("1.0 < 3", true), ("1.0 < 1", false),
            ("\"a\" < \"b\"", true), ("\"a\" < \"a\"", false),
            // Testing <=
            ("1 <= 3", true), ("3 <= 1", false),
            ("3 <= 3.0", true), ("3 <= 1.0", false),
            ("1.0 <= 3.0", true), ("3.0 <= 1.0", false),
            ("1.0 <= 3", true), ("3.0 <= 1", false),
            ("\"a\" <= \"b\"", true), ("\"b\" <= \"a\"", false),
            ("\"a\" <= \"a\"", true),
            // Testing >
            ("3 > 1", true), ("1 > 1", false),
            ("3.0 > 1", true), ("1.0 > 1", false),
            ("3.0 > 1.0", true), ("1.0 > 3.0", false),
            ("3 > 1.0", true), ("1.0 > 1", false),
            ("\"b\" > \"a\"", true), ("\"a\" > \"a\"", false),
            // Testing >=
            ("3 >= 1", true), ("1 >= 3", false),
            ("3 >= 3.0", true), ("1.0 >= 3", false),
            ("3.0 >= 1.0", true), ("1.0 >= 3.0", false),
            ("3 >= 1.0", true), ("1 >= 3.0", false),
            ("\"b\" >= \"a\"", true), ("\"a\" >= \"b\"", false),
            ("\"b\" >= \"b\"", true),
            // Testing ==
            ("3 == 3", true), ("3 == 1", false),
            ("3 == 3.0", true), ("3 == 1.0", false),
            ("3.0 == 3.0", true), ("3.0 == 1.0", false),
            ("3.0 == 3", true), ("3.0 == 1", false),
            ("true == true", true), ("true == false", false),
            ("\"abc\" == \"abc\"", true), ("\"abc\" == \"def\"", false),
            ("\"abc\" == true", false), ("\"abc\" == false", false),
            ("\"abc\" == 3.14", false), ("\"abc\" == 4", false),
            // Testing !=
            ("3 != 1", true), ("3 != 3", false),
            ("3 != 1.0", true), ("3 != 3.0", false),
            ("3.0 != 1.0", true), ("3.0 != 3.0", false),
            ("3.0 != 1", true), ("3.0 != 3", false),
            ("true != false", true), ("true != true", false),
            ("\"abc\" != \"def\"", true), ("\"abc\" != \"abc\"", false),
            ("\"abc\" != true", true), ("\"abc\" != false", true),
            ("\"abc\" != 3.14", true), ("\"abc\" != 4", true),
        ];

        for (input, expected) in cases {
            let result = interpret(input).unwrap();
            assert_eq!(Value::Bool(expected), result, "Interpreting {} should be {}", input, expected);
        }
    }

    #[test]
    fn interpret_array() {
        let result = interpret("[1, 2, 3]").unwrap();
        let expected = Value::Obj(Obj::ArrayObj {
            value: vec![
                Box::new(Value::Int(1)),
                Box::new(Value::Int(2)),
                Box::new(Value::Int(3)),
            ]
        });
        assert_eq!(expected, result);

        let result = interpret("[0, -1, true, 3.4, \"5\"]").unwrap();
        let expected = Value::Obj(Obj::ArrayObj {
            value: vec![
                Box::new(Value::Int(0)),
                Box::new(Value::Int(-1)),
                Box::new(Value::Bool(true)),
                Box::new(Value::Float(3.4)),
                Box::new(Value::Obj(Obj::StringObj { value: Box::new("5".to_string()) })),
            ]
        });
        assert_eq!(expected, result);

        let result = interpret("[[0, -1], [true, false], [\"a\"]]").unwrap();
        let expected = Value::Obj(Obj::ArrayObj {
            value: vec![
                Box::new(Value::Obj(Obj::ArrayObj {
                    value: vec![Box::new(Value::Int(0)), Box::new(Value::Int(-1))]
                })),
                Box::new(Value::Obj(Obj::ArrayObj {
                    value: vec![Box::new(Value::Bool(true)), Box::new(Value::Bool(false))]
                })),
                Box::new(Value::Obj(Obj::ArrayObj {
                    value: vec![Box::new(Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }))]
                })),
            ]
        });
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_array_equality() {
        let result = interpret("[1, 2] == [1, 2]").unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);

        let result = interpret("[1, 3] == [1, 2]").unwrap();
        let expected = Value::Bool(false);
        assert_eq!(expected, result);

        let result = interpret("[[0, 1]] == [[1, 2]]").unwrap();
        let expected = Value::Bool(false);
        assert_eq!(expected, result);

        let result = interpret("[[0, 1], [2, 3]] == [[0, 1], [2, 3]]").unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);

        let result = interpret("[1, 2] == \"hello\"").unwrap();
        let expected = Value::Bool(false);
        assert_eq!(expected, result);
    }
}