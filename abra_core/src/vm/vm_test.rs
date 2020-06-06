// Tests for the vm are in this file rather than in the vm file; that file is going to get massive
// and as will the tests, so it made sense I thought to split them out into a separate file.

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::typechecker::typechecker::typecheck;
    use crate::vm::compiler::compile;
    use crate::vm::value::{Value, Obj, FnValue};
    use crate::vm::vm::{VM, VMContext};
    use crate::vm::opcode::Opcode;
    use std::collections::HashMap;

    fn new_string_obj(string: &str) -> Value {
        Value::new_string_obj(string.to_string())
    }

    fn interpret(input: &str) -> Option<Value> {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let (_, typed_ast) = typecheck(ast).unwrap();
        let (module, _) = compile(typed_ast).unwrap();
        let ctx = VMContext::default();

        let mut vm = VM::new(module, ctx);
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

        let result = interpret("7 % 5").unwrap();
        let expected = Value::Int(2);
        assert_eq!(expected, result);

        let result = interpret("5.25 % 2.5").unwrap();
        let expected = Value::Float(0.25);
        assert_eq!(expected, result);

        let result = interpret("\"hello\" +  \" \"+24  + \" world\"").unwrap();
        let expected = new_string_obj("hello 24 world");
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
    fn interpret_binary_boolean_short_circuiting() {
        let preface = "\n\
          var called = false\n\
          func getTrue() {\n\
            called = true\n\
            true\n\
          }\n\
        ";

        let input = format!("{}\n\
          val res = true || getTrue()
          res + \" \" + called",
                            preface
        );
        let result = interpret(&input).unwrap();
        let expected = new_string_obj("true false");
        assert_eq!(expected, result);

        let input = format!("{}\n\
          val res = false && getTrue()
          res + \" \" + called",
                            preface
        );
        let result = interpret(&input).unwrap();
        let expected = new_string_obj("false false");
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
        let expected = Value::new_array_obj(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
        ]);
        assert_eq!(expected, result);

        let result = interpret("[0, -1, true, 3.4, \"5\"]").unwrap();
        let expected = Value::new_array_obj(vec![
            Value::Int(0),
            Value::Int(-1),
            Value::Bool(true),
            Value::Float(3.4),
            new_string_obj("5"),
        ]);
        assert_eq!(expected, result);

        let result = interpret("[[0, -1], [true, false], [\"a\"]]").unwrap();
        let expected = Value::new_array_obj(vec![
            Value::new_array_obj(vec![Value::Int(0), Value::Int(-1)]),
            Value::new_array_obj(vec![Value::Bool(true), Value::Bool(false)]),
            Value::new_array_obj(vec![new_string_obj("a")]),
        ]);
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

    #[inline]
    fn sorted_map_obj_values(value: Value) -> Vec<(String, Value)> {
        if let Value::Obj(obj) = value {
            match &*obj.borrow() {
                Obj::MapObj(value) => {
                    let mut pairs = value.iter().map(|(k, v)| (k.clone(), v.clone())).collect::<Vec<(String, Value)>>();
                    pairs.sort_by(|(a, _), (b, _)| a.partial_cmp(b).unwrap());
                    pairs
                }
                _ => unreachable!()
            }
        } else {
            panic!("Result should be a MapObj")
        }
    }

    #[test]
    fn interpret_map() {
        let result = interpret("{ a: 1, b: \"hello\", c: true }").unwrap();
        let result_pairs = sorted_map_obj_values(result);
        let expected_pairs = vec![
            ("a".to_string(), Value::Int(1)),
            ("b".to_string(), new_string_obj("hello")),
            ("c".to_string(), Value::Bool(true)),
        ];
        assert_eq!(expected_pairs, result_pairs);

        let result = interpret("{ a: { b: \"hello\" }, c: [1, 2] }").unwrap();
        let result_pairs = sorted_map_obj_values(result);
        let expected_pairs = vec![
            ("a".to_string(), Value::new_map_obj({
                let mut items = HashMap::new();
                items.insert("b".to_string(), new_string_obj("hello"));
                items
            })),
            ("c".to_string(), Value::new_array_obj(vec![Value::Int(1), Value::Int(2)])),
        ];
        assert_eq!(expected_pairs, result_pairs);
    }

    #[test]
    fn interpret_bindings() {
        let input = "\n\
          val a = 123\n
          val b = 456\n
          var c = a + b > b - a\n
          c
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);

        let input = "\n\
          val a1 = 1\n
          val a2 = 2\n
          val a3 = 3\n
          val a = [a1, a2 + a2, 3 * a3]\n
          a
        ";
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![
            Value::Int(1),
            Value::Int(4),
            Value::Int(9),
        ]);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_assignments() {
        let input = "\
          var a = 1\n
          var b = 2\n
          val c = b = a = 3\n\
          a + b + c
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(9);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_assignments_indexing() {
        let input = r#"
          val a = [1]
          a[0] = 123

          val m = { a: 0 }
          m["a"] = 456
          (a[0] ?: 0) + (m["a"] ?: 0)
        "#;
        let result = interpret(input).unwrap();
        let expected = Value::Int(579);
        assert_eq!(expected, result);

        let input = r#"
          val a = [0]
          a[4] = 123
          a
        "#;
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![
            Value::Int(0),
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Int(123),
        ]);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_assignments_fields() {
        let input = r#"
          type Person { name: String }
          val p = Person(name: "ken")
          p.name = "Ken"
          p.name
        "#;
        let result = interpret(input).unwrap();
        let expected = new_string_obj("Ken");
        assert_eq!(expected, result);

        let input = r#"
          type Name { value: String }
          type Person { name: Name }

          val n = Name(value: "ken")
          val p = Person(name: n)
          n.value = "Ken"
          p.name.value
        "#;
        let result = interpret(input).unwrap();
        let expected = new_string_obj("Ken");
        assert_eq!(expected, result);

        let input = r#"
          type Name { value: String }
          type Person { name: Name }

          val p = Person(name: Name(value: "ken"))
          val n = p.name = Name(value: "ken")
          n.value = "Ken"
          p.name.value
        "#;
        let result = interpret(input).unwrap();
        let expected = new_string_obj("Ken");
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_indexing_arrays() {
        let input = "\
          val arr = [1, 2, 3]\n
          val item = arr[1]\n
          item
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(2);
        assert_eq!(expected, result);

        let input = "[1, 2, 3][-1]";
        let result = interpret(input).unwrap();
        let expected = Value::Int(3);
        assert_eq!(expected, result);

        let input = "[][0] == [1, 2][-3]"; // They're both None
        let result = interpret(input).unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_indexing_ranges_arrays() {
        let input = "[1, 2, 3][1:2]";
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![Value::Int(2)]);
        assert_eq!(expected, result);

        let input = "[1, 2, 3][-2:-1]";
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![Value::Int(2)]);
        assert_eq!(expected, result);

        let input = "[1, 2, 3][:1]";
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![Value::Int(1)]);
        assert_eq!(expected, result);

        let input = "[1, 2, 3][1:]";
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![Value::Int(2), Value::Int(3)]);
        assert_eq!(expected, result);

        let input = "[1, 2, 3][-3:]";
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3)
        ]);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_indexing_strings() {
        let input = "\
          val str = \"hello world!\"\n
          val char = str[6]\n
          char
        ";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("w");
        assert_eq!(expected, result);

        let input = "\"hello world\"[-3]";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("r");
        assert_eq!(expected, result);

        let input = "\"hello world\"[100]";
        let result = interpret(input).unwrap();
        let expected = Value::Nil;
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_indexing_ranges_strings() {
        let input = "\"some string\"[1:2]";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("o");
        assert_eq!(expected, result);

        let input = "\"some string\"[-2:-1]";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("n");
        assert_eq!(expected, result);

        let input = "\"some string\"[:4]";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("some");
        assert_eq!(expected, result);

        let input = "\"some string\"[5:]";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("string");
        assert_eq!(expected, result);

        let input = "\"some string\"[-6:]";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("string");
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_coalescing() {
        let input = "\
          val arr = [1, 2, 3]\n
          val item = arr[1] ?: 16\n
          item
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(2);
        assert_eq!(expected, result);

        let input = "\
          val arr = [1, 2, 3]\n
          val item = arr[4] ?: 16\n
          item
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(16);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_if_else_statements() {
        let input = "\
          var res = 0\
          if (1 != 2) {\
            val a = 123\
            res = a\
          } else {\
            res = 456\
          }
        ";
        let result = interpret(input);
        assert!(result.is_none());

        let input = "\
          var res = 0\
          if (1 != 2) {\
            val a = 123\
            res = a\
          } else {\
            res = 456\
          }\
          res
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(123);
        assert_eq!(expected, result);

        let input = "\
          var res = 0\
          if (1 == 2) {\
            res = 123\
          } else {\
            res = 456\
          }\
          res
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(456);
        assert_eq!(expected, result);

        let input = "\
          var res = 0\
          if (1 == 2) {\
            res = 123\
          } else if (2 > 3) {\
            res = 456\
          } else if (4 < 6) {\
            res = 789\
          } else {\
            res = 1011\
          }\
          res
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(789);
        assert_eq!(expected, result);

        let input = "\
          var res = 0\
          val a = 123\
          if (1 != 2) {\
            val b = 123\
            if (true) {\
              val c = a + b \
              res = c
            }\
          } else {\
            res = a + 456\
          }\
          res
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(246);
        assert_eq!(expected, result);

        let input = "\
          val a = 123\
          if (1 != 2) {\
            a + 1\
          } else {\
            a + 2\
          }\
          val b = a + 3\
          b - 3
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(123);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_if_else_expressions() {
        let input = "\
          val abc = if (1 != 2) {\
            123\
          } else {\
            456\
          }\
          abc
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(123);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_declaration() {
        let input = "\
          val ghi = \"hello\"
          func abc(a: Int): Int = 123
          val def = abc
          def
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Fn(FnValue {
            name: "abc".to_string(),
            code: vec![
                Opcode::Constant as u8, 3,
                Opcode::LStore0 as u8,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            upvalues: vec![],
            receiver: None,
        });
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation() {
        let input = "\
          val a = 1\n\
          val b = 2\n\
          val c = 3\n\
          func add(a: Int, b: Int): Int {\n\
            val sum = a + b\n\
            sum\n\
          }\n\
          add(add(a, b), c)\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(6);
        assert_eq!(expected, result);

        // There should be nothing leftover on the stack after a non-returning function executes
        let input = "println(\"hello\")";
        let result = interpret(input);
        assert_eq!(None, result);
    }

    #[test] // Note: this is like a pseudo-closure test, since the variables are globals...
    fn interpret_func_invocation_closure() {
        let input = "\
          val a = 1\n\
          var b = 2\n\
          func getSum(): Int {\n\
            a + b\n\
          }\n\
          b = 17\n\
          getSum()\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(18);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation_nested() {
        let input = "\
          func getSum(): Int {\n\
            func ret1(): Int { 1 }\n\
            func add1(n: Int): Int { n + 1 }\n\
            ret1() + add1(ret1())\n\
          }\n\
          getSum()\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(3);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation_closures() {
        let input = "\
          func getCounter() {\n\
            var count = 100\n\
            func tick() { count = count + 1 }\n\
            count = 0\n\
            tick\n\
          }\n\
          val tick = getCounter()\n\
          val results = [tick(), tick(), tick(), tick(), tick()]\n\
          results\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4),
            Value::Int(5),
        ]);
        assert_eq!(expected, result);

        // Test deeply nested upvalue access
        let input = "\
          func getCounter() {\n\
            var count = 100\n\
            func unnecessaryLayer1() {\n\
              func unnecessaryLayer2() {\n\
                func tick() { count = count + 1 }\n\
                tick\n\
              }\n\
              unnecessaryLayer2\n\
            }\n\
            count = 0\n\
            unnecessaryLayer1()()\n\
          }\n\
          val tick = getCounter()\n\
          val results = [tick(), tick(), tick(), tick(), tick()]\n\
          results\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4),
            Value::Int(5),
        ]);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation_callstack() {
        let input = "\
          val greeting = \"Hello\"\n\
          func exclaim(word: String) {\n\
            val abc = 123\n\
            word + \"!\"\n\
          }\n\
          func greet(recipient: String) {\n\
            greeting + \", \" + exclaim(recipient)\n\
          }\n\
          val languageName = \"Abra\"\n\
          greet(languageName) + \" \" + greet(languageName)\n\
        ";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("Hello, Abra! Hello, Abra!");
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation_nested_if() {
        let input = "\
          val prefix = \"Save the\"\n\
          func exclaim(word: String) {\n\
            val abc = 123\n\
            word + \"!\"\n\
          }\n\
          func save(recipient: String) {\n\
            if (recipient == \"World\") {
              val target = exclaim(recipient)
              prefix + \" \" + target\n\
            } else {
              prefix + \" \" + recipient\n\
            }
          }\n\
          save(\"Cheerleader\") + \", \" + save(\"World\")\n\
        ";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("Save the Cheerleader, Save the World!");
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation_recursion_global() {
        let input = "\
          func fib(n: Int): Int {\n\
            if (n == 0) {\n\
              0\n\
            } else if (n == 1) {\n\
              1\n\
            } else {\n\
              fib(n - 2) + fib(n - 1)\n\
            }\n\
          }\n\
          \n\
          [fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6), fib(7), fib(8)]\n\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![
            Value::Int(0),
            Value::Int(1),
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(5),
            Value::Int(8),
            Value::Int(13),
            Value::Int(21),
        ]);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation_recursion_closure() {
        let input = "\
          func fib(n: Int): Int {\n\
            func fibInner(n: Int): Int {\n\
              if (n == 0) {\n\
                0\n\
              } else if (n == 1) {\n\
                1\n\
              } else {\n\
                fib(n - 2) + fib(n - 1)\n\
              }\n\
            }\n\
            fibInner(n)\n\
          }\n\
          \n\
          [fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6), fib(7), fib(8)]\n\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![
            Value::Int(0),
            Value::Int(1),
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(5),
            Value::Int(8),
            Value::Int(13),
            Value::Int(21),
        ]);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation_default_args() {
        let input = "\
          func abc(a: Int = 2, b = 3, c = 5) { a * b * c }\n\
          [abc(), abc(7), abc(7, 11), abc(7, 11, 13)]\n\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::new_array_obj(vec![
            Value::Int(30),
            Value::Int(105),
            Value::Int(385),
            Value::Int(1001),
        ]);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation_default_args_closure() {
        let input = "
          var called = false
          func getOne() {
            called = true
            1
          }
          func abc(def = getOne()) = def
          abc(1)
          called
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Bool(false);
        assert_eq!(expected, result);

        let input = "
          var called = false
          func getOne() {
            called = true
            1
          }
          func abc(def = getOne()) = def
          abc()
          called
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation_default_args_laziness() {
        let input = "
          func getOne() = 1
          func outer() {
            func abc(def = getOne(), ghi = def, jkl = def + ghi) {
              def + ghi + jkl
            }
            abc
          }
          val fn = outer()
          fn()
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(4);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_while_loop() {
        let input = "\
          var a = 0\n\
          while a < 4 {\n\
            a = a + 1\n\
          }\n\
          a\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(4);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_while_loop_fizzbuzzish() {
        let input = "\
          var i = 1\n\
          var output = \"\"\n\
          while i <= 20 {\n\
            val msg = if i % 15 == 0 {\n\
              \"Fb\"\n\
            } else if i % 3 == 0 {\n\
              \"F\"\n\
            } else if i % 5 == 0 {\n\
              \"B\"\n\
            } else {\n\
              \"\" + i\n\
            }\n\
            \n\
            output = output + msg + \",\"\n\
            i = i + 1\n\
          }\n\
          output\
        ";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("1,2,F,4,B,F,7,8,F,B,11,F,13,14,Fb,16,17,F,19,B,");
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_while_loop_with_break() {
        let input = "\
          var a = 0\n\
          while true {\n\
            a = a + 1\n\
            if a == 3 {\n\
              break\n\
            }\n\
          }\n\
          a\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(3);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_nested_loops_with_break() {
        let input = "\
          var a = 0\n\
          while true {\n\
            a = a + 1\n\
            while true {\n\
              a = a + 1\n\
              if a >= 3 { break }\n\
            }\n\
            if a > 3 {\n\
              break\n\
            }\n\
          }\n\
          a\
        ";
        let result = interpret(input).unwrap();
        // If this returned 3, we'd know that `break` destroyed the outer loop too, but it doesn't
        let expected = Value::Int(5);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_for_loop_fizzbuzzish() {
        let input = "\
          var output = \"\"\n\
          for i in range(1, 21) {\n\
            val msg = if i % 15 == 0 {\n\
              \"Fb\"\n\
            } else if i % 3 == 0 {\n\
              \"F\"\n\
            } else if i % 5 == 0 {\n\
              \"B\"\n\
            } else {\n\
              \"\" + i\n\
            }\n\
            \n\
            output = output + msg + \",\"\n\
          }\n\
          output\
        ";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("1,2,F,4,B,F,7,8,F,B,11,F,13,14,Fb,16,17,F,19,B,");
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_for_loop_nested() {
        let input = "\
          var output = \"\"\n\
          for n1, i in range(1, 4) {\n\
            output = output + n1\n\
            for n2 in range(4, 7) {\n\
              output = output + n2\n\
            }\n\
            if i < 2 {\n\
              output = output + \", \"\n\
            }\n\
          }\n\
          output\
        ";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("1456, 2456, 3456");
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_for_loops_in_fns() {
        let input = "\
          var output = \"\"\n\
          func innerLoop() {\n\
            for a2, i2 in range(0, 2) {\n\
              output = output + (\"Inner \" + a2) + \" \"\n\
            }\n\
          }\n\
          func runLoop() {\n\
            for a1, i1 in range(0, 2) {\n\
              output = output + (\"Outer \" + a1) + \" [\"\n\
              innerLoop()\n\
              output = output + \"], \"
            }\n\
          }\n\
          runLoop()\n\
          output\
        ";
        let result = interpret(input).unwrap();
        let expected = "Outer 0 [Inner 0 Inner 1 ], Outer 1 [Inner 0 Inner 1 ], ";
        let expected = new_string_obj(expected);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_accessor_struct() {
        let input = "\
          type Person { name: String }\n\
          val ken = Person(name: \"Ken\")\n\
          ken.name\n\
        ";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("Ken");
        assert_eq!(expected, result);

        // Test with default value
        let input = "\
          type Person { name: String, age: Int = 0 }\n\
          val aBaby = Person(name: \"Baby\")\n\
          aBaby.age\n\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Int(0);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_accessor_builtins() {
        let tests = vec![
            // Strings
            ("\"\".length", Value::Int(0)),
            ("\"hello\".length", Value::Int(5)),
            // Arrays
            ("[].length", Value::Int(0)),
            ("[1, 2, 3, 4].length", Value::Int(4)),
        ];
        for (input, expected) in tests.into_iter() {
            let result = interpret(input).unwrap();
            assert_eq!(expected, result);
        }
    }

    #[test]
    fn interpret_method_invocation_struct() {
        let input = "\
          type Person {\n\
            name: String\n\
            func introduce(self) = \"I am \" + self.name\n\
          }\n\
          val ken = Person(name: \"Ken\")\n\
          val brian = Person(name: \"Brian\")\n\
          ken.introduce() + \", and \" + brian.introduce()\n\
        ";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("I am Ken, and I am Brian");
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_method_invocation_preserve_receiver() {
        let input = "\
          type Person {\n\
            name: String\n\
            func introduce(self) = \"I am \" + self.name\n\
          }\n\
          val ken = Person(name: \"Ken\")\n\
          val introduceFn = ken.introduce\n\
          introduceFn()\
        ";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("I am Ken");
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_static_method_invocation() {
        let input = "\
          type Person {\n\
            name: String\n\
            func introduce(name: String) = \"I am \" + name\n\
          }\n\
          val ken = Person(name: \"Ken\")\n\
          Person.introduce(ken.name)\
        ";
        let result = interpret(input).unwrap();
        let expected = new_string_obj("I am Ken");
        assert_eq!(expected, result);
    }
}