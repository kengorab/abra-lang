// Tests for the vm are in this file rather than in the vm file; that file is going to get massive
// and as will the tests, so it made sense I thought to split them out into a separate file.

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::typechecker::typechecker::typecheck;
    use crate::vm::compiler::compile;
    use crate::vm::value::{Value, Obj};
    use crate::vm::vm::{VM, VMContext};
    use std::collections::HashMap;
    use crate::vm::opcode::Opcode;

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

    #[inline]
    fn sorted_map_obj_values(value: Value) -> Vec<(String, Value)> {
        if let Value::Obj(Obj::MapObj { value }) = value {
            let mut pairs = value.into_iter().collect::<Vec<(String, Value)>>();
            pairs.sort_by(|(a, _), (b, _)| a.partial_cmp(b).unwrap());
            pairs
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
            ("b".to_string(), Value::Obj(Obj::StringObj { value: Box::new("hello".to_string()) })),
            ("c".to_string(), Value::Bool(true)),
        ];
        assert_eq!(expected_pairs, result_pairs);

        let result = interpret("{ a: { b: \"hello\" }, c: [1, 2] }").unwrap();
        let result_pairs = sorted_map_obj_values(result);
        let expected_pairs = vec![
            ("a".to_string(), Value::Obj(Obj::MapObj {
                value: {
                    let mut items = HashMap::new();
                    items.insert("b".to_string(), Value::Obj(Obj::StringObj { value: Box::new("hello".to_string()) }));
                    items
                }
            })),
            ("c".to_string(), Value::Obj(Obj::ArrayObj {
                value: vec![Box::new(Value::Int(1)), Box::new(Value::Int(2))]
            })),
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
        let expected = Value::Obj(Obj::ArrayObj {
            value: vec![
                Box::new(Value::Int(1)),
                Box::new(Value::Int(4)),
                Box::new(Value::Int(9)),
            ]
        });
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
    fn interpret_indexing_arrays() {
        let input = "\
          val arr = [1, 2, 3]\n
          val item = arr[1]\n
          item
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::OptionObj { value: Some(Box::new(Value::Int(2))) });
        assert_eq!(expected, result);

        let input = "[1, 2, 3][-1]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::OptionObj { value: Some(Box::new(Value::Int(3))) });
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
        let expected = Value::Obj(Obj::ArrayObj {
            value: vec![Box::new(Value::Int(2))]
        });
        assert_eq!(expected, result);

        let input = "[1, 2, 3][-2:-1]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::ArrayObj {
            value: vec![Box::new(Value::Int(2))]
        });
        assert_eq!(expected, result);

        let input = "[1, 2, 3][:1]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::ArrayObj {
            value: vec![Box::new(Value::Int(1))]
        });
        assert_eq!(expected, result);

        let input = "[1, 2, 3][1:]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::ArrayObj {
            value: vec![Box::new(Value::Int(2)), Box::new(Value::Int(3))]
        });
        assert_eq!(expected, result);

        let input = "[1, 2, 3][-3:]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::ArrayObj {
            value: vec![Box::new(Value::Int(1)), Box::new(Value::Int(2)), Box::new(Value::Int(3))]
        });
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
        let expected = Value::Obj(Obj::OptionObj {
            value: Some(
                Box::new(
                    Value::Obj(Obj::StringObj { value: Box::new("w".to_string()) })
                )
            )
        });
        assert_eq!(expected, result);

        let input = "\"hello world\"[-3]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::OptionObj {
            value: Some(
                Box::new(
                    Value::Obj(Obj::StringObj { value: Box::new("r".to_string()) })
                )
            )
        });
        assert_eq!(expected, result);

        let input = "\"hello world\"[100]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::OptionObj { value: None });
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_indexing_ranges_strings() {
        let input = "\"some string\"[1:2]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::StringObj { value: Box::new("o".to_string()) });
        assert_eq!(expected, result);

        let input = "\"some string\"[-2:-1]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::StringObj { value: Box::new("n".to_string()) });
        assert_eq!(expected, result);

        let input = "\"some string\"[:4]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::StringObj { value: Box::new("some".to_string()) });
        assert_eq!(expected, result);

        let input = "\"some string\"[5:]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::StringObj { value: Box::new("string".to_string()) });
        assert_eq!(expected, result);

        let input = "\"some string\"[-6:]";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::StringObj { value: Box::new("string".to_string()) });
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
        let expected = Value::Fn {
            name: "abc".to_string(),
            code: vec![
                Opcode::Constant as u8, 3,
                Opcode::LStore0 as u8,
                Opcode::Return as u8
            ],
        };
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
    }

    #[test]
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
        let expected = Value::Obj(Obj::StringObj { value: Box::new("Hello, Abra! Hello, Abra!".to_string()) });
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
        let expected = Value::Obj(Obj::StringObj { value: Box::new("Save the Cheerleader, Save the World!".to_string()) });
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation_recursion() {
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
        let expected = Value::Obj(Obj::ArrayObj {
            value: vec![
                Box::new(Value::Int(0)),
                Box::new(Value::Int(1)),
                Box::new(Value::Int(1)),
                Box::new(Value::Int(2)),
                Box::new(Value::Int(3)),
                Box::new(Value::Int(5)),
                Box::new(Value::Int(8)),
                Box::new(Value::Int(13)),
                Box::new(Value::Int(21)),
            ]
        });
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_func_invocation_default_args() {
        let input = "\
          func abc(a: Int = 2, b = 3, c = 5) { a * b * c }\n\
          [abc(), abc(7), abc(7, 11), abc(7, 11, 13)]\n\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::ArrayObj {
            value: vec![
                Box::new(Value::Int(30)),
                Box::new(Value::Int(105)),
                Box::new(Value::Int(385)),
                Box::new(Value::Int(1001)),
            ]
        });
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
        let expected = Value::Obj(Obj::StringObj {
            value: Box::new("1,2,F,4,B,F,7,8,F,B,11,F,13,14,Fb,16,17,F,19,B,".to_string())
        });
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
        let expected = Value::Obj(Obj::StringObj {
            value: Box::new("1,2,F,4,B,F,7,8,F,B,11,F,13,14,Fb,16,17,F,19,B,".to_string())
        });
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
        let expected = Value::Obj(Obj::StringObj {
            value: Box::new("1456, 2456, 3456".to_string())
        });
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
        let expected = Value::Obj(Obj::StringObj { value: Box::new(expected.to_string()) });
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_accessor_struct() {
        let input = "\
          type Person { name: String }\n\
          val ken = Person({ name: \"Ken\" })\n\
          ken.name\n\
        ";
        let result = interpret(input).unwrap();
        let expected = Value::Obj(Obj::StringObj { value: Box::new("Ken".to_string()) });
        assert_eq!(expected, result);

        // Test with default value
        let input = "\
          type Person { name: String, age: Int = 0 }\n\
          val aBaby = Person({ name: \"Baby\" })\n\
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
}