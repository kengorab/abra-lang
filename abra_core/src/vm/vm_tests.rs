use crate::vm::opcode::Opcode;
use crate::vm::value::{Value, FnValue};
use crate::vm::vm::{VM, VMContext};
use itertools::Itertools;
use crate::common::test_utils::MockModuleReader;
use crate::parser::ast::ModuleId;

fn new_string_obj(string: &str) -> Value {
                                       Value::new_string_obj(string.to_string())
                                                                                 }

fn interpret(input: &str) -> Value {
    let mut mock_reader = MockModuleReader::default();
    let module_id = ModuleId::parse_module_path("./test").unwrap();
    let modules = crate::compile(module_id, &input.to_string(), &mut mock_reader).unwrap();

    let ctx = VMContext::default();
    let mut vm = VM::new(ctx);
    let mut res = Value::Nil;
    for module in modules {
        res = vm.run(module).unwrap();
    }
    res
}

fn interpret_with_modules(input: &str, modules: Vec<(&str, &str)>) -> Value {
    let mut mock_reader = MockModuleReader::new(modules);
    let module_id = ModuleId::parse_module_path("./test").unwrap();
    let modules = crate::compile(module_id, &input.to_string(), &mut mock_reader).unwrap();

    let ctx = VMContext::default();
    let mut vm = VM::new(ctx);
    let mut res = Value::Nil;
    for module in modules {
        res = vm.run(module).unwrap();
    }
    res
}

#[test]
fn interpret_nothing() {
                     assert_eq!(Value::Nil, interpret(""));
                                                            }

#[test]
fn interpret_constants() {
    let result = interpret("1");
    let expected = Value::Int(1);
    assert_eq!(expected, result);

    let result = interpret("1.23");
    let expected = Value::Float(1.23);
    assert_eq!(expected, result);

    let result = interpret("true");
    let expected = Value::Bool(true);
    assert_eq!(expected, result);

    let result = interpret("false");
    let expected = Value::Bool(false);
    assert_eq!(expected, result);
}

#[test]
fn interpret_unary() {
    let result = interpret("-1");
    let expected = Value::Int(-1);
    assert_eq!(expected, result);

    let result = interpret("-1.23");
    let expected = Value::Float(-1.23);
    assert_eq!(expected, result);
}

#[test]
fn interpret_binary() {
    let result = interpret("1 + 2 * 3.4 / 5");
    let expected = Value::Float(2.36);
    assert_eq!(expected, result);

    let result = interpret("7 % 5");
    let expected = Value::Int(2);
    assert_eq!(expected, result);

    let result = interpret("5.25 % 2.5");
    let expected = Value::Float(0.25);
    assert_eq!(expected, result);

    let result = interpret("\"hello\" +  \" \"+24  + \" world\"");
    let expected = new_string_obj("hello 24 world");
    assert_eq!(expected, result);

    let result = interpret("2 ** 3 ** 4");
    let expected = Value::Float(4096.0);
    assert_eq!(expected, result);

    let result = interpret("2 ** 3.1");
    let expected = Value::Float(8.574187700290345);
    assert_eq!(expected, result);
}

#[test]
fn interpret_binary_boolean() {
    let result = interpret("true || false");
    let expected = Value::Bool(true);
    assert_eq!(expected, result);

    let result = interpret("true && false");
    let expected = Value::Bool(false);
    assert_eq!(expected, result);

    let result = interpret("true && false || true && true");
    let expected = Value::Bool(true);
    assert_eq!(expected, result);

    for (l, r, res) in vec![(true, true, false), (true, false, true), (false, true, true), (false, false, false)] {
        let result = interpret(format!("{} ^ {}", l, r).as_str());
        let expected = Value::Bool(res);
        assert_eq!(expected, result);
    }
}

#[test]
fn interpret_binary_boolean_short_circuiting() {
    let preface = "\n\
      var called = false\n\
      func getTrue(): Bool {\n\
        called = true\n\
        true\n\
      }\n\
    ";

    let input = format!(
        "{}\n\
        val res = true || getTrue()
        res + \" \" + called",
        preface
    );
    let result = interpret(&input);
    let expected = new_string_obj("true false");
    assert_eq!(expected, result);

    let input = format!(
        "{}\n\
        val res = false && getTrue()
        res + \" \" + called",
        preface
    );
    let result = interpret(&input);
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
        let result = interpret(input);
        assert_eq!(Value::Bool(expected), result, "Interpreting {} should be {}", input, expected);
    }
}

#[test]
fn interpret_binary_assignment_operators() {
    let cases = vec![
        ("var a = 1\na += 3\na", Value::Int(4)),
        ("var a = 1\na -= 3\na", Value::Int(-2)),
        ("var a = 1\na *= 3\na", Value::Int(3)),
        ("var a = 1.0\na /= 3\na", Value::Float(1.0 / 3.0)),
        ("var a = true\na &&= false\na", Value::Bool(false)),
        ("var a = true\na ||= false\na", Value::Bool(true)),
        ("var a = None\na ?:= false\na ?: true\na", Value::Bool(false)),
    ];

    for (input, expected) in cases {
        let result = interpret(input);
        assert_eq!(expected, result, "Interpreting {} should be {}", input, expected);
    }
}

#[test]
fn interpret_array() {
    let result = interpret("[1, 2, 3]");
    let expected = Value::new_array_obj(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
    ]);
    assert_eq!(expected, result);

    let result = interpret("[0, -1, true, 3.4, \"5\"]");
    let expected = Value::new_array_obj(vec![
        Value::Int(0),
        Value::Int(-1),
        Value::Bool(true),
        Value::Float(3.4),
        new_string_obj("5"),
    ]);
    assert_eq!(expected, result);

    let result = interpret("[[0, -1], [true, false], [\"a\"]]");
    let expected = Value::new_array_obj(vec![
        Value::new_array_obj(vec![Value::Int(0), Value::Int(-1)]),
        Value::new_array_obj(vec![Value::Bool(true), Value::Bool(false)]),
        Value::new_array_obj(vec![new_string_obj("a")]),
    ]);
    assert_eq!(expected, result);
}

#[test]
fn interpret_array_equality() {
    let result = interpret("[1, 2] == [1, 2]");
    let expected = Value::Bool(true);
    assert_eq!(expected, result);

    let result = interpret("[1, 3] == [1, 2]");
    let expected = Value::Bool(false);
    assert_eq!(expected, result);

    let result = interpret("[[0, 1]] == [[1, 2]]");
    let expected = Value::Bool(false);
    assert_eq!(expected, result);

    let result = interpret("[[0, 1], [2, 3]] == [[0, 1], [2, 3]]");
    let expected = Value::Bool(true);
    assert_eq!(expected, result);

    let result = interpret("[1, 2] == \"hello\"");
    let expected = Value::Bool(false);
    assert_eq!(expected, result);
}

#[test]
fn interpret_string_interpolation() {
    let input = r#"
      val a = "def"
      "abc $a ghi"
    "#;
    let result = interpret(input);
    let expected = new_string_obj("abc def ghi");
    assert_eq!(expected, result);

    let input = r#"
      func a(): String = "def"
      "$a() ghi"
    "#;
    let result = interpret(input);
    let expected = new_string_obj("<func a>() ghi");
    assert_eq!(expected, result);

    let input = r#"
      "abc $true ghi"
    "#;
    let result = interpret(input);
    let expected = new_string_obj("abc true ghi");
    assert_eq!(expected, result);

    let input = r#"
      func a(): String = "def"
      "abc ${a()}"
    "#;
    let result = interpret(input);
    let expected = new_string_obj("abc def");
    assert_eq!(expected, result);

    let input = r#"
      "abc ${[1, 2, 3, 4]
               .map(x => x + 2)
               .join(",")} ghi"
    "#;
    let result = interpret(input);
    let expected = new_string_obj("abc 3,4,5,6 ghi");
    assert_eq!(expected, result);

    let input = r#"
      "${"hello" + " " + "${"world" + "!"}"}"
    "#;
    let result = interpret(input);
    let expected = new_string_obj("hello world!");
    assert_eq!(expected, result);
}

#[inline]
fn assert_maps_eq(expected: Vec<(Value, Value)>, map_value: Value) {
    let map = &*map_value.as_map().borrow();
    let actual = map._inner
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect::<Vec<(Value, Value)>>();

    let len = expected.len();
    let perms = expected.into_iter().permutations(len).collect::<Vec<_>>();
    assert!(perms.contains(&actual));
}

#[test]
fn interpret_map() {
    let result = interpret("{ a: 1, \"b\": \"hello\", (true || false): false }");
    let expected_pairs = vec![
        (new_string_obj("a"), Value::Int(1)),
        (new_string_obj("b"), new_string_obj("hello")),
        (Value::Bool(true), Value::Bool(false)),
    ];
    assert_maps_eq(expected_pairs, result);

    let result = interpret("{ a: { b: \"hello\" }, c: [1, 2] }");
    let expected_pairs = vec![
        (new_string_obj("a"), Value::new_map_obj(vec![
            new_string_obj("b"), new_string_obj("hello")
        ])),
        (new_string_obj("c"), Value::new_array_obj(vec![Value::Int(1), Value::Int(2)])),
    ];
    assert_maps_eq(expected_pairs, result);
}

#[test]
fn interpret_bindings() {
    let input = r#"
      val a = 123
      val b = 456
      var c = a + b > b - a
      c
    "#;
    let result = interpret(input);
    let expected = Value::Bool(true);
    assert_eq!(expected, result);

    let input = r#"
      val a1 = 1
      val a2 = 2
      val a3 = 3
      val a = [a1, a2 + a2, 3 * a3]
      a
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![
        Value::Int(1),
        Value::Int(4),
        Value::Int(9),
    ]);
    assert_eq!(expected, result);

    let input = r#"
      val (a, b) = (1, 2)
      a + b
    "#;
    let result = interpret(input);
    let expected = Value::Int(3);
    assert_eq!(expected, result);

    let input = r#"
      func manhattanDistance(p1: (Int, Int), p2: (Int, Int)): Int {
        val (x1, y1) = p1
        val (x2, y2) = p2
        (x2 - x1).abs() + (y2 - y1).abs()
      }
      manhattanDistance((0, 0), (2, 2))
    "#;
    let result = interpret(input);
    let expected = Value::Int(4);
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
    let result = interpret(input);
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
    let result = interpret(input);
    let expected = Value::Int(579);
    assert_eq!(expected, result);

    let input = r#"
      val a = [0]
      a[4] = 123
      a
    "#;
    let result = interpret(input);
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
    let result = interpret(input);
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
    let result = interpret(input);
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
    let result = interpret(input);
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
    let result = interpret(input);
    let expected = Value::Int(2);
    assert_eq!(expected, result);

    let input = "[1, 2, 3][-1]";
    let result = interpret(input);
    let expected = Value::Int(3);
    assert_eq!(expected, result);

    let input = "[][0] == [1, 2][-3]"; // They're both None
    let result = interpret(input);
    let expected = Value::Bool(true);
    assert_eq!(expected, result);
}

#[test]
fn interpret_indexing_ranges_arrays() {
    let input = "[1, 2, 3][1:2]";
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![Value::Int(2)]);
    assert_eq!(expected, result);

    let input = "[1, 2, 3][-2:-1]";
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![Value::Int(2)]);
    assert_eq!(expected, result);

    let input = "[1, 2, 3][:1]";
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![Value::Int(1)]);
    assert_eq!(expected, result);

    let input = "[1, 2, 3][1:]";
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![Value::Int(2), Value::Int(3)]);
    assert_eq!(expected, result);

    let input = "[1, 2, 3][-3:]";
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
    assert_eq!(expected, result);

    // Starting and ending ranges outside of bounds
    let input = "[1, 2, 3][3:]";
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![]);
    assert_eq!(expected, result);

    let input = "[1, 2, 3][:4]";
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
    assert_eq!(expected, result);
}

#[test]
fn interpret_indexing_strings() {
    let input = "\
      val str = \"hello world!\"\n
      val char = str[6]\n
      char
    ";
    let result = interpret(input);
    let expected = new_string_obj("w");
    assert_eq!(expected, result);

    let input = "\"hello world\"[-3]";
    let result = interpret(input);
    let expected = new_string_obj("r");
    assert_eq!(expected, result);

    let input = "\"hello world\"[100]";
    let result = interpret(input);
    let expected = Value::Nil;
    assert_eq!(expected, result);
}

#[test]
fn interpret_indexing_ranges_strings() {
    let input = "\"some string\"[1:2]";
    let result = interpret(input);
    let expected = new_string_obj("o");
    assert_eq!(expected, result);

    let input = "\"some string\"[-2:-1]";
    let result = interpret(input);
    let expected = new_string_obj("n");
    assert_eq!(expected, result);

    let input = "\"some string\"[:4]";
    let result = interpret(input);
    let expected = new_string_obj("some");
    assert_eq!(expected, result);

    let input = "\"some string\"[5:]";
    let result = interpret(input);
    let expected = new_string_obj("string");
    assert_eq!(expected, result);

    let input = "\"some string\"[-6:]";
    let result = interpret(input);
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
    let result = interpret(input);
    let expected = Value::Int(2);
    assert_eq!(expected, result);

    let input = "\
      val arr = [1, 2, 3]\n
      val item = arr[4] ?: 16\n
      item
    ";
    let result = interpret(input);
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
    assert_eq!(Value::Nil, result);

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
    let result = interpret(input);
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
    let result = interpret(input);
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
    let result = interpret(input);
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
    let result = interpret(input);
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
    let result = interpret(input);
    let expected = Value::Int(123);
    assert_eq!(expected, result);
}

#[test]
fn interpret_if_else_expressions() {
    let input = r#"
      val abc = if (1 != 2) {
        123
      } else {
        456
      }
      abc
    "#;
    let result = interpret(input);
    let expected = Value::Int(123);
    assert_eq!(expected, result);

    let input = "4 + if (true) { 20 } else { 0 }";
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);

    let input = r#"
      func abc(): Int {
        val a = 20
        4 + if (true) {
          val b = 123
          a
        } else { 0 }
      }
      abc()
    "#;
    let result = interpret(input);
    let expected = Value::Int(24);
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
    let result = interpret(input);
    let expected = Value::Fn(FnValue {
        name: "abc".to_string(),
        code: vec![
            Opcode::Constant(1, 1),
            Opcode::LStore(0),
            Opcode::Return
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
    let result = interpret(input);
    let expected = Value::Int(6);
    assert_eq!(expected, result);

    // There should be nothing leftover on the stack after a Unit-returning function executes
    let input = "println(\"hello\")";
    let result = interpret(input);
    assert_eq!(Value::Nil, result);

    let input = r#"
      func foo(fn: () => Unit) = fn()
      foo(() => "hello")
    "#;
    let result = interpret(input);
    assert_eq!(Value::Nil, result);
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
    let result = interpret(input);
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
    let result = interpret(input);
    let expected = Value::Int(3);
    assert_eq!(expected, result);
}

#[test]
fn interpret_func_invocation_closures() {
    let input = "\
      func getCounter(): () => Int {\n\
        var count = 100\n\
        func tick(): Int { count = count + 1 }\n\
        count = 0\n\
        tick\n\
      }\n\
      val tick = getCounter()\n\
      val results = [tick(), tick(), tick(), tick(), tick()]\n\
      results\
    ";
    let result = interpret(input);
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
      func getCounter(): () => Int {\n\
        var count = 100\n\
        func unnecessaryLayer1(): () => () => Int {\n\
          func unnecessaryLayer2(): () => Int {\n\
            func tick(): Int { count = count + 1 }\n\
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
    let result = interpret(input);
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
fn interpret_func_invocation_closures_upvalues_not_yet_closed() {
    let input = r#"
      func abc(): Int {
        val a = 20
        func def(): Int { a }
        4 + def()
      }
      abc()
    "#;
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);

    let input = r#"
      func abc(): Int {
        var a = 20
        func wrapper(): Int {
          val b = 123
          func wrapper2(): Int {
            val c = 456
            if (true) {
              val b = 456
              a = 24
              a
            } else {
              0
            }
          }
          wrapper2()
        }
        wrapper()
      }
      abc()
    "#;

    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);
}

#[test]
fn interpret_func_invocation_callstack() {
    let input = "\
      val greeting = \"Hello\"\n\
      func exclaim(word: String): String {\n\
        val abc = 123\n\
        word + \"!\"\n\
      }\n\
      func greet(recipient: String): String {\n\
        greeting + \", \" + exclaim(recipient)\n\
      }\n\
      val languageName = \"Abra\"\n\
      greet(languageName) + \" \" + greet(languageName)\n\
    ";
    let result = interpret(input);
    let expected = new_string_obj("Hello, Abra! Hello, Abra!");
    assert_eq!(expected, result);
}

#[test]
fn interpret_func_invocation_nested_if() {
    let input = r#"
      val prefix = "Save the"
      func exclaim(word: String): String {
        val abc = 123
        word + "!"
      }
      func save(recipient: String): String {
        if (recipient == "World") {
          val target = exclaim(recipient)
          prefix + " " + target
        } else {
          prefix + " " + recipient
        }
      }
      save("Cheerleader") + ", " + save("World")
    "#;
    let result = interpret(input);
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
    let result = interpret(input);
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
    let result = interpret(input);
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
    let input = r#"
      func abc(a: Int = 2, b = 3, c = 5): Int { a * b * c }
      [abc(), abc(7), abc(7, 11), abc(7, 11, 13)]
    "#;
    let result = interpret(input);
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
    let input = r#"
      var called = false
      func getOne(): Int {
        called = true
        1
      }
      func abc(def = getOne()): Int = def
      abc(1)
      called
    "#;
    let result = interpret(input);
    let expected = Value::Bool(false);
    assert_eq!(expected, result);

    let input = r#"
      var called = false
      func getOne(): Int {
        called = true
        1
      }
      func abc(def = getOne()): Int = def
      abc()
      called
    "#;
    let result = interpret(input);
    let expected = Value::Bool(true);
    assert_eq!(expected, result);
}

#[test]
fn interpret_func_invocation_default_args_laziness() {
    let input = r#"
      func getOne(): Int = 1
      func outer(): () => Int {
        func abc(def = getOne(), ghi = def, jkl = def + ghi): Int {
          def + ghi + jkl
        }
        abc
      }
      val fn = outer()
      fn()
    "#;
    let result = interpret(input);
    let expected = Value::Int(4);
    assert_eq!(expected, result);
}

#[test]
fn interpret_func_invocation_generics() {
    let input = r#"
      func map<T, U>(arr: T[], fn: (T) => U, start: U? = None): U[] {
        val newArr: U[] = []

        if start |u| newArr.push(u)

        for i in arr { newArr.push(fn(i)) }
        newArr
      }
      map(
         arr: ["1", "23", "456"],
         fn: s => (s + "!").length,
         start: 17
      )
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![
        Value::Int(17),
        Value::Int(2),
        Value::Int(3),
        Value::Int(4),
    ]);
    assert_eq!(expected, result);
}

#[test]
fn interpret_func_invocation_lexical_ordering() {
    // Test global fns
    let input = r#"
      func abc(): Int = def()
      func def(): Int = 3
      abc()
    "#;
    let result = interpret(input);
    let expected = Value::Int(3);
    assert_eq!(expected, result);

    // Test nested within fn
    let input = r#"
      func abc(): Int {
        func def(): Int = ghi()
        func ghi(): Int = 4
        def()
      }
      abc()
    "#;
    let result = interpret(input);
    let expected = Value::Int(4);
    assert_eq!(expected, result);

    // Test nested within if-expr
    let input = r#"
      val x = if true {
        func def(): Int? {
          if true { abc() }
        }
        func abc(): Int = 123
        def()
      }
      x
    "#;
    let result = interpret(input);
    let expected = Value::Int(123);
    assert_eq!(expected, result);

    // Test very complex example
    let input = r#"
      func abc(): () => Int {
        var count = -1

        func def(): Int {
          count += 1

          func qrs(): Int = fib(count)
          qrs()
        }

        func fib(n: Int): Int {
          if n <= 1 { return 1 }
          fib(n - 1) + fib(n - 2)
        }
        def
      }
      val fn = abc()
      [fn(), fn(), fn(), fn(), fn(), fn(), fn()]
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![
        Value::Int(1),
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
        Value::Int(5),
        Value::Int(8),
        Value::Int(13),
    ]);
    assert_eq!(expected, result);
}

#[test]
fn interpret_invocation_order() {
    // Verify that the value of `total` passed to `concat` is the value after all the `map` lambda invocations
    let input = r#"
      var total = 0
      val arr = [1, 2]
      arr.map(i => {
        total += i
        i * 3
      }).concat([total])
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![Value::Int(3), Value::Int(6), Value::Int(3)]);
    assert_eq!(expected, result);
}

#[test]
fn interpret_invocation_varargs() {
    let input = r#"
      func abc(a: Int, *b: Int[]): String = [a].concat(b).join(",")
      [abc(1), abc(1, 2, 3, 4)]
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![
        new_string_obj("1"),
        new_string_obj("1,2,3,4"),
    ]);
    assert_eq!(expected, result);

    let input = r#"
      func abc(a: Int, *b = [6, 24]): String = [a].concat(b).join(",")
      [abc(1), abc(1, 2, 3, 4)]
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![
        new_string_obj("1,6,24"),
        new_string_obj("1,2,3,4"),
    ]);
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
    let result = interpret(input);
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
    let result = interpret(input);
    let expected = new_string_obj("1,2,F,4,B,F,7,8,F,B,11,F,13,14,Fb,16,17,F,19,B,");
    assert_eq!(expected, result);
}

#[test]
fn interpret_while_loop_with_break() {
    let input = r#"
      var sum = 0
      while true {
        while true {
          sum += 1
          if sum.isEven() { break }
        }
        if sum > 20 { break }
      }
      sum
    "#;
    let result = interpret(input);
    let expected = Value::Int(22);
    assert_eq!(expected, result);

    let input = r#"
      var sum = 0
      while true {
        if sum > 20 { break }
        while true {
          sum += 1
          if sum.isEven() { break }
        }
      }
      sum
    "#;
    let result = interpret(input);
    let expected = Value::Int(22);
    assert_eq!(expected, result);
}

#[test]
fn interpret_while_loop_with_condition_binding() {
    let input = "\
      var str = \"\"\n\
      var idx = 0\n\
      val arr = [1, 2, 3]\n\
      while arr[idx] |item| {\n\
        str = str + item\n\
        idx = idx + 1\n\
      }\n\
      str\
    ";
    let result = interpret(input);
    let expected = new_string_obj("123");
    assert_eq!(expected, result);
}

#[test]
fn interpret_loops_with_break() {
    let input = r#"
      val arr = [1, 2, 3]
      var idx = 0
      while arr[idx] |item| {
        val inc = if item == 2 { break } else 1
        idx += inc
      }
      idx
    "#;
    let result = interpret(input);
    let expected = Value::Int(1);
    assert_eq!(expected, result);

    let input = r#"
      val arr = [1, 2, 3]
      var count = 0
      for item, i in arr {
        val inc = if item == 2 { break } else 1
        count += inc
      }
      count
    "#;
    let result = interpret(input);
    let expected = Value::Int(1);
    assert_eq!(expected, result);
}

#[test]
fn interpret_loops_with_continue() {
    let input = r#"
      val arr = [1, 2, 3]
      var idx = 0
      var count = 0
      while arr[idx] |item| {
        idx += 1
        val inc = if item == 2 { continue } else 1
        count += inc
      }
      count
    "#;
    let result = interpret(input);
    let expected = Value::Int(2);
    assert_eq!(expected, result);

    let input = r#"
      val arr = [1, 2, 3]
      var count = 0
      for item, i in arr {
        val inc = if item == 2 { continue } else 1
        count += inc
      }
      count
    "#;
    let result = interpret(input);
    let expected = Value::Int(2);
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
    let result = interpret(input);
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
    let result = interpret(input);
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
    let result = interpret(input);
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
    let result = interpret(input);
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
    let result = interpret(input);
    let expected = new_string_obj("Ken");
    assert_eq!(expected, result);

    // Test with default value
    let input = "\
      type Person { name: String, age: Int = 0 }\n\
      val aBaby = Person(name: \"Baby\")\n\
      aBaby.age\n\
    ";
    let result = interpret(input);
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
        let result = interpret(input);
        assert_eq!(expected, result);
    }
}

#[test]
fn interpret_accessor_opt_safe() {
    let input = "\
      type Name { value: String? = None }\n\
      type Person { name: Name? = None }\n\
      val ken = Person()\n\
      ken.name?.value?.length\n\
    ";
    let result = interpret(input);
    let expected = Value::Nil;
    assert_eq!(expected, result);

    // Verify that resolution of optional-safe accessors doesn't pollute the stack mid-expr
    let input = "\
      type Name { value: String? = None }\n\
      type Person { name: Name? = None }\n\
      val ken = Person(name: Name(value: \"Ken\"))\n\
      1 + (ken.name?.value?.length ?: 0)\n\
    ";
    let result = interpret(input);
    let expected = Value::Int(4);
    assert_eq!(expected, result);

    let input = "\
      type Name { value: String? = None }\n\
      type Person { name: Name? = None }\n\
      \"1\" + Person().name?.value?.length\n\
    ";
    let result = interpret(input);
    let expected = new_string_obj("1None");
    assert_eq!(expected, result);

    let input = "\
      type Person { name: String? = None }\n\
      val people = [Person(name: \"a\")]\n\
      [\n\
        people[0]?.name?.length,\n\
        people[1]?.name?.length,\n\
      ]\n\
    ";
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![
        Value::Int(1),
        Value::Nil
    ]);
    assert_eq!(expected, result);
}

#[test]
fn interpret_invocation_opt_safe() {
    let input = r#"
      type Person { name: String? = None }
      val ken = Person()
      ken.name?.toLower()
    "#;
    let result = interpret(input);
    let expected = Value::Nil;
    assert_eq!(expected, result);

    let input = r#"
      type Person { name: String? = None }
      val ken = Person(name: "Ken")
      ken.name?.toLower()
    "#;
    let result = interpret(input);
    let expected = new_string_obj("ken");
    assert_eq!(expected, result);

    let input = r#"
      val arr = [[1, 2], [3, 4]]
      func abc() {
        arr[3]?.push(5)
      }
      abc()
      arr
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![
        Value::new_array_obj(vec![Value::Int(1), Value::Int(2)]),
        Value::new_array_obj(vec![Value::Int(3), Value::Int(4)]),
    ]);
    assert_eq!(expected, result);

    let input = r#"
      val arr = [[1, 2], [3, 4]]
      func abc() {
        arr[1]?.push(5)
      }
      abc()
      arr
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![
        Value::new_array_obj(vec![Value::Int(1), Value::Int(2)]),
        Value::new_array_obj(vec![Value::Int(3), Value::Int(4), Value::Int(5)]),
    ]);
    assert_eq!(expected, result);
}

#[test]
fn interpret_method_invocation_struct() {
    let input = "\
      type Person {\n\
        name: String\n\
        func introduce(self): String = \"I am \" + self.name\n\
      }\n\
      val ken = Person(name: \"Ken\")\n\
      val brian = Person(name: \"Brian\")\n\
      ken.introduce() + \", and \" + brian.introduce()\n\
    ";
    let result = interpret(input);
    let expected = new_string_obj("I am Ken, and I am Brian");
    assert_eq!(expected, result);
}

#[test]
fn interpret_method_invocation_preserve_receiver() {
    let input = "\
      type Person {\n\
        name: String\n\
        func introduce(self): String = \"I am \" + self.name\n\
      }\n\
      val ken = Person(name: \"Ken\")\n\
      val introduceFn = ken.introduce\n\
      introduceFn()\
    ";
    let result = interpret(input);
    let expected = new_string_obj("I am Ken");
    assert_eq!(expected, result);
}

#[test]
fn interpret_static_method_invocation() {
    let input = r#"
      type Person {
        name: String
        func introduce(name: String): String = "I am " + name
      }
      val ken = Person(name: "Ken")
      Person.introduce(ken.name)
    "#;
    let result = interpret(input);
    let expected = new_string_obj("I am Ken");
    assert_eq!(expected, result);
}

#[test]
fn interpret_to_string() {
    // Test the default implementation
    let input = r#"
      type Person { name: String }
      val p = Person(name: "Ken")
      p.toString()
    "#;
    let result = interpret(input);
    let expected = new_string_obj("Person(name: Ken)");
    assert_eq!(expected, result);

    // Test when there is an implementation specified
    let input = r#"
      type Person {
        name: String
        func toString(self): String = self.name
      }
      val p = Person(name: "Ken")
      p.toString()
    "#;
    let result = interpret(input);
    let expected = new_string_obj("Ken");
    assert_eq!(expected, result);

    // Test that it gets called by other toString calls
    let input = r#"
      type Person {
        name: String
        func toString(self): String = self.name
      }
      val p = Person(name: "Ken")
      [p].toString()
    "#;
    let result = interpret(input);
    let expected = new_string_obj("[Ken]");
    assert_eq!(expected, result);

    // Test that it remembers its receiver
    let input = r#"
      type Person {
        name: String
      }
      val p = Person(name: "Ken")
      val toString = p.toString
      p.name = "Meg"
      toString()
    "#;
    let result = interpret(input);
    let expected = new_string_obj("Person(name: Meg)");
    assert_eq!(expected, result);
}

#[test]
fn interpret_enum() {
    let input = r#"
      enum Color {
        Red
        Green
        Blue
        RGB(red: Int, green: Int, blue: Int)

        func white(): Color = Color.RGB(red: 255, green: 255, blue: 255)

        func black(): Color = Color.RGB(red: 0, green: 0, blue: 0)

        func hex(self): String {
          match self {
            Color.Red => "0xff0000"
            Color.Green => "0x00ff00"
            Color.Blue => "0x0000ff"
            Color.RGB(red, green, blue) => {
              val hexes = [red, green, blue].map(c => c.asBase(16).padLeft(2, "0"))
              "0x" + hexes.join()
            }
          }
        }
      }

      [
        Color.Red,
        Color.Green,
        Color.Blue,
        Color.black(),
        Color.white(),
        Color.RGB(red: 128, green: 128, blue: 128)
      ].map(c => c.hex())
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![
        new_string_obj("0xff0000"),
        new_string_obj("0x00ff00"),
        new_string_obj("0x0000ff"),
        new_string_obj("0x000000"),
        new_string_obj("0xffffff"),
        new_string_obj("0x808080"),
    ]);
    assert_eq!(expected, result);

    let input = r#"
      enum Color {
        Red
        Green
        Blue
        RGB(red: Int, green: Int, blue: Int)

        func hexCode(self): String {
          if self == Color.Red "0xff0000"
          else if self == Color.Green "0x00ff00"
          else "0x0000ff"
        }
      }
      Color.Red.hexCode()
    "#;
    let result = interpret(input);
    let expected = new_string_obj("0xff0000");
    assert_eq!(expected, result);

    // Test enum constructors with default-valued arguments
    let input = r#"
      enum Color {
        Red,
        Blue,
        Darken(base: Color = Color.Red, amount: Float = 10.0)
        Darken2(base: Color, amount: Float = 10.0)
      }

      [
        Color.Darken(), Color.Darken(Color.Blue), Color.Darken(Color.Blue, 6.24),
        Color.Darken2(Color.Blue), Color.Darken2(Color.Blue, 6.24),
      ].map(c => c.toString())
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![
        new_string_obj("Color.Darken(Color.Red, 10)"),
        new_string_obj("Color.Darken(Color.Blue, 10)"),
        new_string_obj("Color.Darken(Color.Blue, 6.24)"),
        new_string_obj("Color.Darken2(Color.Blue, 10)"),
        new_string_obj("Color.Darken2(Color.Blue, 6.24)"),
    ]);
    assert_eq!(expected, result);
}

#[test]
fn interpret_enum_generic() {
    let input = r#"
      enum LL<T> {
        Cons(item: T, next: LL<T>)
        Empty
      }
      val l = LL.Cons(1, LL.Cons(2, LL.Empty))
      l.toString()
    "#;
    let result = interpret(input);
    let expected = new_string_obj("LL.Cons(1, LL.Cons(2, LL.Empty))");
    assert_eq!(expected, result);
}

#[test]
fn interpret_match_destructuring_enum() {
    let input = r#"
      enum Foo { Bar(baz: Int, qux: Int) }

      val f = Foo.Bar(baz: 6, qux: 24)
      val f2 = match f {
        Foo.Bar(baz, qux) => Foo.Bar(qux, baz)
      }
      f2.toString()
    "#;
    let result = interpret(input);
    let expected = new_string_obj("Foo.Bar(24, 6)");
    assert_eq!(expected, result);
}

#[test]
fn interpret_linked_list_kinda() {
    // Verify self-referential types, as well as the usage of static methods in default field values
    let input = r#"
      type Node {
        value: String
        next: Node? = Node.empty()

        func empty(): Node? = None

        func toString(self): String {
          var str = self.value + ", "
          var next = self.next

          while next {
            str = str + (next?.value ?: "") + ", "

            next = next?.next
          }

          str[:-2]
        }
      }

      val node = Node(value: "a", next: Node(value: "b", next: Node(value: "c")))
      node.toString()
    "#;
    let result = interpret(input);
    let expected = new_string_obj("a, b, c");
    assert_eq!(expected, result);
}

#[test]
fn interpret_linked_list_even_closer() {
    // Verify self-referential types, as well as the usage of condition bindings for while-loops
    // and if-stmts/exprs.
    let input = r#"
      type Node {
        value: String
        next: Node? = None
      }

      type LinkedList {
        count: Int = 0
        head: Node? = None

        func push(self, item: String): LinkedList { // <- Verify that methods can return instances of Self
          if self.head |head| {
            var node = head
            while node.next |n| { node = n }
            node.next = Node(value: item)
          } else {
            self.head = Node(value: item)
          }

          self
        }

        func toString(self): String {
          if self.head |head| {
            var str = ""
            var node = head

            while node.next |n| {
              str = str + node.value + ", "
              node = n
            }

            str + node.value
          } else {
            "[]"
          }
        }
      }

      val list = LinkedList()
      list.push("a")
        .push("b")
        .push("c")
        .push("d")
        .toString()
    "#;
    let result = interpret(input);
    let expected = new_string_obj("a, b, c, d");
    assert_eq!(expected, result);
}

#[test]
fn interpret_structs_generics() {
    let input = r#"
      type List<T> {
        items: T[] = []

        func push(self, item: T) { self.items.push(item) }

        func map<U>(self, fn: (T) => U): U[] {
          val newArr: U[] = []
          for item in self.items
            newArr.push(fn(item))
          newArr
        }

        func reduce<U>(self, initialValue: U, fn: (U, T) => U): U {
          var acc = initialValue
          for item in self.items
            acc = fn(acc, item)
          acc
        }

        func concat(self, other: List<T>): List<T> {
          val items = self.items
          List(items: items.concat(other.items))
        }
      }

      func sum(list: Int[]): Int = list.reduce(0, (acc, i) => acc + i)

      var list: List<Int> = List(items: [])
      for n in range(1, 500) { list.push(n) }
      list = list.concat(List(items: range(500, 1000)))
      list = List(items: list.map(i => i * 5))
      val nums = list.reduce<Int[]>([], (acc, i) => {
        acc.push(i)
        acc
      })
      sum(nums)
    "#;
    let result = interpret(input);
    let expected = Value::Int(2497500);
    assert_eq!(expected, result);
}

#[test]
fn interpret_lambdas() {
    let input = r#"
      func call(fn: (Int) => Int, value: Int): Int = fn(value)
      call(x => x + 1, 23)
    "#;
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);

    let input = r#"
      func call(fn: (Int) => Int, value: Int): Int = fn(value)
      call((x, y = 1) => x + y + 1, 22)
    "#;
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);

    let input = r#"
      func getAdder(x: Int): (Int) => Int {
        (y, z = 3) => x + y + z
      }
      getAdder(20)(1)
    "#;
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);
}

#[test]
fn interpret_lambdas_closing_over_block_bindings() {
    let input = "\
      var f: () => Int\n\
      if true {\n\
        val x = 24\n\
        f = () => x\n\
      } else {\n\
        val y = 12\n\
        f = () => y\n\
      }\n\
      f()\
    ";
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);

    let input = "\
      var f: () => Int\n\
      while true {\n\
        val x = 24\n\
        f = () => x\n\
        break\n\
      }\n\
      f()\
    ";
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);
}

#[test]
fn interpret_match_expressions_and_statements() {
    let input = r#"
      var r = 0
      val s: (String | Int)? = "woo"
      match s {
        None => r = 0
        _ s => r = 1
      }
      r
    "#;
    let result = interpret(input);
    let expected = Value::Int(1);
    assert_eq!(expected, result);

    let input = r#"
      val s: (String | Int)? = "woo"
      val r = match s {
        None => 0
        _ s => 1
      }
      r
    "#;
    let result = interpret(input);
    let expected = Value::Int(1);
    assert_eq!(expected, result);

    let input = r#"
      type Person { name: String }
      enum AnimalKind { Dog, Cat }
      type Animal { kind: AnimalKind }
      val v: Person | Animal = Person(name: "Meg")
      val r = match v {
        Person p => p.name.length
        Animal a => 0
      }
      r
    "#;
    let result = interpret(input);
    let expected = Value::Int(3);
    assert_eq!(expected, result);

    let input = r#"
      func len(v: (String | Int)?): Int {
        match v {
          Int i => (""+i).length
          String s => s.length
          _ => 0
        }
      }
      len(12340 + 5)
    "#;
    let result = interpret(input);
    let expected = Value::Int(5);
    assert_eq!(expected, result);
}

#[test]
fn interpret_match_expressions_constants() {
    let input = r#"
      val a = match "foobar" {
        "asdf" => 1
        "hello" => 2
        _ => 3
      }
      val b = match (12 + 34) {
        24 => 10
        46 => 20
        _ => 30
      }
      val c = match (1 < 2) {
        true => 100
        false => 200
        _ => 300
      }
      val d = match ("a", 6, true) {
        ("a", 12, false) => 1000
        ("b", 24, true) => 2000
        ("c", 48, false) => 3000
        _ => 4000
      }
      a + b + c + d
    "#;
    let result = interpret(input);
    let expected = Value::Int(4123);
    assert_eq!(expected, result);

    let input = r#"
      enum Foo { Bar(baz: String, qux: Int) }
      func abc(foo: Foo): Int {
        match foo {
          Foo.Bar("asdf", 12) => 1
          Foo.Bar("asdf", q) => 2
          Foo.Bar(b, 12) => 3
          Foo.Bar(b, q) => b.length + q
        }
      }

      [
        abc(Foo.Bar("asdf", 24)), // => 2
        abc(Foo.Bar("zxcv", 12)), // => 3
        abc(Foo.Bar("asdf", 12)), // => 1
        abc(Foo.Bar("zxcv", 0)),  // => 4
      ].join(",")
    "#;
    let result = interpret(input);
    let expected = new_string_obj("2,3,1,4");
    assert_eq!(expected, result);
}

#[test]
fn interpret_recursive_func_in_lambda() {
    // This is an utterly pointless, super contrived example, but the main test case here is
    // whether a non-root-scope function will be correctly recognized as recursive if its only
    // usage is within a lambda
    let input = r#"
      func abc(): Int {
        func def(nums: Int[]): Int {
          if nums[0] |n| {
            n + nums[1:].reduce(0, (acc, i) => acc + def([i]))
          } else {
            0
          }
        }

        def([1, 2, 3, 4])
      }
      abc()
    "#;
    let result = interpret(input);
    let expected = Value::Int(10);
    assert_eq!(expected, result);
}

#[test]
fn interpret_u16_jump_offsets() {
    let input = r#"
      var total = 0
      for i in range(0, 1) {
        total += if true && true && true && true {
          val a = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          val b = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          val c = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          val d = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          val e = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          val f = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          a + b + c + d + e + f
        } else {
          val a = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          val b = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          val c = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          val d = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          val e = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          val f = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
          a + b + c + d + e + f
        }
      }
      total
    "#;
    let result = interpret(input);
    let expected = Value::Int(396);
    assert_eq!(expected, result);
}

#[test]
fn interpret_return_statements() {
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
      contains(arr, 4)
    "#;
    let result = interpret(input);
    let expected = Value::Bool(true);
    assert_eq!(expected, result);

    let input = r#"
      func f(): Int {
        if true {
          val x = 12
          return 24
        }
        return 6
      }
      f()
    "#;
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);

    let input = r#"
      func f(): Int {
        while true {
          val x = 12
          return 24
        }
      }
      f()
    "#;
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);

    let input = r#"
      func f(): Int {
        for _ in [1, 2, 3] {
          val x = 12
          return 24
        }
      }
      f()
    "#;
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);

    let input = r#"
      func f(): Int {
        match "a" {
          _ => {
            val x = 12
            return 24
          }
        }
      }
      f()
    "#;
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);
}

#[test]
fn interpret_destructuring_assignment() {
    let input = r#"
      val (a, b, c) = (1, 2, 3)
      a + b + c
    "#;
    let result = interpret(input);
    let expected = Value::Int(6);
    assert_eq!(expected, result);

    let input = r#"
      val ((a, b), c) = ((1, 2), (3, 4))
      a + b + c[0] + c[1]
    "#;
    let result = interpret(input);
    let expected = Value::Int(10);
    assert_eq!(expected, result);

    let input = r#"
      val [(x1, y1), (x2, y2), (x3, y3)] = [(1, 2), (3, 4)]
      [x1, y1, x2, y2, x3, y3]
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
        Value::Int(4),
        Value::Nil,
        Value::Nil,
    ]);
    assert_eq!(expected, result);

    let input = r#"
      val [(x1, y1), *mid, (x2, y2)] = [(1, 2), (3, 4), (5, 6), (7, 8)]
      (x1, y1, mid, x2, y2)
    "#;
    let result = interpret(input);
    let expected = Value::new_tuple_obj(vec![
        Value::Int(1),
        Value::Int(2),
        Value::new_array_obj(vec![
            Value::new_tuple_obj(vec![Value::Int(3), Value::Int(4)]),
            Value::new_tuple_obj(vec![Value::Int(5), Value::Int(6)]),
        ]),
        Value::Int(7),
        Value::Int(8),
    ]);
    assert_eq!(expected, result);

    let input = r#"
      val [h1, h2, *mid, t1, t2, t3, t4, t5] = [1, 2, 3, 4, 5, 6]
      (h1, h2, mid, t1, t2, t3, t4, t5)
    "#;
    let result = interpret(input);
    let expected = Value::new_tuple_obj(vec![
        Value::Int(1),
        Value::Int(2),
        Value::new_array_obj(vec![]),
        Value::Int(3),
        Value::Int(4),
        Value::Int(5),
        Value::Int(6),
        Value::Nil,
    ]);
    assert_eq!(expected, result);

    let input = r#"
      val [head, *tail] = [1, 2, 3]
      tail
    "#;
    let result = interpret(input);
    let expected = Value::new_array_obj(vec![Value::Int(2), Value::Int(3)]);
    assert_eq!(expected, result);

    let input = r#"
      val [[a], [d], [g, *h]] = ["abc", "def", "ghi"]
      (a, d, g, h)
    "#;
    let result = interpret(input);
    let expected = Value::new_tuple_obj(vec![
        new_string_obj("a"),
        new_string_obj("d"),
        new_string_obj("g"),
        new_string_obj("hi"),
    ]);
    assert_eq!(expected, result);

    let input = r#"
      func wrapper(): Int {
        val ((a, b), c) = ((1, 2), (3, 4))
        a + b + c[0] + c[1]
      }
      wrapper()
    "#;
    let result = interpret(input);
    let expected = Value::Int(10);
    assert_eq!(expected, result);
}

#[test]
fn interpret_destructuring_for_loop() {
    let input = r#"
      val coords = [(1, 2), (3, 4), (5, 6)]
      var total = 0
      for (x, y), i in coords {
        total += (x + y + i)
      }
      total
    "#;
    let result = interpret(input);
    let expected = Value::Int(24);
    assert_eq!(expected, result);

    let input = r#"
      val arrays = [[1], [2, 3], [4, 5, 6]]
      var total = 0
      for [_, *r], i in arrays {
        total += (r.length + i)
      }
      total
    "#;
    let result = interpret(input);
    let expected = Value::Int(6);
    assert_eq!(expected, result);
}

#[test]
fn interpret_destructuring_match_expr() {
    let input = r#"
      enum Point { TwoD(coord: (Int, Int)), ThreeD(coord: (Int, Int, Int)) }

      val p: Point = Point.ThreeD(coord: (1, 2, 3))
      val s = match p {
        Point.TwoD((x, y)) => x + y
        Point.ThreeD((x, y, z)) => x + y + z
      }
      s
    "#;
    let result = interpret(input);
    let expected = Value::Int(6);
    assert_eq!(expected, result);
}

#[test]
fn interpret_destructuring_if_expr() {
    let input = r#"
      val p: (Int, Int)? = (1, 2)
      val r = if p |(x, y)| {
        x + y
      } else { 0 }
      r
    "#;
    let result = interpret(input);
    let expected = Value::Int(3);
    assert_eq!(expected, result);

    let input = r#"
      val a: Int[]? = [1, 2, 3, 4, 5]
      val r = if a |[h, *r]| {
        r.length
      } else { 0 }
      r
    "#;
    let result = interpret(input);
    let expected = Value::Int(4);
    assert_eq!(expected, result);
}

#[test]
fn interpret_imports() {
    // Importing types
    let mod1 = r#"
      import Person from "./person"
      Person(name: "Ken").name
    "#;
    let modules = vec![
        ("./person", "export type Person { name: String }"),
    ];
    let chunk = interpret_with_modules(mod1, modules);
    assert_eq!(new_string_obj("Ken"), chunk);

    // Importing enums
    let mod1 = r#"
      import Direction from "./direction"
      "${Direction.Up}"
    "#;
    let modules = vec![
        ("./direction", "export enum Direction { Up, Down }"),
    ];
    let chunk = interpret_with_modules(mod1, modules);
    assert_eq!(new_string_obj("Direction.Up"), chunk);

    // Importing bindings
    let mod1 = r#"
      import x from "./constants"
      x + 4
    "#;
    let modules = vec![
        ("./constants", "export val x = 123"),
    ];
    let chunk = interpret_with_modules(mod1, modules);
    assert_eq!(Value::Int(127), chunk);

    // Import aliases
    let mod1 = r#"
      import "./constants" as C
      C.x + 4
    "#;
    let modules = vec![
        ("./constants", "export val x = 123"),
    ];
    let chunk = interpret_with_modules(mod1, modules);
    assert_eq!(Value::Int(127), chunk);

    // Import aliases, use types & enums
    let mod1 = r#"
      import "./mod" as m
      val b1 = m.Baz(foo: m.Foo.Bar, bar: 24)
      val b2 = m.Baz(foo: m.Foo.Bar, bar: 12)
      val r = if b1.foo == m.Foo.Bar {
        b1.bar
      } else {
        b2.bar
      }
      r
    "#;
    let modules = vec![
        ("./mod", "export enum Foo { Bar }\nexport type Baz { foo: Foo, bar: Int }"),
    ];
    let chunk = interpret_with_modules(mod1, modules);
    assert_eq!(Value::Int(24), chunk);
}

#[test]
fn interpret_imports_modifying_globals() {
    let mod1 = r#"
      import addName, names from "./names"
      addName("Ken")
      addName("Meg")
      names
    "#;
    let modules = vec![
        ("./names", r#"
          export val names: String[] = []
          export func addName(name: String) = names.push(name)
        "#),
    ];
    let chunk = interpret_with_modules(mod1, modules);
    let expected = Value::new_array_obj(vec![
        new_string_obj("Ken"),
        new_string_obj("Meg"),
    ]);
    assert_eq!(expected, chunk);

    // Bindings are scoped to modules
    let mod1 = r#"
      import addName, getNames from "./names"
      val names: String[] = []
      addName("Ken")
      addName("Meg")
      [getNames(), names]
    "#;
    let modules = vec![
        ("./names", r#"
          export val names: String[] = []
          export func addName(name: String) = names.push(name)
          export func getNames(): String[] = names
        "#),
    ];
    let chunk = interpret_with_modules(mod1, modules);
    let expected = Value::new_array_obj(vec![
        Value::new_array_obj(vec![
            new_string_obj("Ken"),
            new_string_obj("Meg"),
        ]),
        Value::new_array_obj(vec![])
    ]);
    assert_eq!(expected, chunk);
}

#[test]
fn interpret_boolean_operations_on_optionals() {
    let chunk = interpret(r#"
      val bools = [true, false]
      val a = if bools[0] { 1 } else { 0 }
      val b = if bools[1] { 1 } else { 0 }
      a + b
    "#);
    let expected = Value::Int(1);
    assert_eq!(expected, chunk);

    let chunk = interpret(r#"
      val bools = [true, true, false, true]
      var idx = 0
      while bools[idx] {
        idx += 1
      }
      idx
    "#);
    let expected = Value::Int(2);
    assert_eq!(expected, chunk);
}

#[test]
fn interpret_try_expression() {
    // Test with Ok value
    let chunk = interpret(r#"
      func foo(): Result<Int, String> = Result.Ok(23)
      func bar(): Result<Int, String> {
        val i = try foo()
        Result.Ok(i + 1)
      }
      bar().getValue()
    "#);
    let expected = Value::Int(24);
    assert_eq!(expected, chunk);

    // Test with Err value
    let chunk = interpret(r#"
      func foo(): Result<Int, String> = Result.Err("asdf")
      func bar(): Result<Int, String> {
        val i = try foo()
        Result.Ok(i + 1)
      }
      bar().getError()
    "#);
    let expected = new_string_obj("asdf");
    assert_eq!(expected, chunk);
}
