use crate::builtins::gen_native_types::NativeStringMethodsAndFields;
use crate::vm::value::{Value, Obj};
use crate::vm::vm::VM;

macro_rules! obj_as_string {
    ($obj:expr) => {
        if let Value::Obj(obj) = $obj.unwrap() {
            if let Obj::StringObj(value) = &*(obj.borrow()) {
                value.clone()
            } else { unreachable!() }
        } else { unreachable!() }
    };
}

pub type NativeString = crate::builtins::gen_native_types::NativeString;

impl NativeStringMethodsAndFields for crate::builtins::gen_native_types::NativeString {
    fn field_length(obj: Box<Value>) -> Value {
        if let Value::Obj(obj) = *obj {
            match &*(obj.borrow()) {
                Obj::StringObj(value) => Value::Int(value.len() as i64),
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_to_lower(receiver: Option<Value>, _: Vec<Value>, _: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);
        Some(Value::new_string_obj(receiver.to_lowercase()))
    }

    fn method_to_upper(receiver: Option<Value>, _: Vec<Value>, _: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);
        Some(Value::new_string_obj(receiver.to_uppercase()))
    }

    fn method_pad_left(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();

        let amount = args.next().expect("String::padLeft requires 2 arguments");
        let amount = if let Value::Int(amount) = amount { amount } else { unreachable!() };

        if amount <= 0 {
            return receiver;
        }

        let receiver = obj_as_string!(receiver);
        if receiver.len() >= (amount as usize) {
            return Some(Value::new_string_obj(receiver));
        }

        let padding = args.next().expect("String::padLeft requires 2 arguments");
        let padding = if let Value::Obj(obj) = padding {
            match &*(obj.borrow()) {
                Obj::StringObj(value) => value.clone(),
                _ => unreachable!()
            }
        } else { unreachable!() };

        let num_repeats = ((amount as usize) - receiver.len()) / padding.len();
        let padding = padding.repeat(num_repeats);

        Some(Value::new_string_obj(format!("{}{}", padding, receiver)))
    }

    fn method_trim(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);
        Some(Value::new_string_obj(receiver.trim().to_string()))
    }

    fn method_trim_start(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let trim_pattern = match args.into_iter().next() {
            Some(Value::Obj(obj)) => match &*obj.borrow() {
                Obj::StringObj(value) => Some(value.clone()),
                _ => unreachable!()
            },
            _ => None
        };

        let receiver = obj_as_string!(receiver);
        let new_val = if let Some(trim_pattern) = trim_pattern {
            receiver.trim_start_matches(&trim_pattern)
        } else {
            receiver.trim_start()
        };
        Some(Value::new_string_obj(new_val.to_string()))
    }

    fn method_trim_end(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let trim_pattern = match args.into_iter().next() {
            Some(Value::Obj(obj)) => match &*obj.borrow() {
                Obj::StringObj(value) => Some(value.clone()),
                _ => unreachable!()
            },
            _ => None
        };

        let receiver = obj_as_string!(receiver);

        let new_val = if let Some(trim_pattern) = trim_pattern {
            receiver.trim_end_matches(&trim_pattern)
        } else {
            receiver.trim_end()
        };
        Some(Value::new_string_obj(new_val.to_string()))
    }

    fn method_split(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let splitter = args.into_iter().next().expect("String::split requires 1 argument");
        let splitter = match splitter {
            Value::Obj(obj) => match &*obj.borrow() {
                Obj::StringObj(value) => value.clone(),
                _ => unreachable!()
            },
            _ => unreachable!()
        };

        let receiver = obj_as_string!(receiver);
        let items = if splitter.is_empty() {
            receiver.chars()
                .map(|s| Value::new_string_obj(s.to_string()))
                .collect()
        } else {
            receiver.split(&splitter)
                .map(|s| Value::new_string_obj(s.to_string()))
                .collect()
        };

        Some(Value::new_array_obj(items))
    }

    fn method_split_at(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let item = args.into_iter().next().expect("String::splitAt requires 1 argument");
        let index = if let Value::Int(index) = item { index } else { unreachable!() };

        if let Value::Obj(obj) = receiver.unwrap() {
            match *obj.borrow_mut() {
                Obj::StringObj(ref mut string) => {
                    let tuple = if index >= string.len() as i64 {
                        Value::new_tuple_obj(vec![
                            Value::new_string_obj(string.clone()),
                            Value::new_string_obj("".to_string()),
                        ])
                    } else if index < -(string.len() as i64) {
                        Value::new_tuple_obj(vec![
                            Value::new_string_obj("".to_string()),
                            Value::new_string_obj(string.clone()),
                        ])
                    } else {
                        let split_idx = ((string.len() as i64 + index) % string.len() as i64) as usize;
                        let (h1, h2) = string.split_at(split_idx);
                        Value::new_tuple_obj(vec![
                            Value::new_string_obj(h1.to_string()),
                            Value::new_string_obj(h2.to_string()),
                        ])
                    };

                    Some(tuple)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_lines(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);

        let items = receiver.lines()
            .map(|s| Value::new_string_obj(s.to_string()))
            .collect();
        Some(Value::new_array_obj(items))
    }

    fn method_chars(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);
        let items = receiver.chars()
            .map(|s| Value::new_string_obj(s.to_string()))
            .collect();
        Some(Value::new_array_obj(items))
    }

    fn method_parse_int(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let radix = match args.into_iter().next() {
            Some(Value::Int(radix)) => radix,
            _ => 10
        };
        let receiver = obj_as_string!(receiver);
        match i64::from_str_radix(&receiver, radix as u32) {
            Ok(i) => Some(Value::Int(i)),
            Err(_) => Some(Value::Nil)
        }
    }

    fn method_parse_float(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);
        match receiver.parse::<f64>() {
            Ok(f) => Some(Value::Float(f)),
            Err(_) => Some(Value::Nil)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::native::test_utils::{interpret, new_string_obj};
    use crate::vm::value::Value;

    #[test]
    fn test_string_length() {
        let result = interpret("\"asdf qwer\".length");
        let expected = Value::Int(9);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_to_upper() {
        let result = interpret("\"Asdf Qwer\".toUpper()");
        let expected = new_string_obj("ASDF QWER");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_to_lower() {
        let result = interpret("\"aSDF qWER\".toLower()");
        let expected = new_string_obj("asdf qwer");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_pad_left() {
        let result = interpret("\"asdf\".padLeft(7, \"!\")");
        let expected = new_string_obj("!!!asdf");
        assert_eq!(Some(expected), result);

        let result = interpret("\"asdf\".padLeft(4, \"!\")");
        let expected = new_string_obj("asdf");
        assert_eq!(Some(expected), result);

        let result = interpret("\"asdf\".padLeft(-14, \"!\")");
        let expected = new_string_obj("asdf");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_trim() {
        let result = interpret("\"  asdf   \".trim()");
        let expected = new_string_obj("asdf");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_trim_start() {
        let result = interpret("\"  asdf   \".trimStart()");
        let expected = new_string_obj("asdf   ");
        assert_eq!(Some(expected), result);

        let result = interpret("\"!!asdf   \".trimStart(pattern: \"!\")");
        let expected = new_string_obj("asdf   ");
        assert_eq!(Some(expected), result);

        let result = interpret("\"!!!asdf   \".trimStart(\"!!\")");
        let expected = new_string_obj("!asdf   ");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_trim_end() {
        let result = interpret("\"  asdf   \".trimEnd()");
        let expected = new_string_obj("  asdf");
        assert_eq!(Some(expected), result);

        let result = interpret("\"  asdf!!\".trimEnd(pattern: \"!\")");
        let expected = new_string_obj("  asdf");
        assert_eq!(Some(expected), result);

        let result = interpret("\"  asdf!!!\".trimEnd(\"!!\")");
        let expected = new_string_obj("  asdf!");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_split() {
        let result = interpret("\"a s d f\".split(splitter: \" \")");
        let expected = array![
          new_string_obj("a"),
          new_string_obj("s"),
          new_string_obj("d"),
          new_string_obj("f")
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("\"  a  b  c d\".split(\"  \")");
        let expected = array![
          new_string_obj(""),
          new_string_obj("a"),
          new_string_obj("b"),
          new_string_obj("c d")
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("\"asdf\".split(\"qwer\")");
        let expected = array![
          new_string_obj("asdf")
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("\"asdf\".split(\"\")");
        let expected = array![
          new_string_obj("a"),
          new_string_obj("s"),
          new_string_obj("d"),
          new_string_obj("f")
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("\"a\\ns\\nd\\nf\".split(\"\\n\")");
        let expected = array![
          new_string_obj("a"),
          new_string_obj("s"),
          new_string_obj("d"),
          new_string_obj("f")
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_split_at() {
        let result = interpret(r#"
          val arr = "hello!"
          arr.splitAt(0)
        "#);
        let expected = tuple!(
            new_string_obj(""),
            new_string_obj("hello!")
        );
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr = "hello!"
          arr.splitAt(1)
        "#);
        let expected = tuple!(
            new_string_obj("h"),
            new_string_obj("ello!")
        );
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr = "hello!"
          arr.splitAt(-1)
        "#);
        let expected = tuple!(
            new_string_obj("hello"),
            new_string_obj("!")
        );
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr = "hello!"
          arr.splitAt(-8)
        "#);
        let expected = tuple!(
            new_string_obj(""),
            new_string_obj("hello!")
        );
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr = "hello!"
          arr.splitAt(10)
        "#);
        let expected = tuple!(
            new_string_obj("hello!"),
            new_string_obj("")
        );
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_lines() {
        let result = interpret("\"asdf\\nqwer\\nzxcv\".lines()");
        let expected = array![
          new_string_obj("asdf"),
          new_string_obj("qwer"),
          new_string_obj("zxcv")
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_chars() {
        let result = interpret("\"asdf\".chars()");
        let expected = array![
          new_string_obj("a"),
          new_string_obj("s"),
          new_string_obj("d"),
          new_string_obj("f")
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_parse_int() {
        let result = interpret("\"hello\".parseInt()");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);

        let result = interpret("\"123 456\".parseInt()");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);

        let result = interpret("\"123456.7\".parseInt()");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);

        let result = interpret("\"123456\".parseInt()");
        let expected = Value::Int(123456);
        assert_eq!(Some(expected), result);

        let result = interpret("\"-123456\".parseInt()");
        let expected = Value::Int(-123456);
        assert_eq!(Some(expected), result);

        let result = interpret("\"ba55\".parseInt(radix: 16)");
        let expected = Value::Int(47701);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_parse_float() {
        let result = interpret("\"hello\".parseFloat()");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);

        let result = interpret("\"123 456\".parseFloat()");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);

        let result = interpret("\"123456.7\".parseFloat()");
        let expected = Value::Float(123456.7);
        assert_eq!(Some(expected), result);

        let result = interpret("\"-123456.7\".parseFloat()");
        let expected = Value::Float(-123456.7);
        assert_eq!(Some(expected), result);

        let result = interpret("\"123456\".parseFloat()");
        let expected = Value::Float(123456.0);
        assert_eq!(Some(expected), result);

        let result = interpret("\"-123456\".parseFloat()");
        let expected = Value::Float(-123456.0);
        assert_eq!(Some(expected), result);
    }
}
