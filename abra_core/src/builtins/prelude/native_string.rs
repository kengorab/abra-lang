use abra_native::{AbraType, abra_methods};
use crate::vm::value::Value;
use std::fmt::Debug;
use std::hash::Hash;
use crate::vm::vm::VM;
use itertools::Itertools;
use crate::builtins::arguments::Arguments;
use crate::builtins::common::to_string;

#[derive(AbraType, Debug, Clone, Eq, Hash, PartialEq)]
#[abra_type(signature = "String", noconstruct = true, variant = "StringObj")]
pub struct NativeString {
    pub _inner: String,

    #[abra_field(name = "length", field_type = "Int", readonly)]
    length: usize,
}

#[abra_methods]
impl NativeString {
    // This is the actual constructor function for an abra String object, which lifts a rust String
    // into abra-space. There is no language-level constructor for a string, aside from making a
    // string literal.
    pub(crate) fn create(raw: String) -> Self {
        Self { _inner: raw, length: 0 }
    }

    #[abra_getter(field = "length")]
    fn get_length(&self) -> Value {
        Value::Int(self._inner.len() as i64)
    }

    #[abra_method(signature = "toLower(): String")]
    fn to_lower(&self) -> Self {
        Self::create(self._inner.to_lowercase())
    }

    #[abra_method(signature = "toUpper(): String")]
    fn to_upper(&self) -> Self {
        Self::create(self._inner.to_uppercase())
    }

    #[abra_method(signature = "padLeft(totalSize: Int, padding?: String): String")]
    fn pad_left(&self, mut args: Arguments) -> Self {
        let total_size = args.next_int();
        let padding = args.next_string_or_default("");

        if total_size <= 0 {
            return self.clone();
        }

        if self._inner.len() >= (total_size as usize) {
            return self.clone();
        }

        let num_repeats = ((total_size as usize) - self._inner.len()) / padding.len();
        let padding = padding.repeat(num_repeats);

        Self::create(format!("{}{}", padding, self._inner))
    }

    #[abra_method(signature = "trim(): String")]
    fn trim(&self) -> Self {
        Self::create(self._inner.trim().to_string())
    }

    #[abra_method(signature = "trimStart(pattern?: String): String")]
    fn trim_start(&self, mut args: Arguments) -> Self {
        let pattern = args.next_string_or_default("");

        if pattern.is_empty() {
            Self::create(self._inner.trim_start().to_string())
        } else {
            Self::create(self._inner.trim_start_matches(&pattern).to_string())
        }
    }

    #[abra_method(signature = "trimEnd(pattern?: String): String")]
    fn trim_end(&self, mut args: Arguments) -> Self {
        let pattern = args.next_string_or_default("");

        if pattern.is_empty() {
            Self::create(self._inner.trim_end().to_string())
        } else {
            Self::create(self._inner.trim_end_matches(&pattern).to_string())
        }
    }

    #[abra_method(signature = "split(splitter: String): String[]")]
    fn split(&self, mut args: Arguments) -> Value {
        let splitter = args.next_string_or_default("");

        let items = if splitter.is_empty() {
            self._inner.chars()
                .map(|s| Value::new_string_obj(s.to_string()))
                .collect()
        } else {
            self._inner.split(&splitter)
                .map(|s| Value::new_string_obj(s.to_string()))
                .collect()
        };

        Value::new_array_obj(items)
    }

    #[abra_method(signature = "splitAt(index: Int): (String, String)")]
    fn split_at(&self, mut args: Arguments) -> Value {
        let index = args.next_int();

        let string = &self._inner;
        let (p1, p2) = if index >= string.len() as i64 {
            (self._inner.clone(), "".to_string())
        } else if index < -(string.len() as i64) {
            ("".to_string(), self._inner.clone())
        } else {
            let split_idx = ((string.len() as i64 + index) % string.len() as i64) as usize;
            let (h1, h2) = string.split_at(split_idx);
            (h1.to_string(), h2.to_string())
        };
        let p1 = Value::new_string_obj(p1);
        let p2 = Value::new_string_obj(p2);

        Value::new_tuple_obj(vec![p1, p2])
    }

    #[abra_method(signature = "lines(): String[]")]
    fn lines(&self) -> Value {
        let items = self._inner.lines()
            .map(|s| Value::new_string_obj(s.to_string()))
            .collect();
        Value::new_array_obj(items)
    }

    #[abra_method(signature = "chars(): String[]")]
    fn chars(&self) -> Value {
        let items = self._inner.chars()
            .map(|s| Value::new_string_obj(s.to_string()))
            .collect();
        Value::new_array_obj(items)
    }

    #[abra_method(signature = "parseInt(radix?: Int): Int?")]
    fn parse_int(&self, mut args: Arguments) -> Value {
        let radix = args.next_int_or_default(10) as u32;

        match i64::from_str_radix(&self._inner, radix) {
            Ok(i) => Value::Int(i),
            Err(_) => Value::Nil
        }
    }

    #[abra_method(signature = "parseFloat(): Float?")]
    fn parse_float(&self) -> Value {
        match self._inner.parse::<f64>() {
            Ok(f) => Value::Float(f),
            Err(_) => Value::Nil
        }
    }

    #[abra_method(signature = "concat(str: Any, *others: Any[]): String")]
    fn concat(&self, mut args: Arguments, vm: &mut VM) -> Self {
        let str = args.next_value();
        let others = args.varargs();

        let res = vec![self._inner.clone(), to_string(&str, vm)].into_iter()
            .chain(others.into_iter().map(|v| to_string(&v, vm)))
            .join("");
        Self::create(res)
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::test_utils::{interpret, new_string_obj, interpret_get_result};
    use crate::vm::value::Value;

    #[test]
    fn test_string_length() {
        let result = interpret("\"asdf qwer\".length");
        let expected = Value::Int(9);
        assert_eq!(Some(expected), result);

        // Setting length should produce an error
        let is_err = interpret_get_result("\"asdf\".length = 8").is_err();
        assert!(is_err);
    }

    #[test]
    fn test_string_to_string() {
        let result = interpret("\"hello\".toString()");
        let expected = new_string_obj("hello");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_to_lower() {
        let result = interpret("\"aSDF qWER\".toLower()");
        let expected = new_string_obj("asdf qwer");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_to_upper() {
        let result = interpret("\"Asdf Qwer\".toUpper()");
        let expected = new_string_obj("ASDF QWER");
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

    #[test]
    fn test_string_concat() {
        let result = interpret("\"hello\".concat(\"!\")");
        let expected = new_string_obj("hello!");
        assert_eq!(Some(expected), result);

        let result = interpret("\"hello\".concat(\" \", \"world\", \"!\")");
        let expected = new_string_obj("hello world!");
        assert_eq!(Some(expected), result);

        let result = interpret("\"asdf\".concat(true, [1, 2, 3], {a:1})");
        let expected = new_string_obj("asdftrue[1, 2, 3]{ a: 1 }");
        assert_eq!(Some(expected), result);
    }
}
