use abra_native::{AbraType, abra_methods};
use crate::vm::value::Value;
use std::fmt::Debug;
use std::hash::Hash;
use crate::builtins::arguments::Arguments;

#[derive(AbraType, Debug, Clone, Eq, Hash, PartialEq)]
#[abra_type(signature = "Int", pseudotype = true, noconstruct = true)]
pub struct NativeInt;

#[abra_methods]
impl NativeInt {
    #[abra_pseudomethod(signature = "abs(): Int")]
    fn abs(rcv: Value) -> Value {
        Value::Int(rcv.as_int().abs())
    }

    #[abra_pseudomethod(signature = "asBase(base: Int): String")]
    fn _as_base(rcv: Value, mut args: Arguments) -> Value {
        let base = args.next_int();

        let rcv = *rcv.as_int();

        if base <= 1 || base >= 37 || rcv <= 0 {
            return Value::new_string_obj(rcv.to_string());
        }

        let base = base as u32;
        let mut i = rcv as u32;
        let mut digits = Vec::new();
        while i > 0 {
            if let Some(ch) = std::char::from_digit(i % base, base) {
                digits.push(ch);
            }

            i = i / base;
        }

        let str_val = digits.into_iter().rev().collect::<String>();
        Value::new_string_obj(str_val)
    }

    #[abra_pseudomethod(signature = "isEven(): Bool")]
    fn is_even(rcv: Value) -> Value {
        Value::Bool(rcv.as_int() % 2 == 0)
    }

    #[abra_pseudomethod(signature = "isOdd(): Bool")]
    fn is_odd(rcv: Value) -> Value {
        Value::Bool(rcv.as_int() % 2 != 0)
    }

    #[abra_pseudomethod(signature = "isBetween(lower: Int, upper: Int, inclusive?: Bool): Bool")]
    fn is_between(rcv: Value, mut args: Arguments) -> Value {
        let lower = args.next_int();
        let upper = args.next_int();
        let is_inclusive = args.next_bool_or_default(false);

        let rcv = *rcv.as_int();
        if is_inclusive {
            Value::Bool(lower <= rcv && rcv <= upper)
        } else {
            Value::Bool(lower < rcv && rcv < upper)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::vm::value::Value;
    use crate::builtins::test_utils::{interpret, new_string_obj};

    #[test]
    fn test_int_to_string() {
        let result = interpret("24.toString()");
        let expected = new_string_obj("24");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_int_abs() {
        let result = interpret("6.abs()");
        let expected = Value::Int(6);
        assert_eq!(Some(expected), result);

        let result = interpret("(-6).abs()");
        let expected = Value::Int(6);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_int_as_base() {
        let result = interpret("6.asBase(0)");
        let expected = new_string_obj("6");
        assert_eq!(Some(expected), result);

        let result = interpret("6.asBase(1)");
        let expected = new_string_obj("6");
        assert_eq!(Some(expected), result);

        let result = interpret("6.asBase(37)");
        let expected = new_string_obj("6");
        assert_eq!(Some(expected), result);

        let result = interpret("6.asBase(10)");
        let expected = new_string_obj("6");
        assert_eq!(Some(expected), result);

        let result = interpret("24.asBase(8)");
        let expected = new_string_obj("30");
        assert_eq!(Some(expected), result);

        let result = interpret("4040.asBase(16)");
        let expected = new_string_obj("fc8");
        assert_eq!(Some(expected), result);

        let result = interpret("20.asBase(17)");
        let expected = new_string_obj("13");
        assert_eq!(Some(expected), result);

        let result = interpret("24032.asBase(36)");
        let expected = new_string_obj("ijk");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_int_is_even() {
        let result = interpret("0.isEven()");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("6.isEven()");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("(-6).isEven()");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("5.isEven()");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_int_is_odd() {
        let result = interpret("0.isOdd()");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("6.isOdd()");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("(-1).isOdd()");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("1.isOdd()");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_int_is_between() {
        let result = interpret("0.isBetween(0, 5)");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("0.isBetween(lower: 0, upper: 5, inclusive: true)");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("(-1).isBetween(lower: 0, upper: 5, inclusive: true)");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);
    }
}
