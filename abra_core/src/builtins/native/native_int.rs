use crate::builtins::gen_native_types::NativeIntMethodsAndFields;
use crate::vm::value::Value;
use crate::vm::vm::VM;
use crate::builtins::native::common::to_string;

pub type NativeInt = crate::builtins::gen_native_types::NativeInt;

impl NativeIntMethodsAndFields for crate::builtins::gen_native_types::NativeInt {
    fn method_to_string(receiver: Option<Value>, _args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        if let Some(obj) = receiver {
            Some(Value::new_string_obj(to_string(&obj, vm)))
        } else { unreachable!() }
    }

    fn method_abs(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Int(i) = receiver.unwrap() {
            Some(Value::Int(i.abs()))
        } else { unimplemented!() }
    }

    fn method_as_base(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let base = args.into_iter().next().expect("Int::asBase requires 1 argument");
        let base = if let Value::Int(base) = base { base } else { unreachable!() };

        if let Value::Int(i) = receiver.unwrap() {
            if base <= 1 || base >= 37 || i <= 0 {
                return Some(Value::new_string_obj(i.to_string()));
            }

            let base = base as u32;
            let mut i = i as u32;
            let mut digits = Vec::new();
            while i > 0 {
                if let Some(ch) = std::char::from_digit(i % base, base) {
                    digits.push(ch);
                }

                i = i / base;
            }

            let str_val = digits.into_iter().rev().collect::<String>();
            Some(Value::new_string_obj(str_val))
        } else { unimplemented!() }
    }

    fn method_is_even(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Int(i) = receiver.unwrap() {
            Some(Value::Bool(i % 2 == 0))
        } else { unreachable!() }
    }

    fn method_is_odd(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Int(i) = receiver.unwrap() {
            Some(Value::Bool(i % 2 != 0))
        } else { unreachable!() }
    }

    fn method_is_between(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();
        let lower = args.next().expect("Int::isBetween requires 2 arguments");
        let lower = if let Value::Int(lower) = lower { lower } else { unreachable!() };

        let upper = args.next().expect("Int::isBetween requires 2 arguments");
        let upper = if let Value::Int(upper) = upper { upper } else { unreachable!() };

        let is_inclusive = match args.next() {
            None => false,
            Some(val) => match val {
                Value::Bool(val) => val,
                Value::Nil => false,
                _ => unreachable!()
            }
        };

        if let Value::Int(i) = receiver.unwrap() {
            if is_inclusive {
                Some(Value::Bool(lower <= i && i <= upper))
            } else {
                Some(Value::Bool(lower < i && i < upper))
            }
        } else { unreachable!() }
    }
}

#[cfg(test)]
mod test {
    use crate::vm::value::Value;
    use crate::builtins::native::test_utils::{interpret, new_string_obj};

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
