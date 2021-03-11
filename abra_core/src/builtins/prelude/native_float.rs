use abra_native::{AbraType, abra_methods};
use crate::vm::value::Value;
use std::fmt::Debug;
use std::hash::Hash;
use crate::builtins::arguments::Arguments;

#[derive(AbraType, Debug, Clone, Eq, Hash, PartialEq)]
#[abra_type(module = "prelude", signature = "Float", pseudotype = true, noconstruct = true)]
pub struct NativeFloat;

#[abra_methods]
impl NativeFloat {
    #[abra_pseudomethod(signature = "floor(): Int")]
    fn floor(rcv: Value) -> Value {
        Value::Int(rcv.as_float().floor() as i64)
    }

    #[abra_pseudomethod(signature = "ceil(): Int")]
    fn ceil(rcv: Value) -> Value {
        Value::Int(rcv.as_float().ceil() as i64)
    }

    #[abra_pseudomethod(signature = "round(): Int")]
    fn round(rcv: Value) -> Value {
        Value::Int(rcv.as_float().round() as i64)
    }

    #[abra_pseudomethod(signature = "withPrecision(precision: Int): Float")]
    fn with_precision(rcv: Value, mut args: Arguments) -> Value {
        let precision = args.next_int();

        if precision < 0 {
            return rcv;
        } else if precision >= 10 {
            return rcv;
        }

        let rcv = rcv.as_float();
        let power = 10_i32.pow(precision as u32);
        let i = (rcv * (power as f64)).trunc();

        Value::Float(i / (power as f64))
    }

    #[abra_pseudomethod(signature = "abs(): Int")]
    fn abs(rcv: Value) -> Value {
        Value::Float(rcv.as_float().abs())
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::test_utils::{interpret, new_string_obj};
    use crate::vm::value::Value;

    #[test]
    fn test_float_to_string() {
        let result = interpret("6.24.toString()");
        let expected = new_string_obj("6.24");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_float_floor() {
        let result = interpret("6.24.floor()");
        let expected = Value::Int(6);
        assert_eq!(Some(expected), result);

        let result = interpret("val f = 6.7\nf.floor()");
        let expected = Value::Int(6);
        assert_eq!(Some(expected), result);

        let result = interpret("val f = -6.7\nf.floor()");
        let expected = Value::Int(-7);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_float_ceil() {
        let result = interpret("6.24.ceil()");
        let expected = Value::Int(7);
        assert_eq!(Some(expected), result);

        let result = interpret("val f = 6.7\nf.ceil()");
        let expected = Value::Int(7);
        assert_eq!(Some(expected), result);

        let result = interpret("val f = -6.7\nf.ceil()");
        let expected = Value::Int(-6);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_float_round() {
        let result = interpret("6.24.round()");
        let expected = Value::Int(6);
        assert_eq!(Some(expected), result);

        let result = interpret("6.75.round()");
        let expected = Value::Int(7);
        assert_eq!(Some(expected), result);

        let result = interpret("(-6.455).round()");
        let expected = Value::Int(-6);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_float_with_precision() {
        let result = interpret("6.12345.withPrecision(0)");
        let expected = Value::Float(6.0);
        assert_eq!(Some(expected), result);

        let result = interpret("6.98765.withPrecision(0)");
        let expected = Value::Float(6.0);
        assert_eq!(Some(expected), result);

        let result = interpret("6.98765.withPrecision(-1)");
        let expected = Value::Float(6.98765);
        assert_eq!(Some(expected), result);

        let result = interpret("1.23456.withPrecision(2)");
        let expected = Value::Float(1.23);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_float_abs() {
        let result = interpret("6.24.abs()");
        let expected = Value::Float(6.24);
        assert_eq!(Some(expected), result);

        let result = interpret("(-6.24).abs()");
        let expected = Value::Float(6.24);
        assert_eq!(Some(expected), result);
    }
}
