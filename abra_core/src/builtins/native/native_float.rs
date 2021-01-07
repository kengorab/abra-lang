use crate::builtins::gen_native_types::NativeFloatMethodsAndFields;
use crate::vm::value::Value;
use crate::vm::vm::VM;
use crate::builtins::native::common::to_string;

pub type NativeFloat = crate::builtins::gen_native_types::NativeFloat;

impl NativeFloatMethodsAndFields for crate::builtins::gen_native_types::NativeFloat {
    fn method_to_string(receiver: Option<Value>, _args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        if let Some(obj) = receiver {
            Some(Value::new_string_obj(to_string(&obj, vm)))
        } else { unreachable!() }
    }

    fn method_floor(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Float(f) = receiver.unwrap() {
            Some(Value::Int(f.floor() as i64))
        } else { unreachable!() }
    }

    fn method_ceil(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Float(f) = receiver.unwrap() {
            Some(Value::Int(f.ceil() as i64))
        } else { unreachable!() }
    }

    fn method_round(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Float(f) = receiver.unwrap() {
            Some(Value::Int(f.round() as i64))
        } else { unreachable!() }
    }

    fn method_with_precision(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let precision = args.into_iter().next().expect("Float::withPrecision requires 1 argument");
        let precision = if let Value::Int(precision) = precision { precision } else { unreachable!() };

        if precision < 0 {
            return receiver;
        } else if precision >= 10 {
            return receiver;
        }

        if let Value::Float(f) = receiver.unwrap() {
            let power = 10_i32.pow(precision as u32);
            let i = (f * (power as f64)).trunc();

            Some(Value::Float(i / (power as f64)))
        } else { unreachable!() }
    }

    fn method_abs(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Float(f) = receiver.unwrap() {
            Some(Value::Float(f.abs()))
        } else { unreachable!() }
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::native::test_utils::{interpret, new_string_obj};
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
