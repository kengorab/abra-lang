use abra_native::{AbraType, abra_methods};
use std::fmt::Debug;
use std::hash::Hash;
use crate::builtins::arguments::Arguments;
use crate::vm::value::Value;
use crate::vm::vm::VM;
use crate::builtins::common::invoke_fn;

#[derive(AbraType, Debug, Clone, Eq, Hash, PartialEq)]
#[abra_type(module = "prelude", signature = "Result<V, E>", is_enum = true)]
pub struct NativeResult {
    value: Value,
    is_ok: bool,
}

#[abra_methods]
impl NativeResult {
    #[abra_enum_variant(signature = "Ok(value: V)", return_type = "Result<V, Placeholder>")]
    fn ok(mut args: Arguments) -> Self {
        let value = args.next_value();
        Self { value, is_ok: true }
    }

    #[abra_enum_variant(signature = "Err(error: E)", return_type = "Result<Placeholder, E>")]
    fn err(mut args: Arguments) -> Self {
        let value = args.next_value();
        Self { value, is_ok: false }
    }

    #[abra_enum_variant_data]
    fn variant_data(&self) -> (usize, Vec<&Value>) {
        let idx = if self.is_ok { 0 } else { 1 };
        (idx, vec![&self.value])
    }

    #[abra_static_method(signature = "all<V1, E1>(results: Result<V1, E1>[]): Result<V1[], E1>")]
    fn all(mut args: Arguments) -> Self {
        let results = args.next_array();

        let mut value = Vec::new();
        for r in results {
            if let Value::NativeEnumInstanceObj(o) = r {
                let rcv = &*o.borrow();
                let inst: &Self = rcv.inst.downcast_ref::<Self>().unwrap();
                if inst.is_ok {
                    value.push(inst.value.clone());
                } else {
                    return Self { value: inst.value.clone(), is_ok: false };
                }
            } else { unreachable!() }
        }

        Self { value: Value::new_array_obj(value), is_ok: true }
    }

    #[abra_method(signature = "getValue(): V?")]
    fn get_value(&self) -> Value {
        if !self.is_ok {
            return Value::Nil;
        }

        self.value.clone()
    }

    #[abra_method(signature = "getError(): E?")]
    fn get_error(&self) -> Value {
        if self.is_ok {
            return Value::Nil;
        }

        self.value.clone()
    }

    #[abra_method(signature = "map<T>(fn: (V) => T): Result<T, E>")]
    fn map(&self, mut args: Arguments, vm: &mut VM) -> Self {
        let callback = args.next_value();

        if !self.is_ok {
            return self.clone();
        }

        let value = invoke_fn(vm, &callback, vec![self.value.clone()]);
        Self { value, is_ok: true }
    }

    #[abra_method(signature = "mapErr<T>(fn: (E) => T): Result<V, T>")]
    fn map_err(&self, mut args: Arguments, vm: &mut VM) -> Self {
        let callback = args.next_value();

        if self.is_ok {
            return self.clone();
        }

        let value = invoke_fn(vm, &callback, vec![self.value.clone()]);
        Self { value, is_ok: false }
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::test_utils::{interpret, new_string_obj};
    use crate::vm::value::Value;
    use crate::builtins::prelude::native_result::NativeResult;

    #[test]
    fn test_result_ok_variant() {
        let result = interpret("Result.Ok(\"asdf\")");
        if let Value::NativeEnumInstanceObj(o) = result {
            let expected = NativeResult { value: new_string_obj("asdf"), is_ok: true };
            assert_eq!(o.borrow().inst.downcast_ref::<NativeResult>().unwrap(), &expected);
        } else {
            panic!("fail")
        }
    }

    #[test]
    fn test_result_err_variant() {
        let result = interpret("Result.Err(\"asdf\")");
        if let Value::NativeEnumInstanceObj(o) = result {
            let expected = NativeResult { value: new_string_obj("asdf"), is_ok: false };
            assert_eq!(o.borrow().inst.downcast_ref::<NativeResult>().unwrap(), &expected);
        } else {
            panic!("fail")
        }
    }

    #[test]
    fn test_result_matching() {
        let result = interpret(r#"
          val r = Result.Ok("asdf")
          val s = match r {
            Result.Ok(v) => v.toUpper(),
            Result.Err(e) => "hmmm"
          }
          s
        "#);
        assert_eq!(new_string_obj("ASDF"), result);

        let result = interpret(r#"
          val r = Result.Err("qwer")
          val s = match r {
            Result.Ok(v) => "hmmm"
            Result.Err(e) => e.toUpper()
          }
          s
        "#);
        assert_eq!(new_string_obj("QWER"), result);
    }

    #[test]
    fn test_result_static_all() {
        let result = interpret(r#"
          val results = [Result.Ok("asdf"), Result.Ok("qwer")]
          Result.all(results).getValue()
        "#);
        assert_eq!(array![new_string_obj("asdf"), new_string_obj("qwer")], result);

        let result = interpret(r#"
          val results = [Result.Ok("asdf"), Result.Ok("qwer"), Result.Err(123), Result.Ok("zxcv")]
          Result.all(results)
        "#);
        if let Value::NativeEnumInstanceObj(o) = result {
            let expected = NativeResult { value: Value::Int(123), is_ok: false };
            assert_eq!(o.borrow().inst.downcast_ref::<NativeResult>().unwrap(), &expected);
        } else {
            panic!("fail")
        }
    }

    #[test]
    fn test_result_get_value() {
        let result = interpret("Result.Ok(\"asdf\").getValue()");
        assert_eq!(new_string_obj("asdf"), result);

        let result = interpret("Result.Err(\"asdf\").getValue()");
        assert_eq!(Value::Nil, result);
    }

    #[test]
    fn test_result_get_error() {
        let result = interpret("Result.Ok(\"asdf\").getError()");
        assert_eq!(Value::Nil, result);

        let result = interpret("Result.Err(\"asdf\").getError()");
        assert_eq!(new_string_obj("asdf"), result);
    }

    #[test]
    fn test_result_map() {
        // Verify `map` transforms Ok value
        let result = interpret(r#"
          val r = Result.Ok("asdf")
          r.map(v => v.toUpper())
        "#);
        if let Value::NativeEnumInstanceObj(o) = result {
            let expected = NativeResult { value: new_string_obj("ASDF"), is_ok: true };
            assert_eq!(o.borrow().inst.downcast_ref::<NativeResult>().unwrap(), &expected);
        } else {
            panic!("fail")
        }

        // Verify `map` does nothing to Err value
        let result = interpret(r#"
          val r = Result.Err("asdf")
          r.map(v => "hmm")
        "#);
        if let Value::NativeEnumInstanceObj(o) = result {
            let expected = NativeResult { value: new_string_obj("asdf"), is_ok: false };
            assert_eq!(o.borrow().inst.downcast_ref::<NativeResult>().unwrap(), &expected);
        } else {
            panic!("fail")
        }
    }

    #[test]
    fn test_result_map_err() {
        // Verify `mapErr` transforms Err value
        let result = interpret(r#"
          val r = Result.Err("asdf")
          r.mapErr(v => v.toUpper())
        "#);
        if let Value::NativeEnumInstanceObj(o) = result {
            let expected = NativeResult { value: new_string_obj("ASDF"), is_ok: false };
            assert_eq!(o.borrow().inst.downcast_ref::<NativeResult>().unwrap(), &expected);
        } else {
            panic!("fail")
        }

        // Verify `mapErr` does nothing to Ok value
        let result = interpret(r#"
          val r = Result.Ok("asdf")
          r.mapErr(v => "hmm")
        "#);
        if let Value::NativeEnumInstanceObj(o) = result {
            let expected = NativeResult { value: new_string_obj("asdf"), is_ok: true };
            assert_eq!(o.borrow().inst.downcast_ref::<NativeResult>().unwrap(), &expected);
        } else {
            panic!("fail")
        }
    }
}
