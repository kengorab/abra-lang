use crate::typechecker::types::Type;
use crate::vm::compiler::fn_name_for_arity;
use crate::vm::value::{Value, Obj};
use crate::vm::vm::VMContext;
use std::collections::HashMap;
use std::slice::Iter;

// Native functions must return a Value, even if they're of return type Unit.
// If their return type is Unit, they should return None//Value::Nil.
type NativeAbraFn = fn(VMContext, Vec<Value>) -> Option<Value>;

pub struct NativeFn {
    pub name: String,
    pub args: Vec<(Type, Option<Value>)>,
    pub return_type: Type,
    pub native_fn: NativeAbraFn,
}

impl NativeFn {
    pub fn invoke(&self, ctx: VMContext, mut args: Vec<Value>) -> Option<Value> {
        let func = self.native_fn;

        let first_default_arg_value = args.len();

        let mut arguments = Vec::with_capacity(self.args.len());
        arguments.append(&mut args);
        for (_, val) in self.args.iter().skip(first_default_arg_value) {
            arguments.push(val.as_ref().unwrap().clone())
        }

        func(ctx, arguments)
    }
}

lazy_static! {
    pub static ref NATIVE_FNS: Vec<NativeFn> = native_fns();
    pub static ref NATIVE_FNS_MAP: HashMap<String, &'static NativeFn> = native_fns_map();
}

fn native_fns_map() -> HashMap<String, &'static NativeFn> {
    let native_fns: Iter<NativeFn> = NATIVE_FNS.iter();

    let mut map = HashMap::new();
    for native_fn in native_fns {
        let name = native_fn.name.clone();
        map.insert(name.clone(), native_fn);

        // Insert all of the pseudo-fns for each native function with default args
        let mut num_required_args = 0;
        let mut num_optional_args = 0;
        for (_, default_value) in native_fn.args.iter() {
            match default_value {
                None => num_required_args += 1,
                Some(_) => num_optional_args += 1
            }
        }
        for num_opt in 0..num_optional_args {
            let arity = num_required_args + num_opt;
            let name = fn_name_for_arity(name.clone(), arity);
            map.insert(name, native_fn);
        }
    }

    map
}

fn native_fns() -> Vec<NativeFn> {
    let mut native_fns = Vec::new();

    native_fns.push(NativeFn {
        name: "println".to_string(),
        args: vec![(Type::Any, None)],
        return_type: Type::Unit,
        native_fn: println,
    });

    native_fns.push(NativeFn {
        name: "range".to_string(),
        args: vec![(Type::Int, None), (Type::Int, None), (Type::Int, Some(Value::Int(1)))],
        return_type: Type::Array(Box::new(Type::Int)),
        native_fn: range,
    });

    native_fns.push(NativeFn {
        name: "arrayLen".to_string(),
        args: vec![(Type::Array(Box::new(Type::Any)), None)],
        return_type: Type::Int,
        native_fn: arr_len,
    });

    native_fns
}

fn println(ctx: VMContext, args: Vec<Value>) -> Option<Value> {
    let val = args.first().unwrap();
    let print_fn = ctx.print;
    print_fn(&format!("{}", val.to_string()));
    None
}

fn range(_ctx: VMContext, args: Vec<Value>) -> Option<Value> {
    let mut start = if let Some(Value::Int(i)) = args.get(0) { *i } else {
        panic!("range requires an Int as first argument")
    };
    let end = if let Some(Value::Int(i)) = args.get(1) { *i } else {
        panic!("range requires an Int as second argument")
    };
    let incr = if let Some(Value::Int(i)) = args.get(2) { *i } else {
        panic!("range requires an Int as third argument")
    };

    let size = (end - start).abs() / incr;
    let mut values = Vec::with_capacity(size as usize);

    while start < end {
        values.push(Box::new(Value::Int(start)));
        start += incr;
    }

    Some(Value::Obj(Obj::ArrayObj { value: values }))
}

// TODO: Replace this with a method invocation when Array::length is a thing
fn arr_len(_ctx: VMContext, args: Vec<Value>) -> Option<Value> {
    let val = if let Some(Value::Obj(Obj::ArrayObj { value })) = args.first() {
        value.len()
    } else {
        panic!("arr_len requires an Array as first argument, got {:?}", args.first())
    };
    Some(Value::Int(val as i64))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn range_returning_int_array() {
        let ctx = VMContext::default();

        // Test w/ increment of 1
        let arr = range(ctx.clone(), vec![Value::Int(0), Value::Int(5), Value::Int(1)]);
        let expected = Some(Value::Obj(Obj::ArrayObj {
            value: vec![
                Box::new(Value::Int(0)),
                Box::new(Value::Int(1)),
                Box::new(Value::Int(2)),
                Box::new(Value::Int(3)),
                Box::new(Value::Int(4)),
            ]
        }));
        assert_eq!(expected, arr);

        // Test w/ increment of 2
        let arr = range(ctx.clone(), vec![Value::Int(0), Value::Int(5), Value::Int(2)]);
        let expected = Some(Value::Obj(Obj::ArrayObj {
            value: vec![
                Box::new(Value::Int(0)),
                Box::new(Value::Int(2)),
                Box::new(Value::Int(4)),
            ]
        }));
        assert_eq!(expected, arr);
    }

    #[test]
    fn range_returning_single_element_int_array() {
        let ctx = VMContext::default();

        // Test w/ increment larger than range
        let arr = range(ctx.clone(), vec![Value::Int(0), Value::Int(5), Value::Int(5)]);
        let expected = Some(Value::Obj(Obj::ArrayObj { value: vec![Box::new(Value::Int(0))] }));
        assert_eq!(expected, arr);

        // Test w/ [0, 1)
        let arr = range(ctx.clone(), vec![Value::Int(0), Value::Int(1), Value::Int(1)]);
        let expected = Some(Value::Obj(Obj::ArrayObj { value: vec![Box::new(Value::Int(0))] }));
        assert_eq!(expected, arr);

        // Test w/ [0, 0) -> Empty array
        let arr = range(ctx.clone(), vec![Value::Int(0), Value::Int(0), Value::Int(1)]);
        let expected = Some(Value::Obj(Obj::ArrayObj { value: vec![] }));
        assert_eq!(expected, arr);
    }
}
