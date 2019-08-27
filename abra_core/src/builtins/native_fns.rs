use crate::typechecker::types::Type;
use crate::vm::value::{Value, Obj};
use crate::vm::vm::VMContext;
use std::collections::HashMap;
use std::slice::Iter;

// Native functions must return a Value, even if they're of return type Unit.
// If their return type is Unit, they should return None//Value::Nil.
type NativeAbraFn = fn(VMContext, Vec<Value>) -> Option<Value>;

#[derive(PartialEq, Eq, Hash)]
pub struct NativeFn {
    pub name: String,
    pub args: Vec<Type>,
    pub return_type: Type,
    pub native_fn: NativeAbraFn,
}

lazy_static! {
    pub static ref NATIVE_FNS: Vec<NativeFn> = native_fns();
    pub static ref NATIVE_FNS_MAP: HashMap<String, &'static NativeAbraFn> = native_fns_map();
}

fn native_fns_map() -> HashMap<String, &'static NativeAbraFn> {
    let native_fns: Iter<NativeFn> = NATIVE_FNS.iter();

    let mut map = HashMap::new();
    for NativeFn { name, native_fn, .. } in native_fns {
        map.insert(name.clone(), native_fn);
    }

    map
}

fn native_fns() -> Vec<NativeFn> {
    let mut native_fns = Vec::new();

    native_fns.push(NativeFn {
        name: "println".to_string(),
        args: vec![Type::Any],
        return_type: Type::Unit,
        native_fn: println,
    });

    native_fns.push(NativeFn {
        name: "range".to_string(),
        args: vec![Type::Int, Type::Int],
        return_type: Type::Array(Box::new(Type::Int)),
        native_fn: range,
    });

    native_fns.push(NativeFn {
        name: "arrayLen".to_string(),
        args: vec![Type::Array(Box::new(Type::Any))],
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
    let incr = if args.len() == 3 {
        if let Some(Value::Int(i)) = args.get(2) {
            *i
        } else {
            panic!("range requires an Int? as third argument")
        }
    } else { 1 };

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
        let arr = range(ctx.clone(), vec![Value::Int(0), Value::Int(5)]);
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
        let arr = range(ctx.clone(), vec![Value::Int(0), Value::Int(1)]);
        let expected = Some(Value::Obj(Obj::ArrayObj { value: vec![Box::new(Value::Int(0))] }));
        assert_eq!(expected, arr);

        // Test w/ [0, 0) -> Empty array
        let arr = range(ctx.clone(), vec![Value::Int(0), Value::Int(0)]);
        let expected = Some(Value::Obj(Obj::ArrayObj { value: vec![] }));
        assert_eq!(expected, arr);
    }
}
