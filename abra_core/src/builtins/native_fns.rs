use crate::typechecker::types::Type;
use crate::vm::value::{Value, Obj};
use crate::vm::vm::VMContext;
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;
use std::cell::RefCell;

// Native functions must return a Value, even if they're of return type Unit.
// If their return type is Unit, they should return None//Value::Nil.
pub type NativeAbraFn = fn(&VMContext, &Option<Arc<RefCell<Obj>>>, Vec<Value>) -> Option<Value>;

#[derive(Clone)]
pub struct NativeFn {
    pub name: &'static str,
    pub receiver: Option<Arc<RefCell<Obj>>>,
    pub native_fn: NativeAbraFn,
}

impl Debug for NativeFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeFn {{ name: {}, .. }}", self.name)
    }
}

impl PartialEq for NativeFn {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(other.name)
    }
}

impl PartialOrd for NativeFn {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl NativeFn {
    pub fn invoke(&self, ctx: &VMContext, args: Vec<Value>) -> Option<Value> {
        let func = self.native_fn;
        let receiver = &self.receiver;
        func(ctx, receiver, args)
    }
}

pub struct NativeFnDesc<'a> {
    pub name: &'static str,
    pub args: Vec<(&'static str, &'a Type)>,
    pub opt_args: Vec<(&'static str, &'a Type)>,
    pub return_type: Type,
}

impl NativeFnDesc<'_> {
    pub fn get_fn_type(&self) -> Type {
        let req_args = self.args.iter()
            .map(|(name, typ)| (name.to_string(), typ.clone().clone(), false));
        let opt_args = self.opt_args.iter()
            .map(|(name, typ)| (name.to_string(), typ.clone().clone(), true));
        let args = req_args.chain(opt_args).collect();

        Type::Fn(None, args, Box::new(self.return_type.clone()))
    }
}

pub fn native_fns() -> Vec<(NativeFnDesc<'static>, NativeFn)> {
    let mut native_fns = Vec::new();

    native_fns.push((
        NativeFnDesc {
            name: "println",
            args: vec![("_", &Type::Any)],
            opt_args: vec![],
            return_type: Type::Unit,
        },
        NativeFn {
            name: "println",
            receiver: None,
            native_fn: println,
        }));

    native_fns.push((
        NativeFnDesc {
            name: "range",
            args: vec![("from", &Type::Int), ("to", &Type::Int)],
            opt_args: vec![("increment", &Type::Int)],
            return_type: Type::Array(Box::new(Type::Int)),
        },
        NativeFn {
            name: "range",
            receiver: None,
            native_fn: range,
        }));

    native_fns
}

fn println(ctx: &VMContext, _receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>) -> Option<Value> {
    let val = args.first().unwrap();
    let print_fn = ctx.print;
    print_fn(&format!("{}", val.to_string()));
    None
}

fn range(_ctx: &VMContext, _receiver: &Option<Arc<RefCell<Obj>>>, args: Vec<Value>) -> Option<Value> {
    let mut start = if let Some(Value::Int(i)) = args.get(0) { *i } else {
        panic!("range requires an Int as first argument")
    };
    let end = if let Some(Value::Int(i)) = args.get(1) { *i } else {
        panic!("range requires an Int as second argument")
    };
    let incr = match args.get(2) {
        None | Some(Value::Nil) => 1,
        Some(Value::Int(i)) => *i,
        Some(_) => panic!("range requires an Int as third argument")
    };

    let size = (end - start).abs() / incr;
    let mut values = Vec::with_capacity(size as usize);

    while start < end {
        values.push(Value::Int(start));
        start += incr;
    }

    Some(Value::new_array_obj(values))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn range_returning_int_array() {
        let ctx = VMContext::default();

        // Test w/ increment of 1
        let arr = range(&ctx, &None, vec![Value::Int(0), Value::Int(5), Value::Int(1)]);
        let expected = Some(Value::new_array_obj(vec![
            Value::Int(0),
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4),
        ]));
        assert_eq!(expected, arr);

        // Test w/ increment of 2
        let arr = range(&ctx, &None, vec![Value::Int(0), Value::Int(5), Value::Int(2)]);
        let expected = Some(Value::new_array_obj(vec![
            Value::Int(0),
            Value::Int(2),
            Value::Int(4),
        ]));
        assert_eq!(expected, arr);
    }

    #[test]
    fn range_returning_single_element_int_array() {
        let ctx = VMContext::default();

        // Test w/ increment larger than range
        let arr = range(&ctx, &&None, vec![Value::Int(0), Value::Int(5), Value::Int(5)]);
        let expected = Some(Value::new_array_obj(vec![Value::Int(0)]));
        assert_eq!(expected, arr);

        // Test w/ [0, 1)
        let arr = range(&ctx, &None, vec![Value::Int(0), Value::Int(1), Value::Int(1)]);
        let expected = Some(Value::new_array_obj(vec![Value::Int(0)]));
        assert_eq!(expected, arr);

        // Test w/ [0, 0) -> Empty array
        let arr = range(&ctx, &None, vec![Value::Int(0), Value::Int(0), Value::Int(1)]);
        let expected = Some(Value::new_array_obj(vec![]));
        assert_eq!(expected, arr);
    }
}
