use crate::builtins::native::to_string;
use crate::typechecker::types::{Type, FnType};
use crate::vm::value::{Value, Obj};
use crate::vm::vm::VM;
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};

// Native functions must return a Value, even if they're of return type Unit.
// If their return type is Unit, they should return None//Value::Nil.
pub type NativeAbraFn = fn(Option<Value>, Vec<Value>, &mut VM) -> Option<Value>;

#[derive(Clone)]
pub struct NativeFn {
    pub name: &'static str,
    pub receiver: Option<Box<Value>>,
    pub native_fn: NativeAbraFn,
    pub has_return: bool,
}

impl Debug for NativeFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeFn {{ name: {}, .. }}", self.name)
    }
}

impl Eq for NativeFn {}

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
    pub fn invoke(&self, args: Vec<Value>, vm_ref: &mut VM) -> Option<Value> {
        let func = self.native_fn;
        func(self.receiver.as_ref().map(|v| *v.clone()), args, vm_ref)
    }
}

pub struct NativeFnDesc {
    pub name: &'static str,
    pub type_args: Vec<&'static str>,
    pub args: Vec<(&'static str, Type)>,
    pub opt_args: Vec<(&'static str, Type)>,
    pub return_type: Type,
    pub is_variadic: bool,
}

impl NativeFnDesc {
    pub fn get_fn_type(&self) -> Type {
        let type_args = self.type_args.iter()
            .map(|name| name.to_string())
            .collect();

        let req_args = self.args.iter()
            .map(|(name, typ)| (name.to_string(), typ.clone().clone(), false));
        let opt_args = self.opt_args.iter()
            .map(|(name, typ)| (name.to_string(), typ.clone().clone(), true));
        let arg_types = req_args.chain(opt_args).collect();
        let ret_type = Box::new(self.return_type.clone());
        let is_variadic = self.is_variadic;

        Type::Fn(FnType { arg_types, type_args, ret_type, is_variadic })
    }
}

pub fn native_fns() -> Vec<(NativeFnDesc, NativeFn)> {
    let mut native_fns = Vec::new();

    native_fns.push((
        NativeFnDesc {
            name: "println",
            type_args: vec![],
            args: vec![],
            opt_args: vec![("_", Type::Array(Box::new(Type::Any)))],
            return_type: Type::Unit,
            is_variadic: true,
        },
        NativeFn {
            name: "println",
            receiver: None,
            native_fn: println,
            has_return: false,
        }
    ));
    native_fns.push((
        NativeFnDesc {
            name: "range",
            type_args: vec![],
            args: vec![("from", Type::Int), ("to", Type::Int)],
            opt_args: vec![("increment", Type::Int)],
            return_type: Type::Array(Box::new(Type::Int)),
            is_variadic: false,
        },
        NativeFn {
            name: "range",
            receiver: None,
            native_fn: range,
            has_return: true,
        }
    ));
    native_fns.push((
        NativeFnDesc {
            name: "readFile",
            type_args: vec![],
            args: vec![("path", Type::String)],
            opt_args: vec![],
            return_type: Type::Option(Box::new(Type::String)),
            is_variadic: false,
        },
        NativeFn {
            name: "readFile",
            receiver: None,
            native_fn: read_file,
            has_return: true,
        }
    ));

    native_fns
}

fn println(_receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
    let print_fn = vm.ctx.print;

    let mut args = args.into_iter();
    if let Some(arg) = args.next() {
        if let Value::Obj(obj) = arg {
            match &*(obj.borrow()) {
                Obj::NativeInstanceObj(i) => {
                    let vals = &i.as_array().unwrap()._inner;
                    let num_vals = vals.len();
                    for (idx, val) in vals.into_iter().enumerate() {
                        let sp = if idx == num_vals - 1 { "" } else { " " };
                        print_fn(&format!("{}{}", to_string(val, vm), sp));
                    }
                }
                _ => unreachable!()
            }
        } else if arg != Value::Nil { unreachable!() }
    }
    print_fn("\n");

    None
}

fn range(_receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
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

fn read_file(_receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
    let file_name = args.into_iter().next().expect("readFile requires 1 argument");
    let file_name = if let Value::Obj(obj) = file_name {
        match &(*obj.borrow()) {
            Obj::StringObj(s) => s.clone(),
            _ => unreachable!()
        }
    } else {
        panic!("readFile requires a String as first argument")
    };

    match std::fs::read_to_string(file_name) {
        Ok(contents) => Some(Value::new_string_obj(contents)),
        Err(_) => Some(Value::Nil)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::vm::compiler::Module;
    use crate::vm::vm::VMContext;

    fn make_vm() -> VM {
        let module = Module { code: vec![], constants: vec![] };
        VM::new(module, VMContext::default())
    }

    #[test]
    fn range_returning_int_array() {
        let mut vm = make_vm();

        // Test w/ increment of 1
        let arr = range(None, vec![Value::Int(0), Value::Int(5), Value::Int(1)], &mut vm);
        let expected = Some(Value::new_array_obj(vec![
            Value::Int(0),
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4),
        ]));
        assert_eq!(expected, arr);

        // Test w/ increment of 2
        let arr = range(None, vec![Value::Int(0), Value::Int(5), Value::Int(2)], &mut vm);
        let expected = Some(Value::new_array_obj(vec![
            Value::Int(0),
            Value::Int(2),
            Value::Int(4),
        ]));
        assert_eq!(expected, arr);
    }

    #[test]
    fn range_returning_single_element_int_array() {
        let mut vm = make_vm();

        // Test w/ increment larger than range
        let arr = range(None, vec![Value::Int(0), Value::Int(5), Value::Int(5)], &mut vm);
        let expected = Some(Value::new_array_obj(vec![Value::Int(0)]));
        assert_eq!(expected, arr);

        // Test w/ [0, 1)
        let arr = range(None, vec![Value::Int(0), Value::Int(1), Value::Int(1)], &mut vm);
        let expected = Some(Value::new_array_obj(vec![Value::Int(0)]));
        assert_eq!(expected, arr);

        // Test w/ [0, 0) -> Empty array
        let arr = range(None, vec![Value::Int(0), Value::Int(0), Value::Int(1)], &mut vm);
        let expected = Some(Value::new_array_obj(vec![]));
        assert_eq!(expected, arr);
    }
}
