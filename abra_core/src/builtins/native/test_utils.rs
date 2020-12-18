use crate::vm::value::Value;
use crate::lexer::lexer::tokenize;
use crate::parser::parser::parse;
use crate::typechecker::typechecker::typecheck;
use crate::vm::compiler::compile;
use crate::vm::vm::{VM, VMContext};

pub fn new_string_obj(string: &str) -> Value {
    Value::new_string_obj(string.to_string())
}

macro_rules! array {
    ($($i:expr),*) => { Value::new_array_obj(vec![$($i),*]) };
}

macro_rules! set {
    ($($i:expr),*) => { Value::new_set_obj(vec![$($i),*].into_iter().collect()) };
}

macro_rules! map {
    ($($k:expr => $v:expr),*) => {
        Value::new_map_obj(vec![$(($k, $v)),+].into_iter().collect())
    };
}

macro_rules! tuple {
    ($($i:expr),*) => { Value::new_tuple_obj(vec![$($i),*]) };
}

macro_rules! int_array {
    ($($i:expr),*) => { Value::new_array_obj(vec![$($i),*].into_iter().map(Value::Int).collect()) };
}

macro_rules! string_array {
    ($($i:expr),*) => { Value::new_array_obj(vec![$($i),*].into_iter().map(new_string_obj).collect()) };
}

pub fn interpret(input: &str) -> Option<Value> {
    let tokens = tokenize(&input.to_string()).unwrap();
    let ast = parse(tokens).unwrap();
    let (_, typed_ast) = typecheck(ast).unwrap();
    let (module, _) = compile(typed_ast).unwrap();

    let mut vm = VM::new(module, VMContext::default());
    vm.run().unwrap()
}
