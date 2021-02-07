use crate::vm::value::Value;
use crate::lexer::lexer::tokenize;
use crate::parser::parser::parse;
use crate::typechecker::typechecker::typecheck;
use crate::vm::compiler::compile;
use crate::vm::vm::{VM, VMContext};
use crate::Error;

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
    ($($k:expr => $v:expr),*) => { Value::new_map_obj(vec![$($k, $v),+]) };
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
    let module_name = "_test.abra".to_string();

    let tokens = tokenize(&input.to_string()).unwrap();
    let ast = parse(tokens).unwrap();
    let module = typecheck(module_name, ast).unwrap();
    let (module, _) = compile(module).unwrap();

    let mut vm = VM::new(module, VMContext::default());
    vm.run().unwrap()
}

pub fn interpret_get_result<S: AsRef<str>>(input: S) -> Result<Option<Value>, Error> {
    let module_name = "_test.abra".to_string();
    let module = match crate::compile(module_name, &input.as_ref().to_string()) {
        Ok((module, _)) => module,
        Err(error) => return Err(error)
    };

    let ctx = VMContext { print: |input| print!("{}", input) };
    let mut vm = VM::new(module, ctx);
    vm.run().map_err(|e| Error::InterpretError(e))
}
