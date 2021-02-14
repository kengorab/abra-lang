use crate::vm::value::Value;
use crate::vm::vm::{VM, VMContext};
use crate::{Error, compile};
use crate::common::test_utils::MockLoader;

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
    let mut loader = MockLoader::default();
    let module_path = "_test".to_string();
    let (module, _) = compile(module_path, &input.to_string(), &mut loader).unwrap();

    let mut vm = VM::new(module, VMContext::default());
    vm.run().unwrap()
}

pub fn interpret_get_result<S: AsRef<str>>(input: S) -> Result<Option<Value>, Error> {
    let mut loader = MockLoader::default();
    let module_path = "_test".to_string();
    let module = match compile(module_path, &input.as_ref().to_string(), &mut loader) {
        Ok((module, _)) => module,
        Err(error) => return Err(error)
    };

    let ctx = VMContext { print: |input| print!("{}", input) };
    let mut vm = VM::new(module, ctx);
    vm.run().map_err(|e| Error::InterpretError(e))
}
