use crate::vm::value::Value;
use crate::vm::vm::{VM, VMContext};
use crate::{Error, compile};
use crate::common::test_utils::MockModuleReader;
use crate::parser::ast::ModuleId;

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

pub fn interpret(input: &str) -> Value {
    interpret_get_result(input).unwrap()
}

pub fn interpret_get_result<S: AsRef<str>>(input: S) -> Result<Value, Error> {
    let mut mock_reader = MockModuleReader::default();
    let module_id = ModuleId::from_name("_test");
    let modules = match compile(module_id, &input.as_ref().to_string(), &mut mock_reader) {
        Ok(modules) => modules,
        Err(error) => return Err(error)
    };

    let mut vm = VM::new(VMContext::default());
    let mut res = Value::Nil;
    for module in modules {
        res = vm.run(module).map_err(|e| Error::InterpretError(e))?;
    }
    Ok(res)
}
