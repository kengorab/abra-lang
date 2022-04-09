use std::env::current_dir;
use abra_native::abra_function;
use crate::builtins::native_module_builder::{ModuleSpec, ModuleSpecBuilder};
use crate::vm::value::Value;
use crate::builtins::arguments::Arguments;
use crate::vm::vm::VM;

#[abra_function(signature = "readFile(path: String): String?")]
fn read_file(mut args: Arguments) -> Value {
    let file_name = args.next_string();
    match std::fs::read_to_string(file_name) {
        Ok(contents) => Value::new_string_obj(contents),
        Err(_) => Value::Nil
    }
}

#[abra_function(signature = "getCurrentDir(): String")]
fn get_current_dir() -> Value {
    let cwd = current_dir().unwrap().to_str().unwrap().to_string();
    Value::new_string_obj(cwd)
}

#[abra_function(signature = "prompt(msg: String?): String")]
fn prompt(mut args: Arguments, vm: &mut VM) -> Value {
    let msg = args.next_string_or_default("");
    let resp = (vm.ctx.prompt)(&msg);
    Value::new_string_obj(resp)
}

pub fn load_module() -> ModuleSpec {
    ModuleSpecBuilder::new("io")
        .add_function(read_file__gen_spec)
        .add_function(get_current_dir__gen_spec)
        .add_function(prompt__gen_spec)
        .build()
}

#[cfg(test)]
mod test {
    use crate::builtins::test_utils::interpret_get_result;

    #[test]
    fn test_importing_module() {
        let result = interpret_get_result("import readFile, prompt from \"io\"");
        assert!(result.is_ok());
    }
}
