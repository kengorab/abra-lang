use abra_native::{AbraType, abra_methods};
use crate::vm::value::Value;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::collections::HashMap;
use crate::vm::vm::VM;

#[derive(AbraType, Debug, Clone, Eq, PartialEq)]
#[abra_type(module = "prelude", signature = "Process", noconstruct = true)]
pub struct Process {
    #[abra_field(name = "args", field_type = "String[]", readonly)]
    args: Vec<String>,

    #[abra_field(name = "env", field_type = "Map<String, String>", readonly)]
    env: HashMap<String, String>,
}

#[abra_methods]
impl Process {
    pub(crate) fn init(vm: &VM) -> Self {
        Self {
            args: vm.ctx.args.clone(),
            env: vm.ctx.env.clone(),
        }
    }

    #[abra_getter(field = "args")]
    fn get_args(&self) -> Value {
        let args = self.args.iter()
            .map(|a| Value::new_string_obj(a.clone()))
            .collect();
        Value::new_array_obj(args)
    }

    #[abra_getter(field = "env")]
    fn get_env(&self) -> Value {
        let env_values = self.env.iter()
            .flat_map(|(key, val)| vec![
                Value::new_string_obj(key.clone()),
                Value::new_string_obj(val.clone()),
            ])
            .collect();
        Value::new_map_obj(env_values)
    }
}

impl Hash for Process {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        for arg in &self.args {
            arg.hash(hasher);
        }

        for (k, v) in &self.env {
            k.hash(hasher);
            v.hash(hasher);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::test_utils::{interpret, interpret_get_result};
    use crate::vm::value::Value;

    #[test]
    fn test_process_singleton_instance_() {
        let result = interpret(r#"
          val args: String[] | Int = process.args
          val r = match args {
            Int => false
            _ => true
          }
          r
        "#);
        let expected = Value::Bool(true);
        assert_eq!(expected, result);

        let result = interpret(r#"
          val env: Map<String, String> | Int = process.env
          val r = match env {
            Int => false
            _ => true
          }
          r
        "#);
        let expected = Value::Bool(true);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_instantiation_fails() {
        let result = interpret_get_result(r#"
          val p = Process()
        "#);
        assert!(result.is_err());
    }
}
