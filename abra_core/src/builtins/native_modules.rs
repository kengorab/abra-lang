use crate::builtins::native_module_builder::ModuleSpec;
use crate::builtins::{prelude, date};

pub fn load_module(module_name: &str) -> Option<fn() -> ModuleSpec> {
    match module_name {
        "prelude" => Some(prelude::load_module),
        "date" => Some(date::load_module),
        _ => return None
    }
}
