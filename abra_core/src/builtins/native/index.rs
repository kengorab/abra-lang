use crate::parser::ast::ModuleId;

pub fn load_native_module_contents(module_id: &ModuleId) -> Option<String> {
    match module_id.get_name().as_str() {
        "test" => Some(include_str!("test.abra").to_string()),
        _ => None
    }
}
