use std::collections::HashMap;
use crate::parser::ast::ModuleId;
use crate::module_loader::ModuleReader;

#[derive(Debug, Default)]
pub struct MockModuleReader {
    modules_raw: HashMap<String, String>,
}

impl MockModuleReader {
    pub fn new(modules: Vec<(&str, &str)>) -> Self {
        let modules_raw = modules.into_iter().map(|(path, contents)| (path.to_string(), contents.to_string())).collect();
        Self { modules_raw }
    }
}

impl ModuleReader for MockModuleReader {
    fn read_module(&mut self, module_id: &ModuleId) -> Option<String> {
        let module_name = module_id.get_name();
        self.modules_raw.get(&module_name).map(|s| s.to_string())
    }
}
