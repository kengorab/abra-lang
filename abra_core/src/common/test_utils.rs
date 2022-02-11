use std::collections::HashMap;
use std::path::PathBuf;
use crate::parser::ast::{ModuleId, ModulePathSegment};
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
    fn resolve_module_path(&mut self, module_id: &ModuleId, _with_respect_to: &ModuleId) -> String {
        module_id.get_path(PathBuf::from(""))
    }

    fn read_module(&mut self, _module_id: &ModuleId, module_name: &String) -> Option<String> {
        self.modules_raw.get(module_name).map(|s| s.to_string())
    }

    fn get_module_name(&self, module_id: &ModuleId) -> String {
        let ModuleId(is_local, path) = &module_id;
        if !is_local {
            path.first().map(|s| match s {
                ModulePathSegment::Module(m) => m.to_string(),
                _ => unreachable!()
            })
                .unwrap()
        } else {
            PathBuf::from(module_id.get_path(PathBuf::from(""))).to_str().map(|s| s.to_string()).unwrap()
        }
    }
}
