use std::path::PathBuf;
use std::fmt::Debug;
use abra_core::parser::ast::ModuleId;
use abra_core::module_loader::ModuleReader;

#[derive(Debug)]
pub struct FsModuleReader {
    project_root: PathBuf,
}

impl FsModuleReader {
    pub fn new(project_root: PathBuf) -> Self {
        Self { project_root }
    }
}

impl ModuleReader for FsModuleReader {
    fn read_module(&mut self, module_id: &ModuleId) -> Option<String> {
        let file_path = self.project_root.join(&module_id.get_path("abra"));
        match std::fs::read_to_string(file_path) {
            Ok(contents) => Some(contents),
            Err(_) => None
        }
    }
}
