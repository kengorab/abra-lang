use std::path::PathBuf;
use std::fmt::Debug;
use abra_core::parser::ast::ModuleId;
use abra_core::module_loader::ModuleReader;

#[derive(Debug)]
pub struct FsModuleReader {
    pub(crate) project_root: PathBuf,
}

impl FsModuleReader {
    pub fn new(project_root: PathBuf) -> Self {
        Self { project_root }
    }
}

impl ModuleReader for FsModuleReader {
    fn read_module(&self, module_id: &ModuleId) -> Option<String> {
        let file_path = module_id.get_path(Some(&self.project_root));
        match std::fs::read_to_string(file_path) {
            Ok(contents) => Some(contents),
            Err(_) => None
        }
    }
}
