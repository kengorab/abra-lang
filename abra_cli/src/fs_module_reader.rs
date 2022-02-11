use std::collections::HashMap;
use std::path::PathBuf;
use std::fmt::Debug;
use abra_core::parser::ast::{ModuleId, ModulePathSegment};
use abra_core::module_loader::ModuleReader;

#[derive(Clone, Debug)]
pub struct FsModuleReader {
    pub(crate) project_root: PathBuf,
    pub(crate) module_id_paths: HashMap<ModuleId, PathBuf>,
}

impl FsModuleReader {
    pub fn new(entrypoint_module_id: ModuleId, project_root: PathBuf) -> Self {
        let mut module_id_paths = HashMap::new();

        let entrypoint_path = PathBuf::from(entrypoint_module_id.get_path(&project_root));
        module_id_paths.insert(entrypoint_module_id, entrypoint_path);

        Self { project_root, module_id_paths }
    }
}

impl ModuleReader for FsModuleReader {
    fn resolve_module_path(&mut self, module_id: &ModuleId, with_respect_to: &ModuleId) -> String {
        let parent = self.module_id_paths.get(with_respect_to).unwrap().parent().unwrap();
        module_id.get_path(parent)
    }

    fn read_module(&mut self, module_id: &ModuleId, module_name: &String) -> Option<String> {
        let file_path = PathBuf::from(module_name);
        let contents = match std::fs::read_to_string(file_path.with_extension("abra")) {
            Ok(contents) => Some(contents),
            Err(_) => None
        };
        self.module_id_paths.insert(module_id.clone(), file_path);

        contents
    }

    fn get_module_name(&self, module_id: &ModuleId) -> String {
        if !module_id.0 {
            module_id.1.first()
                .map(|s| match s {
                    ModulePathSegment::Module(m) => m.to_string(),
                    _ => unreachable!()
                })
                .unwrap()
        } else {
            self.module_id_paths.get(module_id)
                .map(|p| p.to_str().unwrap().to_string())
                .expect(&format!("Fetching module {:?} without first having read it", &module_id))
        }
    }
}
