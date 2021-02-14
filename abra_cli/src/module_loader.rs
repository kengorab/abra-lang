use std::collections::HashMap;
use std::path::PathBuf;
use abra_core::{ModuleLoader, typecheck, ModuleLoaderError};
use abra_core::typechecker::typechecker::TypedModule;
use std::fmt::Debug;
use abra_core::parser::ast::ModuleId;

#[derive(Debug)]
pub struct Loader {
    project_root: PathBuf,
    cache: HashMap<String, Option<TypedModule>>,
}

impl Loader {
    pub fn new(project_root: PathBuf) -> Self {
        Self { project_root, cache: HashMap::new() }
    }
}

impl ModuleLoader for Loader {
    fn load_module(&mut self, module_id: &ModuleId) -> Result<(), ModuleLoaderError> {
        if !module_id.0 { unimplemented!() }

        let module_name = module_id.get_name();
        match self.cache.get(&module_name) {
            Some(Some(_)) => return Ok(()),
            Some(None) => return Err(ModuleLoaderError::CircularDependency),
            _ => {}
        }

        let file_path = self.project_root.join(&module_id.get_path("abra"));
        let contents = match std::fs::read_to_string(file_path) {
            Ok(contents) => contents,
            Err(_) => return Err(ModuleLoaderError::CannotLoadModule)
        };

        self.cache.insert(module_name.clone(), None);
        match typecheck(module_name.clone(), &contents, self) {
            Ok(module) => {
                self.cache.insert(module_name, Some(module));
                Ok(())
            }
            Err(e) => Err(ModuleLoaderError::WrappedError(e))
        }
    }

    fn get_module(&self, module_id: &ModuleId) -> &TypedModule {
        if !module_id.0 { unimplemented!() }

        let name = module_id.get_name();
        self.cache.get(&name)
            .expect("It should have been loaded previously")
            .as_ref()
            .expect("It should have completed loading")
    }
}
