use std::collections::HashMap;
use crate::typechecker::typechecker::TypedModule;
use crate::{ModuleLoader, typecheck, ModuleLoaderError};
use crate::parser::ast::ModuleId;

#[derive(Debug, Default)]
pub struct MockLoader {
    modules_raw: HashMap<String, String>,
    typed_modules: HashMap<String, Option<TypedModule>>,
}

impl MockLoader {
    pub fn new(modules: Vec<(&str, &str)>) -> Self {
        let modules_raw = modules.into_iter().map(|(path, contents)| (path.to_string(), contents.to_string())).collect();
        Self { modules_raw, ..MockLoader::default() }
    }
}

impl ModuleLoader for MockLoader {
    fn load_module(&mut self, module_id: &ModuleId) -> Result<(), ModuleLoaderError> {
        let module_name = module_id.get_name();

        match self.typed_modules.get(&module_name) {
            Some(Some(_)) => return Ok(()),
            Some(None) => return Err(ModuleLoaderError::CircularDependency),
            _ => {}
        }

        match self.modules_raw.get(&module_name) {
            None => Err(ModuleLoaderError::CannotLoadModule),
            Some(contents) => {
                self.typed_modules.insert(module_name.clone(), None);

                let contents = contents.clone();
                match typecheck(module_name.clone(), &contents, self) {
                    Ok(module) => {
                        self.typed_modules.insert(module_name, Some(module));
                        Ok(())
                    }
                    Err(e) => Err(ModuleLoaderError::WrappedError(e))
                }
            }
        }
    }

    fn get_module(&self, module_id: &ModuleId) -> &TypedModule {
        let module_name = module_id.get_name();
        self.typed_modules.get(&module_name).unwrap().as_ref().unwrap()
    }
}
