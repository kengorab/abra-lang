use crate::parser::ast::ModuleId;
use crate::typechecker::typechecker::TypedModule;
use std::collections::HashMap;
use crate::{Error, typecheck};
use crate::vm::prelude::Prelude;

pub trait ModuleReader {
    fn read_module(&mut self, module_id: &ModuleId) -> Option<String>;
}

pub enum ModuleLoaderError {
    WrappedError(Error),
    CannotLoadModule,
    CircularDependency,
}

#[derive(Debug)]
pub struct ModuleLoader<R: ModuleReader> {
    module_reader: R,
    cache: HashMap<String, Option<TypedModule>>,
}

impl<R: ModuleReader> ModuleLoader<R> {
    pub fn new(module_reader: R) -> Self {
        let mut cache = HashMap::new();

        let prelude = Prelude::typed_module();
        cache.insert(prelude.module_id.get_name(), Some(prelude));

        Self { module_reader, cache }
    }

    pub fn load_module(&mut self, current_module_id: &ModuleId, module_id: &ModuleId) -> Result<(), ModuleLoaderError> {
        let module_name = module_id.get_name();
        match self.cache.get(&module_name) {
            Some(Some(_)) => return Ok(()),
            Some(None) => return Err(ModuleLoaderError::CircularDependency),
            _ => {}
        }

        let contents = match self.module_reader.read_module(&module_id) {
            Some(contents) => contents,
            None => return Err(ModuleLoaderError::CannotLoadModule),
        };

        self.cache.insert(module_name.clone(), None);
        match typecheck(current_module_id.clone(), &contents, self) {
            Ok(module) => {
                self.cache.insert(module_name, Some(module));
                Ok(())
            }
            Err(e) => Err(ModuleLoaderError::WrappedError(e))
        }
    }

    pub fn get_module(&self, module_id: &ModuleId) -> &TypedModule {
        let name = module_id.get_name();
        self.cache.get(&name)
            .expect("It should have been loaded previously")
            .as_ref()
            .expect("It should have completed loading")
    }
}
