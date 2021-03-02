use crate::parser::ast::ModuleId;
use crate::typechecker::typechecker::TypedModule;
use crate::{Error, typecheck};
use crate::vm::prelude::Prelude;
use std::collections::HashMap;
use crate::vm::compiler::{compile, Module, Metadata};

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
    typed_module_cache: HashMap<String, Option<TypedModule>>,
    pub(crate) compiled_modules: Vec<(Module, Option<Metadata>)>,
    compiled_module_constants: HashMap<ModuleId, HashMap<String, usize>>,
    compiled_modules_indices: HashMap<ModuleId, usize>,
    pub(crate) ordering: Vec<ModuleId>,
}

const PRELUDE_MODULE_IDX: usize = 0;

impl<R: ModuleReader> ModuleLoader<R> {
    pub fn new(module_reader: R) -> Self {
        let mut cache = HashMap::new();

        let prelude = Prelude::typed_module();
        let ordering = vec![prelude.module_id.clone()];
        cache.insert(prelude.module_id.get_name(), Some(prelude));

        let compiled_modules = Vec::new();
        let compiled_module_constants = HashMap::new();
        let compiled_modules_indices = HashMap::new();

        Self { module_reader, typed_module_cache: cache, compiled_modules, compiled_module_constants, compiled_modules_indices, ordering }
    }

    pub fn load_module(&mut self, module_id: &ModuleId) -> Result<(), ModuleLoaderError> {
        let module_name = module_id.get_name();
        match self.typed_module_cache.get(&module_name) {
            Some(Some(_)) => return Ok(()),
            Some(None) => return Err(ModuleLoaderError::CircularDependency),
            _ => {}
        }

        let contents = match self.module_reader.read_module(&module_id) {
            Some(contents) => contents,
            None => return Err(ModuleLoaderError::CannotLoadModule),
        };

        self.typed_module_cache.insert(module_name.clone(), None);
        match typecheck(module_id.clone(), &contents, self) {
            Ok(module) => {
                self.typed_module_cache.insert(module_name.clone(), Some(module));
                self.ordering.push(module_id.clone());
                Ok(())
            }
            Err(e) => Err(ModuleLoaderError::WrappedError(e))
        }
    }

    pub fn get_module(&self, module_id: &ModuleId) -> &TypedModule {
        let name = module_id.get_name();
        self.typed_module_cache.get(&name)
            .expect("It should have been loaded previously")
            .as_ref()
            .expect("It should have completed loading")
    }

    pub fn add_typed_module(&mut self, typed_module: TypedModule) {
        self.ordering.push(typed_module.module_id.clone());
        self.typed_module_cache.insert(typed_module.module_id.get_name(), Some(typed_module));
    }

    pub fn compile_all(&mut self) {
        let ordering = self.ordering.clone();
        for module_id in ordering {
            if module_id == ModuleId::from_name("prelude") {
                let (prelude_module, constants_by_idx) = Prelude::compiled_module();
                self.compiled_modules.push((prelude_module, None));
                self.compiled_module_constants.insert(module_id.clone(), constants_by_idx);

                let module_idx = self.compiled_modules.len() - 1;
                debug_assert!(module_idx == PRELUDE_MODULE_IDX);
                self.compiled_modules_indices.insert(module_id.clone(), module_idx);
                continue;
            }

            let typed_module = self.typed_module_cache.get(&module_id.get_name()).unwrap().as_ref().unwrap().clone();

            let module_idx = self.compiled_modules.len();
            self.compiled_modules_indices.insert(module_id.clone(), module_idx);

            let (module, metadata) = compile(typed_module, module_idx, self).unwrap();
            self.compiled_modules.push((module, Some(metadata)));
        }
    }

    pub fn get_compiled_module(&self, module_id: &ModuleId) -> &Module {
        self.compiled_modules_indices.get(module_id)
            .and_then(|idx| self.compiled_modules.get(*idx))
            .map(|(module, _)| module)
            .unwrap()
    }

    fn get_prelude_const_idx(&mut self, const_name: &String) -> Option<(usize, usize)> {
        let prelude_constants = &self.compiled_module_constants[&ModuleId::from_name("prelude")];
        prelude_constants.get(const_name).map(|const_idx| (PRELUDE_MODULE_IDX, *const_idx))
    }

    pub fn get_const_idx(&mut self, module_id: &ModuleId, const_name: &String) -> Option<(usize, usize)> {
        let module_idx = self.compiled_modules_indices[module_id];

        self.compiled_module_constants.get_mut(module_id)
            .and_then(|constants| constants.get(const_name).map(|const_idx| (module_idx, *const_idx)))
            .or_else(|| self.get_prelude_const_idx(const_name))
    }

    pub fn add_const_idx(&mut self, module_id: &ModuleId, const_name: &String, const_idx: usize) {
        match self.compiled_module_constants.get_mut(module_id) {
            Some(constants) => {
                debug_assert!(!constants.contains_key(const_name));
                constants.insert(const_name.clone(), const_idx);
            }
            None => {
                let mut constants = HashMap::new();
                constants.insert(const_name.clone(), const_idx);

                self.compiled_module_constants.insert(module_id.clone(), constants);
            }
        }
    }
}
