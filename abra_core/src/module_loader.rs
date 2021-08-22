use crate::parser::ast::ModuleId;
use crate::typechecker::typechecker::TypedModule;
use crate::{Error, typecheck};
use crate::builtins::{load_native_module};
use std::collections::HashMap;
use crate::vm::compiler::{compile, Module, Metadata};
use crate::typechecker::types::Type;
use crate::builtins::native::load_native_module_contents;
use crate::builtins::native_module_builder::ModuleSpec;

pub trait ModuleReader {
    fn read_module(&self, module_id: &ModuleId) -> Option<String>;
}

pub enum ModuleLoaderError {
    WrappedError(Error),
    NoSuchModule,
    CircularDependency,
}

#[derive(Debug)]
pub struct ModuleLoader<'a, R: ModuleReader> {
    module_reader: &'a R,
    native_module_cache: HashMap<String, ModuleSpec>,
    pub typed_module_cache: HashMap<String, Option<TypedModule>>,
    pub(crate) compiled_modules: Vec<(Module, Option<Metadata>)>,
    pub(crate) ordering: Vec<ModuleId>,
}

impl<'a, R: ModuleReader> ModuleLoader<'a, R> {
    pub fn new(module_reader: &'a R) -> Self {
        let typed_module_cache = HashMap::new();
        let compiled_modules = Vec::new();
        let native_module_cache = HashMap::new();
        let ordering = Vec::new();

        let mut loader = Self { module_reader, typed_module_cache, compiled_modules, native_module_cache, ordering };
        loader.load_builtin("prelude");
        loader
    }

    pub fn load_module(&mut self, module_id: &ModuleId) -> Result<(), ModuleLoaderError> {
        let module_name = module_id.get_name();
        match self.typed_module_cache.get(&module_name) {
            Some(Some(_)) => return Ok(()),
            Some(None) => return Err(ModuleLoaderError::CircularDependency),
            _ => {}
        }

        if self.load_builtin(&module_name) {
            return Ok(());
        }

        let contents = if !module_id.0 {
            load_native_module_contents(module_id)
        } else {
            self.module_reader.read_module(&module_id)
        };
        let contents = contents.ok_or(ModuleLoaderError::NoSuchModule)?;

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

    fn load_builtin(&mut self, module_name: &str) -> bool {
        let mod_spec = match self.native_module_cache.get(module_name) {
            Some(mod_spec) => mod_spec,
            None => {
                match load_native_module(module_name) {
                    None => return false,
                    Some(mod_spec_fn) => {
                        self.native_module_cache.insert(module_name.to_string(), mod_spec_fn());
                        self.native_module_cache.get(module_name).unwrap()
                    }
                }
            }
        };

        let module_id = mod_spec.typed_module.module_id.clone();

        self.ordering.push(module_id.clone());
        self.typed_module_cache.insert(module_id.get_name(), Some(mod_spec.typed_module.clone()));

        true
    }

    pub fn get_module(&self, module_id: &ModuleId) -> &TypedModule {
        let name = module_id.get_name();
        self.typed_module_cache.get(&name)
            .expect("It should have been loaded previously")
            .as_ref()
            .expect("It should have completed loading")
    }

    pub fn resolve_type(&self, type_name: &String) -> Option<&Type> {
        let module_name = type_name.split("/").next()
            .expect("Type name should be properly namespaced");
        let typed_module = self.typed_module_cache.get(module_name)
            .expect("It should have been loaded previously")
            .as_ref()
            .expect("It should have completed loading");
        typed_module.referencable_types.get(type_name)
    }

    pub fn add_typed_module(&mut self, typed_module: TypedModule) {
        self.ordering.push(typed_module.module_id.clone());
        self.typed_module_cache.insert(typed_module.module_id.get_name(), Some(typed_module));
    }

    pub fn compile_all(&mut self) {
        let mut globals = HashMap::<ModuleId, HashMap<String, usize>>::new();

        let ordering = self.ordering.clone();
        for module_id in ordering {
            let module_name = module_id.get_name();

            if let Some(mod_spec) = self.native_module_cache.get(&module_name) {
                self.compiled_modules.push((mod_spec.compiled_module.clone(), None));

                if !globals.contains_key(&module_id) {
                    let num_globals: usize = globals.values().map(|m| m.len()).sum();
                    let map = mod_spec.constant_names.iter().enumerate()
                        .map(|(idx, g)| (g.clone(), num_globals + idx))
                        .collect();
                    globals.insert( module_id.clone(), map);
                }

                continue;
            }

            let typed_module = self.typed_module_cache.get(&module_name).unwrap().as_ref().unwrap().clone();

            let module_idx = self.compiled_modules.len();

            globals.insert(module_id.clone(), HashMap::new());
            let (module, metadata) = compile(typed_module, module_idx, &mut globals).unwrap();
            self.compiled_modules.push((module, Some(metadata)));
        }
    }
}
