use crate::parser::ast::ModuleId;
use crate::typechecker::typechecker::{TypedModule, ScopeBinding, ExportedValue};
use crate::{Error, typecheck};
use crate::builtins::{load_native_module};
use std::collections::HashMap;
use crate::vm::compiler::{compile, Module, Metadata};
use crate::typechecker::types::Type;
use crate::builtins::native::load_native_module_contents;
use crate::builtins::native_module_builder::ModuleSpec;

pub trait ModuleReader {
    fn resolve_module_path(&mut self, module_id: &ModuleId, with_respect_to: &ModuleId) -> String;
    fn read_module(&mut self, module_id: &ModuleId, module_name: &String) -> Option<String>;
    fn get_module_name(&self, module_id: &ModuleId) -> String;
}

pub enum ModuleLoaderError {
    WrappedError(Error),
    NoSuchModule,
    CircularDependency,
}

#[derive(Debug)]
pub struct ModuleLoader<'a, R: ModuleReader> {
    module_reader: &'a mut R,
    native_module_cache: HashMap<String, ModuleSpec>,
    typed_module_cache: HashMap<String, Option<TypedModule>>,
    pub(crate) compiled_modules: Vec<(Module, Option<Metadata>)>,
    pub ordering: Vec<ModuleId>,
}

impl<'a, R: ModuleReader> ModuleLoader<'a, R> {
    pub fn new(module_reader: &'a mut R) -> Self {
        let typed_module_cache = HashMap::new();
        let compiled_modules = Vec::new();
        let native_module_cache = HashMap::new();
        let ordering = Vec::new();

        let mut loader = Self { module_reader, typed_module_cache, compiled_modules, native_module_cache, ordering };
        loader.load_builtin("prelude");
        loader
    }

    pub fn get_module_name(&self, module_id: &ModuleId) -> String {
        self.module_reader.get_module_name(&module_id)
    }

    pub fn load_module(&mut self, current_module_id: &ModuleId, import_module_id: &ModuleId) -> Result<(), ModuleLoaderError> {
        let module_name = match import_module_id {
            ModuleId::External(_) => self.module_reader.get_module_name(&import_module_id),
            ModuleId::Internal(_) => self.module_reader.resolve_module_path(&import_module_id, &current_module_id)
        };
        match self.typed_module_cache.get(&module_name) {
            Some(Some(_)) => return Ok(()),
            Some(None) => return Err(ModuleLoaderError::CircularDependency),
            _ => {}
        }

        if self.load_builtin(&module_name) {
            return Ok(());
        }

        let contents = match &import_module_id {
            ModuleId::External(_) => load_native_module_contents(import_module_id),
            ModuleId::Internal(_) => self.module_reader.read_module(&import_module_id, &module_name),
        };
        let contents = contents.ok_or(ModuleLoaderError::NoSuchModule)?;

        self.typed_module_cache.insert(module_name.clone(), None);
        match typecheck(import_module_id.clone(), &contents, self) {
            Ok(module) => {
                self.typed_module_cache.insert(module_name.clone(), Some(module));
                self.ordering.push(import_module_id.clone());
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
        let module_name = self.module_reader.get_module_name(&module_id);
        self.typed_module_cache.insert(module_name, Some(mod_spec.typed_module.clone()));

        true
    }

    pub fn get_module(&self, module_id: &ModuleId) -> &TypedModule {
        let name = self.module_reader.get_module_name(&module_id);
        self.get_module_by_name(&name)
    }

    pub fn get_module_by_name(&self, module_name: &String) -> &TypedModule {
        self.typed_module_cache.get(module_name)
            .expect(&format!("Module '{}' should have been loaded previously", module_name))
            .as_ref()
            .expect(&format!("Module '{}' should have completed loading", module_name))
    }

    pub fn resolve_binding_type(&self, name: &String) -> Option<&Type> {
        self.typed_module_cache.values()
            .filter_map(|m| {
                if let Some(m) = m {
                    m.global_bindings.get(name)
                        .map(|ScopeBinding(_, typ, _)| typ)
                        .or_else(|| {
                            m.exports.get(name).map(|e| match e {
                                ExportedValue::Binding(t) => t,
                                ExportedValue::Type { backing_type, .. } => backing_type,
                            })
                        })
                } else {
                    None
                }
            })
            .next()
    }

    pub fn resolve_type(&self, type_name: &String) -> Option<&Type> {
        let slash_idx = type_name.rfind('/').unwrap();
        let (module_name, _) = type_name.split_at(slash_idx) ;

        let typed_module = self.typed_module_cache.get(module_name)
            .expect(&format!("Module '{}' should have been loaded previously", &module_name))
            .as_ref()
            .expect(&format!("Module '{}' should have completed loading", &module_name));
        typed_module.referencable_types.get(type_name)
    }

    pub fn add_typed_module(&mut self, typed_module: TypedModule) {
        self.ordering.push(typed_module.module_id.clone());
        let module_name = self.module_reader.get_module_name(&typed_module.module_id);
        self.typed_module_cache.insert(module_name, Some(typed_module));
    }

    pub fn compile_all(&mut self) {
        let mut globals = HashMap::<ModuleId, HashMap<String, usize>>::new();

        let ordering = self.ordering.clone();
        for module_id in ordering {
            let module_name = self.module_reader.get_module_name(&module_id);

            if let Some(mod_spec) = self.native_module_cache.get(&module_name) {
                self.compiled_modules.push((mod_spec.compiled_module.clone(), None));

                if !globals.contains_key(&module_id) {
                    let num_globals: usize = globals.values().map(|m| m.len()).sum();
                    let map = mod_spec.constant_names.iter().enumerate()
                        .map(|(idx, g)| (g.clone(), num_globals + idx))
                        .collect();
                    globals.insert(module_id.clone(), map);
                }

                continue;
            }

            let typed_module = self.typed_module_cache.get(&module_name).unwrap().as_ref().unwrap().clone();

            let module_idx = self.compiled_modules.len();
            let module_name = self.module_reader.get_module_name(&module_id);

            globals.insert(module_id.clone(), HashMap::new());
            let (module, metadata) = compile(typed_module, module_idx, module_name, &mut globals).unwrap();
            self.compiled_modules.push((module, Some(metadata)));
        }
    }
}
