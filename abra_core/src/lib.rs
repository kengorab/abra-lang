extern crate abra_native;
extern crate itertools;
extern crate strum;
#[macro_use]
extern crate strum_macros;

use crate::vm::compiler::{Module, Metadata};
use crate::typechecker::typechecker::TypedModule;
use crate::common::display_error::DisplayError;
use crate::parser::parser::ParseResult;
use crate::typechecker::typechecker_error::{TypecheckerErrorKind, TypecheckerError};
use crate::module_loader::{ModuleLoader, ModuleReader, ModuleLoaderError};
use crate::parser::ast::ModuleId;

pub mod builtins;
pub mod common;
pub mod lexer;
pub mod module_loader;
pub mod parser;
pub mod typechecker;
pub mod vm;

#[derive(Debug)]
pub enum Error {
    LexerError(lexer::lexer_error::LexerError),
    ParseError(parser::parse_error::ParseError),
    TypecheckerError(typechecker::typechecker_error::TypecheckerError),
    InterpretError(vm::vm::InterpretError),
}

impl Error {
    pub fn module_id(&self) -> &ModuleId {
        match self {
            Error::LexerError(e) => &e.module_id,
            Error::ParseError(e) => &e.module_id,
            Error::TypecheckerError(e) => &e.module_id,
            Error::InterpretError(_) => unreachable!()
        }
    }
}

impl DisplayError for Error {
    fn message_for_error(&self, file_name: &String, lines: &Vec<&str>) -> String {
        match self {
            Error::LexerError(e) => e.message_for_error(file_name, lines),
            Error::ParseError(e) => e.message_for_error(file_name, lines),
            Error::TypecheckerError(e) => e.message_for_error(file_name, lines),
            Error::InterpretError(_) => "Runtime error!".to_string()
        }
    }
}

fn tokenize_and_parse(module_id: &ModuleId, input: &String) -> Result<ParseResult, Error> {
    match lexer::lexer::tokenize(module_id, input) {
        Err(e) => Err(Error::LexerError(e)),
        Ok(tokens) => match parser::parser::parse(module_id.clone(), tokens) {
            Err(e) => Err(Error::ParseError(e)),
            Ok(nodes) => Ok(nodes)
        }
    }
}

pub fn typecheck<R>(module_id: ModuleId, input: &String, loader: &mut ModuleLoader<R>) -> Result<TypedModule, Error>
    where R: ModuleReader
{
    let ParseResult { imports, nodes } = tokenize_and_parse(&module_id, input)?;
    for (import_token, import_module_id) in imports {
        loader.load_module(&import_module_id).map_err(|e| match e {
            ModuleLoaderError::WrappedError(e) => e,
            ModuleLoaderError::CannotLoadModule => {
                let kind = TypecheckerErrorKind::InvalidModuleImport {
                    token: import_token,
                    module_name: import_module_id.get_name(),
                    circular: false,
                };
                Error::TypecheckerError(TypecheckerError { module_id: import_module_id, kind })
            }
            ModuleLoaderError::CircularDependency => {
                let kind = TypecheckerErrorKind::InvalidModuleImport {
                    token: import_token,
                    module_name: import_module_id.get_name(),
                    circular: true,
                };
                Error::TypecheckerError(TypecheckerError { module_id: import_module_id, kind })
            }
        })?
    }

    match typechecker::typechecker::typecheck(module_id, nodes, loader) {
        Err(e) => Err(Error::TypecheckerError(e)),
        Ok(module) => Ok(module)
    }
}

pub fn compile<R>(module_id: ModuleId, input: &String, module_reader: R) -> Result<Vec<Module>, Error>
    where R: ModuleReader
{
    let mut loader = ModuleLoader::new(module_reader);
    let module = typecheck(module_id, input, &mut loader)?;
    loader.add_typed_module(module);

    loader.compile_all();

    let modules = loader.compiled_modules.into_iter()
        .map(|(module, _)| module)
        .collect();
    Ok(modules)
}

pub fn compile_and_disassemble<R>(module_id: ModuleId, input: &String, module_reader: R) -> Result<String, Error>
    where R: ModuleReader
{
    let mut loader = ModuleLoader::new(module_reader);
    let module = typecheck(module_id, input, &mut loader)?;
    loader.add_typed_module(module);

    loader.compile_all();

    let modules = loader.compiled_modules.into_iter()
        .map(|(module, metadata)| (module, metadata.unwrap_or(Metadata::default())))
        .collect();
    let dis = vm::disassembler::disassemble(modules);
    Ok(dis)
}
