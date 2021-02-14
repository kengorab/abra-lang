extern crate abra_native;
extern crate itertools;
extern crate permutate;
extern crate strum;
#[macro_use]
extern crate strum_macros;

use crate::vm::compiler::{Metadata, Module};
use crate::typechecker::typechecker::TypedModule;
use crate::common::display_error::DisplayError;
use crate::parser::parser::ParseResult;
use crate::typechecker::typechecker_error::TypecheckerError;
use crate::module_loader::{ModuleLoader, ModuleReader, ModuleLoaderError};

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

impl DisplayError for Error {
    fn message_for_error(&self, lines: &Vec<&str>) -> String {
        match self {
            Error::LexerError(e) => e.message_for_error(lines),
            Error::ParseError(e) => e.message_for_error(lines),
            Error::TypecheckerError(e) => e.message_for_error(lines),
            Error::InterpretError(_) => "Runtime error!".to_string()
        }
    }
}

fn tokenize_and_parse(input: &String) -> Result<ParseResult, Error> {
    match lexer::lexer::tokenize(input) {
        Err(e) => Err(Error::LexerError(e)),
        Ok(tokens) => match parser::parser::parse(tokens) {
            Err(e) => Err(Error::ParseError(e)),
            Ok(nodes) => Ok(nodes)
        }
    }
}

pub fn typecheck<R>(module_name: String, input: &String, loader: &mut ModuleLoader<R>) -> Result<TypedModule, Error>
    where R: ModuleReader
{
    let ParseResult { imports, nodes } = tokenize_and_parse(input)?;
    for (import_token, module_id) in imports {
        loader.load_module(&module_id).map_err(|e| match e {
            ModuleLoaderError::WrappedError(e) => e,
            ModuleLoaderError::CannotLoadModule => Error::TypecheckerError(TypecheckerError::InvalidModuleImport {
                token: import_token,
                module_name: module_id.get_name(),
                circular: false,
            }),
            ModuleLoaderError::CircularDependency => Error::TypecheckerError(TypecheckerError::InvalidModuleImport {
                token: import_token,
                module_name: module_id.get_name(),
                circular: true,
            })
        })?
    }

    match typechecker::typechecker::typecheck(module_name, nodes, loader) {
        Err(e) => Err(Error::TypecheckerError(e)),
        Ok(module) => Ok(module)
    }
}

pub fn compile<R>(module_name: String, input: &String, module_reader: R) -> Result<(Module, Metadata), Error>
    where R: ModuleReader
{
    let mut loader = ModuleLoader::new(module_reader);
    let module = typecheck(module_name, input, &mut loader)?;
    let result = vm::compiler::compile(module).unwrap();
    Ok(result)
}

pub fn compile_and_disassemble<R>(module_name: String, input: &String, module_reader: R) -> Result<String, Error>
    where R: ModuleReader
{
    let (compiled_module, metadata) = compile(module_name, input, module_reader)?;
    Ok(vm::disassembler::disassemble(compiled_module, metadata))
}
