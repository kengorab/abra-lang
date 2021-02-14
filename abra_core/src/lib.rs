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
use crate::parser::ast::ModuleId;

pub mod builtins;
pub mod common;
pub mod lexer;
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

pub enum ModuleLoaderError {
    WrappedError(Error),
    CannotLoadModule,
    CircularDependency,
}

pub trait ModuleLoader {
    fn load_module(&mut self, module_id: &ModuleId) -> Result<(), ModuleLoaderError>;
    fn get_module(&self, module_id: &ModuleId) -> &TypedModule;
}

pub fn typecheck<M>(module_name: String, input: &String, loader: &mut M) -> Result<TypedModule, Error>
    where M: ModuleLoader
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

pub fn compile<M>(module_path: String, input: &String, loader: &mut M) -> Result<(Module, Metadata), Error>
    where M: ModuleLoader
{
    let module = typecheck(module_path, input, loader)?;
    let result = vm::compiler::compile(module).unwrap();
    Ok(result)
}

pub fn compile_and_disassemble<M>(module_path: String, input: &String, loader: &mut M) -> Result<String, Error>
    where M: ModuleLoader
{
    let (compiled_module, metadata) = compile(module_path, input, loader)?;
    Ok(vm::disassembler::disassemble(compiled_module, metadata))
}
