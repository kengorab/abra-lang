extern crate permutate;
extern crate strum;
#[macro_use]
extern crate strum_macros;

use crate::vm::value::Value;
use crate::vm::vm::VMContext;
use crate::vm::compiler::{Metadata, Module};
use crate::typechecker::typed_ast::TypedAstNode;
use crate::common::display_error::DisplayError;

pub mod builtins;
pub mod common;
pub mod lexer;
pub mod parser;
pub mod typechecker;
pub mod vm;

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

pub fn typecheck(input: String) -> Result<Vec<TypedAstNode>, Error> {
    match lexer::lexer::tokenize(&input) {
        Err(e) => Err(Error::LexerError(e)),
        Ok(tokens) => match parser::parser::parse(tokens) {
            Err(e) => Err(Error::ParseError(e)),
            Ok(ast) => {
                match typechecker::typechecker::typecheck(ast) {
                    Err(e) => Err(Error::TypecheckerError(e)),
                    Ok((_, nodes)) => Ok(nodes)
                }
            }
        }
    }
}

pub fn compile(input: String) -> Result<(Module, Metadata), Error> {
    let typed_ast_nodes = typecheck(input)?;
    let result = vm::compiler::compile(typed_ast_nodes).unwrap();
    Ok(result)
}

pub fn compile_and_run(input: String, ctx: VMContext) -> Result<Option<Value>, Error> {
    let (module, _) = compile(input)?;
    let mut vm = vm::vm::VM::new(module, ctx);
    match vm.run() {
        Ok(Some(v)) => Ok(Some(v)),
        Ok(None) => Ok(None),
        Err(e) => Err(Error::InterpretError(e)),
    }
}

pub fn compile_and_disassemble(input: String) -> Result<String, Error> {
    let (compiled_module, metadata) = compile(input)?;
    Ok(vm::disassembler::disassemble(compiled_module, metadata))
}
