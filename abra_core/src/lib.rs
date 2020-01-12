#[macro_use]
extern crate lazy_static;
extern crate strum;
#[macro_use]
extern crate strum_macros;

use crate::vm::value::Value;
use crate::vm::vm::VMContext;
use crate::vm::compiler::{Metadata, ObjFunction};

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

pub fn compile(input: String) -> Result<(ObjFunction, Metadata), Error> {
    match lexer::lexer::tokenize(&input) {
        Err(e) => Err(Error::LexerError(e)),
        Ok(tokens) => match parser::parser::parse(tokens) {
            Err(e) => Err(Error::ParseError(e)),
            Ok(ast) => {
                match typechecker::typechecker::typecheck(ast) {
                    Err(e) => Err(Error::TypecheckerError(e)),
                    Ok((_, nodes)) => {
                        let result = vm::compiler::compile(nodes).unwrap();
                        Ok(result)
                    }
                }
            }
        }
    }
}

pub fn compile_and_run(input: String, ctx: VMContext) -> Result<Option<Value>, Error> {
    let (top_level_fn, _) = compile(input)?;
    let mut vm = vm::vm::VM::new(top_level_fn, ctx);
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
