#[macro_use]
extern crate lazy_static;
extern crate strum;
#[macro_use]
extern crate strum_macros;

use crate::vm::value::Value;
use crate::vm::chunk::CompiledModule;
use crate::vm::vm::VMContext;

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

pub fn compile(input: String) -> Result<CompiledModule<'static>, Error> {
    match lexer::lexer::tokenize(&input) {
        Err(e) => Err(Error::LexerError(e)),
        Ok(tokens) => match parser::parser::parse(tokens) {
            Err(e) => Err(Error::ParseError(e)),
            Ok(ast) => {
                match typechecker::typechecker::typecheck(ast) {
                    Err(e) => Err(Error::TypecheckerError(e)),
                    Ok((_, nodes)) => {
                        let chunk = vm::compiler::compile("<default>", nodes).unwrap();
                        Ok(chunk)
                    }
                }
            }
        }
    }
}

pub fn compile_and_run(input: String, ctx: VMContext) -> Result<Option<Value>, Error> {
    let mut compiled_module = compile(input)?;
    let mut vm = vm::vm::VM::new(&mut compiled_module, ctx);
    match vm.run() {
        Ok(Some(v)) => Ok(Some(v)),
        Ok(None) => Ok(None),
        Err(e) => Err(Error::InterpretError(e)),
    }
}
