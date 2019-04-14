#[macro_use]
extern crate lazy_static;

use crate::common::display_error::DisplayError;

mod common;
mod lexer;
mod parser;
mod typechecker;
mod vm;

pub fn compile_and_run(input: String) {
    match lexer::lexer::tokenize(&input) {
        Err(e) => eprintln!("{}", e.get_message(&input)),
        Ok(tokens) => match parser::parser::parse(tokens) {
            Err(e) => eprintln!("{}", e.get_message(&input)),
            Ok(ast) => {
                match typechecker::typechecker::typecheck(ast) {
                    Err(e) => eprintln!("{}", e.get_message(&input)),
                    Ok((_, nodes)) => {
                        let chunk = vm::compiler::compile(nodes).unwrap();

                        let mut vm = vm::vm::VM::new(&chunk);
                        match vm.run() {
                            Ok(Some(v)) => println!("{}", v),
                            Ok(None) => println!(),
                            Err(e) => eprintln!("{:?}", e)
                        }
                    }
                }
            }
        }
    }
}
