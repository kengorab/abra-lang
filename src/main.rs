use crate::common::display_error::DisplayError;

mod common;
mod lexer;
mod parser;
mod typechecker;
mod vm;

fn main() {
    let input = "1".to_string();
    let tokens = lexer::lexer::tokenize(&input);

    match parser::parser::parse(tokens) {
        Err(e) => eprintln!("{}", e.get_message(&input)),
        Ok(ast) => {
            match typechecker::typechecker::typecheck(ast) {
                Err(e) => eprintln!("{}", e.get_message(&input)),
                Ok(nodes) => {
                    let chunk = vm::compiler::compile(nodes).unwrap();

                    println!("{:?}", chunk);
                }
            }
        }
    }
}
