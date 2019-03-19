use crate::typechecker::typechecker::Typechecker;
use crate::typechecker::typechecker_error::TypecheckerError;
use crate::common::display_error::DisplayError;

mod common;
mod lexer;
mod parser;
mod typechecker;

fn main() {
    let input = "-+".to_string();
    let tokens = lexer::lexer::tokenize(&input);

    match parser::parser::parse(tokens) {
        Ok(ast) => {
            let typechecker = Typechecker {};
            match typechecker.typecheck(ast) {
                Ok(nodes) => println!("{:?}", nodes),
                Err(e) => eprintln!("{}", e.get_message(&input))
            }
        }
        Err(e) => eprintln!("{}", e.get_message(&input))
    }
}
