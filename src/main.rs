use crate::typechecker::typechecker::Typechecker;

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
                Err(e) => println!("{:?}", e)
            }
        }
        Err(e) => println!("{:?}", e)
    }
}
