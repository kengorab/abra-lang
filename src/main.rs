use crate::common::display_error::DisplayError;

mod common;
mod lexer;
mod parser;
mod typechecker;
mod vm;

fn main() {
    let input = "\"hello\" + 3.14 + \"world\" + 4 + \"!\"".to_string();

    match lexer::lexer::tokenize(&input) {
        Err(e) => eprintln!("{}", e.get_message(&input)),
        Ok(tokens) => match parser::parser::parse(tokens) {
            Err(e) => eprintln!("{}", e.get_message(&input)),
            Ok(ast) => {
                match typechecker::typechecker::typecheck(ast) {
                    Err(e) => eprintln!("{}", e.get_message(&input)),
                    Ok(nodes) => {
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
