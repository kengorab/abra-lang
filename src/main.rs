use crate::typechecker::typechecker::Typechecker;

mod common;
mod lexer;
mod parser;
mod typechecker;

fn main() {
    let input = "-4.56 + 12 / 4".to_string();
    let tokens = lexer::lexer::tokenize(&input);

    let res = parser::parser::parse(tokens);

    let typechecker = Typechecker {};
    let nodes = typechecker.typecheck(res.unwrap()).unwrap();
    println!("{:?}", nodes);
}
