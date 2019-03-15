mod lexer;
mod parser;

fn main() {
    let input = "   123 + -4.56".to_string();
    let tokens = lexer::lexer::tokenize(&input);

    println!("tokens: {:?}", tokens);

    let res = parser::parser::parse(tokens);
    println!("{:?}", res);
}
