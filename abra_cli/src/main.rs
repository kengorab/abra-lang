use std::env;
use abra_core::{compile_and_run, Error, compile_and_disassemble};
use abra_core::common::display_error::DisplayError;
use abra_core::vm::value::Value;
use abra_core::vm::vm::VMContext;

fn main() {
    let args: Vec<String> = env::args().collect();

    // TODO: Handle when _not_ running via `cargo run`
    match args.get(1) {
        Some(file_name) => {
            match std::fs::read_to_string(file_name) {
                Ok(contents) => {
                    let ctx = VMContext {
                        print: |input| print!("{}\n", input)
                    };

                    match compile_and_run(contents.clone(), ctx) {
                        Ok(Some(res)) => match res {
                            Value::Nil => {}
                            res @ _ => println!("{}", res.to_string())
                        }
                        Err(error) => match error {
                            Error::LexerError(e) => eprintln!("{}", e.get_message(&contents)),
                            Error::ParseError(e) => eprintln!("{}", e.get_message(&contents)),
                            Error::TypecheckerError(e) => eprintln!("{}", e.get_message(&contents)),
                            Error::InterpretError(e) => eprintln!("{:?}", e),
                        }
                        _ => println!()
                    }
//                    match compile_and_disassemble(contents.clone()) {
//                        Ok(output) => {
//                            println!("{}", output)
//                        },
//                        Err(error) => match error {
//                            Error::LexerError(e) => eprintln!("{}", e.get_message(&contents)),
//                            Error::ParseError(e) => eprintln!("{}", e.get_message(&contents)),
//                            Error::TypecheckerError(e) => eprintln!("{}", e.get_message(&contents)),
//                            Error::InterpretError(e) => eprintln!("{:?}", e),
//                        }
//                    }
                }
                Err(err) => {
                    eprintln!("Could not read file {}: {}", file_name, err);
                    std::process::exit(1);
                }
            }
        }
        None => {
            println!("Usage: abra [file-name]");
            std::process::exit(1);
        }
    }
}

