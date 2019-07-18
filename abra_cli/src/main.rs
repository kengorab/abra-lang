use std::env;
use abra_core::{compile_and_run, Error};
use abra_core::common::display_error::DisplayError;

fn main() {
    let args: Vec<String> = env::args().collect();

    // TODO: Handle when _not_ running via `cargo run`
    match args.get(1) {
        Some(file_name) => {
            match std::fs::read_to_string(file_name) {
                Ok(contents) => {
                    match compile_and_run(contents.clone()) {
                        Ok(Some(res)) => println!("{}", res.to_string()),
                        Err(error) => match error {
                            Error::LexerError(e) => eprintln!("{}", e.get_message(&contents)),
                            Error::ParseError(e) => eprintln!("{}", e.get_message(&contents)),
                            Error::TypecheckerError(e) => eprintln!("{}", e.get_message(&contents)),
                            Error::InterpretError(e) => eprintln!("{:?}", e),
                        }
                        _ => println!()
                    }
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

