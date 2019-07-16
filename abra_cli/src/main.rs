use std::env;
use abra_core::compile_and_run;

fn main() {
    let args: Vec<String> = env::args().collect();

    // TODO: Handle when _not_ running via `cargo run`
    match args.get(1) {
        Some(file_name) => {
            match std::fs::read_to_string(file_name) {
                Ok(contents) => {
                    if let Some(res) = compile_and_run(contents) {
                        println!("{}", res.to_string());
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

