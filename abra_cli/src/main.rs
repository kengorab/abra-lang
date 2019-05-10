use std::string::ToString;
use abra_core::compile_and_run;

fn main() {
    let input = "[[1]][0] ?: [1]".to_string();
    if let Some(res) = compile_and_run(input) {
        println!("{}", res.to_string());
    }
}

