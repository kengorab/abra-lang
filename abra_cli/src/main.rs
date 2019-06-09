use std::string::ToString;
use abra_core::compile_and_run;

fn main() {
    let input = "func abc() = 123\nabc".to_string();
    if let Some(res) = compile_and_run(input) {
        println!("{}", res.to_string());
    }
}

