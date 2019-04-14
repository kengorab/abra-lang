use std::string::ToString;
use abra_core::compile_and_run;

fn main() {
    let input = "val abc = 3\nabc = 5".to_string();
    compile_and_run(input);
}

