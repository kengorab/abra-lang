use std::string::ToString;
use abra_core::compile_and_run;

fn main() {
    let input = "var a = 1\nfunc abc() { val c = 3 }\nval b = 2".to_string();
    if let Some(res) = compile_and_run(input) {
        println!("{}", res.to_string());
    }
}

