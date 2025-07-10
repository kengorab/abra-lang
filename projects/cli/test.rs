use std::fs::OpenOptions;
use std::os::unix::fs::OpenOptionsExt;
//use std::fs::File;

fn main() {
    let file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .mode(0o644)
        .open("/Users/kennethg/Desktop/abra-lang/projects/cli/._abra/example2.ssa");
    //let file = File::create("/Users/kennethg/Desktop/abra-lang/projects/cli/._abra/example2.ssa");
    
    match file {
        Ok(_) => println!("Rust succeeded"),
        Err(e) => println!("Rust failed: {}", e),
    }
}
