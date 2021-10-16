use std::path::PathBuf;
use std::process::Command;
use crate::transpile::get_project_root::get_project_root;

pub fn gcc<S: AsRef<str>>(dotabra_dir: &PathBuf, src_file: S, out_file: S) -> Result<(), String> {
    let src_file = join_path(dotabra_dir, src_file.as_ref());
    let out_file = join_path(dotabra_dir, out_file.as_ref());

    let project_root = get_project_root().map_err(|_| "Could not determine project root; could not locate include files".to_string())?;
    let abra_base_path = target_path(&project_root, "abra");
    let libgc_base_path = target_path(&project_root, "libgc");

    println!("/usr/local/bin/clang -v");
    let output = Command::new("/usr/local/bin/clang")
        .arg("-v")
        .output()
        .unwrap();
    println!("{}", String::from_utf8(output.stdout).unwrap());
    println!("{}", String::from_utf8(output.stderr).unwrap());

    println!("/usr/bin/clang -v");
    let output = Command::new("/usr/bin/clang")
        .arg("-v")
        .output()
        .unwrap();
    println!("{}", String::from_utf8(output.stdout).unwrap());
    println!("{}", String::from_utf8(output.stderr).unwrap());

    println!("gcc -v");
    let output = Command::new("gcc")
        .arg("-v")
        .output()
        .unwrap();
    println!("{}", String::from_utf8(output.stdout).unwrap());
    println!("{}", String::from_utf8(output.stderr).unwrap());

    let output = Command::new("/usr/local/bin/clang")
        .arg(src_file)
        .arg("-o").arg(out_file)
        .arg(format!("-I{}", join_path(&abra_base_path, "include")))
        .arg(format!("-I{}", join_path(&libgc_base_path, "include")))
        .arg(format!("-L{}", join_path(&libgc_base_path, "lib")))
        .arg("-lgc")
        .output()
        .unwrap();
    if !output.status.success() {
        return Err(String::from_utf8(output.stderr).unwrap());
    }

    Ok(())
}

fn join_path<S: AsRef<str>>(pwd: &PathBuf, file: S) -> String {
    pwd.join(file.as_ref()).as_path().to_str().unwrap().to_string()
}

fn target_path<S: AsRef<str>>(project_root_dir: &PathBuf, dir: S) -> PathBuf {
    project_root_dir.join("abra_core").join("src").join("transpile").join("target").join("c").join(dir.as_ref())
}
