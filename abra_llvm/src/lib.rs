use std::{env, fs, io};
use std::fs::read_dir;
use std::io::ErrorKind;
use std::path::PathBuf;
use std::process::{Command, Output};
use inkwell::context::Context;
use inkwell::module::Module;
use abra_core::module_loader::{ModuleLoader, ModuleReader};
use abra_core::parser::ast::ModuleId;
use abra_core::{Error, typecheck};
use abra_core::common::util::random_string;

#[cfg(test)]
mod compiler_tests;

mod compiler;

const ADDL_PRELUDE_CONTENTS: &str = r#"
type Process_ {
  args: String[]
  env: Map<String, String>
}
"#;

#[cfg(not(test))]
pub fn compile_to_llvm_and_run<R>(module_id: ModuleId, contents: &String, module_reader: &mut R) -> Result<(), Error>
    where R: ModuleReader
{
    use std::io::Write;

    let mut loader = ModuleLoader::new(module_reader);

    let supplemental_prelude_module = typecheck(ModuleId::External("prelude.1".to_string()), &ADDL_PRELUDE_CONTENTS.to_string(), &mut loader)?;

    let module = typecheck(module_id, contents, &mut loader)?;
    loader.add_typed_module(module.clone());

    let mod_idxs = loader.ordering.iter().enumerate().map(|(idx, mod_id)| (mod_id.clone(), idx)).collect();

    let context = Context::create();
    let mut compiler = compiler::Compiler::new(&context, &mod_idxs);
    compiler.initialize(supplemental_prelude_module);

    let num_mods = loader.ordering.len();
    for (mod_idx, module_id) in loader.ordering.iter().enumerate() {
        let typed_module = loader.get_module(module_id).clone();
        let is_main = mod_idx == num_mods - 1;
        compiler.compile_module(typed_module, mod_idx, is_main).unwrap();
    }
    let llvm_module = compiler.finish();

    match compile_and_run(&llvm_module) {
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
        Ok(output) => {
            io::stdout().write_all(&output.stdout).unwrap();
            io::stderr().write_all(&output.stderr).unwrap();
            Ok(())
        }
    }
}

#[cfg(test)]
pub fn compile_to_llvm_and_run<'ctx, R>(module_id: ModuleId, contents: &String, module_reader: &mut R, context: &'ctx Context) -> Result<String, Error>
    where R: ModuleReader
{
    let mut loader = ModuleLoader::new(module_reader);

    let supplemental_prelude_module = typecheck(ModuleId::External("prelude.1".to_string()), &ADDL_PRELUDE_CONTENTS.to_string(), &mut loader)?;

    let module = typecheck(module_id, contents, &mut loader)?;
    loader.add_typed_module(module.clone());

    let mod_idxs = loader.ordering.iter().enumerate().map(|(idx, mod_id)| (mod_id.clone(), idx)).collect();

    let mut compiler = compiler::Compiler::new(&context, &mod_idxs);
    compiler.initialize(supplemental_prelude_module);
    let num_mods = loader.ordering.len();
    for (mod_idx, module_id) in loader.ordering.iter().enumerate() {
        let typed_module = loader.get_module(module_id).clone();
        let is_main = mod_idx == num_mods - 1;
        compiler.compile_module(typed_module, mod_idx, is_main).unwrap();
    }
    let llvm_module = compiler.finish();

    let output = compile_and_run(&llvm_module).unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();
    Ok(stdout)
}

pub fn get_project_root() -> io::Result<PathBuf> {
    let path = env::current_dir()?;
    let mut path_ancestors = path.as_path().ancestors();

    while let Some(p) = path_ancestors.next() {
        let has_cargo = read_dir(p)?
            .into_iter()
            .any(|p| p.unwrap().file_name().to_str() == Some("Cargo.lock"));
        if has_cargo {
            return Ok(PathBuf::from(p));
        }
    }
    Err(io::Error::new(ErrorKind::NotFound, "Ran out of places to find Cargo.toml"))
}

fn join_path<S: AsRef<str>>(pwd: &PathBuf, file: S) -> String {
    pwd.join(file.as_ref()).as_path().to_str().unwrap().to_string()
}

fn compile_and_run(llvm_module: &Module) -> Result<Output, String> {
    let working_dir = env::temp_dir().join(random_string(7));
    fs::create_dir(&working_dir).map_err(|_| format!("Could not create tmp dir {}", working_dir.as_path().to_str().unwrap()))?;

    let ll_file = working_dir.join("module.ll");
    llvm_module.print_to_file(&ll_file).map_err(|e| e.to_string())?;

    let project_root = get_project_root().map_err(|_| "Could not determine project root; could not locate include files".to_string())?;
    let ext_base_path = project_root.join("abra_llvm").join("ext");

    let src_file_path = ll_file.as_path().to_str().unwrap();
    let out_file_path = join_path(&working_dir, "module");
    let wrapper_file_path = join_path(&ext_base_path, "_wrapper.c");
    let runtime_file_path = join_path(&ext_base_path, "rt.c");
    let libgc_lib_path = join_path(&ext_base_path.join("libgc").join("lib"), "libgc.a");
    let libgc_header_path = join_path(&ext_base_path.join("libgc"), "include");

    // println!("{}", &out_file_path);

    let output = Command::new("clang")
        .arg(src_file_path)
        .arg(runtime_file_path)
        .arg(wrapper_file_path)
        .arg(libgc_lib_path)
        .arg("-o").arg(&out_file_path)
        .arg(format!("-I{}", libgc_header_path))
        .arg("-lm")
        .output()
        .map_err(|e| format!("clang error: {}", e.to_string()))?;
    if !output.status.success() {
        return Err(String::from_utf8(output.stderr).unwrap());
    }

    Command::new(out_file_path)
        .output()
        .map_err(|e| format!("exec error: {}", e.to_string()))
}
