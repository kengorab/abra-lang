extern crate ansi_term;
#[macro_use]
extern crate clap;
extern crate dirs;
extern crate itertools;
extern crate rustyline;

use crate::repl::Repl;
use abra_core::common::fs_module_reader::FsModuleReader;
use abra_core::{compile, compile_and_disassemble, compile_to_c, Error};
use abra_core::builtins::common::to_string;
use abra_core::common::display_error::DisplayError;
use abra_core::module_loader::ModuleReader;
use abra_core::parser::ast::ModuleId;
use abra_core::vm::value::Value;
use abra_core::vm::vm::{VM, VMContext};
use abra_llvm::compile_to_llvm_and_run;
use std::path::PathBuf;
use std::process::Command;
use itertools::Either;
use abra_core::typechecker::typechecker2::{LoadModule, ModuleLoader, Project, Typechecker2};

mod repl;

#[derive(Clap)]
#[clap(name = "abra", version = crate_version ! ())]
struct Opts {
    #[clap(subcommand)]
    sub_cmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    Typecheck(RunOpts),
    Run(RunOpts),
    Compile(CompileOpts),
    Jit(JitOpts),
    Disassemble(DisassembleOpts),
    Test(TestOpts),
    Repl,
}

#[derive(Clap)]
struct RunOpts {
    #[clap(help = "Path to an abra file to run")]
    file_path: String,

    #[clap(last = true, help = "Arguments to pass to the abra program")]
    args: Vec<String>,
}

#[derive(Clap)]
struct CompileOpts {
    #[clap(help = "Path to an abra file to compile")]
    file_path: String,
}

#[derive(Clap)]
struct JitOpts {
    #[clap(help = "Path to an abra file to compile")]
    file_path: String,
}

#[derive(Clap)]
struct DisassembleOpts {
    #[clap(help = "Path to an abra file to disassemble")]
    file_path: String,

    #[clap(short = "o", help = "Where to write bytecode to (default: stdout)")]
    out_file: Option<String>,
}

#[derive(Clap)]
struct TestOpts {
    #[clap(help = "Path to a test file to run. If omitted, will run all tests in current directory matching test regex")]
    file_path: Option<String>,

    #[clap(short = "s", long = "show-passing", help = "Output passing tests in addition to failures (default: false)")]
    show_passing: bool,

    #[clap(short = "p", long = "pattern", help = "Glob pattern used to find tests to run (default: **/*_test.abra)")]
    test_pattern: Option<String>,
}

fn main() -> Result<(), ()> {
    let opts: Opts = Opts::parse();

    match opts.sub_cmd {
        SubCommand::Typecheck(opts) => cmd_typecheck2(opts),
        SubCommand::Run(opts) => cmd_compile_and_run(opts),
        SubCommand::Compile(opts) => cmd_compile_to_c_and_run(opts),
        SubCommand::Jit(opts) => cmd_compile_llvm_and_run(opts),
        SubCommand::Disassemble(opts) => cmd_disassemble(opts),
        SubCommand::Test(opts) => cmd_test(opts),
        SubCommand::Repl => Ok(Repl::run()),
    }
}

fn cmd_typecheck2(opts: RunOpts) -> Result<(), ()> {
    let current_path = std::env::current_dir().unwrap();
    let file_path = current_path.join(&opts.file_path);

    let root = file_path.parent().unwrap().to_path_buf();
    let module_name = file_path.file_name().unwrap().to_str().unwrap().to_string();
    let module_id = ModuleId::parse_module_path(&format!("./{}", module_name)).unwrap();

    let module_loader = ModuleLoader::new(&root);
    let mut project = Project::default();
    let mut tc = Typechecker2::new(&module_loader, &mut project);
    tc.typecheck_prelude();

    match tc.typecheck_module(&module_id) {
        Ok(_) => {}
        Err(e) => {
            let file_name = module_loader.resolve_path(&module_id);
            let contents = std::fs::read_to_string(&file_name).unwrap();

            match e {
                Either::Left(Either::Left(e)) => eprintln!("{}", e.get_message(&file_name, &contents)),
                Either::Left(Either::Right(e)) => eprintln!("{}", e.get_message(&file_name, &contents)),
                Either::Right(e) => eprintln!("{}", e.message(&project, &file_name, &contents)),
            }
            std::process::exit(1);
        }
    }

    // dbg!(&project);

    Ok(())
}

fn cmd_compile_and_run(opts: RunOpts) -> Result<(), ()> {
    let current_path = std::env::current_dir().unwrap();
    let file_path = current_path.join(&opts.file_path);
    let contents = read_file(&file_path)?;

    let root = file_path.parent().unwrap().to_path_buf();
    let module_name = file_path.file_name().unwrap().to_str().unwrap().to_string();
    let module_id = ModuleId::parse_module_path(&format!("./{}", module_name)).unwrap();

    let env = std::env::vars().collect();
    let mut vm = VM::new(VMContext::new(opts.args, env));
    let result = compile_and_run(module_id, contents, root, &mut vm)?;
    if result != Value::Nil {
        println!("{}", to_string(&result, &mut vm));
    }

    Ok(())
}

fn cmd_compile_to_c_and_run(opts: CompileOpts) -> Result<(), ()> {
    let current_path = std::env::current_dir().unwrap();
    let file_path = current_path.join(&opts.file_path);
    let contents = read_file(&file_path)?;

    let root = file_path.parent().unwrap().to_path_buf();
    let module_name = file_path.file_name().unwrap().to_str().unwrap().to_string();
    let module_id = ModuleId::parse_module_path(&format!("./{}", module_name)).unwrap();

    let working_dir = file_path.parent().unwrap();
    let dotabra_dir = working_dir.join(".abra");
    if !dotabra_dir.exists() {
        if std::fs::create_dir(&dotabra_dir).is_err() {
            eprintln!("{}", format!("Could not create .abra directory at {}", dotabra_dir.to_str().unwrap()));
            std::process::exit(1);
        }
    }

    let exec_name = format!("main_{}", module_name.replace(".abra", ""));
    let mut module_reader = FsModuleReader::new(module_id.clone(), &root);
    if let Err(e) = compile_to_c(module_id, &contents, &root, &mut module_reader, &dotabra_dir, &exec_name) {
        report_error(e, &module_reader);
    }

    let mut run_cmd = Command::new(dotabra_dir.join(exec_name).to_str().unwrap());
    let run_output = run_cmd.output().unwrap();
    if !run_output.status.success() {
        eprintln!("Error: {}", String::from_utf8(run_output.stderr).unwrap());
    }
    if !run_output.stdout.is_empty() {
        print!("{}", String::from_utf8(run_output.stdout).unwrap());
    }

    Ok(())
}

fn cmd_compile_llvm_and_run(opts: JitOpts) -> Result<(), ()> {
    let current_path = std::env::current_dir().unwrap();
    let file_path = current_path.join(&opts.file_path);
    let contents = read_file(&file_path)?;

    let root = file_path.parent().unwrap().to_path_buf();
    let module_name = file_path.file_name().unwrap().to_str().unwrap().to_string();
    let module_id = ModuleId::parse_module_path(&format!("./{}", module_name)).unwrap();

    let mut module_reader = FsModuleReader::new(module_id.clone(), &root);
    if let Err(e) = compile_to_llvm_and_run(module_id, &contents, &mut module_reader) {
        report_error(e, &module_reader);
    }

    Ok(())
}

fn cmd_disassemble(opts: DisassembleOpts) -> Result<(), ()> {
    let current_path = std::env::current_dir().unwrap();
    let file_path = current_path.join(&opts.file_path);
    let contents = read_file(&file_path)?;

    let module_id = ModuleId::parse_module_path(&opts.file_path).unwrap();
    let mut module_reader = FsModuleReader::new(module_id.clone(), &current_path);
    match compile_and_disassemble(module_id, &contents, &mut module_reader) {
        Ok(output) => {
            match opts.out_file {
                None => println!("{}", output),
                Some(out_file) => write_file(&out_file, output)?,
            }
        }
        Err(error) => report_error(error, &module_reader),
    };

    Ok(())
}

fn cmd_test(opts: TestOpts) -> Result<(), ()> {
    let mut current_path = std::env::current_dir().unwrap();
    let is_dir = if let Some(file_path) = &opts.file_path {
        let p = current_path.join(file_path.replace("./", ""));
        if !p.exists() {
            eprintln!("Cannot find file: {}", p.to_str().unwrap());
            std::process::exit(1);
        }

        let is_dir = p.is_dir();
        if is_dir { current_path = p; }
        is_dir
    } else {
        true
    };
    let current_path = current_path;

    let (root_dir, module_ids) = if !is_dir {
        let file_path = opts.file_path.unwrap().replace("./", "");
        let file_path = current_path.join(&file_path);

        let file_name = file_path.file_name().unwrap().to_str().unwrap();
        let module_id = ModuleId::parse_module_path(&file_name.to_string()).unwrap();

        let root_dir = file_path.to_str().unwrap().replace(file_name, "");
        let root_dir = PathBuf::from(root_dir);
        (root_dir, vec![module_id])
    } else {
        let test_pattern = opts.test_pattern.unwrap_or("**/*_test.abra".to_string());
        let glob_path = current_path.join(&test_pattern);
        let matches = match glob::glob(glob_path.to_str().unwrap()) {
            Ok(matches) => matches,
            Err(_) => {
                eprintln!("Invalid test pattern: '{}'; test patterns must be valid globs", test_pattern);
                std::process::exit(1);
            }
        };
        let module_ids = matches.into_iter()
            .map(|m| {
                let m = m.unwrap();
                let match_path = m.to_str().unwrap().to_string();
                let relative_path = match_path.replace(&format!("{}/", current_path.to_str().unwrap()), "");
                ModuleId::parse_module_path(&relative_path).unwrap()
            })
            .collect();
        (current_path, module_ids)
    };

    let mut mock_file = vec!["import runTests from \"test\"\n".to_string()];
    for m in module_ids {
        mock_file.push(format!("import * from \"{}\"\n", m.get_path(".")));
    }
    mock_file.push(format!("\nrunTests(showPassing: {})\n", opts.show_passing));
    let mock_file = mock_file.into_iter().collect::<String>();

    let mock_module_id = ModuleId::parse_module_path("./tests").unwrap();
    let mut vm = VM::new(VMContext::default());
    let result = compile_and_run(mock_module_id, mock_file, root_dir, &mut vm)?;
    match result {
        Value::Int(0) => Ok(()),
        _ => std::process::exit(1)
    }
}

fn compile_and_run(module_id: ModuleId, contents: String, root_dir: PathBuf, vm: &mut VM) -> Result<Value, ()> {
    let mut module_reader = FsModuleReader::new(module_id.clone(), &root_dir);
    let modules = match compile(module_id, &contents, &mut module_reader) {
        Ok(modules) => modules,
        Err(error) => report_error(error, &module_reader),
    };

    let mut result = Value::Nil;
    for module in modules {
        match vm.run(module) {
            Ok(v) => result = v,
            Err(e) => {
                eprintln!("{:?}", e);
                break;
            }
        };
    }
    Ok(result)
}

fn read_file(file_name: &PathBuf) -> Result<String, ()> {
    std::fs::read_to_string(file_name).map_err(|err| {
        eprintln!("Could not read file {}: {}", file_name.to_str().unwrap(), err);
        std::process::exit(1);
    })
}

fn write_file(file_name: &String, output: String) -> Result<(), ()> {
    std::fs::write(file_name, output).map_err(|err| {
        eprintln!("Could not write to file {}: {}", file_name, err);
        std::process::exit(1);
    })
}

fn report_error<R: ModuleReader>(e: Error, module_reader: &R) -> ! {
    let module_id = e.module_id();
    let file_name = PathBuf::from(module_reader.get_module_name(&module_id))
        .with_extension("abra")
        .canonicalize()
        .unwrap();
    let contents = std::fs::read_to_string(&file_name).unwrap();
    let file_name = file_name.to_str().unwrap().to_string();

    match e {
        Error::LexerError(e) => eprintln!("{}", e.get_message(&file_name, &contents)),
        Error::ParseError(e) => eprintln!("{}", e.get_message(&file_name, &contents)),
        Error::TypecheckerError(e) => eprintln!("{}", e.get_message(&file_name, &contents)),
        Error::InterpretError(_) => unreachable!("Compilation should not raise an InterpretError")
    }
    std::process::exit(1)
}
