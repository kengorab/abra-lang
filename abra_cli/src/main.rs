#[macro_use]
extern crate clap;

use abra_core::{compile_and_run, Error, compile_and_disassemble};
use abra_core::common::display_error::DisplayError;
use abra_core::vm::value::Value;
use abra_core::vm::vm::VMContext;

#[derive(Clap)]
#[clap(name = "abra", version = "0.0.1")]
struct Opts {
    #[clap(subcommand)]
    sub_cmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    Run(RunOpts),
    Disassemble(DisassembleOpts),
}

#[derive(Clap)]
struct RunOpts {
    file_name: String
}

#[derive(Clap)]
struct DisassembleOpts {
    file_name: String,

    #[clap(short = "o", help = "Where to write bytecode to (default: stdout)")]
    out_file: Option<String>,
}

fn main() -> Result<(), ()> {
    let opts: Opts = Opts::parse();

    match opts.sub_cmd {
        SubCommand::Run(opts) => cmd_compile_and_run(opts),
        SubCommand::Disassemble(opts) => cmd_disassemble(opts),
    }
}

fn cmd_compile_and_run(opts: RunOpts) -> Result<(), ()> {
    let contents = read_file(&opts.file_name)?;

    let ctx = VMContext {
        print: |input| print!("{}\n", input)
    };

    match compile_and_run(contents.clone(), ctx) {
        Ok(Some(res)) => match res {
            Value::Nil => {}
            res @ _ => println!("{}", res.to_string())
        }
        Err(error) => match error {
            Error::LexerError(e) => eprintln!("{}", e.get_message(&contents)),
            Error::ParseError(e) => eprintln!("{}", e.get_message(&contents)),
            Error::TypecheckerError(e) => eprintln!("{}", e.get_message(&contents)),
            Error::InterpretError(e) => eprintln!("{:?}", e),
        }
        _ => println!()
    };

    Ok(())
}

fn cmd_disassemble(opts: DisassembleOpts) -> Result<(), ()> {
    let contents = read_file(&opts.file_name)?;

    match compile_and_disassemble(contents.clone()) {
        Ok(output) => {
            match opts.out_file {
                None => println!("{}", output),
                Some(out_file) => write_file(&out_file, output)?,
            }
        }
        Err(error) => match error {
            Error::LexerError(e) => eprintln!("{}", e.get_message(&contents)),
            Error::ParseError(e) => eprintln!("{}", e.get_message(&contents)),
            Error::TypecheckerError(e) => eprintln!("{}", e.get_message(&contents)),
            Error::InterpretError(e) => eprintln!("{:?}", e),
        }
    };

    Ok(())
}

fn read_file(file_name: &String) -> Result<String, ()> {
    std::fs::read_to_string(file_name).map_err(|err| {
        eprintln!("Could not read file {}: {}", file_name, err);
        std::process::exit(1);
    })
}

fn write_file(file_name: &String, output: String) -> Result<(), ()> {
    std::fs::write(file_name, output).map_err(|err| {
        eprintln!("Could not write to file {}: {}", file_name, err);
        std::process::exit(1);
    })
}
