extern crate abra_native;
extern crate itertools;
extern crate strum;
#[macro_use]
extern crate strum_macros;

use std::path::PathBuf;
use crate::vm::compiler::{Module, Metadata};
use crate::typechecker::typechecker::TypedModule;
use crate::common::display_error::DisplayError;
use crate::lexer::tokens::Range;
use crate::parser::parser::ParseResult;
use crate::typechecker::typechecker_error::{TypecheckerErrorKind, TypecheckerError};
use crate::module_loader::{ModuleLoader, ModuleReader, ModuleLoaderError};
use crate::parser::ast::ModuleId;
use crate::transpile::clang::clang;
use crate::transpile::genc::{CCompiler, normalize_module_name};

pub mod builtins;
pub mod common;
pub mod lexer;
pub mod module_loader;
pub mod parser;
pub mod transpile;
pub mod typechecker;
pub mod vm;

#[derive(Debug)]
pub enum Error {
    LexerError(lexer::lexer_error::LexerError),
    ParseError(parser::parse_error::ParseError),
    TypecheckerError(typechecker::typechecker_error::TypecheckerError),
    InterpretError(vm::vm::InterpretError),
}

impl Error {
    pub fn module_id(&self) -> &ModuleId {
        match self {
            Error::LexerError(e) => &e.module_id,
            Error::ParseError(e) => &e.module_id,
            Error::TypecheckerError(e) => &e.module_id,
            Error::InterpretError(_) => unreachable!()
        }
    }

    pub fn get_range(&self) -> Range {
        match self {
            Error::LexerError(e) => e.get_range(),
            Error::ParseError(e) => e.get_range(),
            Error::TypecheckerError(e) => e.get_range(),
            Error::InterpretError(_) => unreachable!()
        }
    }
}

impl DisplayError for Error {
    fn message_for_error(&self, file_name: &String, lines: &Vec<&str>) -> String {
        match self {
            Error::LexerError(e) => e.message_for_error(file_name, lines),
            Error::ParseError(e) => e.message_for_error(file_name, lines),
            Error::TypecheckerError(e) => e.message_for_error(file_name, lines),
            Error::InterpretError(_) => "Runtime error!".to_string()
        }
    }
}

fn tokenize_and_parse(module_id: &ModuleId, input: &String) -> Result<ParseResult, Error> {
    match lexer::lexer::tokenize(module_id, input) {
        Err(e) => Err(Error::LexerError(e)),
        Ok(tokens) => match parser::parser::parse(module_id.clone(), tokens) {
            Err(e) => Err(Error::ParseError(e)),
            Ok(nodes) => Ok(nodes)
        }
    }
}

fn tokenize_and_parse_stub(module_id: &ModuleId, input: &String) -> Result<ParseResult, Error> {
    match lexer::lexer::tokenize(module_id, input) {
        Err(e) => Err(Error::LexerError(e)),
        Ok(tokens) => match parser::parser::parse_stub(module_id.clone(), tokens) {
            Err(e) => Err(Error::ParseError(e)),
            Ok(nodes) => Ok(nodes)
        }
    }
}

pub fn typecheck<R>(module_id: ModuleId, input: &String, loader: &mut ModuleLoader<R>) -> Result<TypedModule, Error>
    where R: ModuleReader
{
    let ParseResult { imports, nodes } = tokenize_and_parse(&module_id, input)?;
    for (import_token, import_module_id) in imports {
        loader.load_module(&module_id, &import_module_id).map_err(|e| match e {
            ModuleLoaderError::WrappedError(e) => e,
            ModuleLoaderError::NoSuchModule => {
                let module_name = loader.get_module_name(&import_module_id);
                let kind = TypecheckerErrorKind::InvalidModuleImport { token: import_token, module_name };
                Error::TypecheckerError(TypecheckerError { module_id: module_id.clone(), kind })
            }
            ModuleLoaderError::CircularDependency => {
                let module_name = loader.get_module_name(&import_module_id);
                let kind = TypecheckerErrorKind::CircularModuleImport { token: import_token, module_name };
                Error::TypecheckerError(TypecheckerError { module_id: import_module_id, kind })
            }
        })?
    }

    match typechecker::typechecker::typecheck(module_id, nodes, loader) {
        Err(e) => Err(Error::TypecheckerError(e)),
        Ok(module) => Ok(module)
    }
}

pub fn compile<R>(module_id: ModuleId, input: &String, module_reader: &mut R) -> Result<Vec<Module>, Error>
    where R: ModuleReader
{
    let mut loader = ModuleLoader::new(module_reader);
    let module = typecheck(module_id, input, &mut loader)?;
    loader.add_typed_module(module);

    loader.compile_all();

    let modules = loader.compiled_modules.into_iter()
        .map(|(module, _)| module)
        .collect();
    Ok(modules)
}

pub fn compile_and_disassemble<R>(module_id: ModuleId, input: &String, module_reader: &mut R) -> Result<String, Error>
    where R: ModuleReader
{
    let mut loader = ModuleLoader::new(module_reader);
    let module = typecheck(module_id, input, &mut loader)?;
    loader.add_typed_module(module);

    loader.compile_all();

    let modules = loader.compiled_modules.into_iter()
        .map(|(module, metadata)| (module, metadata.unwrap_or(Metadata::default())))
        .collect();
    let dis = vm::disassembler::disassemble(modules);
    Ok(dis)
}

pub fn compile_to_c<R>(
    module_id: ModuleId,
    contents: &String,
    root: &PathBuf,
    module_reader: &mut R,
    dotabra_dir: &PathBuf,
    exec_name: &String,
) -> Result<(), Error>
    where R: ModuleReader
{
    let mut loader = ModuleLoader::new(module_reader);

    let module = typecheck(module_id, contents, &mut loader)?;
    loader.add_typed_module(module);

    let mut main_file_buf = vec!["#include \"abra.h\"".to_string()];
    let dependencies = loader.ordering.clone();
    for m in &dependencies {
        let include_line = match m {
            ModuleId::External(n) => format!("#include \"modules/{}/_mod.h\"", n),
            ModuleId::Internal(_) => {
                let name = loader.get_module_name(&m);
                let module_name = normalize_module_name(&name, &root);

                let typed_module = loader.get_module_by_name(&name);
                let ast = typed_module.typed_nodes.clone();

                let gen_src_file = format!("{}.c", &module_name);
                let c_code = CCompiler::gen_c(&mut loader, &root, &module_name, ast).unwrap();
                std::fs::write(dotabra_dir.join(&gen_src_file), c_code).unwrap();

                format!("#include \"./{}\"", gen_src_file)
            }
        };
        main_file_buf.push(include_line);
    }

    main_file_buf.push("int main(int argc, char** argv) {".to_string());
    main_file_buf.push("abra_init();".to_string());

    for m in &dependencies {
        let normalized_module_name = match m {
            ModuleId::External(n) => n.clone(),
            ModuleId::Internal(_) => {
                let name = loader.get_module_name(&m);
                normalize_module_name(&name, &root)
            }
        };
        main_file_buf.push(format!("init_module_{}();", normalized_module_name));
    }
    main_file_buf.push("return 0;\n}".to_string());

    let main_src_file_name = format!("__{}__.c", exec_name);
    std::fs::write(dotabra_dir.join(&main_src_file_name), main_file_buf.join("\n")).unwrap();

    if let Err(e) = clang(&dotabra_dir, &main_src_file_name, &exec_name) {
        eprintln!("{}", e);
        std::process::exit(1);
    }

    Ok(())
}
