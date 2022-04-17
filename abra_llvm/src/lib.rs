use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;
use abra_core::module_loader::{ModuleLoader, ModuleReader};
use abra_core::parser::ast::ModuleId;
use abra_core::{Error, typecheck};
use crate::compiler::{ENTRY_FN_NAME, ModEntryFn};

#[cfg(test)]
mod compiler_tests;

mod compiler;

#[cfg(not(test))]
pub fn compile_to_llvm_and_run<R>(module_id: ModuleId, contents: &String, module_reader: &mut R) -> Result<(), Error>
    where R: ModuleReader
{
    let mut loader = ModuleLoader::new(module_reader);

    let module = typecheck(module_id, contents, &mut loader)?;
    loader.add_typed_module(module.clone());

    let context = Context::create();
    let llvm_module = compiler::Compiler::compile_module(&context, module, false).unwrap();

    let execution_engine = llvm_module.create_jit_execution_engine(OptimizationLevel::None).expect("Failed to initialize native target");
    unsafe {
        let entry_fn: JitFunction<'_, ModEntryFn> = execution_engine.get_function(ENTRY_FN_NAME).ok().unwrap();
        entry_fn.call();
    };

    Ok(())
}

#[cfg(test)]
pub fn compile_to_llvm_and_run<'ctx, R>(module_id: ModuleId, contents: &String, module_reader: &mut R, context: &'ctx Context) -> Result<String, Error>
    where R: ModuleReader
{
    use std::ffi::CStr;

    let mut loader = ModuleLoader::new(module_reader);

    let module = typecheck(module_id, contents, &mut loader)?;
    loader.add_typed_module(module.clone());

    let llvm_module = compiler::Compiler::compile_module(&context, module, true).unwrap();

    let execution_engine = llvm_module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("Failed to initialize native target");

    unsafe {
        let entry_fn: JitFunction<'_, ModEntryFn> = execution_engine.get_function(ENTRY_FN_NAME).ok().unwrap();
        let res = entry_fn.call();
        let res = CStr::from_ptr(res).to_str().unwrap().to_owned().to_string();
        Ok(res)
    }
}
