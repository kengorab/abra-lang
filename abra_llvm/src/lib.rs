use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;
use abra_core::module_loader::{ModuleLoader, ModuleReader};
use abra_core::parser::ast::ModuleId;
use abra_core::{Error, typecheck};
use crate::compiler::{ENTRY_FN_NAME, ModEntryFn};

mod compiler;

pub fn compile_to_llvm<R>(module_id: ModuleId, contents: &String, module_reader: &mut R) -> Result<(), Error>
    where R: ModuleReader
{
    let mut loader = ModuleLoader::new(module_reader);

    let module = typecheck(module_id, contents, &mut loader)?;
    loader.add_typed_module(module.clone());

    let context = Context::create();
    let llvm_module = compiler::Compiler::compile_module(&context, module).unwrap();

    let execution_engine = llvm_module.create_jit_execution_engine(OptimizationLevel::None).expect("Failed to initialize native target");
    unsafe {
        let entry_fn: JitFunction<'_, ModEntryFn> = execution_engine.get_function(ENTRY_FN_NAME).ok().unwrap();
        println!("{}", entry_fn.call());
    };

    Ok(())
}
