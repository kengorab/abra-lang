use inkwell::context::Context;
use abra_core::common::test_utils::MockModuleReader;
use abra_core::module_loader::ModuleLoader;
use abra_core::parser::ast::ModuleId;
use crate::compile_to_llvm_and_run;

#[cfg(test)]
fn test_run_with_modules(input: &str, modules: Vec<(&str, &str)>) -> String {
    let mut mock_reader = MockModuleReader::new(modules);
    let mut mock_loader = ModuleLoader::new(&mut mock_reader);
    let module_id = ModuleId::parse_module_path("./test").unwrap();
    let module = crate::typecheck(module_id.clone(), &input.to_string(), &mut mock_loader)
        .map_err(|e| if let crate::Error::TypecheckerError(e) = e { e.kind } else { unreachable!() })
        .unwrap();
    mock_loader.add_typed_module(module);

    let context = Context::create();
    compile_to_llvm_and_run(module_id, &input.to_string(), &mut mock_reader, &context).unwrap()
}

#[test]
fn test_literals() {
    let res = test_run_with_modules("24", vec![]);
    assert_eq!(res, "24");

    let res = test_run_with_modules("24.6", vec![]);
    assert_eq!(res, "24.6");
}

#[test]
fn test_negation() {
    let res = test_run_with_modules("-24", vec![]);
    assert_eq!(res, "-24");

    let res = test_run_with_modules("-24.6", vec![]);
    assert_eq!(res, "-24.6");
}
