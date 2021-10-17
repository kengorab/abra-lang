use std::fs;
use std::path::PathBuf;
use std::process::Command;
use crate::common::display_error::DisplayError;
use crate::common::test_utils::MockModuleReader;
use crate::common::util::random_string;
use crate::module_loader::ModuleLoader;
use crate::parser::ast::ModuleId;
use crate::transpile::clang::clang;
use crate::transpile::genc::CCompiler;
use crate::transpile::get_project_root::get_project_root;

// If empty, all tests will be run. If given the name of a test (eg. `primitives`), only that
// test will be run.
const ONLY: &str = "";

// If true, the locations of the generated c files in the tmp directory will be printed. This could
// aid in debugging.
const PRINT_TMP_FILES: bool = false;

#[test]
pub fn run_tests() {
    let mut results = Vec::new();

    let test_cases = find_test_cases();
    if test_cases.len() == 1 && !ONLY.is_empty() {
        println!("Only running 1 test: {}", ONLY);
    } else if test_cases.is_empty() {
        if !ONLY.is_empty() {
            panic!("No test cases to run! Does ONLY match up with a file in the cases directory?");
        } else {
            panic!("No test cases to run! Are there files in the cases directory?")
        }
    } else {
        println!("Running tests...");
    }

    for case in test_cases {
        let res = run_test(&case);
        results.push((case.name, res));
    }

    println!("\nResults:");
    let mut has_failure = false;
    for (name, msg) in results {
        println!("  {}: {}", name, if msg.is_none() { "✔" } else { "ⅹ" });
        if let Some(msg) = msg {
            has_failure = true;
            println!("    {}", msg);
        }
    }
    if has_failure { panic!("Encountered test failures for native target 'C'"); }
}

fn run_test(case: &TestCase) -> Option<String> {
    match read_test_case(&case) {
        Ok((contents, assertions)) => {
            match compile_and_run(&case, &contents) {
                Ok(output) => {
                    if output != assertions {
                        let err = format!("Expected:\n{}\n    Got:\n{}", assertions, output);
                        Some(err)
                    } else { None }
                }
                Err(err) => Some(err),
            }
        }
        Err(err) => Some(err)
    }
}

fn compile_and_run(case: &TestCase, input: &String) -> Result<String, String> {
    let mock_reader = MockModuleReader::new(vec![]);
    let mut mock_loader = ModuleLoader::new(&mock_reader);
    let module_id = ModuleId::from_name(&case.name);
    let module = crate::typecheck(module_id, &input.to_string(), &mut mock_loader).map_err(|e|
        if let crate::Error::TypecheckerError(e) = e {
            e.get_message(&case.path.to_str().unwrap().to_string(), input)
        } else { unreachable!() }
    )?;
    let typed_ast = module.typed_nodes;

    let working_dir = std::env::temp_dir().join(random_string(7));
    fs::create_dir(&working_dir)
        .map_err(|_| format!("Could not create tmp dir {}", working_dir.as_path().to_str().unwrap()))?;

    let c_code = CCompiler::gen_c(typed_ast).unwrap();

    let src_file = format!("{}.c", &case.name);
    let out_file = &case.name;
    std::fs::write(working_dir.join(&src_file), c_code).unwrap();
    if PRINT_TMP_FILES {
        println!("Wrote {}", working_dir.join(&src_file).display());
    }

    if let Err(e) = clang(&working_dir, &src_file, &out_file) {
        return Err(e);
    }

    let mut run_cmd = Command::new(working_dir.join(out_file).to_str().unwrap());
    let run_output = run_cmd.output().unwrap();
    if !run_output.status.success() {
        return Err(String::from_utf8(run_output.stderr).unwrap());
    }

    let stdout = String::from_utf8(run_output.stdout).unwrap();
    Ok(stdout)
}

struct TestCase {
    name: String,
    path: PathBuf,
}

fn read_test_case(case: &TestCase) -> Result<(String, String), String> {
    let contents = fs::read_to_string(&case.path)
        .map_err(|_| format!("Could not read file {}", &case.path.as_path().to_str().unwrap()))?;
    let mut split = contents.split("/* assert(stdout):");

    let source = split.next().expect("Even if no assertions are present, this will not be None").to_string();

    let assertion = split.next().ok_or("File contained no assertions")?;
    let assertion = format!("{}\n", assertion.to_string().replace("*/", "").trim());

    Ok((source, assertion))
}

fn find_test_cases() -> Vec<TestCase> {
    let project_root = get_project_root().unwrap();
    let test_cases_dir = project_root.join("abra_core").join("src").join("transpile").join("test").join("cases");
    assert!(test_cases_dir.exists() && test_cases_dir.is_dir());

    let mut cases = Vec::new();
    for file in test_cases_dir.read_dir().unwrap() {
        let file = file.unwrap();
        let name = file.file_name().to_str().unwrap().replace(".abra", "").to_string();
        let path = file.path();

        if !ONLY.is_empty() {
            if ONLY == name {
                cases.push(TestCase { name, path });
                break;
            }
            continue;
        }

        cases.push(TestCase { name, path });
    }

    cases
}
