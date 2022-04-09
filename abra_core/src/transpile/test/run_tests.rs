use std::fs;
use std::path::PathBuf;
use std::process::Command;
use crate::common::fs_module_reader::FsModuleReader;
use crate::common::util::random_string;
use crate::compile_to_c;
use crate::parser::ast::ModuleId;
use crate::transpile::get_project_root::get_project_root;

// If empty, all tests will be run. If given the name of a test (eg. `primitives`), only that
// test will be run.
const ONLY: &str = "";

// Allow skipping of tests which temporarily fail
static SKIP: &[&str] = &["types"];

// If true, the locations of the built executables in the tmp directory will be printed. This could
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

    let working_dir = std::env::temp_dir().join(random_string(7));
    fs::create_dir(&working_dir)
        .map_err(|_| format!("Could not create tmp dir {}", working_dir.as_path().to_str().unwrap()))
        .unwrap();
    let dotabra_dir = PathBuf::from(&working_dir).join(".abra");
    fs::create_dir(&dotabra_dir)
        .map_err(|_| format!("Could not create tmp .abra dir {}", dotabra_dir.as_path().to_str().unwrap()))
        .unwrap();
    for case in test_cases {
        if SKIP.contains(&case.name.as_str()) {
            println!("Skipping {}", &case.name);
            continue;
        }

        let res = run_test(&working_dir, &dotabra_dir, &case);
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

fn run_test(working_dir: &PathBuf, dotabra_dir: &PathBuf, case: &TestCase) -> Option<String> {
    match read_test_case(&case) {
        Ok((contents, assertions)) => {
            match compile_and_run(&working_dir, dotabra_dir, case, &contents) {
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

fn compile_and_run(working_dir: &PathBuf, dotabra_dir: &PathBuf, case: &TestCase, input: &String) -> Result<String, String> {
    let module_id = ModuleId::parse_module_path(&format!("./{}", &case.name)).unwrap();

    let root = case.path.parent().unwrap().to_path_buf();
    let mut reader = FsModuleReader::new(module_id.clone(), &root);

    let exec_name = format!("{}", &case.name);
    if let Err(e) = compile_to_c(module_id, input, &root, &mut reader, &dotabra_dir, &exec_name) {
        return Err(format!("{:?}", e));
    }

    if PRINT_TMP_FILES {
        println!("Wrote executable: {}", working_dir.join(&exec_name).display());
    }

    let mut run_cmd = Command::new(dotabra_dir.join(exec_name).to_str().unwrap());
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

    fn walk(dir: PathBuf, cases: &mut Vec<TestCase>) {
        for item in dir.read_dir().unwrap() {
            let item = item.unwrap();

            if item.file_type().unwrap().is_dir() {
                walk(item.path(), cases);
                continue;
            }

            let file = item;
            let file_name = file.file_name().to_str().unwrap().to_string();
            if !file_name.ends_with(".abra") || file_name.starts_with('_') {
                continue;
            }

            let name = file_name.replace(".abra", "").to_string();
            let path = file.path();

            cases.push(TestCase { name, path });
        }
    }

    walk(test_cases_dir, &mut cases);

    if !ONLY.is_empty() {
        cases = cases.into_iter().filter(|c| c.name == ONLY).collect();
    }

    cases.sort_by(|c1, c2| c1.name.cmp(&c2.name));
    cases
}
