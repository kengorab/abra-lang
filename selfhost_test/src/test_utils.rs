use std::fs;
use std::collections::HashMap;
use std::env::temp_dir;
use std::path::Path;
use std::process::Command;
use assert_cmd::cargo::CommandCargoExt;
use similar::{TextDiff, ChangeTag};
use abra_core::common::util::{get_project_root, random_string};
use itertools::{EitherOrBoth, Itertools};

enum TestType {
    VsTxt(&'static str, &'static str),
}

pub struct TestRunner {
    runner_name: &'static str,
    bin_path: String,
    selfhosted_bin_path: Option<String>,
    tests: Vec<TestType>,
}

impl TestRunner {
    pub fn lexer_test_runner() -> Self {
        Self::test_runner("lexer", "lexer.test.abra", "lexer_test", true)
    }

    pub fn parser_test_runner() -> Self {
        Self::test_runner("parser", "parser.test.abra", "parser_test", true)
    }

    pub fn typechecker_test_runner() -> Self {
        Self::test_runner("typechecker", "typechecker.test.abra", "typechecker_test", true)
    }

    pub fn compiler_test_runner() -> CompilerTestRunner {
        CompilerTestRunner {tests: vec![]}
    }

    pub fn test_runner(runner_name: &'static str, src_file: &str, output_bin_file: &str, test_selfhosted: bool) -> Self {
        let bin_path = build_test_runner(&src_file, &output_bin_file);

        let selfhosted_bin_path = if test_selfhosted {
            let selfhosted_compiler_bin = build_test_runner("compiler.test.abra", &format!("selfhosted_compiler_for_{}", runner_name));

            let project_root = get_project_root().unwrap();
            let selfhost_dir = project_root.join("selfhost");
            let src_file = &selfhost_dir.join("src").join(&src_file);
            let abra_wrapper_script = selfhost_dir.join("abra");
            let output = Command::new(&abra_wrapper_script)
                .current_dir(&selfhost_dir)
                .env("COMPILER_BIN", &selfhosted_compiler_bin)
                .arg("build")
                .arg("-o")
                .arg(&output_bin_file)
                .arg(&src_file)
                .output()
                .unwrap();
            if !output.stderr.is_empty() {
                eprintln!("Failed to build test runner '{runner_name}'");
                eprintln!("  Failed to compile {src_file:?} using selfhosted compiler: {}", String::from_utf8(output.stderr).unwrap());
                panic!();
            }
            Some(selfhost_dir.join("._abra").join(&output_bin_file).to_str().unwrap().to_owned())
        } else {
            None
        };
        Self { runner_name, bin_path, selfhosted_bin_path, tests: vec![] }
    }

    pub fn add_test_vs_file(mut self, test_path: &'static str, txt_path: &'static str) -> Self {
        self.tests.push(TestType::VsTxt(test_path, txt_path));
        self
    }

    pub fn run_tests(self) {
        let project_root = get_project_root().unwrap();

        let selfhost_dir = project_root.join("selfhost");

        let abra_std_dir = project_root.join("abra_core/std");
        let abra_std_dir = abra_std_dir.to_str().unwrap();

        let Self { runner_name, bin_path, selfhosted_bin_path, tests } = self;

        let mut failures = vec![];
        for test in tests {
            let (test_path, expected_output) = match test {
                TestType::VsTxt(test_file_path, comparison_file_path) => {
                    let test_dir = selfhost_dir.join("test");
                    let test_path = test_dir.join(test_file_path);
                    let test_path = test_path.to_str().unwrap().to_string();
                    println!("Running {runner_name} test (vs file) {test_path}");

                    let comparison_path = test_dir.join(comparison_file_path);
                    let comparison = std::fs::read_to_string(&comparison_path).unwrap_or_else(|_| {
                        println!("No such file {}", &comparison_file_path);
                        panic!();
                    });
                    let comparison = comparison.replace("%FILE_NAME%", &test_path);

                    let test_dir_path = test_dir.to_str().unwrap().to_string();
                    let comparison = comparison.replace("%TEST_DIR%", &test_dir_path);

                    (test_path, comparison)
                }
            };

            let output = Command::new(&bin_path)
                .env("ABRA_HOME", &abra_std_dir)
                .arg(&test_path)
                .output()
                .unwrap();
            assert!(output.stderr.is_empty(), "Runtime error: {}", String::from_utf8(output.stderr).unwrap());
            let abra_output = String::from_utf8(output.stdout).unwrap();

            if expected_output != abra_output {
                eprintln!("  Difference detected between:");
                eprintln!("    (The expected output is the 'old' and abra output is the 'new')");
                let diff = TextDiff::from_lines(&expected_output, &abra_output);
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        ChangeTag::Equal => " ",
                        ChangeTag::Delete => "-",
                        ChangeTag::Insert => "+",
                    };
                    eprint!("  {sign}{change}");
                }
                if selfhosted_bin_path.is_some() {
                    failures.push(format!("{test_path} (not selfhost-compiled)"));
                } else {
                    failures.push(test_path.clone());
                }
            }

            if let Some(selfhosted_bin_path) = &selfhosted_bin_path {
                let output = Command::new(&selfhosted_bin_path)
                    .env("ABRA_HOME", &abra_std_dir)
                    .arg(&test_path)
                    .output()
                    .unwrap();
                assert!(output.stderr.is_empty(), "Runtime error: {}", String::from_utf8(output.stderr).unwrap());
                let abra_output = String::from_utf8(output.stdout).unwrap();

                if expected_output != abra_output {
                    eprintln!("  Difference detected between:");
                    eprintln!("    (The expected output is the 'old' and abra selfhosted output is the 'new')");
                    let diff = TextDiff::from_lines(&expected_output, &abra_output);
                    for change in diff.iter_all_changes() {
                        let sign = match change.tag() {
                            ChangeTag::Equal => " ",
                            ChangeTag::Delete => "-",
                            ChangeTag::Insert => "+",
                        };
                        eprint!("  {sign}{change}");
                    }
                    failures.push(format!("{test_path} (selfhost-compiled)"));
                }
            }
        }

        if !failures.is_empty() {
            eprintln!("Failures running {runner_name} tests:");
            for test_path in failures {
                eprintln!("  Test path '{}' failed", &test_path)
            }
            panic!("Failures running {} tests!", runner_name);
        } else {
            println!("All tests passed for {runner_name}!")
        }
    }
}

struct CompilerTest {
    test_path: &'static str,
    program_args: &'static [&'static str],
    env: &'static [(&'static str, &'static str)],
}

pub struct CompilerTestRunner {
    tests: Vec<CompilerTest>,
}

impl CompilerTestRunner {
    pub fn add_test(self, test_path: &'static str) -> Self {
        self.add_test_with_args_and_env(test_path, &[], &[])
    }

    pub fn add_test_with_args_and_env(mut self, test_path: &'static str, program_args: &'static [&'static str], env: &'static [(&'static str, &'static str)]) -> Self {
        self.tests.push(CompilerTest { test_path, program_args, env });
        self
    }

    pub fn run_tests(self) {
        let mut failures = vec![];
        let selfhost_dir = get_project_root().unwrap().join("selfhost");

        let compiler_test_bin = build_test_runner("compiler.test.abra", "compiler_test");
        let abra_wrapper_script = selfhost_dir.join("abra");

        for test in self.tests {
            let CompilerTest { test_path, program_args, env } = test;
            let test_path = selfhost_dir.join("test").join(test_path);
            let test_path = test_path.to_str().unwrap().to_string();
            let test_file = fs::read_to_string(&test_path).unwrap_or_else(|_| {
                println!("No such file {}", &test_path);
                panic!();
            });

            let output = Command::new(&abra_wrapper_script)
                .current_dir(&selfhost_dir)
                .env("COMPILER_BIN", &compiler_test_bin)
                .envs(env.to_vec().into_iter().collect::<HashMap<_, _>>())
                .arg(&test_path)
                .args(program_args)
                .output()
                .unwrap();
            if !output.stderr.is_empty() {
                eprintln!("Compilation error: {}", String::from_utf8(output.stderr).unwrap());
                failures.push(test_path);
                continue;
            }
            let output = String::from_utf8(output.stdout).unwrap();

            let prefix = "/// Expect: ";
            let expectations = test_file.lines()
                .map(|line| line.trim())
                .enumerate()
                .filter(|(_, line)| line.starts_with(prefix))
                .map(|(line_num, line)| (line_num + 1, line.replace(prefix, "")))
                .collect_vec();

            let output_lines = output.lines();
            for pair in expectations.iter().zip_longest(output_lines) {
                match pair {
                    EitherOrBoth::Both((line_num, expected), actual) => {
                        if expected != actual {
                            eprintln!("Expectation mismatch at {}:{} (expected '{}' but got '{}')", &test_path, line_num, expected, actual);
                            failures.push(test_path);
                            break;
                        }
                    }
                    EitherOrBoth::Left((line_num, expected)) => {
                        eprintln!("Expected: {} (line {}), but reached end of output", expected, line_num);
                        failures.push(test_path);
                        break;
                    }
                    EitherOrBoth::Right(actual) => {
                        eprintln!("Received line: {}, but there were no more expectations", actual);
                        failures.push(test_path);
                        break;
                    }
                }
            }
        }

        if !failures.is_empty() {
            eprintln!("Failures running compiler tests:");
            for test_path in failures {
                eprintln!("  Test path '{}' failed", &test_path)
            }
            panic!("Failures running compiler tests!");
        } else {
            println!("All tests passed for compiler!")
        }
    }
}

fn build_test_runner(runner_src_file: &str, output_bin_file: &str) -> String {
    let selfhost_dir = get_project_root().unwrap().join("selfhost");
    let build_dir = if let Some(test_temp_dir) = std::env::var("TEST_TMP_DIR").ok() {
        let dir = Path::new(&test_temp_dir).join(random_string(12));
        fs::create_dir(&dir).unwrap();
        dir
    } else {
        temp_dir()
    };

    println!("Building {}...", &runner_src_file);
    let output = Command::cargo_bin("abra").unwrap()
        .arg("build")
        .arg(&selfhost_dir.join("src").join(runner_src_file))
        .arg("-o")
        .arg(output_bin_file)
        .arg("-b")
        .arg(&build_dir)
        .output()
        .unwrap();
    assert!(output.stderr.is_empty(), "Compilation error: {}", String::from_utf8(output.stderr).unwrap());

    let runner_bin = build_dir.join(".abra").join(output_bin_file).to_str().unwrap().to_string();
    println!("...built {}", &runner_bin);

    runner_bin
}
