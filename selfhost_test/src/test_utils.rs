use std::{fs, io};
use std::env::temp_dir;
use std::io::BufWriter;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use assert_cmd::cargo::CommandCargoExt;
use abra_core::lexer;
use abra_core::lexer::tokens::Token;
use abra_core::parser::ast::ModuleId;
use similar::{TextDiff, ChangeTag};
use abra_core::common::display_error::DisplayError;
use abra_core::common::util::{get_project_root, random_string};
use itertools::{EitherOrBoth, Itertools};

enum TestType {
    VsRust(&'static str),
    VsTxt(&'static str, &'static str),
}

pub struct TestRunner {
    runner_name: &'static str,
    bin_path: String,
    tests: Vec<TestType>,
}

impl TestRunner {
    pub fn lexer_test_runner() -> Self {
        Self::test_runner("lexer", "lexer.test.abra", "lexer_test")
    }

    pub fn parser_test_runner() -> Self {
        Self::test_runner("parser", "parser.test.abra", "parser_test")
    }

    pub fn typechecker_test_runner() -> Self {
        Self::test_runner("typechecker", "typechecker.test.abra", "typechecker_test")
    }

    pub fn compiler_test_runner() -> CompilerTestRunner {
        CompilerTestRunner {tests: vec![]}
    }

    pub fn test_runner(runner_name: &'static str, src_file: &str, output_bin_file: &str) -> Self {
        let bin_path = build_test_runner(src_file, output_bin_file);
        Self { runner_name, bin_path, tests: vec![] }
    }

    pub fn add_test_vs_rust(mut self, test_path: &'static str) -> Self {
        self.tests.push(TestType::VsRust(test_path));
        self
    }

    pub fn add_test_vs_txt(mut self, test_path: &'static str, txt_path: &'static str) -> Self {
        self.tests.push(TestType::VsTxt(test_path, txt_path));
        self
    }

    pub fn run_tests(self) {
        let project_root = get_project_root().unwrap();

        let selfhost_dir = project_root.join("selfhost");

        let abra_std_dir = project_root.join("abra_core/std");
        let abra_std_dir = abra_std_dir.to_str().unwrap();

        let Self { runner_name, bin_path, tests } = self;

        let mut failures = vec![];
        for test in tests {
            let (test_path, expected_output) = match test {
                TestType::VsRust(test_file_path) => {
                    let test_path = selfhost_dir.join("test").join(test_file_path);
                    let test_path = test_path.to_str().unwrap().to_string();
                    println!("Running {runner_name} test (vs rust) {test_path}");

                    let module_id = ModuleId::parse_module_path("./test").unwrap();
                    let contents = std::fs::read_to_string(&test_path).unwrap();
                    let rust_output = match lexer::lexer::tokenize(&module_id, &contents) {
                        Ok(tokens) => tokens_to_json(&tokens).unwrap(),
                        Err(err) => err.get_message(&test_path, &contents)
                    };

                    (test_path, rust_output)
                }
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
                failures.push(test_path);
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

pub struct CompilerTestRunner {
    tests: Vec<&'static str>,
}

impl CompilerTestRunner {
    pub fn add_test(mut self, test_path: &'static str) -> Self {
        self.tests.push(test_path);
        self
    }

    pub fn run_tests(self) {
        let mut failures = vec![];
        let selfhost_dir = get_project_root().unwrap().join("selfhost");

        let compiler_test_bin = build_test_runner("compiler.test.abra", "compiler_test");
        let abra_wrapper_script = selfhost_dir.join("abra");

        for test_file_path in self.tests {
            let test_path = selfhost_dir.join("test").join(test_file_path);
            let test_path = test_path.to_str().unwrap().to_string();
            let test_file = fs::read_to_string(&test_path).unwrap_or_else(|_| {
                println!("No such file {}", &test_path);
                panic!();
            });

            let output = Command::new(&abra_wrapper_script)
                .env("COMPILER_BIN", &compiler_test_bin)
                .arg(&test_path)
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

fn tokens_to_json(tokens: &Vec<Token>) -> io::Result<String> {
    let mut buf = BufWriter::new(Vec::new());

    writeln!(&mut buf, "[")?;
    let len = tokens.len();
    for (idx, token) in tokens.iter().enumerate() {
        writeln!(&mut buf, "  {{")?;
        let pos = token.get_position();
        writeln!(&mut buf, "    \"position\": [{}, {}],", pos.line, pos.col)?;
        writeln!(&mut buf, "    \"kind\": {{")?;
        match token {
            Token::Int(_, val) => {
                writeln!(&mut buf, "      \"name\": \"Int\",")?;
                writeln!(&mut buf, "      \"value\": {}", val)?;
            }
            Token::Float(_, val) => {
                writeln!(&mut buf, "      \"name\": \"Float\",")?;
                writeln!(&mut buf, "      \"value\": {}", val)?;
            }
            Token::String(_, val) => {
                writeln!(&mut buf, "      \"name\": \"String\",")?;
                writeln!(&mut buf, "      \"value\": \"{}\"", val)?;
            }
            Token::StringInterp(_, _) => todo!(),
            Token::Bool(_, val) => {
                writeln!(&mut buf, "      \"name\": \"Bool\",")?;
                writeln!(&mut buf, "      \"value\": {}", val)?;
            }
            Token::Func(_) => writeln!(&mut buf, "      \"name\": \"Func\"")?,
            Token::Val(_) => writeln!(&mut buf, "      \"name\": \"Val\"")?,
            Token::Var(_) => writeln!(&mut buf, "      \"name\": \"Var\"")?,
            Token::If(_) => writeln!(&mut buf, "      \"name\": \"If\"")?,
            Token::Else(_) => writeln!(&mut buf, "      \"name\": \"Else\"")?,
            Token::While(_) => writeln!(&mut buf, "      \"name\": \"While\"")?,
            Token::Break(_) => writeln!(&mut buf, "      \"name\": \"Break\"")?,
            Token::Continue(_) => writeln!(&mut buf, "      \"name\": \"Continue\"")?,
            Token::For(_) => writeln!(&mut buf, "      \"name\": \"For\"")?,
            Token::In(_) => writeln!(&mut buf, "      \"name\": \"In\"")?,
            Token::Match(_) => writeln!(&mut buf, "      \"name\": \"Match\"")?,
            Token::Type(_) => writeln!(&mut buf, "      \"name\": \"Type\"")?,
            Token::Enum(_) => writeln!(&mut buf, "      \"name\": \"Enum\"")?,
            Token::Return(_, _) => writeln!(&mut buf, "      \"name\": \"Return\"")?,
            Token::Readonly(_) => writeln!(&mut buf, "      \"name\": \"Readonly\"")?,
            Token::Import(_) => writeln!(&mut buf, "      \"name\": \"Import\"")?,
            Token::Export(_) => writeln!(&mut buf, "      \"name\": \"Export\"")?,
            Token::From(_) => writeln!(&mut buf, "      \"name\": \"From\"")?,
            Token::As(_) => writeln!(&mut buf, "      \"name\": \"As\"")?,
            Token::Try(_) => writeln!(&mut buf, "      \"name\": \"Try\"")?,
            Token::Ident(_, name) => {
                writeln!(&mut buf, "      \"name\": \"Ident\",")?;
                writeln!(&mut buf, "      \"value\": \"{}\"", name)?;
            }
            Token::Self_(_) => writeln!(&mut buf, "      \"name\": \"Self\"")?,
            Token::None(_) => writeln!(&mut buf, "      \"name\": \"None\"")?,
            Token::Assign(_) => writeln!(&mut buf, "      \"name\": \"Assign\"")?,
            Token::Plus(_) => writeln!(&mut buf, "      \"name\": \"Plus\"")?,
            Token::PlusEq(_) => writeln!(&mut buf, "      \"name\": \"PlusEq\"")?,
            Token::Minus(_) => writeln!(&mut buf, "      \"name\": \"Minus\"")?,
            Token::MinusEq(_) => writeln!(&mut buf, "      \"name\": \"MinusEq\"")?,
            Token::Star(_) => writeln!(&mut buf, "      \"name\": \"Star\"")?,
            Token::StarEq(_) => writeln!(&mut buf, "      \"name\": \"StarEq\"")?,
            Token::Slash(_) => writeln!(&mut buf, "      \"name\": \"Slash\"")?,
            Token::SlashEq(_) => writeln!(&mut buf, "      \"name\": \"SlashEq\"")?,
            Token::Percent(_) => writeln!(&mut buf, "      \"name\": \"Percent\"")?,
            Token::PercentEq(_) => writeln!(&mut buf, "      \"name\": \"PercentEq\"")?,
            Token::And(_) => writeln!(&mut buf, "      \"name\": \"And\"")?,
            Token::AndEq(_) => writeln!(&mut buf, "      \"name\": \"AndEq\"")?,
            Token::Or(_) => writeln!(&mut buf, "      \"name\": \"Or\"")?,
            Token::OrEq(_) => writeln!(&mut buf, "      \"name\": \"OrEq\"")?,
            Token::Caret(_) => writeln!(&mut buf, "      \"name\": \"Caret\"")?,
            Token::Elvis(_) => writeln!(&mut buf, "      \"name\": \"Elvis\"")?,
            Token::ElvisEq(_) => writeln!(&mut buf, "      \"name\": \"ElvisEq\"")?,
            Token::GT(_) => writeln!(&mut buf, "      \"name\": \"GT\"")?,
            Token::GTE(_) => writeln!(&mut buf, "      \"name\": \"GTE\"")?,
            Token::LT(_) => writeln!(&mut buf, "      \"name\": \"LT\"")?,
            Token::LTE(_) => writeln!(&mut buf, "      \"name\": \"LTE\"")?,
            Token::Eq(_) => writeln!(&mut buf, "      \"name\": \"Eq\"")?,
            Token::Neq(_) => writeln!(&mut buf, "      \"name\": \"Neq\"")?,
            Token::Bang(_) => writeln!(&mut buf, "      \"name\": \"Bang\"")?,
            Token::StarStar(_) =>  writeln!(&mut buf, "      \"name\": \"StarStar\"")?,
            Token::LParen(_, _) => writeln!(&mut buf, "      \"name\": \"LParen\"")?,
            Token::RParen(_) => writeln!(&mut buf, "      \"name\": \"RParen\"")?,
            Token::LBrack(_, _) => writeln!(&mut buf, "      \"name\": \"LBrack\"")?,
            Token::RBrack(_) => writeln!(&mut buf, "      \"name\": \"RBrack\"")?,
            Token::LBrace(_) => writeln!(&mut buf, "      \"name\": \"LBrace\"")?,
            Token::RBrace(_) => writeln!(&mut buf, "      \"name\": \"RBrace\"")?,
            Token::LBraceHash(_) => writeln!(&mut buf, "      \"name\": \"LBraceHash\"")?,
            Token::Pipe(_) => writeln!(&mut buf, "      \"name\": \"Pipe\"")?,
            Token::Colon(_) => writeln!(&mut buf, "      \"name\": \"Colon\"")?,
            Token::Comma(_) => writeln!(&mut buf, "      \"name\": \"Comma\"")?,
            Token::Question(_) => writeln!(&mut buf, "      \"name\": \"Question\"")?,
            Token::Dot(_) => writeln!(&mut buf, "      \"name\": \"Dot\"")?,
            Token::QuestionDot(_) => writeln!(&mut buf, "      \"name\": \"QuestionDot\"")?,
            Token::Arrow(_) => writeln!(&mut buf, "      \"name\": \"Arrow\"")?,
            Token::At(_) => writeln!(&mut buf, "      \"name\": \"At\"")?,
        }
        writeln!(&mut buf, "    }}")?;
        write!(&mut buf, "  }}")?;
        if idx != len - 1 {
            writeln!(&mut buf, ",")?;
        } else {
            writeln!(&mut buf, "")?;
        }
    }

    writeln!(&mut buf, "]")?;

    let bytes = buf.into_inner()?;
    Ok(String::from_utf8(bytes).unwrap())
}
