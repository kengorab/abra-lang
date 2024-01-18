use std::io;
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

pub fn tokens_to_json(tokens: &Vec<Token>) -> io::Result<String> {
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
            Token::Float(_, _) => todo!(),
            Token::String(_, _) => todo!(),
            Token::StringInterp(_, _) => todo!(),
            Token::Bool(_, _) => todo!(),
            Token::Func(_) => todo!(),
            Token::Val(_) => todo!(),
            Token::Var(_) => todo!(),
            Token::If(_) => todo!(),
            Token::Else(_) => todo!(),
            Token::While(_) => todo!(),
            Token::Break(_) => todo!(),
            Token::Continue(_) => todo!(),
            Token::For(_) => todo!(),
            Token::In(_) => todo!(),
            Token::Match(_) => todo!(),
            Token::Type(_) => todo!(),
            Token::Enum(_) => todo!(),
            Token::Return(_, _) => todo!(),
            Token::Readonly(_) => todo!(),
            Token::Import(_) => todo!(),
            Token::Export(_) => todo!(),
            Token::From(_) => todo!(),
            Token::As(_) => todo!(),
            Token::Try(_) => todo!(),
            Token::Ident(_, name) => {
                writeln!(&mut buf, "      \"name\": \"Ident\",")?;
                writeln!(&mut buf, "      \"value\": \"{}\"", name)?;
            }
            Token::Self_(_) => todo!(),
            Token::None(_) => todo!(),
            Token::Assign(_) => todo!(),
            Token::Plus(_) => todo!(),
            Token::PlusEq(_) => todo!(),
            Token::Minus(_) => todo!(),
            Token::MinusEq(_) => todo!(),
            Token::Star(_) => todo!(),
            Token::StarEq(_) => todo!(),
            Token::Slash(_) => todo!(),
            Token::SlashEq(_) => todo!(),
            Token::Percent(_) => todo!(),
            Token::PercentEq(_) => todo!(),
            Token::And(_) => todo!(),
            Token::AndEq(_) => todo!(),
            Token::Or(_) => todo!(),
            Token::OrEq(_) => todo!(),
            Token::Caret(_) => todo!(),
            Token::Elvis(_) => todo!(),
            Token::ElvisEq(_) => todo!(),
            Token::GT(_) => todo!(),
            Token::GTE(_) => todo!(),
            Token::LT(_) => todo!(),
            Token::LTE(_) => todo!(),
            Token::Eq(_) => todo!(),
            Token::Neq(_) => todo!(),
            Token::Bang(_) => todo!(),
            Token::StarStar(_) => todo!(),
            Token::LParen(_, _) => todo!(),
            Token::RParen(_) => todo!(),
            Token::LBrack(_, _) => todo!(),
            Token::RBrack(_) => todo!(),
            Token::LBrace(_) => todo!(),
            Token::RBrace(_) => todo!(),
            Token::LBraceHash(_) => todo!(),
            Token::Pipe(_) => todo!(),
            Token::Colon(_) => todo!(),
            Token::Comma(_) => todo!(),
            Token::Question(_) => todo!(),
            Token::Dot(_) => {
                writeln!(&mut buf, "      \"name\": \"Dot\"")?;
            }
            Token::QuestionDot(_) => todo!(),
            Token::Arrow(_) => todo!(),
            Token::At(_) => todo!(),
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

pub fn exec_test(test_file_path: &str) {
    let selfhost_dir = get_project_root().unwrap().join("selfhost");

    let test_path = selfhost_dir.join("test").join(test_file_path);

    let module_id = ModuleId::parse_module_path("./test").unwrap();
    let contents = std::fs::read_to_string(&test_path).unwrap();
    let rust_output = match lexer::lexer::tokenize(&module_id, &contents) {
        Ok(tokens) => tokens_to_json(&tokens).unwrap(),
        Err(err) => err.get_message(&test_path.to_str().unwrap().to_string(), &contents)
    };

    let build_dir = if let Some(test_temp_dir) = std::env::var("TEST_TMP_DIR").ok() {
        let dir = Path::new(&test_temp_dir).join(random_string(12));
        std::fs::create_dir(&dir).unwrap();
        dir
    } else {
        temp_dir()
    };

    let output = Command::cargo_bin("abra").unwrap()
        .arg("build")
        .arg("--run")
        .arg(&selfhost_dir.join("src/lexer.test.abra"))
        .arg("-o")
        .arg("lexer.test")
        .arg("-b")
        .arg(&build_dir)
        .arg("--")
        .arg(&test_path)
        .output()
        .unwrap();
    assert!(output.stderr.is_empty(), "Compilation error: {}", String::from_utf8(output.stderr).unwrap());
    let abra_output = String::from_utf8(output.stdout).unwrap();

    if rust_output != abra_output {
        eprintln!("Difference detected between rust implementation and abra implementation:");
        eprintln!("  (The rust output is the 'old' and abra output is the 'new')");
        let diff = TextDiff::from_lines(
            &rust_output,
            &abra_output,
        );
        for change in diff.iter_all_changes() {
            let sign = match change.tag() {
                ChangeTag::Equal => " ",
                ChangeTag::Delete => "-",
                ChangeTag::Insert => "+",
            };
            eprint!("{sign}{change}");
        }
        panic!("Test at path '{}' failed", test_path.to_str().unwrap());
    }
}
