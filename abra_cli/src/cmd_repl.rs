use rustyline_derive::{Completer, Helper, Hinter, Validator};
use rustyline::highlight::Highlighter;
use std::borrow::Cow;
use std::collections::HashSet;
use ansi_term::{Color, Style};
use rustyline::Editor;
use std::env::current_dir;
use abra_core::compile;
use crate::fs_module_reader::FsModuleReader;
use abra_core::parser::ast::ModuleId;
use abra_core::common::display_error::DisplayError;
use itertools::Itertools;
use abra_core::vm::vm::{VM, VMContext};
use abra_core::vm::value::Value;
use abra_core::builtins::common::to_string;

#[derive(Helper, Completer, Hinter, Validator)]
struct AbraHighlighter<'a> {
    keywords: HashSet<&'a str>,
    builtins: HashSet<&'a str>,
}

impl<'a> Highlighter for AbraHighlighter<'a> {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let highlighted = tokenize(line).into_iter()
            .map(|(token_type, word)| {
                let color = match token_type {
                    TokenizeState::Ident if self.keywords.contains(&word.as_str()) => Color::Yellow.bold(),
                    TokenizeState::Ident if self.builtins.contains(&word.as_str()) => Color::Yellow.italic(),
                    TokenizeState::Ident if word.starts_with(|ch: char| ch.is_uppercase()) => Color::Purple.normal(),
                    TokenizeState::Number => Color::Blue.normal(),
                    TokenizeState::String => Color::Green.normal(),
                    _ => Style::new()
                };

                color.paint(word)
            })
            .join("");
        Cow::Owned(highlighted)
    }

    fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
        // TODO: Improve performance here; can we avoid re-rendering the line on every keypress?
        true
    }
}

#[derive(Clone, Debug, PartialEq)]
enum TokenizeState {
    Init,
    Whitespace,
    Ident,
    Number,
    String,
    Symbol,
}

fn tokenize(input: &str) -> Vec<(TokenizeState, String)> {
    let mut chars = input.chars();
    let mut result = Vec::new();
    let mut buf = String::new();
    let mut state = TokenizeState::Init;

    while let Some(ch) = chars.next() {
        let is_boundary = match &state {
            TokenizeState::Init => true,
            TokenizeState::Whitespace => !ch.is_whitespace(),
            TokenizeState::Ident => !ch.is_alphanumeric(),
            TokenizeState::Number => !ch.is_numeric(),
            TokenizeState::String => ch == '"',
            TokenizeState::Symbol => ch.is_alphanumeric() || ch.is_whitespace() || ch == '"'
        };
        if is_boundary {
            let within_string = state == TokenizeState::String;

            if within_string {
                buf.push(ch); // Push closing quote before clearing buffer
            }

            if state != TokenizeState::Init {
                result.push((state.clone(), buf.clone()));
                buf.clear();
            }

            state = if ch.is_whitespace() {
                TokenizeState::Whitespace
            } else if ch.is_alphabetic() {
                TokenizeState::Ident
            } else if ch.is_numeric() {
                TokenizeState::Number
            } else if ch == '"' && within_string {
                TokenizeState::Init
            } else if ch == '"' && !within_string {
                TokenizeState::String
            } else {
                TokenizeState::Symbol
            };

            // Don't push closing quote char into next buffer
            if within_string { continue; }
        }

        buf.push(ch);
    }

    if !buf.is_empty() {
        result.push((state, buf));
    }

    result
}

pub fn cmd_repl() -> Result<(), ()> {
    let mut rl = Editor::new();

    let keywords = vec![
        "func", "val", "var", "if", "else", "while", "break", "continue", "for", "in", "match",
        "type", "enum", "return", "readonly", "import", "export", "from", "as",
    ];
    let builtins = vec!["println", "print", "None", "self", "range"];
    let h = AbraHighlighter {
        keywords: keywords.into_iter().collect(),
        builtins: builtins.into_iter().collect(),
    };
    rl.set_helper(Some(h));

    let mut module_reader = FsModuleReader { project_root: current_dir().unwrap() };
    let module_id = ModuleId::from_name(".repl");

    let mut code = Vec::new();
    let mut buf = String::new();
    let mut indentations = Vec::new();
    loop {
        let indent_level = indentations.len();

        let prompt = if indent_level == 0 {
            "> ".to_string()
        } else {
            format!(">>{}", "  ".repeat(indent_level))
        };

        match rl.readline(&prompt) {
            Ok(input) if input.starts_with('.') => {
                let mut input = input.split_ascii_whitespace();
                let cmd = input.next().expect("There is at least a '.'");

                match cmd {
                    ".exit" => {
                        println!("Goodbye!");
                        break;
                    }
                    ".save" => {
                        if let Some(filename) = input.next() {
                            let current_dir = current_dir().unwrap();
                            let file_path = current_dir.join(filename);

                            let code = code.iter().join("\n");
                            match std::fs::write(file_path, code) {
                                Ok(_) => println!("Repl buffer saved to {}", filename),
                                Err(_) => println!("Could not save buffer to file {}", filename),
                            }
                        } else {
                            println!(".save requires a filename argument");
                        }
                    }
                    ".help" => {
                        println!("Available special commands:");
                        println!("  .help                 Prints this help message");
                        println!("  .exit                 Exits the repl");
                        println!("  .save [filename]      Save repl contents to a file");
                    }
                    s => println!("Unrecognized special command {} (try .help to see a list of special commands)", s)
                }
            }
            Ok(input) => {
                let input = format!("{}{}", "  ".repeat(indent_level), input.trim_end());
                if input.ends_with(|ch| ch == '{' || ch == '(' || ch == '[') {
                    let last = input.chars().last().expect("We just asserted above that it was an opener");
                    indentations.push(last);
                } else if input.ends_with(|ch| ch == '}' || ch == ')' || ch == ']') {
                    if let Some(ch) = indentations.last() {
                        let is_balanced = match input.chars().last() {
                            Some(')') => ch == &'(',
                            Some(']') => ch == &'[',
                            Some('}') => ch == &'{',
                            _ => false
                        };

                        if is_balanced {
                            indentations.pop();

                            if indentations.is_empty() {
                                code.push(buf.clone());
                                buf.clear();
                            }
                        }
                    }
                }

                if !indentations.is_empty() {
                    buf.push_str(&input);
                    buf.push('\n');
                    continue;
                }

                code.push(input);

                let file = code.iter().join("\n");
                match compile(module_id.clone(), &file, &mut module_reader) {
                    Err(e) => {
                        code.pop();

                        let msg = e.get_message(&"(repl)".to_string(), &file);
                        println!("\n{}", msg);
                    }
                    Ok(modules) => {
                        let mut vm = VM::new(VMContext::default());
                        let mut result = Value::Nil;
                        for module in modules {
                            match vm.run(module) {
                                Ok(v) => result = v,
                                Err(e) => {
                                    println!("{}", Color::Red.paint(format!("{:?}", e)));
                                    println!("Cannot recover from VM error, quitting repl");
                                    break;
                                }
                            };
                        }
                        if result != Value::Nil {
                            println!("{}", to_string(&result, &mut vm));
                        }
                    }
                }
            }
            Err(rustyline::error::ReadlineError::Interrupted) if indent_level > 0 => {
                println!("Current continuation discarded. (To exit, press ^C again)");
                buf.clear();
                indentations.clear();
            }
            _ => break,
        }
    }

    Ok(())
}
