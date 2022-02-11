use std::collections::HashMap;
use ansi_term::Color;
use rustyline::Editor;
use std::env::current_dir;
use abra_core::{compile, typecheck};
use crate::fs_module_reader::FsModuleReader;
use abra_core::parser::ast::ModuleId;
use abra_core::common::display_error::DisplayError;
use itertools::Itertools;
use abra_core::vm::vm::{VM, VMContext};
use abra_core::vm::value::Value;
use abra_core::builtins::common::to_string;
use abra_core::module_loader::ModuleLoader;
use abra_core::typechecker::typechecker::ScopeBinding;
use crate::repl::highlighter::AbraHighlighter;
use std::path::PathBuf;
use rustyline::error::ReadlineError;

pub struct Repl<'a> {
    running: bool,
    rl: Editor<AbraHighlighter<'a>>,
    code: Vec<String>,
    continuation_buf: String,
    indentations: Vec<char>,
    module_reader: FsModuleReader,
}

impl<'a> Repl<'a> {
    pub fn run() {
        let mut repl = Repl::new().init();
        repl.start();
    }

    fn abra_dir() -> Option<PathBuf> {
        dirs::home_dir().map(|dir| dir.join(".abra"))
    }

    fn repl_history_file(abra_dir: &PathBuf) -> PathBuf {
        abra_dir.join("repl-history.txt")
    }

    fn module_id() -> ModuleId {
        ModuleId::from_name(".repl")
    }

    fn new() -> Self {
        let rl = Editor::<AbraHighlighter>::new();
        let module_reader = FsModuleReader { project_root: current_dir().unwrap(), module_id_paths: HashMap::new() };

        Self { running: false, rl, code: vec![], continuation_buf: "".to_string(), indentations: vec![], module_reader }
    }

    fn init(mut self) -> Self {
        let keywords = vec![
            "func", "val", "var", "if", "else", "while", "break", "continue", "for", "in", "match",
            "type", "enum", "return", "readonly", "import", "export", "from", "as",
        ];
        let builtins = vec!["println", "print", "None", "self", "range"];
        let commands = vec![".exit", ".save", ".help", ".type", ".history"];
        let highlighter = AbraHighlighter {
            keywords: keywords.into_iter().collect(),
            builtins: builtins.into_iter().collect(),
            commands: commands.into_iter().collect(),
        };
        self.rl.set_helper(Some(highlighter));

        if let Some(abra_dir) = Self::abra_dir() {
            if !abra_dir.exists() {
                let _discarded = std::fs::create_dir(&abra_dir);
            }
            if self.rl.load_history(&Self::repl_history_file(&abra_dir)).is_err() {
                println!("No previous history loaded")
            }
        }

        self
    }

    fn start(&mut self) {
        self.running = true;
        while self.running {
            let prompt = self.prompt();
            match self.rl.readline(&prompt) {
                Ok(input) if input.starts_with('.') => self.handle_repl_command(&input),
                Ok(input) => self.handle_input(&input),
                Err(ReadlineError::Interrupted) if self.indent_level() > 0 => {
                    println!("Current continuation discarded. (To exit, press ^C again)");
                    self.continuation_buf.clear();
                    self.indentations.clear();
                }
                _ => self.exit(),
            }
        }

        if let Some(history_file) = Self::abra_dir().as_ref().map(Self::repl_history_file) {
            let _discarded = self.rl.append_history(&history_file);
        }
    }

    fn handle_repl_command(&mut self, input: &str) {
        let mut input = input.split_ascii_whitespace();
        let cmd = input.next().expect("There is at least a '.'");

        match cmd {
            ".exit" => self.exit(),
            ".save" => self.handle_save_command(input),
            ".type" => self.handle_type_command(input),
            ".history" => self.handle_history_command(input),
            ".help" => {
                println!("Available special commands:");
                println!("  .help                     Prints this help message");
                println!("  .exit                     Exits the repl");
                println!("  .save [filename]          Save repl contents to a file");
                println!("  .type [var_name]          Display the type information for a given name");
                println!("  .history [path|clear]     Display the history path / clear history");
            }
            s => println!("Unrecognized special command {} (try .help to see a list of special commands)", s)
        }
    }

    fn handle_save_command<I>(&self, mut input: I)
        where I: Iterator<Item=&'a str>
    {
        if let Some(filename) = input.next() {
            let current_dir = current_dir().unwrap();
            let file_path = current_dir.join(filename);

            let code = self.code.iter().join("\n");
            match std::fs::write(file_path, code) {
                Ok(_) => println!("Repl buffer saved to {}", filename),
                Err(_) => println!("Could not save buffer to file {}", filename),
            }
        } else {
            println!(".save requires a filename argument (try .help for more details)");
        }
    }

    fn handle_type_command<I>(&self, mut input: I)
        where I: Iterator<Item=&'a str>
    {
        if let Some(binding_name) = input.next() {
            let file = self.code.iter().join("\n");
            let mut module_reader = self.module_reader.clone();
            let mut loader = ModuleLoader::new(&mut module_reader);
            match typecheck(Self::module_id(), &file, &mut loader) {
                Err(_) => println!("Could not determine type for name '{}'", binding_name),
                Ok(m) => {
                    let binding_name = binding_name.to_string();
                    let typ = match m.global_bindings.get(&binding_name) {
                        None => loader.resolve_binding_type(&binding_name),
                        Some(ScopeBinding(_, typ, _)) => Some(typ),
                    };

                    if let Some(typ) = typ {
                        println!("{}: {}", binding_name, typ.repr())
                    } else {
                        println!("No such name '{}'", binding_name)
                    }
                }
            }
        } else {
            println!(".type requires a variable name (try .help for more details)");
        }
    }

    fn handle_history_command<I>(&self, mut input: I)
        where I: Iterator<Item=&'a str>
    {
        match input.next() {
            None => println!(".history requires a subcommand (path/clear) (try .help for more details)"),
            Some("path") => {
                let history_path = Self::abra_dir().as_ref()
                    .map(Self::repl_history_file)
                    .and_then(|f| if f.exists() { Some(f) } else { None });
                if let Some(history_path) = history_path {
                    println!("{}", history_path.as_path().to_str().unwrap());
                } else {
                    println!("There currently is no history file");
                }
            }
            Some("clear") => {
                let history_path = Self::abra_dir().as_ref()
                    .map(Self::repl_history_file)
                    .and_then(|f| if f.exists() { Some(f) } else { None });
                if let Some(history_path) = history_path {
                    if std::fs::remove_file(history_path).is_ok() {
                        println!("History cleared");
                    } else {
                        println!("Error clearing history");
                    }
                } else {
                    println!("There currently is no history file");
                }
            }
            Some(cmd) => println!("Unrecognized subcommand {}", cmd),
        }
    }

    fn handle_input(&mut self, input: &str) {
        let mut input = format!("{}{}", "  ".repeat(self.indent_level()), input.trim_end());
        if input.ends_with(|ch| ch == '{' || ch == '(' || ch == '[') {
            let last = input.chars().last().expect("We just asserted above that it was an opener");
            self.indentations.push(last);
        } else if input.ends_with(|ch| ch == '}' || ch == ')' || ch == ']') {
            if let Some(ch) = self.indentations.last() {
                let is_balanced = match input.chars().last() {
                    Some(')') => ch == &'(',
                    Some(']') => ch == &'[',
                    Some('}') => ch == &'{',
                    _ => false
                };

                if is_balanced {
                    self.indentations.pop();

                    if self.indentations.is_empty() {
                        self.continuation_buf.push_str(&input);
                        input = self.continuation_buf.drain(..).collect();
                    }
                }
            }
        }
        let input = input; // Remove mutability

        if !self.indentations.is_empty() {
            self.continuation_buf.push_str(&input);
            self.continuation_buf.push('\n');
            return;
        }

        self.rl.add_history_entry(input.as_str());
        self.code.push(input);

        let input = self.code.iter().join("\n");
        self.evaluate(input);
    }

    fn evaluate(&mut self, input: String) {
        match compile(Self::module_id(), &input, &mut self.module_reader) {
            Err(e) => {
                self.code.pop();

                let msg = e.get_message(&"(repl)".to_string(), &input);
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

    fn exit(&mut self) {
        self.running = false;
    }

    fn prompt(&self) -> String {
        if self.indent_level() == 0 {
            "> ".to_string()
        } else {
            format!(">>{}", "  ".repeat(self.indent_level()))
        }
    }

    fn indent_level(&self) -> usize {
        self.indentations.len()
    }
}
