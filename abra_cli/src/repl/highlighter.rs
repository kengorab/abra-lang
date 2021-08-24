use std::collections::HashSet;
use rustyline_derive::{Completer, Helper, Hinter, Validator};
use rustyline::highlight::Highlighter;
use std::borrow::Cow;
use ansi_term::{Color, Style};
use itertools::Itertools;

#[derive(Helper, Completer, Hinter, Validator)]
pub struct AbraHighlighter<'a> {
    pub keywords: HashSet<&'a str>,
    pub builtins: HashSet<&'a str>,
    pub commands: HashSet<&'a str>,
}

impl<'a> Highlighter for AbraHighlighter<'a> {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let highlighted = tokenize(line).into_iter()
            .map(|(token_type, word)| {
                let color = match token_type {
                    TokenizeState::Ident if self.keywords.contains(&word.as_str()) => Color::Yellow.bold(),
                    TokenizeState::Ident if self.builtins.contains(&word.as_str()) => Color::Yellow.italic(),
                    TokenizeState::Ident if word.starts_with(|ch: char| ch.is_uppercase()) => Color::Purple.normal(),
                    TokenizeState::ReplCmd if self.commands.contains(&word.as_str()) => Color::Green.italic(),
                    TokenizeState::ReplCmd if !self.commands.contains(&word.as_str()) => Color::Red.italic(),
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
    ReplCmd,
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
            TokenizeState::Ident | TokenizeState::ReplCmd => !ch.is_alphanumeric(),
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
            } else if ch == '.' && result.is_empty() {
                // Only consider it a repl cmd if it's the first thing typed
                TokenizeState::ReplCmd
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

#[cfg(test)]
mod test {
    use crate::repl::highlighter::{tokenize, TokenizeState, AbraHighlighter};
    use std::collections::HashSet;
    use rustyline::highlight::Highlighter;
    use ansi_term::{Color, Style};

    #[test]
    fn tokenize_repl_commands() {
        let res = tokenize(".history show");
        let expected = vec![
            (TokenizeState::ReplCmd, ".history".to_string()),
            (TokenizeState::Whitespace, " ".to_string()),
            (TokenizeState::Ident, "show".to_string()),
        ];
        assert_eq!(expected, res);

        let res = tokenize(".foo bar   123");
        let expected = vec![
            (TokenizeState::ReplCmd, ".foo".to_string()),
            (TokenizeState::Whitespace, " ".to_string()),
            (TokenizeState::Ident, "bar".to_string()),
            (TokenizeState::Whitespace, "   ".to_string()),
            (TokenizeState::Number, "123".to_string()),
        ];
        assert_eq!(expected, res);
    }

    #[test]
    fn tokenize_strings() {
        let res = tokenize("\"asdf\"  \"zxv\"");
        let expected = vec![
            (TokenizeState::String, "\"asdf\"".to_string()),
            (TokenizeState::Whitespace, "  ".to_string()),
            (TokenizeState::String, "\"zxv\"".to_string()),
        ];
        assert_eq!(expected, res);

    }

    #[test]
    fn tokenize_numbers() {
        let res = tokenize("123.456 789.1011");
        let expected = vec![
            (TokenizeState::Number, "123".to_string()),
            (TokenizeState::Symbol, ".".to_string()),
            (TokenizeState::Number, "456".to_string()),
            (TokenizeState::Whitespace, " ".to_string()),
            (TokenizeState::Number, "789".to_string()),
            (TokenizeState::Symbol, ".".to_string()),
            (TokenizeState::Number, "1011".to_string()),
        ];
        assert_eq!(expected, res);
    }

    #[test]
    fn highlight_keywords() {
        let highlighter = AbraHighlighter {
            keywords: vec!["func", "type", "foo"].into_iter().collect(),
            builtins: HashSet::new(),
            commands: HashSet::new(),
        };
        let text = highlighter.highlight("func type  foo bar", 0);
        let expected = format!(
            "{} {}  {} {}",
            Color::Yellow.bold().paint("func"),
            Color::Yellow.bold().paint("type"),
            Color::Yellow.bold().paint("foo"),
            Style::new().paint("bar"),
        );
        assert_eq!(expected, text);
    }

    #[test]
    fn highlight_builtins() {
        let highlighter = AbraHighlighter {
            keywords: HashSet::new(),
            builtins: vec!["println", "range", "foo"].into_iter().collect(),
            commands: HashSet::new(),
        };
        let text = highlighter.highlight("println range  foo bar", 0);
        let expected = format!(
            "{} {}  {} {}",
            Color::Yellow.italic().paint("println"),
            Color::Yellow.italic().paint("range"),
            Color::Yellow.italic().paint("foo"),
            Style::new().paint("bar"),
        );
        assert_eq!(expected, text);
    }

    #[test]
    fn highlight_commands() {
        // Test valid command
        let highlighter = AbraHighlighter {
            keywords: HashSet::new(),
            builtins: HashSet::new(),
            commands: vec![".exit", ".history"].into_iter().collect(),
        };
        let text = highlighter.highlight(".history  show", 0);
        let expected = format!(
            "{}  {}",
            Color::Green.italic().paint(".history"),
            Style::new().paint("show"),
        );
        assert_eq!(expected, text);

        // Test something that looks like a command, but isn't
        let highlighter = AbraHighlighter {
            keywords: HashSet::new(),
            builtins: HashSet::new(),
            commands: vec![".exit"].into_iter().collect(),
        };
        let text = highlighter.highlight(".history  clear", 0);
        let expected = format!(
            "{}  {}",
            Color::Red.italic().paint(".history"),
            Style::new().paint("clear"),
        );
        assert_eq!(expected, text);
    }

    #[test]
    fn highlight_type_idents() {
        let highlighter = AbraHighlighter {
            keywords: HashSet::new(),
            builtins: HashSet::new(),
            commands: HashSet::new(),
        };
        let text = highlighter.highlight("String Int Bool Foo<T>", 0);
        let expected = format!(
            "{} {} {} {}<{}>",
            Color::Purple.normal().paint("String"),
            Color::Purple.normal().paint("Int"),
            Color::Purple.normal().paint("Bool"),
            Color::Purple.normal().paint("Foo"),
            Color::Purple.normal().paint("T"),
        );
        assert_eq!(expected, text);
    }

    #[test]
    fn highlight_numbers() {
        let highlighter = AbraHighlighter {
            keywords: HashSet::new(),
            builtins: HashSet::new(),
            commands: HashSet::new(),
        };
        let text = highlighter.highlight("12.34  56.78  910", 0);
        let expected = format!(
            "{}.{}  {}.{}  {}",
            Color::Blue.normal().paint("12"),
            Color::Blue.normal().paint("34"),
            Color::Blue.normal().paint("56"),
            Color::Blue.normal().paint("78"),
            Color::Blue.normal().paint("910"),
        );
        assert_eq!(expected, text);
    }

    #[test]
    fn highlight_strings() {
        let highlighter = AbraHighlighter {
            keywords: HashSet::new(),
            builtins: HashSet::new(),
            commands: HashSet::new(),
        };
        let text = highlighter.highlight("\"abc\"  \"def\"", 0);
        let expected = format!(
            "{}  {}",
            Color::Green.normal().paint("\"abc\""),
            Color::Green.normal().paint("\"def\""),
        );
        assert_eq!(expected, text);
    }
}
