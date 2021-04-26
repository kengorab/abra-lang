use crate::lexer::tokens::{Token, Range, Position};

pub const IND_AMT: usize = 2;

pub trait DisplayError {
    fn get_underline(left_padding: usize, length: usize) -> String {
        format!("{}{}", " ".repeat(left_padding), "^".repeat(length))
    }

    fn indent() -> String {
        " ".repeat(IND_AMT)
    }

    fn get_underlined_line(lines: &Vec<&str>, token: &Token) -> String {
        let pos = token.get_position();
        let range = token.get_range();
        Self::get_underlined_line_no_token(lines, &pos, &range)
    }

    fn get_underlined_line_no_token(lines: &Vec<&str>, pos: &Position, range: &Range) -> String {
        let line = lines.get(pos.line - 1).expect("There should be a line");
        let length = range.end.col - range.start.col + 1;
        let underline = Self::get_underline(2 * IND_AMT + pos.col, length);
        let indent = Self::indent();
        format!("{}|{}{}\n{}", indent, indent, line, underline)
    }

    fn get_message(&self, file_name: &String, source: &String) -> String {
        let lines: Vec<&str> = source.split("\n").collect();
        self.message_for_error(file_name, &lines)
    }

    fn message_for_error(&self, file_name: &String, lines: &Vec<&str>) -> String;
}