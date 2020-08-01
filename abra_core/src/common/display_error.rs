use crate::lexer::tokens::Token;

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
        let line = lines.get(pos.line - 1).expect("There should be a line");

        let range = token.get_range();
        let length = range.end.col - range.start.col + 1;
        let underline = Self::get_underline(2 * IND_AMT + pos.col, length);
        let indent = Self::indent();
        format!("{}|{}{}\n{}", indent, indent, line, underline)
    }

    fn get_message(&self, source: &String) -> String {
        let lines: Vec<&str> = source.split("\n").collect();
        self.message_for_error(&lines)
    }

    fn message_for_error(&self, lines: &Vec<&str>) -> String;
}