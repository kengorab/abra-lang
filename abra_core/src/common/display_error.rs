pub const IND_AMT: usize = 2;

pub trait DisplayError {
    fn get_cursor(left_padding: usize) -> String {
        format!("{}^", " ".repeat(left_padding))
    }

    fn indent() -> String {
        " ".repeat(IND_AMT)
    }

    fn get_message(&self, source: &String) -> String {
        let lines: Vec<&str> = source.split("\n").collect();
        self.message_for_error(&lines)
    }

    fn message_for_error(&self, lines: &Vec<&str>) -> String;
}