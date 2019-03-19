use crate::lexer::tokens::{Token, Position};
use std::str::Chars;
use std::iter::Peekable;

pub fn tokenize(input: &String) -> Vec<Token> {
    let mut lexer = Lexer::new(input);

    let mut tokens: Vec<Token> = vec![];

    loop {
        match lexer.next_token() {
            Some(tok) => tokens.push(tok),
            None => break tokens
        }
    }
}

struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a String) -> Self {
        let input = input.chars().peekable();

        Lexer {
            input,
            line: 1,
            col: 0,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.col += 1;
        self.input.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(&ch) => {
                    if ch.is_whitespace() {
                        if ch == '\n' {
                            self.line += 1;
                            self.col = 0;
                            self.advance();

                            // Subtract 1 to account for consuming whitespace; alternatively,
                            // just call self.input.next() here
                            self.col -= 1;
                        } else {
                            self.advance();
                        }
                    } else {
                        break;
                    }
                }
                _ => break
            }
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let ch = self.advance()?;

        if ch.is_digit(10) {
            let pos = Position::new(self.line, self.col);

            let mut chars = vec![ch];
            let mut is_float = false;
            loop {
                if let Some(&ch) = self.peek() {
                    if ch.is_digit(10) {
                        chars.push(self.advance()?);
                        continue;
                    } else if ch == '.' {
                        if is_float {
                            panic!("Unexpected '.'");
                        }
                        is_float = true;
                        chars.push(self.advance()?);
                        continue;
                    }
                }
                break;
            };

            let s: String = chars.into_iter().collect();
            if is_float {
                let f: f64 = s.parse().expect("Parsing string of digits (with '.') into float");
                return Some(Token::Float(pos, f));
            } else {
                let i: i64 = s.parse().expect("Parsing string of digits into int");
                return Some(Token::Int(pos, i));
            }
        }

        let pos = Position::new(self.line, self.col);
        match ch {
            '+' => Some(Token::Plus(pos)),
            '-' => Some(Token::Minus(pos)),
            '*' => Some(Token::Star(pos)),
            '/' => Some(Token::Slash(pos)),
            _ => None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_ints() {
        let input = "123\n 456";
        let tokens = tokenize(&input.to_string());
        let expected = vec![
            Token::Int(Position::new(1, 1), 123),
            Token::Int(Position::new(2, 2), 456),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_floats() {
        let input = "1.23\n 0.456";
        let tokens = tokenize(&input.to_string());
        let expected = vec![
            Token::Float(Position::new(1, 1), 1.23),
            Token::Float(Position::new(2, 2), 0.456),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_single_char_operators() {
        let input = "+ - * /";
        let tokens = tokenize(&input.to_string());
        let expected = vec![
            Token::Plus(Position::new(1, 1)),
            Token::Minus(Position::new(1, 3)),
            Token::Star(Position::new(1, 5)),
            Token::Slash(Position::new(1, 7)),
        ];
        assert_eq!(expected, tokens);
    }
}
