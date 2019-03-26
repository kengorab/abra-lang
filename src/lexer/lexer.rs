use crate::lexer::tokens::{Token, Position};
use std::str::Chars;
use std::iter::Peekable;
use crate::lexer::lexer_error::LexerError;

pub fn tokenize(input: &String) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(input);

    let mut tokens: Vec<Token> = vec![];

    loop {
        match lexer.next_token()? {
            Some(tok) => tokens.push(tok),
            None => break
        }
    };

    Ok(tokens)
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

    fn expect_next(&mut self) -> Result<char, LexerError> {
        self.col += 1;
        self.input.next().ok_or(LexerError::UnexpectedEof(Position::new(self.line, self.col)))
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

    fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        self.skip_whitespace();

        let ch = match self.advance() {
            None => return Ok(None),
            Some(ch) => ch
        };

        if ch.is_digit(10) {
            let pos = Position::new(self.line, self.col);

            let mut chars = vec![ch];
            let mut is_float = false;
            loop {
                if let Some(&ch) = self.peek() {
                    if ch.is_digit(10) {
                        chars.push(self.expect_next()?);
                        continue;
                    } else if ch == '.' {
                        if is_float {
                            let pos = Position::new(self.line, self.col + 1);
                            return Err(LexerError::UnexpectedChar(pos, ch.to_string()));
                        }
                        is_float = true;
                        chars.push(self.expect_next()?);
                        continue;
                    }
                }
                break;
            };

            let s: String = chars.into_iter().collect();
            if is_float {
                let f: f64 = s.parse().expect("Parsing string of digits (with '.') into float");
                return Ok(Some(Token::Float(pos, f)));
            } else {
                let i: i64 = s.parse().expect("Parsing string of digits into int");
                return Ok(Some(Token::Int(pos, i)));
            }
        }

        if ch == '"' {
            let pos = Position::new(self.line, self.col);
            let mut chars: Vec<char> = vec![];

            loop {
                if let Some(&ch) = self.peek() {
                    if ch == '\n' {
                        return Err(LexerError::UnterminatedString(Position::new(self.line, self.col + 1)));
                    } else if ch == '"' {
                        // Consume closing quote
                        self.expect_next()?;
                        break;
                    } else if ch == '\\' {
                        // TODO: Address escaped characters
                    }

                    chars.push(self.expect_next()?);
                } else {
                    return Err(LexerError::UnterminatedString(Position::new(self.line, self.col + 1)));
                }
            }

            let s: String = chars.into_iter().collect();
            return Ok(Some(Token::String(pos, s)));
        }

        let pos = Position::new(self.line, self.col);
        Ok(match ch {
            '+' => Some(Token::Plus(pos)),
            '-' => Some(Token::Minus(pos)),
            '*' => Some(Token::Star(pos)),
            '/' => Some(Token::Slash(pos)),
            _ => None
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_ints() {
        let input = "123\n 456";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::Int(Position::new(1, 1), 123),
            Token::Int(Position::new(2, 2), 456),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_floats() {
        let input = "1.23\n 0.456";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::Float(Position::new(1, 1), 1.23),
            Token::Float(Position::new(2, 2), 0.456),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_floats_error() {
        let input = "1..23";
        let e = tokenize(&input.to_string()).unwrap_err();
        let expected = LexerError::UnexpectedChar(Position::new(1, 3), ".".to_string());
        assert_eq!(expected, e);
    }

    #[test]
    fn test_tokenize_single_char_operators() {
        let input = "+ - * /";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::Plus(Position::new(1, 1)),
            Token::Minus(Position::new(1, 3)),
            Token::Star(Position::new(1, 5)),
            Token::Slash(Position::new(1, 7)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_strings_empty() {
        let input = "\"\"";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::String(Position::new(1, 1), "".to_string())
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_strings() {
        let input = "\"hello wörld: 1 + 2   ! 👩🏻‍⚕️\"";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::String(Position::new(1, 1), "hello wörld: 1 + 2   ! 👩🏻‍⚕️".to_string())
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_strings_error() {
        let input = "\"";
        let tokens = tokenize(&input.to_string()).unwrap_err();
        let expected = LexerError::UnterminatedString(Position::new(1, 2));
        assert_eq!(expected, tokens);

        let input = "\"\n\"";
        let tokens = tokenize(&input.to_string()).unwrap_err();
        let expected = LexerError::UnterminatedString(Position::new(1, 2));
        assert_eq!(expected, tokens);
    }
}
