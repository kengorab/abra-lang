use std::str::Chars;
use std::iter::Peekable;
use std::collections::HashMap;
use crate::lexer::tokens::{Token, Position, Keyword};
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

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Keyword> = {
        let mut keywords = HashMap::new();
        keywords.insert("true", Keyword::True);
        keywords.insert("false", Keyword::False);
        keywords.insert("val", Keyword::Val);
        keywords.insert("var", Keyword::Var);
        keywords
    };
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
                        return Err(LexerError::UnterminatedString(pos, Position::new(self.line, self.col + 1)));
                    } else if ch == '"' {
                        // Consume closing quote
                        self.expect_next()?;
                        break;
                    } else if ch == '\\' {
                        // TODO: Address escaped characters
                    }

                    chars.push(self.expect_next()?);
                } else {
                    return Err(LexerError::UnterminatedString(pos, Position::new(self.line, self.col + 1)));
                }
            }

            let s: String = chars.into_iter().collect();
            return Ok(Some(Token::String(pos, s)));
        }

        if ch.is_alphabetic() || ch == '_' {
            let pos = Position::new(self.line, self.col);
            let mut chars = vec![ch];

            while let Some(ch) = self.peek() {
                if ch.is_alphanumeric() || ch == &'_' {
                    chars.push(self.expect_next()?);
                } else {
                    break;
                }
            }

            let s: String = chars.into_iter().collect();
            return match KEYWORDS.get(&*s) {
                Some(keyword) => match keyword {
                    Keyword::True => Ok(Some(Token::Bool(pos, true))),
                    Keyword::False => Ok(Some(Token::Bool(pos, false))),
                    Keyword::Val => Ok(Some(Token::Val(pos))),
                    Keyword::Var => Ok(Some(Token::Var(pos))),
                }
                None => Ok(Some(Token::Ident(pos, s)))
            };
        }

        let pos = Position::new(self.line, self.col);
        match ch {
            '+' => Ok(Some(Token::Plus(pos))),
            '-' => Ok(Some(Token::Minus(pos))),
            '*' => Ok(Some(Token::Star(pos))),
            '/' => Ok(Some(Token::Slash(pos))),
            '&' => {
                let ch = self.expect_next()?;
                if ch != '&' {
                    let pos = Position::new(self.line, self.col);
                    Err(LexerError::UnexpectedChar(pos, ch.to_string()))
                } else {
                    Ok(Some(Token::And(pos)))
                }
            }
            '|' => {
                let ch = self.expect_next()?;
                if ch != '|' {
                    let pos = Position::new(self.line, self.col);
                    Err(LexerError::UnexpectedChar(pos, ch.to_string()))
                } else {
                    Ok(Some(Token::Or(pos)))
                }
            }
            '?' => {
                if let Some(':') = self.peek() {
                    self.expect_next()?; // Consume ':' token
                    Ok(Some(Token::Elvis(pos)))
                } else {
                    Ok(Some(Token::Question(pos)))
                }
            }
            '>' => {
                if let Some('=') = self.peek() {
                    self.expect_next()?; // Consume '=' token
                    Ok(Some(Token::GTE(pos)))
                } else {
                    Ok(Some(Token::GT(pos)))
                }
            }
            '<' => {
                if let Some('=') = self.peek() {
                    self.expect_next()?; // Consume '=' token
                    Ok(Some(Token::LTE(pos)))
                } else {
                    Ok(Some(Token::LT(pos)))
                }
            }
            '!' => {
                if let Some('=') = self.peek() {
                    self.expect_next()?; // Consume '=' token
                    Ok(Some(Token::Neq(pos)))
                } else {
                    Ok(Some(Token::Bang(pos)))
                }
            }
            '=' => {
                if let Some('=') = self.peek() {
                    self.expect_next()?; // Consume '=' token
                    Ok(Some(Token::Eq(pos)))
                } else {
                    Ok(Some(Token::Assign(pos)))
                }
            }
            '(' => Ok(Some(Token::LParen(pos))),
            ')' => Ok(Some(Token::RParen(pos))),
            '[' => Ok(Some(Token::LBrack(pos))),
            ']' => Ok(Some(Token::RBrack(pos))),
            ',' => Ok(Some(Token::Comma(pos))),
            ':' => Ok(Some(Token::Colon(pos))),
            _ => Ok(None)
        }
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
        let input = "+ - * / < > ! =";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::Plus(Position::new(1, 1)),
            Token::Minus(Position::new(1, 3)),
            Token::Star(Position::new(1, 5)),
            Token::Slash(Position::new(1, 7)),
            Token::LT(Position::new(1, 9)),
            Token::GT(Position::new(1, 11)),
            Token::Bang(Position::new(1, 13)),
            Token::Assign(Position::new(1, 15)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_multi_char_operators() {
        let input = "&& || <= >= != == ?:";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::And(Position::new(1, 1)),
            Token::Or(Position::new(1, 4)),
            Token::LTE(Position::new(1, 7)),
            Token::GTE(Position::new(1, 10)),
            Token::Neq(Position::new(1, 13)),
            Token::Eq(Position::new(1, 16)),
            Token::Elvis(Position::new(1, 19)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_single_char_separators() {
        let input = "( ) [ ] , : ?";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::LParen(Position::new(1, 1)),
            Token::RParen(Position::new(1, 3)),
            Token::LBrack(Position::new(1, 5)),
            Token::RBrack(Position::new(1, 7)),
            Token::Comma(Position::new(1, 9)),
            Token::Colon(Position::new(1, 11)),
            Token::Question(Position::new(1, 13)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_multi_char_operators_error() {
        let input = "&";
        let error = tokenize(&input.to_string()).unwrap_err();
        let expected = LexerError::UnexpectedEof(Position::new(1, 2));
        assert_eq!(expected, error);

        let input = "&+";
        let error = tokenize(&input.to_string()).unwrap_err();
        let expected = LexerError::UnexpectedChar(Position::new(1, 2), "+".to_string());
        assert_eq!(expected, error);

        let input = "& &";
        let error = tokenize(&input.to_string()).unwrap_err();
        let expected = LexerError::UnexpectedChar(Position::new(1, 2), " ".to_string());
        assert_eq!(expected, error);

        let input = "|";
        let error = tokenize(&input.to_string()).unwrap_err();
        let expected = LexerError::UnexpectedEof(Position::new(1, 2));
        assert_eq!(expected, error);

        let input = "|-";
        let error = tokenize(&input.to_string()).unwrap_err();
        let expected = LexerError::UnexpectedChar(Position::new(1, 2), "-".to_string());
        assert_eq!(expected, error);

        let input = "| |";
        let error = tokenize(&input.to_string()).unwrap_err();
        let expected = LexerError::UnexpectedChar(Position::new(1, 2), " ".to_string());
        assert_eq!(expected, error);
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
        let expected = LexerError::UnterminatedString(Position::new(1, 1), Position::new(1, 2));
        assert_eq!(expected, tokens);

        let input = "\"\n\"";
        let tokens = tokenize(&input.to_string()).unwrap_err();
        let expected = LexerError::UnterminatedString(Position::new(1, 1), Position::new(1, 2));
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_keywords() {
        let input = "true false val var";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::Bool(Position::new(1, 1), true),
            Token::Bool(Position::new(1, 6), false),
            Token::Val(Position::new(1, 12)),
            Token::Var(Position::new(1, 16)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_identifier() {
        let input = "abc abc1 abC_2";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::Ident(Position::new(1, 1), "abc".to_string()),
            Token::Ident(Position::new(1, 5), "abc1".to_string()),
            Token::Ident(Position::new(1, 10), "abC_2".to_string()),
        ];
        assert_eq!(expected, tokens);
    }
}