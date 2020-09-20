use std::str::Chars;
use std::iter::Peekable;
use crate::lexer::tokens::{Token, Position};
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

    fn skip_whitespace(&mut self) -> bool {
        let mut saw_newline = false;
        loop {
            match self.peek() {
                Some(&ch) => {
                    if ch.is_whitespace() {
                        if ch == '\n' {
                            saw_newline = true;

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

        saw_newline
    }

    fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        let skipped_newline = self.skip_whitespace();

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
            return if is_float {
                let f: f64 = s.parse().expect("Parsing string of digits (with '.') into float");
                Ok(Some(Token::Float(pos, f)))
            } else {
                let i: i64 = s.parse().expect("Parsing string of digits into int");
                Ok(Some(Token::Int(pos, i)))
            };
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

            let s = chars.into_iter().collect::<String>();
            let token = match s.as_ref() {
                "true" => Token::Bool(pos, true),
                "false" => Token::Bool(pos, false),
                "if" => Token::If(pos),
                "else" => Token::Else(pos),
                "val" => Token::Val(pos),
                "var" => Token::Var(pos),
                "func" => Token::Func(pos),
                "self" => Token::Self_(pos),
                "while" => Token::While(pos),
                "break" => Token::Break(pos),
                "for" => Token::For(pos),
                "in" => Token::In(pos),
                "type" => Token::Type(pos),
                "enum" => Token::Enum(pos),
                "None" => Token::None(pos),
                s @ _ => Token::Ident(pos, s.to_string())
            };
            return Ok(Some(token));
        }

        let pos = Position::new(self.line, self.col);
        match ch {
            '+' => {
                if let Some('=') = self.peek() {
                    self.expect_next()?; // Consume '=' token
                    Ok(Some(Token::PlusEq(pos)))
                } else {
                    Ok(Some(Token::Plus(pos)))
                }
            }
            '-' => {
                if let Some('=') = self.peek() {
                    self.expect_next()?; // Consume '=' token
                    Ok(Some(Token::MinusEq(pos)))
                } else {
                    Ok(Some(Token::Minus(pos)))
                }
            }
            '*' => {
                if let Some('=') = self.peek() {
                    self.expect_next()?; // Consume '=' token
                    Ok(Some(Token::StarEq(pos)))
                } else {
                    Ok(Some(Token::Star(pos)))
                }
            }
            '/' => {
                if let Some('/') = self.peek() {
                    self.expect_next()?; // Consume '/' token
                    while let Some(ch) = self.peek() {
                        if ch == &'\n' {
                            break; // The \n will get picked up later on, to increment the line
                        }
                        self.expect_next()?; // Consume next token
                    }
                    self.next_token()
                } else if let Some('*') = self.peek() {
                    self.expect_next()?; // Consume '*' token
                    while let Some(ch) = self.peek() {
                        if ch == &'*' {
                            self.expect_next()?; // Consume '*' token
                            if let Some('/') = self.peek() {
                                self.expect_next()?; // Consume '/' token
                                break;
                            }
                        } else if ch.is_whitespace() {
                            self.skip_whitespace(); // Use skip_whitespace to ensure newlines get counted
                        } else {
                            self.expect_next()?; // Consume next token
                        }
                    }
                    self.next_token()
                } else if let Some('=') = self.peek() {
                    self.expect_next()?; // Consume '=' token
                    Ok(Some(Token::SlashEq(pos)))
                } else {
                    Ok(Some(Token::Slash(pos)))
                }
            }
            '%' => {
                if let Some('=') = self.peek() {
                    self.expect_next()?; // Consume '=' token
                    Ok(Some(Token::PercentEq(pos)))
                } else {
                    Ok(Some(Token::Percent(pos)))
                }
            }
            '&' => {
                let ch = self.expect_next()?;
                if ch != '&' {
                    let pos = Position::new(self.line, self.col);
                    Err(LexerError::UnexpectedChar(pos, ch.to_string()))
                } else {
                    if let Some('=') = self.peek() {
                        self.expect_next()?; // Consume '=' token
                        Ok(Some(Token::AndEq(pos)))
                    } else {
                        Ok(Some(Token::And(pos)))
                    }
                }
            }
            '|' => {
                if let Some('|') = self.peek() {
                    self.expect_next()?; // Consume '|' token

                    if let Some('=') = self.peek() {
                        self.expect_next()?; // Consume '=' token
                        Ok(Some(Token::OrEq(pos)))
                    } else {
                        Ok(Some(Token::Or(pos)))
                    }
                } else {
                    Ok(Some(Token::Pipe(pos)))
                }
            }
            '?' => {
                if let Some(':') = self.peek() {
                    self.expect_next()?; // Consume ':' token

                    if let Some('=') = self.peek() {
                        self.expect_next()?; // Consume '=' token
                        Ok(Some(Token::ElvisEq(pos)))
                    } else {
                        Ok(Some(Token::Elvis(pos)))
                    }
                } else if let Some('.') = self.peek() {
                    self.expect_next()?; // Consume '.' token
                    Ok(Some(Token::QuestionDot(pos)))
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
                } else if let Some('>') = self.peek() {
                    self.expect_next()?; // Consume '>' token
                    Ok(Some(Token::Arrow(pos)))
                } else {
                    Ok(Some(Token::Assign(pos)))
                }
            }
            '(' => Ok(Some(Token::LParen(pos, skipped_newline))),
            ')' => Ok(Some(Token::RParen(pos))),
            '[' => Ok(Some(Token::LBrack(pos, skipped_newline))),
            ']' => Ok(Some(Token::RBrack(pos))),
            '{' => Ok(Some(Token::LBrace(pos))),
            '}' => Ok(Some(Token::RBrace(pos))),
            ',' => Ok(Some(Token::Comma(pos))),
            ':' => Ok(Some(Token::Colon(pos))),
            '.' => Ok(Some(Token::Dot(pos))),
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
        let input = "+ - * / % < > ! = .";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::Plus(Position::new(1, 1)),
            Token::Minus(Position::new(1, 3)),
            Token::Star(Position::new(1, 5)),
            Token::Slash(Position::new(1, 7)),
            Token::Percent(Position::new(1, 9)),
            Token::LT(Position::new(1, 11)),
            Token::GT(Position::new(1, 13)),
            Token::Bang(Position::new(1, 15)),
            Token::Assign(Position::new(1, 17)),
            Token::Dot(Position::new(1, 19)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_multi_char_operators() {
        let input = "&& || <= >= != == ?: ?. =>";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::And(Position::new(1, 1)),
            Token::Or(Position::new(1, 4)),
            Token::LTE(Position::new(1, 7)),
            Token::GTE(Position::new(1, 10)),
            Token::Neq(Position::new(1, 13)),
            Token::Eq(Position::new(1, 16)),
            Token::Elvis(Position::new(1, 19)),
            Token::QuestionDot(Position::new(1, 22)),
            Token::Arrow(Position::new(1, 25)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_multi_char_eq_operators() {
        let input = "+= -= *= /= %= &&= ||= ?:=";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::PlusEq(Position::new(1, 1)),
            Token::MinusEq(Position::new(1, 4)),
            Token::StarEq(Position::new(1, 7)),
            Token::SlashEq(Position::new(1, 10)),
            Token::PercentEq(Position::new(1, 13)),
            Token::AndEq(Position::new(1, 16)),
            Token::OrEq(Position::new(1, 20)),
            Token::ElvisEq(Position::new(1, 24)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_single_char_separators() {
        let input = "( ) [ ] { } | , : ?";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::LParen(Position::new(1, 1), false),
            Token::RParen(Position::new(1, 3)),
            Token::LBrack(Position::new(1, 5), false),
            Token::RBrack(Position::new(1, 7)),
            Token::LBrace(Position::new(1, 9)),
            Token::RBrace(Position::new(1, 11)),
            Token::Pipe(Position::new(1, 13)),
            Token::Comma(Position::new(1, 15)),
            Token::Colon(Position::new(1, 17)),
            Token::Question(Position::new(1, 19)),
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
        let input = "true false val var if else func while break for in type enum self";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::Bool(Position::new(1, 1), true),
            Token::Bool(Position::new(1, 6), false),
            Token::Val(Position::new(1, 12)),
            Token::Var(Position::new(1, 16)),
            Token::If(Position::new(1, 20)),
            Token::Else(Position::new(1, 23)),
            Token::Func(Position::new(1, 28)),
            Token::While(Position::new(1, 33)),
            Token::Break(Position::new(1, 39)),
            Token::For(Position::new(1, 45)),
            Token::In(Position::new(1, 49)),
            Token::Type(Position::new(1, 52)),
            Token::Enum(Position::new(1, 57)),
            Token::Self_(Position::new(1, 62)),
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

    #[test]
    fn test_tokenize_single_line_comment() {
        let input = "123\n// this should disappear\n456";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::Int(Position::new(1, 1), 123),
            Token::Int(Position::new(3, 1), 456),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_multi_line_comment() {
        let input = "123\n/* this\nshould\n* disappear\n*/ 456";
        let tokens = tokenize(&input.to_string()).unwrap();
        let expected = vec![
            Token::Int(Position::new(1, 1), 123),
            Token::Int(Position::new(5, 4), 456),
        ];
        assert_eq!(expected, tokens);
    }
}
