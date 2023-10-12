use peekmore::{PeekMore, PeekMoreIterator};
use std::str::Chars;
use crate::lexer::tokens::{Token, Position};
use crate::lexer::lexer_error::{LexerErrorKind, LexerError};
use crate::parser::ast::ModuleId;
use itertools::Itertools;

pub fn tokenize(module_id: &ModuleId, input: &String) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(input);

    let mut tokens: Vec<Token> = vec![];

    loop {
        match lexer.next_token() {
            Err(kind) => return Err(LexerError { module_id: module_id.clone(), kind }),
            Ok(tok) => match tok {
                Some(tok) => tokens.push(tok),
                None => break
            }
        }
    };

    Ok(tokens)
}

struct Lexer<'a> {
    input: PeekMoreIterator<Chars<'a>>,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a String) -> Self {
        let input = input.chars().peekmore();

        Lexer { input, line: 1, col: 0 }
    }

    fn advance(&mut self) -> Option<char> {
        self.col += 1;
        self.input.next()
    }

    fn expect_next(&mut self) -> Result<char, LexerErrorKind> {
        self.col += 1;
        self.input.next().ok_or(LexerErrorKind::UnexpectedEof(Position::new(self.line, self.col)))
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

    fn parse_unicode_escape(&mut self, start_pos: &Position) -> Result<char, LexerErrorKind> {
        let mut chars = Vec::new();
        for _ in 0..4 {
            let esc_seq = format!("\\u{}", chars.iter().join(""));

            let ch = self.expect_next().map_err(|_| {
                LexerErrorKind::UnsupportedEscapeSequence(start_pos.clone(), esc_seq.clone(), true)
            })?;
            if !ch.is_digit(16) {
                return Err(LexerErrorKind::UnsupportedEscapeSequence(start_pos.clone(), esc_seq, true));
            }
            chars.push(ch);
        }

        let esc_seq = format!("\\u{}", chars.iter().join(""));
        let i = u32::from_str_radix(&chars.iter().join(""), 16).map_err(|_| {
            LexerErrorKind::UnsupportedEscapeSequence(start_pos.clone(), esc_seq.clone(), true)
        })?;

        // Borrowed from the rust implementation of char::from_u32 (until it becomes stable): https://github.com/rust-lang/rust/blob/master/library/core/src/char/convert.rs#L203
        let ch = if (i > u32::max_value()) || (i >= 0xD800 && i <= 0xDFFF) {
            '�'
        } else {
            // SAFETY: checked that it's a legal unicode value
            unsafe { std::mem::transmute(i) }
        };
        Ok(ch)
    }

    fn next_token(&mut self) -> Result<Option<Token>, LexerErrorKind> {
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
                        self.input.advance_cursor();

                        if let Some(&ch) = self.input.peek() {
                            self.input.reset_cursor();

                            if ch.is_digit(10) && is_float {
                                let pos = Position::new(self.line, self.col + 1);
                                return Err(LexerErrorKind::UnexpectedChar(pos, ".".to_string()));
                            } else if ch.is_digit(10) {
                                is_float = true;
                                chars.push(self.expect_next()?);
                                continue;
                            } else {
                                break;
                            }
                        }
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
            let start_pos = Position::new(self.line, self.col);
            let mut pos = Position::new(self.line, self.col);
            let mut chunks = Vec::new();
            let mut chars: Vec<char> = vec![];

            loop {
                if let Some(&ch) = self.peek() {
                    if ch == '\n' {
                        return Err(LexerErrorKind::UnterminatedString(start_pos, Position::new(self.line, self.col + 1)));
                    } else if ch == '"' {
                        // Consume closing quote
                        self.expect_next()?;
                        break;
                    } else if ch == '\\' {
                        self.expect_next()?; // Consume '\'
                        let pos = Position::new(self.line, self.col);
                        let ch = match self.expect_next()? {
                            'n' => '\n',
                            '\\' => '\\',
                            'r' => '\r',
                            't' => '\t',
                            '\'' => '\'',
                            '"' => '"',
                            '$' => '$',
                            'u' => self.parse_unicode_escape(&pos)?,
                            ch => {
                                let esc_seq = format!("\\{}", ch);
                                return Err(LexerErrorKind::UnsupportedEscapeSequence(pos, esc_seq, false));
                            }
                        };

                        chars.push(ch);
                        continue;
                    } else if ch == '$' {
                        self.input.advance_cursor();
                        if self.input.peek().map_or(false, |ch| ch.is_alphabetic() || *ch == '_') {
                            let chunk = chars.into_iter().collect();
                            chunks.push(Token::String(pos.clone(), chunk));
                            chars = Vec::new();

                            self.expect_next()?; // Consume '$'
                            let next_tok = self.next_token()?.expect("There is at least 1 alphabetic character there");
                            chunks.push(next_tok);

                            pos = Position::new(self.line, self.col + 1);
                            continue;
                        } else if self.input.peek().map_or(false, |ch| *ch == '{') {
                            let chunk = chars.into_iter().collect();
                            chunks.push(Token::String(pos.clone(), chunk));
                            chars = Vec::new();

                            self.expect_next()?; // Consume '$'
                            self.expect_next()?; // Consume '{'
                            let mut num_braces = 1;

                            loop {
                                let next_tok = self.next_token()?;
                                let next_tok = next_tok.ok_or(LexerErrorKind::UnexpectedEof(Position::new(self.line, self.col)))?;
                                match &next_tok {
                                    Token::LBrace(_) => num_braces += 1,
                                    Token::RBrace(_) => num_braces -= 1,
                                    _ => {}
                                };
                                if num_braces == 0 {
                                    break;
                                } else {
                                    chunks.push(next_tok);
                                }
                            }

                            pos = Position::new(self.line, self.col + 1);
                            continue;
                        } else {
                            self.input.reset_cursor();
                        }
                    }

                    chars.push(self.expect_next()?);
                } else {
                    return Err(LexerErrorKind::UnterminatedString(start_pos, Position::new(self.line, self.col + 1)));
                }
            }

            let chunk = chars.into_iter().collect();
            return if chunks.is_empty() {
                Ok(Some(Token::String(start_pos, chunk)))
            } else {
                if !chunk.is_empty() {
                    chunks.push(Token::String(pos, chunk));
                }
                Ok(Some(Token::StringInterp(start_pos, chunks)))
            };
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
                "continue" => Token::Continue(pos),
                "for" => Token::For(pos),
                "in" => Token::In(pos),
                "match" => Token::Match(pos),
                "type" => Token::Type(pos),
                "enum" => Token::Enum(pos),
                "return" => {
                    let saw_newline = self.skip_whitespace();
                    let has_newline = saw_newline || self.peek().is_none();
                    Token::Return(pos, has_newline)
                }
                "readonly" => Token::Readonly(pos),
                "import" => Token::Import(pos),
                "export" => Token::Export(pos),
                "from" => Token::From(pos),
                "as" => Token::As(pos),
                "try" => Token::Try(pos),
                "None" => Token::None(pos),
                s => Token::Ident(pos, s.to_string())
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
                } else if let Some('*') = self.peek() {
                    self.expect_next()?; // Consume '*' token
                    Ok(Some(Token::StarStar(pos)))
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
            '^' => Ok(Some(Token::Caret(pos))),
            '&' => {
                let ch = self.expect_next()?;
                if ch != '&' {
                    let pos = Position::new(self.line, self.col);
                    Err(LexerErrorKind::UnexpectedChar(pos, ch.to_string()))
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
            '#' => {
                let ch = self.expect_next()?;
                if ch == '{' {
                    Ok(Some(Token::LBraceHash(pos)))
                } else {
                    let pos = Position::new(self.line, self.col);
                    Err(LexerErrorKind::UnexpectedChar(pos, ch.to_string()))
                }
            }
            ',' => Ok(Some(Token::Comma(pos))),
            ':' => Ok(Some(Token::Colon(pos))),
            '.' => Ok(Some(Token::Dot(pos))),
            '@' => Ok(Some(Token::At(pos))),
            _ => Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize<S: AsRef<str>>(input: S) -> Result<Vec<Token>, LexerErrorKind> {
        let module_id = ModuleId::parse_module_path("./test").unwrap();
        super::tokenize(&module_id, &input.as_ref().to_string()).map_err(|err| err.kind)
    }

    #[test]
    fn test_tokenize_ints() {
        let input = "123\n 456";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::Int(Position::new(1, 1), 123),
            Token::Int(Position::new(2, 2), 456),
        ];
        assert_eq!(expected, tokens);

        let input = "123.\n456.";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::Int(Position::new(1, 1), 123),
            Token::Dot(Position::new(1, 4)),
            Token::Int(Position::new(2, 1), 456),
            Token::Dot(Position::new(2, 4)),
        ];
        assert_eq!(expected, tokens);

        let input = "1.a";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::Int(Position::new(1, 1), 1),
            Token::Dot(Position::new(1, 2)),
            Token::Ident(Position::new(1, 3), "a".to_string()),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_floats() {
        let input = "1.23\n0.456";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::Float(Position::new(1, 1), 1.23),
            Token::Float(Position::new(2, 1), 0.456),
        ];
        assert_eq!(expected, tokens);

        let input = "1..23";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::Int(Position::new(1, 1), 1),
            Token::Dot(Position::new(1, 2)),
            Token::Dot(Position::new(1, 3)),
            Token::Int(Position::new(1, 4), 23),
        ];
        assert_eq!(expected, tokens);

        let input = "1.3.a";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::Float(Position::new(1, 1), 1.3),
            Token::Dot(Position::new(1, 4)),
            Token::Ident(Position::new(1, 5), "a".to_string()),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_single_char_operators() {
        let input = "+ - * / % < > ! = . ^";
        let tokens = tokenize(input).unwrap();
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
            Token::Caret(Position::new(1, 21)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_multi_char_operators() {
        let input = "&& || <= >= != == ?: ?. => **";
        let tokens = tokenize(input).unwrap();
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
            Token::StarStar(Position::new(1, 28)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_multi_char_eq_operators() {
        let input = "+= -= *= /= %= &&= ||= ?:=";
        let tokens = tokenize(input).unwrap();
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
    fn test_tokenize_separators() {
        let input = "( ) [ ] { } | , : ? #{ @";
        let tokens = tokenize(input).unwrap();
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
            Token::LBraceHash(Position::new(1, 21)),
            Token::At(Position::new(1, 24)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_multi_char_operators_error() {
        let input = "&";
        let error = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnexpectedEof(Position::new(1, 2));
        assert_eq!(expected, error);

        let input = "&+";
        let error = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnexpectedChar(Position::new(1, 2), "+".to_string());
        assert_eq!(expected, error);

        let input = "& &";
        let error = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnexpectedChar(Position::new(1, 2), " ".to_string());
        assert_eq!(expected, error);

        let input = "#+";
        let error = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnexpectedChar(Position::new(1, 2), "+".to_string());
        assert_eq!(expected, error);

        let input = "#";
        let error = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnexpectedEof(Position::new(1, 2));
        assert_eq!(expected, error);
    }

    #[test]
    fn test_tokenize_strings_empty() {
        let input = "\"\"";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::String(Position::new(1, 1), "".to_string())
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_strings() {
        let input = "\"hello wörld: 1 + 2   ! 👩🏻‍⚕️\"";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::String(Position::new(1, 1), "hello wörld: 1 + 2   ! 👩🏻‍⚕️".to_string())
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_strings_escape_sequence() {
        let input = "\"a\\nb\\tc\\\\nd\\'e\\\"f\\$$\"";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::String(Position::new(1, 1), "a\nb\tc\\nd'e\"f$$".to_string())
        ];
        assert_eq!(expected, tokens);

        let input = "\"\\u007a\\u306e\\u77e7\\ud801\"";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::String(Position::new(1, 1), "zの矧�".to_string())
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_strings_error() {
        let input = "\"";
        let tokens = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnterminatedString(Position::new(1, 1), Position::new(1, 2));
        assert_eq!(expected, tokens);

        let input = "\"\n\"";
        let tokens = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnterminatedString(Position::new(1, 1), Position::new(1, 2));
        assert_eq!(expected, tokens);

        let input = "\"\\z\"";
        let tokens = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnsupportedEscapeSequence(Position::new(1, 2), "\\z".to_string(), false);
        assert_eq!(expected, tokens);

        let input = "\"\\u38\"";
        let tokens = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnsupportedEscapeSequence(Position::new(1, 2), "\\u38".to_string(), true);
        assert_eq!(expected, tokens);

        let input = "\"\\u3xy8\"";
        let tokens = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnsupportedEscapeSequence(Position::new(1, 2), "\\u3".to_string(), true);
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_string_interpolation() {
        let input = "\"abc $a def\"";
        let tokens = tokenize(input).unwrap();
        let expected = Token::StringInterp(
            Position::new(1, 1),
            vec![
                Token::String(Position::new(1, 1), "abc ".to_string()),
                Token::Ident(Position::new(1, 7), "a".to_string()),
                Token::String(Position::new(1, 8), " def".to_string()),
            ],
        );
        assert_eq!(expected, tokens[0]);

        let input = "\"abc $def ghi\"";
        let tokens = tokenize(input).unwrap();
        let expected = Token::StringInterp(
            Position::new(1, 1),
            vec![
                Token::String(Position::new(1, 1), "abc ".to_string()),
                Token::Ident(Position::new(1, 7), "def".to_string()),
                Token::String(Position::new(1, 10), " ghi".to_string()),
            ],
        );
        assert_eq!(expected, tokens[0]);

        let input = "\"abc ${1 + 2} ghi\"";
        let tokens = tokenize(input).unwrap();
        let expected = Token::StringInterp(
            Position::new(1, 1),
            vec![
                Token::String(Position::new(1, 1), "abc ".to_string()),
                Token::Int(Position::new(1, 8), 1),
                Token::Plus(Position::new(1, 10)),
                Token::Int(Position::new(1, 12), 2),
                Token::String(Position::new(1, 14), " ghi".to_string()),
            ],
        );
        assert_eq!(expected, tokens[0]);

        let input = "\"abc ${1 +\n 2} ghi\"";
        let tokens = tokenize(input).unwrap();
        let expected = Token::StringInterp(
            Position::new(1, 1),
            vec![
                Token::String(Position::new(1, 1), "abc ".to_string()),
                Token::Int(Position::new(1, 8), 1),
                Token::Plus(Position::new(1, 10)),
                Token::Int(Position::new(2, 2), 2),
                Token::String(Position::new(2, 4), " ghi".to_string()),
            ],
        );
        assert_eq!(expected, tokens[0]);
    }

    #[test]
    fn test_tokenize_string_interpolation_error() {
        let input = "\"${1 +";
        let tokens = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnexpectedEof(Position::new(1, 7));
        assert_eq!(expected, tokens);

        let input = "\"$a ";
        let tokens = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnterminatedString(Position::new(1, 1), Position::new(1, 5));
        assert_eq!(expected, tokens);

        let input = "\"$a \n";
        let tokens = tokenize(input).unwrap_err();
        let expected = LexerErrorKind::UnterminatedString(Position::new(1, 1), Position::new(1, 5));
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_keywords() {
        let input = "true false val var if else func while break for in \
        type enum self match readonly import export from as try continue";
        let tokens = tokenize(input).unwrap();
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
            Token::Match(Position::new(1, 67)),
            Token::Readonly(Position::new(1, 73)),
            Token::Import(Position::new(1, 82)),
            Token::Export(Position::new(1, 89)),
            Token::From(Position::new(1, 96)),
            Token::As(Position::new(1, 101)),
            Token::Try(Position::new(1, 104)),
            Token::Continue(Position::new(1, 108)),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_keyword_return() {
        let tokens = tokenize("return").unwrap();
        let expected = vec![
            Token::Return(Position::new(1, 1), true),
        ];
        assert_eq!(expected, tokens);

        let tokens = tokenize("return     123").unwrap();
        let expected = vec![
            Token::Return(Position::new(1, 1), false),
            Token::Int(Position::new(1, 12), 123)
        ];
        assert_eq!(expected, tokens);

        let tokens = tokenize("return\n123").unwrap();
        let expected = vec![
            Token::Return(Position::new(1, 1), true),
            Token::Int(Position::new(2, 1), 123)
        ];
        assert_eq!(expected, tokens);

        let tokens = tokenize("return  \t  \n123").unwrap();
        let expected = vec![
            Token::Return(Position::new(1, 1), true),
            Token::Int(Position::new(2, 1), 123)
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_identifier() {
        let input = "abc abc1 abC_2";
        let tokens = tokenize(input).unwrap();
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
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::Int(Position::new(1, 1), 123),
            Token::Int(Position::new(3, 1), 456),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_tokenize_multi_line_comment() {
        let input = "123\n/* this\nshould\n* disappear\n*/ 456";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::Int(Position::new(1, 1), 123),
            Token::Int(Position::new(5, 4), 456),
        ];
        assert_eq!(expected, tokens);
    }
}
