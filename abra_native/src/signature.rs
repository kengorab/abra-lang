use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub struct Signature {
    pub(crate) func_name: String,
    pub(crate) args: Vec<MethodArgSpec>,
    pub(crate) return_type: TypeRepr,
}

#[derive(Debug, PartialEq)]
pub struct MethodArgSpec {
    pub(crate) name: String,
    pub(crate) typ: TypeRepr,
    pub(crate) is_optional: bool,
    pub(crate) is_variadic: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeRepr {
    SelfType(Vec<TypeRepr>),
    Ident(String, Vec<TypeRepr>),
    Fn(Vec<TypeRepr>, Box<TypeRepr>),
    Tuple(Vec<TypeRepr>),
    Array(Box<TypeRepr>),
    Union(Vec<TypeRepr>),
    Opt(Box<TypeRepr>),
    Generic(String),
    Unit,
}

pub fn parse_fn_signature(parent_type_name: Option<&String>, type_args: &Vec<String>, input: &String) -> Result<Signature, String> {
    let input = input.chars().filter(|ch| !ch.is_whitespace()).collect::<String>();

    SignatureParser::new(parent_type_name, type_args, &input).parse()
}

pub fn parse_type(type_name: Option<&String>, generics: &Vec<String>, input: &String) -> Result<TypeRepr, String> {
    let input = input.chars().filter(|ch| !ch.is_whitespace()).collect::<String>();

    let mut parser = SignatureParser::new(type_name, generics, &input);
    let type_repr = parser.parse_type_ident()?;
    parser.assert_consumed_all()?;

    Ok(type_repr)
}

struct SignatureParser<'a> {
    chars: Peekable<Chars<'a>>,
    parent_type_name: Option<&'a String>,
    parent_type_args: &'a Vec<String>,
    type_args: Vec<String>,
}

impl<'a> SignatureParser<'a> {
    pub fn new(parent_type_name: Option<&'a String>, type_args: &'a Vec<String>, input: &'a String) -> Self {
        let chars = input.chars().peekable();

        Self { chars, parent_type_name, parent_type_args: type_args, type_args: vec![] }
    }

    pub fn parse(&mut self) -> Result<Signature, String> {
        let func_name = self.parse_ident()?;

        if let Some('<') = self.chars.peek() {
            self.parse_expected_ch('<')?;

            loop {
                if let Some('>') = self.chars.peek() {
                    self.parse_expected_ch('>')?;
                    break;
                }

                let name = self.parse_ident()?;
                if self.parent_type_args.contains(&name) {
                    return Err(format!(
                        "Duplicate type parameter {} in signature; {} is already declared in the type declaration",
                        name, name
                    ));
                } else if self.type_args.contains(&name) {
                    return Err(format!(
                        "Duplicate type parameter {} in signature; {} is declared more than once in the signature",
                        name, name
                    ));
                }
                self.type_args.push(name);

                if let Some(ch) = self.chars.peek() {
                    if *ch == ',' {
                        self.parse_expected_ch(',')?;
                    }
                } else {
                    return Err("Expected token ',' or '>', got 'EOF'".to_string());
                }
            }
        }

        let args = self.parse_fn_args()?;

        let return_type = if let Some(':') = self.chars.peek() {
            self.parse_expected_ch(':')?;

            match self.parse_type_ident()? {
                TypeRepr::Array(inner) if self.parent_type_name == Some(&"Array".to_string()) => {
                    TypeRepr::SelfType(vec![*inner])
                }
                TypeRepr::Ident(s, _) if self.parent_type_name == Some(&"String".to_string()) && s == "String" => {
                    TypeRepr::SelfType(vec![])
                }
                t => t
            }
        } else {
            TypeRepr::Unit
        };

        Ok(Signature { func_name, args, return_type })
    }

    fn parse_fn_args(&mut self) -> Result<Vec<MethodArgSpec>, String> {
        self.parse_expected_ch('(')?;

        let mut seen_variadic = false;
        let mut args = Vec::new();

        loop {
            if let Some(')') = self.chars.peek() {
                self.parse_expected_ch(')')?;
                break;
            }

            let is_variadic = if let Some('*') = self.chars.peek() {
                self.parse_expected_ch('*')?;
                if seen_variadic {
                    return Err("Unexpected variadic parameter; only the last parameter may be marked as variadic".to_string());
                }
                seen_variadic = true;
                true
            } else { false };

            let name = self.parse_ident()?;

            let is_optional = if let Some('?') = self.chars.peek() {
                self.parse_expected_ch('?')?;
                true
            } else { false };
            self.parse_expected_ch(':')?;

            let typ = self.parse_type_ident()?;

            if is_variadic {
                if let TypeRepr::Array(_) = typ {} else {
                    return Err(format!("Variadic parameter `{}` must be an Array type", name));
                }
            }
            let is_optional = is_optional || is_variadic;

            args.push(MethodArgSpec { name, typ, is_optional, is_variadic });

            if let Some(ch) = self.chars.peek() {
                if *ch == ',' {
                    self.parse_expected_ch(',')?;
                }
            } else {
                return Err("Expected token ',' or ')', got 'EOF'".to_string());
            }
        }

        Ok(args)
    }

    fn parse_type_ident(&mut self) -> Result<TypeRepr, String> {
        let mut base = match self.chars.peek() {
            Some('(') => self.parse_type_ident_grouped()?,
            Some(ch) if ch.is_alphanumeric() => {
                let base = self.parse_ident()?;
                let is_self = if let Some(parent_type_name) = &self.parent_type_name {
                    base == **parent_type_name
                } else { false };

                let generics = if let Some('<') = self.chars.peek() {
                    self.parse_expected_ch('<')?;
                    let mut generics = Vec::new();

                    loop {
                        if let Some('>') = self.chars.peek() {
                            self.parse_expected_ch('>')?;
                            break;
                        }
                        let mut t = self.parse_type_ident()?;
                        if let TypeRepr::Ident(name, g) = &t {
                            if self.parent_type_args.contains(&name) || self.type_args.contains(&name) {
                                if !g.is_empty() {
                                    return Err(format!("Unexpected type arguments for generic type {}", name));
                                }
                                t = TypeRepr::Generic(name.clone());
                            }
                        }

                        generics.push(t);

                        if let Some(ch) = self.chars.peek() {
                            if *ch == ',' {
                                self.parse_expected_ch(',')?;
                            }
                        } else {
                            return Err("Expected token ',' or '>', got 'EOF'".to_string());
                        }
                    }

                    generics
                } else { vec![] };

                if is_self {
                    TypeRepr::SelfType(generics)
                } else if self.parent_type_args.contains(&base) || self.type_args.contains(&base) {
                    TypeRepr::Generic(base)
                } else {
                    TypeRepr::Ident(base, generics)
                }
            }
            _ => {
                let next = self.chars.next().map_or("EOF".to_string(), |ch| ch.to_string());
                return Err(format!("Expected token 'identifier' or '(', got '{}'", next));
            }
        };

        loop {
            match self.chars.peek() {
                Some('?') => {
                    self.parse_expected_ch('?')?;
                    base = TypeRepr::Opt(Box::new(base));
                }
                Some('[') => {
                    self.parse_expected_ch('[')?;
                    self.parse_expected_ch(']')?;
                    base = TypeRepr::Array(Box::new(base));
                }
                Some('|') => {
                    self.parse_expected_ch('|')?;

                    // Flatten nested union types
                    let next = self.parse_type_ident()?;
                    if let TypeRepr::Union(mut opts) = next {
                        let mut union_opts = vec![base];
                        union_opts.append(&mut opts);
                        base = TypeRepr::Union(union_opts)
                    } else {
                        base = TypeRepr::Union(vec![base, next])
                    }
                }
                _ => break
            }
        }

        Ok(base)
    }

    fn parse_type_ident_grouped(&mut self) -> Result<TypeRepr, String> {
        self.parse_expected_ch('(')?;

        let mut idents = Vec::new();

        loop {
            if let Some(')') = self.chars.peek() {
                self.parse_expected_ch(')')?;
                break;
            }

            idents.push(self.parse_type_ident()?);

            if let Some(ch) = self.chars.peek() {
                if *ch == ',' {
                    self.parse_expected_ch(',')?;
                }
            } else {
                return Err("Expected token ',' or ')', got 'EOF'".to_string());
            }
        }

        if let Some('=') = self.chars.peek() {
            self.parse_arrow()?;

            let ret_type = self.parse_type_ident()?;
            Ok(TypeRepr::Fn(idents, Box::new(ret_type)))
        } else {
            Ok(TypeRepr::Tuple(idents))
        }
    }

    fn parse_ident(&mut self) -> Result<String, String> {
        let mut chars = Vec::new();
        while let Some(ch) = self.chars.peek() {
            if ch.is_alphanumeric() {
                chars.push(self.chars.next().unwrap());
            } else {
                break;
            }
        }
        if chars.is_empty() {
            let next = self.chars.next().map_or("EOF".to_string(), |ch| ch.to_string());
            Err(format!("Expected token 'identifier', got '{}'", next))
        } else {
            Ok(chars.into_iter().collect())
        }
    }

    fn parse_expected_ch(&mut self, expected: char) -> Result<char, String> {
        if let Some(ch) = self.chars.peek() {
            if *ch == expected {
                self.chars.next(); // Consume char
                return Ok(expected);
            }
        }
        let next = self.chars.next().map_or("EOF".to_string(), |ch| ch.to_string());
        Err(format!("Expected token '{}', got '{}'", expected, next))
    }

    fn parse_arrow(&mut self) -> Result<&str, String> {
        if let Some('=') = self.chars.peek() {
            self.parse_expected_ch('=')?;
            if let Some('>') = self.chars.peek() {
                self.parse_expected_ch('>')?;
                Ok("=>")
            } else {
                let next = self.chars.next().map_or("EOF".to_string(), |ch| ch.to_string());
                Err(format!("Expected token '=>', got '=' followed by '{}'", next))
            }
        } else {
            let next = self.chars.next().map_or("EOF".to_string(), |ch| ch.to_string());
            Err(format!("Expected token '=>', got '{}'", next))
        }
    }

    pub fn assert_consumed_all(&mut self) -> Result<(), String> {
        if let Some(ch) = self.chars.peek() {
            Err(format!("Unexpected character {}", ch))
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod test {
    use crate::signature::{parse_fn_signature, Signature, MethodArgSpec, TypeRepr, parse_type};

    fn parse(input: &str) -> Signature {
        parse_fn_signature(None, &vec![], &input.to_string()).unwrap()
    }

    #[test]
    fn test_parse_signature_no_args() {
        let sig = parse("abc(): Int");
        let expected = Signature {
            func_name: "abc".to_string(),
            args: vec![],
            return_type: TypeRepr::Ident("Int".to_string(), vec![]),
        };
        assert_eq!(expected, sig);
    }

    #[test]
    fn test_parse_signature_no_return() {
        let sig = parse("abc()");
        let expected = Signature {
            func_name: "abc".to_string(),
            args: vec![],
            return_type: TypeRepr::Unit,
        };
        assert_eq!(expected, sig);
    }

    #[test]
    fn test_parse_signature_basic_args() {
        let sig = parse("abc(i: Int, f: Float): Int");
        let expected = Signature {
            func_name: "abc".to_string(),
            args: vec![
                MethodArgSpec {
                    name: "i".to_string(),
                    typ: TypeRepr::Ident("Int".to_string(), vec![]),
                    is_optional: false,
                    is_variadic: false,
                },
                MethodArgSpec {
                    name: "f".to_string(),
                    typ: TypeRepr::Ident("Float".to_string(), vec![]),
                    is_optional: false,
                    is_variadic: false,
                }
            ],
            return_type: TypeRepr::Ident("Int".to_string(), vec![]),
        };
        assert_eq!(expected, sig);
    }

    #[test]
    fn test_parse_signature_complex_args() {
        let sig = parse(
            "abc(i: (Int) => Int[], f: (Int, Bool[]) => Float?[]): Map<Int, Set<String?>[]>"
        );
        let expected = Signature {
            func_name: "abc".to_string(),
            args: vec![
                MethodArgSpec {
                    name: "i".to_string(),
                    typ: TypeRepr::Fn(
                        vec![
                            TypeRepr::Ident("Int".to_string(), vec![]),
                        ],
                        Box::new(TypeRepr::Array(
                            Box::new(TypeRepr::Ident("Int".to_string(), vec![]))
                        )),
                    ),
                    is_optional: false,
                    is_variadic: false,
                },
                MethodArgSpec {
                    name: "f".to_string(),
                    typ: TypeRepr::Fn(
                        vec![
                            TypeRepr::Ident("Int".to_string(), vec![]),
                            TypeRepr::Array(Box::new(TypeRepr::Ident("Bool".to_string(), vec![]))),
                        ],
                        Box::new(TypeRepr::Array(
                            Box::new(TypeRepr::Opt(
                                Box::new(TypeRepr::Ident("Float".to_string(), vec![]))
                            ))
                        )),
                    ),
                    is_optional: false,
                    is_variadic: false,
                }
            ],
            return_type: TypeRepr::Ident(
                "Map".to_string(),
                vec![
                    TypeRepr::Ident("Int".to_string(), vec![]),
                    TypeRepr::Array(
                        Box::new(TypeRepr::Ident(
                            "Set".to_string(),
                            vec![
                                TypeRepr::Opt(
                                    Box::new(TypeRepr::Ident("String".to_string(), vec![]))
                                )
                            ],
                        ))
                    )
                ],
            ),
        };
        assert_eq!(expected, sig);
    }

    #[test]
    fn test_parse_signature_with_generics() {
        let sig = parse("abc<X, Y, Z>(i: X, f: Y): Z");
        let expected = Signature {
            func_name: "abc".to_string(),
            args: vec![
                MethodArgSpec {
                    name: "i".to_string(),
                    typ: TypeRepr::Generic("X".to_string()),
                    is_optional: false,
                    is_variadic: false,
                },
                MethodArgSpec {
                    name: "f".to_string(),
                    typ: TypeRepr::Generic("Y".to_string()),
                    is_optional: false,
                    is_variadic: false,
                }
            ],
            return_type: TypeRepr::Generic("Z".to_string()),
        };
        assert_eq!(expected, sig);
    }

    #[test]
    fn test_parse_signature_with_generics_error() {
        let err = parse_fn_signature(
            None,
            &vec!["X".to_string()],
            &"abc<X, Y, Z>(i: X, f: Y): Z".to_string(),
        ).unwrap_err();
        let expected = "Duplicate type parameter X in signature; X is already declared in the type declaration".to_string();
        assert_eq!(expected, err);
    }

    #[test]
    fn test_parse_signature_returns_self_type() {
        let sig = parse_fn_signature(
            Some(&"MyType".to_string()),
            &vec![],
            &"abc(m: MyType): MyType".to_string(),
        ).unwrap();
        let expected = Signature {
            func_name: "abc".to_string(),
            args: vec![
                MethodArgSpec {
                    name: "m".to_string(),
                    typ: TypeRepr::SelfType(vec![]),
                    is_optional: false,
                    is_variadic: false,
                }
            ],
            return_type: TypeRepr::SelfType(vec![]),
        };
        assert_eq!(expected, sig);

        let sig = parse_fn_signature(
            Some(&"MyType".to_string()),
            &vec!["K".to_string(), "V".to_string()],
            &"abc<T, U>(m: MyType<K, V>): MyType<T, U>".to_string(),
        ).unwrap();
        let expected = Signature {
            func_name: "abc".to_string(),
            args: vec![
                MethodArgSpec {
                    name: "m".to_string(),
                    typ: TypeRepr::SelfType(vec![
                        TypeRepr::Generic("K".to_string()),
                        TypeRepr::Generic("V".to_string()),
                    ]),
                    is_optional: false,
                    is_variadic: false,
                }
            ],
            return_type: TypeRepr::SelfType(vec![
                TypeRepr::Generic("T".to_string()),
                TypeRepr::Generic("U".to_string()),
            ]),
        };
        assert_eq!(expected, sig);
    }

    #[test]
    fn test_parse_type_union() {
        let type_repr = parse_type(
            Some(&"".to_string()),
            &vec![],
            &"Int | Float | String[]".to_string(),
        ).unwrap();
        let expected = TypeRepr::Union(vec![
            TypeRepr::Ident("Int".to_string(), vec![]),
            TypeRepr::Ident("Float".to_string(), vec![]),
            TypeRepr::Array(Box::new(
                TypeRepr::Ident("String".to_string(), vec![]),
            )),
        ]);
        assert_eq!(expected, type_repr);
    }
}
