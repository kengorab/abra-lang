use abra_core::lexer::tokens::{Token, Range};
use serde::{Serialize, Serializer};
use crate::js_value::position::JsPosition;

pub struct JsRange<'a>(pub &'a Range);

impl<'a> Serialize for JsRange<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        let Range { start, end } = &self.0;
        let mut obj = serializer.serialize_map(Some(2))?;
        obj.serialize_entry("start", &JsPosition(start))?;
        obj.serialize_entry("end", &JsPosition(end))?;
        obj.end()
    }
}

pub struct JsToken<'a>(pub &'a Token);

impl<'a> Serialize for JsToken<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match &self.0 {
            Token::Int(pos, val) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "int")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.serialize_entry("val", &val)?;
                obj.end()
            }
            Token::Float(pos, val) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "float")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.serialize_entry("val", &val)?;
                obj.end()
            }
            Token::String(pos, val) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "string")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.serialize_entry("val", &val)?;
                obj.end()
            }
            Token::StringInterp(pos, toks) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "stringInterp")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;

                let chunks = toks.iter().map(|t| JsToken(t)).collect::<Vec<_>>();
                obj.serialize_entry("chunks", &chunks)?;
                obj.end()
            }
            Token::Bool(pos, val) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "bool")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.serialize_entry("val", &val)?;
                obj.end()
            }
            Token::Func(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "func")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Val(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "val")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Var(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "var")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::If(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "if")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Else(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "else")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::While(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "while")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Break(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "break")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Continue(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "continue")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::For(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "for")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::In(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "in")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Match(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "match")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Type(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "type")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Enum(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "enum")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Interface(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "interface")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Readonly(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "readonly")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Import(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "import")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Export(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "export")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::From(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "from")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Return(pos, _) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "return")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Ident(pos, val) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "ident")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.serialize_entry("val", &val)?;
                obj.end()
            }
            Token::Self_(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "self")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::None(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "none")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Assign(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "assign")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Plus(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "plus")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::PlusEq(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "plusEq")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Minus(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "minus")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::MinusEq(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "minusEq")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Star(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "star")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::StarEq(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "starEq")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Slash(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "slash")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::SlashEq(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "slashEq")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Percent(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "percent")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::PercentEq(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "percentEq")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::StarStar(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "starStar")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::And(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "and")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::AndEq(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "andEq")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Or(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "or")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::OrEq(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "orEq")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Caret(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "caretCaret")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Elvis(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "elvis")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::ElvisEq(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "elvisEq")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::GT(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "gt")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::GTE(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "gte")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::LT(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "lt")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::LTE(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "lte")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Eq(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "eq")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Neq(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "neq")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Bang(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "bang")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::LParen(pos, is_preceded_by_newline) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "lParen")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.serialize_entry("isPrecededByNewline", is_preceded_by_newline)?;
                obj.end()
            }
            Token::RParen(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "rParen")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::LBrack(pos, is_preceded_by_newline) => {
                let mut obj = serializer.serialize_map(Some(3))?;
                obj.serialize_entry("kind", "lBrack")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.serialize_entry("isPrecededByNewline", is_preceded_by_newline)?;
                obj.end()
            }
            Token::RBrack(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "rBrack")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::LBrace(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "lBrace")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::RBrace(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "rBrace")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::LBraceHash(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "lBraceHash")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Pipe(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "pipe")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Colon(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "colon")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Comma(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "comma")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Question(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "question")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Dot(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "dot")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::QuestionDot(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "questionDot")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
            Token::Arrow(pos) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("kind", "arrow")?;
                obj.serialize_entry("pos", &JsPosition(pos))?;
                obj.end()
            }
        }
    }
}
