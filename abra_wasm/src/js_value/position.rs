use abra_core::lexer::tokens::Position;
use serde::{Serialize, Serializer};

pub struct JsPosition<'a>(pub &'a Position);

impl<'a> Serialize for JsPosition<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeSeq;

        let Position { line, col } = self.0;
        let mut seq = serializer.serialize_seq(Some(2))?;
        seq.serialize_element(&line)?;
        seq.serialize_element(&col)?;
        seq.end()
    }
}
