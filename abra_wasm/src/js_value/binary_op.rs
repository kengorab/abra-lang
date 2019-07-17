use abra_core::parser::ast::BinaryOp;
use serde::{Serialize, Serializer};

pub struct JsBinaryOp<'a>(pub &'a BinaryOp);

impl<'a> Serialize for JsBinaryOp<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let op_str = self.0.to_string().to_lowercase();
        serializer.serialize_str(&op_str)
    }
}
