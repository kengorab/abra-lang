#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Or(Vec<Type>),
    Int,
    Float,
    String,
}
