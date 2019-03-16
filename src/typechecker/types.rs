#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unknown,
    Or(Vec<Type>),
    Int,
    Float,
}
