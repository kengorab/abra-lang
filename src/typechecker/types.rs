#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Or(Vec<Type>),
    Int,
    Float,
    String,
    Bool,
    Array(Option<Box<Type>>),
}
