#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Unit,
    Or(Vec<Type>),
    Int,
    Float,
    String,
    Bool,
    Array(Option<Box<Type>>),
}
