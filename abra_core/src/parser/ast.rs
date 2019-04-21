use crate::lexer::tokens::Token;

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Literal(Token, AstLiteralNode),
    Unary(Token, UnaryNode),
    Binary(Token, BinaryNode),
    Array(Token, ArrayNode),
    BindingDecl(Token, BindingDeclNode),
    Identifier(Token),
    Assignment(Token, AssignmentNode),
}

#[derive(Debug, PartialEq)]
pub enum AstLiteralNode {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Minus,
    Negate,
}

#[derive(Debug, PartialEq)]
pub struct UnaryNode {
    pub op: UnaryOp,
    pub expr: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Lt,
    Lte,
    Gt,
    Gte,
    Neq,
    Eq,
}

#[derive(Debug, PartialEq)]
pub struct BinaryNode {
    pub right: Box<AstNode>,
    pub op: BinaryOp,
    pub left: Box<AstNode>,
}

#[derive(Debug, PartialEq)]
pub struct ArrayNode {
    pub items: Vec<Box<AstNode>>,
}

#[derive(Debug, PartialEq)]
pub struct BindingDeclNode {
    // Must be a Token::Ident
    pub ident: Token,
    pub type_ann: Option<TypeIdentifier>,
    pub expr: Option<Box<AstNode>>,
    pub is_mutable: bool,
}

#[derive(Debug, PartialEq)]
pub struct AssignmentNode {
    pub target: Box<AstNode>,
    pub expr: Box<AstNode>,
}

#[derive(Debug, PartialEq)]
pub struct TypeIdentifier {
    pub ident: Token,
    pub is_arr: bool,
}
