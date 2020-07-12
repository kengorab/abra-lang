use crate::lexer::tokens::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum AstNode {
    Literal(Token, AstLiteralNode),
    Unary(Token, UnaryNode),
    Binary(Token, BinaryNode),
    Grouped(Token, GroupedNode),
    Array(Token, ArrayNode),
    Map(Token, MapNode),
    BindingDecl(Token, BindingDeclNode),
    FunctionDecl(Token, FunctionDeclNode),
    TypeDecl(Token, TypeDeclNode),
    Identifier(Token),
    Assignment(Token, AssignmentNode),
    Indexing(Token, IndexingNode),
    IfStatement(Token, IfNode),
    IfExpression(Token, IfNode),
    Invocation(Token, InvocationNode),
    ForLoop(Token, ForLoopNode),
    WhileLoop(Token, WhileLoopNode),
    Break(Token),
    Accessor(Token, AccessorNode),
    Lambda(Token, LambdaNode),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstLiteralNode {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Minus,
    Negate,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryNode {
    pub op: UnaryOp,
    pub expr: Box<AstNode>,
}

#[derive(Clone, Debug, Display, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Coalesce,
    Lt,
    Lte,
    Gt,
    Gte,
    Neq,
    Eq,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryNode {
    pub right: Box<AstNode>,
    pub op: BinaryOp,
    pub left: Box<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GroupedNode {
    pub expr: Box<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayNode {
    pub items: Vec<Box<AstNode>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MapNode {
    pub items: Vec<(Token, AstNode)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BindingDeclNode {
    // Must be a Token::Ident
    pub ident: Token,
    pub type_ann: Option<TypeIdentifier>,
    pub expr: Option<Box<AstNode>>,
    pub is_mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclNode {
    // Must be a Token::Ident
    pub name: Token,
    // Tokens represent arg idents, and must be Token::Ident
    pub args: Vec<(Token, Option<TypeIdentifier>, Option<AstNode>)>,
    pub ret_type: Option<TypeIdentifier>,
    pub body: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LambdaNode {
    pub args: Vec<(Token, Option<TypeIdentifier>, Option<AstNode>)>,
    pub body: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDeclNode {
    // Must be a Token::Ident
    pub name: Token,
    // Tokens represent arg idents, and must be Token::Ident
    pub fields: Vec<(Token, TypeIdentifier, Option<AstNode>)>,
    pub methods: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AssignmentNode {
    pub target: Box<AstNode>,
    pub expr: Box<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum IndexingMode<T> {
    Index(Box<T>),
    Range(Option<Box<T>>, Option<Box<T>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct IndexingNode {
    pub target: Box<AstNode>,
    pub index: IndexingMode<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfNode {
    pub condition: Box<AstNode>,
    pub condition_binding: Option<Token>,
    pub if_block: Vec<AstNode>,
    pub else_block: Option<Vec<AstNode>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InvocationNode {
    pub target: Box<AstNode>,
    pub args: Vec<(Option<Token>, AstNode)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForLoopNode {
    pub iteratee: Token,
    pub index_ident: Option<Token>,
    pub iterator: Box<AstNode>,
    pub body: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileLoopNode {
    pub condition: Box<AstNode>,
    pub condition_binding: Option<Token>,
    pub body: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AccessorNode {
    pub target: Box<AstNode>,
    pub field: Token,
    pub is_opt_safe: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeIdentifier {
    Normal { ident: Token },
    Array { inner: Box<TypeIdentifier> },
    Option { inner: Box<TypeIdentifier> },
    Func {
        args: Vec<TypeIdentifier>,
        ret: Box<TypeIdentifier>
    },
}

impl TypeIdentifier {
    pub fn get_ident(&self) -> Token {
        match self {
            TypeIdentifier::Normal { ident } => ident.clone(),
            TypeIdentifier::Array { inner } => inner.get_ident(),
            TypeIdentifier::Option { inner } => inner.get_ident(),
            TypeIdentifier::Func { ret, .. } => ret.get_ident()
        }
    }
}
