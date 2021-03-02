use crate::lexer::tokens::Token;
use itertools::Itertools;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub enum AstNode {
    Literal(Token, AstLiteralNode),
    Unary(Token, UnaryNode),
    Binary(Token, BinaryNode),
    Grouped(Token, GroupedNode),
    Array(Token, ArrayNode),
    Set(Token, SetNode),
    Map(Token, MapNode),
    BindingDecl(Token, BindingDeclNode),
    FunctionDecl(Token, FunctionDeclNode),
    TypeDecl(Token, TypeDeclNode),
    EnumDecl(Token, EnumDeclNode),
    Identifier(Token, Option<Vec<TypeIdentifier>>),
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
    MatchStatement(Token, MatchNode),
    MatchExpression(Token, MatchNode),
    Tuple(Token, Vec<AstNode>),
    ReturnStatement(Token, Option<Box<AstNode>>),
    ImportStatement(Token, ImportNode),
}

impl AstNode {
    pub fn get_token(&self) -> &Token {
        match self {
            AstNode::Literal(token, _) |
            AstNode::Unary(token, _) |
            AstNode::Binary(token, _) |
            AstNode::Grouped(token, _) |
            AstNode::Array(token, _) |
            AstNode::Map(token, _) |
            AstNode::Set(token, _) |
            AstNode::Tuple(token, _) |
            AstNode::Lambda(token, _) |
            AstNode::BindingDecl(token, _) |
            AstNode::FunctionDecl(token, _) |
            AstNode::TypeDecl(token, _) |
            AstNode::EnumDecl(token, _) |
            AstNode::Identifier(token, _) |
            AstNode::Assignment(token, _) |
            AstNode::Indexing(token, _) |
            AstNode::IfStatement(token, _) |
            AstNode::IfExpression(token, _) |
            AstNode::Invocation(token, _) |
            AstNode::ForLoop(token, _) |
            AstNode::WhileLoop(token, _) |
            AstNode::Break(token) |
            AstNode::ReturnStatement(token, _) |
            AstNode::ImportStatement(token, _) |
            AstNode::Accessor(token, _) |
            AstNode::MatchStatement(token, _) |
            AstNode::MatchExpression(token, _) => token,
        }
    }
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
    AddEq,
    Sub,
    SubEq,
    Mul,
    MulEq,
    Div,
    DivEq,
    Mod,
    ModEq,
    And,
    AndEq,
    Or,
    OrEq,
    Xor,
    Coalesce,
    CoalesceEq,
    Lt,
    Lte,
    Gt,
    Gte,
    Neq,
    Eq,
    Pow,
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
    pub items: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SetNode {
    pub items: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MapNode {
    pub items: Vec<(Token, AstNode)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BindingPattern {
    Variable(/* ident: */ Token),
    Tuple(/* lparen_tok: */ Token, /* patterns: */ Vec<BindingPattern>),
    Array(/* lbrack_tok: */ Token, /* patterns: */ Vec<(BindingPattern, /* is_splat: */ bool)>, /* is_string: */ bool),
}

impl BindingPattern {
    pub fn get_token(&self) -> &Token {
        match &self {
            BindingPattern::Variable(ident) => ident,
            BindingPattern::Tuple(lparen_tok, _) => lparen_tok,
            BindingPattern::Array(lbrack_tok, _, _) => lbrack_tok,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BindingDeclNode {
    pub export_token: Option<Token>,
    pub binding: BindingPattern,
    pub type_ann: Option<TypeIdentifier>,
    pub expr: Option<Box<AstNode>>,
    pub is_mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclNode {
    pub export_token: Option<Token>,
    // Must be a Token::Ident
    pub name: Token,
    // Must be a Token::Idents
    pub type_args: Vec<Token>,
    // Tokens represent arg idents, and must be Token::Ident
    pub args: Vec<(Token, Option<TypeIdentifier>, bool, Option<AstNode>)>,
    pub ret_type: Option<TypeIdentifier>,
    pub body: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LambdaNode {
    pub args: Vec<(Token, Option<TypeIdentifier>, bool, Option<AstNode>)>,
    pub body: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDeclNode {
    pub export_token: Option<Token>,
    // Must be a Token::Ident
    pub name: Token,
    // Must be Token::Idents
    pub type_args: Vec<Token>,
    pub fields: Vec<TypeDeclField>,
    pub methods: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDeclField {
    pub ident: Token,
    pub type_ident: TypeIdentifier,
    pub default_value: Option<AstNode>,
    pub readonly: Option<Token>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumDeclNode {
    pub export_token: Option<Token>,
    // Must be a Token::Ident
    pub name: Token,
    // Must be Token::Idents
    pub type_args: Vec<Token>,
    // Tokens represent arg idents, and must be Token::Ident
    pub variants: Vec<(/* ident: */ Token, /* args: */ Option<Vec<(Token, Option<TypeIdentifier>, bool, Option<AstNode>)>>)>,
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
    pub condition_binding: Option<BindingPattern>,
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
    pub binding: BindingPattern,
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
    // Must be an AstNode::Identifier
    pub field: Box<AstNode>,
    pub is_opt_safe: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchNode {
    pub target: Box<AstNode>,
    pub branches: Vec<(MatchCase, Vec<AstNode>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchCase {
    pub token: Token,
    pub match_type: MatchCaseType,
    pub case_binding: Option<Token>,
    pub args: Option<Vec<BindingPattern>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MatchCaseType {
    Ident(Token),
    Compound(Vec<Token>),
    Wildcard(Token),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImportNode {
    pub imports: Vec<Token>,
    pub star_token: Option<Token>,
    pub leading_dot_token: Option<Token>,
    pub path: Vec<Token>,
}

impl ImportNode {
    pub fn get_path(&self) -> (bool, String) {
        let is_local_import = self.leading_dot_token.is_some();
        let path = self.path.iter().map(|p| Token::get_ident_name(p)).join("/");
        (is_local_import, path)
    }

    pub fn get_module_id(&self) -> ModuleId {
        let is_local_import = self.leading_dot_token.is_some();
        let path = self.path.iter().map(|p| Token::get_ident_name(p)).collect();
        ModuleId(is_local_import, path)
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct ModuleId(pub bool, pub Vec<String>);

impl Display for ModuleId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.get_name())
    }
}

impl ModuleId {
    pub fn get_name(&self) -> String {
        let name = self.1.join(".");
        format!("{}{}", if self.0 { "." } else { "" }, name)
    }

    pub fn get_path(&self, extension: &str) -> String {
        let path = self.1.join("/");
        format!("{}.{}", path, extension)
    }

    pub fn from_path(path: &String) -> Self {
        ModuleId(true, path.replace(".abra", "").split("/").map(|s| s.to_string()).collect())
    }

    pub fn from_name<S: AsRef<str>>(name: S) -> Self {
        let is_local = name.as_ref().starts_with(".");
        let parts = name.as_ref().split(".")
            .map(|s| s.to_string())
            .filter(|s| !s.is_empty())
            .collect();
        ModuleId(is_local, parts)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeIdentifier {
    Normal { ident: Token, type_args: Option<Vec<TypeIdentifier>> },
    Array { inner: Box<TypeIdentifier> },
    Tuple { types: Vec<TypeIdentifier> },
    Option { inner: Box<TypeIdentifier> },
    Union { left: Box<TypeIdentifier>, right: Box<TypeIdentifier> },
    Func {
        args: Vec<TypeIdentifier>,
        ret: Box<TypeIdentifier>,
    },
}

impl TypeIdentifier {
    pub fn get_ident(&self) -> Token {
        match self {
            TypeIdentifier::Normal { ident, .. } => ident.clone(),
            TypeIdentifier::Array { inner } => inner.get_ident(),
            TypeIdentifier::Tuple { types } => types.iter().next().unwrap().get_ident(),
            TypeIdentifier::Option { inner } => inner.get_ident(),
            TypeIdentifier::Union { left, .. } => left.get_ident(),
            TypeIdentifier::Func { ret, .. } => ret.get_ident()
        }
    }
}
