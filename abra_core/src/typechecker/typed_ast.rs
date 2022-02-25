use crate::typechecker::types::Type;
use crate::parser::ast::{UnaryOp, BinaryOp, IndexingMode, LambdaNode, BindingPattern, ModuleId};
use crate::lexer::tokens::Token;
use crate::typechecker::typechecker::Scope;

#[derive(Clone, Debug, PartialEq)]
pub enum TypedAstNode {
    Literal(Token, TypedLiteralNode),
    Unary(Token, TypedUnaryNode),
    Binary(Token, TypedBinaryNode),
    Grouped(Token, TypedGroupedNode),
    Array(Token, TypedArrayNode),
    Map(Token, TypedMapNode),
    Set(Token, TypedSetNode),
    Tuple(Token, TypedTupleNode),
    Lambda(Token, TypedLambdaNode),
    BindingDecl(Token, TypedBindingDeclNode),
    FunctionDecl(Token, TypedFunctionDeclNode),
    TypeDecl(Token, TypedTypeDeclNode),
    EnumDecl(Token, TypedEnumDeclNode),
    Identifier(Token, TypedIdentifierNode),
    Assignment(Token, TypedAssignmentNode),
    Indexing(Token, TypedIndexingNode),
    IfStatement(Token, TypedIfNode),
    IfExpression(Token, TypedIfNode),
    Invocation(Token, TypedInvocationNode),
    Instantiation(Token, TypedInstantiationNode),
    ForLoop(Token, TypedForLoopNode),
    WhileLoop(Token, TypedWhileLoopNode),
    Break(Token),
    Continue(Token),
    ReturnStatement(Token, TypedReturnNode),
    Accessor(Token, TypedAccessorNode),
    MatchStatement(Token, TypedMatchNode),
    MatchExpression(Token, TypedMatchNode),
    ImportStatement(Token, TypedImportNode),
    _Nil(Token),
}

impl TypedAstNode {
    pub fn get_token(&self) -> &Token {
        match self {
            TypedAstNode::Literal(token, _) |
            TypedAstNode::Unary(token, _) |
            TypedAstNode::Binary(token, _) |
            TypedAstNode::Grouped(token, _) |
            TypedAstNode::Array(token, _) |
            TypedAstNode::Map(token, _) |
            TypedAstNode::Set(token, _) |
            TypedAstNode::Tuple(token, _) |
            TypedAstNode::Lambda(token, _) |
            TypedAstNode::BindingDecl(token, _) |
            TypedAstNode::FunctionDecl(token, _) |
            TypedAstNode::TypeDecl(token, _) |
            TypedAstNode::EnumDecl(token, _) |
            TypedAstNode::Identifier(token, _) |
            TypedAstNode::Assignment(token, _) |
            TypedAstNode::Indexing(token, _) |
            TypedAstNode::IfStatement(token, _) |
            TypedAstNode::IfExpression(token, _) |
            TypedAstNode::Invocation(token, _) |
            TypedAstNode::Instantiation(token, _) |
            TypedAstNode::ForLoop(token, _) |
            TypedAstNode::WhileLoop(token, _) |
            TypedAstNode::Break(token) |
            TypedAstNode::Continue(token) |
            TypedAstNode::ReturnStatement(token, _) |
            TypedAstNode::Accessor(token, _) |
            TypedAstNode::MatchStatement(token, _) |
            TypedAstNode::MatchExpression(token, _) |
            TypedAstNode::ImportStatement(token, _) |
            TypedAstNode::_Nil(token) => token,
        }
    }

    pub fn get_type(&self) -> Type {
        match &self {
            TypedAstNode::Literal(_, node) => match node {
                TypedLiteralNode::IntLiteral(_) => Type::Int,
                TypedLiteralNode::FloatLiteral(_) => Type::Float,
                TypedLiteralNode::StringLiteral(_) => Type::String,
                TypedLiteralNode::BoolLiteral(_) => Type::Bool,
            },
            TypedAstNode::Unary(_, node) => node.typ.clone(),
            TypedAstNode::Binary(_, node) => node.typ.clone(),
            TypedAstNode::Grouped(_, node) => node.typ.clone(),
            TypedAstNode::Array(_, node) => node.typ.clone(),
            TypedAstNode::Map(_, node) => node.typ.clone(),
            TypedAstNode::Set(_, node) => node.typ.clone(),
            TypedAstNode::Tuple(_, node) => node.typ.clone(),
            TypedAstNode::Lambda(_, node) => node.typ.clone(),
            TypedAstNode::FunctionDecl(_, _) |
            TypedAstNode::BindingDecl(_, _) |
            TypedAstNode::TypeDecl(_, _) |
            TypedAstNode::EnumDecl(_, _) |
            TypedAstNode::WhileLoop(_, _) |
            TypedAstNode::Break(_) |
            TypedAstNode::Continue(_) |
            TypedAstNode::ImportStatement(_, _) |
            TypedAstNode::ForLoop(_, _) => Type::Unit,
            TypedAstNode::ReturnStatement(_, node) => node.typ.clone(),
            TypedAstNode::Identifier(_, node) => node.typ.clone(),
            TypedAstNode::Assignment(_, node) => node.typ.clone(),
            TypedAstNode::Indexing(_, node) => node.typ.clone(),
            TypedAstNode::IfStatement(_, node) => node.typ.clone(),
            TypedAstNode::IfExpression(_, node) => node.typ.clone(),
            TypedAstNode::Invocation(_, node) => node.typ.clone(),
            TypedAstNode::Instantiation(_, node) => node.typ.clone(),
            TypedAstNode::Accessor(_, node) => node.typ.clone(),
            TypedAstNode::MatchStatement(_, node) => node.typ.clone(),
            TypedAstNode::MatchExpression(_, node) => node.typ.clone(),
            TypedAstNode::_Nil(_) => Type::Any,
        }
    }


    pub fn is_expression(&self) -> bool {
        match self {
            TypedAstNode::BindingDecl(_, _) |
            TypedAstNode::FunctionDecl(_, _) |
            TypedAstNode::TypeDecl(_, _) |
            TypedAstNode::EnumDecl(_, _) |
            TypedAstNode::IfStatement(_, _) |
            TypedAstNode::MatchStatement(_, _) |
            TypedAstNode::Break(_) | // This is here for completeness; the type for this node should never matter
            TypedAstNode::ForLoop(_, _) |
            TypedAstNode::WhileLoop(_, _) |
            TypedAstNode::ReturnStatement(_, _) |
            TypedAstNode::ImportStatement(_, _) => false,
            _ => true
        }
    }

    // The usage of Option<bool> here is weird:
    //   - if None, then no termination (break/continue/return) occurred
    //   - if Some(false), then a non-return occurred; Some(true) means a return occurred
    pub fn all_branches_terminate(&self) -> Option<bool> {
        match self {
            TypedAstNode::IfStatement(_, TypedIfNode { if_block, else_block, .. }) |
            TypedAstNode::IfExpression(_, TypedIfNode { if_block, else_block, .. }) => {
                let if_block_terminator = if_block.last().map(|n| n.all_branches_terminate()).unwrap_or(None);
                if if_block_terminator.is_none() {
                    None
                } else if let Some(else_block) = else_block {
                    let else_block_terminator = else_block.last().map(|n| n.all_branches_terminate()).unwrap_or(None);
                    match (&if_block_terminator, &else_block_terminator) {
                        (Some(true), Some(true)) => Some(true),
                        (Some(_), Some(_)) => Some(false),
                        (None, _) | (_, None) => None,
                    }
                } else {
                    None
                }
            }
            TypedAstNode::MatchStatement(_, TypedMatchNode { branches, .. }) |
            TypedAstNode::MatchExpression(_, TypedMatchNode { branches, .. }) => {
                branches.iter().fold(Some(true), |acc, (_, _, body)| {
                    let terminator = body.last().map(|n| n.all_branches_terminate()).unwrap_or(None);
                    match (acc, terminator) {
                        (Some(true), Some(true)) => Some(true),
                        (Some(_), Some(_)) => Some(false),
                        (None, _) | (_, None) => None,
                    }
                })
            }
            TypedAstNode::ForLoop(_, TypedForLoopNode { body, .. }) |
            TypedAstNode::WhileLoop(_, TypedWhileLoopNode { body, .. }) => {
                // A loop can only be guaranteed to terminate if all branches terminate in a return
                body.iter().find(|n| n.all_branches_terminate() == Some(true))
                    .map(|_| true)
            }
            TypedAstNode::ReturnStatement(_, _) => Some(true),
            TypedAstNode::Break(_) | TypedAstNode::Continue(_) => Some(false),
            _ => None
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedLiteralNode {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedUnaryNode {
    pub typ: Type,
    pub op: UnaryOp,
    pub expr: Box<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedBinaryNode {
    pub typ: Type,
    pub right: Box<TypedAstNode>,
    pub op: BinaryOp,
    pub left: Box<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedGroupedNode {
    pub typ: Type,
    pub expr: Box<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedArrayNode {
    pub typ: Type,
    pub items: Vec<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedMapNode {
    pub typ: Type,
    pub items: Vec<(TypedAstNode, TypedAstNode)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedSetNode {
    pub typ: Type,
    pub items: Vec<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedTupleNode {
    pub typ: Type,
    pub items: Vec<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedLambdaNode {
    pub idx: usize,
    pub typ: Type,
    pub args: Vec<(Token, Type, Option<TypedAstNode>)>,
    pub typed_body: Option<Vec<TypedAstNode>>,
    pub orig_node: Option<(LambdaNode, Vec<Scope>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedBindingDeclNode {
    pub binding: BindingPattern,
    pub expr: Option<Box<TypedAstNode>>,
    pub is_mutable: bool,
    pub is_exported: bool,
    pub scope_depth: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedFunctionDeclNode {
    // Must be a Token::Ident
    pub name: Token,
    // Tokens represent arg idents, and must be Token::Ident
    pub args: Vec<(Token, Type, bool, Option<TypedAstNode>)>,
    pub ret_type: Type,
    pub body: Vec<TypedAstNode>,
    pub scope_depth: usize,
    pub is_recursive: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedTypeDeclNode {
    // Must be a Token::Ident
    pub name: Token,
    // Tokens represent arg idents, and must be Token::Ident
    pub fields: Vec<TypedTypeDeclField>,
    pub static_fields: Vec<(Token, Type, Option<TypedAstNode>)>,
    pub methods: Vec<(String, TypedAstNode)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedTypeDeclField {
    pub ident: Token,
    pub typ: Type,
    pub default_value: Option<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum EnumVariantKind {
    Basic,
    Constructor(/* args: */ Vec<(Token, Type, Option<TypedAstNode>)>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedEnumDeclNode {
    // Must be a Token::Ident
    pub name: Token,
    // Tokens represent arg idents, and must be Token::Ident
    pub variants: Vec<(Token, (Type, Option<Vec<Option<TypedAstNode>>>))>,
    pub static_fields: Vec<(Token, Type, Option<TypedAstNode>)>,
    pub methods: Vec<(String, TypedAstNode)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedIdentifierNode {
    pub typ: Type,
    pub name: String,
    pub is_mutable: bool,
    pub scope_depth: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssignmentTargetKind {
    Identifier,
    ArrayIndex,
    MapIndex,
    Field,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedAssignmentNode {
    pub kind: AssignmentTargetKind,
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    pub expr: Box<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedIndexingNode {
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    pub index: IndexingMode<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedIfNode {
    pub typ: Type,
    pub condition: Box<TypedAstNode>,
    pub condition_binding: Option<BindingPattern>,
    pub if_block: Vec<TypedAstNode>,
    pub else_block: Option<Vec<TypedAstNode>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedInvocationNode {
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    pub args: Vec<Option<TypedAstNode>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedInstantiationNode {
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    pub fields: Vec<(String, TypedAstNode)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedWhileLoopNode {
    pub condition: Box<TypedAstNode>,
    pub condition_binding: Option<Token>,
    pub body: Vec<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedForLoopNode {
    pub binding: BindingPattern,
    pub index_ident: Option<Token>,
    pub iterator: Box<TypedAstNode>,
    pub body: Vec<TypedAstNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedAccessorNode {
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    // Must be Token::Ident
    pub field_ident: Token,
    pub field_idx: usize,
    pub is_opt_safe: bool,
    pub is_method: bool,
    pub is_readonly: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedMatchNode {
    pub typ: Type,
    pub target: Box<TypedAstNode>,
    pub branches: Vec<(/* match_kind: */ TypedMatchKind, /* binding: */ Option<String>, /* body: */ Vec<TypedAstNode>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedMatchCaseArgument {
    Pattern(BindingPattern),
    Literal(TypedAstNode),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedMatchKind {
    Wildcard,
    None,
    Type { type_name: String, args: Option<Vec<TypedMatchCaseArgument>> },
    EnumVariant { enum_name: String, variant_idx: usize, variant_name: String, args: Option<Vec<(String, TypedMatchCaseArgument)>> },
    Constant { node: TypedAstNode },
    Tuple { nodes: Vec<TypedAstNode> },
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedImportNode {
    pub imports: Vec<String>,
    pub module_id: ModuleId,
    pub alias_name: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedReturnNode {
    pub typ: Type,
    pub target: Option<Box<TypedAstNode>>,
}
