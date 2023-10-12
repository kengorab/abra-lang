use crate::lexer::tokens::{Range, Token};
use std::path::Path;
use std::hash::{Hash, Hasher};

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
    Continue(Token),
    Accessor(Token, AccessorNode),
    Try(Token, TryNode),
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
            AstNode::Continue(token) |
            AstNode::ReturnStatement(token, _) |
            AstNode::ImportStatement(token, _) |
            AstNode::Accessor(token, _) |
            AstNode::Try(token, _) |
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

impl Eq for AstLiteralNode {}

impl Hash for AstLiteralNode {
    fn hash<H: Hasher>(&self, h: &mut H) {
        match self {
            AstLiteralNode::IntLiteral(i) => i.hash(h),
            AstLiteralNode::FloatLiteral(f) => f.to_string().hash(h),
            AstLiteralNode::StringLiteral(s) => s.hash(h),
            AstLiteralNode::BoolLiteral(b) => b.hash(h),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

impl BinaryOp {
    pub(crate) fn repr(&self) -> &str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::AddEq => "+=",
            BinaryOp::Sub => "-",
            BinaryOp::SubEq => "-=",
            BinaryOp::Mul => "*",
            BinaryOp::MulEq => "*=",
            BinaryOp::Div => "/",
            BinaryOp::DivEq => "/=",
            BinaryOp::Mod => "%",
            BinaryOp::ModEq => "%=",
            BinaryOp::And => "&&",
            BinaryOp::AndEq => "&&=",
            BinaryOp::Or => "||",
            BinaryOp::OrEq => "||=",
            BinaryOp::Xor => "^",
            BinaryOp::Coalesce => "?:",
            BinaryOp::CoalesceEq => "?:=",
            BinaryOp::Lt => "<",
            BinaryOp::Lte => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Gte => ">=",
            BinaryOp::Neq => "!=",
            BinaryOp::Eq => "==",
            BinaryOp::Pow => "**",
        }
    }
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
    pub items: Vec<(AstNode, AstNode)>,
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

    pub fn get_span(&self) -> Range {
        match self {
            BindingPattern::Variable(ident_token) => ident_token.get_range(),
            BindingPattern::Tuple(lparen_token, patterns) => {
                let start = lparen_token.get_range();
                if let Some(span) = patterns.last().map(|i| i.get_span()) {
                    start.expand(&span)
                } else {
                    start
                }
            }
            BindingPattern::Array(lbrack_token, patterns, _) => {
                let start = lbrack_token.get_range();
                if let Some(span) = patterns.last().map(|(p, _)| p.get_span()) {
                    start.expand(&span)
                } else {
                    start
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BindingDeclNode {
    pub decorators: Vec<DecoratorNode>,
    pub export_token: Option<Token>,
    pub binding: BindingPattern,
    pub type_ann: Option<TypeIdentifier>,
    pub expr: Option<Box<AstNode>>,
    pub is_mutable: bool,
}

#[derive(Debug)]
pub struct Parameter<'a> {
    pub ident: &'a Token,
    pub type_ident: &'a Option<TypeIdentifier>,
    pub is_vararg: bool,
    pub default_value: &'a Option<AstNode>,
}

pub fn args_to_parameters(raw_arg_tuple: &(Token, Option<TypeIdentifier>, bool, Option<AstNode>)) -> Parameter {
    let (arg_ident, type_ident, is_vararg, default_value) = raw_arg_tuple;
    Parameter { ident: arg_ident, type_ident, is_vararg: *is_vararg, default_value }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclNode {
    pub decorators: Vec<DecoratorNode>,
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

impl FunctionDeclNode {
    pub fn parameters(&self) -> Vec<Parameter> {
        self.args.iter().map(args_to_parameters).collect()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LambdaNode {
    pub args: Vec<(/* arg_ident: */ Token, /* type_ident: */ Option<TypeIdentifier>, /* is_vararg: */ bool, /* default_value: */ Option<AstNode>)>,
    pub body: Vec<AstNode>,
}

impl LambdaNode {
    pub fn parameters(&self) -> Vec<Parameter> {
        self.args.iter().map(args_to_parameters).collect()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDeclNode {
    pub decorators: Vec<DecoratorNode>,
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
    pub decorators: Vec<DecoratorNode>,
    pub export_token: Option<Token>,
    // Must be a Token::Ident
    pub name: Token,
    // Must be Token::Idents
    pub type_args: Vec<Token>,
    // Tokens represent arg idents, and must be Token::Ident
    pub variants: Vec<(/* ident: */ Token, /* args: */ Option<Vec<(/* arg_ident: */ Token, /* type_ident: */ Option<TypeIdentifier>, /* is_varargs: */ bool, /* default_value */ Option<AstNode>)>>)>,
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
pub struct TryNode {
    pub expr: Box<AstNode>,
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
}

#[derive(Clone, Debug, PartialEq)]
pub enum MatchCaseArgument {
    Pattern(BindingPattern),
    Literal(AstNode),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MatchCaseType {
    None(Token),
    Ident(/* ident_tok: */ Token, /* args: */ Option<Vec<MatchCaseArgument>>),
    Compound(/* idents: */ Vec<Token>, /* args: */ Option<Vec<MatchCaseArgument>>),
    Wildcard(Token),
    Constant(AstNode),
    Tuple(Token, Vec<AstNode>),
}

impl MatchCaseType {
    pub fn get_lit_args(args: &Option<Vec<MatchCaseArgument>>) -> Option<Vec<Option<AstLiteralNode>>> {
        if let Some(args) = args {
            let mut saw_lit = false;
            let mut lits = Vec::new();
            for arg in args {
                let lit = match arg {
                    MatchCaseArgument::Pattern(_) => None,
                    MatchCaseArgument::Literal(ast) => {
                        saw_lit = true;
                        match ast {
                            AstNode::Literal(_, lit) => Some(lit.clone()),
                            _ => unreachable!("Literals in match cases must be literal nodes")
                        }
                    }
                };
                lits.push(lit);
            }

            if saw_lit { Some(lits) } else { None }
        } else { None }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImportKind {
    ImportAll(/* star_token: */ Token),
    ImportList(/* imports: */ Vec<Token>),
    Alias(/* alias_token: */ Token),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImportNode {
    pub kind: ImportKind,
    pub module_token: Token,
    pub module_id: ModuleId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DecoratorNode {
    pub at_token: Token,
    // Must be a Token::Ident
    pub name: Token,
    pub args: Vec<(Option<Token>, AstNode)>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModulePathSegment {
    CurrentDir,
    UpDir,
    Directory(String),
    Module(String)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModuleId {
    External(/* module_name: */ String),
    Internal(/* path: */ Vec<ModulePathSegment>),
}

const EXTENSION: &str = "abra";

impl ModuleId {
    pub fn get_path<P: AsRef<Path>>(&self, root: P) -> String {
        let mut path = root.as_ref().join("");
        match &self {
            ModuleId::External(n) => path = path.join(n),
            ModuleId::Internal(segments) => {
                for seg in segments {
                    match seg {
                        ModulePathSegment::CurrentDir => path = path.join("."),
                        ModulePathSegment::UpDir => path = path.join(".."),
                        ModulePathSegment::Directory(d) => path = path.join(d),
                        ModulePathSegment::Module(m) => path = path.join(m)
                    }
                }
            }
        }

        path.to_str().unwrap().to_string()
    }

    pub fn parse_module_path<S: AsRef<str>>(module_path: S) -> Option<ModuleId> {
        let segments = module_path.as_ref().split("/").collect::<Vec<_>>();

        let num_segments = segments.len();
        let mut path = vec![];
        for (idx, seg) in segments.into_iter().enumerate() {
            if seg.is_empty() { return None; }

            let segment = if seg == "." {
                ModulePathSegment::CurrentDir
            } else if seg == ".." {
                ModulePathSegment::UpDir
            } else {
                let is_valid = seg.chars().any(|ch| {
                    ch.is_alphanumeric() || ch == '.' || ch == '_' || ch == '-'
                });
                if !is_valid { return None; }

                if idx == num_segments - 1 {
                    ModulePathSegment::Module(seg.replace(&format!(".{}", EXTENSION), ""))
                } else {
                    ModulePathSegment::Directory(seg.to_string())
                }
            };
            path.push(segment);
        }

        if let Some(ModulePathSegment::Module(s)) = path.last() {
            if s.starts_with(".") { return None; }
        }

        let m = match path.first() {
            None => return None,
            Some(ModulePathSegment::UpDir | ModulePathSegment::CurrentDir) => ModuleId::Internal(path),
            Some(ModulePathSegment::Directory(name) | ModulePathSegment::Module(name)) => {
                if path.len() != 1 {
                    // TODO: Support nested external/builtin imports (ie. time/date)?
                    // Right now, if the first segment is a directory, the rest of the path is discarded
                    unimplemented!()
                }

                ModuleId::External(name.clone())
            }
        };

        Some(m)
    }

    pub fn is_prelude(&self) -> bool {
        if let ModuleId::External(name) = &self { name == "prelude" } else { false }
    }

    pub fn prelude() -> Self {
        Self::parse_module_path("prelude").unwrap()
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
