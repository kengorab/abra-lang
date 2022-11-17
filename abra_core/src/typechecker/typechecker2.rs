use std::path::PathBuf;
use itertools::Either;
use crate::parser;
use crate::parser::parser::ParseResult;
use crate::lexer::lexer_error::LexerError;
use crate::lexer::tokens::{Range, Token};
use crate::parser::ast::{AstLiteralNode, AstNode, UnaryNode, UnaryOp};
use crate::parser::parse_error::ParseError;

pub trait LoadModule {
    fn resolve_path(&self, module_id: &parser::ast::ModuleId) -> String;

    fn load_file(&self, file_name: &String) -> String;

    fn load_untyped_ast(&self, module_id: &parser::ast::ModuleId) -> Result<(String, ParseResult), Either<LexerError, ParseError>> {
        use crate::{lexer::lexer, parser::parser};

        let file_name = self.resolve_path(&module_id);
        let file_contents = self.load_file(&file_name);

        match lexer::tokenize(module_id, &file_contents) {
            Err(e) => Err(Either::Left(e)),
            Ok(tokens) => match parser::parse(module_id.clone(), tokens) {
                Err(e) => Err(Either::Right(e)),
                Ok(nodes) => Ok((file_name, nodes))
            }
        }
    }
}

pub struct ModuleLoader<'a> {
    program_root: &'a PathBuf,
}

impl<'a> ModuleLoader<'a> {
    pub fn new(program_root: &'a PathBuf) -> ModuleLoader {
        ModuleLoader { program_root }
    }
}

impl<'a> LoadModule for ModuleLoader<'a> {
    fn resolve_path(&self, module_id: &parser::ast::ModuleId) -> String {
        let path = PathBuf::from(module_id.get_path(&self.program_root)).with_extension("abra").canonicalize().unwrap();
        path.to_str().unwrap().to_string()
    }

    fn load_file(&self, file_name: &String) -> String {
        std::fs::read_to_string(file_name).map_err(|err| {
            eprintln!("Could not read file {}: {}", file_name, err);
            std::process::exit(1);
        }).unwrap()
    }
}

#[derive(Debug, Default)]
pub struct Project {
    pub modules: Vec<TypedModule>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ModuleId {
    pub id: usize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct TypeId {
    pub module_id: ModuleId,
    pub id: usize,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Builtin(/* prelude_type_idx: */ usize)
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypedModule {
    pub id: ModuleId,
    pub name: String,
    pub types: Vec<Type>,
    pub code: Vec<TypedNode>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TypedNode {
    Literal { token: Token, value: TypedLiteral, type_id: TypeId },
    Unary { token: Token, op: UnaryOp, expr: Box<TypedNode> },
}

impl TypedNode {
    fn type_id(&self) -> &TypeId {
        match self {
            TypedNode::Literal { type_id, .. } => type_id,
            TypedNode::Unary { expr, .. } => expr.type_id(),
        }
    }

    fn span(&self) -> Range {
        match self {
            TypedNode::Literal { token, .. } => token.get_range(),
            TypedNode::Unary { token, expr, .. } => token.get_range().expand(&expr.span()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TypedLiteral {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

impl Eq for TypedLiteral {}

pub const PRELUDE_MODULE_ID: ModuleId = ModuleId { id: 0 };
pub const PRELUDE_INT_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 0 };
pub const PRELUDE_FLOAT_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 1 };
pub const PRELUDE_BOOL_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 2 };
pub const PRELUDE_STRING_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 3 };

pub type TypecheckError = Either<Either<LexerError, ParseError>, TypeError>;

#[derive(Debug, PartialEq)]
pub enum TypeError {
    TypeMismatch { span: Range, expected: Vec<TypeId>, received: TypeId },
}

pub struct Typechecker2<'a, L: LoadModule> {
    module_loader: &'a L,
    project: &'a mut Project,
}

impl<'a, L: LoadModule> Typechecker2<'a, L> {
    pub fn new(module_loader: &'a L, project: &'a mut Project) -> Typechecker2<'a, L> {
        Typechecker2 { module_loader, project }
    }

    /* UTILITIES */

    fn current_module_mut(&mut self) -> &mut TypedModule {
        self.project.modules.last_mut().expect("Internal error: there must always be a module being typechecked")
    }

    /* TYPECHECKING */

    pub fn typecheck_prelude(&mut self) {
        debug_assert!(self.project.modules.is_empty());

        let mut prelude_module = TypedModule { id: PRELUDE_MODULE_ID, name: "prelude".to_string(), types: vec![], code: vec![] };
        prelude_module.types.push(Type::Builtin(PRELUDE_INT_TYPE_ID.id));
        prelude_module.types.push(Type::Builtin(PRELUDE_FLOAT_TYPE_ID.id));
        prelude_module.types.push(Type::Builtin(PRELUDE_BOOL_TYPE_ID.id));
        prelude_module.types.push(Type::Builtin(PRELUDE_STRING_TYPE_ID.id));

        self.project.modules.push(prelude_module);
    }

    pub fn typecheck_module(&mut self, m_id: &parser::ast::ModuleId) -> Result<(), TypecheckError> {
        debug_assert!(self.project.modules.len() == 1, "Prelude must be loaded in order to typecheck further modules");

        let (file_name, parse_result) = self.module_loader.load_untyped_ast(&m_id).map_err(Either::Left)?;

        if !parse_result.imports.is_empty() {
            unimplemented!("Typechecking imports");
        }

        let id = ModuleId { id: self.project.modules.len() };
        self.project.modules.push(TypedModule { id, name: file_name, types: vec![], code: vec![] });

        self.typecheck_block(parse_result.nodes).map_err(Either::Right)?;

        Ok(())
    }

    fn typecheck_block(&mut self, nodes: Vec<AstNode>) -> Result<(), TypeError> {
        let mut func_decls = Vec::new();
        let mut type_decls = Vec::new();
        let mut enum_decls = Vec::new();
        let mut code = Vec::new();

        for node in nodes {
            match node {
                n @ AstNode::FunctionDecl(_, _) => func_decls.push(n),
                n @ AstNode::TypeDecl(_, _) => type_decls.push(n),
                n @ AstNode::EnumDecl(_, _) => enum_decls.push(n),
                n => code.push(n),
            }
        }

        debug_assert!(func_decls.is_empty());
        debug_assert!(type_decls.is_empty());
        debug_assert!(enum_decls.is_empty());

        for node in code {
            let typed_node = self.typecheck_statement(node)?;

            let current_module = self.current_module_mut();
            current_module.code.push(typed_node);
        }

        Ok(())
    }

    fn typecheck_statement(&mut self, node: AstNode) -> Result<TypedNode, TypeError> {
        match node {
            AstNode::BindingDecl(_, _) |
            AstNode::IfStatement(_, _) |
            AstNode::ForLoop(_, _) |
            AstNode::WhileLoop(_, _) |
            AstNode::Break(_) |
            AstNode::Continue(_) |
            AstNode::MatchStatement(_, _) |
            AstNode::ReturnStatement(_, _) => todo!(),
            AstNode::FunctionDecl(_, _) |
            AstNode::TypeDecl(_, _) |
            AstNode::EnumDecl(_, _) => unreachable!("Internal error: node should have been handled in typecheck_block"),
            AstNode::ImportStatement(_, _) => unreachable!("Internal error: imports should have been handled before typechecking the body"),
            n => self.typecheck_expression(n)
        }
    }

    fn typecheck_expression(&mut self, node: AstNode) -> Result<TypedNode, TypeError> {
        match node {
            AstNode::Literal(token, n) => match n {
                AstLiteralNode::IntLiteral(i) => Ok(TypedNode::Literal { token, value: TypedLiteral::Int(i), type_id: PRELUDE_INT_TYPE_ID }),
                AstLiteralNode::FloatLiteral(f) => Ok(TypedNode::Literal { token, value: TypedLiteral::Float(f), type_id: PRELUDE_FLOAT_TYPE_ID }),
                AstLiteralNode::BoolLiteral(b) => Ok(TypedNode::Literal { token, value: TypedLiteral::Bool(b), type_id: PRELUDE_BOOL_TYPE_ID }),
                AstLiteralNode::StringLiteral(s) => Ok(TypedNode::Literal { token, value: TypedLiteral::String(s), type_id: PRELUDE_STRING_TYPE_ID })
            }
            AstNode::Unary(token, n) => {
                let UnaryNode { op, expr } = n;

                let typed_expr = self.typecheck_expression(*expr)?;
                let type_id = typed_expr.type_id();

                let span = token.get_range().expand(&typed_expr.span());
                match op {
                    UnaryOp::Minus if *type_id != PRELUDE_INT_TYPE_ID && *type_id != PRELUDE_FLOAT_TYPE_ID => {
                        Err(TypeError::TypeMismatch { span, expected: vec![PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID], received: *type_id })
                    }
                    UnaryOp::Negate if *type_id != PRELUDE_BOOL_TYPE_ID => {
                        Err(TypeError::TypeMismatch { span, expected: vec![PRELUDE_BOOL_TYPE_ID], received: *type_id })
                    }
                    _ => Ok(TypedNode::Unary { token, op, expr: Box::new(typed_expr) })
                }
            }
            AstNode::Binary(_, _) |
            AstNode::Grouped(_, _) |
            AstNode::Array(_, _) |
            AstNode::Set(_, _) |
            AstNode::Map(_, _) |
            AstNode::Tuple(_, _) |
            AstNode::Identifier(_, _) |
            AstNode::Assignment(_, _) |
            AstNode::Indexing(_, _) |
            AstNode::Accessor(_, _) |
            AstNode::Invocation(_, _) |
            AstNode::IfExpression(_, _) |
            AstNode::MatchExpression(_, _) |
            AstNode::Lambda(_, _) |
            AstNode::Try(_, _) => todo!(),
            n => unreachable!("Internal error: node is not an expression: {:?}", n),
        }
    }
}
