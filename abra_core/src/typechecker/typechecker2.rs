use std::path::PathBuf;
use itertools::Either;
use crate::parser;
use crate::parser::parser::ParseResult;
use crate::lexer::lexer_error::LexerError;
use crate::lexer::tokens::{Range, Token};
use crate::parser::ast::{AstLiteralNode, AstNode, BindingDeclNode, BindingPattern, TypeIdentifier, UnaryNode, UnaryOp};
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ScopeId {
    pub module_id: ModuleId,
    pub id: usize,
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub id: ScopeId,
    pub vars: Vec<Variable>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VarId {
    pub scope_id: ScopeId,
    pub id: usize,
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub id: VarId,
    pub name: String,
    pub type_id: TypeId,
    pub is_mutable: bool,
    pub is_initialized: bool,
    pub defined_span: Range,
}

#[derive(Debug, PartialEq)]
pub struct TypedModule {
    pub id: ModuleId,
    pub name: String,
    pub types: Vec<Type>,
    pub code: Vec<TypedNode>,
    pub scopes: Vec<Scope>,
}

#[derive(Debug, PartialEq)]
pub enum TypedNode {
    // Expressions
    Literal { token: Token, value: TypedLiteral, type_id: TypeId },
    Unary { token: Token, op: UnaryOp, expr: Box<TypedNode> },

    // Statements
    BindingDeclaration { token: Token, pattern: BindingPattern, vars: Vec<VarId>, expr: Option<Box<TypedNode>> },
}

impl TypedNode {
    fn type_id(&self) -> &TypeId {
        match self {
            // Expressions
            TypedNode::Literal { type_id, .. } => type_id,
            TypedNode::Unary { expr, .. } => expr.type_id(),

            // Statements
            TypedNode::BindingDeclaration { .. } => &PRELUDE_UNIT_TYPE_ID,
        }
    }

    fn span(&self) -> Range {
        match self {
            // Expressions
            TypedNode::Literal { token, .. } => token.get_range(),
            TypedNode::Unary { token, expr, .. } => token.get_range().expand(&expr.span()),

            // Statements
            TypedNode::BindingDeclaration { token, pattern, expr, .. } => {
                let start = token.get_range();
                if let Some(expr) = expr {
                    start.expand(&expr.span())
                } else {
                    match pattern {
                        BindingPattern::Variable(v) => start.expand(&v.get_range()),
                        BindingPattern::Tuple(_, _) |
                        BindingPattern::Array(_, _, _) => todo!()
                    }
                }
            }
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
pub const PRELUDE_UNIT_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 0 };
pub const PRELUDE_INT_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 1 };
pub const PRELUDE_FLOAT_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 2 };
pub const PRELUDE_BOOL_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 3 };
pub const PRELUDE_STRING_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 4 };

pub type TypecheckError = Either<Either<LexerError, ParseError>, TypeError>;

#[derive(Debug, PartialEq)]
pub enum TypeError {
    TypeMismatch { span: Range, expected: Vec<TypeId>, received: TypeId },
    UnknownType { span: Range },
    MissingBindingInitializer { span: Range, is_also_missing_type_hint: bool },
    DuplicateBinding { span: Range, original_span: Range },
}

pub struct Typechecker2<'a, L: LoadModule> {
    module_loader: &'a L,
    project: &'a mut Project,
    current_scope: usize,
}

impl<'a, L: LoadModule> Typechecker2<'a, L> {
    pub fn new(module_loader: &'a L, project: &'a mut Project) -> Typechecker2<'a, L> {
        Typechecker2 { module_loader, project, current_scope: 0 }
    }

    /* UTILITIES */

    fn current_module_mut(&mut self) -> &mut TypedModule {
        self.project.modules.last_mut().expect("Internal error: there must always be a module being typechecked")
    }

    fn type_satisfies_other(&self, base_type: &TypeId, target_type: &TypeId) -> bool {
        base_type == target_type
    }

    fn resolve_type_identifier(&self, type_identifier: &TypeIdentifier) -> Result<TypeId, TypeError> {
        match type_identifier {
            TypeIdentifier::Normal { ident, .. } => {
                let ident_name = Token::get_ident_name(ident);
                match ident_name.as_str() {
                    "Unit" => Ok(PRELUDE_UNIT_TYPE_ID),
                    "Int" => Ok(PRELUDE_INT_TYPE_ID),
                    "Float" => Ok(PRELUDE_FLOAT_TYPE_ID),
                    "Bool" => Ok(PRELUDE_BOOL_TYPE_ID),
                    "String" => Ok(PRELUDE_STRING_TYPE_ID),
                    _ => Err(TypeError::UnknownType { span: ident.get_range() })
                }
            }
            TypeIdentifier::Array { .. } |
            TypeIdentifier::Tuple { .. } |
            TypeIdentifier::Option { .. } |
            TypeIdentifier::Union { .. } |
            TypeIdentifier::Func { .. } => todo!()
        }
    }

    fn add_variable_to_current_scope(&mut self, name: String, type_id: TypeId, is_mutable: bool, is_initialized: bool, span: Range) -> Result<VarId, TypeError> {
        let current_scope_idx = self.current_scope;
        let current_scope = &mut self.current_module_mut().scopes[current_scope_idx];

        for var in &current_scope.vars {
            if var.name == name {
                return Err(TypeError::DuplicateBinding { span, original_span: var.defined_span.clone() });
            }
        }

        let id = VarId { scope_id: current_scope.id, id: current_scope.vars.len() };
        let var = Variable { id, name, type_id, is_mutable, is_initialized, defined_span: span };
        current_scope.vars.push(var);

        Ok(id)
    }

    /* TYPECHECKING */

    pub fn typecheck_prelude(&mut self) {
        debug_assert!(self.project.modules.is_empty());

        let mut prelude_module = TypedModule { id: PRELUDE_MODULE_ID, name: "prelude".to_string(), types: vec![], code: vec![], scopes: vec![] };
        prelude_module.types.push(Type::Builtin(PRELUDE_UNIT_TYPE_ID.id));
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
        let scope_id = ScopeId { module_id: id, id: 0 };
        let root_scope = Scope { id: scope_id, vars: vec![] };
        self.project.modules.push(TypedModule {
            id,
            name: file_name,
            types: vec![],
            code: vec![],
            scopes: vec![root_scope],
        });

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
            AstNode::BindingDecl(token, n) => {
                let BindingDeclNode { export_token, binding, type_ann, expr, is_mutable } = n;
                if export_token.is_some() { unimplemented!("Internal error: imports/exports") }

                let BindingPattern::Variable(var_token) = &binding else { unimplemented!() };
                let var_name = Token::get_ident_name(&var_token);
                let var_span = var_token.get_range();

                let type_hint_id = if let Some(type_identifier) = type_ann {
                    Some(self.resolve_type_identifier(&type_identifier)?)
                } else { None };

                let mut vars = vec![];
                let typed_expr = match (type_hint_id, expr) {
                    (None, None) => return Err(TypeError::MissingBindingInitializer { span: var_token.get_range(), is_also_missing_type_hint: true }),
                    (Some(type_hint_id), None) => {
                        if !is_mutable {
                            return Err(TypeError::MissingBindingInitializer { span: var_token.get_range(), is_also_missing_type_hint: false });
                        }
                        let var_id = self.add_variable_to_current_scope(var_name, type_hint_id, is_mutable, false, var_span)?;
                        vars.push(var_id);

                        None
                    }
                    (None, Some(expr)) => {
                        let typed_expr = self.typecheck_expression(*expr, None)?;
                        let var_id = self.add_variable_to_current_scope(var_name, *typed_expr.type_id(), is_mutable, true, var_span)?;
                        vars.push(var_id);

                        Some(Box::new(typed_expr))
                    }
                    (Some(type_hint_id), Some(expr)) => {
                        let typed_expr = self.typecheck_expression(*expr, Some(type_hint_id))?;

                        let type_id = typed_expr.type_id();
                        if !self.type_satisfies_other(type_id, &type_hint_id) {
                            let span = typed_expr.span();
                            return Err(TypeError::TypeMismatch { span, expected: vec![type_hint_id], received: *type_id });
                        }

                        let var_id = self.add_variable_to_current_scope(var_name, type_hint_id, is_mutable, true, var_span)?;
                        vars.push(var_id);

                        Some(Box::new(typed_expr))
                    }
                };

                Ok(TypedNode::BindingDeclaration { token, pattern: binding, vars, expr: typed_expr })
            }
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
            n => self.typecheck_expression(n, None)
        }
    }

    fn typecheck_expression(&mut self, node: AstNode, _type_hint: Option<TypeId>) -> Result<TypedNode, TypeError> {
        match node {
            AstNode::Literal(token, n) => match n {
                AstLiteralNode::IntLiteral(i) => Ok(TypedNode::Literal { token, value: TypedLiteral::Int(i), type_id: PRELUDE_INT_TYPE_ID }),
                AstLiteralNode::FloatLiteral(f) => Ok(TypedNode::Literal { token, value: TypedLiteral::Float(f), type_id: PRELUDE_FLOAT_TYPE_ID }),
                AstLiteralNode::BoolLiteral(b) => Ok(TypedNode::Literal { token, value: TypedLiteral::Bool(b), type_id: PRELUDE_BOOL_TYPE_ID }),
                AstLiteralNode::StringLiteral(s) => Ok(TypedNode::Literal { token, value: TypedLiteral::String(s), type_id: PRELUDE_STRING_TYPE_ID })
            }
            AstNode::Unary(token, n) => {
                let UnaryNode { op, expr } = n;

                let typed_expr = self.typecheck_expression(*expr, None)?;
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
