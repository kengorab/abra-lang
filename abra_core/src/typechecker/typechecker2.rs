use std::collections::{HashSet, VecDeque};
use std::path::PathBuf;
use itertools::{Either, Itertools};
use crate::parser;
use crate::parser::parser::ParseResult;
use crate::lexer::lexer_error::LexerError;
use crate::lexer::tokens::{Range, Token};
use crate::parser::ast::{AstLiteralNode, AstNode, BindingDeclNode, BindingPattern, FunctionDeclNode, InvocationNode, TypeDeclField, TypeDeclNode, TypeIdentifier, UnaryNode, UnaryOp};
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

#[derive(Debug)]
pub struct Project {
    pub modules: Vec<TypedModule>,

    // cached values
    pub prelude_option_struct_id: StructId,
    pub prelude_array_struct_id: StructId,
    pub prelude_tuple_struct_id: StructId,
    pub prelude_set_struct_id: StructId,
    pub prelude_map_struct_id: StructId,
}

impl Default for Project {
    fn default() -> Self {
        let placeholder_struct_id = StructId(PRELUDE_MODULE_ID, 0);

        Self {
            modules: vec![],
            prelude_option_struct_id: placeholder_struct_id,
            prelude_array_struct_id: placeholder_struct_id,
            prelude_tuple_struct_id: placeholder_struct_id,
            prelude_set_struct_id: placeholder_struct_id,
            prelude_map_struct_id: placeholder_struct_id,
        }
    }
}

impl Project {
    pub fn prelude_module(&self) -> &TypedModule {
        &self.modules[PRELUDE_MODULE_ID.0]
    }

    pub fn get_type_by_id(&self, type_id: &TypeId) -> &Type {
        let TypeId(ScopeId(ModuleId(module_idx), scope_idx), idx) = type_id;
        let scope = &self.modules[*module_idx].scopes[*scope_idx];
        &scope.types[*idx]
    }

    pub fn get_struct_by_id(&self, struct_id: &StructId) -> &Struct {
        let StructId(ModuleId(module_idx), idx) = struct_id;
        let module = &self.modules[*module_idx];
        &module.structs[*idx]
    }

    pub fn get_struct_by_id_mut(&mut self, struct_id: &StructId) -> &mut Struct {
        let StructId(ModuleId(module_idx), idx) = struct_id;
        let module = &mut self.modules[*module_idx];
        &mut module.structs[*idx]
    }

    pub fn get_func_by_id(&self, func_id: &FuncId) -> &Function {
        let FuncId(ScopeId(ModuleId(module_idx), scope_idx), idx) = func_id;
        let scope = &self.modules[*module_idx].scopes[*scope_idx];
        &scope.funcs[*idx]
    }

    pub fn get_func_by_id_mut(&mut self, func_id: &FuncId) -> &mut Function {
        let FuncId(ScopeId(ModuleId(module_idx), scope_idx), idx) = func_id;
        let scope = &mut self.modules[*module_idx].scopes[*scope_idx];
        &mut scope.funcs[*idx]
    }

    pub fn get_var_by_id(&self, var_id: &VarId) -> &Variable {
        let VarId(ScopeId(ModuleId(module_idx), scope_idx), idx) = var_id;
        let scope = &self.modules[*module_idx].scopes[*scope_idx];
        &scope.vars[*idx]
    }

    pub fn get_var_by_id_mut(&mut self, var_id: &VarId) -> &mut Variable {
        let VarId(ScopeId(ModuleId(module_idx), scope_idx), idx) = var_id;
        let scope = &mut self.modules[*module_idx].scopes[*scope_idx];
        &mut scope.vars[*idx]
    }

    pub fn find_struct_by_name(&self, module_id: &ModuleId, name: &String) -> Option<&Struct> {
        let module = &self.modules[module_id.0];
        module.structs.iter()
            .find(|s| s.name == *name)
            .or_else(|| {
                // If struct cannot be found in current module, look in the prelude module
                self.prelude_module().structs.iter()
                    .find(|s| s.name == *name)
            })
    }

    pub fn find_type_id(&self, module_id: &ModuleId, ty: &Type) -> Option<TypeId> {
        let module = &self.modules[module_id.0];
        for type_id in module.type_ids.iter() {
            let type_by_id = self.get_type_by_id(type_id);
            if type_by_id == ty {
                return Some(*type_id);
            }
        }

        None
    }

    fn add_type_id(&mut self, scope_id: &ScopeId, ty: Type) -> TypeId {
        let ScopeId(ModuleId(module_id), scope_idx) = scope_id;
        let module = &mut self.modules[*module_id];
        let scope = &mut module.scopes[*scope_idx];

        let type_id = TypeId(*scope_id, scope.types.len());

        scope.types.push(ty);
        module.type_ids.push(type_id);

        type_id
    }

    pub fn add_or_find_type_id(&mut self, scope_id: &ScopeId, ty: Type) -> TypeId {
        let ScopeId(module_id, _) = scope_id;

        if let Some(type_id) = self.find_type_id(&module_id, &ty) {
            type_id
        } else {
            self.add_type_id(scope_id, ty)
        }
    }

    pub fn find_variable_by_name(&self, scope_id: &ScopeId, name: &String) -> Option<&Variable> {
        let mut scope_id = &Some(*scope_id);
        while let Some(ScopeId(ModuleId(module_idx), idx)) = scope_id {
            let scope = &self.modules[*module_idx].scopes[*idx];

            for var in &scope.vars {
                if var.name == *name {
                    return Some(var);
                }
            }

            scope_id = &scope.parent;
        }

        None
    }

    pub fn option_type(&self, mut inner_type_id: TypeId) -> Type {
        let mut inner = self.get_type_by_id(&inner_type_id);
        loop {
            match inner {
                Type::GenericInstance(struct_id, generic_ids) if *struct_id == self.prelude_option_struct_id => {
                    inner_type_id = generic_ids[0];
                    inner = self.get_type_by_id(&inner_type_id);
                }
                _ => break
            }
        }

        Type::GenericInstance(self.prelude_option_struct_id, vec![inner_type_id])
    }

    pub fn array_type(&self, inner_type_id: TypeId) -> Type {
        Type::GenericInstance(self.prelude_array_struct_id, vec![inner_type_id])
    }

    pub fn tuple_type(&self, inner_type_ids: Vec<TypeId>) -> Type {
        Type::GenericInstance(self.prelude_tuple_struct_id, inner_type_ids)
    }

    pub fn set_type(&self, inner_type_id: TypeId) -> Type {
        Type::GenericInstance(self.prelude_set_struct_id, vec![inner_type_id])
    }

    pub fn map_type(&self, key_type_id: TypeId, val_type_id: TypeId) -> Type {
        Type::GenericInstance(self.prelude_map_struct_id, vec![key_type_id, val_type_id])
    }

    pub fn function_type(&self, param_type_ids: Vec<TypeId>, return_type_id: TypeId) -> Type {
        Type::Function(param_type_ids, return_type_id)
    }

    pub fn struct_type(&self, struct_id: StructId) -> Type {
        Type::Struct(struct_id)
    }

    pub fn type_repr(&self, type_id: &TypeId) -> String {
        let ty = self.get_type_by_id(type_id);
        match ty {
            Type::Builtin(builtin_id) => {
                if *builtin_id == PRELUDE_UNIT_TYPE_ID.1 {
                    "Unit".to_string()
                } else if *builtin_id == PRELUDE_INT_TYPE_ID.1 {
                    "Int".to_string()
                } else if *builtin_id == PRELUDE_FLOAT_TYPE_ID.1 {
                    "Float".to_string()
                } else if *builtin_id == PRELUDE_BOOL_TYPE_ID.1 {
                    "Bool".to_string()
                } else if *builtin_id == PRELUDE_STRING_TYPE_ID.1 {
                    "String".to_string()
                } else {
                    unreachable!("Unknown builtin type: {}", builtin_id)
                }
            }
            Type::Generic(name) => name.to_string(),
            Type::GenericInstance(struct_id, generic_ids) => {
                if *struct_id == self.prelude_option_struct_id {
                    debug_assert!(generic_ids.len() == 1, "An option should have and only 1 generic type");
                    let inner_type_repr = self.type_repr(&generic_ids[0]);
                    format!("{}?", inner_type_repr)
                } else if *struct_id == self.prelude_array_struct_id {
                    debug_assert!(generic_ids.len() == 1, "An array should have and only 1 generic type");
                    let inner_type_repr = self.type_repr(&generic_ids[0]);
                    format!("{}[]", inner_type_repr)
                } else if *struct_id == self.prelude_tuple_struct_id {
                    let inner_type_reprs = generic_ids.iter().map(|type_id| self.type_repr(type_id)).join(", ");
                    format!("({})", inner_type_reprs)
                } else {
                    let struct_ = self.get_struct_by_id(struct_id);
                    let inner_type_reprs = generic_ids.iter().map(|type_id| self.type_repr(type_id)).join(", ");
                    format!("{}<{}>", struct_.name, inner_type_reprs)
                }
            }
            Type::Function(param_type_ids, return_type_id) => {
                let param_reprs = param_type_ids.iter().map(|type_id| self.type_repr(type_id)).join(", ");
                let return_repr = self.type_repr(return_type_id);
                format!("({}) => {}", param_reprs, return_repr)
            }
            Type::Struct(struct_id) => {
                let struct_ = self.get_struct_by_id(struct_id);
                struct_.name.to_string()
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ModuleId(/* idx: */ pub usize);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct StructId(/* module_id: */ pub ModuleId, /* idx: */ pub usize);

#[derive(Debug, PartialEq)]
pub struct Struct {
    pub id: StructId,
    pub name: String,
    // Structs with no defined_span are builtins
    pub defined_span: Option<Range>,
    pub generics: Option<Vec<TypeId>>,
    pub fields: Vec<StructField>,
    pub methods: Vec<FuncId>,
    pub static_methods: Vec<FuncId>,
}

#[derive(Debug, PartialEq)]
pub struct StructField {
    pub name: String,
    pub type_id: TypeId,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct TypeId(/* scope_id: */ pub ScopeId, /* idx: */ pub usize);

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Builtin(/* prelude_type_idx: */ usize),
    Generic(String),
    GenericInstance(StructId, Vec<TypeId>),
    Function(Vec<TypeId>, TypeId),
    Struct(StructId),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ScopeId(/* module_id: */ pub ModuleId, /* idx: */ pub usize);

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub label: String,
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub types: Vec<Type>,
    pub vars: Vec<Variable>,
    pub funcs: Vec<Function>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VarId(/* scope_id: */ pub ScopeId, /* idx: */ pub usize);

#[derive(Debug, PartialEq)]
pub enum VariableAlias {
    None,
    Function(FuncId),
    Struct(StructId),
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub id: VarId,
    pub name: String,
    pub type_id: TypeId,
    pub is_mutable: bool,
    pub is_initialized: bool,
    // Variables with no defined_span are builtins
    pub defined_span: Option<Range>,
    pub is_captured: bool,
    pub alias: VariableAlias,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FuncId(/* scope_id: */ pub ScopeId, /* idx: */ pub usize);

#[derive(Debug, PartialEq)]
pub struct Function {
    pub id: FuncId,
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub return_type_id: TypeId,
    // Functions with no defined_span are builtins
    pub defined_span: Option<Range>,
    pub body: Vec<TypedNode>,
    pub captured_vars: Vec<VarId>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionParam {
    pub name: String,
    pub type_id: TypeId,
    // Params with no defined_span are for builtin functions
    pub defined_span: Option<Range>,
    pub default_value: Option<TypedNode>,
}

#[derive(Debug, PartialEq)]
pub struct TypedModule {
    pub id: ModuleId,
    pub name: String,
    pub type_ids: Vec<TypeId>,
    // TODO: is this necessary?
    pub functions: Vec<FuncId>,
    pub structs: Vec<Struct>,
    pub code: Vec<TypedNode>,
    pub scopes: Vec<Scope>,
}

#[derive(Debug, PartialEq)]
pub enum TypedNode {
    // Expressions
    Literal { token: Token, value: TypedLiteral, type_id: TypeId },
    Unary { token: Token, op: UnaryOp, expr: Box<TypedNode> },
    Grouped { token: Token, expr: Box<TypedNode> },
    Array { token: Token, items: Vec<TypedNode>, type_id: TypeId },
    Tuple { token: Token, items: Vec<TypedNode>, type_id: TypeId },
    Set { token: Token, items: Vec<TypedNode>, type_id: TypeId },
    Map { token: Token, items: Vec<(TypedNode, TypedNode)>, type_id: TypeId },
    Identifier { token: Token, var_id: VarId, type_id: TypeId },
    Invocation { target: Box<TypedNode>, arguments: Vec<Option<TypedNode>>, type_id: TypeId },

    // Statements
    BindingDeclaration { token: Token, pattern: BindingPattern, vars: Vec<VarId>, expr: Option<Box<TypedNode>> },
}

impl TypedNode {
    pub fn type_id(&self) -> &TypeId {
        match self {
            // Expressions
            TypedNode::Literal { type_id, .. } => type_id,
            TypedNode::Unary { expr, .. } => expr.type_id(),
            TypedNode::Grouped { expr, .. } => expr.type_id(),
            TypedNode::Array { type_id, .. } => type_id,
            TypedNode::Tuple { type_id, .. } => type_id,
            TypedNode::Set { type_id, .. } => type_id,
            TypedNode::Map { type_id, .. } => type_id,
            TypedNode::Identifier { type_id, .. } => type_id,
            TypedNode::Invocation { type_id, .. } => type_id,

            // Statements
            TypedNode::BindingDeclaration { .. } => &PRELUDE_UNIT_TYPE_ID,
        }
    }

    fn span(&self) -> Range {
        match self {
            // Expressions
            TypedNode::Literal { token, .. } => token.get_range(),
            TypedNode::Unary { token, expr, .. } => token.get_range().expand(&expr.span()),
            TypedNode::Grouped { token, expr } => token.get_range().expand(&expr.span()),
            TypedNode::Array { token, items, .. } => token.get_range().expand(&items.last().map(|i| i.span()).unwrap_or(token.get_range())),
            TypedNode::Tuple { token, items, .. } => token.get_range().expand(&items.last().map(|i| i.span()).unwrap_or(token.get_range())),
            TypedNode::Set { token, items, .. } => token.get_range().expand(&items.last().map(|i| i.span()).unwrap_or(token.get_range())),
            TypedNode::Map { token, items, .. } => token.get_range().expand(&items.last().map(|(_, v)| v.span()).unwrap_or(token.get_range())),
            TypedNode::Identifier { token, .. } => token.get_range(),
            TypedNode::Invocation { target, arguments, .. } => {
                let start = target.span();
                let mut max = None;
                for arg in arguments {
                    if let Some(arg) = arg {
                        let arg_span = arg.span();
                        match &max {
                            None => max = Some(arg_span),
                            Some(max_span) => if arg_span.end.line > max_span.end.line && arg_span.end.col > max_span.end.col {
                                max = Some(arg_span)
                            }
                        }
                    }
                }

                if let Some(max) = &max {
                    start.expand(max)
                } else {
                    start
                }
            }

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

pub const PRELUDE_MODULE_ID: ModuleId = ModuleId(0);
pub const PRELUDE_SCOPE_ID: ScopeId = ScopeId(PRELUDE_MODULE_ID, 0);
pub const PRELUDE_UNIT_TYPE_ID: TypeId = TypeId(PRELUDE_SCOPE_ID, 0);
pub const PRELUDE_INT_TYPE_ID: TypeId = TypeId(PRELUDE_SCOPE_ID, 1);
pub const PRELUDE_FLOAT_TYPE_ID: TypeId = TypeId(PRELUDE_SCOPE_ID, 2);
pub const PRELUDE_BOOL_TYPE_ID: TypeId = TypeId(PRELUDE_SCOPE_ID, 3);
pub const PRELUDE_STRING_TYPE_ID: TypeId = TypeId(PRELUDE_SCOPE_ID, 4);

pub type TypecheckError = Either<Either<LexerError, ParseError>, TypeError>;

#[derive(Debug, PartialEq)]
pub enum DestructuringMismatchKind {
    CannotDestructureAsTuple,
    InvalidTupleArity(/* actual_arity: */ usize, /* attempted_arity: */ usize),
    CannotDestructureAsArray,
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    TypeMismatch { span: Range, expected: Vec<TypeId>, received: TypeId },
    UnknownType { span: Range, name: String },
    UnknownIdentifier { span: Range, token: Token },
    MissingBindingInitializer { span: Range, is_mutable: bool },
    DuplicateBinding { span: Range, name: String, original_span: Option<Range> },
    ForbiddenAssignment { span: Range, type_id: TypeId },
    DestructuringMismatch { span: Range, kind: DestructuringMismatchKind, type_id: TypeId },
    DuplicateSplat { span: Range },
    DuplicateParameter { span: Range, name: String },
    ReturnTypeMismatch { span: Range, func_name: String, expected: TypeId, received: TypeId },
    IllegalInvocation { span: Range, type_id: TypeId },
    UnexpectedArgumentName { span: Range, arg_name: String },
    MixedArgumentType { span: Range },
    DuplicateArgumentLabel { span: Range, name: String },
    InvalidArity { span: Range, num_required_args: usize, num_provided_args: usize },
    DuplicateMember { span: Range, name: String, kind: &'static str },
    InvalidSelfParam { span: Range },
    InvalidSelfParamPosition { span: Range },
}

impl TypeError {
    const INDENT_AMOUNT: usize = 2;

    fn get_underline(left_padding: usize, length: usize) -> String {
        format!("{}{}", " ".repeat(left_padding), "^".repeat(length))
    }

    fn indent() -> String {
        " ".repeat(Self::INDENT_AMOUNT)
    }

    fn get_underlined_line(lines: &Vec<&str>, span: &Range) -> String {
        // debug_assert!(span.start.line == span.end.line, "TODO: Displaying errors for multi-line spans");

        let line = lines.get(span.start.line - 1).expect("There should be a line");
        let length = span.end.col - span.start.col + 1;
        let underline = Self::get_underline(2 * Self::INDENT_AMOUNT + span.start.col, length);
        let indent = Self::indent();
        format!("{}|{}{}\n{}", indent, indent, line, underline)
    }

    pub fn message(&self, project: &Project, file_name: &String, source: &String) -> String {
        let span = match self {
            TypeError::TypeMismatch { span, .. } |
            TypeError::UnknownType { span, .. } |
            TypeError::UnknownIdentifier { span, .. } |
            TypeError::MissingBindingInitializer { span, .. } |
            TypeError::DuplicateBinding { span, .. } |
            TypeError::ForbiddenAssignment { span, .. } |
            TypeError::DestructuringMismatch { span, .. } |
            TypeError::DuplicateSplat { span } |
            TypeError::DuplicateParameter { span, .. } |
            TypeError::ReturnTypeMismatch { span, .. } |
            TypeError::IllegalInvocation { span, .. } |
            TypeError::UnexpectedArgumentName { span, .. } |
            TypeError::MixedArgumentType { span, .. } |
            TypeError::DuplicateArgumentLabel { span, .. } |
            TypeError::InvalidArity { span, .. } |
            TypeError::DuplicateMember { span, .. } |
            TypeError::InvalidSelfParam { span } |
            TypeError::InvalidSelfParamPosition { span } => span,
        };
        let lines: Vec<&str> = source.split("\n").collect();
        let cursor_line = Self::get_underlined_line(&lines, span);

        let msg = match self {
            TypeError::TypeMismatch { expected, received, .. } => {
                let multiple_expected = expected.len() > 1;
                let expected = expected.iter().map(|type_id| project.type_repr(type_id)).join(", ");
                let received = project.type_repr(received);

                format!(
                    "Type mismatch\n{}\n\
                    Expected{}{}\n\
                    but instead saw: {}",
                    cursor_line,
                    if multiple_expected { " one of: " } else { ": " }, expected,
                    received
                )
            }
            TypeError::UnknownType { name, .. } => {
                format!(
                    "Unknown type '{}'\n{}\n\
                    No type with that name is visible in current scope",
                    name, cursor_line
                )
            }
            TypeError::UnknownIdentifier { token, .. } => {
                let ident = Token::get_ident_name(token);
                if &ident == "_" {
                    format!(
                        "Unknown identifier '{}'\n{}\n\
                        The _ represents an anonymous identifier; please give the variable a name if you want to reference it",
                        ident, cursor_line
                    )
                } else {
                    format!(
                        "Unknown identifier '{}'\n{}\n\
                        No variable with that name is visible in current scope",
                        ident, cursor_line
                    )
                }
            }
            TypeError::MissingBindingInitializer { is_mutable, .. } => {
                let msg = if *is_mutable {
                    "Since 'var' was used, you can provide an initial value or a type annotation"
                } else {
                    "Since 'val' was used, you must provide an initial value"
                };

                format!(
                    "Could not determine type of {} variable\n{}\n{}",
                    if *is_mutable { "mutable" } else { "immutable" },
                    cursor_line, msg
                )
            }
            TypeError::DuplicateBinding { name, original_span, .. } => {
                let first_msg = format!("Duplicate name '{}'\n{}", &name, cursor_line);

                let second_msg = if let Some(original_span) = original_span {
                    let pos = &original_span.start;
                    let cursor_line = Self::get_underlined_line(&lines, original_span);
                    format!("Already declared in scope at ({}:{})\n{}", pos.line, pos.col, cursor_line)
                } else {
                    "Already declared as built-in value".to_string()
                };

                format!("{}\n{}", first_msg, second_msg)
            }
            TypeError::ForbiddenAssignment { type_id, .. } => {
                if *type_id == PRELUDE_UNIT_TYPE_ID {
                    format!(
                        "Forbidden type for variable\n{}\n\
                        Variables cannot be of type {}",
                        cursor_line, project.type_repr(&PRELUDE_UNIT_TYPE_ID)
                    )
                } else {
                    format!(
                        "Could not determine type\n{}\n\
                        Please use an explicit type annotation to denote the type",
                        cursor_line
                    )
                }
            }
            TypeError::DestructuringMismatch { kind, type_id, .. } => {
                let msg = match kind {
                    DestructuringMismatchKind::CannotDestructureAsTuple => {
                        format!("Cannot destructure a value of type {} as a tuple", project.type_repr(type_id))
                    }
                    DestructuringMismatchKind::InvalidTupleArity(actual_arity, attempted_arity) => {
                        format!("Cannot destructure a tuple of {} elements into {} values", actual_arity, attempted_arity)
                    }
                    DestructuringMismatchKind::CannotDestructureAsArray => {
                        format!("Cannot destructure a value of type {} as an array", project.type_repr(type_id))
                    }
                };

                format!(
                    "Invalid destructuring pattern for assignment\n{}\n{}",
                    cursor_line, msg
                )
            }
            TypeError::DuplicateSplat { .. } => {
                format!(
                    "Invalid destructuring pattern for assignment\n{}\n\
                    Cannot have more than one splat (*) instance in an array destructuring",
                    cursor_line
                )
            }
            TypeError::DuplicateParameter { name, .. } => {
                format!("Duplicate parameter '{}'\n{}", &name, cursor_line)
            }
            TypeError::ReturnTypeMismatch { expected, received, func_name, .. } => {
                let expected_repr = project.type_repr(expected);
                let received_repr = project.type_repr(received);

                format!(
                    "Return type mismatch for function '{}'\n{}\n\
                    Expected: {}\n\
                    but instead saw: {}",
                    func_name, cursor_line,
                    expected_repr,
                    received_repr,
                )
            }
            TypeError::IllegalInvocation { type_id, .. } => {
                format!(
                    "Cannot invoke target as function\n{}\n\
                    Type {} is not callable",
                    cursor_line, project.type_repr(type_id)
                )
            }

            TypeError::UnexpectedArgumentName { arg_name, .. } => {
                format!(
                    "Unexpected argument label '{}'\n{}\n\
                    This function doesn't have a parameter called '{}'",
                    arg_name, cursor_line, arg_name,
                )
            }
            TypeError::MixedArgumentType { .. } => {
                format!(
                    "Invalid function call\n{}\n\
                    Cannot mix named and positional arguments.",
                    cursor_line
                )
            }
            TypeError::DuplicateArgumentLabel { name, .. } => {
                format!(
                    "Duplicate parameter name '{}'\n{}\n\
                    A value has already been passed for this parameter",
                    name, cursor_line,
                )
            }
            TypeError::InvalidArity { num_required_args, num_provided_args, .. } => {
                format!(
                    "Incorrect arity for invocation\n{}\n\
                    Expected {} required argument{}, but {} {} passed",
                    cursor_line,
                    num_required_args, if *num_required_args == 1 { "" } else { "s" },
                    num_provided_args, if *num_provided_args == 1 { "was" } else { "were" },
                )
            }
            TypeError::DuplicateMember { name, kind, .. } => {
                format!(
                    "Duplicate field '{}'\n{}\n\
                    {} with that name is already declared in this type",
                    name, cursor_line,
                    kind
                )
            }
            TypeError::InvalidSelfParam { .. } => {
                format!(
                    "Invalid usage of `self` parameter\n{}\n\
                    `self` can only appear within methods on types",
                    cursor_line
                )
            }
            TypeError::InvalidSelfParamPosition { .. } => {
                format!(
                    "Invalid position for `self`\n{}\n\
                    `self` must appear as the first parameter",
                    cursor_line
                )
            }
        };

        let error_line = format!("Error at {}:{}:{}", file_name, span.start.line, span.start.col);
        format!("{}\n{}", error_line, msg)
    }
}

pub struct Typechecker2<'a, L: LoadModule> {
    module_loader: &'a L,
    project: &'a mut Project,
    current_scope: usize,
    current_type_decl: Option<TypeId>,
    current_function: Option<FuncId>,
}

impl<'a, L: LoadModule> Typechecker2<'a, L> {
    pub fn new(module_loader: &'a L, project: &'a mut Project) -> Typechecker2<'a, L> {
        Typechecker2 { module_loader, project, current_scope: 0, current_type_decl: None, current_function: None }
    }

    /* UTILITIES */

    fn current_module_mut(&mut self) -> &mut TypedModule {
        self.project.modules.last_mut().expect("Internal error: there must always be a module being typechecked")
    }

    fn current_module(&self) -> &TypedModule {
        self.project.modules.last().expect("Internal error: there must always be a module being typechecked")
    }

    fn add_or_find_type_id(&mut self, ty: Type) -> TypeId {
        let current_module_id = self.current_module_mut().id;
        let current_scope_id = ScopeId(current_module_id, self.current_scope);
        self.project.add_or_find_type_id(&current_scope_id, ty)
    }

    fn get_struct_by_name(&self, name: &String) -> Option<&Struct> {
        let current_module_id = self.current_module().id;
        self.project.find_struct_by_name(&current_module_id, name)
    }

    fn type_satisfies_other(&self, base_type: &TypeId, target_type: &TypeId) -> bool {
        let base_ty = self.project.get_type_by_id(base_type);
        let target_ty = self.project.get_type_by_id(target_type);

        match (base_ty, target_ty) {
            (Type::Generic(_), Type::Generic(_)) => base_type == target_type,
            (_, Type::Generic(_)) => unreachable!("Test: we shouldn't reach here because before any attempt to test types, we should substitute generics. See if this assumption is true (there will surely be a counterexample someday)"),
            (Type::Builtin(idx1), Type::Builtin(idx2)) => idx1 == idx2,
            (Type::GenericInstance(struct_id_1, generic_ids_1), Type::GenericInstance(struct_id_2, generic_ids_2)) => {
                if struct_id_1 != struct_id_2 || generic_ids_1.len() != generic_ids_2.len() {
                    return false;
                }
                for (generic_type_id_1, generic_type_id_2) in generic_ids_1.iter().zip(generic_ids_2.iter()) {
                    if !self.type_satisfies_other(generic_type_id_1, generic_type_id_2) {
                        return false;
                    }
                }

                true
            }
            (Type::GenericInstance(struct_id, _), _) if *struct_id == self.project.prelude_option_struct_id => {
                false
            }
            (_, Type::GenericInstance(struct_id, generic_ids)) if *struct_id == self.project.prelude_option_struct_id => {
                self.type_satisfies_other(base_type, &generic_ids[0])
            }
            _ => false
        }
    }

    fn substitute_generics(&mut self, hint_type_id: &TypeId, var_type_id: &TypeId) -> TypeId {
        let hint_ty = self.project.get_type_by_id(&hint_type_id);
        let var_ty = self.project.get_type_by_id(&var_type_id);

        match (hint_ty, var_ty) {
            (Type::Generic(_), _) => unreachable!("The hint should always be a concrete (non-generic) type"),
            (_, Type::Builtin(_)) => *var_type_id,
            (_, Type::Generic(_)) => *hint_type_id,
            (_, Type::GenericInstance(var_struct_id, var_generic_ids)) if *var_struct_id == self.project.prelude_option_struct_id => {
                let generic_id = var_generic_ids[0];
                let inner = self.substitute_generics(&hint_type_id, &generic_id);
                self.add_or_find_type_id(self.project.option_type(inner))
            }
            (Type::GenericInstance(hint_struct_id, hint_generic_ids), Type::GenericInstance(var_struct_id, var_generic_ids)) => {
                if var_struct_id == hint_struct_id && hint_generic_ids.len() == var_generic_ids.len() {
                    let hint_generic_ids = hint_generic_ids.clone();
                    let var_generic_ids = var_generic_ids.clone();
                    let var_struct_id = *var_struct_id;

                    let mut new_var_generic_ids = vec![];
                    for (hint_generic_type_id, var_generic_type_id) in hint_generic_ids.iter().zip(var_generic_ids.iter()) {
                        new_var_generic_ids.push(self.substitute_generics(hint_generic_type_id, var_generic_type_id));
                    }
                    self.add_or_find_type_id(Type::GenericInstance(var_struct_id, new_var_generic_ids))
                } else {
                    *var_type_id
                }
            }
            _ => *var_type_id
        }
    }

    fn type_contains_generics(&self, type_id: &TypeId) -> bool {
        let ty = self.project.get_type_by_id(type_id);
        match ty {
            Type::Builtin(_) => false,
            Type::Generic(_) => true,
            Type::GenericInstance(_, generic_ids) => {
                for type_id in generic_ids {
                    if self.type_contains_generics(type_id) {
                        return true;
                    }
                }

                false
            }
            Type::Function(param_type_ids, return_type_id) => {
                if self.type_contains_generics(return_type_id) {
                    return true;
                }

                for type_id in param_type_ids {
                    if self.type_contains_generics(type_id) {
                        return true;
                    }
                }

                false
            }
            Type::Struct(struct_id) => {
                let struct_ = self.project.get_struct_by_id(struct_id);
                struct_.generics.is_some()
            }
        }
    }

    fn resolve_type_identifier(&mut self, type_identifier: &TypeIdentifier) -> Result<TypeId, TypeError> {
        match type_identifier {
            TypeIdentifier::Normal { ident, type_args } => {
                let ident_name = Token::get_ident_name(ident);
                match ident_name.as_str() {
                    "Unit" => Ok(PRELUDE_UNIT_TYPE_ID),
                    "Int" => Ok(PRELUDE_INT_TYPE_ID),
                    "Float" => Ok(PRELUDE_FLOAT_TYPE_ID),
                    "Bool" => Ok(PRELUDE_BOOL_TYPE_ID),
                    "String" => Ok(PRELUDE_STRING_TYPE_ID),
                    _ => {
                        let struct_id = self.get_struct_by_name(&ident_name)
                            .ok_or_else(|| TypeError::UnknownType { span: ident.get_range(), name: ident_name })?
                            .id;

                        let mut generic_ids = vec![];
                        if let Some(type_args) = type_args {
                            for type_arg_identifier in type_args {
                                let type_id = self.resolve_type_identifier(type_arg_identifier)?;
                                generic_ids.push(type_id);
                            }
                        }

                        Ok(self.add_or_find_type_id(Type::GenericInstance(struct_id, generic_ids)))
                    }
                }
            }
            TypeIdentifier::Array { inner } => {
                let inner = self.resolve_type_identifier(&*inner)?;
                let ty = self.project.array_type(inner);
                Ok(self.add_or_find_type_id(ty))
            }
            TypeIdentifier::Tuple { types } => {
                let mut inners = vec![];
                for type_identifier in types {
                    inners.push(self.resolve_type_identifier(type_identifier)?);
                }
                let ty = self.project.tuple_type(inners);
                Ok(self.add_or_find_type_id(ty))
            }
            TypeIdentifier::Option { inner } => {
                let inner = self.resolve_type_identifier(&*inner)?;
                let ty = self.project.option_type(inner);
                Ok(self.add_or_find_type_id(ty))
            }
            TypeIdentifier::Union { .. } |
            TypeIdentifier::Func { .. } => todo!()
        }
    }

    fn add_variable_to_current_scope(&mut self, name: String, type_id: TypeId, is_mutable: bool, is_initialized: bool, span: &Range) -> Result<VarId, TypeError> {
        let current_scope_idx = self.current_scope;
        let current_scope = &mut self.current_module_mut().scopes[current_scope_idx];

        for var in &current_scope.vars {
            if var.name == name {
                return Err(TypeError::DuplicateBinding { span: span.clone(), name, original_span: var.defined_span.clone() });
            }
        }

        let id = VarId(current_scope.id, current_scope.vars.len());
        let var = Variable { id, name, type_id, is_mutable, is_initialized, defined_span: Some(span.clone()), is_captured: false, alias: VariableAlias::None };
        current_scope.vars.push(var);

        Ok(id)
    }

    fn add_function_variable_alias_to_current_scope(&mut self, ident: &Token, func_id: &FuncId) -> Result<(), TypeError> {
        let func = self.project.get_func_by_id(func_id);
        let name = Token::get_ident_name(ident);
        let span = ident.get_range();

        let param_type_ids = func.params.iter().map(|p| p.type_id).collect();

        let fn_type_id = self.add_or_find_type_id(self.project.function_type(param_type_ids, func.return_type_id));
        let fn_var_id = self.add_variable_to_current_scope(name, fn_type_id, false, true, &span)?;
        let variable = self.project.get_var_by_id_mut(&fn_var_id);
        variable.alias = VariableAlias::Function(*func_id);

        Ok(())
    }

    fn add_function_to_current_scope(&mut self, name_token: &Token, params: Vec<FunctionParam>, return_type_id: TypeId) -> Result<FuncId, TypeError> {
        let current_scope_idx = self.current_scope;
        let current_scope = &mut self.current_module_mut().scopes[current_scope_idx];

        let name = Token::get_ident_name(name_token);
        let span = name_token.get_range();
        for func in &current_scope.funcs {
            if func.name == name {
                return Err(TypeError::DuplicateBinding { span, name, original_span: func.defined_span.clone() });
            }
        }

        let func_id = FuncId(current_scope.id, current_scope.funcs.len());
        let func = Function { id: func_id, name: name.clone(), params, return_type_id, defined_span: Some(span.clone()), body: vec![], captured_vars: vec![] };
        current_scope.funcs.push(func);

        Ok(func_id)
    }

    fn add_struct_to_current_module(&mut self, name_token: &Token) -> Result<StructId, TypeError> {
        let current_module = self.current_module_mut();

        let name = Token::get_ident_name(name_token);
        let span = name_token.get_range();
        for struct_ in &current_module.structs {
            if struct_.name == name {
                return Err(TypeError::DuplicateBinding { span, name, original_span: struct_.defined_span.clone() });
            }
        }

        let struct_id = StructId(current_module.id, current_module.structs.len());
        let struct_ = Struct {
            id: struct_id,
            name: name.clone(),
            defined_span: Some(name_token.get_range()),
            generics: None,
            fields: vec![],
            methods: vec![],
            static_methods: vec![],
        };
        current_module.structs.push(struct_);

        let struct_type_id = self.add_or_find_type_id(self.project.struct_type(struct_id));
        let struct_var_id = self.add_variable_to_current_scope(name, struct_type_id, false, true, &span)?;
        let variable = self.project.get_var_by_id_mut(&struct_var_id);
        variable.alias = VariableAlias::Struct(struct_id);

        Ok(struct_id)
    }

    fn begin_child_scope<S: AsRef<str>>(&mut self, label: S) -> ScopeId {
        let current_scope_id = self.current_scope;
        let current_module = self.current_module_mut();
        let current_scope = &current_module.scopes[current_scope_id];
        let new_scope_id = ScopeId(current_module.id, current_module.scopes.len());

        let child_scope = Scope {
            label: label.as_ref().to_string(),
            id: new_scope_id,
            parent: Some(current_scope.id),
            types: vec![],
            vars: vec![],
            funcs: vec![],
        };
        current_module.scopes.push(child_scope);

        self.current_scope = new_scope_id.1;

        new_scope_id
    }

    fn end_child_scope(&mut self) -> ScopeId {
        let current_scope_id = self.current_scope;
        let current_module = self.current_module_mut();
        let current_scope = &current_module.scopes[current_scope_id];

        let Some(parent) = current_scope.parent else {
            unreachable!("Internal error: a child scope must always have a parent");
        };
        let ScopeId(_, parent_scope_idx) = parent;
        self.current_scope = parent_scope_idx;

        parent
    }

    fn scope_contains_other(&self, inner: &ScopeId, outer: &ScopeId) -> bool {
        let ScopeId(inner_scope_module_id, inner_scope_idx) = inner;
        let inner_scope = &self.project.modules[inner_scope_module_id.0].scopes[*inner_scope_idx];
        let mut parent = inner_scope.parent;
        while let Some(parent_id) = parent {
            if parent_id == *outer {
                return true;
            }

            let ScopeId(ModuleId(parent_scope_module_idx), parent_scope_idx) = parent_id;
            let parent_scope = &self.project.modules[parent_scope_module_idx].scopes[parent_scope_idx];
            parent = parent_scope.parent;
        }

        false
    }

    /// Unifying types. For input type `input_type_id`, and target/hint type `hint_type_id`:
    /// 1) Substitute any generics in `input_type_id` using type data from `hint_type_id` (eg. `{ a: [], b: [1] }`).
    /// 2) If `hint_type_id` is assignable to `input_type_id` (eg. they're both `Int`), then continue on, returning
    ///    the `input_type_id` with all possible generic substitutions performed.
    /// 3) Otherwise, attempt to expand `input_type_id` to include `hint_type_id`. If `input_type_id` can be
    ///    assignable to `hint_type_id`, then `hint_type_id` is a broader type, and `input_type_id` becomes
    ///    `hint_type_id` (eg. `{ a: 123, b: None }`). If not, then the types do not overlap and `None` is returned.
    fn unify_types(&mut self, input_type_id: &TypeId, hint_type_id: &TypeId) -> Option<TypeId> {
        let mut input_type_id = *input_type_id;

        if self.type_contains_generics(&input_type_id) {
            input_type_id = self.substitute_generics(hint_type_id, &input_type_id);
        }

        if !self.type_satisfies_other(&hint_type_id, &input_type_id) {
            if self.type_satisfies_other(&input_type_id, &hint_type_id) {
                input_type_id = *hint_type_id;
            } else {
                return None;
            }
        }

        Some(input_type_id)
    }

    /* TYPECHECKING */

    pub fn typecheck_prelude(&mut self) {
        debug_assert!(self.project.modules.is_empty());

        let mut prelude_module = TypedModule { id: PRELUDE_MODULE_ID, name: "prelude".to_string(), type_ids: vec![], functions: vec![], structs: vec![], code: vec![], scopes: vec![] };

        // TODO: Add println, print, and range
        prelude_module.scopes.push(Scope { label: "prelude.root".to_string(), id: PRELUDE_SCOPE_ID, parent: None, types: vec![], vars: vec![], funcs: vec![] });
        let prelude_scope = &mut prelude_module.scopes[PRELUDE_SCOPE_ID.1];

        for type_id in [PRELUDE_UNIT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_BOOL_TYPE_ID, PRELUDE_STRING_TYPE_ID] {
            prelude_scope.types.push(Type::Builtin(type_id.1));
            prelude_module.type_ids.push(type_id);
        }

        self.project.modules.push(prelude_module);

        let mut prelude_scope_idx = 1;

        // TODO: Read this in from some prototype file (eg. `prelude.d.abra`), which contains stubs
        // Define `Option<T>` struct
        {
            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let scope_id = ScopeId(PRELUDE_MODULE_ID, prelude_scope_idx);
            prelude_scope_idx += 1;
            let scope = Scope { label: "prelude.Option".to_string(), id: scope_id, parent: Some(PRELUDE_SCOPE_ID), types: vec![], vars: vec![], funcs: vec![] };
            prelude_module.scopes.push(scope);
            let generic_t_type_id = self.project.add_type_id(&scope_id, Type::Generic("T".to_string()));

            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let option_struct_id = StructId(PRELUDE_MODULE_ID, prelude_module.structs.len());
            prelude_module.structs.push(Struct { id: option_struct_id, name: "Option".to_string(), defined_span: None, generics: Some(vec![generic_t_type_id]), fields: vec![], methods: vec![], static_methods: vec![] });
            self.project.prelude_option_struct_id = option_struct_id;
        }
        // Define `Array<T>` struct
        {
            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let scope_id = ScopeId(PRELUDE_MODULE_ID, prelude_scope_idx);
            prelude_scope_idx += 1;
            let scope = Scope { label: "prelude.Array".to_string(), id: scope_id, parent: Some(PRELUDE_SCOPE_ID), types: vec![], vars: vec![], funcs: vec![] };
            prelude_module.scopes.push(scope);
            let generic_t_type_id = self.project.add_type_id(&scope_id, Type::Generic("T".to_string()));

            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let array_struct_id = StructId(PRELUDE_MODULE_ID, prelude_module.structs.len());
            prelude_module.structs.push(Struct { id: array_struct_id, name: "Array".to_string(), defined_span: None, generics: Some(vec![generic_t_type_id]), fields: vec![], methods: vec![], static_methods: vec![] });
            self.project.prelude_array_struct_id = array_struct_id;
        }
        // Define `Tuple<T...>` struct
        // (There's no way of representing variadic generics like how tuple works, and it's not something that's necessary to add, so let's do a bit of hand-waving here)
        {
            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let tuple_struct_id = StructId(PRELUDE_MODULE_ID, prelude_module.structs.len());
            prelude_module.structs.push(Struct { id: tuple_struct_id, name: "Tuple".to_string(), defined_span: None, generics: None, fields: vec![], methods: vec![], static_methods: vec![] });
            self.project.prelude_tuple_struct_id = tuple_struct_id;
        }
        // Define `Set<T>` struct
        {
            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let scope_id = ScopeId(PRELUDE_MODULE_ID, prelude_scope_idx);
            prelude_scope_idx += 1;
            let scope = Scope { label: "prelude.Set".to_string(), id: scope_id, parent: Some(PRELUDE_SCOPE_ID), types: vec![], vars: vec![], funcs: vec![] };
            prelude_module.scopes.push(scope);
            let generic_t_type_id = self.project.add_type_id(&scope_id, Type::Generic("T".to_string()));

            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let set_struct_id = StructId(PRELUDE_MODULE_ID, prelude_module.structs.len());
            prelude_module.structs.push(Struct { id: set_struct_id, name: "Set".to_string(), defined_span: None, generics: Some(vec![generic_t_type_id]), fields: vec![], methods: vec![], static_methods: vec![] });
            self.project.prelude_set_struct_id = set_struct_id;
        }
        // Define `Map<K, V>` struct
        {
            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let scope_id = ScopeId(PRELUDE_MODULE_ID, prelude_scope_idx);
            prelude_scope_idx += 1;
            let scope = Scope { label: "prelude.Map".to_string(), id: scope_id, parent: Some(PRELUDE_SCOPE_ID), types: vec![], vars: vec![], funcs: vec![] };
            prelude_module.scopes.push(scope);
            let generic_k_type_id = self.project.add_type_id(&scope_id, Type::Generic("K".to_string()));
            let generic_v_type_id = self.project.add_type_id(&scope_id, Type::Generic("V".to_string()));

            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let map_struct_id = StructId(PRELUDE_MODULE_ID, prelude_module.structs.len());
            prelude_module.structs.push(Struct { id: map_struct_id, name: "Map".to_string(), defined_span: None, generics: Some(vec![generic_k_type_id, generic_v_type_id]), fields: vec![], methods: vec![], static_methods: vec![] });
            self.project.prelude_map_struct_id = map_struct_id;
        }
        // Define `None` builtin, which is of type `T?`
        {
            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let scope_id = ScopeId(PRELUDE_MODULE_ID, prelude_scope_idx);
            let scope = Scope { label: "prelude.None".to_string(), id: scope_id, parent: Some(PRELUDE_SCOPE_ID), types: vec![], vars: vec![], funcs: vec![] };
            prelude_module.scopes.push(scope);
            let generic_t_type_id = self.project.add_type_id(&scope_id, Type::Generic("T".to_string()));

            let none_type_id = self.project.add_type_id(&PRELUDE_SCOPE_ID, self.project.option_type(generic_t_type_id));
            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            prelude_module.scopes[PRELUDE_SCOPE_ID.1].vars.push(
                Variable {
                    id: VarId(ScopeId(PRELUDE_MODULE_ID, 0), 0),
                    name: "None".to_string(),
                    type_id: none_type_id,
                    is_mutable: false,
                    is_initialized: true,
                    defined_span: None,
                    is_captured: false,
                    alias: VariableAlias::None,
                }
            );
        }
    }

    pub fn typecheck_module(&mut self, m_id: &parser::ast::ModuleId) -> Result<(), TypecheckError> {
        debug_assert!(self.project.modules.len() == 1, "Prelude must be loaded in order to typecheck further modules");

        let (file_name, parse_result) = self.module_loader.load_untyped_ast(&m_id).map_err(Either::Left)?;

        if !parse_result.imports.is_empty() {
            unimplemented!("Typechecking imports");
        }

        let prelude_scope_id = ScopeId(PRELUDE_MODULE_ID, 0);

        let module_id = ModuleId(self.project.modules.len());
        let scope_id = ScopeId(module_id, 0);
        let label = format!("{:?}.root", &module_id);
        let root_scope = Scope { label, id: scope_id, parent: Some(prelude_scope_id), types: vec![], vars: vec![], funcs: vec![] };
        self.project.modules.push(TypedModule {
            id: module_id,
            name: file_name,
            type_ids: vec![],
            functions: vec![],
            structs: vec![],
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

        for node in &nodes {
            match node {
                AstNode::FunctionDecl(_, node) => func_decls.push(node),
                AstNode::TypeDecl(_, node) => type_decls.push(node),
                n @ AstNode::EnumDecl(_, _) => enum_decls.push(n),
                _ => {}
            }
        }

        let mut func_ids = VecDeque::with_capacity(func_decls.len());
        for node in func_decls {
            let func_id = self.typecheck_function_pass_1(node, false)?;
            self.add_function_variable_alias_to_current_scope(&node.name, &func_id)?;
            func_ids.push_back(func_id);
        }

        let mut structs = VecDeque::with_capacity(type_decls.len());
        for node in type_decls {
            let struct_data = self.typecheck_struct_pass_1(node)?;
            structs.push_back(struct_data);
        }

        debug_assert!(enum_decls.is_empty());

        for node in nodes {
            match node {
                AstNode::FunctionDecl(_, decl_node) => {
                    let func_id = func_ids.pop_front()
                        .expect("There should be a FuncId for each function declaration");
                    self.typecheck_function_pass_2(func_id, decl_node)?;
                }
                AstNode::TypeDecl(_, decl_node) => {
                    let (struct_id, method_func_ids) = structs.pop_front()
                        .expect("There should be Struct metadata for each declaration in this block");
                    self.typecheck_struct_pass_2(struct_id, method_func_ids, decl_node)?;
                }
                node => {
                    let typed_node = self.typecheck_statement(node, None)?;

                    let current_module = self.current_module_mut();
                    current_module.code.push(typed_node);
                }
            }
        }

        Ok(())
    }

    fn typecheck_function_pass_1(&mut self, node: &FunctionDeclNode, allow_self: bool) -> Result<FuncId, TypeError> {
        let FunctionDeclNode { export_token, name, type_args, args, ret_type, .. } = node;

        if export_token.is_some() { unimplemented!("Internal error: imports/exports") }
        if !type_args.is_empty() { unimplemented!("Internal error: function type arguments") }

        let mut seen_self = false;
        let mut seen_arg_names = HashSet::new();
        let mut params = vec![];
        for (idx, (ident, type_ident, is_vararg, default_value)) in args.iter().enumerate() {
            if *is_vararg { unimplemented!("Internal error: variadic parameters") }

            let arg_name = Token::get_ident_name(ident);

            if let Token::Self_(_) = &ident {
                let self_type_id = match &self.current_type_decl {
                    Some(type_id) if allow_self => type_id,
                    _ => return Err(TypeError::InvalidSelfParam { span: ident.get_range() }),
                };

                if seen_self || idx != 0 {
                    return Err(TypeError::InvalidSelfParamPosition { span: ident.get_range() });
                }
                seen_self = true;

                params.push(FunctionParam {
                    name: arg_name,
                    type_id: *self_type_id,
                    defined_span: Some(ident.get_range()),
                    default_value: None,
                });

                continue;
            }

            if seen_arg_names.contains(&arg_name) {
                return Err(TypeError::DuplicateParameter { span: ident.get_range(), name: arg_name });
            }
            seen_arg_names.insert(arg_name.clone());

            let mut param_type_id = None;
            if let Some(type_ident) = type_ident {
                param_type_id = Some(self.resolve_type_identifier(type_ident)?);
            }
            let typed_default_value_expr = if let Some(default_value) = default_value {
                // TODO: Handling retries for first-pass function typechecking with default-value expressions which reference code that hasn't been visited yet
                // For example:
                //   func foo(bar = baz()) = ...
                //   func baz() = ...
                // This would fail because `baz` doesn't yet exist when typechecking `foo`.
                Some(self.typecheck_expression(default_value.clone(), param_type_id)?)
            } else { None };
            let type_id = match (param_type_id, &typed_default_value_expr) {
                (None, None) => unreachable!("Internal error: should have failed to parse"),
                (Some(param_type_id), None) => param_type_id,
                (None, Some(typed_default_value_expr)) => *typed_default_value_expr.type_id(),
                (Some(param_type_id), Some(typed_default_value_expr)) => {
                    let default_value_type_id = typed_default_value_expr.type_id();
                    if !self.type_satisfies_other(default_value_type_id, &param_type_id) {
                        return Err(TypeError::TypeMismatch {
                            span: typed_default_value_expr.span(),
                            expected: vec![param_type_id],
                            received: *default_value_type_id,
                        });
                    }
                    param_type_id
                }
            };

            params.push(FunctionParam {
                name: arg_name,
                type_id,
                defined_span: Some(ident.get_range()),
                default_value: typed_default_value_expr,
            });
        }

        let mut return_type_id = PRELUDE_UNIT_TYPE_ID;
        if let Some(ret_type) = ret_type {
            return_type_id = self.resolve_type_identifier(ret_type)?;
        }
        let func_id = self.add_function_to_current_scope(name, params, return_type_id)?;
        self.current_module_mut().functions.push(func_id);

        Ok(func_id)
    }

    fn typecheck_function_pass_2(&mut self, func_id: FuncId, node: FunctionDeclNode) -> Result<(), TypeError> {
        let orig_func_id = self.current_function;
        self.current_function = Some(func_id);

        self.begin_child_scope(format!("{:?}.{}", &self.current_module().id, Token::get_ident_name(&node.name)));

        let func = self.project.get_func_by_id(&func_id);
        let func_name = func.name.clone();
        let return_type_id = func.return_type_id;
        // Why: rust cannot guarantee that `add_variable_to_current_scope` won't modify `func`; intermediate variable solves
        let params = func.params.iter().map(|p| (p.name.clone(), p.type_id, p.defined_span.clone())).collect_vec();
        for (name, type_id, defined_span, ..) in params {
            let Some(defined_span) = defined_span else { unreachable!("Internal error: when typechecking a user-defined function, parameters' spans will always be known"); };
            let defined_span = defined_span.clone();

            self.add_variable_to_current_scope(name, type_id, false, true, &defined_span)?;
        }

        let mut body = vec![];
        let num_nodes = node.body.len();
        for (idx, node) in node.body.into_iter().enumerate() {
            let is_last = idx == num_nodes - 1;
            let type_hint = if is_last {
                Some(return_type_id)
            } else {
                None
            };

            // TODO: Handle nested function declaration (and raise error on nested Type declaration)
            let typed_node = self.typecheck_statement(node, type_hint)?;
            let type_id = typed_node.type_id();

            if (is_last && return_type_id != PRELUDE_UNIT_TYPE_ID) && !self.type_satisfies_other(type_id, &return_type_id) {
                return Err(TypeError::ReturnTypeMismatch { span: typed_node.span(), func_name, expected: return_type_id, received: *type_id });
            }

            body.push(typed_node);
        }

        let func = self.project.get_func_by_id_mut(&func_id);
        func.body = body;

        self.end_child_scope();
        self.current_function = orig_func_id;

        Ok(())
    }

    fn typecheck_struct_pass_1(&mut self, node: &TypeDeclNode) -> Result<(StructId, Vec<FuncId>), TypeError> {
        let TypeDeclNode { export_token, name, type_args, methods, .. } = node;

        if export_token.is_some() { unimplemented!("Internal error: imports/exports") }
        if !type_args.is_empty() { unimplemented!("Internal error: function type arguments") }

        let struct_id = self.add_struct_to_current_module(name)?;
        debug_assert!(self.current_type_decl.is_none(), "At the moment, types cannot be nested within other types");

        let type_id = self.add_or_find_type_id(Type::GenericInstance(struct_id, vec![])); // TODO: Fix when generics are implemented
        self.current_type_decl = Some(type_id);

        self.begin_child_scope(format!("{:?}.{}", &self.current_module().id, Token::get_ident_name(&name)));

        let mut method_func_ids = vec![];
        for method in methods {
            let AstNode::FunctionDecl(_, decl_node) = method else { unreachable!("Internal error: a type's methods must be of type AstNode::FunctionDecl") };
            let func_id = self.typecheck_function_pass_1(decl_node, true)?;
            method_func_ids.push(func_id);
        }

        self.current_type_decl = None;

        self.end_child_scope();

        Ok((struct_id, method_func_ids))
    }

    fn typecheck_struct_pass_2(&mut self, struct_id: StructId, method_func_ids: Vec<FuncId>, node: TypeDeclNode) -> Result<(), TypeError> {
        let TypeDeclNode { fields, methods, .. } = node;

        let mut seen_fields = HashSet::new();
        let mut typed_fields = Vec::with_capacity(fields.len());
        for TypeDeclField { ident, type_ident, default_value, readonly } in fields {
            if default_value.is_some() { unimplemented!("Internal error: field default values") }
            if readonly.is_some() { unimplemented!("Internal error: readonly") }

            let field_name = Token::get_ident_name(&ident);
            if seen_fields.contains(&field_name) {
                return Err(TypeError::DuplicateMember { span: ident.get_range(), name: field_name, kind: "Field" });
            }
            seen_fields.insert(field_name.clone());

            let field_type_id = self.resolve_type_identifier(&type_ident)?;
            let field = StructField {
                name: field_name,
                type_id: field_type_id,
            };
            typed_fields.push(field);
        }

        let mut instance_method_func_ids = vec![];
        let mut static_method_func_ids = vec![];
        debug_assert!(methods.len() == method_func_ids.len(), "There should be a FuncId for each method (by pass 1)");
        for (func_id, method) in method_func_ids.iter().zip(methods.into_iter()) {
            let AstNode::FunctionDecl(_, decl_node) = method else { unreachable!("Internal error: a type's methods must be of type AstNode::FunctionDecl") };
            let is_method = decl_node.args.get(0).map(|(token, _, _, _)| if let Token::Self_(_) = token { true } else { false }).unwrap_or(false);

            self.typecheck_function_pass_2(*func_id, decl_node)?;

            if is_method {
                instance_method_func_ids.push(*func_id);
            } else {
                static_method_func_ids.push(*func_id);
            }
        }

        let struct_ = self.project.get_struct_by_id_mut(&struct_id);
        struct_.fields = typed_fields;
        struct_.methods = instance_method_func_ids;
        struct_.static_methods = static_method_func_ids;

        Ok(())
    }

    fn typecheck_statement(&mut self, node: AstNode, type_hint: Option<TypeId>) -> Result<TypedNode, TypeError> {
        match node {
            AstNode::BindingDecl(token, n) => {
                let BindingDeclNode { export_token, binding, type_ann, expr, is_mutable } = n;
                if export_token.is_some() { unimplemented!("Internal error: imports/exports") }

                let type_hint_id = if let Some(type_identifier) = type_ann {
                    Some(self.resolve_type_identifier(&type_identifier)?)
                } else { None };

                let mut var_ids = vec![];
                let typed_expr = match (type_hint_id, expr) {
                    (None, None) => return Err(TypeError::MissingBindingInitializer { span: binding.get_span(), is_mutable }),
                    (Some(type_hint_id), None) => {
                        if !is_mutable {
                            return Err(TypeError::MissingBindingInitializer { span: binding.get_span(), is_mutable });
                        }
                        self.typecheck_binding_pattern(is_mutable, false, &binding, &type_hint_id, &mut var_ids)?;

                        None
                    }
                    (None, Some(expr)) => {
                        let typed_expr = self.typecheck_expression(*expr, None)?;
                        let type_id = typed_expr.type_id();
                        if self.type_contains_generics(type_id) {
                            return Err(TypeError::ForbiddenAssignment { span: typed_expr.span(), type_id: *type_id });
                        }
                        self.typecheck_binding_pattern(is_mutable, true, &binding, &type_id, &mut var_ids)?;

                        Some(Box::new(typed_expr))
                    }
                    (Some(type_hint_id), Some(expr)) => {
                        let typed_expr = self.typecheck_expression(*expr, Some(type_hint_id))?;
                        let type_id = typed_expr.type_id();

                        if !self.type_satisfies_other(type_id, &type_hint_id) {
                            let span = typed_expr.span();
                            return Err(TypeError::TypeMismatch { span, expected: vec![type_hint_id], received: *type_id });
                        }
                        self.typecheck_binding_pattern(is_mutable, true, &binding, &type_hint_id, &mut var_ids)?;

                        Some(Box::new(typed_expr))
                    }
                };

                Ok(TypedNode::BindingDeclaration { token, pattern: binding, vars: var_ids, expr: typed_expr })
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
            n => self.typecheck_expression(n, type_hint)
        }
    }

    fn typecheck_binding_pattern(&mut self, is_mutable: bool, is_initialized: bool, pattern: &BindingPattern, type_id: &TypeId, var_ids: &mut Vec<VarId>) -> Result<(), TypeError> {
        match pattern {
            BindingPattern::Variable(var_token) => {
                let var_name = Token::get_ident_name(&var_token);

                let var_id = self.add_variable_to_current_scope(var_name, *type_id, is_mutable, is_initialized, &var_token.get_range())?;
                var_ids.push(var_id);
            }
            BindingPattern::Tuple(_, patterns) => {
                let mut err_kind = None;
                if let Type::GenericInstance(struct_id, generic_ids) = self.project.get_type_by_id(type_id) {
                    if *struct_id != self.project.prelude_tuple_struct_id {
                        err_kind = Some(DestructuringMismatchKind::CannotDestructureAsTuple);
                    } else if patterns.len() != generic_ids.len() {
                        err_kind = Some(DestructuringMismatchKind::InvalidTupleArity(generic_ids.len(), patterns.len()));
                    } else {
                        let generic_ids = generic_ids.clone();
                        for (pattern, type_id) in patterns.iter().zip(generic_ids.iter()) {
                            self.typecheck_binding_pattern(is_mutable, is_initialized, pattern, type_id, var_ids)?;
                        }
                    }
                } else {
                    err_kind = Some(DestructuringMismatchKind::CannotDestructureAsTuple);
                };
                if let Some(kind) = err_kind {
                    return Err(TypeError::DestructuringMismatch { span: pattern.get_span(), kind, type_id: *type_id });
                }
            }
            BindingPattern::Array(_, patterns, _) => {
                let mut err_kind = None;
                let mut inner_type_id = None;
                if let Type::GenericInstance(struct_id, generic_ids) = self.project.get_type_by_id(type_id) {
                    if *struct_id != self.project.prelude_array_struct_id {
                        err_kind = Some(DestructuringMismatchKind::CannotDestructureAsArray);
                    } else {
                        debug_assert!(generic_ids.len() == 1, "Array type should have exactly 1 generic");
                        inner_type_id = Some(generic_ids[0]);
                    }
                } else if *type_id != PRELUDE_STRING_TYPE_ID {
                    err_kind = Some(DestructuringMismatchKind::CannotDestructureAsArray);
                };
                if let Some(kind) = err_kind {
                    return Err(TypeError::DestructuringMismatch { span: pattern.get_span(), kind, type_id: *type_id });
                }

                let is_string = *type_id == PRELUDE_STRING_TYPE_ID;
                let inner_type_id = inner_type_id.unwrap();

                let mut seen_splat = false;
                for (pattern, is_splat) in patterns {
                    let type_id = if *is_splat {
                        if seen_splat {
                            let span = pattern.get_token().get_range();
                            return Err(TypeError::DuplicateSplat { span });
                        }
                        seen_splat = true;

                        if is_string {
                            PRELUDE_STRING_TYPE_ID
                        } else {
                            *type_id
                        }
                    } else {
                        self.add_or_find_type_id(self.project.option_type(inner_type_id))
                    };

                    self.typecheck_binding_pattern(is_mutable, is_initialized, pattern, &type_id, var_ids)?;
                }
            }
        }

        Ok(())
    }

    fn typecheck_expression(&mut self, node: AstNode, type_hint: Option<TypeId>) -> Result<TypedNode, TypeError> {
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
            AstNode::Binary(_, _) => todo!(),
            AstNode::Grouped(token, n) => {
                let typed_expr = self.typecheck_expression(*n.expr, type_hint)?;
                Ok(TypedNode::Grouped { token, expr: Box::new(typed_expr) })
            }
            AstNode::Array(token, n) => {
                // If we have a type_hint and it's an Array, establish the type_hint for the items as the type_hint's first (and only) generic type value.
                // If we don't have a type_hint (or if it's not an Array, which is fine since that'll be caught at the callsite), continue onward.
                let mut inner_type_id = None;
                if let Some(type_hint_id) = &type_hint {
                    let ty = self.project.get_type_by_id(&type_hint_id);
                    if let Type::GenericInstance(struct_id, generic_ids) = ty {
                        if *struct_id == self.project.prelude_array_struct_id {
                            inner_type_id = Some(generic_ids[0]);
                        }
                    }
                }

                let mut typed_items = vec![];
                for item in n.items {
                    let typed_item = self.typecheck_expression(item, inner_type_id)?;
                    let current_value_type_id = typed_item.type_id();

                    match inner_type_id {
                        None => inner_type_id = Some(*current_value_type_id),
                        Some(type_id) => {
                            let Some(unified_type) = self.unify_types(&type_id, current_value_type_id) else {
                                let span = typed_item.span();
                                return Err(TypeError::TypeMismatch { span, expected: vec![type_id], received: *current_value_type_id });
                            };
                            inner_type_id = Some(unified_type);
                        }
                    }

                    typed_items.push(typed_item);
                }

                let type_id = match inner_type_id {
                    None => {
                        let array_struct = &self.project.prelude_module().structs[self.project.prelude_array_struct_id.1];
                        let inner_type_id = array_struct.generics.as_ref().expect("prelude.Array has exactly 1 generic")[0];
                        self.add_or_find_type_id(self.project.array_type(inner_type_id))
                    }
                    Some(inner_type_id) => self.add_or_find_type_id(self.project.array_type(inner_type_id)),
                };

                Ok(TypedNode::Array { token, items: typed_items, type_id })
            }
            AstNode::Set(token, n) => {
                let mut inner_type_id = None;
                if let Some(type_hint_id) = &type_hint {
                    let ty = self.project.get_type_by_id(&type_hint_id);
                    if let Type::GenericInstance(struct_id, generic_ids) = ty {
                        if *struct_id == self.project.prelude_set_struct_id {
                            inner_type_id = Some(generic_ids[0]);
                        }
                    }
                }

                let mut typed_items = vec![];
                for item in n.items {
                    let typed_item = self.typecheck_expression(item, inner_type_id)?;
                    let current_value_type_id = typed_item.type_id();

                    match inner_type_id {
                        None => inner_type_id = Some(*current_value_type_id),
                        Some(type_id) => {
                            let Some(unified_type) = self.unify_types(&type_id, current_value_type_id) else {
                                let span = typed_item.span();
                                return Err(TypeError::TypeMismatch { span, expected: vec![type_id], received: *current_value_type_id });
                            };
                            inner_type_id = Some(unified_type);
                        }
                    }

                    typed_items.push(typed_item);
                }

                let type_id = match inner_type_id {
                    None => {
                        let set_struct = &self.project.prelude_module().structs[self.project.prelude_set_struct_id.1];
                        let inner_type_id = set_struct.generics.as_ref().expect("prelude.Set has exactly 1 generic")[0];
                        self.add_or_find_type_id(self.project.set_type(inner_type_id))
                    }
                    Some(inner_type_id) => self.add_or_find_type_id(self.project.set_type(inner_type_id)),
                };

                Ok(TypedNode::Set { token, items: typed_items, type_id })
            }
            AstNode::Map(token, n) => {
                let mut key_type_id = None;
                let mut val_type_id = None;
                if let Some(type_hint_id) = &type_hint {
                    let ty = self.project.get_type_by_id(&type_hint_id);
                    if let Type::GenericInstance(struct_id, generic_ids) = ty {
                        if *struct_id == self.project.prelude_map_struct_id {
                            key_type_id = Some(generic_ids[0]);
                            val_type_id = Some(generic_ids[1]);
                        }
                    }
                }

                let mut typed_items = vec![];
                for (key_node, val_node) in n.items {
                    let typed_key_node = self.typecheck_expression(key_node, key_type_id)?;
                    let key_node_type_id = *typed_key_node.type_id();

                    match key_type_id {
                        None => key_type_id = Some(key_node_type_id),
                        Some(type_id) => {
                            let Some(unified_type) = self.unify_types(&type_id, &key_node_type_id) else {
                                let span = typed_key_node.span();
                                return Err(TypeError::TypeMismatch { span, expected: vec![type_id], received: key_node_type_id });
                            };
                            key_type_id = Some(unified_type);
                        }
                    }

                    let typed_val_node = self.typecheck_expression(val_node, val_type_id)?;
                    let val_node_type_id = *typed_val_node.type_id();
                    match val_type_id {
                        None => val_type_id = Some(val_node_type_id),
                        Some(type_id) => {
                            let Some(unified_type) = self.unify_types(&type_id, &val_node_type_id) else {
                                let span = typed_val_node.span();
                                return Err(TypeError::TypeMismatch { span, expected: vec![type_id], received: val_node_type_id });
                            };
                            val_type_id = Some(unified_type);
                        }
                    }

                    typed_items.push((typed_key_node, typed_val_node));
                }

                let type_id = match (key_type_id, val_type_id) {
                    (Some(key_type_id), Some(val_type_id)) => self.add_or_find_type_id(self.project.map_type(key_type_id, val_type_id)),
                    _ => {
                        let map_struct = &self.project.prelude_module().structs[self.project.prelude_map_struct_id.1];
                        let map_generics = map_struct.generics.as_ref().expect("prelude.Map has exactly 2 generics");
                        let key_type_id = map_generics[0];
                        let val_type_id = map_generics[1];
                        self.add_or_find_type_id(self.project.map_type(key_type_id, val_type_id))
                    }
                };

                Ok(TypedNode::Map { token, items: typed_items, type_id })
            }
            AstNode::Tuple(token, items) => {
                let mut inner_type_ids = None;
                if let Some(type_hint_id) = &type_hint {
                    let ty = self.project.get_type_by_id(&type_hint_id);
                    if let Type::GenericInstance(struct_id, generic_ids) = ty {
                        if *struct_id == self.project.prelude_tuple_struct_id {
                            inner_type_ids = Some(generic_ids.clone());
                        }
                    }
                }

                let mut typed_items = vec![];
                let mut typed_item_ids = vec![];
                for (idx, item) in items.into_iter().enumerate() {
                    let typed_item = match inner_type_ids.as_ref().and_then(|type_ids| type_ids.get(idx).map(|i| *i)) {
                        None => self.typecheck_expression(item, None)?,
                        Some(type_id) => {
                            let typed_item = self.typecheck_expression(item, Some(type_id))?;
                            let current_value_type_id = typed_item.type_id();
                            if type_id != *current_value_type_id {
                                let span = typed_item.span();
                                return Err(TypeError::TypeMismatch { span, expected: vec![type_id], received: *current_value_type_id });
                            }

                            typed_item
                        }
                    };
                    typed_item_ids.push(*typed_item.type_id());
                    typed_items.push(typed_item);
                }

                let received_type_ids = typed_items.iter().map(|i| *i.type_id()).collect();
                let type_id = self.add_or_find_type_id(self.project.tuple_type(received_type_ids));

                if let Some(inner_type_ids) = &inner_type_ids {
                    if typed_items.len() != inner_type_ids.len() {
                        let span = token.get_range().expand(&typed_items.last().map(|i| i.span()).unwrap_or(token.get_range()));
                        let expected = self.add_or_find_type_id(self.project.tuple_type(inner_type_ids.clone()));//Type::GenericInstance(self.project.prelude_tuple_struct_id, inner_type_ids.clone()));
                        return Err(TypeError::TypeMismatch { span, expected: vec![expected], received: type_id });
                    }
                }

                Ok(TypedNode::Tuple { token, items: typed_items, type_id })
            }
            AstNode::Identifier(token, n) => {
                debug_assert!(n.is_none(), "Not implemented yet");

                let name = Token::get_ident_name(&token);
                let scope_id = ScopeId(self.current_module().id, self.current_scope);
                let variable = self.project.find_variable_by_name(&scope_id, &name);
                let Some(Variable { id, type_id, .. }) = variable else {
                    return Err(TypeError::UnknownIdentifier { span: token.get_range(), token });
                };
                let var_id = *id;
                let mut var_type_id = *type_id;

                if let Some(type_hint) = type_hint {
                    var_type_id = self.substitute_generics(&type_hint, &var_type_id);
                }

                if let Some(func_id) = self.current_function {
                    let VarId(var_scope_id, _) = var_id;
                    let FuncId(func_scope_id, _) = func_id;

                    if !self.scope_contains_other(&var_scope_id, &func_scope_id) {
                        let var = self.project.get_var_by_id_mut(&var_id);
                        var.is_captured = true;

                        let func = self.project.get_func_by_id_mut(&func_id);
                        func.captured_vars.push(var_id);
                    }
                }

                Ok(TypedNode::Identifier { token, var_id, type_id: var_type_id })
            }
            AstNode::Assignment(_, _) |
            AstNode::Indexing(_, _) |
            AstNode::Accessor(_, _) => todo!(),
            AstNode::Invocation(token, n) => {
                let InvocationNode { target, args } = n;

                let typed_target = self.typecheck_expression(*target, None)?;
                let (params_data, return_type_id) = match &typed_target {
                    TypedNode::Identifier { var_id, type_id, .. } => {
                        let var = self.project.get_var_by_id(var_id);
                        if let VariableAlias::Function(alias_func_id) = var.alias {
                            let function = self.project.get_func_by_id(&alias_func_id);

                            let params_data = function.params.iter().enumerate().map(|(idx, p)| (idx, p.name.clone(), p.type_id, p.default_value.is_some())).collect_vec();
                            (params_data, function.return_type_id)
                        } else {
                            return Err(TypeError::IllegalInvocation { span: typed_target.span(), type_id: *type_id });
                        }
                    }
                    _ => unimplemented!("Internal error: invocation of target expression not implemented")
                };

                let num_required_args = params_data.len();
                let num_provided_args = args.len();

                let mut seen_labels = HashSet::new();
                let mut typed_arguments = (0..params_data.len()).map(|_| None).collect_vec();
                for (idx, (label, arg_node)) in args.into_iter().enumerate() {
                    let (param_idx, _, param_type_id, _) = if let Some(label) = label {
                        if idx > 0 && seen_labels.is_empty() {
                            return Err(TypeError::MixedArgumentType { span: label.get_range() });
                        }

                        let label_name = Token::get_ident_name(&label);
                        if seen_labels.contains(&label_name) {
                            return Err(TypeError::DuplicateArgumentLabel { span: label.get_range(), name: label_name });
                        }
                        let Some(param_data) = params_data.iter().find(|(_, param_name, _, _)| *param_name == label_name) else {
                            return Err(TypeError::UnexpectedArgumentName { span: label.get_range(), arg_name: label_name });
                        };
                        if idx >= params_data.len() {
                            // This _should_ be unreachable given the two cases above, but just in case let's return an error here as well
                            return Err(TypeError::InvalidArity { span: label.get_range(), num_required_args, num_provided_args });
                        }

                        seen_labels.insert(label_name);

                        param_data
                    } else if idx > 0 && !seen_labels.is_empty() {
                        return Err(TypeError::MixedArgumentType { span: arg_node.get_token().get_range() });
                    } else {
                        if idx >= params_data.len() {
                            let span = typed_target.span().expand(&token.get_range());
                            return Err(TypeError::InvalidArity { span, num_required_args, num_provided_args });
                        }

                        &params_data[idx]
                    };

                    let typed_arg_value = self.typecheck_expression(arg_node, Some(*param_type_id))?;
                    let arg_type_id = typed_arg_value.type_id();
                    if !self.type_satisfies_other(arg_type_id, param_type_id) {
                        return Err(TypeError::TypeMismatch { span: typed_arg_value.span(), expected: vec![*param_type_id], received: *arg_type_id });
                    }

                    typed_arguments[*param_idx] = Some(typed_arg_value);
                }

                for (param_idx, _, _, param_is_optional) in params_data {
                    if typed_arguments[param_idx] == None && !param_is_optional {
                        let span = typed_target.span().expand(&token.get_range());
                        return Err(TypeError::InvalidArity { span, num_required_args, num_provided_args });
                    }
                }

                let type_id = if let Some(type_hint_id) = type_hint {
                    self.substitute_generics(&type_hint_id, &return_type_id)
                } else {
                    return_type_id
                };

                Ok(TypedNode::Invocation { target: Box::new(typed_target), arguments: typed_arguments, type_id })
            }
            AstNode::IfExpression(_, _) |
            AstNode::MatchExpression(_, _) |
            AstNode::Lambda(_, _) |
            AstNode::Try(_, _) => todo!(),
            n => unreachable!("Internal error: node is not an expression: {:?}", n),
        }
    }
}
