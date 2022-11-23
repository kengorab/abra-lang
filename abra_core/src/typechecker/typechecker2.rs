use std::collections::HashSet;
use std::path::PathBuf;
use itertools::{Either, Itertools};
use crate::parser;
use crate::parser::parser::ParseResult;
use crate::lexer::lexer_error::LexerError;
use crate::lexer::tokens::{Range, Token};
use crate::parser::ast::{AstLiteralNode, AstNode, BindingDeclNode, BindingPattern, FunctionDeclNode, TypeIdentifier, UnaryNode, UnaryOp};
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
}

impl Default for Project {
    fn default() -> Self {
        let placeholder_struct_id = StructId { module_id: PRELUDE_MODULE_ID, id: 0 };

        Self {
            modules: vec![],
            prelude_option_struct_id: placeholder_struct_id,
            prelude_array_struct_id: placeholder_struct_id,
            prelude_tuple_struct_id: placeholder_struct_id,
            prelude_set_struct_id: placeholder_struct_id,
        }
    }
}

impl Project {
    pub fn prelude_module(&self) -> &TypedModule {
        &self.modules[PRELUDE_MODULE_ID.id]
    }

    pub fn get_type_by_id(&self, type_id: &TypeId) -> &Type {
        let TypeId { module_id: ModuleId { id: module_id }, id } = type_id;
        let module = &self.modules[*module_id];
        &module.types[*id]
    }

    pub fn get_struct_by_id(&self, struct_id: &StructId) -> &Struct {
        let StructId { module_id: ModuleId { id: module_id }, id } = struct_id;
        let module = &self.modules[*module_id];
        &module.structs[*id]
    }

    pub fn get_func_by_id(&self, func_id: &FuncId) -> &Function {
        let FuncId { scope_id: ScopeId { module_id: ModuleId { id: module_id }, id: scope_id }, id } = func_id;
        let scope = &self.modules[*module_id].scopes[*scope_id];
        &scope.funcs[*id]
    }

    pub fn get_func_by_id_mut(&mut self, func_id: &FuncId) -> &mut Function {
        let FuncId { scope_id: ScopeId { module_id: ModuleId { id: module_id }, id: scope_id }, id } = func_id;
        let scope = &mut self.modules[*module_id].scopes[*scope_id];
        &mut scope.funcs[*id]
    }

    pub fn get_var_by_id(&self, var_id: &VarId) -> &Variable {
        let VarId { scope_id: ScopeId { module_id: ModuleId { id: module_id }, id: scope_id }, id } = var_id;
        let scope = &self.modules[*module_id].scopes[*scope_id];
        &scope.vars[*id]
    }

    pub fn get_var_by_id_mut(&mut self, var_id: &VarId) -> &mut Variable {
        let VarId { scope_id: ScopeId { module_id: ModuleId { id: module_id }, id: scope_id }, id } = var_id;
        let scope = &mut self.modules[*module_id].scopes[*scope_id];
        &mut scope.vars[*id]
    }

    pub fn find_struct_by_name(&self, module_id: &ModuleId, name: &String) -> Option<&Struct> {
        let module = &self.modules[module_id.id];
        module.structs.iter()
            .find(|s| s.name == *name)
            .or_else(|| {
                // If struct cannot be found in current module, look in the prelude module
                self.prelude_module().structs.iter()
                    .find(|s| s.name == *name)
            })
    }

    pub fn find_type_id(&self, module_id: &ModuleId, ty: &Type) -> Option<TypeId> {
        let module = &self.modules[module_id.id];
        for (idx, t) in module.types.iter().enumerate() {
            if t == ty {
                return Some(TypeId { module_id: module.id, id: idx });
            }
        }

        None
    }

    pub fn add_or_find_type_id(&mut self, module_id: &ModuleId, ty: Type) -> TypeId {
        if let Some(type_id) = self.find_type_id(&module_id, &ty) {
            type_id
        } else {
            let module = &mut self.modules[module_id.id];
            let type_id = TypeId { module_id: module.id, id: module.types.len() };
            module.types.push(ty);

            type_id
        }
    }

    pub fn find_variable_by_name(&self, scope_id: &ScopeId, name: &String) -> Option<&Variable> {
        let mut scope_id = &Some(*scope_id);
        while let Some(ScopeId { module_id: ModuleId { id: module_id }, id }) = scope_id {
            let scope = &self.modules[*module_id].scopes[*id];

            for var in &scope.vars {
                if var.name == *name {
                    return Some(var);
                }
            }

            scope_id = &scope.parent;
        }

        None
    }

    pub fn option_type(&self, inner_type_id: TypeId) -> Type {
        Type::GenericInstance(self.prelude_option_struct_id, vec![inner_type_id])
    }

    pub fn array_type(&self, inner_type_id: TypeId) -> Type {
        Type::GenericInstance(self.prelude_array_struct_id, vec![inner_type_id])
    }

    pub fn tuple_type(&self, inner_type_ids: Vec<TypeId>) -> Type {
        Type::GenericInstance(self.prelude_tuple_struct_id, inner_type_ids)
    }

    pub fn type_repr(&self, type_id: &TypeId) -> String {
        if *type_id == PRELUDE_UNKNOWN_TYPE_ID {
            return "_".to_string();
        }

        let ty = self.get_type_by_id(type_id);
        match ty {
            Type::Builtin(builtin_id) => {
                if *builtin_id == PRELUDE_UNKNOWN_TYPE_ID.id {
                    "_".to_string()
                } else if *builtin_id == PRELUDE_UNIT_TYPE_ID.id {
                    "Unit".to_string()
                } else if *builtin_id == PRELUDE_INT_TYPE_ID.id {
                    "Int".to_string()
                } else if *builtin_id == PRELUDE_FLOAT_TYPE_ID.id {
                    "Float".to_string()
                } else if *builtin_id == PRELUDE_BOOL_TYPE_ID.id {
                    "Bool".to_string()
                } else if *builtin_id == PRELUDE_STRING_TYPE_ID.id {
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
        }
    }
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct StructId {
    pub module_id: ModuleId,
    pub id: usize,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Struct {
    pub id: StructId,
    pub name: String,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Builtin(/* prelude_type_idx: */ usize),
    Generic(String),
    GenericInstance(StructId, Vec<TypeId>),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ScopeId {
    pub module_id: ModuleId,
    pub id: usize,
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub vars: Vec<Variable>,
    pub funcs: Vec<Function>,
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
    // Variables with no defined_span are builtins
    pub defined_span: Option<Range>,
    pub is_captured: bool,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FuncId {
    pub scope_id: ScopeId,
    pub id: usize,
}

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
    // Params with no defined_span are builtins
    pub defined_span: Option<Range>,
    pub default_value: Option<TypedNode>,
}

#[derive(Debug, PartialEq)]
pub struct TypedModule {
    pub id: ModuleId,
    pub name: String,
    pub types: Vec<Type>,
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
    Identifier { token: Token, var_id: VarId, type_id: TypeId },

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
            TypedNode::Identifier { type_id, .. } => type_id,

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
            TypedNode::Identifier { token, .. } => token.get_range(),

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
pub const PRELUDE_UNKNOWN_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: usize::MAX };
pub const PRELUDE_UNIT_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 0 };
pub const PRELUDE_INT_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 1 };
pub const PRELUDE_FLOAT_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 2 };
pub const PRELUDE_BOOL_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 3 };
pub const PRELUDE_STRING_TYPE_ID: TypeId = TypeId { module_id: PRELUDE_MODULE_ID, id: 4 };

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
        debug_assert!(span.start.line == span.end.line, "TODO: Displaying errors for multi-line spans");

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
            TypeError::ReturnTypeMismatch { span, .. } => span
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
        };

        let error_line = format!("Error at {}:{}:{}", file_name, span.start.line, span.start.col);
        format!("{}\n{}", error_line, msg)
    }
}

pub struct Typechecker2<'a, L: LoadModule> {
    module_loader: &'a L,
    project: &'a mut Project,
    current_scope: usize,
    current_function: Option<FuncId>,
}

impl<'a, L: LoadModule> Typechecker2<'a, L> {
    pub fn new(module_loader: &'a L, project: &'a mut Project) -> Typechecker2<'a, L> {
        Typechecker2 { module_loader, project, current_scope: 0, current_function: None }
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
        self.project.add_or_find_type_id(&current_module_id, ty)
    }

    fn get_struct_by_name(&self, name: &String) -> Option<&Struct> {
        let current_module_id = self.current_module().id;
        self.project.find_struct_by_name(&current_module_id, name)
    }

    fn type_satisfies_other(&self, base_type: &TypeId, target_type: &TypeId) -> bool {
        let base_ty = self.project.get_type_by_id(base_type);
        let target_ty = self.project.get_type_by_id(target_type);

        match (base_ty, target_ty) {
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

        let id = VarId { scope_id: current_scope.id, id: current_scope.vars.len() };
        let var = Variable { id, name, type_id, is_mutable, is_initialized, defined_span: Some(span.clone()), is_captured: false };
        current_scope.vars.push(var);

        Ok(id)
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

        let id = FuncId { scope_id: current_scope.id, id: current_scope.funcs.len() };
        let func = Function { id, name, params, return_type_id, defined_span: Some(span), body: vec![], captured_vars: vec![] };
        current_scope.funcs.push(func);

        Ok(id)
    }

    fn begin_child_scope(&mut self) -> ScopeId {
        let current_scope_id = self.current_scope;
        let current_module = self.current_module_mut();
        let current_scope = &current_module.scopes[current_scope_id];
        let new_scope_id = ScopeId { module_id: current_module.id, id: current_module.scopes.len() };

        let child_scope = Scope {
            id: new_scope_id,
            parent: Some(current_scope.id),
            vars: vec![],
            funcs: vec![],
        };
        current_module.scopes.push(child_scope);

        self.current_scope = new_scope_id.id;

        new_scope_id
    }

    fn end_child_scope(&mut self) -> ScopeId {
        let current_scope_id = self.current_scope;
        let current_module = self.current_module_mut();
        let current_scope = &current_module.scopes[current_scope_id];

        let Some(parent) = current_scope.parent else {
            unreachable!("Internal error: a child scope must always have a parent");
        };
        self.current_scope = parent.id;

        parent
    }

    fn scope_contains_other(&self, inner: &ScopeId, outer: &ScopeId) -> bool {
        // if inner == outer {
        //     return true;
        // }

        let inner_scope = &self.project.modules[inner.module_id.id].scopes[inner.id];
        let mut parent = inner_scope.parent;
        while let Some(parent_id) = parent {
            if parent_id == *outer {
                return true;
            }

            let parent_scope = &self.project.modules[parent_id.module_id.id].scopes[parent_id.id];
            parent = parent_scope.parent;
        }

        false
    }

    /* TYPECHECKING */

    pub fn typecheck_prelude(&mut self) {
        debug_assert!(self.project.modules.is_empty());

        let mut prelude_module = TypedModule { id: PRELUDE_MODULE_ID, name: "prelude".to_string(), types: vec![], functions: vec![], structs: vec![], code: vec![], scopes: vec![] };

        prelude_module.types.push(Type::Builtin(PRELUDE_UNIT_TYPE_ID.id));
        prelude_module.types.push(Type::Builtin(PRELUDE_INT_TYPE_ID.id));
        prelude_module.types.push(Type::Builtin(PRELUDE_FLOAT_TYPE_ID.id));
        prelude_module.types.push(Type::Builtin(PRELUDE_BOOL_TYPE_ID.id));
        prelude_module.types.push(Type::Builtin(PRELUDE_STRING_TYPE_ID.id));

        let option_struct_id = StructId { module_id: PRELUDE_MODULE_ID, id: prelude_module.structs.len() };
        prelude_module.structs.push(Struct { id: option_struct_id, name: "Option".to_string() });
        self.project.prelude_option_struct_id = option_struct_id;

        let array_struct_id = StructId { module_id: PRELUDE_MODULE_ID, id: prelude_module.structs.len() };
        prelude_module.structs.push(Struct { id: array_struct_id, name: "Array".to_string() });
        self.project.prelude_array_struct_id = array_struct_id;

        let tuple_struct_id = StructId { module_id: PRELUDE_MODULE_ID, id: prelude_module.structs.len() };
        prelude_module.structs.push(Struct { id: tuple_struct_id, name: "Tuple".to_string() });
        self.project.prelude_tuple_struct_id = tuple_struct_id;

        let set_struct_id = StructId { module_id: PRELUDE_MODULE_ID, id: prelude_module.structs.len() };
        prelude_module.structs.push(Struct { id: set_struct_id, name: "Set".to_string() });
        self.project.prelude_set_struct_id = set_struct_id;


        self.project.modules.push(prelude_module);

        // TODO: Generics should be scoped, either to a function or a type. This scopes the generic `T` to the _module_ level, which is clumsy.
        let generic_t_type_id = self.project.add_or_find_type_id(&PRELUDE_MODULE_ID, Type::Generic("T".to_string()));
        let none_type_id = self.project.add_or_find_type_id(&PRELUDE_MODULE_ID, self.project.option_type(generic_t_type_id));
        self.project.modules[0].scopes.push(Scope {
            id: ScopeId { module_id: PRELUDE_MODULE_ID, id: 0 },
            parent: None,
            vars: vec![
                Variable {
                    id: VarId { scope_id: ScopeId { module_id: PRELUDE_MODULE_ID, id: 0 }, id: 0 },
                    name: "None".to_string(),
                    type_id: none_type_id,
                    is_mutable: false,
                    is_initialized: true,
                    defined_span: None,
                    is_captured: false,
                }
            ],
            funcs: vec![], // TODO: Add println, print, and range
        });
    }

    pub fn typecheck_module(&mut self, m_id: &parser::ast::ModuleId) -> Result<(), TypecheckError> {
        debug_assert!(self.project.modules.len() == 1, "Prelude must be loaded in order to typecheck further modules");

        let (file_name, parse_result) = self.module_loader.load_untyped_ast(&m_id).map_err(Either::Left)?;

        if !parse_result.imports.is_empty() {
            unimplemented!("Typechecking imports");
        }

        let prelude_scope_id = ScopeId { module_id: PRELUDE_MODULE_ID, id: 0 };

        let id = ModuleId { id: self.project.modules.len() };
        let scope_id = ScopeId { module_id: id, id: 0 };
        let root_scope = Scope { id: scope_id, parent: Some(prelude_scope_id), vars: vec![], funcs: vec![] };
        self.project.modules.push(TypedModule {
            id,
            name: file_name,
            types: vec![],
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
                AstNode::FunctionDecl(token, node) => func_decls.push((token, node)),
                n @ AstNode::TypeDecl(_, _) => type_decls.push(n),
                n @ AstNode::EnumDecl(_, _) => enum_decls.push(n),
                _ => {}
            }
        }

        let func_ids = self.typecheck_functions_pass_1(func_decls)?;

        debug_assert!(type_decls.is_empty());
        debug_assert!(enum_decls.is_empty());

        let mut fn_idx = 0;
        for node in nodes {
            match node {
                AstNode::FunctionDecl(token, decl_node) => {
                    debug_assert!(fn_idx < func_ids.len(), "There should be a FuncId for each declaration in this block");

                    let func_id = func_ids[fn_idx];
                    fn_idx += 1;

                    self.typecheck_function_pass_2(func_id, (token, decl_node))?;
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

    fn typecheck_functions_pass_1(&mut self, func_decls: Vec<(&Token, &FunctionDeclNode)>) -> Result<Vec<FuncId>, TypeError> {
        let mut func_ids = vec![];

        for (_, node) in func_decls {
            let FunctionDeclNode { export_token, name, type_args, args, ret_type, .. } = node;
            if export_token.is_some() { unimplemented!("Internal error: imports/exports") }
            if !type_args.is_empty() { unimplemented!("Internal error: function type arguments") }

            let mut seen_arg_names = HashSet::new();
            let mut params = vec![];
            for (ident, type_ident, is_vararg, default_value) in args {
                if *is_vararg { unimplemented!("Internal error: variadic parameters") }

                let arg_name = Token::get_ident_name(ident);
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
            func_ids.push(func_id);
        }

        Ok(func_ids)
    }

    fn typecheck_function_pass_2(&mut self, func_id: FuncId, func_decl: (Token, FunctionDeclNode)) -> Result<(), TypeError> {
        let (_, node) = func_decl;
        let orig_func_id = self.current_function;
        self.current_function = Some(func_id);

        self.begin_child_scope();

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
                        if *type_id == PRELUDE_UNKNOWN_TYPE_ID || self.type_contains_generics(type_id) {
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
                        self.typecheck_binding_pattern(is_mutable, true, &binding, &type_id, &mut var_ids)?;

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
                            if type_id != *current_value_type_id {
                                let span = typed_item.span();
                                return Err(TypeError::TypeMismatch { span, expected: vec![type_id], received: *current_value_type_id });
                            }
                        }
                    }

                    typed_items.push(typed_item);
                }

                let type_id = match inner_type_id {
                    None => PRELUDE_UNKNOWN_TYPE_ID,
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
                            if type_id != *current_value_type_id {
                                let span = typed_item.span();
                                return Err(TypeError::TypeMismatch { span, expected: vec![type_id], received: *current_value_type_id });
                            }
                        }
                    }

                    typed_items.push(typed_item);
                }

                let type_id = match inner_type_id {
                    None => PRELUDE_UNKNOWN_TYPE_ID,
                    Some(inner_type_id) => self.add_or_find_type_id(Type::GenericInstance(self.project.prelude_set_struct_id, vec![inner_type_id])),
                };

                Ok(TypedNode::Set { token, items: typed_items, type_id })
            }
            AstNode::Map(_, _) => todo!(),
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
                let scope_id = ScopeId { module_id: self.current_module().id, id: self.current_scope };
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
                    if !self.scope_contains_other(&var_id.scope_id, &func_id.scope_id) {
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
