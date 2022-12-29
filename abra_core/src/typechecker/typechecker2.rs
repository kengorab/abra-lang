use std::collections::{HashMap, HashSet, VecDeque};
use std::path::PathBuf;
use itertools::{Either, EitherOrBoth, Itertools};
use crate::{parser, tokenize_and_parse_stub};
use crate::parser::parser::{ParseResult};
use crate::lexer::lexer_error::LexerError;
use crate::lexer::tokens::{POSITION_BOGUS, Range, Token};
use crate::parser::ast::{AssignmentNode, AstLiteralNode, AstNode, BinaryNode, BinaryOp, BindingDeclNode, BindingPattern, FunctionDeclNode, InvocationNode, Parameter, TypeDeclField, TypeDeclNode, TypeIdentifier, UnaryNode, UnaryOp};
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
    pub prelude_none_type_id: TypeId,
    pub prelude_option_struct_id: StructId,
    pub prelude_int_struct_id: StructId,
    pub prelude_float_struct_id: StructId,
    pub prelude_bool_struct_id: StructId,
    pub prelude_string_struct_id: StructId,
    pub prelude_array_struct_id: StructId,
    pub prelude_tuple_struct_id: StructId,
    pub prelude_set_struct_id: StructId,
    pub prelude_map_struct_id: StructId,
}

const PLACEHOLDER_STRUCT_ID: StructId = StructId(PRELUDE_MODULE_ID, usize::MAX);
const PLACEHOLDER_TYPE_ID: TypeId = TypeId(PRELUDE_SCOPE_ID, usize::MAX);

impl Default for Project {
    fn default() -> Self {
        Self {
            modules: vec![],
            prelude_none_type_id: PLACEHOLDER_TYPE_ID,
            prelude_option_struct_id: PLACEHOLDER_STRUCT_ID,
            prelude_int_struct_id: PLACEHOLDER_STRUCT_ID,
            prelude_float_struct_id: PLACEHOLDER_STRUCT_ID,
            prelude_bool_struct_id: PLACEHOLDER_STRUCT_ID,
            prelude_string_struct_id: PLACEHOLDER_STRUCT_ID,
            prelude_array_struct_id: PLACEHOLDER_STRUCT_ID,
            prelude_tuple_struct_id: PLACEHOLDER_STRUCT_ID,
            prelude_set_struct_id: PLACEHOLDER_STRUCT_ID,
            prelude_map_struct_id: PLACEHOLDER_STRUCT_ID,
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

    pub fn get_struct_by_type_id(&self, type_id: &TypeId) -> Option<(&Struct, HashMap<TypeId, TypeId>)> {
        let mut generic_substitutions = HashMap::new();
        let ty = self.get_type_by_id(type_id);
        let struct_ = match ty {
            Type::GenericInstance(struct_id, generic_type_ids) => {
                let struct_ = self.get_struct_by_id(struct_id);
                generic_substitutions.extend(struct_.generic_ids.iter().zip(generic_type_ids).map(|(g_id, t_id)| (*g_id, *t_id)));
                struct_
            }
            Type::Primitive(primitive_type) => {
                let struct_id = match primitive_type {
                    PrimitiveType::Any | PrimitiveType::Unit => return None,
                    PrimitiveType::Int => &self.prelude_int_struct_id,
                    PrimitiveType::Float => &self.prelude_float_struct_id,
                    PrimitiveType::Bool => &self.prelude_bool_struct_id,
                    PrimitiveType::String => &self.prelude_string_struct_id,
                };
                self.get_struct_by_id(struct_id)
            }
            Type::Struct(_) |
            Type::Generic(_, _) |
            Type::Function(_, _, _, _) => todo!()
        };

        Some((struct_, generic_substitutions))
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

    pub fn find_type_id_by<F>(&self, scope_id: &ScopeId, finder: F) -> Option<TypeId>
        where F: Fn(&Type) -> bool
    {
        self.walk_scope_chain_with(scope_id, |scope| {
            for (idx, typ) in scope.types.iter().enumerate() {
                if finder(typ) {
                    return Some(TypeId(scope.id, idx));
                }
            }
            None
        })
    }

    pub fn find_type_id(&self, scope_id: &ScopeId, ty: &Type) -> Option<TypeId> {
        self.find_type_id_by(scope_id, |typ| typ == ty)
    }

    pub fn find_type_id_for_generic<S: AsRef<str>>(&self, scope_id: &ScopeId, name: S) -> Option<TypeId> {
        self.find_type_id_by(scope_id, |typ| match typ {
            Type::Generic(_, generic_name) if generic_name == name.as_ref() => true,
            _ => false,
        })
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
        if let Some(type_id) = self.find_type_id(&scope_id, &ty) {
            type_id
        } else {
            self.add_type_id(scope_id, ty)
        }
    }

    pub fn find_variable_by_name(&self, scope_id: &ScopeId, name: &String) -> Option<&Variable> {
        self.walk_scope_chain_with(scope_id, |scope| {
            for var in &scope.vars {
                if var.name == *name {
                    return Some(var);
                }
            }
            None
        })
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

    pub fn function_type(&self, param_type_ids: Vec<TypeId>, num_required: usize, is_variadic: bool, return_type_id: TypeId) -> Type {
        Type::Function(param_type_ids, num_required, is_variadic, return_type_id)
    }

    pub fn function_type_for_function(&self, func: &Function, trim_self: bool) -> Type {
        let mut num_required = 0;

        let iter = if func.has_self && trim_self {
            func.params.iter().skip(1)
        } else {
            func.params.iter().skip(0)
        };

        let param_type_ids = iter
            .map(|param| {
                if param.default_value.is_none() && !param.is_variadic { num_required += 1; }
                param.type_id
            })
            .collect();
        let is_variadic = func.params.last().map(|p| p.is_variadic).unwrap_or(false);
        self.function_type(param_type_ids, num_required, is_variadic, func.return_type_id)
    }

    pub fn struct_type(&self, struct_id: StructId) -> Type {
        Type::Struct(struct_id)
    }

    pub fn type_repr(&self, type_id: &TypeId) -> String {
        let ty = self.get_type_by_id(type_id);
        match ty {
            Type::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Unit => "Unit".to_string(),
                PrimitiveType::Any => "Any".to_string(),
                PrimitiveType::Int => "Int".to_string(),
                PrimitiveType::Float => "Float".to_string(),
                PrimitiveType::Bool => "Bool".to_string(),
                PrimitiveType::String => "String".to_string(),
            }
            Type::Generic(_, name) => name.to_string(),
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
                    if !generic_ids.is_empty() {
                        let inner_type_reprs = generic_ids.iter().map(|type_id| self.type_repr(type_id)).join(", ");
                        format!("{}<{}>", struct_.name, inner_type_reprs)
                    } else {
                        format!("{}", struct_.name)
                    }
                }
            }
            Type::Function(param_type_ids, num_required_params, is_variadic, return_type_id) => {
                let num_params = *num_required_params + if *is_variadic { 1 } else { 0 };
                let param_reprs = param_type_ids.iter()
                    .take(num_params)
                    .enumerate()
                    .map(|(idx, type_id)| {
                        let repr = self.type_repr(type_id);
                        if idx == num_params - 1 && *is_variadic { format!("...{}", repr) } else { repr }
                    })
                    .join(", ");
                let return_repr = self.type_repr(return_type_id);
                format!("({}) => {}", param_reprs, return_repr)
            }
            Type::Struct(struct_id) => {
                let struct_ = self.get_struct_by_id(struct_id);
                struct_.name.to_string()
            }
        }
    }

    fn walk_scope_chain_with<'a, F, V>(&'a self, starting_scope_id: &ScopeId, f: F) -> Option<V>
        where F: Fn(&'a Scope) -> Option<V>
    {
        let mut cur_scope_id = &Some(*starting_scope_id);
        while let Some(scope_id) = cur_scope_id {
            let ScopeId(ModuleId(module_idx), scope_idx) = scope_id;
            let scope = &self.modules[*module_idx].scopes[*scope_idx];

            let v = f(scope);
            if v.is_some() { return v; }

            cur_scope_id = &scope.parent;
        }

        None
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct ModuleId(/* idx: */ pub usize);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct StructId(/* module_id: */ pub ModuleId, /* idx: */ pub usize);

#[derive(Debug, PartialEq)]
pub struct Struct {
    pub id: StructId,
    pub struct_scope_id: ScopeId,
    pub name: String,
    // Structs with no defined_span are builtins
    pub defined_span: Option<Range>,
    pub generic_ids: Vec<TypeId>,
    pub self_type_id: TypeId,
    pub fields: Vec<StructField>,
    pub methods: Vec<FuncId>,
    pub static_methods: Vec<FuncId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructField {
    pub name: String,
    pub type_id: TypeId,
    pub defined_span: Range,
    pub is_readonly: bool,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeId(/* scope_id: */ pub ScopeId, /* idx: */ pub usize);

#[derive(Clone, Debug, PartialEq)]
pub enum PrimitiveType {
    Unit,
    Any,
    Int,
    Float,
    Bool,
    String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Generic(/*span: */ Option<Range>, /* name: */ String),
    GenericInstance(StructId, Vec<TypeId>),
    Function(/* parameter_type_ids: */ Vec<TypeId>, /* num_required_params: */ usize, /* is_variadic: */ bool, /* return_type_id: */ TypeId),
    Struct(StructId),
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
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
    pub is_parameter: bool,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FuncId(/* scope_id: */ pub ScopeId, /* idx: */ pub usize);

#[derive(Debug, PartialEq)]
pub struct Function {
    pub id: FuncId,
    pub fn_scope_id: ScopeId,
    pub name: String,
    pub generic_ids: Vec<TypeId>,
    pub has_self: bool,
    // TODO: Could be a slice? we won't expand once they're known
    pub params: Vec<FunctionParam>,
    pub return_type_id: TypeId,
    // Functions with no defined_span are builtins or lambdas (since they can't have name collisions anyway)
    pub defined_span: Option<Range>,
    pub body: Vec<TypedNode>,
    pub captured_vars: Vec<VarId>,
}

impl Function {
    fn is_variadic(&self) -> bool {
        self.params.last().map(|p| p.is_variadic).unwrap_or(false)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionParam {
    pub name: String,
    pub type_id: TypeId,
    // Params with no defined_span are for builtin functions
    pub defined_span: Option<Range>,
    pub default_value: Option<TypedNode>,
    pub is_variadic: bool,
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
pub enum AccessorKind {
    Field,
    Method,
    StaticMethod,
}

#[derive(Debug, PartialEq)]
pub enum AssignmentKind {
    Identifier { var_id: VarId },
    Accessor { target: Box<TypedNode>, kind: AccessorKind, member_idx: usize },
}

#[derive(Debug, PartialEq)]
pub enum TypedNode {
    // Expressions
    Literal { token: Token, value: TypedLiteral, type_id: TypeId },
    Unary { token: Token, op: UnaryOp, expr: Box<TypedNode> },
    Binary { op: BinaryOp, left: Box<TypedNode>, right: Box<TypedNode>, type_id: TypeId },
    Grouped { token: Token, expr: Box<TypedNode> },
    Array { token: Token, items: Vec<TypedNode>, type_id: TypeId },
    Tuple { token: Token, items: Vec<TypedNode>, type_id: TypeId },
    Set { token: Token, items: Vec<TypedNode>, type_id: TypeId },
    Map { token: Token, items: Vec<(TypedNode, TypedNode)>, type_id: TypeId },
    Identifier { token: Token, var_id: VarId, type_arg_ids: Vec<(TypeId, Range)>, type_id: TypeId },
    NoneValue { token: Token, type_id: TypeId },
    Invocation { target: Box<TypedNode>, arguments: Vec<Option<TypedNode>>, type_id: TypeId },
    Accessor { target: Box<TypedNode>, kind: AccessorKind, member_idx: usize, member_span: Range, type_id: TypeId },
    Lambda { span: Range, func_id: FuncId, type_id: TypeId },
    Assignment { span: Range, kind: AssignmentKind, type_id: TypeId },

    // Statements
    BindingDeclaration { token: Token, pattern: BindingPattern, vars: Vec<VarId>, expr: Option<Box<TypedNode>> },
}

impl TypedNode {
    pub fn type_id(&self) -> &TypeId {
        match self {
            // Expressions
            TypedNode::Literal { type_id, .. } => type_id,
            TypedNode::Unary { expr, .. } => expr.type_id(),
            TypedNode::Binary { type_id, .. } => type_id,
            TypedNode::Grouped { expr, .. } => expr.type_id(),
            TypedNode::Array { type_id, .. } => type_id,
            TypedNode::Tuple { type_id, .. } => type_id,
            TypedNode::Set { type_id, .. } => type_id,
            TypedNode::Map { type_id, .. } => type_id,
            TypedNode::Identifier { type_id, .. } => type_id,
            TypedNode::NoneValue { type_id, .. } => type_id,
            TypedNode::Invocation { type_id, .. } => type_id,
            TypedNode::Accessor { type_id, .. } => type_id,
            TypedNode::Lambda { type_id, .. } => type_id,
            TypedNode::Assignment { type_id, .. } => type_id,

            // Statements
            TypedNode::BindingDeclaration { .. } => &PRELUDE_UNIT_TYPE_ID,
        }
    }

    fn span(&self) -> Range {
        match self {
            // Expressions
            TypedNode::Literal { token, .. } => token.get_range(),
            TypedNode::Unary { token, expr, .. } => token.get_range().expand(&expr.span()),
            TypedNode::Binary { left, right, .. } => left.span().expand(&right.span()),
            TypedNode::Grouped { token, expr } => token.get_range().expand(&expr.span()),
            TypedNode::Array { token, items, .. } => token.get_range().expand(&items.last().map(|i| i.span()).unwrap_or(token.get_range())),
            TypedNode::Tuple { token, items, .. } => token.get_range().expand(&items.last().map(|i| i.span()).unwrap_or(token.get_range())),
            TypedNode::Set { token, items, .. } => token.get_range().expand(&items.last().map(|i| i.span()).unwrap_or(token.get_range())),
            TypedNode::Map { token, items, .. } => token.get_range().expand(&items.last().map(|(_, v)| v.span()).unwrap_or(token.get_range())),
            TypedNode::Identifier { token, .. } => token.get_range(),
            TypedNode::NoneValue { token, .. } => token.get_range(),
            TypedNode::Invocation { target, arguments, .. } => {
                let start = target.span();
                let mut max = None;
                for arg in arguments {
                    if let Some(arg) = arg {
                        let arg_span = arg.span();
                        match &max {
                            None => max = Some(arg_span),
                            Some(max_span) => if arg_span.end.line > max_span.end.line || arg_span.end.col > max_span.end.col {
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
            TypedNode::Accessor { target, member_span, .. } => target.span().expand(member_span),
            TypedNode::Lambda { span, .. } => span.clone(),
            TypedNode::Assignment { span, .. } => span.clone(),

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
pub const PRELUDE_ANY_TYPE_ID: TypeId = TypeId(PRELUDE_SCOPE_ID, 1);
pub const PRELUDE_INT_TYPE_ID: TypeId = TypeId(PRELUDE_SCOPE_ID, 2);
pub const PRELUDE_FLOAT_TYPE_ID: TypeId = TypeId(PRELUDE_SCOPE_ID, 3);
pub const PRELUDE_BOOL_TYPE_ID: TypeId = TypeId(PRELUDE_SCOPE_ID, 4);
pub const PRELUDE_STRING_TYPE_ID: TypeId = TypeId(PRELUDE_SCOPE_ID, 5);

pub type TypecheckError = Either<Either<LexerError, ParseError>, TypeError>;

#[derive(Debug, PartialEq)]
pub enum DestructuringMismatchKind {
    CannotDestructureAsTuple,
    InvalidTupleArity(/* actual_arity: */ usize, /* attempted_arity: */ usize),
    CannotDestructureAsArray,
}

#[derive(Debug, PartialEq)]
pub enum DuplicateNameKind {
    Variable,
    Function,
    TypeArgument,
    Type,
    Field,
    Method,
}

#[derive(Debug, PartialEq)]
pub enum ImmutableAssignmentKind {
    Parameter,
    Variable,
    Field(/* type_name: */ String),
    Method(/* type_name: */ String),
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    TypeMismatch { span: Range, expected: Vec<TypeId>, received: TypeId },
    IllegalOperator { span: Range, op: BinaryOp, left: TypeId, right: TypeId },
    UnknownType { span: Range, name: String },
    UnknownIdentifier { span: Range, token: Token },
    MissingBindingInitializer { span: Range, is_mutable: bool },
    DuplicateName { span: Range, name: String, original_span: Option<Range>, kind: DuplicateNameKind },
    ForbiddenAssignment { span: Range, type_id: TypeId, purpose: &'static str },
    DestructuringMismatch { span: Range, kind: DestructuringMismatchKind, type_id: TypeId },
    DuplicateSplat { span: Range },
    DuplicateParameter { span: Range, name: String },
    ReturnTypeMismatch { span: Range, func_name: String, expected: TypeId, received: TypeId },
    IllegalInvocation { span: Range, type_id: TypeId },
    UnexpectedArgumentName { span: Range, arg_name: String, is_instantiation: bool },
    MixedArgumentType { span: Range },
    DuplicateArgumentLabel { span: Range, name: String },
    InvalidArity { span: Range, num_possible_args: usize, num_required_args: usize, num_provided_args: usize },
    InvalidSelfParam { span: Range },
    InvalidSelfParamPosition { span: Range },
    InvalidRequiredParamPosition { span: Range, is_variadic: bool },
    InvalidVarargPosition { span: Range },
    InvalidVarargType { span: Range, type_id: TypeId },
    InvalidTypeArgumentArity { span: Range, num_required_args: usize, num_provided_args: usize },
    UnknownMember { span: Range, field_name: String, type_id: TypeId },
    MissingRequiredArgumentLabels { span: Range },
    UnknownTypeForParameter { span: Range, param_name: String },
    AssignmentToImmutable { span: Range, var_name: String, defined_span: Option<Range>, kind: ImmutableAssignmentKind },
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
            TypeError::IllegalOperator { span, .. } => span,
            TypeError::UnknownType { span, .. } |
            TypeError::UnknownIdentifier { span, .. } |
            TypeError::MissingBindingInitializer { span, .. } |
            TypeError::DuplicateName { span, .. } |
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
            TypeError::InvalidSelfParam { span } |
            TypeError::InvalidSelfParamPosition { span } |
            TypeError::InvalidRequiredParamPosition { span, .. } |
            TypeError::InvalidVarargPosition { span } |
            TypeError::InvalidVarargType { span, .. } |
            TypeError::InvalidTypeArgumentArity { span, .. } |
            TypeError::UnknownMember { span, .. } |
            TypeError::MissingRequiredArgumentLabels { span } |
            TypeError::UnknownTypeForParameter { span, .. } |
            TypeError::AssignmentToImmutable { span, .. } => span,
        };
        let lines: Vec<&str> = source.split("\n").collect();
        let cursor_line = Self::get_underlined_line(&lines, span);

        let msg = match self {
            TypeError::TypeMismatch { expected, received, .. } => {
                if *received == PRELUDE_UNIT_TYPE_ID {
                    format!(
                        "Type mismatch\n{}\n\
                        Cannot use instance of type {} as value",
                        cursor_line, project.type_repr(&PRELUDE_UNIT_TYPE_ID),
                    )
                } else {
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
            }
            TypeError::IllegalOperator { op, left, right, .. } => {
                format!(
                    "Illegal operator\n{}\n\
                    No operator '{}' exists between types {} and {}",
                    cursor_line,
                    op.repr(), project.type_repr(left), project.type_repr(right),
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
            TypeError::DuplicateName { name, original_span, kind, .. } => {
                let kind = match kind {
                    DuplicateNameKind::Variable => "name",
                    DuplicateNameKind::Function => "function",
                    DuplicateNameKind::TypeArgument => "type argument",
                    DuplicateNameKind::Type => "type",
                    DuplicateNameKind::Field => "field",
                    DuplicateNameKind::Method => "method",
                };

                let first_msg = format!("Duplicate {} '{}'\n{}", &kind, &name, cursor_line);

                let second_msg = if let Some(original_span) = original_span {
                    let pos = &original_span.start;
                    let cursor_line = Self::get_underlined_line(&lines, original_span);
                    format!("This {} is already declared at ({}:{})\n{}", kind, pos.line, pos.col, cursor_line)
                } else {
                    format!("This {} is already declared as built-in value", kind)
                };

                format!("{}\n{}", first_msg, second_msg)
            }
            TypeError::ForbiddenAssignment { type_id, purpose, .. } => {
                let type_repr = project.type_repr(type_id);

                if *type_id == PRELUDE_UNIT_TYPE_ID {
                    format!(
                        "Forbidden type for variable\n{}\n\
                        Instances of type {} cannot be used as {} values",
                        cursor_line, type_repr, purpose
                    )
                } else {
                    format!(
                        "Could not determine type\n{}\n\
                        Type {} has unbound generics. Please use an explicit type annotation to denote the type",
                        cursor_line, type_repr
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
            TypeError::UnexpectedArgumentName { arg_name, is_instantiation, .. } => {
                let second_line = if *is_instantiation {
                    format!("This constructor doesn't have a field called '{}'", arg_name)
                } else {
                    format!("This function doesn't have a parameter called '{}'", arg_name)
                };
                format!("Unexpected argument label '{}'\n{}\n{}", arg_name, cursor_line, second_line)
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
            TypeError::InvalidArity { num_possible_args, num_required_args, num_provided_args, .. } => {
                if num_provided_args < num_required_args {
                    format!(
                        "Not enough arguments for invocation\n{}\n\
                         {} argument{} required, but {} {} provided",
                        cursor_line,
                        num_required_args, if *num_required_args == 1 { "" } else { "s" },
                        num_provided_args, if *num_provided_args == 1 { "was" } else { "were" },
                    )
                } else if num_provided_args > num_possible_args {
                    format!(
                        "Too many arguments for invocation\n{}\n\
                         Expected no more than {} argument{}, but {} {} passed",
                        cursor_line,
                        num_possible_args, if *num_possible_args == 1 { "" } else { "s" },
                        num_provided_args, if *num_provided_args == 1 { "was" } else { "were" },
                    )
                } else {
                    unreachable!()
                }
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
            TypeError::InvalidRequiredParamPosition { is_variadic, .. } => {
                if *is_variadic {
                    format!(
                        "Invalid usage of variable-length parameter\n{}\n\
                        Functions with optional parameters cannot have a variadic parameter",
                        cursor_line,
                    )
                } else {
                    format!(
                        "Invalid position for required parameter\n{}\n\
                        Required parameters must all be listed before any optional parameters",
                        cursor_line,
                    )
                }
            }
            TypeError::InvalidVarargPosition { .. } => {
                format!(
                    "Invalid position for vararg parameter\n{}\n\
                    Vararg parameters must be the last in the parameter list",
                    cursor_line
                )
            }
            TypeError::InvalidVarargType { type_id, .. } => {
                format!(
                    "Invalid type for vararg parameter\n{}\n\
                    Vararg parameters must be an Array type, but got {}",
                    cursor_line, project.type_repr(type_id)
                )
            }
            TypeError::InvalidTypeArgumentArity { num_required_args, num_provided_args, .. } => {
                format!(
                    "Incorrect number of type arguments\n{}\n\
                    Expected {}, but {} {} passed",
                    cursor_line,
                    num_required_args,
                    num_provided_args, if *num_provided_args == 1 { "was" } else { "were" },
                )
            }
            TypeError::UnknownMember { field_name, type_id, .. } => {
                format!(
                    "Unknown member '{}'\n{}\n\
                    Type {} does not have a member with name '{}'",
                    field_name, cursor_line,
                    project.type_repr(type_id), field_name
                )
            }
            TypeError::MissingRequiredArgumentLabels { .. } => {
                format!(
                    "Invalid instantiation call\n{}\n\
                    Constructor functions must be called with argument labels",
                    cursor_line
                )
            }
            TypeError::UnknownTypeForParameter { param_name, .. } => {
                format!(
                    "Could not determine type for parameter '{}'\n{}\n\
                    Consider adding a type annotation",
                    param_name, cursor_line
                )
            }
            TypeError::AssignmentToImmutable { var_name, defined_span, kind, .. } => {
                let kind_name = match kind {
                    ImmutableAssignmentKind::Parameter => "parameter",
                    ImmutableAssignmentKind::Variable => "variable",
                    ImmutableAssignmentKind::Field(_) |
                    ImmutableAssignmentKind::Method(_) => "field",
                };

                let second_line = match kind {
                    ImmutableAssignmentKind::Parameter => "Function parameters are automatically declared as immutable".to_string(),
                    ImmutableAssignmentKind::Variable => "Variable is declared as immutable. Use 'var' when declaring variable to allow it to be reassigned".to_string(),
                    ImmutableAssignmentKind::Field(type_name) => format!("Field '{}' is marked readonly in type '{}'", var_name, type_name),
                    ImmutableAssignmentKind::Method(type_name) => format!("Function '{}' is a method of type '{}'", var_name, type_name),
                };
                let second_line = format!(
                    "{}{}",
                    second_line,
                    defined_span.as_ref().map(|span| format!("\n{}", Self::get_underlined_line(&lines, span))).unwrap_or("".to_string())
                );

                format!(
                    "Cannot assign to {} '{}'\n{}\n{}",
                    kind_name, var_name, cursor_line,
                    second_line
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
    current_scope_id: ScopeId,
    current_type_decl: Option<TypeId>,
    current_function: Option<FuncId>,
}

impl<'a, L: LoadModule> Typechecker2<'a, L> {
    pub fn new(module_loader: &'a L, project: &'a mut Project) -> Typechecker2<'a, L> {
        Typechecker2 { module_loader, project, current_scope_id: PRELUDE_SCOPE_ID, current_type_decl: None, current_function: None }
    }

    /* UTILITIES */

    fn current_module_mut(&mut self) -> &mut TypedModule {
        self.project.modules.last_mut().expect("Internal error: there must always be a module being typechecked")
    }

    fn current_module(&self) -> &TypedModule {
        self.project.modules.last().expect("Internal error: there must always be a module being typechecked")
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        let ScopeId(ModuleId(module_idx), scope_idx) = self.current_scope_id;
        &mut self.project.modules[module_idx].scopes[scope_idx]
    }

    fn current_scope(&self) -> &Scope {
        let ScopeId(ModuleId(module_idx), scope_idx) = self.current_scope_id;
        &self.project.modules[module_idx].scopes[scope_idx]
    }

    fn add_or_find_type_id(&mut self, ty: Type) -> TypeId {
        self.project.add_or_find_type_id(&self.current_scope_id, ty)
    }

    fn get_struct_by_name(&self, name: &String) -> Option<&Struct> {
        let current_module_id = self.current_module().id;
        self.project.find_struct_by_name(&current_module_id, name)
    }

    fn type_is_option(&self, type_id: &TypeId) -> Option<TypeId> {
        match self.project.get_type_by_id(&type_id) {
            Type::GenericInstance(struct_id, generic_ids) if *struct_id == self.project.prelude_option_struct_id => Some(generic_ids[0]),
            _ => None
        }
    }

    fn type_is_array(&self, type_id: &TypeId) -> Option<TypeId> {
        match self.project.get_type_by_id(&type_id) {
            Type::GenericInstance(struct_id, generic_ids) if *struct_id == self.project.prelude_array_struct_id => Some(generic_ids[0]),
            _ => None
        }
    }

    fn type_satisfies_other(&self, base_type: &TypeId, target_type: &TypeId) -> bool {
        let base_ty = self.project.get_type_by_id(base_type);
        let target_ty = self.project.get_type_by_id(target_type);

        match (base_ty, target_ty) {
            (_, Type::Primitive(PrimitiveType::Any)) => true,
            (Type::Generic(_, _), Type::Generic(_, _)) => base_type == target_type,
            (_, Type::Generic(_, _)) => unreachable!("Test: we shouldn't reach here because before any attempt to test types, we should substitute generics. See if this assumption is true (there will surely be a counterexample someday)"),
            (Type::Primitive(idx1), Type::Primitive(idx2)) => idx1 == idx2,
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
            (Type::Function(base_param_type_ids, base_num_req, _is_variadic, base_return_type_id), Type::Function(target_param_type_ids, _, __is_variadic, target_return_type_id)) => {
                if !self.type_satisfies_other(base_return_type_id, target_return_type_id) {
                    return false;
                }

                // Cannot assign a function to a type with fewer parameters, eg:
                //   val f: (Int) => Int = (a, b) => a + b
                // When calling `f` (eg. `f(12)`) the parameter `b` will not receive a value. And since it has no default value, this would be undefined behavior
                if *base_num_req > target_param_type_ids.len() { return false; }

                // 1. If the number of parameters is the same in both, that's ok as long as their types match, eg:
                //      val f: (Int, Int) => Int = (a: Int, b: Int) => a + b
                //      val f: (Int, Int) => Int = (a: Int, b = 12) => a + b // Even though the second parameter here is optional, its type still has to match
                // 2. If there are more parameters in the type being assigned to than in the provided type, that's ok, as long as their types match, eg:
                //      val f: (Int, Int) => Int = (a: Int) => a
                //    Values passed into `f` when calling will just be ignored since the assigned function only cares about the first argument.
                // 3. If there are fewer parameters in the type being assigned to than in the provided type, that's ok as long as the overlapping types
                //    match AND the remainder of the parameters in the provided type have default values, eg:
                //      val f: (Int) => Int = (a: Int, b = 4) => a + b
                //    The value of the `b` parameter will be its default value when the function executes.
                debug_assert!(*base_num_req <= target_param_type_ids.len());
                for (base_param_type_id, target_param_type_id) in base_param_type_ids.iter().zip(target_param_type_ids) {
                    if !self.type_satisfies_other(base_param_type_id, target_param_type_id) {
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
            (Type::Generic(_, _), _) => *var_type_id,
            (_, Type::Primitive(_)) => *var_type_id,
            (_, Type::Generic(_, _)) => *hint_type_id,
            (_, Type::GenericInstance(var_struct_id, var_generic_ids)) if *var_struct_id == self.project.prelude_option_struct_id => {
                let generic_id = var_generic_ids[0];
                let inner = self.substitute_generics(&hint_type_id, &generic_id);
                self.add_or_find_type_id(self.project.option_type(inner))
            }
            (Type::GenericInstance(hint_struct_id, hint_generic_ids), Type::GenericInstance(var_struct_id, var_generic_ids)) => {
                if var_struct_id == hint_struct_id && hint_generic_ids.len() == var_generic_ids.len() {
                    // Why: Rust can't know that I'm not mutating these refs in the substitute_generics call below :/
                    let hint_generic_ids = hint_generic_ids.clone();
                    let var_struct_id = *var_struct_id;
                    let var_generic_ids = var_generic_ids.clone();

                    let mut new_var_generic_ids = vec![];
                    for (hint_generic_type_id, var_generic_type_id) in hint_generic_ids.iter().zip(var_generic_ids.iter()) {
                        new_var_generic_ids.push(self.substitute_generics(hint_generic_type_id, var_generic_type_id));
                    }
                    self.add_or_find_type_id(Type::GenericInstance(var_struct_id, new_var_generic_ids))
                } else {
                    *var_type_id
                }
            }
            (Type::Function(hint_param_type_ids, _, _, hint_return_type_id), Type::Function(var_param_type_ids, var_num_req, var_is_variadic, var_return_type_id)) => {
                // Why: Rust can't know that I'm not mutating these refs in the substitute_generics call below :/
                let hint_param_type_ids = hint_param_type_ids.clone();
                let hint_return_type_id = *hint_return_type_id;
                let var_num_req = *var_num_req;
                let var_param_type_ids = var_param_type_ids.clone();
                let var_return_type_id = *var_return_type_id;
                let var_is_variadic = *var_is_variadic;

                // Try to substitute as much as we can.
                // If we have the same number of types in the working type and in the hint, substitute all hint params into the working type.
                // If we have more params in the working type than in the hint, we can't substitute any values from the hint so we use the remaining working type's params.
                // If we have more params in the hint type than in the working, break; we can't substitute them anywhere so we don't care about them.
                let mut param_type_ids = Vec::with_capacity(var_param_type_ids.len());
                for pair in hint_param_type_ids.iter().zip_longest(var_param_type_ids.iter()) {
                    let substituted_type_id = match pair {
                        EitherOrBoth::Both(hint_param_type_id, var_param_type_id) => self.substitute_generics(hint_param_type_id, var_param_type_id),
                        EitherOrBoth::Left(_) => break,
                        EitherOrBoth::Right(var_param_type_id) => *var_param_type_id,
                    };
                    param_type_ids.push(substituted_type_id);
                }

                let return_type_id = self.substitute_generics(&hint_return_type_id, &var_return_type_id);

                self.add_or_find_type_id(self.project.function_type(param_type_ids, var_num_req, var_is_variadic, return_type_id))
            }
            _ => *var_type_id
        }
    }

    fn extract_values_for_generics(&self, hint_type_id: &TypeId, type_id_containing_generics: &TypeId, substitutions: &mut HashMap<TypeId, TypeId>) {
        let hint_ty = self.project.get_type_by_id(hint_type_id);
        let ty_with_generics = self.project.get_type_by_id(type_id_containing_generics);

        match (ty_with_generics, hint_ty) {
            (Type::Generic(_, _), Type::Generic(_, _)) => {}
            (Type::Generic(_, _), _) => {
                if let Some(_) = substitutions.get(type_id_containing_generics) {
                    // If we already have a substitution for this generic, don't overwrite. If the known value does not align with the hint
                    // type, it should be reported by this function's caller.
                    return;
                }
                substitutions.insert(*type_id_containing_generics, *hint_type_id);
            }
            (Type::GenericInstance(s_id1, g_ids1), Type::GenericInstance(s_id2, g_ids2)) if s_id1 == s_id2 => {
                debug_assert!(g_ids1.len() == g_ids2.len());

                for (g_id1, g_id2) in g_ids1.iter().zip(g_ids2) {
                    self.extract_values_for_generics(g_id2, g_id1, substitutions);
                }
            }
            (Type::Function(ty_param_type_ids, _, _, ty_return_type_id), Type::Function(hint_param_type_ids, _, _, hint_return_type_id)) => {
                for (ty_param_type_id, hint_param_type_id) in ty_param_type_ids.iter().zip(hint_param_type_ids.iter()) {
                    self.extract_values_for_generics(hint_param_type_id, ty_param_type_id, substitutions);
                }

                self.extract_values_for_generics(hint_return_type_id, ty_return_type_id, substitutions);
            }
            _ => {}
        }
    }

    fn substitute_generics_with_known(&mut self, type_id: &TypeId, substitutions: &HashMap<TypeId, TypeId>) -> TypeId {
        let ty = self.project.get_type_by_id(&type_id);

        match ty.clone() {
            Type::Generic(_, _) => {
                substitutions.get(&type_id)
                    .map(|substituted_type_id| *substituted_type_id)
                    .unwrap_or(*type_id)
            }
            Type::GenericInstance(struct_id, generic_ids) => {
                let substituted_generic_ids = generic_ids.iter().map(|generic_type_id| self.substitute_generics_with_known(generic_type_id, substitutions)).collect();
                self.add_or_find_type_id(Type::GenericInstance(struct_id, substituted_generic_ids))
            }
            Type::Function(arg_type_ids, num_required_params, is_variadic, ret_type_id) => {
                let substituted_arg_type_ids = arg_type_ids.iter().map(|arg_type_id| self.substitute_generics_with_known(arg_type_id, substitutions)).collect();
                let substituted_ret_type_id = self.substitute_generics_with_known(&ret_type_id, substitutions);
                self.add_or_find_type_id(self.project.function_type(substituted_arg_type_ids, num_required_params, is_variadic, substituted_ret_type_id))
            }
            Type::Primitive(_) | Type::Struct(_) => *type_id,
        }
    }

    fn type_contains_generics(&self, type_id: &TypeId) -> bool {
        let ty = self.project.get_type_by_id(type_id);
        match ty {
            Type::Primitive(_) => false,
            Type::Generic(_, _) => true,
            Type::GenericInstance(_, generic_ids) => {
                for type_id in generic_ids {
                    if self.type_contains_generics(type_id) {
                        return true;
                    }
                }

                false
            }
            Type::Function(param_type_ids, _, _, return_type_id) => {
                for type_id in param_type_ids {
                    if self.type_contains_generics(type_id) {
                        return true;
                    }
                }

                self.type_contains_generics(return_type_id)
            }
            Type::Struct(struct_id) => {
                let struct_ = self.project.get_struct_by_id(struct_id);
                !struct_.generic_ids.is_empty()
            }
        }
    }

    fn resolve_type_identifier(&mut self, type_identifier: &TypeIdentifier) -> Result<TypeId, TypeError> {
        match type_identifier {
            TypeIdentifier::Normal { ident, type_args } => {
                let ident_name = Token::get_ident_name(ident);
                match ident_name.as_str() {
                    "Unit" => Ok(PRELUDE_UNIT_TYPE_ID),
                    "Any" => Ok(PRELUDE_ANY_TYPE_ID),
                    "Int" => Ok(PRELUDE_INT_TYPE_ID),
                    "Float" => Ok(PRELUDE_FLOAT_TYPE_ID),
                    "Bool" => Ok(PRELUDE_BOOL_TYPE_ID),
                    "String" => Ok(PRELUDE_STRING_TYPE_ID),
                    _ => {
                        if let Some(generic_type_id) = self.project.find_type_id_for_generic(&self.current_scope_id, &ident_name) {
                            if let Some(type_args) = type_args {
                                if let Some(first) = type_args.get(0) {
                                    let span = first.get_ident().get_range();
                                    return Err(TypeError::InvalidTypeArgumentArity { span, num_required_args: 0, num_provided_args: type_args.len() });
                                }
                            }

                            return Ok(generic_type_id);
                        }

                        let struct_id = self.get_struct_by_name(&ident_name)
                            .ok_or_else(|| TypeError::UnknownType { span: ident.get_range(), name: ident_name })?
                            .id;

                        let mut generic_ids = vec![];
                        if let Some(type_args) = type_args { // TODO: verify that struct expects to receive type arguments
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
            TypeIdentifier::Union { .. } => todo!(),
            TypeIdentifier::Func { args, ret } => {
                let num_params = args.len();
                let param_type_ids = args.iter().map(|arg| self.resolve_type_identifier(arg)).collect::<Result<Vec<_>, _>>()?;
                let ret_type_id = self.resolve_type_identifier(&*ret)?;
                let ty = self.project.function_type(param_type_ids, num_params, false, ret_type_id);
                Ok(self.add_or_find_type_id(ty))
            }
        }
    }

    fn add_variable_to_current_scope(&mut self, name: String, type_id: TypeId, is_mutable: bool, is_initialized: bool, span: &Range, is_parameter: bool) -> Result<VarId, TypeError> {
        let current_scope = self.current_scope_mut();

        for var in &current_scope.vars {
            if var.name == name {
                return Err(TypeError::DuplicateName { span: span.clone(), name, original_span: var.defined_span.clone(), kind: DuplicateNameKind::Variable });
            }
        }

        let id = VarId(current_scope.id, current_scope.vars.len());
        let var = Variable { id, name, type_id, is_mutable, is_initialized, defined_span: Some(span.clone()), is_captured: false, alias: VariableAlias::None, is_parameter };
        current_scope.vars.push(var);

        Ok(id)
    }

    fn add_function_variable_alias_to_current_scope(&mut self, ident: &Token, func_id: &FuncId) -> Result<(), TypeError> {
        let func = self.project.get_func_by_id(func_id);
        let name = Token::get_ident_name(ident);
        let span = ident.get_range();

        let fn_type_id = self.add_or_find_type_id(self.project.function_type_for_function(&func, false));
        let fn_var_id = self.add_variable_to_current_scope(name, fn_type_id, false, true, &span, false)?;
        let variable = self.project.get_var_by_id_mut(&fn_var_id);
        variable.alias = VariableAlias::Function(*func_id);

        Ok(())
    }

    fn add_function_to_current_scope(&mut self, fn_scope_id: ScopeId, name_token: &Token, generic_ids: Vec<TypeId>, has_self: bool, params: Vec<FunctionParam>, return_type_id: TypeId) -> Result<FuncId, TypeError> {
        let is_method = self.current_type_decl.is_some();
        let current_scope = self.current_scope_mut();

        let name = Token::get_ident_name(name_token);
        let span = name_token.get_range();
        for func in &current_scope.funcs {
            if func.name == name {
                let kind = if is_method { DuplicateNameKind::Method } else { DuplicateNameKind::Function };
                return Err(TypeError::DuplicateName { span, name, original_span: func.defined_span.clone(), kind });
            }
        }

        let func_id = FuncId(current_scope.id, current_scope.funcs.len());
        let func = Function { id: func_id, fn_scope_id, name: name.clone(), generic_ids, has_self, params, return_type_id, defined_span: Some(span.clone()), body: vec![], captured_vars: vec![] };
        current_scope.funcs.push(func);

        Ok(func_id)
    }

    fn new_lambda_fn_name(&self) -> String {
        let mut name = VecDeque::new();
        let mut cur_scope_id = &Some(self.current_scope_id);
        while let Some(scope_id) = cur_scope_id {
            let ScopeId(ModuleId(module_idx), scope_idx) = scope_id;
            let scope = &self.project.modules[*module_idx].scopes[*scope_idx];

            name.push_front(scope.id.1);

            cur_scope_id = &scope.parent;
        }

        let current_scope = self.current_scope();
        format!("lambda_{}_{}_{}", self.current_module().id.0, name.into_iter().join("_"), current_scope.funcs.len())
    }

    fn add_lambda_function_to_current_scope(&mut self, fn_scope_id: ScopeId, params: Vec<FunctionParam>) -> Result<FuncId, TypeError> {
        let current_scope = self.current_scope();

        let name = self.new_lambda_fn_name();
        let func_id = FuncId(current_scope.id, current_scope.funcs.len());
        let func = Function { id: func_id, fn_scope_id, name, generic_ids: vec![], has_self: false, params, return_type_id: PRELUDE_ANY_TYPE_ID, defined_span: None, body: vec![], captured_vars: vec![] };

        let current_scope = self.current_scope_mut();
        current_scope.funcs.push(func);

        Ok(func_id)
    }

    fn add_struct_to_current_module(&mut self, struct_scope_id: ScopeId, name_token: &Token, generic_ids: Vec<TypeId>) -> Result<StructId, TypeError> {
        let current_module = self.current_module();

        let name = Token::get_ident_name(name_token);
        let span = name_token.get_range();
        for struct_ in &current_module.structs {
            if struct_.name == name {
                return Err(TypeError::DuplicateName { span, name, original_span: struct_.defined_span.clone(), kind: DuplicateNameKind::Type });
            }
        }

        let struct_id = StructId(current_module.id, current_module.structs.len());
        let self_type_id = self.add_or_find_type_id(Type::GenericInstance(struct_id, generic_ids.clone()));

        let struct_ = Struct {
            id: struct_id,
            struct_scope_id,
            name: name.clone(),
            defined_span: Some(name_token.get_range()),
            generic_ids,
            self_type_id,
            fields: vec![],
            methods: vec![],
            static_methods: vec![],
        };
        self.current_module_mut().structs.push(struct_);

        let struct_type_id = self.add_or_find_type_id(self.project.struct_type(struct_id));
        let struct_var_id = self.add_variable_to_current_scope(name, struct_type_id, false, true, &span, false)?;
        let variable = self.project.get_var_by_id_mut(&struct_var_id);
        variable.alias = VariableAlias::Struct(struct_id);

        Ok(struct_id)
    }

    fn create_child_scope<S: AsRef<str>>(&mut self, label: S) -> ScopeId {
        let parent_scope = self.current_scope_id;
        let current_module = self.current_module_mut();
        let new_scope_id = ScopeId(current_module.id, current_module.scopes.len());

        let child_scope = Scope {
            label: label.as_ref().to_string(),
            id: new_scope_id,
            parent: Some(parent_scope),
            types: vec![],
            vars: vec![],
            funcs: vec![],
        };
        current_module.scopes.push(child_scope);

        new_scope_id
    }

    fn begin_child_scope<S: AsRef<str>>(&mut self, label: S) -> ScopeId {
        self.current_scope_id = self.create_child_scope(label);

        self.current_scope_id
    }

    fn end_child_scope(&mut self) -> ScopeId {
        let Some(parent) = self.current_scope().parent else { unreachable!("Internal error: a child scope must always have a parent") };
        self.current_scope_id = parent;

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
        let mut prelude_scope = Scope { label: "prelude.root".to_string(), id: PRELUDE_SCOPE_ID, parent: None, types: vec![], vars: vec![], funcs: vec![] };

        let primitives = [
            (PRELUDE_UNIT_TYPE_ID, PrimitiveType::Unit),
            (PRELUDE_ANY_TYPE_ID, PrimitiveType::Any),
            (PRELUDE_INT_TYPE_ID, PrimitiveType::Int),
            (PRELUDE_FLOAT_TYPE_ID, PrimitiveType::Float),
            (PRELUDE_BOOL_TYPE_ID, PrimitiveType::Bool),
            (PRELUDE_STRING_TYPE_ID, PrimitiveType::String)
        ];
        for (type_id, primitive_type) in primitives {
            prelude_scope.types.push(Type::Primitive(primitive_type));
            prelude_module.type_ids.push(type_id);
        }

        prelude_module.scopes.push(prelude_scope);
        self.project.modules.push(prelude_module);

        // Define `Tuple<T...>` struct
        // (There's no way of representing variadic generics like how tuple works, and it's not something that's necessary to add, so let's do a bit of hand-waving here)
        {
            let prelude_module = &self.project.modules[PRELUDE_MODULE_ID.0];
            let tuple_struct_id = StructId(PRELUDE_MODULE_ID, prelude_module.structs.len());
            let self_type_id = self.project.add_type_id(&PRELUDE_SCOPE_ID, Type::GenericInstance(tuple_struct_id, vec![]));
            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            prelude_module.structs.push(Struct { id: tuple_struct_id, self_type_id, struct_scope_id: PRELUDE_SCOPE_ID, name: "Tuple".to_string(), defined_span: None, generic_ids: vec![], fields: vec![], methods: vec![], static_methods: vec![] });
            self.project.prelude_tuple_struct_id = tuple_struct_id;
        }
        // Define `Option<T>` struct
        {
            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let scope_id = ScopeId(PRELUDE_MODULE_ID, prelude_module.scopes.len());
            let scope = Scope { label: "prelude.Option".to_string(), id: scope_id, parent: Some(PRELUDE_SCOPE_ID), types: vec![], vars: vec![], funcs: vec![] };
            prelude_module.scopes.push(scope);
            let generic_t_type_id = self.project.add_type_id(&scope_id, Type::Generic(None, "T".to_string()));

            let prelude_module = &self.project.modules[PRELUDE_MODULE_ID.0];
            let option_struct_id = StructId(PRELUDE_MODULE_ID, prelude_module.structs.len());
            let self_type_id = self.project.add_type_id(&PRELUDE_SCOPE_ID, Type::GenericInstance(option_struct_id, vec![generic_t_type_id]));
            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            prelude_module.structs.push(Struct { id: option_struct_id, self_type_id, struct_scope_id: scope_id, name: "Option".to_string(), defined_span: None, generic_ids: vec![generic_t_type_id], fields: vec![], methods: vec![], static_methods: vec![] });
            self.project.prelude_option_struct_id = option_struct_id;
        }
        // Define `None` builtin, which is of type `T?`
        {
            let prelude_module = &mut self.project.modules[PRELUDE_MODULE_ID.0];
            let scope_id = ScopeId(PRELUDE_MODULE_ID, prelude_module.scopes.len());
            let scope = Scope { label: "prelude.None".to_string(), id: scope_id, parent: Some(PRELUDE_SCOPE_ID), types: vec![], vars: vec![], funcs: vec![] };
            prelude_module.scopes.push(scope);
            let generic_t_type_id = self.project.add_type_id(&scope_id, Type::Generic(None, "T".to_string()));

            self.project.prelude_none_type_id = self.project.add_type_id(&PRELUDE_SCOPE_ID, self.project.option_type(generic_t_type_id));
        }

        self.current_scope_id = PRELUDE_SCOPE_ID;

        let prelude_stub_file = include_str!("prelude.stub.abra");
        let parse_result = tokenize_and_parse_stub(&parser::ast::ModuleId::External("prelude".to_string()), &prelude_stub_file.to_string())
            .expect("There should not be a problem parsing the prelude file");
        self.typecheck_block(parse_result.nodes)
            .expect("There should not be a problem typechecking the prelude file");
        debug_assert_ne!(self.project.prelude_none_type_id, PLACEHOLDER_TYPE_ID);
        debug_assert_ne!(self.project.prelude_int_struct_id, PLACEHOLDER_STRUCT_ID);
        debug_assert_ne!(self.project.prelude_float_struct_id, PLACEHOLDER_STRUCT_ID);
        debug_assert_ne!(self.project.prelude_bool_struct_id, PLACEHOLDER_STRUCT_ID);
        debug_assert_ne!(self.project.prelude_string_struct_id, PLACEHOLDER_STRUCT_ID);
        debug_assert_ne!(self.project.prelude_array_struct_id, PLACEHOLDER_STRUCT_ID);
        debug_assert_ne!(self.project.prelude_set_struct_id, PLACEHOLDER_STRUCT_ID);
        debug_assert_ne!(self.project.prelude_map_struct_id, PLACEHOLDER_STRUCT_ID);
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

        self.current_scope_id = scope_id;

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

        let num_type_decls = type_decls.len();
        let mut struct_ids = Vec::with_capacity(num_type_decls);
        for node in &type_decls {
            let struct_id = self.typecheck_struct_pass_0(node)?;
            if self.current_scope_id == PRELUDE_SCOPE_ID {
                let struct_ = self.project.get_struct_by_id(&struct_id);
                if struct_.name == "Int" {
                    self.project.prelude_int_struct_id = struct_.id;
                } else if struct_.name == "Float" {
                    self.project.prelude_float_struct_id = struct_.id;
                } else if struct_.name == "Bool" {
                    self.project.prelude_bool_struct_id = struct_.id;
                } else if struct_.name == "String" {
                    self.project.prelude_string_struct_id = struct_.id;
                } else if struct_.name == "Array" {
                    self.project.prelude_array_struct_id = struct_.id;
                } else if struct_.name == "Set" {
                    self.project.prelude_set_struct_id = struct_.id;
                } else if struct_.name == "Map" {
                    self.project.prelude_map_struct_id = struct_.id;
                }
            }
            struct_ids.push(struct_id);
        }

        let mut structs = VecDeque::with_capacity(num_type_decls);
        for (node, struct_id) in type_decls.iter().zip(struct_ids) {
            let method_func_ids = self.typecheck_struct_pass_1(node, &struct_id)?;
            structs.push_back((struct_id, method_func_ids));
        }

        let mut func_ids = VecDeque::with_capacity(num_type_decls);
        for node in func_decls {
            let func_id = self.typecheck_function_pass_1(node, false)?;
            self.add_function_variable_alias_to_current_scope(&node.name, &func_id)?;
            func_ids.push_back(func_id);
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

    fn add_generics_to_scope(&mut self, scope_id: &ScopeId, type_args: &Vec<Token>) -> Result<Vec<TypeId>, TypeError> {
        let mut generic_ids = Vec::with_capacity(type_args.len());
        for generic_ident in type_args {
            let generic_name = Token::get_ident_name(generic_ident);
            let possible_match = self.project
                .find_type_id_for_generic(&scope_id, &generic_name)
                .map(|type_id| self.project.get_type_by_id(&type_id));
            if let Some(ty) = possible_match {
                let span = generic_ident.get_range();
                let Type::Generic(original_span, name) = ty.clone() else { unreachable!("We know it's a generic since it was identified as such") };
                return Err(TypeError::DuplicateName { span, name: name.clone(), original_span, kind: DuplicateNameKind::TypeArgument });
            }

            let generic_id = self.project.add_or_find_type_id(&scope_id, Type::Generic(Some(generic_ident.get_range()), generic_name));
            generic_ids.push(generic_id);
        }

        Ok(generic_ids)
    }

    fn typecheck_function_parameters(&mut self, allow_self: bool, parameters: &Vec<(Parameter, Option<TypeId>)>) -> Result<Vec<FunctionParam>, TypeError> {
        let mut seen_self = false;
        let mut seen_param_names = HashSet::new();
        let mut seen_default_valued_params = false;
        let mut params = vec![];
        let num_params = parameters.len();
        for (idx, (Parameter { ident, type_ident, is_vararg, default_value }, type_hint)) in parameters.iter().enumerate() {
            if *is_vararg && idx != num_params - 1 {
                return Err(TypeError::InvalidVarargPosition { span: ident.get_range() });
            }

            let param_name = Token::get_ident_name(ident);

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
                    name: param_name,
                    type_id: *self_type_id,
                    defined_span: Some(ident.get_range()),
                    default_value: None,
                    is_variadic: false,
                });

                continue;
            }

            if seen_param_names.contains(&param_name) {
                return Err(TypeError::DuplicateParameter { span: ident.get_range(), name: param_name });
            }
            seen_param_names.insert(param_name.clone());

            let mut param_type_id = None;
            if let Some(type_ident) = type_ident {
                param_type_id = Some(self.resolve_type_identifier(type_ident)?);
            }
            if let Some(type_hint) = type_hint {
                if let Some(param_type_id) = param_type_id {
                    if !self.type_satisfies_other(&param_type_id, type_hint) {
                        return Err(TypeError::TypeMismatch { span: ident.get_range(), expected: vec![*type_hint], received: param_type_id });
                    }
                } else {
                    param_type_id = Some(*type_hint);
                }
            }
            let typed_default_value_expr = if let Some(default_value) = default_value {
                seen_default_valued_params = true;
                // TODO: Handling retries for first-pass function declaration typechecking with default-value expressions which reference code that hasn't been visited yet
                // For example:
                //   func foo(bar = baz()) = ...
                //   func baz() = ...
                // This would fail because `baz` doesn't yet exist when typechecking `foo`.
                Some(self.typecheck_expression(default_value.clone(), param_type_id)?)
            } else {
                if seen_default_valued_params {
                    return Err(TypeError::InvalidRequiredParamPosition { span: ident.get_range(), is_variadic: *is_vararg });
                }
                None
            };
            let mut type_id = match (param_type_id, &typed_default_value_expr) {
                (None, None) => return Err(TypeError::UnknownTypeForParameter { span: ident.get_range(), param_name }),
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
            if *is_vararg {
                let Some(inner_type_id) = self.type_is_array(&type_id) else {
                    return Err(TypeError::InvalidVarargType { span: ident.get_range(), type_id });
                };
                type_id = inner_type_id;
            }

            params.push(FunctionParam {
                name: param_name,
                type_id,
                defined_span: Some(ident.get_range()),
                default_value: typed_default_value_expr,
                is_variadic: *is_vararg,
            });
        }

        Ok(params)
    }

    fn typecheck_function_pass_1(&mut self, node: &FunctionDeclNode, allow_self: bool) -> Result<FuncId, TypeError> {
        let FunctionDeclNode { export_token, name, type_args, ret_type, .. } = node;

        if export_token.is_some() { unimplemented!("Internal error: imports/exports") }

        let fn_scope_id = self.begin_child_scope(format!("{:?}.{}", &self.current_module().id, Token::get_ident_name(&node.name)));

        let generic_ids = self.add_generics_to_scope(&fn_scope_id, type_args)?;
        let node_parameters = node.parameters().into_iter().map(|p| (p, None)).collect();
        let params = self.typecheck_function_parameters(allow_self, &node_parameters)?;

        let mut return_type_id = PRELUDE_UNIT_TYPE_ID;
        if let Some(ret_type) = ret_type {
            return_type_id = self.resolve_type_identifier(ret_type)?;
        }

        self.end_child_scope();

        let has_self = params.first().map(|p| p.name == "self").unwrap_or(false);
        let func_id = self.add_function_to_current_scope(fn_scope_id, name, generic_ids, has_self, params, return_type_id)?;
        self.current_module_mut().functions.push(func_id);

        Ok(func_id)
    }

    fn typecheck_function_pass_2(&mut self, func_id: FuncId, node: FunctionDeclNode) -> Result<(), TypeError> {
        let prev_func_id = self.current_function;
        self.current_function = Some(func_id);

        let func = self.project.get_func_by_id(&func_id);
        let prev_scope_id = self.current_scope_id;
        self.current_scope_id = func.fn_scope_id;

        let func_name = func.name.clone();
        let return_type_id = func.return_type_id;
        // Why: rust cannot guarantee that `add_variable_to_current_scope` won't modify `func`; intermediate variable solves
        let params = func.params.iter().map(|p| (p.name.clone(), p.type_id, p.defined_span.clone())).collect_vec();
        for (name, type_id, defined_span, ..) in params {
            let Some(defined_span) = defined_span else { unreachable!("Internal error: when typechecking a user-defined function, parameters' spans will always be known"); };
            let defined_span = defined_span.clone();

            self.add_variable_to_current_scope(name, type_id, false, true, &defined_span, true)?;
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

        self.current_function = prev_func_id;
        self.current_scope_id = prev_scope_id;

        Ok(())
    }

    fn typecheck_struct_pass_0(&mut self, node: &TypeDeclNode) -> Result<StructId, TypeError> {
        let TypeDeclNode { export_token, name, type_args, .. } = node;

        if export_token.is_some() { unimplemented!("Internal error: imports/exports") }

        let struct_scope_id = self.create_child_scope(format!("{:?}.{}", &self.current_module().id, Token::get_ident_name(&name)));

        // A struct's generics are scoped to the struct declaration, but the instance type should be scoped to the outer scope.
        let generic_ids = self.add_generics_to_scope(&struct_scope_id, type_args)?;
        let struct_id = self.add_struct_to_current_module(struct_scope_id, name, generic_ids)?;
        debug_assert!(self.current_type_decl.is_none(), "At the moment, types cannot be nested within other types");

        Ok(struct_id)
    }

    fn typecheck_struct_pass_1(&mut self, node: &TypeDeclNode, struct_id: &StructId) -> Result<Vec<FuncId>, TypeError> {
        let TypeDeclNode { methods, .. } = node;

        let struct_ = self.project.get_struct_by_id(struct_id);

        self.current_scope_id = struct_.struct_scope_id;
        self.current_type_decl = Some(struct_.self_type_id);

        let mut method_func_ids = vec![];
        for method in methods {
            let AstNode::FunctionDecl(_, decl_node) = method else { unreachable!("Internal error: a type's methods must be of type AstNode::FunctionDecl") };
            let func_id = self.typecheck_function_pass_1(decl_node, true)?;
            method_func_ids.push(func_id);
        }

        self.current_type_decl = None;

        self.end_child_scope();

        Ok(method_func_ids)
    }

    fn typecheck_struct_pass_2(&mut self, struct_id: StructId, method_func_ids: Vec<FuncId>, node: TypeDeclNode) -> Result<(), TypeError> {
        let TypeDeclNode { fields, methods, .. } = node;

        let struct_ = self.project.get_struct_by_id(&struct_id);

        let prev_scope_id = self.current_scope_id;
        self.current_scope_id = struct_.struct_scope_id;

        let mut seen_fields: HashMap<String, Token> = HashMap::new();
        for TypeDeclField { ident, type_ident, default_value, readonly } in fields {
            if default_value.is_some() { unimplemented!("Internal error: field default values") }
            let is_readonly = readonly.is_some();

            let field_name = Token::get_ident_name(&ident);
            if let Some(orig_field) = seen_fields.get(&field_name) {
                return Err(TypeError::DuplicateName { span: ident.get_range(), name: field_name, original_span: Some(orig_field.get_range()), kind: DuplicateNameKind::Field });
            }
            seen_fields.insert(field_name.clone(), ident.clone());

            let field_type_id = self.resolve_type_identifier(&type_ident)?;
            let field = StructField {
                name: field_name,
                type_id: field_type_id,
                defined_span: ident.get_range(),
                is_readonly,
            };
            self.project.get_struct_by_id_mut(&struct_id).fields.push(field);
        }

        debug_assert!(methods.len() == method_func_ids.len(), "There should be a FuncId for each method (by pass 1)");
        for (func_id, method) in method_func_ids.iter().zip(methods.into_iter()) {
            let AstNode::FunctionDecl(_, decl_node) = method else { unreachable!("Internal error: a type's methods must be of type AstNode::FunctionDecl") };
            let is_method = decl_node.args.get(0).map(|(token, _, _, _)| if let Token::Self_(_) = token { true } else { false }).unwrap_or(false);

            self.typecheck_function_pass_2(*func_id, decl_node)?;

            if is_method {
                self.project.get_struct_by_id_mut(&struct_id).methods.push(*func_id);
            } else {
                self.project.get_struct_by_id_mut(&struct_id).static_methods.push(*func_id);
            }
        }

        self.current_scope_id = prev_scope_id;

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
                        if *type_id == PRELUDE_UNIT_TYPE_ID || self.type_contains_generics(type_id) {
                            return Err(TypeError::ForbiddenAssignment { span: typed_expr.span(), type_id: *type_id, purpose: "assignment" });
                        }
                        self.typecheck_binding_pattern(is_mutable, true, &binding, &type_id, &mut var_ids)?;

                        Some(Box::new(typed_expr))
                    }
                    (Some(type_hint_id), Some(expr)) => {
                        let typed_expr = self.typecheck_expression(*expr, Some(type_hint_id))?;
                        let mut type_id = *typed_expr.type_id();

                        if self.type_contains_generics(&type_id) {
                            type_id = self.substitute_generics(&type_hint_id, &type_id);
                        }
                        if !self.type_satisfies_other(&type_id, &type_hint_id) {
                            let span = typed_expr.span();
                            return Err(TypeError::TypeMismatch { span, expected: vec![type_hint_id], received: type_id });
                        };
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

                let var_id = self.add_variable_to_current_scope(var_name, *type_id, is_mutable, is_initialized, &var_token.get_range(), false)?;
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
            AstNode::Binary(_, n) => {
                let BinaryNode { op, left, right } = n;
                let typed_left = self.typecheck_expression(*left, None)?;
                let typed_right = self.typecheck_expression(*right, None)?;
                let l_type_id = typed_left.type_id();
                let r_type_id = typed_right.type_id();

                let type_id = match &op {
                    BinaryOp::Add => match (*l_type_id, *r_type_id) {
                        (PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID) => PRELUDE_INT_TYPE_ID,
                        (PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) => PRELUDE_FLOAT_TYPE_ID,
                        (_, PRELUDE_STRING_TYPE_ID) | (PRELUDE_STRING_TYPE_ID, _) => PRELUDE_STRING_TYPE_ID,
                        (left, right) => {
                            let span = typed_left.span().expand(&typed_right.span());
                            return Err(TypeError::IllegalOperator { span, op, left, right });
                        }
                    }
                    BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Mod => match (*l_type_id, *r_type_id) {
                        (PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID) => PRELUDE_INT_TYPE_ID,
                        (PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) => PRELUDE_FLOAT_TYPE_ID,
                        (left, right) => {
                            let span = typed_left.span().expand(&typed_right.span());
                            return Err(TypeError::IllegalOperator { span, op, left, right });
                        }
                    }
                    BinaryOp::Div | BinaryOp::Pow => match (*l_type_id, *r_type_id) {
                        (PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID) |
                        (PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) => PRELUDE_FLOAT_TYPE_ID,
                        (left, right) => {
                            let span = typed_left.span().expand(&typed_right.span());
                            return Err(TypeError::IllegalOperator { span, op, left, right });
                        }
                    }
                    BinaryOp::Coalesce => {
                        let Some(mut inner) = self.type_is_option(&l_type_id) else {
                            // SHORT-CIRCUIT: If the LHS is not an Option, we can just return the LHS here.
                            return Ok(typed_left);
                        };

                        if self.type_contains_generics(&inner) {
                            inner = self.substitute_generics(r_type_id, &inner);
                        }
                        if !self.type_satisfies_other(r_type_id, &inner) {
                            return Err(TypeError::TypeMismatch { span: typed_right.span(), expected: vec![inner], received: *r_type_id });
                        }
                        *r_type_id
                    }
                    BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => match (*l_type_id, *r_type_id) {
                        (PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID) |
                        (PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) |
                        (PRELUDE_STRING_TYPE_ID, PRELUDE_STRING_TYPE_ID) => PRELUDE_BOOL_TYPE_ID,
                        (left, right) => {
                            let span = typed_left.span().expand(&typed_right.span());
                            return Err(TypeError::IllegalOperator { span, op, left, right });
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Neq => PRELUDE_BOOL_TYPE_ID,

                    // Boolean operators
                    BinaryOp::And | BinaryOp::Or | BinaryOp::Xor => match (*l_type_id, *r_type_id) {
                        (PRELUDE_BOOL_TYPE_ID, PRELUDE_BOOL_TYPE_ID) => PRELUDE_BOOL_TYPE_ID,
                        (left, right) => {
                            let span = typed_left.span().expand(&typed_right.span());
                            return Err(TypeError::IllegalOperator { span, op, left, right });
                        }
                    }

                    // Assignment operators
                    BinaryOp::AddEq |
                    BinaryOp::SubEq |
                    BinaryOp::MulEq |
                    BinaryOp::DivEq |
                    BinaryOp::ModEq |
                    BinaryOp::AndEq |
                    BinaryOp::OrEq |
                    BinaryOp::CoalesceEq => todo!()
                };

                Ok(TypedNode::Binary { op, left: Box::new(typed_left), right: Box::new(typed_right), type_id })
            }
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
                        let inner_type_id = array_struct.generic_ids[0];
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
                        let inner_type_id = set_struct.generic_ids[0];
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
                        let map_generics = &map_struct.generic_ids;
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
            AstNode::Identifier(token, type_args) => {
                if let Token::None(_) = &token {
                    let mut type_id = self.project.prelude_none_type_id;
                    if let Some(type_hint) = type_hint {
                        type_id = self.substitute_generics(&type_hint, &type_id);
                    }

                    return Ok(TypedNode::NoneValue { token, type_id });
                }

                let name = Token::get_ident_name(&token);
                let variable = self.project.find_variable_by_name(&self.current_scope_id, &name);
                let Some(Variable { id, type_id, .. }) = variable else {
                    return Err(TypeError::UnknownIdentifier { span: token.get_range(), token });
                };
                let var_id = *id;
                let mut var_type_id = *type_id;

                if let Some(type_hint) = type_hint {
                    var_type_id = self.substitute_generics(&type_hint, &var_type_id);
                }

                let mut type_arg_ids = Vec::with_capacity(type_args.as_ref().map(|args| args.len()).unwrap_or(0));
                if let Some(type_args) = type_args {
                    for type_arg in type_args {
                        let type_id = self.resolve_type_identifier(&type_arg)?;
                        type_arg_ids.push((type_id, type_arg.get_ident().get_range()));
                    }
                }

                // Track closed-over variables for current function
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

                Ok(TypedNode::Identifier { token, var_id, type_arg_ids, type_id: var_type_id })
            }
            AstNode::Assignment(_, n) => {
                let AssignmentNode { target, expr } = n;

                let typed_target = self.typecheck_expression(*target, type_hint)?;
                let target_span = typed_target.span();
                let target_type_id = *typed_target.type_id();
                let typed_expr = self.typecheck_expression(*expr, Some(target_type_id))?;

                let kind = match typed_target {
                    TypedNode::Identifier { var_id, .. } => {
                        let variable = self.project.get_var_by_id(&var_id);
                        if !variable.is_mutable {
                            let kind = if variable.is_parameter { ImmutableAssignmentKind::Parameter } else { ImmutableAssignmentKind::Variable };
                            return Err(TypeError::AssignmentToImmutable { span: typed_target.span(), var_name: variable.name.clone(), defined_span: variable.defined_span.clone(), kind });
                        }

                        AssignmentKind::Identifier { var_id }
                    }
                    TypedNode::Accessor { target, kind, member_idx, .. } => {
                        let (struct_, _) = self.project.get_struct_by_type_id(target.type_id()).expect("Internal error: This should have been caught when typechecking the Accessor");

                        match kind {
                            AccessorKind::Field => {
                                let field = &struct_.fields[member_idx];
                                if field.is_readonly {
                                    let type_name = struct_.name.clone();
                                    return Err(TypeError::AssignmentToImmutable { span: target_span, var_name: field.name.clone(), defined_span: Some(field.defined_span.clone()), kind: ImmutableAssignmentKind::Field(type_name) });
                                }
                            }
                            AccessorKind::Method => {
                                let type_name = struct_.name.clone();
                                let func_id = &struct_.methods[member_idx];
                                let function = self.project.get_func_by_id(func_id);
                                return Err(TypeError::AssignmentToImmutable { span: target_span, var_name: function.name.clone(), defined_span: function.defined_span.clone(), kind: ImmutableAssignmentKind::Method(type_name) });
                            }
                            AccessorKind::StaticMethod => todo!()
                        };

                        AssignmentKind::Accessor { target, kind, member_idx }
                    }
                    _ => todo!()
                };

                let mut type_id = *typed_expr.type_id();
                if self.type_contains_generics(&type_id) {
                    type_id = self.substitute_generics(&target_type_id, &type_id);
                }
                if !self.type_satisfies_other(&type_id, &target_type_id) {
                    return Err(TypeError::TypeMismatch { span: typed_expr.span(), expected: vec![target_type_id], received: type_id });
                }

                let span = target_span.expand(&typed_expr.span());
                Ok(TypedNode::Assignment { span, kind, type_id: target_type_id })
            }
            AstNode::Indexing(_, _) => todo!(),
            AstNode::Accessor(_, n) => {
                if n.is_opt_safe { unimplemented!("Internal error: option-safe accessor") }

                let AstNode::Identifier(field_ident, _) = *n.field else { unreachable!("Internal error: an accessor's `field` must be an identifier") };
                let field_name = Token::get_ident_name(&field_ident);

                let typed_target = self.typecheck_expression(*n.target, None)?;
                let target_type_id = typed_target.type_id();

                let mut field_data = None;
                let Some((struct_, generic_substitutions)) = self.project.get_struct_by_type_id(target_type_id) else {
                    return Err(TypeError::UnknownMember { span: field_ident.get_range(), field_name, type_id: *target_type_id });
                };
                let fields = struct_.fields.clone();
                let methods = struct_.methods.clone();
                for (idx, field) in fields.iter().enumerate() {
                    if *field.name == field_name {
                        let type_id = self.substitute_generics_with_known(&field.type_id, &generic_substitutions);
                        field_data = Some((idx, type_id, AccessorKind::Field));
                        break;
                    }
                }
                if field_data.is_none() {
                    for (idx, func_id) in methods.iter().enumerate() {
                        let func = self.project.get_func_by_id(func_id);
                        if func.name == field_name {
                            let mut func_type_id = self.add_or_find_type_id(self.project.function_type_for_function(&func, true));
                            func_type_id = self.substitute_generics_with_known(&func_type_id, &generic_substitutions);
                            field_data = Some((idx, func_type_id, AccessorKind::Method));
                            break;
                        }
                    }
                }

                let Some((field_idx, type_id, kind)) = field_data else {
                    return Err(TypeError::UnknownMember { span: field_ident.get_range(), field_name, type_id: *target_type_id });
                };

                Ok(TypedNode::Accessor {
                    target: Box::new(typed_target),
                    kind,
                    member_idx: field_idx,
                    member_span: field_ident.get_range(),
                    type_id,
                })
            }
            AstNode::Invocation(token, n) => {
                let InvocationNode { target, args } = n;

                let typed_target = self.typecheck_expression(*target, None)?;

                let mut filled_in_generic_types = HashMap::new();

                let params_data;
                let mut return_type_id;
                let mut is_instantiation = false;
                let mut fn_is_variadic = false;
                let mut forbid_labels = false;
                match &typed_target {
                    TypedNode::Identifier { var_id, type_arg_ids: type_args, .. } if self.project.get_var_by_id(var_id).alias != VariableAlias::None => {
                        let var = self.project.get_var_by_id(var_id);

                        let generic_ids;
                        match var.alias {
                            VariableAlias::Function(alias_func_id) => {
                                let function = self.project.get_func_by_id(&alias_func_id);
                                fn_is_variadic = function.is_variadic();
                                generic_ids = &function.generic_ids;
                                params_data = function.params.iter().enumerate().map(|(idx, p)| (idx, p.name.clone(), p.type_id, p.default_value.is_some(), p.is_variadic)).collect_vec();
                                return_type_id = function.return_type_id;
                            }
                            VariableAlias::Struct(alias_struct_id) => {
                                let struct_ = self.project.get_struct_by_id(&alias_struct_id);
                                // TODO: Struct fields default values
                                generic_ids = &struct_.generic_ids;
                                params_data = struct_.fields.iter().enumerate().map(|(idx, f)| (idx, f.name.clone(), f.type_id, false, false)).collect_vec();
                                return_type_id = struct_.self_type_id;
                                is_instantiation = true;
                            }
                            VariableAlias::None => unreachable!("VariableAlias::None identifiers are excluded from this match case and are handled below"),
                        }
                        if !type_args.is_empty() && type_args.len() != generic_ids.len() {
                            let span = if type_args.len() > generic_ids.len() {
                                let (_, span) = &type_args[generic_ids.len()];
                                span.clone()
                            } else {
                                typed_target.span()
                            };
                            return Err(TypeError::InvalidTypeArgumentArity { span, num_required_args: generic_ids.len(), num_provided_args: type_args.len() });
                        }
                        for (generic_id, (type_arg_id, _)) in generic_ids.iter().zip(type_args.iter()) {
                            filled_in_generic_types.insert(*generic_id, *type_arg_id);
                        }
                    }
                    TypedNode::Accessor { target, kind, member_idx, .. } => {
                        let target_type_id = target.type_id();
                        let target_ty = self.project.get_type_by_id(target_type_id);
                        match target_ty {
                            Type::GenericInstance(struct_id, generic_ids) => {
                                let struct_ = self.project.get_struct_by_id(struct_id);
                                match kind {
                                    AccessorKind::Field => {
                                        let field_ty = self.project.get_type_by_id(&struct_.fields[*member_idx].type_id);
                                        let Type::Function(param_type_ids, num_required_args, is_variadic, ret_type_id) = field_ty else {
                                            return Err(TypeError::IllegalInvocation { span: typed_target.span(), type_id: *target_type_id });
                                        };
                                        fn_is_variadic = *is_variadic;
                                        let num_param_type_ids = param_type_ids.len();
                                        params_data = param_type_ids.iter().enumerate()
                                            .map(|(idx, param_type_id)| {
                                                let param_is_variadic = *is_variadic && idx == num_param_type_ids - 1;
                                                (idx, format!("_{}", idx), *param_type_id, idx >= *num_required_args, param_is_variadic)
                                            })
                                            .collect_vec();
                                        return_type_id = *ret_type_id;
                                        forbid_labels = true;
                                    }
                                    AccessorKind::Method => {
                                        let function = self.project.get_func_by_id(&struct_.methods[*member_idx]);
                                        fn_is_variadic = function.is_variadic();
                                        params_data = function.params.iter().skip(1).enumerate().map(|(idx, p)| (idx, p.name.clone(), p.type_id, p.default_value.is_some(), p.is_variadic)).collect_vec();
                                        return_type_id = function.return_type_id;
                                    }
                                    AccessorKind::StaticMethod => todo!()
                                }

                                for (generic_id, type_arg_id) in struct_.generic_ids.iter().zip(generic_ids.iter()) {
                                    filled_in_generic_types.insert(*generic_id, *type_arg_id);
                                }
                            }
                            Type::Primitive(primitive_type) => {
                                let struct_id = match primitive_type {
                                    PrimitiveType::Unit | PrimitiveType::Any => unreachable!("Internal error: accessor of these primitives should have been caught already"),
                                    PrimitiveType::Int => &self.project.prelude_int_struct_id,
                                    PrimitiveType::Float => &self.project.prelude_float_struct_id,
                                    PrimitiveType::Bool => &self.project.prelude_bool_struct_id,
                                    PrimitiveType::String => &self.project.prelude_string_struct_id,
                                };
                                let struct_ = self.project.get_struct_by_id(&struct_id);
                                let function = self.project.get_func_by_id(&struct_.methods[*member_idx]);
                                fn_is_variadic = function.is_variadic();
                                params_data = function.params.iter().skip(1).enumerate().map(|(idx, p)| (idx, p.name.clone(), p.type_id, p.default_value.is_some(), p.is_variadic)).collect_vec();
                                return_type_id = function.return_type_id;
                            }
                            Type::Generic(_, _) |
                            Type::Function(_, _, _, _) |
                            Type::Struct(_) => todo!()
                        }
                    }
                    _ => {
                        let target_type_id = typed_target.type_id();
                        let target_ty = self.project.get_type_by_id(target_type_id);
                        let Type::Function(param_type_ids, num_required_args, is_variadic, ret_type_id) = target_ty else {
                            return Err(TypeError::IllegalInvocation { span: typed_target.span(), type_id: *target_type_id });
                        };
                        fn_is_variadic = *is_variadic;
                        let num_param_type_ids = param_type_ids.len();
                        params_data = param_type_ids.iter().enumerate()
                            .map(|(idx, param_type_id)| {
                                let param_is_variadic = *is_variadic && idx == num_param_type_ids - 1;
                                (idx, format!("_{}", idx), *param_type_id, idx >= *num_required_args, param_is_variadic)
                            })
                            .collect_vec();
                        return_type_id = *ret_type_id;
                        forbid_labels = true;
                    }
                }

                let num_possible_args = params_data.len();
                let num_required_args = params_data.iter().filter(|(_, _, _, is_optional, _)| !*is_optional).count();
                let num_provided_args = args.len();

                let mut seen_labels = HashSet::new();
                let mut typed_arguments = (0..params_data.len()).map(|_| None).collect_vec();
                let mut variadic_arguments = Vec::new();
                for (idx, (label, arg_node)) in args.into_iter().enumerate() {
                    if is_instantiation && label.is_none() && !forbid_labels {
                        return Err(TypeError::MissingRequiredArgumentLabels { span: arg_node.get_token().get_range() });
                    }

                    let param_idx;
                    let mut param_type_id;
                    let gather_variadic_arguments;
                    if let Some(label) = label {
                        let label_name = Token::get_ident_name(&label);

                        if forbid_labels {
                            debug_assert!(!is_instantiation, "We should always require labels if we're instantiating");
                            return Err(TypeError::UnexpectedArgumentName { span: label.get_range(), arg_name: label_name, is_instantiation });
                        }

                        if idx > 0 && seen_labels.is_empty() {
                            return Err(TypeError::MixedArgumentType { span: label.get_range() });
                        }

                        if seen_labels.contains(&label_name) {
                            return Err(TypeError::DuplicateArgumentLabel { span: label.get_range(), name: label_name });
                        }
                        let Some(param_data) = params_data.iter().find(|(_, param_name, _, _, _)| *param_name == label_name) else {
                            return Err(TypeError::UnexpectedArgumentName { span: label.get_range(), arg_name: label_name, is_instantiation });
                        };
                        if idx >= params_data.len() {
                            // This _should_ be unreachable given the two cases above, but just in case let's return an error here as well
                            return Err(TypeError::InvalidArity { span: label.get_range(), num_possible_args, num_required_args, num_provided_args });
                        }

                        seen_labels.insert(label_name);

                        param_idx = param_data.0;
                        param_type_id = param_data.2;
                        if param_data.4 { // is variadic
                            param_type_id = self.add_or_find_type_id(self.project.array_type(param_data.2));
                        }
                        gather_variadic_arguments = false;
                    } else if idx > 0 && !seen_labels.is_empty() {
                        return Err(TypeError::MixedArgumentType { span: arg_node.get_token().get_range() });
                    } else {
                        gather_variadic_arguments = idx >= params_data.len().saturating_sub(1) && fn_is_variadic;

                        if idx >= params_data.len() {
                            if !fn_is_variadic {
                                let span = typed_target.span().expand(&token.get_range());
                                return Err(TypeError::InvalidArity { span, num_possible_args, num_required_args, num_provided_args });
                            }

                            let param = params_data.last().expect("If the function is variadic, then the last param is the variadic param");
                            param_idx = param.0;
                            param_type_id = param.2;
                        } else {
                            param_idx = params_data[idx].0;
                            param_type_id = params_data[idx].2;
                        }
                    };

                    if let Some(type_hint) = type_hint {
                        if self.type_contains_generics(&return_type_id) {
                            self.extract_values_for_generics(&type_hint, &return_type_id, &mut filled_in_generic_types);
                        }
                    }

                    // Fill in any known generics (ie. from the instance, for a method) before typechecking arg expression
                    if self.type_contains_generics(&param_type_id) {
                        param_type_id = self.substitute_generics_with_known(&param_type_id, &filled_in_generic_types);
                    }

                    let typed_arg_value = self.typecheck_expression(arg_node, Some(param_type_id))?;
                    let arg_type_id = *typed_arg_value.type_id();

                    if arg_type_id == PRELUDE_UNIT_TYPE_ID {
                        return Err(TypeError::ForbiddenAssignment { span: typed_arg_value.span(), type_id: arg_type_id, purpose: "parameter" });
                    }

                    // Fill in any resolved generics that are only determined after typechecking arg expression (ie. function return types)
                    if self.type_contains_generics(&param_type_id) {
                        self.extract_values_for_generics(&arg_type_id, &param_type_id, &mut filled_in_generic_types);

                        param_type_id = self.substitute_generics_with_known(&param_type_id, &filled_in_generic_types);
                    }

                    if !self.type_satisfies_other(&arg_type_id, &param_type_id) {
                        return Err(TypeError::TypeMismatch { span: typed_arg_value.span(), expected: vec![param_type_id], received: arg_type_id });
                    }

                    if gather_variadic_arguments {
                        variadic_arguments.push(typed_arg_value);
                    } else {
                        typed_arguments[param_idx] = Some(typed_arg_value);
                    }
                }

                for (param_idx, _, param_type_id, param_is_optional, param_is_variadic) in params_data {
                    if param_is_variadic {
                        debug_assert!(param_idx == typed_arguments.len() - 1);
                        if typed_arguments[param_idx].is_none() {
                            let start_pos = variadic_arguments.get(0).map(|a| a.span().start).unwrap_or(POSITION_BOGUS);

                            typed_arguments[param_idx] = Some(TypedNode::Array {
                                token: Token::LBrack(start_pos, false),
                                items: variadic_arguments.drain(..).collect(),
                                type_id: self.add_or_find_type_id(self.project.array_type(param_type_id)),
                            })
                        }
                    } else if typed_arguments[param_idx].is_none() && !param_is_optional {
                        let span = typed_target.span().expand(&token.get_range());
                        return Err(TypeError::InvalidArity { span, num_possible_args, num_required_args, num_provided_args });
                    }
                }

                if self.type_contains_generics(&return_type_id) {
                    return_type_id = self.substitute_generics_with_known(&return_type_id, &filled_in_generic_types);
                }
                let type_id = if let Some(type_hint_id) = type_hint {
                    self.substitute_generics(&type_hint_id, &return_type_id)
                } else {
                    return_type_id
                };

                Ok(TypedNode::Invocation { target: Box::new(typed_target), arguments: typed_arguments, type_id })
            }
            AstNode::IfExpression(_, _) |
            AstNode::MatchExpression(_, _) => todo!(),
            AstNode::Lambda(token, n) => {
                let mut arg_hints = None;
                let mut ret_hint = None;
                if let Some(type_hint_id) = &type_hint {
                    let ty = self.project.get_type_by_id(type_hint_id);
                    if let Type::Function(param_type_ids, _, _, return_type_id) = ty {
                        arg_hints = Some(param_type_ids.clone());
                        ret_hint = Some(*return_type_id);
                    }
                }

                let parameters = n.parameters().into_iter().enumerate()
                    .map(|(idx, p)| {
                        let param_hint = arg_hints.as_ref().and_then(|hints| hints.get(idx).map(|t| *t));
                        (p, param_hint)
                    })
                    .collect();

                let fn_scope_id = self.begin_child_scope(format!("{:?}.lambda_{}", &self.current_module().id, self.new_lambda_fn_name()));
                let parameters = self.typecheck_function_parameters(false, &parameters)?;

                let lambda_func_id = self.add_lambda_function_to_current_scope(fn_scope_id, parameters)?;
                let prev_func_id = self.current_function;
                self.current_function = Some(lambda_func_id);

                let func = self.project.get_func_by_id(&lambda_func_id);
                let params = func.params.iter().map(|p| (p.name.clone(), p.type_id, p.defined_span.clone())).collect_vec();
                for (name, type_id, defined_span, ..) in params {
                    let Some(defined_span) = defined_span else { unreachable!("Internal error: when typechecking a user-defined function, parameters' spans will always be known"); };
                    let defined_span = defined_span.clone();

                    self.add_variable_to_current_scope(name, type_id, false, true, &defined_span, true)?;
                }

                let mut body = vec![];
                let mut last_type_id = PRELUDE_UNIT_TYPE_ID;
                let num_nodes = n.body.len();
                for (idx, node) in n.body.into_iter().enumerate() {
                    let is_last = idx == num_nodes - 1;
                    let type_hint = if is_last { ret_hint } else { None };

                    // TODO: Handle nested function declaration (and raise error on nested Type declaration)
                    let typed_node = self.typecheck_statement(node, type_hint)?;
                    let type_id = typed_node.type_id();
                    last_type_id = *type_id;

                    if is_last {
                        if let Some(type_hint) = type_hint {
                            if !self.type_contains_generics(&type_hint) && !self.type_satisfies_other(&last_type_id, &type_hint) {
                                return Err(TypeError::TypeMismatch { span: typed_node.span(), expected: vec![type_hint], received: last_type_id });
                            }
                        }
                    }

                    body.push(typed_node);
                }

                let func = self.project.get_func_by_id_mut(&lambda_func_id);
                func.body = body;
                func.return_type_id = last_type_id;

                let func = self.project.get_func_by_id(&lambda_func_id);
                let span = func.params.first()
                    .and_then(|p| p.defined_span.as_ref()).unwrap_or(&token.get_range())
                    .expand(&func.body.last().map(|n| n.span()).unwrap_or(token.get_range()));
                let func_type_id = self.add_or_find_type_id(self.project.function_type_for_function(&func, false));

                self.end_child_scope();
                self.current_function = prev_func_id;


                Ok(TypedNode::Lambda { span, func_id: lambda_func_id, type_id: func_type_id })
            }
            AstNode::Try(_, _) => todo!(),
            n => unreachable!("Internal error: node is not an expression: {:?}", n),
        }
    }
}
