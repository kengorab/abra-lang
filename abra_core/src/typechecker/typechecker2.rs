use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::{Hash, Hasher};
use std::io::BufRead;
use std::path::{Path, PathBuf};
use itertools::{Either, EitherOrBoth, Itertools};
use crate::{parser, tokenize_and_parse_stub};
use crate::common::util::integer_decode;
use crate::parser::parser::{ParseResult};
use crate::lexer::lexer_error::LexerError;
use crate::lexer::tokens::{POSITION_BOGUS, Range, Token};
use crate::parser::ast::{args_to_parameters, AssignmentNode, AstLiteralNode, AstNode, BinaryNode, BinaryOp, BindingDeclNode, BindingPattern, EnumDeclNode, ForLoopNode, FunctionDeclNode, IfNode, ImportKind, ImportNode, IndexingMode, IndexingNode, InvocationNode, MatchCase, MatchCaseArgument, MatchCaseType, MatchNode, Parameter, TypeDeclField, TypeDeclNode, TypeIdentifier, UnaryNode, UnaryOp, WhileLoopNode};
use crate::parser::parse_error::ParseError;

pub trait LoadModule {
    fn resolve_path(&self, module_id: &parser::ast::ModuleId) -> Option<String>;
    fn get_path(&self, module_id: &ModuleId) -> Option<String>;
    fn register(&mut self, m_id: &parser::ast::ModuleId, module_id: &ModuleId);
    fn get_module_id(&self, m_id: &parser::ast::ModuleId) -> Option<&ModuleId>;
    fn module_exists(&self, m_id: &parser::ast::ModuleId) -> bool;
    fn load_file(&self, file_name: &String) -> Option<String>;
    fn load_untyped_ast(&self, module_id: &parser::ast::ModuleId) -> Result<Option<(String, ParseResult)>, Either<LexerError, ParseError>> {
        use crate::{lexer::lexer, parser::parser};

        let Some(file_name) = self.resolve_path(&module_id) else { return Ok(None); };
        let Some(file_contents) = self.load_file(&file_name) else { return Ok(None); };

        match lexer::tokenize(module_id, &file_contents) {
            Err(e) => Err(Either::Left(e)),
            Ok(tokens) => match parser::parse(module_id.clone(), tokens) {
                Err(e) => Err(Either::Right(e)),
                Ok(nodes) => Ok(Some((file_name, nodes)))
            }
        }
    }
}

pub struct ModuleLoader<'a> {
    program_root: &'a PathBuf,
    module_id_map: HashMap::<ModuleId, parser::ast::ModuleId>,
    module_id_map_rev: HashMap::<parser::ast::ModuleId, ModuleId>,
    prelude_raw: &'static str,
}

impl<'a> ModuleLoader<'a> {
    pub fn new(program_root: &'a PathBuf) -> ModuleLoader {
        let prelude_raw = include_str!("prelude.stub.abra");

        ModuleLoader {
            program_root,
            module_id_map: HashMap::new(),
            module_id_map_rev: HashMap::new(),
            prelude_raw,
        }
    }
}

impl<'a> LoadModule for ModuleLoader<'a> {
    fn resolve_path(&self, module_id: &parser::ast::ModuleId) -> Option<String> {
        let mut path = PathBuf::new();

        let path_buf = PathBuf::from(module_id.get_path(&self.program_root)).with_extension("abra");
        for c in path_buf.components() {
            path.push(c.as_os_str())
        }

        path.to_str().map(|s| s.to_string())
    }

    fn get_path(&self, module_id: &ModuleId) -> Option<String> {
        let m_id = self.module_id_map.get(module_id).expect(&format!("Internal error: expected module for {:?}", module_id));
        self.resolve_path(m_id)
    }

    fn register(&mut self, m_id: &parser::ast::ModuleId, module_id: &ModuleId) {
        self.module_id_map.insert(*module_id, m_id.clone());
        self.module_id_map_rev.insert(m_id.clone(), *module_id);
    }

    fn get_module_id(&self, m_id: &parser::ast::ModuleId) -> Option<&ModuleId> {
        self.module_id_map_rev.get(m_id)
    }

    fn module_exists(&self, m_id: &parser::ast::ModuleId) -> bool {
        if m_id.is_prelude() { return true; }

        let Some(path) = self.resolve_path(m_id) else { return false; };
        Path::try_exists(Path::new(&path)).unwrap_or(false)
    }

    fn load_file(&self, file_name: &String) -> Option<String> {
        if file_name == "prelude.stub.abra" {
            return Some(self.prelude_raw.to_string());
        }

        std::fs::read_to_string(file_name).ok()
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

    pub fn get_scope_by_id(&self, scope_id: &ScopeId) -> &Scope {
        let ScopeId(ModuleId(module_idx), idx) = scope_id;
        &self.modules[*module_idx].scopes[*idx]
    }

    pub fn get_scope_by_id_mut(&mut self, scope_id: &ScopeId) -> &mut Scope {
        let ScopeId(ModuleId(module_idx), idx) = scope_id;
        &mut self.modules[*module_idx].scopes[*idx]
    }

    pub fn get_type_by_id(&self, type_id: &TypeId) -> &Type {
        if let Some(_) = type_id.as_module_type_alias() {
            return &Type::ModuleAlias;
        }

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

    pub fn get_enum_by_id(&self, enum_id: &EnumId) -> &Enum {
        let EnumId(ModuleId(module_idx), idx) = enum_id;
        let module = &self.modules[*module_idx];
        &module.enums[*idx]
    }

    pub fn get_enum_by_id_mut(&mut self, enum_id: &EnumId) -> &mut Enum {
        let EnumId(ModuleId(module_idx), idx) = enum_id;
        let module = &mut self.modules[*module_idx];
        &mut module.enums[*idx]
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
            Type::GenericEnumInstance(_, _, _) => return None,
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
            Type::Type(_) | Type::Generic(_, _) | Type::Function(_, _, _, _) | Type::ModuleAlias => return None,
        };

        Some((struct_, generic_substitutions))
    }

    pub fn get_enum_by_type_id(&self, type_id: &TypeId) -> Option<(&Enum, HashMap<TypeId, TypeId>, Option<usize>)> {
        let mut generic_substitutions = HashMap::new();
        if let Type::GenericEnumInstance(enum_id, generic_type_ids, variant_idx) = self.get_type_by_id(type_id) {
            let enum_ = self.get_enum_by_id(enum_id);
            generic_substitutions.extend(enum_.generic_ids.iter().zip(generic_type_ids).map(|(g_id, t_id)| (*g_id, *t_id)));
            Some((enum_, generic_substitutions, variant_idx.clone()))
        } else {
            None
        }
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

    pub fn find_enum_by_name(&self, module_id: &ModuleId, name: &String) -> Option<&Enum> {
        let module = &self.modules[module_id.0];
        module.enums.iter()
            .find(|s| s.name == *name)
            .or_else(|| {
                // If enum cannot be found in current module, look in the prelude module
                self.prelude_module().enums.iter()
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
        if name == "_" { return None; }

        self.walk_scope_chain_with(scope_id, |scope| {
            for var in &scope.vars {
                if var.name == *name {
                    return Some(var);
                }
            }
            None
        })
    }

    pub fn find_var_id_by_alias(&self, alias: VariableAlias) -> Option<VarId> {
        let scope_id = match alias {
            VariableAlias::None => return None,
            VariableAlias::Function(FuncId(scope_id, _)) => scope_id,
            VariableAlias::Type(TypeKind::Struct(StructId(module_id, _))) |
            VariableAlias::Type(TypeKind::Enum(EnumId(module_id, _))) => ScopeId(module_id, 0),
        };

        let ScopeId(module_id, scope_idx) = scope_id;
        let scope = &self.modules[module_id.0].scopes[scope_idx];
        scope.vars.iter().enumerate().find_map(|(idx, var)| {
            if var.alias == alias {
                Some(VarId(scope_id, idx))
            } else {
                None
            }
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
        Type::Type(TypeKind::Struct(struct_id))
    }

    pub fn enum_type(&self, enum_id: EnumId) -> Type {
        Type::Type(TypeKind::Enum(enum_id))
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
            Type::GenericEnumInstance(enum_id, generic_ids, _) => {
                let enum_ = self.get_enum_by_id(enum_id);
                if !generic_ids.is_empty() {
                    let inner_type_reprs = generic_ids.iter().map(|type_id| self.type_repr(type_id)).join(", ");
                    format!("{}<{}>", enum_.name, inner_type_reprs)
                } else {
                    format!("{}", enum_.name)
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
            Type::Type(TypeKind::Struct(struct_id)) => self.get_struct_by_id(struct_id).name.clone(),
            Type::Type(TypeKind::Enum(enum_id)) => self.get_enum_by_id(enum_id).name.clone(),
            Type::ModuleAlias => "<module>".to_string(),
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

    fn find_parent_scope_of_kind(&self, starting_scope_id: &ScopeId, kind: &ScopeKind) -> Option<&Scope> {
        self.walk_scope_chain_with(starting_scope_id, |sc| if &sc.kind == kind { Some(sc) } else { None })
    }

    fn find_parent_fn_scope(&self, starting_scope_id: &ScopeId) -> Option<&Scope> {
        self.walk_scope_chain_with(starting_scope_id, |sc| if matches!(&sc.kind, ScopeKind::Function(_)) { Some(sc) } else { None })
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct ModuleId(/* idx: */ pub usize);

impl ModuleId {
    pub const BOGUS: ModuleId = ModuleId(usize::MAX);
}

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    module_id: ModuleId,
    range: Range,
}

impl Span {
    #[cfg(test)]
    pub(crate) fn new(module_id: ModuleId, (start_line, start_col): (usize, usize), (end_line, end_col): (usize, usize)) -> Span {
        use crate::lexer::tokens::Position;

        Span {
            module_id,
            range: Range { start: Position { line: start_line, col: start_col }, end: Position { line: end_line, col: end_col } },
        }
    }

    fn from_range(module_id: ModuleId, range: Range) -> Span {
        Span { module_id, range }
    }

    pub fn expand(&self, other: &Span) -> Span {
        self.expand_range(&other.range)
    }

    pub fn expand_range(&self, other: &Range) -> Span {
        Span {
            module_id: self.module_id,
            range: self.range.expand(&other),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct StructId(/* module_id: */ pub ModuleId, /* idx: */ pub usize);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct EnumId(/* module_id: */ pub ModuleId, /* idx: */ pub usize);

#[derive(Debug, PartialEq)]
pub struct Struct {
    pub id: StructId,
    pub struct_scope_id: ScopeId,
    pub name: String,
    // Structs with no defined_span are builtins
    pub defined_span: Option<Span>,
    pub generic_ids: Vec<TypeId>,
    pub self_type_id: TypeId,
    pub fields: Vec<StructField>,
    pub methods: Vec<FuncId>,
    pub static_methods: Vec<FuncId>,
}

#[derive(Debug, PartialEq)]
pub struct StructField {
    pub name: String,
    pub type_id: TypeId,
    pub defined_span: Span,
    pub is_readonly: bool,
    pub default_value: Option<TypedNode>,
}

#[derive(Debug, PartialEq)]
pub struct Enum {
    pub id: EnumId,
    pub enum_scope_id: ScopeId,
    pub name: String,
    pub defined_span: Span,
    pub generic_ids: Vec<TypeId>,
    pub self_type_id: TypeId,
    pub variants: Vec<EnumVariant>,
    pub methods: Vec<FuncId>,
    pub static_methods: Vec<FuncId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub defined_span: Span,
    pub kind: EnumVariantKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum EnumVariantKind {
    Constant,
    Container(FuncId),
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeId(/* scope_id: */ pub ScopeId, /* idx: */ pub usize);

impl TypeId {
    const PLACEHOLDER_SLOT_MARKER: ScopeId = ScopeId::BOGUS;
    const MODULE_ALIAS_MARKER: ScopeId = ScopeId(ScopeId::BOGUS.0, ScopeId::BOGUS.1 - 1);

    fn placeholder_slot(idx: usize) -> TypeId {
        TypeId(Self::PLACEHOLDER_SLOT_MARKER, idx)
    }

    fn is_placeholder_slot(&self) -> bool {
        self.0 == Self::PLACEHOLDER_SLOT_MARKER
    }

    fn module_type_alias(module_id: &ModuleId) -> TypeId {
        let ModuleId(module_idx) = module_id;
        TypeId(Self::MODULE_ALIAS_MARKER, *module_idx)
    }

    fn as_module_type_alias(&self) -> Option<ModuleId> {
        if self.0 == Self::MODULE_ALIAS_MARKER { Some(ModuleId(self.1)) } else { None }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrimitiveType {
    Unit,
    Any,
    Int,
    Float,
    Bool,
    String,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TypeKind {
    Struct(StructId),
    Enum(EnumId),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Generic(/*span: */ Option<Range>, /* name: */ String),
    GenericInstance(StructId, Vec<TypeId>),
    GenericEnumInstance(EnumId, Vec<TypeId>, /* variant_idx: */ Option<usize>),
    Function(/* parameter_type_ids: */ Vec<TypeId>, /* num_required_params: */ usize, /* is_variadic: */ bool, /* return_type_id: */ TypeId),
    Type(TypeKind),
    ModuleAlias,
}

impl Type {
    pub fn get_field<'a>(&self, project: &'a Project, field_idx: usize) -> Option<(&'a String, &'a TypeId)> {
        match self {
            Type::Primitive(PrimitiveType::Unit) |
            Type::Primitive(PrimitiveType::Any) => None,
            Type::Primitive(PrimitiveType::Int) => {
                let struct_field = &project.get_struct_by_id(&project.prelude_int_struct_id).fields[field_idx];
                Some((&struct_field.name, &struct_field.type_id))
            }
            Type::Primitive(PrimitiveType::Float) => {
                let struct_field = &project.get_struct_by_id(&project.prelude_float_struct_id).fields[field_idx];
                Some((&struct_field.name, &struct_field.type_id))
            }
            Type::Primitive(PrimitiveType::Bool) => {
                let struct_field = &project.get_struct_by_id(&project.prelude_bool_struct_id).fields[field_idx];
                Some((&struct_field.name, &struct_field.type_id))
            }
            Type::Primitive(PrimitiveType::String) => {
                let struct_field = &project.get_struct_by_id(&project.prelude_string_struct_id).fields[field_idx];
                Some((&struct_field.name, &struct_field.type_id))
            }
            Type::Generic(_, _) => None,
            Type::GenericInstance(struct_id, _) => {
                let struct_field = &project.get_struct_by_id(struct_id).fields[field_idx];
                Some((&struct_field.name, &struct_field.type_id))
            }
            Type::GenericEnumInstance(_, _, _) |
            Type::Function(_, _, _, _) |
            Type::Type(_) |
            Type::ModuleAlias => todo!()
        }
    }

    pub fn get_method<'a>(&self, project: &'a Project, method_idx: usize) -> Option<FuncId> {
        match self {
            Type::Primitive(PrimitiveType::Unit) |
            Type::Primitive(PrimitiveType::Any) => None,
            Type::Primitive(PrimitiveType::Int) => Some(project.get_struct_by_id(&project.prelude_int_struct_id).methods[method_idx]),
            Type::Primitive(PrimitiveType::Float) => Some(project.get_struct_by_id(&project.prelude_float_struct_id).methods[method_idx]),
            Type::Primitive(PrimitiveType::Bool) => Some(project.get_struct_by_id(&project.prelude_bool_struct_id).methods[method_idx]),
            Type::Primitive(PrimitiveType::String) => Some(project.get_struct_by_id(&project.prelude_string_struct_id).methods[method_idx]),
            Type::Generic(_, _) => None,
            Type::GenericInstance(struct_id, _) => Some(project.get_struct_by_id(struct_id).methods[method_idx]),
            Type::GenericEnumInstance(_, _, _) |
            Type::Function(_, _, _, _) |
            Type::Type(_) |
            Type::ModuleAlias => todo!()
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct ScopeId(/* module_id: */ pub ModuleId, /* idx: */ pub usize);

impl ScopeId {
    pub const BOGUS: ScopeId = ScopeId(ModuleId::BOGUS, usize::MAX);
}

#[derive(Debug, PartialEq)]
pub enum ScopeKind {
    Module(ModuleId),
    Function(FuncId),
    Type,
    If,
    Match,
    Loop,
}

#[derive(Debug, PartialEq)]
pub enum ControlFlowTerminator {
    Break,
    Continue,
    Return,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TerminatorKind {
    NonReturning,
    Returning,
}

fn compound_terminator_kinds(tk1: Option<TerminatorKind>, tk2: Option<TerminatorKind>) -> Option<TerminatorKind> {
    match (tk1, tk2) {
        (Some(TerminatorKind::Returning), Some(TerminatorKind::Returning)) => Some(TerminatorKind::Returning),
        (Some(TerminatorKind::NonReturning), Some(_)) | (Some(_), Some(TerminatorKind::NonReturning)) => Some(TerminatorKind::NonReturning),
        (None, _) | (_, None) => None,
    }
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub label: String,
    pub kind: ScopeKind,
    pub terminator: Option<TerminatorKind>,
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
    Type(TypeKind),
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub id: VarId,
    pub name: String,
    pub type_id: TypeId,
    pub is_mutable: bool,
    pub is_initialized: bool,
    // Variables with no defined_span are builtins
    pub defined_span: Option<Span>,
    pub is_captured: bool,
    pub alias: VariableAlias,
    pub is_parameter: bool,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FuncId(/* scope_id: */ pub ScopeId, /* idx: */ pub usize);

impl FuncId {
    pub const BOGUS: FuncId = FuncId(ScopeId::BOGUS, usize::MAX);
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub id: FuncId,
    pub fn_scope_id: ScopeId,
    pub name: String,
    pub generic_ids: Vec<TypeId>,
    pub has_self: bool,
    pub params: Vec<FunctionParam>,
    pub return_type_id: TypeId,
    // Functions with no defined_span are builtins or lambdas (since they can't have name collisions anyway)
    pub defined_span: Option<Span>,
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
    pub var_id: VarId,
    // Params with no defined_span are for builtin functions
    pub defined_span: Option<Span>,
    pub default_value: Option<TypedNode>,
    pub is_variadic: bool,
    pub is_incomplete: bool,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ExportedValue {
    Function(FuncId),
    Type(TypeKind),
    Variable(VarId),
}

#[derive(Debug, PartialEq)]
pub struct TypedModule {
    pub id: ModuleId,
    pub name: String,
    pub type_ids: Vec<TypeId>,
    // TODO: is this necessary?
    pub functions: Vec<FuncId>,
    pub structs: Vec<Struct>,
    pub enums: Vec<Enum>,
    pub code: Vec<TypedNode>,
    pub scopes: Vec<Scope>,
    pub exports: HashMap<String, ExportedValue>,
    pub completed: bool,
}

#[derive(Debug, PartialEq)]
pub enum AccessorKind {
    Field,
    Method,
    StaticMethod,
    EnumVariant,
}

#[derive(Debug, PartialEq)]
pub enum AssignmentKind {
    Identifier { var_id: VarId },
    Accessor { target: Box<TypedNode>, kind: AccessorKind, member_idx: usize },
    Indexing { target: Box<TypedNode>, index: Box<TypedNode> },
}

#[derive(Debug, PartialEq)]
pub struct TypedMatchCase {
    pub body: Vec<TypedNode>,
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
    Accessor { target: Box<TypedNode>, kind: AccessorKind, is_opt_safe: bool, member_idx: usize, member_span: Range, type_id: TypeId },
    Indexing { target: Box<TypedNode>, index: IndexingMode<TypedNode>, type_id: TypeId },
    Lambda { span: Range, func_id: FuncId, type_id: TypeId },
    Assignment { span: Range, kind: AssignmentKind, type_id: TypeId, expr: Box<TypedNode> },
    If { if_token: Token, condition: Box<TypedNode>, condition_binding: Option<BindingPattern>, if_block: Vec<TypedNode>, else_block: Vec<TypedNode>, is_statement: bool, type_id: TypeId },
    Match { match_token: Token, target: Box<TypedNode>, cases: Vec<TypedMatchCase>, is_statement: bool, type_id: TypeId },

    // Statements
    FuncDeclaration(FuncId),
    TypeDeclaration(StructId),
    EnumDeclaration(EnumId),
    BindingDeclaration { token: Token, pattern: BindingPattern, vars: Vec<VarId>, expr: Option<Box<TypedNode>> },
    ForLoop { token: Token, binding: BindingPattern, binding_var_ids: Vec<VarId>, index_var_id: Option<VarId>, iterator: Box<TypedNode>, body: Vec<TypedNode> },
    WhileLoop { token: Token, condition: Box<TypedNode>, condition_var_id: Option<VarId>, body: Vec<TypedNode> },
    Break { token: Token },
    Continue { token: Token },
    Return { token: Token, expr: Option<Box<TypedNode>> },
    Import { token: Token, kind: ImportKind, module_id: ModuleId },
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
            TypedNode::Indexing { type_id, .. } => type_id,
            TypedNode::If { type_id, .. } => type_id,
            TypedNode::Match { type_id, .. } => type_id,

            // Statements
            TypedNode::FuncDeclaration(_) |
            TypedNode::TypeDeclaration(_) |
            TypedNode::EnumDeclaration(_) |
            TypedNode::BindingDeclaration { .. } |
            TypedNode::ForLoop { .. } |
            TypedNode::WhileLoop { .. } |
            TypedNode::Break { .. } |
            TypedNode::Continue { .. } => &PRELUDE_UNIT_TYPE_ID,
            TypedNode::Return { expr, .. } => expr.as_ref().map_or(&PRELUDE_UNIT_TYPE_ID, |expr| expr.type_id()),
            TypedNode::Import { .. } => &PRELUDE_UNIT_TYPE_ID,
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
            TypedNode::Indexing { target, index, .. } => {
                let start = target.span();
                match index {
                    IndexingMode::Index(idx_node) => start.expand(&idx_node.span()),
                    IndexingMode::Range(start_node, end_node) => {
                        let end = end_node.as_ref().map(|n| n.span()).or_else(|| start_node.as_ref().map(|n| n.span()));
                        if let Some(end) = end {
                            start.expand(&end)
                        } else {
                            start
                        }
                    }
                }
            }
            TypedNode::If { if_token, condition, condition_binding, if_block, else_block, .. } => {
                let start = if_token.get_range();
                if let Some(end) = else_block.last().map(|n| n.span()) {
                    start.expand(&end)
                } else if let Some(end) = if_block.last().map(|n| n.span()) {
                    start.expand(&end)
                } else if let Some(binding) = condition_binding {
                    start.expand(&binding.get_span())
                } else {
                    start.expand(&condition.span())
                }
            }
            TypedNode::Match { match_token, target, .. } => {
                let start = match_token.get_range();
                start.expand(&target.span())
            }

            // Statements
            TypedNode::FuncDeclaration(_) |
            TypedNode::TypeDeclaration(_) |
            TypedNode::EnumDeclaration(_) => todo!(),
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
            TypedNode::ForLoop { token, iterator, body, .. } => {
                let start = token.get_range();
                let end = body.last().map(|n| n.span()).unwrap_or_else(|| iterator.span());
                start.expand(&end)
            }
            TypedNode::WhileLoop { token, condition, body, .. } => {
                let start = token.get_range();
                let end = body.last().map(|n| n.span()).unwrap_or_else(|| condition.span());
                start.expand(&end)
            }
            TypedNode::Break { token } |
            TypedNode::Continue { token } => token.get_range(),
            TypedNode::Return { token, expr } => {
                let start = token.get_range();
                if let Some(expr) = expr {
                    start.expand(&expr.span())
                } else {
                    start
                }
            }
            TypedNode::Import { token, .. } => token.get_range(),
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

impl Hash for TypedLiteral {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match self {
            TypedLiteral::Int(i) => i.hash(hasher),
            TypedLiteral::Float(f) => integer_decode(*f).hash(hasher),
            TypedLiteral::Bool(b) => b.hash(hasher),
            TypedLiteral::String(s) => s.hash(hasher),
        }
    }
}

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
    Enum,
    Field,
    Method,
    EnumVariant,
    StaticMethodOrVariant,
}

#[derive(Debug, PartialEq)]
pub enum ImmutableAssignmentKind {
    Parameter,
    Variable,
    Field(/* type_name: */ String),
    Method(/* method_name: */ String),
    StaticMethod(/* type_name: */ String),
    EnumVariant(/* variant_name: */ String),
}

#[derive(Debug, PartialEq)]
pub enum InvalidTupleIndexKind {
    OutOfBounds(i64),
    NonConstant,
}

#[derive(Debug, PartialEq)]
pub enum InvalidAssignmentTargetKind {
    IndexingRange,
    IndexingString,
    IndexingTuple,
    UnsupportedAssignmentTarget,
}

#[derive(Debug, PartialEq)]
pub enum UnreachableMatchCaseKind {
    AlreadyCovered,
    NoTypeOverlap { case_type: Option<TypeId>, target_type: TypeId, target_span: Span },
}

#[derive(Debug, PartialEq)]
pub enum InvalidLoopTargetKind {
    For,
    While,
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    UnimplementedFeature { span: Span, desc: &'static str },
    TypeMismatch { span: Span, expected: Vec<TypeId>, received: TypeId },
    BranchTypeMismatch { span: Span, orig_span: Span, expected: TypeId, received: TypeId },
    IllegalOperator { span: Span, op: BinaryOp, left: TypeId, right: TypeId },
    UnknownType { span: Span, name: String },
    UnknownIdentifier { span: Span, token: Token },
    MissingBindingInitializer { span: Span, is_mutable: bool },
    DuplicateName { span: Span, name: String, original_span: Option<Span>, kind: DuplicateNameKind },
    ForbiddenAssignment { span: Span, type_id: TypeId, purpose: &'static str },
    DestructuringMismatch { span: Span, kind: DestructuringMismatchKind, type_id: TypeId },
    DuplicateSplat { span: Span },
    DuplicateParameter { span: Span, name: String },
    ReturnTypeMismatch { span: Span, func_name: String, expected: TypeId, received: TypeId },
    IllegalInvocation { span: Span, type_id: TypeId },
    IllegalEnumVariantConstruction { span: Span, enum_id: EnumId, variant_idx: usize },
    UnexpectedArgumentName { span: Span, arg_name: String, is_instantiation: bool },
    MixedArgumentType { span: Span },
    DuplicateArgumentLabel { span: Span, name: String },
    InvalidArity { span: Span, num_possible_args: usize, num_required_args: usize, num_provided_args: usize },
    InvalidSelfParam { span: Span },
    InvalidSelfParamPosition { span: Span },
    InvalidRequiredParamPosition { span: Span, is_variadic: bool },
    InvalidVarargPosition { span: Span },
    InvalidVarargType { span: Span, type_id: TypeId },
    InvalidTypeArgumentArity { span: Span, num_required_args: usize, num_provided_args: usize },
    UnknownMember { span: Span, field_name: String, type_id: TypeId },
    MissingRequiredArgumentLabels { span: Span },
    UnknownTypeForParameter { span: Span, param_name: String },
    AssignmentToImmutable { span: Span, var_name: String, defined_span: Option<Span>, kind: ImmutableAssignmentKind },
    InvalidIndexableType { span: Span, is_range: bool, type_id: TypeId },
    InvalidIndexType { span: Span, required_type_id: TypeId, provided_type_id: TypeId },
    InvalidTupleIndex { span: Span, kind: InvalidTupleIndexKind, type_id: TypeId },
    InvalidAssignmentTarget { span: Span, kind: InvalidAssignmentTargetKind },
    DuplicateMatchCase { span: Span, orig_span: Span },
    EmptyMatchBlock { span: Span },
    UnreachableMatchCase { span: Span, kind: UnreachableMatchCaseKind },
    NonExhaustiveMatch { span: Span, type_id: TypeId },
    InvalidLoopTarget { span: Span, type_id: TypeId, kind: InvalidLoopTargetKind },
    InvalidControlFlowTerminator { span: Span, terminator: ControlFlowTerminator },
    UnreachableCode { span: Span },
    InvalidExportScope { span: Span },
    CircularModuleImport { span: Span },
    UnknownModule { span: Span, module_path: Option<String> },
    UnknownExport { span: Span, module_id: ModuleId, import_name: String, is_aliased: bool },
}

impl TypeError {
    const INDENT_AMOUNT: usize = 2;

    fn get_underline(left_padding: usize, length: usize) -> String {
        format!("{}{}", " ".repeat(left_padding), "^".repeat(length))
    }

    fn indent() -> String {
        " ".repeat(Self::INDENT_AMOUNT)
    }

    fn get_underlined_line(loader: &ModuleLoader, span: &Span) -> String {
        let lines = if &span.module_id == &PRELUDE_MODULE_ID {
            std::io::BufReader::new(loader.prelude_raw.as_bytes())
                .lines()
                .skip(span.range.start.line - 1)
                .take(span.range.end.line - span.range.start.line + 1)
                .collect::<Result<Vec<_>, _>>()
                .unwrap()
        } else {
            let file_name = loader.get_path(&span.module_id)
                .expect("Internal error: cannot report on errors in a file that never existed in the first place");
            let file = std::fs::File::open(&file_name).unwrap();
            std::io::BufReader::new(file)
                .lines()
                .skip(span.range.start.line - 1)
                .take(span.range.end.line - span.range.start.line + 1)
                .collect::<Result<Vec<_>, _>>()
                .unwrap()
        };

        let num_lines = lines.len();
        if num_lines == 1 {
            let line = &lines[0];
            let length = span.range.end.col - span.range.start.col + 1;
            let underline = Self::get_underline(2 * Self::INDENT_AMOUNT + span.range.start.col, length);
            let indent = Self::indent();
            format!("{}|{}{}\n{}", indent, indent, line, underline)
        } else {
            lines.into_iter()
                .enumerate()
                .flat_map(|(idx, line)| {
                    let cursor_line = if idx == 0 {
                        Self::get_underline(span.range.start.col - 1, line.len() - span.range.start.col + 1)
                    } else if idx == num_lines - 1 {
                        Self::get_underline(0, span.range.end.col + 1)
                    } else {
                        Self::get_underline(0, line.len())
                    };

                    let indent = Self::indent();
                    vec![
                        format!("{}|{}{}", indent, indent, line),
                        format!("{} {}{}", indent, indent, cursor_line),
                    ]
                })
                .join("\n")
        }
    }

    pub fn message(&self, loader: &ModuleLoader, project: &Project) -> String {
        let span = match self {
            TypeError::UnimplementedFeature { span, .. } |
            TypeError::TypeMismatch { span, .. } |
            TypeError::BranchTypeMismatch { span, .. } |
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
            TypeError::IllegalEnumVariantConstruction { span, .. } |
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
            TypeError::AssignmentToImmutable { span, .. } |
            TypeError::InvalidIndexableType { span, .. } |
            TypeError::InvalidIndexType { span, .. } |
            TypeError::InvalidTupleIndex { span, .. } |
            TypeError::InvalidAssignmentTarget { span, .. } |
            TypeError::DuplicateMatchCase { span, .. } |
            TypeError::EmptyMatchBlock { span } |
            TypeError::UnreachableMatchCase { span, .. } |
            TypeError::NonExhaustiveMatch { span, .. } |
            TypeError::InvalidLoopTarget { span, .. } |
            TypeError::InvalidControlFlowTerminator { span, .. } |
            TypeError::UnreachableCode { span } |
            TypeError::InvalidExportScope { span } |
            TypeError::CircularModuleImport { span } |
            TypeError::UnknownModule { span, .. } |
            TypeError::UnknownExport { span, .. } => span
        };
        let cursor_line = Self::get_underlined_line(loader, span);

        let msg = match self {
            TypeError::UnimplementedFeature { desc, .. } => {
                format!("Unimplemented feature: {}\n{}", desc, cursor_line)
            }
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
                        but instead found: {}",
                        cursor_line,
                        if multiple_expected { " one of: " } else { ": " }, expected,
                        received
                    )
                }
            }
            TypeError::BranchTypeMismatch { orig_span, expected, received, .. } => {
                format!(
                    "Type mismatch between branches\n{}\n\
                    Found type {}, but expected type {} because of prior branch\n{}",
                    cursor_line,
                    project.type_repr(received), project.type_repr(expected),
                    Self::get_underlined_line(loader, orig_span)
                )
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
                if *kind == DuplicateNameKind::StaticMethodOrVariant {
                    let first_msg = format!("Duplicate member '{}'\n{}", &name, cursor_line);

                    let Some(original_span) = original_span else { unreachable!() };
                    let pos = &original_span.range.start;
                    let cursor_line = Self::get_underlined_line(loader, original_span);
                    format!(
                        "{}\n\
                        There is already a variant declared in this enum with that name at ({}:{})\n{}",
                        first_msg,
                        pos.line, pos.col, cursor_line
                    )
                } else {
                    let kind = match kind {
                        DuplicateNameKind::Variable => "name",
                        DuplicateNameKind::Function => "function",
                        DuplicateNameKind::TypeArgument => "type argument",
                        DuplicateNameKind::Type => "type",
                        DuplicateNameKind::Enum => "enum",
                        DuplicateNameKind::Field => "field",
                        DuplicateNameKind::Method => "method",
                        DuplicateNameKind::EnumVariant => "enum variant",
                        DuplicateNameKind::StaticMethodOrVariant => unreachable!("Handled as a special case above"),
                    };

                    let first_msg = format!("Duplicate {} '{}'\n{}", &kind, &name, cursor_line);

                    let second_msg = if let Some(original_span) = original_span {
                        let pos = &original_span.range.start;
                        let cursor_line = Self::get_underlined_line(loader, original_span);
                        format!("This {} is already declared at ({}:{})\n{}", kind, pos.line, pos.col, cursor_line)
                    } else {
                        format!("This {} is already declared as built-in value", kind)
                    };

                    format!("{}\n{}", first_msg, second_msg)
                }
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

                let first_line = if is_lambda_fn(&func_name) {
                    "Return type mismatch for lambda function".to_string()
                } else {
                    format!("Return type mismatch for function '{}'", func_name)
                };

                format!(
                    "{}\n{}\n\
                    Expected: {}\n\
                    but instead saw: {}",
                    first_line, cursor_line,
                    expected_repr,
                    received_repr,
                )
            }
            TypeError::IllegalInvocation { type_id, .. } => {
                let type_repr = project.type_repr(type_id);
                let hint = if let Some((enum_, _, _)) = project.get_enum_by_type_id(type_id) {
                    let example = if let Some(variant) = enum_.variants.first() {
                        format!(" (eg. '{}.{}')", type_repr, variant.name)
                    } else {
                        "".to_string()
                    };
                    format!("Type '{}' is an enum and can only be constructed using a variant{}", type_repr, example)
                } else {
                    format!("Type '{}' is not callable", type_repr)
                };

                format!(
                    "Cannot invoke target as function\n{}\n{}",
                    cursor_line, hint
                )
            }
            TypeError::IllegalEnumVariantConstruction { enum_id, variant_idx, .. } => {
                let enum_ = project.get_enum_by_id(enum_id);
                let enum_name = &enum_.name;
                let variant_name = &enum_.variants[*variant_idx].name;
                format!(
                    "Cannot invoke target as function\n{}\n\
                    Variant {} of enum {} cannot be constructed",
                    cursor_line,
                    variant_name, enum_name,
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
                    ImmutableAssignmentKind::Method(_) |
                    ImmutableAssignmentKind::StaticMethod(_) => "field",
                    ImmutableAssignmentKind::EnumVariant(_) => "enum variant",
                };

                let second_line = match kind {
                    ImmutableAssignmentKind::Parameter => "Function parameters are automatically declared as immutable".to_string(),
                    ImmutableAssignmentKind::Variable => "Variable is declared as immutable. Use 'var' when declaring variable to allow it to be reassigned".to_string(),
                    ImmutableAssignmentKind::Field(type_name) => format!("Field '{}' is marked readonly in type '{}'", var_name, type_name),
                    ImmutableAssignmentKind::Method(type_name) => format!("Function '{}' is a method on type '{}'", var_name, type_name),
                    ImmutableAssignmentKind::StaticMethod(type_name) => format!("Function '{}' is a static method on type '{}'", var_name, type_name),
                    ImmutableAssignmentKind::EnumVariant(enum_name) => format!("'{}' is a variant of enum '{}'", var_name, enum_name),
                };
                let second_line = format!(
                    "{}{}",
                    second_line,
                    defined_span.as_ref().map(|span| format!("\n{}", Self::get_underlined_line(loader, span))).unwrap_or("".to_string())
                );

                format!(
                    "Cannot assign to {} '{}'\n{}\n{}",
                    kind_name, var_name, cursor_line,
                    second_line
                )
            }
            TypeError::InvalidIndexableType { type_id, is_range, .. } => {
                format!(
                    "Unsupported indexing operation\n{}\n\
                    Type '{}' is not indexable{}",
                    cursor_line, project.type_repr(type_id),
                    if *is_range { " as a range" } else { "" }
                )
            }
            TypeError::InvalidIndexType { required_type_id, provided_type_id, .. } => {
                format!(
                    "Invalid type for index argument\n{}\n\
                    Expected: {}\n\
                    but instead saw: {}",
                    cursor_line, project.type_repr(required_type_id), project.type_repr(provided_type_id),
                )
            }
            TypeError::InvalidTupleIndex { kind, type_id, .. } => {
                let message = match kind {
                    InvalidTupleIndexKind::OutOfBounds(idx) => format!("No value at index {} for tuple of type '{}'", idx, project.type_repr(type_id)),
                    InvalidTupleIndexKind::NonConstant => "Index values for tuples must be constant non-negative integers".to_string(),
                };

                format!(
                    "Unsupported indexing into tuple\n{}\n{}",
                    cursor_line, message
                )
            }
            TypeError::InvalidAssignmentTarget { kind, .. } => {
                let message = match kind {
                    InvalidAssignmentTargetKind::IndexingRange => "Left-hand side of assignment cannot be a range",
                    InvalidAssignmentTargetKind::IndexingString => "Strings are immutable and cannot be updated with index operations",
                    InvalidAssignmentTargetKind::IndexingTuple => "Tuples are immutable and their values cannot be reassigned",
                    InvalidAssignmentTargetKind::UnsupportedAssignmentTarget => "Unsupported expression for left-hand side of assignment",
                };

                format!(
                    "Cannot perform assignment\n{}\n{}",
                    cursor_line, message
                )
            }
            TypeError::DuplicateMatchCase { orig_span, .. } => {
                let orig_line = Self::get_underlined_line(loader, orig_span);

                format!(
                    "Duplicate match case\n{}\n\
                    Match case already handled here\n{}",
                    cursor_line, orig_line
                )
            }
            TypeError::EmptyMatchBlock { .. } => {
                format!(
                    "Empty block for match case\n{}\n\
                    Each case in a match must result in a value",
                    cursor_line,
                )
            }
            TypeError::UnreachableMatchCase { kind, .. } => {
                let message = match kind {
                    UnreachableMatchCaseKind::AlreadyCovered => "This case has already been covered by a previous case".to_string(),
                    UnreachableMatchCaseKind::NoTypeOverlap { case_type, target_type, target_span } => {
                        let target_type_repr = project.type_repr(target_type);
                        let target_underline = Self::get_underlined_line(loader, target_span);

                        if let Some(case_type_id) = case_type {
                            format!(
                                "No overlap between case type '{}' and match target type '{}'\n{}",
                                project.type_repr(case_type_id), target_type_repr,
                                target_underline
                            )
                        } else {
                            format!("Match target type '{}' can never be None\n{}", target_type_repr, target_underline)
                        }
                    }
                };
                format!(
                    "Unreachable match case\n{}\n{}",
                    cursor_line, message
                )
            }
            TypeError::NonExhaustiveMatch { type_id, .. } => {
                format!(
                    "Non-exhaustive match\n{}\n\
                    Match target type '{}' is not covered by all match cases.\n\
                    You can use a wildcard to capture remaining cases.",
                    cursor_line, project.type_repr(type_id),
                )
            }
            TypeError::InvalidLoopTarget { type_id, kind, .. } => {
                let type_repr = project.type_repr(type_id);
                let (loop_type, message) = match kind {
                    InvalidLoopTargetKind::For => ("for", format!("Type '{}' is not iterable", type_repr)),
                    InvalidLoopTargetKind::While => ("while", format!("Expected Bool or Option type, got '{}'", type_repr)),
                };

                format!(
                    "Invalid type for {}-loop target\n{}\n{}",
                    loop_type, cursor_line,
                    message
                )
            }
            TypeError::InvalidControlFlowTerminator { terminator, .. } => {
                let (keyword, msg) = match terminator {
                    ControlFlowTerminator::Break => ("break", "A break keyword cannot appear outside of a loop"),
                    ControlFlowTerminator::Continue => ("continue", "A continue keyword cannot appear outside of a loop"),
                    ControlFlowTerminator::Return => ("return", "A return keyword cannot appear outside of a function"),
                };

                format!(
                    "Unexpected {} keyword\n{}\n{}",
                    keyword, cursor_line, msg
                )
            }
            TypeError::UnreachableCode { .. } => {
                format!("Unreachable code\n{}", cursor_line)
            }
            TypeError::InvalidExportScope { .. } => {
                format!(
                    "Invalid export modifier\n{}\n\
                    Exported values may only appear at the top level scope",
                    cursor_line
                )
            }
            TypeError::CircularModuleImport { .. } => {
                format!(
                    "Could not import module due to circular dependency\n{}\n\
                    The current module is imported by the desired module (or one of its imports), resulting in a cycle",
                    cursor_line
                )
            }
            TypeError::UnknownModule { module_path, .. } => {
                let path_msg = if let Some(module_path) = module_path {
                    format!(" at '{}'", module_path)
                } else {
                    "".to_string()
                };

                format!(
                    "Could not import module\n{}\nNo such module exists{}",
                    cursor_line, path_msg
                )
            }
            TypeError::UnknownExport { module_id, import_name, is_aliased, .. } => {
                let first_line = if *is_aliased { "Unknown member" } else { "Invalid import" };

                format!(
                    "{}\n{}\nThere's no exported value named '{}' in module '{}'",
                    first_line, cursor_line, import_name, project.modules[module_id.0].name,
                )
            }
        };

        let file_name = loader.get_path(&span.module_id)
            .expect("Internal error: cannot report on errors in a file that never existed in the first place");
        let error_line = format!("Error at {}:{}:{}", file_name, span.range.start.line, span.range.start.col);
        format!("{}\n{}", error_line, msg)
    }
}

#[derive(Debug, PartialEq)]
enum FunctionPass {
    NotStarted,
    Pass0,
    Pass1 { just_saw_function_call: bool },
    Pass2,
}

const LAMBDA_FN_NAME_PREFIX: &str = "lambda_";

fn is_lambda_fn(name: &String) -> bool {
    name.starts_with(LAMBDA_FN_NAME_PREFIX)
}

pub struct Typechecker2<'a, L: LoadModule> {
    module_loader: &'a mut L,
    project: &'a mut Project,
    current_scope_id: ScopeId,
    current_type_decl: Option<TypeId>,
    current_function: Option<FuncId>,
    function_pass: FunctionPass,
}

impl<'a, L: LoadModule> Typechecker2<'a, L> {
    pub fn new(module_loader: &'a mut L, project: &'a mut Project) -> Typechecker2<'a, L> {
        Typechecker2 { module_loader, project, current_scope_id: PRELUDE_SCOPE_ID, current_type_decl: None, current_function: None, function_pass: FunctionPass::NotStarted }
    }

    /* UTILITIES */

    fn current_module_mut(&mut self) -> &mut TypedModule {
        &mut self.project.modules[self.current_scope_id.0.0]
    }

    fn current_module(&self) -> &TypedModule {
        &self.project.modules[self.current_scope_id.0.0]
    }

    fn make_span(&self, range: &Range) -> Span {
        Span::from_range(self.current_module().id, range.clone())
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

    fn get_enum_by_name(&self, name: &String) -> Option<&Enum> {
        let current_module_id = self.current_module().id;
        self.project.find_enum_by_name(&current_module_id, name)
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

    fn type_satisfies_other(&mut self, base_type: &TypeId, target_type: &TypeId) -> bool {
        #[inline]
        fn type_satisfies_other_impl<L: LoadModule>(zelf: &Typechecker2<L>, base_type_id: &TypeId, target_type_id: &TypeId, substitutions: &HashMap<TypeId, TypeId>) -> bool {
            let base_type_id = substitutions.get(&base_type_id).unwrap_or(&base_type_id);
            let target_type_id = substitutions.get(&target_type_id).unwrap_or(&target_type_id);

            if base_type_id.is_placeholder_slot() && target_type_id.is_placeholder_slot() {
                return base_type_id.1 == target_type_id.1;
            }

            let base_ty = zelf.project.get_type_by_id(base_type_id);
            let target_ty = zelf.project.get_type_by_id(target_type_id);

            match (base_ty, target_ty) {
                (_, Type::Primitive(PrimitiveType::Any)) => true,
                (Type::Generic(_, _), Type::Generic(_, _)) => base_type_id == target_type_id,
                (_, Type::Generic(_, _)) => false, // unreachable!("Test: we shouldn't reach here because before any attempt to test types, we should substitute generics. See if this assumption is true (there will surely be a counterexample someday)"),
                (Type::Primitive(idx1), Type::Primitive(idx2)) => idx1 == idx2,
                (Type::GenericInstance(struct_id_1, generic_ids_1), Type::GenericInstance(struct_id_2, generic_ids_2)) => {
                    if struct_id_1 != struct_id_2 || generic_ids_1.len() != generic_ids_2.len() {
                        return false;
                    }
                    for (generic_type_id_1, generic_type_id_2) in generic_ids_1.iter().zip(generic_ids_2.iter()) {
                        if !type_satisfies_other_impl(zelf, generic_type_id_1, generic_type_id_2, substitutions) {
                            return false;
                        }
                    }

                    true
                }
                (Type::GenericInstance(struct_id, _), _) if *struct_id == zelf.project.prelude_option_struct_id => {
                    false
                }
                (_, Type::GenericInstance(struct_id, generic_ids)) if *struct_id == zelf.project.prelude_option_struct_id => {
                    type_satisfies_other_impl(zelf, base_type_id, &generic_ids[0], substitutions)
                }
                (Type::GenericEnumInstance(enum_id, generic_ids, _), Type::GenericEnumInstance(target_enum_id, target_generic_ids, _)) => {
                    if enum_id != target_enum_id || generic_ids.len() != target_generic_ids.len() { return false; }

                    for (generic_type_id_1, generic_type_id_2) in generic_ids.iter().zip(target_generic_ids.iter()) {
                        if !type_satisfies_other_impl(zelf, generic_type_id_1, generic_type_id_2, substitutions) { return false; }
                    }

                    true
                }
                (Type::Function(base_param_type_ids, base_num_req, _, base_return_type_id), Type::Function(target_param_type_ids, _, _, target_return_type_id)) => {
                    let mut base_generics = zelf.extract_generic_slots(base_type_id);
                    let mut target_generics = zelf.extract_generic_slots(target_type_id);
                    let mut new_substitutions = substitutions.clone();
                    if !base_generics.is_empty() && !target_generics.is_empty() {
                        base_generics.drain(..).enumerate().for_each(|(idx, type_id)| { new_substitutions.insert(type_id, TypeId::placeholder_slot(idx)); });
                        target_generics.drain(..).enumerate().for_each(|(idx, type_id)| { new_substitutions.insert(type_id, TypeId::placeholder_slot(idx)); });
                    }

                    if !type_satisfies_other_impl(zelf, base_return_type_id, target_return_type_id, &new_substitutions) {
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
                        if !type_satisfies_other_impl(zelf, base_param_type_id, target_param_type_id, &new_substitutions) {
                            return false;
                        }
                    }

                    true
                }
                _ => false
            }
        }

        type_satisfies_other_impl(self, base_type, target_type, &HashMap::new())
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

                    let iter = hint_generic_ids.iter().zip(var_generic_ids.iter());
                    let mut new_var_generic_ids = Vec::with_capacity(iter.len());
                    for (hint_generic_type_id, var_generic_type_id) in iter {
                        new_var_generic_ids.push(self.substitute_generics(hint_generic_type_id, var_generic_type_id));
                    }
                    self.add_or_find_type_id(Type::GenericInstance(var_struct_id, new_var_generic_ids))
                } else {
                    *var_type_id
                }
            }
            (Type::GenericEnumInstance(hint_enum_id, hint_generic_ids, _), Type::GenericEnumInstance(var_enum_id, var_generic_ids, var_variant_idx)) => {
                if var_enum_id == hint_enum_id && hint_generic_ids.len() == var_generic_ids.len() {
                    let hint_generic_ids = hint_generic_ids.clone();
                    let var_enum_id = *var_enum_id;
                    let var_generic_ids = var_generic_ids.clone();
                    let var_variant_idx = *var_variant_idx;

                    let iter = hint_generic_ids.iter().zip(var_generic_ids.iter());
                    let mut new_var_generic_ids = Vec::with_capacity(iter.len());
                    for (hint_generic_type_id, var_generic_type_id) in iter {
                        new_var_generic_ids.push(self.substitute_generics(hint_generic_type_id, var_generic_type_id));
                    }
                    self.add_or_find_type_id(Type::GenericEnumInstance(var_enum_id, new_var_generic_ids, var_variant_idx))
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
            (Type::Generic(_, _), _) => {
                substitutions.insert(*type_id_containing_generics, *hint_type_id);
            }
            (Type::GenericInstance(s_id1, g_ids1), Type::GenericInstance(s_id2, g_ids2)) if s_id1 == s_id2 => {
                debug_assert!(g_ids1.len() == g_ids2.len());

                for (g_id1, g_id2) in g_ids1.iter().zip(g_ids2) {
                    self.extract_values_for_generics(g_id2, g_id1, substitutions);
                }
            }
            (Type::GenericEnumInstance(e_id1, g_ids1, _), Type::GenericEnumInstance(e_id2, g_ids2, _)) if e_id1 == e_id2 => {
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
                let mut resolved_type_id = substitutions.get(&type_id)
                    .map(|substituted_type_id| *substituted_type_id)
                    .unwrap_or(*type_id);
                // The `substitutions` map is not flattened; it's possible that, in order to resolve
                // a generic's substitution, it may point to another generic which also needs to have
                // substitution applied (and so on). For example, consider this structure:
                //   type A<AT> { x: AT }
                //   type B<BT> { a: A<BT> }
                //   val b = B(a: A(x: 12))
                // When resolving ^^^^^^^^, the typechecker has the following as its known substitutions:
                //   { AT -> BT, BT -> Int }
                // In order to learn that AT -> Int, we need to flatten the map. In doing so, we bubble
                // up the proper type, so the instantiation of B correctly typechecks to `B<Int>`.
                // TODO: This could maybe be improved by flattening as it's being built (in `extract_values_for_generics`).
                while self.type_contains_generics(&resolved_type_id) {
                    if let Some(type_id) = substitutions.get(&resolved_type_id) {
                        if resolved_type_id == *type_id { break; }
                        resolved_type_id = *type_id;
                    } else {
                        break;
                    }
                }

                resolved_type_id
            }
            Type::GenericInstance(struct_id, generic_ids) => {
                let substituted_generic_ids = generic_ids.iter().map(|generic_type_id| self.substitute_generics_with_known(generic_type_id, substitutions)).collect();
                self.add_or_find_type_id(Type::GenericInstance(struct_id, substituted_generic_ids))
            }
            Type::GenericEnumInstance(enum_id, generic_ids, variant_idx) => {
                let substituted_generic_ids = generic_ids.iter().map(|generic_type_id| self.substitute_generics_with_known(generic_type_id, substitutions)).collect();
                self.add_or_find_type_id(Type::GenericEnumInstance(enum_id, substituted_generic_ids, variant_idx))
            }
            Type::Function(arg_type_ids, num_required_params, is_variadic, ret_type_id) => {
                let substituted_arg_type_ids = arg_type_ids.iter().map(|arg_type_id| self.substitute_generics_with_known(arg_type_id, substitutions)).collect();
                let substituted_ret_type_id = self.substitute_generics_with_known(&ret_type_id, substitutions);
                self.add_or_find_type_id(self.project.function_type(substituted_arg_type_ids, num_required_params, is_variadic, substituted_ret_type_id))
            }
            Type::Primitive(_) | Type::Type(_) | Type::ModuleAlias => *type_id,
        }
    }

    fn extract_generic_slots(&self, type_id: &TypeId) -> Vec<TypeId> {
        #[inline]
        fn extract_generic_slots_impl<L: LoadModule>(zelf: &Typechecker2<L>, type_id: &TypeId, generics: &mut Vec<TypeId>) {
            match zelf.project.get_type_by_id(type_id) {
                Type::Primitive(_) | Type::ModuleAlias => {}
                Type::Generic(_, _) => {
                    generics.push(*type_id);
                }
                Type::GenericInstance(_, generic_ids) => {
                    for type_id in generic_ids {
                        extract_generic_slots_impl(zelf, type_id, generics);
                    }
                }
                Type::GenericEnumInstance(_, generic_ids, _) => {
                    for type_id in generic_ids {
                        extract_generic_slots_impl(zelf, type_id, generics);
                    }
                }
                Type::Function(param_type_ids, _, _, return_type_id) => {
                    for type_id in param_type_ids {
                        extract_generic_slots_impl(zelf, type_id, generics);
                    }
                    extract_generic_slots_impl(zelf, return_type_id, generics);
                }
                Type::Type(id) => {
                    let generic_ids = match id {
                        TypeKind::Struct(struct_id) => &zelf.project.get_struct_by_id(struct_id).generic_ids,
                        TypeKind::Enum(enum_id) => &zelf.project.get_enum_by_id(enum_id).generic_ids,
                    };
                    for type_id in generic_ids {
                        extract_generic_slots_impl(zelf, type_id, generics);
                    }
                }
            }
        }

        let mut generics = Vec::new();
        extract_generic_slots_impl(self, type_id, &mut generics);
        generics.into_iter().unique().collect()
    }

    fn type_contains_generics(&self, type_id: &TypeId) -> bool {
        !self.extract_generic_slots(type_id).is_empty()
    }

    fn resolve_type_identifier(&mut self, type_identifier: &TypeIdentifier) -> Result<TypeId, TypeError> {
        match type_identifier {
            TypeIdentifier::Normal { ident, type_args } => {
                let default_type_args = vec![];
                let type_args = type_args.as_ref().unwrap_or(&default_type_args);
                let mut generic_ids = Vec::with_capacity(type_args.len());
                for type_arg_identifier in type_args {
                    let type_id = self.resolve_type_identifier(&type_arg_identifier)?;
                    generic_ids.push(type_id);
                }

                let assert_expected_type_args = |num_required_args: usize| -> Result<(), TypeError> {
                    let num_provided_args = generic_ids.len();
                    if num_required_args != num_provided_args {
                        let range = if num_provided_args > num_required_args {
                            type_args[num_required_args].get_ident().get_range().expand(&type_args[num_provided_args - 1].get_ident().get_range())
                        } else {
                            ident.get_range()
                        };

                        let span = self.make_span(&range);
                        return Err(TypeError::InvalidTypeArgumentArity { span, num_required_args, num_provided_args });
                    }

                    Ok(())
                };
                let assert_no_type_args = || assert_expected_type_args(0);

                let ident_name = Token::get_ident_name(ident);
                match ident_name.as_str() {
                    "Unit" => assert_no_type_args().and(Ok(PRELUDE_UNIT_TYPE_ID)),
                    "Any" => assert_no_type_args().and(Ok(PRELUDE_ANY_TYPE_ID)),
                    "Int" => assert_no_type_args().and(Ok(PRELUDE_INT_TYPE_ID)),
                    "Float" => assert_no_type_args().and(Ok(PRELUDE_FLOAT_TYPE_ID)),
                    "Bool" => assert_no_type_args().and(Ok(PRELUDE_BOOL_TYPE_ID)),
                    "String" => assert_no_type_args().and(Ok(PRELUDE_STRING_TYPE_ID)),
                    _ => {
                        if let Some(generic_type_id) = self.project.find_type_id_for_generic(&self.current_scope_id, &ident_name) {
                            if let Some(first) = type_args.get(0) {
                                let span = self.make_span(&first.get_ident().get_range());
                                return Err(TypeError::InvalidTypeArgumentArity { span, num_required_args: 0, num_provided_args: type_args.len() });
                            }

                            return Ok(generic_type_id);
                        }

                        if let Some(struct_) = self.get_struct_by_name(&ident_name) {
                            assert_expected_type_args(struct_.generic_ids.len())?;

                            let struct_id = struct_.id;
                            Ok(self.add_or_find_type_id(Type::GenericInstance(struct_id, generic_ids)))
                        } else if let Some(enum_) = self.get_enum_by_name(&ident_name) {
                            assert_expected_type_args(enum_.generic_ids.len())?;

                            let enum_id = enum_.id;
                            Ok(self.add_or_find_type_id(Type::GenericEnumInstance(enum_id, generic_ids, None)))
                        } else {
                            Err(TypeError::UnknownType { span: self.make_span(&ident.get_range()), name: ident_name })
                        }
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

    fn add_variable_to_current_scope(&mut self, name: String, type_id: TypeId, is_mutable: bool, is_initialized: bool, span: &Span, is_parameter: bool) -> Result<VarId, TypeError> {
        let current_scope = self.current_scope_mut();

        for var in &current_scope.vars {
            if var.name == name && name != "_" {
                return Err(TypeError::DuplicateName { span: span.clone(), name, original_span: var.defined_span.clone(), kind: DuplicateNameKind::Variable });
            }
        }

        let id = VarId(current_scope.id, current_scope.vars.len());
        let var = Variable { id, name, type_id, is_mutable, is_initialized, defined_span: Some(span.clone()), is_captured: false, alias: VariableAlias::None, is_parameter };
        current_scope.vars.push(var);

        Ok(id)
    }

    fn add_function_variable_alias_to_current_scope(&mut self, ident: &Token, func_id: &FuncId) -> Result<VarId, TypeError> {
        let func = self.project.get_func_by_id(func_id);
        let name = Token::get_ident_name(ident);
        let span = self.make_span(&ident.get_range());

        let fn_type_id = self.add_or_find_type_id(self.project.function_type_for_function(&func, false));
        let fn_var_id = self.add_variable_to_current_scope(name, fn_type_id, false, true, &span, false)?;
        let variable = self.project.get_var_by_id_mut(&fn_var_id);
        variable.alias = VariableAlias::Function(*func_id);

        Ok(fn_var_id)
    }

    fn add_function_to_current_scope(&mut self, fn_scope_id: ScopeId, name_token: &Token, generic_ids: Vec<TypeId>, has_self: bool, params: Vec<FunctionParam>, return_type_id: TypeId) -> Result<FuncId, TypeError> {
        let is_method = self.current_type_decl.is_some();
        let current_scope = self.current_scope_mut();

        let name = Token::get_ident_name(name_token);
        let span = Span::from_range(current_scope.id.0, name_token.get_range());
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
        format!("{}{}_{}_{}", LAMBDA_FN_NAME_PREFIX, self.current_module().id.0, name.into_iter().join("_"), current_scope.funcs.len())
    }

    fn add_lambda_function_to_scope(&mut self, fn_decl_scope_id: &ScopeId, fn_scope_id: ScopeId, params: Vec<FunctionParam>) -> Result<FuncId, TypeError> {
        let name = self.new_lambda_fn_name();

        let fn_decl_scope = self.project.get_scope_by_id_mut(fn_decl_scope_id);

        let func_id = FuncId(fn_decl_scope.id, fn_decl_scope.funcs.len());
        let func = Function { id: func_id, fn_scope_id, name, generic_ids: vec![], has_self: false, params, return_type_id: PRELUDE_ANY_TYPE_ID, defined_span: None, body: vec![], captured_vars: vec![] };

        fn_decl_scope.funcs.push(func);

        Ok(func_id)
    }

    fn verify_type_name_unique_in_module(&self, module: &TypedModule, name: &String, range: &Range) -> Result<(), TypeError> {
        for struct_ in &module.structs {
            if struct_.name == *name {
                let span = self.make_span(range);
                return Err(TypeError::DuplicateName { span: span.clone(), name: name.clone(), original_span: struct_.defined_span.clone(), kind: DuplicateNameKind::Type });
            }
        }

        for enum_ in &module.enums {
            if enum_.name == *name {
                let span = self.make_span(range);
                return Err(TypeError::DuplicateName { span: span.clone(), name: name.clone(), original_span: Some(enum_.defined_span.clone()), kind: DuplicateNameKind::Enum });
            }
        }

        return Ok(());
    }

    fn add_struct_to_current_module(&mut self, struct_scope_id: ScopeId, name_token: &Token, generic_ids: Vec<TypeId>) -> Result<StructId, TypeError> {
        let current_module = self.current_module();

        let name = Token::get_ident_name(name_token);
        self.verify_type_name_unique_in_module(&current_module, &name, &name_token.get_range())?;
        let span = Span::from_range(current_module.id, name_token.get_range());

        let struct_id = StructId(current_module.id, current_module.structs.len());
        let self_type_id = self.add_or_find_type_id(Type::GenericInstance(struct_id, generic_ids.clone()));

        let struct_ = Struct {
            id: struct_id,
            struct_scope_id,
            name: name.clone(),
            defined_span: Some(span.clone()),
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
        variable.alias = VariableAlias::Type(TypeKind::Struct(struct_id));

        Ok(struct_id)
    }

    fn add_enum_to_current_module(&mut self, enum_scope_id: ScopeId, name_token: &Token, generic_ids: Vec<TypeId>) -> Result<EnumId, TypeError> {
        let current_module = self.current_module();

        let name = Token::get_ident_name(name_token);
        self.verify_type_name_unique_in_module(&current_module, &name, &name_token.get_range())?;
        let span = Span::from_range(current_module.id, name_token.get_range());

        let enum_id = EnumId(current_module.id, current_module.enums.len());
        let self_type_id = self.add_or_find_type_id(Type::GenericEnumInstance(enum_id, generic_ids.clone(), None));

        let enum_ = Enum {
            id: enum_id,
            enum_scope_id,
            name: name.clone(),
            defined_span: span.clone(),
            generic_ids,
            self_type_id,
            variants: vec![],
            methods: vec![],
            static_methods: vec![],
        };
        self.current_module_mut().enums.push(enum_);

        let enum_type_id = self.add_or_find_type_id(self.project.enum_type(enum_id));
        let enum_var_id = self.add_variable_to_current_scope(name, enum_type_id, false, true, &span, false)?;
        let variable = self.project.get_var_by_id_mut(&enum_var_id);
        variable.alias = VariableAlias::Type(TypeKind::Enum(enum_id));

        Ok(enum_id)
    }

    fn create_child_scope<S: AsRef<str>>(&mut self, label: S, kind: ScopeKind) -> ScopeId {
        let parent_scope = self.current_scope_id;
        let current_module = self.current_module_mut();
        let new_scope_id = ScopeId(current_module.id, current_module.scopes.len());

        let child_scope = Scope {
            label: label.as_ref().to_string(),
            kind,
            terminator: None,
            id: new_scope_id,
            parent: Some(parent_scope),
            types: vec![],
            vars: vec![],
            funcs: vec![],
        };
        current_module.scopes.push(child_scope);

        new_scope_id
    }

    fn begin_child_scope<S: AsRef<str>>(&mut self, label: S, kind: ScopeKind) -> ScopeId {
        self.current_scope_id = self.create_child_scope(label, kind);

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

        self.module_loader.register(&parser::ast::ModuleId::prelude(), &PRELUDE_MODULE_ID);
        let mut prelude_module = TypedModule { id: PRELUDE_MODULE_ID, name: "prelude".to_string(), type_ids: vec![], functions: vec![], structs: vec![], enums: vec![], code: vec![], scopes: vec![], exports: HashMap::new(), completed: false };
        let mut prelude_scope = Scope { label: "prelude.root".to_string(), kind: ScopeKind::Module(PRELUDE_MODULE_ID), terminator: None, id: PRELUDE_SCOPE_ID, parent: None, types: vec![], vars: vec![], funcs: vec![] };

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
            let scope = Scope { label: "prelude.Option".to_string(), kind: ScopeKind::Type, terminator: None, id: scope_id, parent: Some(PRELUDE_SCOPE_ID), types: vec![], vars: vec![], funcs: vec![] };
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
            let scope = Scope { label: "prelude.None".to_string(), kind: ScopeKind::Type, terminator: None, id: scope_id, parent: Some(PRELUDE_SCOPE_ID), types: vec![], vars: vec![], funcs: vec![] };
            prelude_module.scopes.push(scope);
            let generic_t_type_id = self.project.add_type_id(&scope_id, Type::Generic(None, "T".to_string()));

            self.project.prelude_none_type_id = self.project.add_type_id(&PRELUDE_SCOPE_ID, self.project.option_type(generic_t_type_id));
        }

        self.current_scope_id = PRELUDE_SCOPE_ID;

        let prelude_stub_file = self.module_loader.load_file(&"prelude.stub.abra".to_string()).expect("Internal error: should always be able to load prelude");
        let parse_result = tokenize_and_parse_stub(&parser::ast::ModuleId::External("prelude".to_string()), &prelude_stub_file)
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
        debug_assert!(self.project.modules.len() >= 1 && self.project.modules[0].name == "prelude", "Prelude must be loaded in order to typecheck further modules");

        let (file_name, parse_result) = self.module_loader.load_untyped_ast(&m_id).map_err(Either::Left)?
            .expect("Internal error");

        let module_id = ModuleId(self.project.modules.len());
        self.module_loader.register(m_id, &module_id);

        let scope_id = ScopeId(module_id, 0);
        let label = format!("{:?}.root", &module_id);
        let root_scope = Scope { label, kind: ScopeKind::Module(module_id), terminator: None, id: scope_id, parent: Some(PRELUDE_SCOPE_ID), types: vec![], vars: vec![], funcs: vec![] };
        self.project.modules.push(TypedModule {
            id: module_id,
            name: file_name,
            type_ids: vec![],
            functions: vec![],
            structs: vec![],
            enums: vec![],
            code: vec![],
            scopes: vec![root_scope],
            exports: HashMap::new(),
            completed: false,
        });

        self.current_scope_id = scope_id;

        if !parse_result.imports.is_empty() {
            for (_, import_m_id, module_token) in parse_result.imports {
                if !self.module_loader.module_exists(&import_m_id) { continue; }
                if let Some(m) = self.module_loader.get_module_id(&import_m_id).and_then(|module_id| self.project.modules.get(module_id.0)) {
                    if m.completed { continue; }

                    let span = self.make_span(&module_token.get_range());
                    return Err(Either::Right(TypeError::CircularModuleImport { span }));
                }

                let mut tc = Typechecker2::new(self.module_loader, self.project);
                tc.typecheck_module(&import_m_id)?;
            }
        }

        self.current_scope_id = scope_id;
        self.typecheck_block(parse_result.nodes).map_err(Either::Right)?;

        self.current_module_mut().completed = true;

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
                AstNode::EnumDecl(_, node) => enum_decls.push(node),
                _ => {}
            }
        }

        // --- BEGIN PASS 0 for types, enums, and functions

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

        let num_enum_decls = enum_decls.len();
        let mut enum_ids = Vec::with_capacity(num_enum_decls);
        for node in &enum_decls {
            let enum_id = self.typecheck_enum_pass_0(node)?;
            enum_ids.push(enum_id);
        }

        let mut func_ids = Vec::with_capacity(func_decls.len());
        for node in &func_decls {
            let func_id = self.typecheck_function_pass_0(node)?;
            let func_var_id = self.add_function_variable_alias_to_current_scope(&node.name, &func_id)?;
            func_ids.push((func_id, func_var_id));
        }

        // --- END PASS 0 for types, enums, and functions
        // --- BEGIN PASS 1 for types, enums, and functions

        let mut struct_ids = VecDeque::from(struct_ids);
        debug_assert!(num_type_decls == struct_ids.len());
        for (node, struct_id) in type_decls.iter().zip(&struct_ids) {
            self.typecheck_struct_pass_1(node, &struct_id)?;
        }

        let mut enum_ids = VecDeque::from(enum_ids);
        debug_assert!(num_enum_decls == enum_ids.len());
        for (node, enum_id) in enum_decls.iter().zip(&enum_ids) {
            self.typecheck_enum_pass_1(node, &enum_id)?;
        }

        let mut func_ids = VecDeque::from(func_ids);
        debug_assert!(func_decls.len() == func_ids.len());
        for (node, (func_id, func_var_id)) in func_decls.iter().zip(&func_ids) {
            self.typecheck_function_pass_1(func_id, node, false)?;

            let func = self.project.get_func_by_id(func_id);
            let fn_type_id = self.add_or_find_type_id(self.project.function_type_for_function(&func, false));
            self.project.get_var_by_id_mut(func_var_id).type_id = fn_type_id;
        }

        // --- END PASS 1 for types, enums, and functions
        self.function_pass = FunctionPass::Pass2;

        for node in nodes {
            match node {
                AstNode::FunctionDecl(_, decl_node) => {
                    let (func_id, func_var_id) = func_ids.pop_front().expect("There should be a func_id for each function declaration in this block");
                    self.typecheck_function_pass_2(func_id, decl_node)?;

                    let func = self.project.get_func_by_id(&func_id);
                    let fn_type_id = self.add_or_find_type_id(self.project.function_type_for_function(&func, false));
                    self.project.get_var_by_id_mut(&func_var_id).type_id = fn_type_id;

                    let current_module = self.current_module_mut();
                    current_module.code.push(TypedNode::FuncDeclaration(func_id));
                }
                AstNode::TypeDecl(_, decl_node) => {
                    let struct_id = struct_ids.pop_front().expect("There should be a struct_id for each type declaration in this block");
                    self.typecheck_struct_pass_2(struct_id, decl_node)?;

                    let current_module = self.current_module_mut();
                    current_module.code.push(TypedNode::TypeDeclaration(struct_id));
                }
                AstNode::EnumDecl(_, decl_node) => {
                    let enum_id = enum_ids.pop_front().expect("There should be an enum_id for each enum declaration in this block");
                    self.typecheck_enum_pass_2(enum_id, decl_node)?;

                    let current_module = self.current_module_mut();
                    current_module.code.push(TypedNode::EnumDeclaration(enum_id));
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
                let span = self.make_span(&generic_ident.get_range());
                let Type::Generic(orig_range, name) = ty.clone() else { unreachable!("We know it's a generic since it was identified as such") };
                let original_span = orig_range.map(|orig_range| self.make_span(&orig_range));
                return Err(TypeError::DuplicateName { span, name: name.clone(), original_span, kind: DuplicateNameKind::TypeArgument });
            }

            let generic_id = self.project.add_or_find_type_id(&scope_id, Type::Generic(Some(generic_ident.get_range()), generic_name));
            generic_ids.push(generic_id);
        }

        Ok(generic_ids)
    }

    fn verify_export_scope(&self, export_token: &Token) -> Result<(), TypeError> {
        let ScopeId(_, scope_idx) = self.current_scope_id;
        if scope_idx == 0 {
            Ok(())
        } else {
            Err(TypeError::InvalidExportScope { span: self.make_span(&export_token.get_range()) })
        }
    }

    fn typecheck_function_parameters_pass_1(&mut self, allow_self: bool, parameters: &Vec<(Parameter, Option<TypeId>)>, do_partial_completion: bool) -> Result<Vec<FunctionParam>, TypeError> {
        let mut seen_self = false;
        let mut seen_param_names = HashSet::new();
        let mut seen_default_valued_params = false;
        let mut params = vec![];
        let num_params = parameters.len();
        for (idx, (Parameter { ident, type_ident, is_vararg, default_value }, type_hint)) in parameters.iter().enumerate() {
            let ident_span = self.make_span(&ident.get_range());

            if *is_vararg && idx != num_params - 1 {
                return Err(TypeError::InvalidVarargPosition { span: ident_span });
            }

            let param_name = Token::get_ident_name(ident);

            if let Token::Self_(_) = &ident {
                let self_type_id = match &self.current_type_decl {
                    Some(type_id) if allow_self => *type_id,
                    _ => return Err(TypeError::InvalidSelfParam { span: ident_span }),
                };

                if seen_self || idx != 0 {
                    return Err(TypeError::InvalidSelfParamPosition { span: ident_span });
                }
                seen_self = true;

                let var_id = self.add_variable_to_current_scope(param_name.clone(), self_type_id, false, true, &ident_span, true)?;
                params.push(FunctionParam { name: param_name, type_id: self_type_id, var_id, defined_span: Some(ident_span.clone()), default_value: None, is_variadic: false, is_incomplete: false });

                continue;
            }

            if seen_param_names.contains(&param_name) {
                return Err(TypeError::DuplicateParameter { span: ident_span, name: param_name });
            }
            seen_param_names.insert(param_name.clone());

            let mut param_type_id = None;
            if let Some(type_ident) = type_ident {
                param_type_id = Some(self.resolve_type_identifier(type_ident)?);
            }
            if let Some(type_hint) = type_hint {
                if let Some(param_type_id) = param_type_id {
                    if !self.type_satisfies_other(&param_type_id, type_hint) {
                        let span = self.make_span(&ident.get_range());
                        return Err(TypeError::TypeMismatch { span, expected: vec![*type_hint], received: param_type_id });
                    }
                } else {
                    param_type_id = Some(*type_hint);
                }
            }
            let typed_default_value_expr = if let Some(default_value) = default_value {
                seen_default_valued_params = true;

                debug_assert!(if let FunctionPass::Pass1 { just_saw_function_call } = &self.function_pass { *just_saw_function_call == false } else { true }, "This flag should be un-set after each possible param default value");
                match self.typecheck_expression(default_value.clone(), param_type_id) {
                    Ok(typed_expr) => {
                        if let FunctionPass::Pass1 { just_saw_function_call } = &mut self.function_pass {
                            *just_saw_function_call = false;
                        };

                        Some(typed_expr)
                    }
                    Err(_) if do_partial_completion && matches!(self.function_pass, FunctionPass::Pass1 { just_saw_function_call: true }) => {
                        if let FunctionPass::Pass1 { just_saw_function_call } = &mut self.function_pass {
                            *just_saw_function_call = false;
                        };

                        let type_id = PRELUDE_ANY_TYPE_ID;
                        let var_id = self.add_variable_to_current_scope(param_name.clone(), type_id, false, true, &ident_span, true)?;
                        params.push(FunctionParam {
                            name: param_name,
                            type_id,
                            var_id,
                            defined_span: Some(ident_span),
                            default_value: None,
                            is_variadic: *is_vararg,
                            is_incomplete: true,
                        });
                        continue;
                    }
                    Err(e) => return Err(e)
                }
            } else {
                if seen_default_valued_params {
                    return Err(TypeError::InvalidRequiredParamPosition { span: ident_span, is_variadic: *is_vararg });
                }
                None
            };
            let type_id = match (param_type_id, &typed_default_value_expr) {
                (None, None) => return Err(TypeError::UnknownTypeForParameter { span: ident_span, param_name }),
                (Some(param_type_id), None) => param_type_id,
                (None, Some(typed_default_value_expr)) => *typed_default_value_expr.type_id(),
                (Some(param_type_id), Some(typed_default_value_expr)) => {
                    let default_value_type_id = typed_default_value_expr.type_id();
                    if !self.type_satisfies_other(default_value_type_id, &param_type_id) {
                        return Err(TypeError::TypeMismatch {
                            span: self.make_span(&typed_default_value_expr.span()),
                            expected: vec![param_type_id],
                            received: *default_value_type_id,
                        });
                    }
                    param_type_id
                }
            };

            let is_incomplete = do_partial_completion && typed_default_value_expr.is_some();
            let default_value = if do_partial_completion { None } else { typed_default_value_expr };
            let var_id = self.add_variable_to_current_scope(param_name.clone(), type_id, false, true, &ident_span, true)?;

            let mut param_type_id = type_id;
            if *is_vararg {
                let Some(inner_type_id) = self.type_is_array(&type_id) else {
                    return Err(TypeError::InvalidVarargType { span: ident_span, type_id });
                };
                param_type_id = inner_type_id;
            };
            params.push(FunctionParam {
                name: param_name,
                type_id: param_type_id,
                var_id,
                defined_span: Some(ident_span),
                default_value,
                is_variadic: *is_vararg,
                is_incomplete,
            });
        }

        Ok(params)
    }

    fn typecheck_function_pass_0(&mut self, node: &FunctionDeclNode) -> Result<FuncId, TypeError> {
        self.function_pass = FunctionPass::Pass0;

        let FunctionDeclNode { export_token, name, type_args, args, ret_type, .. } = node;
        let is_exported = export_token.is_some();
        if let Some(export_token) = export_token { self.verify_export_scope(export_token)?; }

        let fn_scope_id = self.begin_child_scope(format!("{:?}.{}", &self.current_module().id, Token::get_ident_name(&node.name)), ScopeKind::Function(FuncId::BOGUS));

        let generic_ids = self.add_generics_to_scope(&fn_scope_id, type_args)?;
        let has_self = args.first().map(|(tok, _, _, _)| matches!(tok, Token::Self_(_))).unwrap_or(false);
        let mut return_type_id = PRELUDE_UNIT_TYPE_ID;
        if let Some(ret_type) = ret_type {
            return_type_id = self.resolve_type_identifier(ret_type)?;
        }

        self.end_child_scope();

        let func_name = Token::get_ident_name(name);
        let func_id = self.add_function_to_current_scope(fn_scope_id, name, generic_ids, has_self, vec![], return_type_id)?;
        self.current_module_mut().functions.push(func_id);
        let ScopeKind::Function(id) = &mut self.project.get_scope_by_id_mut(&fn_scope_id).kind else { unreachable!() };
        *id = func_id;

        if is_exported {
            self.current_module_mut().exports.insert(func_name, ExportedValue::Function(func_id));
        }

        Ok(func_id)
    }

    fn typecheck_function_pass_1(&mut self, func_id: &FuncId, node: &FunctionDeclNode, allow_self: bool) -> Result<(), TypeError> {
        self.function_pass = FunctionPass::Pass1 { just_saw_function_call: false };

        let func = self.project.get_func_by_id(&func_id);
        let prev_scope_id = self.current_scope_id;
        self.current_scope_id = func.fn_scope_id;

        let node_parameters = node.parameters().into_iter().map(|p| (p, None)).collect();
        let params = self.typecheck_function_parameters_pass_1(allow_self, &node_parameters, true)?;

        debug_assert!(self.project.get_func_by_id(func_id).params.is_empty(), "In pass 0, an empty list of parameters should have been inserted, which we here overwrite");
        self.project.get_func_by_id_mut(func_id).params = params;

        self.current_scope_id = prev_scope_id;

        Ok(())
    }

    fn typecheck_function_pass_2(&mut self, func_id: FuncId, node: FunctionDeclNode) -> Result<(), TypeError> {
        debug_assert!(self.function_pass == FunctionPass::Pass2);

        let prev_func_id = self.current_function;
        self.current_function = Some(func_id);

        let func = self.project.get_func_by_id(&func_id);
        let prev_scope_id = self.current_scope_id;
        self.current_scope_id = func.fn_scope_id;

        let func_name = func.name.clone();
        let return_type_id = func.return_type_id;

        let node_parameters = node.args;
        debug_assert!(node_parameters.len() == func.params.len());
        for (param_idx, (_, _, _, default_value)) in (0..func.params.len()).zip(node_parameters) {
            let param = &self.project.get_func_by_id(&func_id).params[param_idx];
            if !param.is_incomplete { continue; }

            debug_assert!(param.default_value.is_none(), "There should be no reason for incompleteness other than lack of fully-typed default value");
            let Some(default_value) = default_value else {
                unreachable!("Internal error: misalignment attempting to hydrate parameter's default value");
            };

            let mut param_type_id = param.type_id;
            let type_hint = if param_type_id == PRELUDE_ANY_TYPE_ID { None } else { Some(param_type_id) };
            let typed_default_value = self.typecheck_expression(default_value, type_hint)?;
            let type_id = *typed_default_value.type_id();

            // If the default value expression contains an unbound generic, this is an unacceptable state.
            if self.type_contains_generics(&type_id) {
                return Err(TypeError::ForbiddenAssignment { span: self.make_span(&typed_default_value.span()), type_id, purpose: "parameter" });
            }

            // If the param's known type is the sentinel value `Any`, then don't perform any typechecks.
            if param_type_id != PRELUDE_ANY_TYPE_ID {
                if self.type_contains_generics(&param_type_id) {
                    param_type_id = self.substitute_generics(&type_id, &param_type_id);
                }
                if !self.type_satisfies_other(&type_id, &param_type_id) {
                    return Err(TypeError::TypeMismatch { span: self.make_span(&typed_default_value.span()), expected: vec![param_type_id], received: type_id });
                }
            }

            let mut param = &mut self.project.get_func_by_id_mut(&func_id).params[param_idx];
            param.default_value = Some(typed_default_value);
            param.type_id = type_id;
            param.is_incomplete = false;

            let param_var_id = param.var_id;
            let param_var = self.project.get_var_by_id_mut(&param_var_id);
            param_var.type_id = type_id;
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
                let span = self.make_span(&typed_node.span());
                return Err(TypeError::ReturnTypeMismatch { span, func_name, expected: return_type_id, received: *type_id });
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
        let TypeDeclNode { export_token, name, type_args, fields, .. } = node;
        let is_exported = export_token.is_some();
        if let Some(export_token) = export_token { self.verify_export_scope(export_token)?; }

        let struct_name = Token::get_ident_name(&name);
        let struct_scope_id = self.create_child_scope(format!("{:?}.{}", &self.current_module().id, &struct_name), ScopeKind::Type);

        // A struct's generics are scoped to the struct declaration, but the instance type should be scoped to the outer scope.
        let generic_ids = self.add_generics_to_scope(&struct_scope_id, type_args)?;
        let struct_id = self.add_struct_to_current_module(struct_scope_id, name, generic_ids)?;

        let prev_scope_id = self.current_scope_id;
        self.current_scope_id = struct_scope_id;

        let mut seen_fields: HashMap<String, Token> = HashMap::new();
        for TypeDeclField { ident, type_ident, readonly, .. } in fields {
            let is_readonly = readonly.is_some();

            let field_name = Token::get_ident_name(&ident);
            if let Some(orig_field) = seen_fields.get(&field_name) {
                let span = self.make_span(&ident.get_range());
                let original_span = Some(self.make_span(&orig_field.get_range()));
                return Err(TypeError::DuplicateName { span, name: field_name, original_span, kind: DuplicateNameKind::Field });
            }
            seen_fields.insert(field_name.clone(), ident.clone());

            let field_type_id = self.resolve_type_identifier(&type_ident)?;
            let field = StructField {
                name: field_name,
                type_id: field_type_id,
                defined_span: self.make_span(&ident.get_range()),
                is_readonly,
                default_value: None,
            };
            self.project.get_struct_by_id_mut(&struct_id).fields.push(field);
        }

        self.current_scope_id = prev_scope_id;

        if is_exported {
            self.current_module_mut().exports.insert(struct_name, ExportedValue::Type(TypeKind::Struct(struct_id)));
        }

        Ok(struct_id)
    }

    fn typecheck_struct_pass_1(&mut self, node: &TypeDeclNode, struct_id: &StructId) -> Result<(), TypeError> {
        let TypeDeclNode { methods, .. } = node;

        let struct_ = self.project.get_struct_by_id(struct_id);

        let prev_scope_id = self.current_scope_id;
        self.current_scope_id = struct_.struct_scope_id;
        debug_assert!(self.current_type_decl.is_none(), "At the moment, types cannot be nested within other types");
        self.current_type_decl = Some(struct_.self_type_id);

        for method in methods {
            let AstNode::FunctionDecl(_, decl_node) = method else { unreachable!("Internal error: a type's methods must be of type AstNode::FunctionDecl") };
            let func_id = self.typecheck_function_pass_0(decl_node)?;

            let is_method = decl_node.args.get(0).map(|(token, _, _, _)| matches!(token, Token::Self_(_))).unwrap_or(false);
            if is_method {
                self.project.get_struct_by_id_mut(struct_id).methods.push(func_id);
            } else {
                self.project.get_struct_by_id_mut(struct_id).static_methods.push(func_id);
            }

            self.typecheck_function_pass_1(&func_id, decl_node, true)?;
        }

        self.current_type_decl = None;

        self.current_scope_id = prev_scope_id;

        Ok(())
    }

    fn typecheck_struct_pass_2(&mut self, struct_id: StructId, node: TypeDeclNode) -> Result<(), TypeError> {
        let TypeDeclNode { fields, methods, .. } = node;
        let struct_ = self.project.get_struct_by_id(&struct_id);

        let prev_scope_id = self.current_scope_id;
        self.current_scope_id = struct_.struct_scope_id;

        for (idx, TypeDeclField { default_value, .. }) in fields.into_iter().enumerate() {
            let Some(default_value_node) = default_value else { continue; };
            let struct_ = self.project.get_struct_by_id(&struct_id);
            let field_type_id = struct_.fields[idx].type_id;
            let default_value = self.typecheck_expression(default_value_node, Some(field_type_id))?;

            let mut type_id = *default_value.type_id();

            if self.type_contains_generics(&type_id) {
                type_id = self.substitute_generics(&field_type_id, &type_id);
            }
            if !self.type_satisfies_other(&type_id, &field_type_id) {
                let span = self.make_span(&default_value.span());
                return Err(TypeError::TypeMismatch { span, expected: vec![field_type_id], received: type_id });
            }

            let struct_ = self.project.get_struct_by_id_mut(&struct_id);
            struct_.fields[idx].default_value = Some(default_value);
        }

        let mut method_func_id_idx = 0;
        let mut static_method_func_id_idx = 0;
        for method in methods.into_iter() {
            let AstNode::FunctionDecl(_, decl_node) = method else { unreachable!("Internal error: a type's methods must be of type AstNode::FunctionDecl") };
            let is_method = decl_node.args.get(0).map(|(token, _, _, _)| if let Token::Self_(_) = token { true } else { false }).unwrap_or(false);

            let struct_ = self.project.get_struct_by_id(&struct_id);
            let func_id = if is_method {
                method_func_id_idx += 1;
                struct_.methods[method_func_id_idx - 1]
            } else {
                static_method_func_id_idx += 1;
                struct_.static_methods[static_method_func_id_idx - 1]
            };

            self.typecheck_function_pass_2(func_id, decl_node)?;
        }

        self.current_scope_id = prev_scope_id;

        Ok(())
    }

    fn typecheck_enum_pass_0(&mut self, node: &EnumDeclNode) -> Result<EnumId, TypeError> {
        let EnumDeclNode { export_token, name, type_args, .. } = node;
        let is_exported = export_token.is_some();
        if let Some(export_token) = export_token { self.verify_export_scope(export_token)?; }

        let enum_name = Token::get_ident_name(&name);
        let enum_scope_id = self.create_child_scope(format!("{:?}.{}", &self.current_module().id, &enum_name), ScopeKind::Type);

        // An enum's generics are scoped to the enum declaration, but the instance type should be scoped to the outer scope.
        let generic_ids = self.add_generics_to_scope(&enum_scope_id, type_args)?;
        let enum_id = self.add_enum_to_current_module(enum_scope_id, name, generic_ids)?;
        debug_assert!(self.current_type_decl.is_none(), "At the moment, types cannot be nested within other types");

        if is_exported {
            self.current_module_mut().exports.insert(enum_name, ExportedValue::Type(TypeKind::Enum(enum_id)));
        }

        Ok(enum_id)
    }

    fn typecheck_enum_pass_1(&mut self, node: &EnumDeclNode, enum_id: &EnumId) -> Result<(), TypeError> {
        let EnumDeclNode { variants, methods, .. } = node;

        let enum_ = self.project.get_enum_by_id(enum_id);

        self.current_scope_id = enum_.enum_scope_id;
        self.current_type_decl = Some(enum_.self_type_id);

        let mut seen_variants = HashMap::<String, &Token>::new();
        for (variant_ident, variant_args) in variants {
            let name = Token::get_ident_name(variant_ident);
            let variant_ident_range = variant_ident.get_range();
            if let Some(seen_variant) = seen_variants.get(&name) {
                let span = self.make_span(&variant_ident_range);
                let original_span = Some(self.make_span(&seen_variant.get_range()));
                return Err(TypeError::DuplicateName { span, name, original_span, kind: DuplicateNameKind::EnumVariant });
            }
            seen_variants.insert(name.clone(), variant_ident);

            let kind = if variant_args.is_some() { EnumVariantKind::Container(FuncId::BOGUS) } else { EnumVariantKind::Constant };
            let defined_span = self.make_span(&variant_ident_range);
            let variant = EnumVariant { name, defined_span, kind };
            self.project.get_enum_by_id_mut(enum_id).variants.push(variant);
        }

        for method in methods {
            let AstNode::FunctionDecl(_, decl_node) = method else { unreachable!("Internal error: an enum's methods must be of type AstNode::FunctionDecl") };
            let func_id = self.typecheck_function_pass_0(decl_node)?;

            let is_method = decl_node.args.get(0).map(|(token, _, _, _)| matches!(token, Token::Self_(_))).unwrap_or(false);
            if is_method {
                self.project.get_enum_by_id_mut(enum_id).methods.push(func_id);
            } else {
                let enum_ = self.project.get_enum_by_id(&enum_id);
                let func_name = Token::get_ident_name(&decl_node.name);
                if let Some(variant) = enum_.variants.iter().find(|v| v.name == func_name) {
                    let func_name_span = self.make_span(&decl_node.name.get_range());
                    return Err(TypeError::DuplicateName { span: func_name_span, name: func_name, original_span: Some(variant.defined_span.clone()), kind: DuplicateNameKind::StaticMethodOrVariant });
                }

                self.project.get_enum_by_id_mut(enum_id).static_methods.push(func_id);
            }

            self.typecheck_function_pass_1(&func_id, decl_node, true)?;
        }

        self.current_type_decl = None;

        self.end_child_scope();

        Ok(())
    }

    fn typecheck_enum_pass_2(&mut self, enum_id: EnumId, node: EnumDeclNode) -> Result<(), TypeError> {
        let EnumDeclNode { variants, methods, .. } = node;
        let enum_ = self.project.get_enum_by_id(&enum_id);
        let enum_variants = enum_.variants.clone(); // Need to clone, sadly :/
        let generic_ids = enum_.generic_ids.clone();

        let prev_scope_id = self.current_scope_id;
        self.current_scope_id = enum_.enum_scope_id;

        debug_assert!(enum_.variants.len() == variants.len());
        for (idx, ((variant_decl_token, variant_decl_args), variant)) in variants.iter().zip(&enum_variants).enumerate() {
            if let EnumVariantKind::Container(_) = variant.kind {
                let Some(variant_decl_args) = variant_decl_args else { unreachable!("Internal error: incorrectly assigned EnumVariantKind::Container") };
                let fn_scope_id = self.begin_child_scope(format!("{:?}.{}", &self.current_module().id, Token::get_ident_name(&node.name)), ScopeKind::Function(FuncId::BOGUS));

                let params = variant_decl_args.iter().map(|arg| (args_to_parameters(arg), None)).collect_vec();
                let params = self.typecheck_function_parameters_pass_1(false, &params, false)?;

                let return_type_id = self.add_or_find_type_id(Type::GenericEnumInstance(enum_id, generic_ids.clone(), Some(idx)));
                self.end_child_scope();

                let variant_func_id = self.add_function_to_current_scope(fn_scope_id, variant_decl_token, vec![], false, params, return_type_id)?;
                let EnumVariantKind::Container(func_id) = &mut self.project.get_enum_by_id_mut(&enum_id).variants[idx].kind else { unreachable!() };
                *func_id = variant_func_id;
                let ScopeKind::Function(id) = &mut self.project.get_scope_by_id_mut(&fn_scope_id).kind else { unreachable!() };
                *id = variant_func_id;
            }
        }

        let mut method_func_id_idx = 0;
        let mut static_method_func_id_idx = 0;
        for method in methods.into_iter() {
            let AstNode::FunctionDecl(_, decl_node) = method else { unreachable!("Internal error: an enum's methods must be of type AstNode::FunctionDecl") };
            let is_method = decl_node.args.get(0).map(|(token, _, _, _)| if let Token::Self_(_) = token { true } else { false }).unwrap_or(false);

            let enum_ = self.project.get_enum_by_id(&enum_id);
            let func_id = if is_method {
                method_func_id_idx += 1;
                enum_.methods[method_func_id_idx - 1]
            } else {
                static_method_func_id_idx += 1;
                enum_.static_methods[static_method_func_id_idx - 1]
            };
            self.typecheck_function_pass_2(func_id, decl_node)?;
        }

        self.current_scope_id = prev_scope_id;

        Ok(())
    }

    fn typecheck_statement(&mut self, node: AstNode, type_hint: Option<TypeId>) -> Result<TypedNode, TypeError> {
        if self.current_scope().terminator.is_some() {
            return Err(TypeError::UnreachableCode { span: self.make_span(&node.get_token().get_range()) });
        }

        match node {
            AstNode::BindingDecl(token, n) => {
                let BindingDeclNode { export_token, mut binding, type_ann, expr, is_mutable } = n;
                let is_exported = export_token.is_some();
                if let Some(export_token) = export_token { self.verify_export_scope(&export_token)?; }

                let type_hint_id = if let Some(type_identifier) = type_ann {
                    Some(self.resolve_type_identifier(&type_identifier)?)
                } else { None };

                let mut var_ids = vec![];
                let typed_expr = match (type_hint_id, expr) {
                    (None, None) => {
                        let span = self.make_span(&binding.get_span());
                        return Err(TypeError::MissingBindingInitializer { span, is_mutable });
                    }
                    (Some(type_hint_id), None) => {
                        if !is_mutable {
                            let span = self.make_span(&binding.get_span());
                            return Err(TypeError::MissingBindingInitializer { span, is_mutable });
                        }
                        self.typecheck_binding_pattern(is_mutable, false, &mut binding, &type_hint_id, &mut var_ids)?;

                        None
                    }
                    (None, Some(expr)) => {
                        let typed_expr = self.typecheck_expression(*expr, None)?;
                        let type_id = typed_expr.type_id();
                        if *type_id == PRELUDE_UNIT_TYPE_ID || self.type_contains_generics(type_id) {
                            let span = self.make_span(&typed_expr.span());
                            return Err(TypeError::ForbiddenAssignment { span, type_id: *type_id, purpose: "assignment" });
                        }
                        self.typecheck_binding_pattern(is_mutable, true, &mut binding, &type_id, &mut var_ids)?;

                        Some(Box::new(typed_expr))
                    }
                    (Some(type_hint_id), Some(expr)) => {
                        let typed_expr = self.typecheck_expression(*expr, Some(type_hint_id))?;
                        let mut type_id = *typed_expr.type_id();

                        if self.type_contains_generics(&type_id) {
                            type_id = self.substitute_generics(&type_hint_id, &type_id);
                        }
                        if !self.type_satisfies_other(&type_id, &type_hint_id) {
                            let span = self.make_span(&typed_expr.span());
                            return Err(TypeError::TypeMismatch { span, expected: vec![type_hint_id], received: type_id });
                        };
                        self.typecheck_binding_pattern(is_mutable, true, &mut binding, &type_hint_id, &mut var_ids)?;

                        Some(Box::new(typed_expr))
                    }
                };

                if is_exported {
                    for var_id in &var_ids {
                        let var = self.project.get_var_by_id(var_id);
                        let var_name = var.name.clone();
                        self.current_module_mut().exports.insert(var_name, ExportedValue::Variable(*var_id));
                    }
                }

                Ok(TypedNode::BindingDeclaration { token, pattern: binding, vars: var_ids, expr: typed_expr })
            }
            AstNode::IfStatement(token, if_node) => self.typecheck_if_node(token, if_node, false, type_hint),
            AstNode::ForLoop(token, for_loop_node) => {
                let ForLoopNode { mut binding, index_ident, iterator, body } = for_loop_node;

                let typed_iterator = self.typecheck_expression(*iterator, None)?;
                let iterator_type_id = typed_iterator.type_id();
                let iterator_type = self.project.get_type_by_id(iterator_type_id);
                let (iteratee_type_id, index_type_id) = match iterator_type {
                    Type::GenericInstance(struct_id, generic_ids) if *struct_id == self.project.prelude_array_struct_id => (generic_ids[0], PRELUDE_INT_TYPE_ID),
                    Type::GenericInstance(struct_id, generic_ids) if *struct_id == self.project.prelude_set_struct_id => (generic_ids[0], PRELUDE_INT_TYPE_ID),
                    Type::GenericInstance(struct_id, generic_ids) if *struct_id == self.project.prelude_map_struct_id => (generic_ids[0], generic_ids[1]),
                    _ => return Err(TypeError::InvalidLoopTarget { span: self.make_span(&typed_iterator.span()), type_id: *iterator_type_id, kind: InvalidLoopTargetKind::For }),
                };

                self.begin_child_scope("for_loop_block", ScopeKind::Loop);
                let mut binding_var_ids = vec![];
                self.typecheck_binding_pattern(false, true, &mut binding, &iteratee_type_id, &mut binding_var_ids)?;
                let index_var_id = if let Some(index_ident) = index_ident {
                    let var_name = Token::get_ident_name(&index_ident);
                    let span = self.make_span(&index_ident.get_range());
                    let var_id = self.add_variable_to_current_scope(var_name, index_type_id, false, true, &span, false)?;
                    Some(var_id)
                } else {
                    None
                };

                let mut typed_body = Vec::with_capacity(body.len());
                for node in body {
                    typed_body.push(self.typecheck_statement(node, None)?);
                }

                let loop_scope_terminator = self.current_scope().terminator;
                self.end_child_scope();
                if let Some(TerminatorKind::Returning) = loop_scope_terminator {
                    self.current_scope_mut().terminator = Some(TerminatorKind::Returning);
                }

                Ok(TypedNode::ForLoop { token, binding, binding_var_ids, index_var_id, iterator: Box::new(typed_iterator), body: typed_body })
            }
            AstNode::WhileLoop(token, while_loop_node) => {
                let WhileLoopNode { condition, condition_binding, body } = while_loop_node;

                let typed_condition = self.typecheck_expression(*condition, None)?;
                let condition_type_id = typed_condition.type_id();
                if *condition_type_id != PRELUDE_BOOL_TYPE_ID && self.type_is_option(condition_type_id).is_none() {
                    return Err(TypeError::InvalidLoopTarget { span: self.make_span(&typed_condition.span()), type_id: *condition_type_id, kind: InvalidLoopTargetKind::While });
                }

                self.begin_child_scope("while_loop_block", ScopeKind::Loop);
                let condition_var_id = if let Some(condition_binding) = condition_binding {
                    let condition_type_id = self.type_is_option(condition_type_id).unwrap_or(PRELUDE_BOOL_TYPE_ID);

                    let var_name = Token::get_ident_name(&condition_binding);
                    let span = self.make_span(&condition_binding.get_range());
                    let var_id = self.add_variable_to_current_scope(var_name, condition_type_id, false, true, &span, false)?;
                    Some(var_id)
                } else {
                    None
                };

                let mut typed_body = Vec::with_capacity(body.len());
                for node in body {
                    typed_body.push(self.typecheck_statement(node, None)?);
                }

                let loop_scope_terminator = self.current_scope().terminator;
                self.end_child_scope();
                if let Some(TerminatorKind::Returning) = loop_scope_terminator {
                    self.current_scope_mut().terminator = Some(TerminatorKind::Returning);
                }

                Ok(TypedNode::WhileLoop { token, condition: Box::new(typed_condition), condition_var_id, body: typed_body })
            }
            AstNode::Break(token) => {
                let Some(_) = self.project.find_parent_scope_of_kind(&self.current_scope_id, &ScopeKind::Loop) else {
                    let span = self.make_span(&token.get_range());
                    return Err(TypeError::InvalidControlFlowTerminator { span, terminator: ControlFlowTerminator::Break });
                };

                self.current_scope_mut().terminator = Some(TerminatorKind::NonReturning);

                Ok(TypedNode::Break { token })
            }
            AstNode::Continue(token) => {
                let Some(_) = self.project.find_parent_scope_of_kind(&self.current_scope_id, &ScopeKind::Loop) else {
                    let span = self.make_span(&token.get_range());
                    return Err(TypeError::InvalidControlFlowTerminator { span, terminator: ControlFlowTerminator::Continue });
                };

                self.current_scope_mut().terminator = Some(TerminatorKind::NonReturning);

                Ok(TypedNode::Continue { token })
            }
            AstNode::MatchStatement(token, match_node) => self.typecheck_match_node(token, match_node, false, type_hint),
            AstNode::ReturnStatement(token, ret_expr) => {
                let Some(sc) = self.project.find_parent_fn_scope(&self.current_scope_id) else {
                    let span = self.make_span(&token.get_range());
                    return Err(TypeError::InvalidControlFlowTerminator { span, terminator: ControlFlowTerminator::Return });
                };

                let ScopeKind::Function(parent_func_id) = sc.kind else { unreachable!() };
                self.current_scope_mut().terminator = Some(TerminatorKind::Returning);

                let parent_func = self.project.get_func_by_id(&parent_func_id);
                let func_name = parent_func.name.clone();

                let return_type_id = parent_func.return_type_id;
                let typed_ret_expr = if let Some(ret_expr) = ret_expr {
                    let expr = self.typecheck_expression(*ret_expr, Some(return_type_id))?;
                    Some(Box::new(expr))
                } else {
                    None
                };

                if let Some(typed_ret_expr) = &typed_ret_expr {
                    let mut expr_type_id = *typed_ret_expr.type_id();
                    if self.type_contains_generics(&expr_type_id) {
                        expr_type_id = self.substitute_generics(&return_type_id, &expr_type_id);
                    }
                    if !self.type_satisfies_other(&expr_type_id, &return_type_id) {
                        let span = self.make_span(&typed_ret_expr.span());
                        return Err(TypeError::ReturnTypeMismatch { span, func_name, expected: return_type_id, received: expr_type_id });
                    }
                } else {
                    if !self.type_satisfies_other(&PRELUDE_UNIT_TYPE_ID, &return_type_id) {
                        let span = self.make_span(&token.get_range());
                        return Err(TypeError::ReturnTypeMismatch { span, func_name, expected: return_type_id, received: PRELUDE_UNIT_TYPE_ID });
                    }
                }

                let parent_func = self.project.get_func_by_id_mut(&parent_func_id);
                if parent_func.return_type_id == PRELUDE_ANY_TYPE_ID {
                    let type_id = typed_ret_expr.as_ref().map_or(&PRELUDE_UNIT_TYPE_ID, |expr| expr.type_id());
                    parent_func.return_type_id = *type_id;
                }

                Ok(TypedNode::Return { token, expr: typed_ret_expr })
            }
            AstNode::FunctionDecl(_, _) | AstNode::TypeDecl(_, _) | AstNode::EnumDecl(_, _) => unreachable!("Internal error: node should have been handled in typecheck_block"),
            AstNode::ImportStatement(token, import_node) => {
                let ImportNode { kind, module_token, module_id } = import_node;
                let Some(module_id) = self.module_loader.get_module_id(&module_id) else {
                    let span = self.make_span(&module_token.get_range());
                    let module_path = self.module_loader.resolve_path(&module_id);
                    return Err(TypeError::UnknownModule { span, module_path });
                };
                let module_id = *module_id;
                let ModuleId(module_idx) = module_id;

                match &kind {
                    ImportKind::ImportAll(star_token) => {
                        let import_module = &self.project.modules[module_idx];
                        let exports = import_module.exports.values().map(|e| e.clone()).collect_vec();

                        for export in exports {
                            self.add_imported_value(export, star_token)?;
                        }
                    }
                    ImportKind::ImportList(imports) => {
                        for import_tok in imports {
                            let import_name = Token::get_ident_name(import_tok);
                            let import_module = &self.project.modules[module_idx];
                            let Some(export) = import_module.exports.get(&import_name) else {
                                let span = self.make_span(&import_tok.get_range());
                                return Err(TypeError::UnknownExport { span, module_id, import_name, is_aliased: false });
                            };

                            self.add_imported_value(*export, import_tok)?;
                        }
                    }
                    ImportKind::Alias(alias_token) => {
                        let alias_name = Token::get_ident_name(alias_token);
                        let module_type_id = TypeId::module_type_alias(&module_id);
                        let span = self.make_span(&alias_token.get_range());
                        self.add_variable_to_current_scope(alias_name, module_type_id, false, true, &span, false)?;
                    }
                }

                Ok(TypedNode::Import { token, kind, module_id })
            }
            n => self.typecheck_expression(n, type_hint)
        }
    }

    fn add_imported_value(&mut self, exported_value: ExportedValue, import_token: &Token) -> Result<(), TypeError> {
        let span = self.make_span(&import_token.get_range());

        match exported_value {
            ExportedValue::Function(func_id) => {
                self.add_function_variable_alias_to_current_scope(import_token, &func_id)?;
            }
            ExportedValue::Type(type_kind) => match type_kind {
                TypeKind::Struct(struct_id) => {
                    let struct_type_id = self.add_or_find_type_id(self.project.struct_type(struct_id));
                    let struct_ = self.project.get_struct_by_id(&struct_id);
                    let struct_var_id = self.add_variable_to_current_scope(struct_.name.clone(), struct_type_id, false, true, &span, false)?;
                    let variable = self.project.get_var_by_id_mut(&struct_var_id);
                    variable.alias = VariableAlias::Type(TypeKind::Struct(struct_id));
                }
                TypeKind::Enum(enum_id) => {
                    let enum_type_id = self.add_or_find_type_id(self.project.enum_type(enum_id));
                    let enum_ = self.project.get_enum_by_id(&enum_id);
                    let enum_var_id = self.add_variable_to_current_scope(enum_.name.clone(), enum_type_id, false, true, &span, false)?;
                    let variable = self.project.get_var_by_id_mut(&enum_var_id);
                    variable.alias = VariableAlias::Type(TypeKind::Enum(enum_id));
                }
            }
            ExportedValue::Variable(var_id) => {
                let imported_var = self.project.get_var_by_id(&var_id);
                let is_captured = imported_var.is_captured;
                let var_id = self.add_variable_to_current_scope(imported_var.name.clone(), imported_var.type_id, false, imported_var.is_initialized, &span, false)?;
                let var = self.project.get_var_by_id_mut(&var_id);
                var.is_captured = is_captured;
            }
        }

        Ok(())
    }

    fn typecheck_if_node(&mut self, if_token: Token, if_node: IfNode, is_expr: bool, type_hint: Option<TypeId>) -> Result<TypedNode, TypeError> {
        let is_statement = if let Some(type_hint) = &type_hint { *type_hint == PRELUDE_UNIT_TYPE_ID } else { !is_expr };

        let IfNode { condition, mut condition_binding, if_block, else_block } = if_node;

        let typed_condition = self.typecheck_expression(*condition, None)?;
        let condition_type_id = typed_condition.type_id();
        if !self.type_satisfies_other(condition_type_id, &PRELUDE_BOOL_TYPE_ID) {
            let span = self.make_span(&typed_condition.span());
            return Err(TypeError::TypeMismatch { span, expected: vec![PRELUDE_BOOL_TYPE_ID], received: *condition_type_id });
        }

        let mut type_id = None;
        if !is_statement {
            if let Some(type_hint) = &type_hint {
                type_id = Some(*type_hint);
            }
        }

        let if_block_scope_id = self.begin_child_scope("if_block", ScopeKind::If);
        if let Some(ref mut condition_binding) = &mut condition_binding {
            self.typecheck_binding_pattern(false, true, condition_binding, condition_type_id, &mut vec![])?;
        }
        let if_block_len = if_block.len();
        let mut typed_if_block = Vec::with_capacity(if_block_len);
        if if_block_len != 0 {
            // If the if-block has a body, typecheck it. The type of the last node should become the working type
            // for the overarching expression, and is compared against the provided type_hint, if one exists.
            for (idx, node) in if_block.into_iter().enumerate() {
                let typed_node = if idx == if_block_len - 1 {
                    let typed_node = self.typecheck_statement(node, type_id)?;
                    let mut node_type_id = *typed_node.type_id();

                    if self.current_scope().terminator.is_none() {
                        if let Some(hint_type_id) = &type_id {
                            if self.type_contains_generics(&node_type_id) {
                                node_type_id = self.substitute_generics(&hint_type_id, &node_type_id);
                            }
                            if !self.type_satisfies_other(&node_type_id, &hint_type_id) {
                                let span = self.make_span(&typed_node.span());
                                return Err(TypeError::TypeMismatch { span, expected: vec![*hint_type_id], received: node_type_id });
                            };
                        }

                        if !is_statement {
                            type_id = Some(node_type_id);
                        }
                    }

                    typed_node
                } else {
                    self.typecheck_statement(node, None)?
                };
                typed_if_block.push(typed_node);
            }
        } else if let Some(hint_type_id) = &type_id {
            // If there is no if-block body and there is a provided type_hint, then the working type for
            // the expression becomes the hint type wrapped in an option.
            type_id = Some(self.add_or_find_type_id(self.project.option_type(*hint_type_id)));
        } else if !is_statement {
            // If there is no if-block body and there is also no provided type_hint, we can't make any
            // assumptions about what the type should be, so we default to the type of the `None` builtin.
            type_id = Some(self.project.prelude_none_type_id);
        }
        self.end_child_scope();
        let if_block_terminator = self.project.get_scope_by_id(&if_block_scope_id).terminator;

        let mut else_block_terminator = None;
        let else_block = else_block.unwrap_or(vec![]);
        let else_block_len = else_block.len();
        let mut typed_else_block = Vec::with_capacity(else_block_len);
        if else_block_len != 0 {
            // If the else-block has a body, typecheck it. The type of the last node should be compared
            // against the working type to determine a match.
            let else_block_scope_id = self.begin_child_scope("else_block", ScopeKind::If);
            for (idx, node) in else_block.into_iter().enumerate() {
                let typed_node = if idx == else_block_len - 1 {
                    let typed_node = self.typecheck_statement(node, type_id)?;
                    let node_type_id = *typed_node.type_id();

                    if !is_statement && self.current_scope().terminator.is_none() {
                        type_id = if let Some(hint_type_id) = &type_id {
                            let Some(unified_type_id) = self.unify_types(hint_type_id, &node_type_id) else {
                                let span = self.make_span(&typed_node.span());
                                return if let Some(last_if_block_expr) = typed_if_block.last() {
                                    let orig_span = self.make_span(&last_if_block_expr.span());
                                    Err(TypeError::BranchTypeMismatch { span, orig_span, expected: *hint_type_id, received: node_type_id })
                                } else if !is_statement {
                                    let hint_type_id = self.type_is_option(hint_type_id).unwrap_or(*hint_type_id);
                                    Err(TypeError::TypeMismatch { span, expected: vec![hint_type_id], received: node_type_id })
                                } else { unreachable!() };
                            };
                            Some(unified_type_id)
                        } else {
                            Some(node_type_id)
                        };
                    }

                    typed_node
                } else {
                    self.typecheck_statement(node, None)?
                };

                typed_else_block.push(typed_node);
            }
            self.end_child_scope();
            else_block_terminator = self.project.get_scope_by_id(&else_block_scope_id).terminator;
        } else if let Some(hint_type_id) = &type_id {
            type_id = Some(self.add_or_find_type_id(self.project.option_type(*hint_type_id)));
        }

        self.current_scope_mut().terminator = compound_terminator_kinds(if_block_terminator, else_block_terminator);

        let type_id = if is_statement {
            PRELUDE_UNIT_TYPE_ID
        } else {
            type_id.unwrap_or(PRELUDE_UNIT_TYPE_ID)
        };

        Ok(TypedNode::If {
            if_token,
            condition: Box::new(typed_condition),
            condition_binding,
            if_block: typed_if_block,
            else_block: typed_else_block,
            is_statement,
            type_id,
        })
    }

    fn typecheck_match_node(&mut self, match_token: Token, match_node: MatchNode, is_expr: bool, type_hint: Option<TypeId>) -> Result<TypedNode, TypeError> {
        let is_statement = if let Some(type_hint) = &type_hint { *type_hint == PRELUDE_UNIT_TYPE_ID } else { !is_expr };
        let MatchNode { target, branches } = match_node;

        let typed_target = self.typecheck_expression(*target, None)?;
        let target_type_id = *typed_target.type_id();

        let mut type_id = None;
        if !is_statement {
            if let Some(type_hint) = &type_hint {
                type_id = Some(*type_hint);
            }
        }

        // Start off assuming there will be a terminator in one of the branches (aka "true"). The branches' values will effectively be ANDed together.
        let mut all_branches_terminator = Some(TerminatorKind::NonReturning);

        // Keep track of all possibilities, and whether they've been completed
        let mut seen_wildcard_token: Option<Token> = None;
        let mut seen_constant_values = HashMap::<TypedLiteral, Token>::new();
        let mut seen_none_token: Option<Token> = None;
        let mut none_case_covered = self.type_is_option(&target_type_id).is_none();
        let mut seen_type_token: Option<Token> = None;
        let mut type_case_covered = match self.project.get_type_by_id(&self.type_is_option(&target_type_id).unwrap_or(target_type_id)) {
            Type::GenericInstance(_, _) | Type::Primitive(_) => false,
            _ => true
        };
        let mut seen_enum_variants = HashMap::<(EnumId, usize), Range>::new();
        let mut enum_variants_covered = !matches!(self.project.get_type_by_id(&self.type_is_option(&target_type_id).unwrap_or(target_type_id)), Type::GenericEnumInstance(_, _, _));
        let mut all_cases_covered = false;

        let mut typed_match_cases = Vec::<TypedMatchCase>::with_capacity(branches.len());
        for (match_case_idx, (match_case, match_case_body)) in branches.into_iter().enumerate() {
            let MatchCase { token: match_case_token, match_type, case_binding } = match_case;

            if all_cases_covered {
                return Err(TypeError::UnreachableMatchCase { span: self.make_span(&match_case_token.get_range()), kind: UnreachableMatchCaseKind::AlreadyCovered });
            }

            self.begin_child_scope(&format!("match_case_{}", match_case_idx), ScopeKind::Match);

            let case_type_id;
            match match_type {
                MatchCaseType::None(none_token) => {
                    let Some(unwrapped_type) = self.type_is_option(&target_type_id) else {
                        let kind = UnreachableMatchCaseKind::NoTypeOverlap { case_type: None, target_type: target_type_id, target_span: self.make_span(&typed_target.span()) };
                        return Err(TypeError::UnreachableMatchCase { span: self.make_span(&none_token.get_range()), kind });
                    };
                    debug_assert!(self.type_is_option(&unwrapped_type).is_none(), "A nested Option type should be fully unwrapped");

                    if let Some(orig_token) = seen_none_token {
                        return Err(TypeError::DuplicateMatchCase { span: self.make_span(&none_token.get_range()), orig_span: self.make_span(&orig_token.get_range()) });
                    }
                    seen_none_token = Some(none_token);
                    none_case_covered = true;

                    case_type_id = self.project.prelude_none_type_id;
                }
                MatchCaseType::Ident(ident_token, args) => {
                    if let Some(destructured_arg) = args.as_ref().and_then(|args| args.first()) {
                        let tok = match destructured_arg {
                            MatchCaseArgument::Pattern(p) => p.get_token(),
                            MatchCaseArgument::Literal(n) => n.get_token(),
                        };
                        return Err(TypeError::UnimplementedFeature { span: self.make_span(&tok.get_range()), desc: "destructuring of non-enum type in match case" });
                    }
                    if let Some(orig_token) = seen_type_token {
                        return Err(TypeError::DuplicateMatchCase { span: self.make_span(&ident_token.get_range()), orig_span: self.make_span(&orig_token.get_range()) });
                    }
                    seen_type_token = Some(ident_token.clone());

                    let resolved_case_type_id = self.resolve_type_identifier(&TypeIdentifier::Normal { ident: ident_token.clone(), type_args: None })?;
                    if !self.type_satisfies_other(&resolved_case_type_id, &target_type_id) {
                        let kind = UnreachableMatchCaseKind::NoTypeOverlap { case_type: Some(resolved_case_type_id), target_type: target_type_id, target_span: self.make_span(&typed_target.span()) };
                        return Err(TypeError::UnreachableMatchCase { span: self.make_span(&ident_token.get_range()), kind });
                    }
                    if resolved_case_type_id == target_type_id || matches!(self.type_is_option(&target_type_id), Some(unwrapped_opt_type) if unwrapped_opt_type == resolved_case_type_id) {
                        type_case_covered = true;
                    }

                    case_type_id = resolved_case_type_id;
                }
                MatchCaseType::Compound(path_tokens, args) => {
                    debug_assert!(args.is_none(), "Destructuring enum variants not implemented yet");
                    let mut path_tokens_iter = path_tokens.into_iter();
                    let enum_name_token = path_tokens_iter.next().expect("There should be at least 2 tokens in the path");
                    let enum_name = Token::get_ident_name(&enum_name_token);
                    let Some(enum_) = self.get_enum_by_name(&enum_name) else {
                        return Err(TypeError::UnknownType { span: self.make_span(&enum_name_token.get_range()), name: enum_name });
                    };

                    let variant_name_token = path_tokens_iter.next().expect("There should be 2 tokens in the path");
                    let match_case_range = enum_name_token.get_range().expand(&variant_name_token.get_range());

                    let variant_name = Token::get_ident_name(&variant_name_token);
                    let Some((variant_idx, _variant)) = enum_.variants.iter().enumerate().find(|(_, v)| v.name == variant_name) else {
                        return Err(TypeError::UnknownMember { span: self.make_span(&variant_name_token.get_range()), field_name: variant_name, type_id: enum_.self_type_id });
                    };
                    let inner_type_id = self.type_is_option(&target_type_id).unwrap_or(target_type_id);
                    let target_ty = self.project.get_type_by_id(&inner_type_id);
                    let generic_ids = match target_ty {
                        Type::GenericEnumInstance(enum_id, generic_ids, _) if *enum_id == enum_.id => generic_ids,
                        _ => return Err(TypeError::UnreachableMatchCase {
                            span: self.make_span(&match_case_range),
                            kind: UnreachableMatchCaseKind::NoTypeOverlap { case_type: Some(enum_.self_type_id), target_type: target_type_id, target_span: self.make_span(&typed_target.span()) },
                        }),
                    };
                    if let Some(orig_range) = seen_enum_variants.get(&(enum_.id, variant_idx)) {
                        return Err(TypeError::DuplicateMatchCase { span: self.make_span(&match_case_range), orig_span: self.make_span(orig_range) });
                    }
                    seen_enum_variants.insert((enum_.id, variant_idx), match_case_range);
                    if enum_.variants.iter().enumerate().all(|(v_idx, _)| seen_enum_variants.contains_key(&(enum_.id, v_idx))) {
                        enum_variants_covered = true;
                    }

                    case_type_id = self.add_or_find_type_id(Type::GenericEnumInstance(enum_.id, generic_ids.clone(), Some(variant_idx)));
                }
                MatchCaseType::Wildcard(wildcard_token) => {
                    if let Some(seen_wildcard_token) = seen_wildcard_token {
                        return Err(TypeError::DuplicateMatchCase { span: self.make_span(&wildcard_token.get_range()), orig_span: self.make_span(&seen_wildcard_token.get_range()) });
                    }
                    seen_wildcard_token = Some(wildcard_token);
                    all_cases_covered = true;
                    case_type_id = target_type_id;
                }
                MatchCaseType::Constant(const_node) => {
                    let typed_const_node = self.typecheck_expression(const_node, None)?;
                    let node_type_id = *typed_const_node.type_id();
                    if !self.type_satisfies_other(&node_type_id, &target_type_id) {
                        let kind = UnreachableMatchCaseKind::NoTypeOverlap { case_type: Some(node_type_id), target_type: target_type_id, target_span: self.make_span(&typed_target.span()) };
                        return Err(TypeError::UnreachableMatchCase { span: self.make_span(&typed_const_node.span()), kind });
                    }
                    let TypedNode::Literal { token: const_token, value: const_val, .. } = typed_const_node else {
                        unreachable!("Internal error: constant case expressions must be int, float, bool, or string")
                    };
                    if let Some(orig_token) = seen_constant_values.get(&const_val) {
                        return Err(TypeError::DuplicateMatchCase { span: self.make_span(&const_token.get_range()), orig_span: self.make_span(&orig_token.get_range()) });
                    }

                    // Handle the special case where, if we're matching on a Bool and we have `true` and `false` literal match cases, then we know we have an exhaustive match.
                    if target_type_id == PRELUDE_BOOL_TYPE_ID {
                        if const_val == TypedLiteral::Bool(true) && seen_constant_values.contains_key(&TypedLiteral::Bool(false)) ||
                            const_val == TypedLiteral::Bool(false) && seen_constant_values.contains_key(&TypedLiteral::Bool(true)) {
                            all_cases_covered = true;
                        }
                    }
                    seen_constant_values.insert(const_val, const_token);

                    case_type_id = node_type_id;
                }
                MatchCaseType::Tuple(_, _) => {
                    // I think I want this to work slightly differently (better?) in this version. For example:
                    //   val arr = [1, 2, 3]
                    //   match arr[0], arr[1] {
                    //     None, 2 | 2, None => println("here")
                    //     _ => {}
                    //   }
                    // This "multi-match" syntax allows for tuple-like matching without the need to construct (and
                    // allocate) a tuple, and I think it's a little nicer to read and implement. If a tuple instance
                    // were passed to the "single-match" syntax, it'd desugar to the above.
                    todo!()
                }
            }

            if let Some(case_binding_tok) = case_binding {
                let binding_name = Token::get_ident_name(&case_binding_tok);
                let span = self.make_span(&case_binding_tok.get_range());
                let _var_id = self.add_variable_to_current_scope(binding_name, case_type_id, false, true, &span, false)?;
            }

            if match_case_body.is_empty() {
                let span = self.make_span(&match_case_token.get_range());
                return Err(TypeError::EmptyMatchBlock { span });
            }

            let num_body_nodes = match_case_body.len();
            let mut typed_body = Vec::with_capacity(num_body_nodes);
            for (idx, node) in match_case_body.into_iter().enumerate() {
                let typed_node = if idx == num_body_nodes - 1 {
                    let typed_node = self.typecheck_statement(node, type_id)?;
                    let mut node_type_id = *typed_node.type_id();

                    if !is_statement && self.current_scope().terminator.is_none() {
                        type_id = if match_case_idx == 0 {
                            if let Some(hint_type_id) = &type_id {
                                if self.type_contains_generics(&node_type_id) {
                                    node_type_id = self.substitute_generics(&hint_type_id, &node_type_id);
                                }
                                if !self.type_satisfies_other(&node_type_id, &hint_type_id) {
                                    let span = self.make_span(&typed_node.span());
                                    return Err(TypeError::TypeMismatch { span, expected: vec![*hint_type_id], received: node_type_id });
                                };
                            }

                            Some(node_type_id)
                        } else {
                            if let Some(hint_type_id) = &type_id {
                                let Some(unified_type_id) = self.unify_types(hint_type_id, &node_type_id) else {
                                    let span = self.make_span(&typed_node.span());

                                    return if let Some(last_match_block_expr) = typed_match_cases.last().and_then(|case| case.body.last()) {
                                        let orig_span = self.make_span(&last_match_block_expr.span());
                                        Err(TypeError::BranchTypeMismatch { span, orig_span, expected: *hint_type_id, received: node_type_id })
                                    } else {
                                        let hint_type_id = self.type_is_option(hint_type_id).unwrap_or(*hint_type_id);
                                        Err(TypeError::TypeMismatch { span, expected: vec![hint_type_id], received: node_type_id })
                                    };
                                };
                                Some(unified_type_id)
                            } else {
                                Some(node_type_id)
                            }
                        };
                    }

                    typed_node
                } else {
                    self.typecheck_statement(node, None)?
                };
                typed_body.push(typed_node);
            }

            all_branches_terminator = compound_terminator_kinds(all_branches_terminator, self.current_scope().terminator);
            self.end_child_scope();

            typed_match_cases.push(TypedMatchCase { body: typed_body })
        }

        self.current_scope_mut().terminator = all_branches_terminator;

        all_cases_covered |= none_case_covered && type_case_covered && enum_variants_covered;
        if !all_cases_covered {
            return Err(TypeError::NonExhaustiveMatch { span: self.make_span(&match_token.get_range()), type_id: target_type_id });
        }

        let type_id = if is_statement {
            PRELUDE_UNIT_TYPE_ID
        } else {
            type_id.unwrap_or(PRELUDE_UNIT_TYPE_ID)
        };

        Ok(TypedNode::Match {
            match_token,
            target: Box::new(typed_target),
            cases: typed_match_cases,
            is_statement,
            type_id,
        })
    }

    fn typecheck_binding_pattern(&mut self, is_mutable: bool, is_initialized: bool, pattern: &mut BindingPattern, type_id: &TypeId, var_ids: &mut Vec<VarId>) -> Result<(), TypeError> {
        match pattern {
            BindingPattern::Variable(var_token) => {
                let var_name = Token::get_ident_name(&var_token);

                let span = self.make_span(&var_token.get_range());
                let var_id = self.add_variable_to_current_scope(var_name, *type_id, is_mutable, is_initialized, &span, false)?;
                var_ids.push(var_id);
            }
            BindingPattern::Tuple(_, patterns) => {
                let mut err_kind = None;
                let mut was_option = false;
                let type_id = if let Some(wrapped_type_id) = self.type_is_option(&type_id) {
                    was_option = true;
                    wrapped_type_id
                } else {
                    *type_id
                };
                if let Type::GenericInstance(struct_id, generic_ids) = self.project.get_type_by_id(&type_id) {
                    if *struct_id != self.project.prelude_tuple_struct_id {
                        err_kind = Some(DestructuringMismatchKind::CannotDestructureAsTuple);
                    } else if patterns.len() != generic_ids.len() {
                        err_kind = Some(DestructuringMismatchKind::InvalidTupleArity(generic_ids.len(), patterns.len()));
                    } else {
                        let generic_ids = generic_ids.clone();
                        for (mut pattern, type_id) in patterns.iter_mut().zip(generic_ids.iter()) {
                            let type_id = if was_option {
                                self.add_or_find_type_id(self.project.option_type(*type_id))
                            } else {
                                *type_id
                            };
                            self.typecheck_binding_pattern(is_mutable, is_initialized, &mut pattern, &type_id, var_ids)?;
                        }
                    }
                } else {
                    err_kind = Some(DestructuringMismatchKind::CannotDestructureAsTuple);
                };
                if let Some(kind) = err_kind {
                    return Err(TypeError::DestructuringMismatch { span: self.make_span(&pattern.get_span()), kind, type_id });
                }
            }
            BindingPattern::Array(_, patterns, pattern_is_string) => {
                let mut err_kind = None;
                let mut inner_type_id = None;
                let type_id = if let Some(wrapped_type_id) = self.type_is_option(&type_id) {
                    wrapped_type_id
                } else {
                    *type_id
                };
                if let Type::GenericInstance(struct_id, generic_ids) = self.project.get_type_by_id(&type_id) {
                    if *struct_id != self.project.prelude_array_struct_id {
                        err_kind = Some(DestructuringMismatchKind::CannotDestructureAsArray);
                    } else {
                        debug_assert!(generic_ids.len() == 1, "Array type should have exactly 1 generic");
                        inner_type_id = Some(generic_ids[0]);
                    }
                } else if type_id != PRELUDE_STRING_TYPE_ID {
                    err_kind = Some(DestructuringMismatchKind::CannotDestructureAsArray);
                };
                if let Some(kind) = err_kind {
                    return Err(TypeError::DestructuringMismatch { span: self.make_span(&pattern.get_span()), kind, type_id });
                }

                let is_string = type_id == PRELUDE_STRING_TYPE_ID;
                let inner_type_id = if is_string { PRELUDE_STRING_TYPE_ID } else { inner_type_id.unwrap() };
                *pattern_is_string = is_string;

                let mut seen_splat = false;
                for (pattern, is_splat) in patterns {
                    let type_id = if *is_splat {
                        if seen_splat {
                            let span = self.make_span(&pattern.get_token().get_range());
                            return Err(TypeError::DuplicateSplat { span });
                        }
                        seen_splat = true;

                        if is_string {
                            PRELUDE_STRING_TYPE_ID
                        } else {
                            type_id
                        }
                    } else if *pattern_is_string {
                        inner_type_id
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

                let span = self.make_span(&token.get_range().expand(&typed_expr.span()));
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
            AstNode::Binary(token, n) => {
                let BinaryNode { op, left, right } = n;

                let typecheck_transformed_expr = |left: Box<AstNode>, op: BinaryOp, right: Box<AstNode>| {
                    let transformed_node = AstNode::Assignment(
                        Token::Assign(token.get_position()),
                        AssignmentNode { target: left.clone(), expr: Box::new(AstNode::Binary(token, BinaryNode { right, op, left })) },
                    );
                    self.typecheck_expression(transformed_node, type_hint)
                };
                match &op {
                    BinaryOp::AddEq => return typecheck_transformed_expr(left, BinaryOp::Add, right),
                    BinaryOp::SubEq => return typecheck_transformed_expr(left, BinaryOp::Sub, right),
                    BinaryOp::MulEq => return typecheck_transformed_expr(left, BinaryOp::Mul, right),
                    BinaryOp::DivEq => return typecheck_transformed_expr(left, BinaryOp::Div, right),
                    BinaryOp::ModEq => return typecheck_transformed_expr(left, BinaryOp::Mod, right),
                    BinaryOp::AndEq => return typecheck_transformed_expr(left, BinaryOp::And, right),
                    BinaryOp::OrEq => return typecheck_transformed_expr(left, BinaryOp::Or, right),
                    BinaryOp::CoalesceEq => return typecheck_transformed_expr(left, BinaryOp::Coalesce, right),
                    _ => { /* other non-assignment cases handled down below */ }
                };

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
                            let span = self.make_span(&typed_left.span().expand(&typed_right.span()));
                            return Err(TypeError::IllegalOperator { span, op, left, right });
                        }
                    }
                    BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Mod => match (*l_type_id, *r_type_id) {
                        (PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID) => PRELUDE_INT_TYPE_ID,
                        (PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) => PRELUDE_FLOAT_TYPE_ID,
                        (left, right) => {
                            let span = self.make_span(&typed_left.span().expand(&typed_right.span()));
                            return Err(TypeError::IllegalOperator { span, op, left, right });
                        }
                    }
                    BinaryOp::Div | BinaryOp::Pow => match (*l_type_id, *r_type_id) {
                        (PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID) |
                        (PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) => PRELUDE_FLOAT_TYPE_ID,
                        (left, right) => {
                            let span = self.make_span(&typed_left.span().expand(&typed_right.span()));
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
                            let span = self.make_span(&typed_right.span());
                            return Err(TypeError::TypeMismatch { span, expected: vec![inner], received: *r_type_id });
                        }
                        *r_type_id
                    }
                    BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => match (*l_type_id, *r_type_id) {
                        (PRELUDE_INT_TYPE_ID, PRELUDE_INT_TYPE_ID) |
                        (PRELUDE_INT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID) | (PRELUDE_FLOAT_TYPE_ID, PRELUDE_FLOAT_TYPE_ID) |
                        (PRELUDE_STRING_TYPE_ID, PRELUDE_STRING_TYPE_ID) => PRELUDE_BOOL_TYPE_ID,
                        (left, right) => {
                            let span = self.make_span(&typed_left.span().expand(&typed_right.span()));
                            return Err(TypeError::IllegalOperator { span, op, left, right });
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Neq => PRELUDE_BOOL_TYPE_ID,

                    // Boolean operators
                    BinaryOp::And | BinaryOp::Or | BinaryOp::Xor => match (*l_type_id, *r_type_id) {
                        (PRELUDE_BOOL_TYPE_ID, PRELUDE_BOOL_TYPE_ID) => PRELUDE_BOOL_TYPE_ID,
                        (left, right) => {
                            let span = self.make_span(&typed_left.span().expand(&typed_right.span()));
                            return Err(TypeError::IllegalOperator { span, op, left, right });
                        }
                    }

                    // Assignment operators handled above
                    BinaryOp::AddEq | BinaryOp::SubEq | BinaryOp::MulEq | BinaryOp::DivEq | BinaryOp::ModEq | BinaryOp::AndEq | BinaryOp::OrEq | BinaryOp::CoalesceEq => unreachable!()
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
                                let span = self.make_span(&typed_item.span());
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
                                let span = self.make_span(&typed_item.span());
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
                                let span = self.make_span(&typed_key_node.span());
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
                                let span = self.make_span(&typed_val_node.span());
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
                                let span = self.make_span(&typed_item.span());
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
                        let range = token.get_range().expand(&typed_items.last().map(|i| i.span()).unwrap_or(token.get_range()));
                        let span = self.make_span(&range);
                        let expected = self.add_or_find_type_id(self.project.tuple_type(inner_type_ids.clone()));
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
                    let span = self.make_span(&token.get_range());
                    return Err(TypeError::UnknownIdentifier { span, token });
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
                        if var.alias == VariableAlias::None {
                            var.is_captured = true;

                            let func = self.project.get_func_by_id_mut(&func_id);
                            if !func.captured_vars.contains(&var_id) {
                                func.captured_vars.push(var_id);
                            }
                        }
                    }
                }

                Ok(TypedNode::Identifier { token, var_id, type_arg_ids, type_id: var_type_id })
            }
            AstNode::Assignment(_, n) => {
                let AssignmentNode { target, expr } = n;

                let typed_target = self.typecheck_expression(*target, type_hint)?;
                let target_span = self.make_span(&typed_target.span());
                let target_type_id = *typed_target.type_id();
                let typed_expr = self.typecheck_expression(*expr, Some(target_type_id))?;

                let kind = match typed_target {
                    TypedNode::Identifier { var_id, .. } => {
                        let variable = self.project.get_var_by_id(&var_id);
                        if !variable.is_mutable {
                            let kind = if variable.is_parameter { ImmutableAssignmentKind::Parameter } else { ImmutableAssignmentKind::Variable };
                            return Err(TypeError::AssignmentToImmutable { span: target_span, var_name: variable.name.clone(), defined_span: variable.defined_span.clone(), kind });
                        }

                        AssignmentKind::Identifier { var_id }
                    }
                    TypedNode::Accessor { target, kind, member_idx, .. } => {
                        let ty = self.project.get_type_by_id(target.type_id());

                        match (ty, &kind) {
                            (Type::GenericInstance(_, _), AccessorKind::Field) => {
                                let (struct_, _) = self.project.get_struct_by_type_id(target.type_id()).expect("Internal error: This should have been caught when typechecking the Accessor");

                                let field = &struct_.fields[member_idx];
                                if field.is_readonly {
                                    let type_name = struct_.name.clone();
                                    return Err(TypeError::AssignmentToImmutable { span: target_span, var_name: field.name.clone(), defined_span: Some(field.defined_span.clone()), kind: ImmutableAssignmentKind::Field(type_name) });
                                }
                            }
                            (Type::GenericEnumInstance(_, _, Some(_)), AccessorKind::Field) => {}
                            (Type::GenericInstance(_, _), AccessorKind::Method) => {
                                let (struct_, _) = self.project.get_struct_by_type_id(target.type_id()).expect("Internal error: This should have been caught when typechecking the Accessor");
                                let type_name = struct_.name.clone();
                                let func_id = &struct_.methods[member_idx];
                                let function = self.project.get_func_by_id(func_id);
                                return Err(TypeError::AssignmentToImmutable { span: target_span, var_name: function.name.clone(), defined_span: function.defined_span.clone(), kind: ImmutableAssignmentKind::Method(type_name) });
                            }
                            (Type::GenericEnumInstance(_, _, _), AccessorKind::Method) => {
                                let (enum_, _, _) = self.project.get_enum_by_type_id(target.type_id()).expect("Internal error: This should have been caught when typechecking the Accessor");
                                let type_name = enum_.name.clone();
                                let func_id = &enum_.methods[member_idx];
                                let function = self.project.get_func_by_id(func_id);
                                return Err(TypeError::AssignmentToImmutable { span: target_span, var_name: function.name.clone(), defined_span: function.defined_span.clone(), kind: ImmutableAssignmentKind::Method(type_name) });
                            }
                            (Type::GenericInstance(_, _), AccessorKind::StaticMethod) => {
                                let (struct_, _) = self.project.get_struct_by_type_id(target.type_id()).expect("Internal error: This should have been caught when typechecking the Accessor");
                                let type_name = struct_.name.clone();
                                let func_id = &struct_.static_methods[member_idx];
                                let function = self.project.get_func_by_id(func_id);
                                return Err(TypeError::AssignmentToImmutable { span: target_span, var_name: function.name.clone(), defined_span: function.defined_span.clone(), kind: ImmutableAssignmentKind::StaticMethod(type_name) });
                            }
                            (Type::GenericEnumInstance(_, _, _), AccessorKind::StaticMethod) => {
                                let (enum_, _, _) = self.project.get_enum_by_type_id(target.type_id()).expect("Internal error: This should have been caught when typechecking the Accessor");
                                let type_name = enum_.name.clone();
                                let func_id = &enum_.static_methods[member_idx];
                                let function = self.project.get_func_by_id(func_id);
                                return Err(TypeError::AssignmentToImmutable { span: target_span, var_name: function.name.clone(), defined_span: function.defined_span.clone(), kind: ImmutableAssignmentKind::StaticMethod(type_name) });
                            }
                            (Type::Type(TypeKind::Enum(enum_id)), AccessorKind::EnumVariant) => {
                                let enum_ = self.project.get_enum_by_id(enum_id);
                                let type_name = enum_.name.clone();
                                let variant = &enum_.variants[member_idx];
                                return Err(TypeError::AssignmentToImmutable { span: target_span, var_name: variant.name.clone(), defined_span: Some(variant.defined_span.clone()), kind: ImmutableAssignmentKind::EnumVariant(type_name) });
                            }
                            _ => unreachable!()
                        };

                        AssignmentKind::Accessor { target, kind, member_idx }
                    }
                    TypedNode::Indexing { target, index: IndexingMode::Index(index), .. } => {
                        let index_target_type_id = target.type_id();
                        if let Type::GenericInstance(struct_id, _) = self.project.get_type_by_id(index_target_type_id) {
                            if struct_id == &self.project.prelude_tuple_struct_id {
                                return Err(TypeError::InvalidAssignmentTarget { span: target_span, kind: InvalidAssignmentTargetKind::IndexingTuple });
                            }
                        }
                        if index_target_type_id == &PRELUDE_STRING_TYPE_ID {
                            return Err(TypeError::InvalidAssignmentTarget { span: target_span, kind: InvalidAssignmentTargetKind::IndexingString });
                        }

                        AssignmentKind::Indexing { target, index }
                    }
                    TypedNode::Indexing { index: IndexingMode::Range(_, _), .. } => return Err(TypeError::InvalidAssignmentTarget { span: target_span, kind: InvalidAssignmentTargetKind::IndexingRange }),
                    _ => return Err(TypeError::InvalidAssignmentTarget { span: target_span, kind: InvalidAssignmentTargetKind::UnsupportedAssignmentTarget })
                };

                let mut type_id = *typed_expr.type_id();
                if self.type_contains_generics(&type_id) {
                    type_id = self.substitute_generics(&target_type_id, &type_id);
                }
                if !self.type_satisfies_other(&type_id, &target_type_id) {
                    return Err(TypeError::TypeMismatch { span: self.make_span(&typed_expr.span()), expected: vec![target_type_id], received: type_id });
                }

                let span = target_span.range.expand(&typed_expr.span());
                Ok(TypedNode::Assignment { span, kind, type_id, expr: Box::new(typed_expr) })
            }
            AstNode::Indexing(_, n) => {
                let IndexingNode { target, index } = n;

                let typed_target = self.typecheck_expression(*target, None)?;
                let target_type_id = *typed_target.type_id();
                let target_span = self.make_span(&typed_target.span());
                let target_ty = self.project.get_type_by_id(&target_type_id);

                // Handle tuples separately, since they have a special Literal requirement
                if let Type::GenericInstance(struct_id, generic_ids) = &target_ty {
                    if struct_id == &self.project.prelude_tuple_struct_id {
                        let tuple_items = generic_ids.clone();

                        let IndexingMode::Index(idx_node) = index else {
                            return Err(TypeError::InvalidIndexableType { span: target_span, is_range: true, type_id: target_type_id });
                        };
                        let typed_idx_node = self.typecheck_expression(*idx_node, Some(PRELUDE_INT_TYPE_ID))?;
                        let typed_idx_node_span = self.make_span(&typed_idx_node.span());
                        let TypedNode::Literal { value: TypedLiteral::Int(idx), .. } = typed_idx_node else {
                            return Err(TypeError::InvalidTupleIndex { span: typed_idx_node_span, kind: InvalidTupleIndexKind::NonConstant, type_id: target_type_id });
                        };
                        let Some(type_id) = tuple_items.get(idx as usize) else {
                            return Err(TypeError::InvalidTupleIndex { span: typed_idx_node_span, kind: InvalidTupleIndexKind::OutOfBounds(idx), type_id: target_type_id });
                        };

                        return Ok(TypedNode::Indexing { target: Box::new(typed_target), index: IndexingMode::Index(Box::new(typed_idx_node)), type_id: *type_id });
                    }
                }

                let (required_index_type_id, result_type_id) = match (&index, target_ty) {
                    (IndexingMode::Index(_), Type::GenericInstance(struct_id, generic_ids)) if struct_id == &self.project.prelude_array_struct_id => {
                        let opt = self.add_or_find_type_id(self.project.option_type(generic_ids[0]));
                        (PRELUDE_INT_TYPE_ID, opt)
                    }
                    (IndexingMode::Range(_, _), Type::GenericInstance(struct_id, _)) if struct_id == &self.project.prelude_array_struct_id => (PRELUDE_INT_TYPE_ID, target_type_id),
                    (IndexingMode::Index(_), Type::Primitive(PrimitiveType::String)) |
                    (IndexingMode::Range(_, _), Type::Primitive(PrimitiveType::String)) => (PRELUDE_INT_TYPE_ID, PRELUDE_STRING_TYPE_ID),
                    (IndexingMode::Index(_), Type::GenericInstance(struct_id, generic_ids)) if struct_id == &self.project.prelude_map_struct_id => {
                        let key_type_id = generic_ids[0];
                        let val_type_id = self.add_or_find_type_id(self.project.option_type(generic_ids[1]));
                        (key_type_id, val_type_id)
                    }
                    (index, _) => {
                        let is_range = matches!(&index, IndexingMode::Range(_, _));
                        return Err(TypeError::InvalidIndexableType { span: target_span, is_range, type_id: target_type_id });
                    }
                };

                let typed_index = match index {
                    IndexingMode::Index(idx_node) => {
                        let typed_idx_node = self.typecheck_expression(*idx_node, Some(required_index_type_id))?;
                        let mut type_id = *typed_idx_node.type_id();

                        if self.type_contains_generics(&type_id) {
                            type_id = self.substitute_generics(&required_index_type_id, &type_id);
                        }
                        if !self.type_satisfies_other(&type_id, &required_index_type_id) {
                            return Err(TypeError::TypeMismatch { span: self.make_span(&typed_idx_node.span()), expected: vec![required_index_type_id], received: type_id });
                        };

                        IndexingMode::Index(Box::new(typed_idx_node))
                    }
                    IndexingMode::Range(range_start_node, range_end_node) => {
                        let mut typed_nodes = Vec::with_capacity(2);
                        for node in [range_start_node, range_end_node] {
                            if let Some(node) = node {
                                let typed_node = self.typecheck_expression(*node, Some(PRELUDE_INT_TYPE_ID))?;
                                let mut type_id = *typed_node.type_id();
                                if self.type_contains_generics(&type_id) {
                                    type_id = self.substitute_generics(&required_index_type_id, &type_id);
                                }
                                if !self.type_satisfies_other(&type_id, &required_index_type_id) {
                                    return Err(TypeError::TypeMismatch { span: self.make_span(&typed_node.span()), expected: vec![required_index_type_id], received: type_id });
                                };
                                typed_nodes.push(Some(Box::new(typed_node)));
                            } else {
                                typed_nodes.push(None);
                            }
                        }
                        let typed_end_node = typed_nodes.pop().unwrap();
                        let typed_start_node = typed_nodes.pop().unwrap();

                        IndexingMode::Range(typed_start_node, typed_end_node)
                    }
                };

                Ok(TypedNode::Indexing { target: Box::new(typed_target), index: typed_index, type_id: result_type_id })
            }
            AstNode::Accessor(_, n) => {
                let AstNode::Identifier(field_ident, type_args) = *n.field else { unreachable!("Internal error: an accessor's `field` must be an identifier") };
                let field_name = Token::get_ident_name(&field_ident);
                let field_span = self.make_span(&field_ident.get_range());

                let typed_target = self.typecheck_expression(*n.target, None)?;
                let mut target_type_id = *typed_target.type_id();
                let mut target_is_option_type = false;
                if n.is_opt_safe {
                    if let Some(inner_type_id) = self.type_is_option(&target_type_id) {
                        target_type_id = inner_type_id;
                        target_is_option_type = true;
                    }
                }

                if let Some(alias_module_id) = target_type_id.as_module_type_alias() {
                    let m = &self.project.modules[alias_module_id.0];
                    let Some(export) = m.exports.iter().find_map(|(name, val)| if name == &field_name { Some(val) } else { None }) else {
                        return Err(TypeError::UnknownExport { span: field_span, module_id: alias_module_id, import_name: field_name, is_aliased: true });
                    };

                    let var_id = match export {
                        ExportedValue::Function(func_id) => self.project.find_var_id_by_alias(VariableAlias::Function(*func_id)).expect("Internal error: no aliased variable for function"),
                        ExportedValue::Type(type_kind) => self.project.find_var_id_by_alias(VariableAlias::Type(*type_kind)).expect("Internal error: no aliased variable for type"),
                        ExportedValue::Variable(var_id) => *var_id,
                    };
                    let type_id = self.project.get_var_by_id(&var_id).type_id;

                    let mut type_arg_ids = Vec::with_capacity(type_args.as_ref().map(|args| args.len()).unwrap_or(0));
                    if let Some(type_args) = type_args {
                        for type_arg in type_args {
                            let type_id = self.resolve_type_identifier(&type_arg)?;
                            type_arg_ids.push((type_id, type_arg.get_ident().get_range()));
                        }
                    }

                    return Ok(TypedNode::Identifier { token: field_ident, var_id, type_arg_ids, type_id });
                }

                let mut field_data = None;
                let target_type = self.project.get_type_by_id(&target_type_id);
                if let Type::Type(id) = target_type {
                    match id {
                        TypeKind::Struct(_struct_id) => todo!("Static methods/fields not yet implemented"),
                        TypeKind::Enum(enum_id) => {
                            let enum_ = self.project.get_enum_by_id(enum_id);

                            for (idx, variant) in enum_.variants.iter().enumerate() {
                                if *variant.name == field_name {
                                    match variant.kind {
                                        EnumVariantKind::Constant => {
                                            let mut generic_ids = &enum_.generic_ids;
                                            if let Some(type_hint_id) = &type_hint {
                                                let ty = self.project.get_type_by_id(&type_hint_id);
                                                if let Type::GenericEnumInstance(_, hint_generic_ids, _) = ty {
                                                    generic_ids = hint_generic_ids;
                                                }
                                            }

                                            let type_id = self.add_or_find_type_id(Type::GenericEnumInstance(*enum_id, generic_ids.clone(), Some(idx)));
                                            field_data = Some((AccessorKind::EnumVariant, idx, type_id));
                                            break;
                                        }
                                        EnumVariantKind::Container(func_id) => {
                                            let function = self.project.get_func_by_id(&func_id);
                                            let mut type_id = self.add_or_find_type_id(self.project.function_type_for_function(function, false));
                                            if let Some(type_hint_id) = &type_hint {
                                                let ty = self.project.get_type_by_id(type_hint_id);
                                                if matches!(ty, Type::Function(_, _, _, _)) {
                                                    type_id = self.substitute_generics(type_hint_id, &type_id);
                                                }
                                            }
                                            field_data = Some((AccessorKind::EnumVariant, idx, type_id));
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else if matches!(target_type, Type::GenericEnumInstance(_, _, _)) {
                    let Some((enum_, generic_substitutions, variant_idx)) = self.project.get_enum_by_type_id(&target_type_id) else {
                        return Err(TypeError::UnknownMember { span: field_span, field_name, type_id: target_type_id });
                    };

                    if let Some(variant_idx) = variant_idx {
                        let variant = &enum_.variants[variant_idx];
                        if let EnumVariantKind::Container(func_id) = &variant.kind {
                            let variant_constructor = self.project.get_func_by_id(func_id);

                            field_data = variant_constructor.params.iter().enumerate().find_map(|(param_idx, param)| {
                                if param.name == field_name { Some((AccessorKind::Field, param_idx, param.type_id)) } else { None }
                            });
                        }
                    }
                    if field_data.is_none() {
                        for (idx, func_id) in enum_.methods.iter().enumerate() {
                            let func = self.project.get_func_by_id(func_id);
                            if func.name == field_name {
                                let type_id = self.add_or_find_type_id(self.project.function_type_for_function(&func, true));
                                field_data = Some((AccessorKind::Method, idx, type_id));
                                break;
                            }
                        }
                    }
                    if let Some((_, _, type_id)) = &mut field_data {
                        *type_id = self.substitute_generics_with_known(&type_id, &generic_substitutions);
                    }
                } else {
                    let Some((struct_, generic_substitutions)) = self.project.get_struct_by_type_id(&target_type_id) else {
                        return Err(TypeError::UnknownMember { span: field_span, field_name, type_id: target_type_id });
                    };

                    field_data = struct_.fields.iter().enumerate().find_map(|(idx, field)| {
                        if *field.name == field_name { Some((AccessorKind::Field, idx, field.type_id)) } else { None }
                    });
                    if field_data.is_none() {
                        for (idx, func_id) in struct_.methods.iter().enumerate() {
                            let func = self.project.get_func_by_id(func_id);
                            if func.name == field_name {
                                let type_id = self.add_or_find_type_id(self.project.function_type_for_function(&func, true));
                                field_data = Some((AccessorKind::Method, idx, type_id));
                                break;
                            }
                        }
                    }
                    if let Some((_, _, type_id)) = &mut field_data {
                        *type_id = self.substitute_generics_with_known(&type_id, &generic_substitutions);
                    }
                }

                if let Some((kind, member_idx, mut type_id)) = field_data {
                    if n.is_opt_safe && target_is_option_type {
                        type_id = self.add_or_find_type_id(self.project.option_type(type_id))
                    }

                    Ok(TypedNode::Accessor { target: Box::new(typed_target), kind, is_opt_safe: n.is_opt_safe, member_idx, member_span: field_ident.get_range(), type_id })
                } else {
                    Err(TypeError::UnknownMember { span: field_span, field_name, type_id: target_type_id })
                }
            }
            AstNode::Invocation(token, n) => {
                let InvocationNode { target, args } = n;

                let typed_target = self.typecheck_expression(*target, None)?;

                let mut filled_in_generic_types = HashMap::new();

                let is_param_optional = |p: &FunctionParam| {
                    // A parameter is optional if it has a default value; if we're currently in function pass 2, then the default values will be
                    // temporarily set to None and is_incomplete will be set instead.
                    p.default_value.is_some() || (matches!(self.function_pass, FunctionPass::Pass2) && p.is_incomplete)
                };

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
                                params_data = function.params.iter().enumerate().map(|(idx, p)| (idx, p.name.clone(), p.type_id, is_param_optional(&p), p.is_variadic)).collect_vec();
                                return_type_id = function.return_type_id;
                            }
                            VariableAlias::Type(id) => {
                                match id {
                                    TypeKind::Struct(alias_struct_id) => {
                                        let struct_ = self.project.get_struct_by_id(&alias_struct_id);
                                        generic_ids = &struct_.generic_ids;
                                        params_data = struct_.fields.iter().enumerate().map(|(idx, f)| (idx, f.name.clone(), f.type_id, f.default_value.is_some(), false)).collect_vec();
                                        return_type_id = struct_.self_type_id;
                                        is_instantiation = true;
                                    }
                                    TypeKind::Enum(alias_enum_id) => {
                                        let type_id = self.project.get_enum_by_id(&alias_enum_id).self_type_id;
                                        return Err(TypeError::IllegalInvocation { span: self.make_span(&typed_target.span()), type_id });
                                    }
                                }
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
                            let span = self.make_span(&span);
                            return Err(TypeError::InvalidTypeArgumentArity { span, num_required_args: generic_ids.len(), num_provided_args: type_args.len() });
                        }
                        for (generic_id, (type_arg_id, _)) in generic_ids.iter().zip(type_args.iter()) {
                            filled_in_generic_types.insert(*generic_id, *type_arg_id);
                        }
                    }
                    TypedNode::Accessor { target, kind, member_idx, is_opt_safe, .. } => {
                        let mut target_type_id = *target.type_id();
                        let mut target_is_option_type = false;
                        if *is_opt_safe {
                            if let Some(inner_type_id) = self.type_is_option(&target_type_id) {
                                target_type_id = inner_type_id;
                                target_is_option_type = true;
                            }
                        }
                        let target_ty = self.project.get_type_by_id(&target_type_id);

                        match target_ty {
                            Type::GenericInstance(struct_id, generic_ids) => {
                                let struct_ = self.project.get_struct_by_id(struct_id);
                                match kind {
                                    AccessorKind::Field => {
                                        let field_ty = self.project.get_type_by_id(&struct_.fields[*member_idx].type_id);
                                        let Type::Function(param_type_ids, num_required_args, is_variadic, ret_type_id) = field_ty else {
                                            return Err(TypeError::IllegalInvocation { span: self.make_span(&typed_target.span()), type_id: target_type_id });
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
                                        params_data = function.params.iter().skip(1).enumerate().map(|(idx, p)| (idx, p.name.clone(), p.type_id, is_param_optional(&p), p.is_variadic)).collect_vec();
                                        return_type_id = function.return_type_id;
                                    }
                                    AccessorKind::StaticMethod => todo!(),
                                    AccessorKind::EnumVariant => unreachable!(),
                                }

                                for (generic_id, type_arg_id) in struct_.generic_ids.iter().zip(generic_ids.iter()) {
                                    filled_in_generic_types.insert(*generic_id, *type_arg_id);
                                }
                            }
                            Type::GenericEnumInstance(enum_id, _generic_ids, _variant_idx) => {
                                let enum_ = self.project.get_enum_by_id(enum_id);
                                match kind {
                                    AccessorKind::Field => todo!(),
                                    AccessorKind::Method => {
                                        let function = self.project.get_func_by_id(&enum_.methods[*member_idx]);
                                        fn_is_variadic = function.is_variadic();
                                        params_data = function.params.iter().skip(1).enumerate().map(|(idx, p)| (idx, p.name.clone(), p.type_id, is_param_optional(&p), p.is_variadic)).collect_vec();
                                        return_type_id = function.return_type_id;
                                    }
                                    AccessorKind::StaticMethod => todo!(),
                                    AccessorKind::EnumVariant => todo!(),
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
                                params_data = function.params.iter().skip(1).enumerate().map(|(idx, p)| (idx, p.name.clone(), p.type_id, is_param_optional(&p), p.is_variadic)).collect_vec();
                                return_type_id = function.return_type_id;
                            }
                            Type::Type(TypeKind::Struct(_struct_id)) => todo!("Static methods on types"),
                            Type::Type(TypeKind::Enum(enum_id)) => {
                                let enum_ = self.project.get_enum_by_id(enum_id);
                                let variant = &enum_.variants[*member_idx];
                                let EnumVariantKind::Container(func_id) = variant.kind else {
                                    return Err(TypeError::IllegalEnumVariantConstruction { span: self.make_span(&typed_target.span()), enum_id: *enum_id, variant_idx: *member_idx });
                                };
                                let function = self.project.get_func_by_id(&func_id);
                                fn_is_variadic = function.is_variadic();
                                params_data = function.params.iter().enumerate().map(|(idx, p)| (idx, p.name.clone(), p.type_id, is_param_optional(&p), p.is_variadic)).collect_vec();
                                return_type_id = function.return_type_id;
                            }
                            Type::Generic(_, _) |
                            Type::Function(_, _, _, _) => todo!(),
                            Type::ModuleAlias => unreachable!(),
                        }

                        if *is_opt_safe && target_is_option_type {
                            return_type_id = self.add_or_find_type_id(self.project.option_type(return_type_id));
                        }
                    }
                    _ => {
                        let target_type_id = typed_target.type_id();
                        let target_ty = self.project.get_type_by_id(target_type_id);
                        let Type::Function(param_type_ids, num_required_args, is_variadic, ret_type_id) = target_ty else {
                            return Err(TypeError::IllegalInvocation { span: self.make_span(&typed_target.span()), type_id: *target_type_id });
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

                if let FunctionPass::Pass1 { just_saw_function_call } = &mut self.function_pass {
                    *just_saw_function_call = true;
                    return Ok(TypedNode::Invocation { target: Box::new(typed_target), arguments: vec![], type_id: return_type_id });
                }

                let num_possible_args = params_data.len();
                let num_required_args = params_data.iter().filter(|(_, _, _, is_optional, _)| !*is_optional).count();
                let num_provided_args = args.len();

                let mut seen_labels = HashSet::new();
                let mut typed_arguments = (0..params_data.len()).map(|_| None).collect_vec();
                let mut variadic_arguments = Vec::new();
                for (idx, (label, arg_node)) in args.into_iter().enumerate() {
                    if is_instantiation && label.is_none() && !forbid_labels {
                        return Err(TypeError::MissingRequiredArgumentLabels { span: self.make_span(&arg_node.get_token().get_range()) });
                    }

                    let param_idx;
                    let mut param_type_id;
                    let gather_variadic_arguments;
                    if let Some(label) = label {
                        let label_name = Token::get_ident_name(&label);
                        let label_span = self.make_span(&label.get_range());

                        if forbid_labels {
                            debug_assert!(!is_instantiation, "We should always require labels if we're instantiating");
                            return Err(TypeError::UnexpectedArgumentName { span: label_span, arg_name: label_name, is_instantiation });
                        }

                        if idx > 0 && seen_labels.is_empty() {
                            return Err(TypeError::MixedArgumentType { span: label_span });
                        }

                        if seen_labels.contains(&label_name) {
                            return Err(TypeError::DuplicateArgumentLabel { span: label_span, name: label_name });
                        }
                        let Some(param_data) = params_data.iter().find(|(_, param_name, _, _, _)| *param_name == label_name) else {
                            return Err(TypeError::UnexpectedArgumentName { span: label_span, arg_name: label_name, is_instantiation });
                        };
                        if idx >= params_data.len() {
                            // This _should_ be unreachable given the two cases above, but just in case let's return an error here as well
                            return Err(TypeError::InvalidArity { span: label_span, num_possible_args, num_required_args, num_provided_args });
                        }

                        seen_labels.insert(label_name);

                        param_idx = param_data.0;
                        param_type_id = param_data.2;
                        if param_data.4 { // is variadic
                            param_type_id = self.add_or_find_type_id(self.project.array_type(param_data.2));
                        }
                        gather_variadic_arguments = false;
                    } else if idx > 0 && !seen_labels.is_empty() {
                        return Err(TypeError::MixedArgumentType { span: self.make_span(&arg_node.get_token().get_range()) });
                    } else {
                        gather_variadic_arguments = idx >= params_data.len().saturating_sub(1) && fn_is_variadic;

                        if idx >= params_data.len() {
                            if !fn_is_variadic {
                                let span = self.make_span(&typed_target.span().expand(&token.get_range()));
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
                    if self.type_contains_generics(&param_type_id) && !filled_in_generic_types.is_empty() {
                        param_type_id = self.substitute_generics_with_known(&param_type_id, &filled_in_generic_types);
                    }

                    let typed_arg_value = self.typecheck_expression(arg_node, Some(param_type_id))?;
                    let arg_type_id = *typed_arg_value.type_id();

                    if arg_type_id == PRELUDE_UNIT_TYPE_ID {
                        return Err(TypeError::ForbiddenAssignment { span: self.make_span(&typed_arg_value.span()), type_id: arg_type_id, purpose: "parameter" });
                    }

                    // Fill in any resolved generics that are only determined after typechecking arg expression (ie. function return types)
                    if self.type_contains_generics(&param_type_id) {
                        self.extract_values_for_generics(&arg_type_id, &param_type_id, &mut filled_in_generic_types);

                        param_type_id = self.substitute_generics_with_known(&param_type_id, &filled_in_generic_types);
                    }

                    if !self.type_satisfies_other(&arg_type_id, &param_type_id) {
                        return Err(TypeError::TypeMismatch { span: self.make_span(&typed_arg_value.span()), expected: vec![param_type_id], received: arg_type_id });
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
                        let span = self.make_span(&typed_target.span().expand(&token.get_range()));
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
            AstNode::IfExpression(token, if_node) => self.typecheck_if_node(token, if_node, true, type_hint),
            AstNode::MatchExpression(token, match_node) => self.typecheck_match_node(token, match_node, true, type_hint),
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

                let prev_scope_id = self.current_scope_id;
                let fn_scope_id = self.begin_child_scope(format!("{:?}.lambda_{}", &self.current_module().id, self.new_lambda_fn_name()), ScopeKind::Function(FuncId::BOGUS));
                let parameters = self.typecheck_function_parameters_pass_1(false, &parameters, false)?;

                let lambda_func_id = self.add_lambda_function_to_scope(&prev_scope_id, fn_scope_id, parameters)?;
                let ScopeKind::Function(id) = &mut self.project.get_scope_by_id_mut(&fn_scope_id).kind else { unreachable!() };
                *id = lambda_func_id;
                let prev_func_id = self.current_function;
                self.current_function = Some(lambda_func_id);

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
                                return Err(TypeError::TypeMismatch { span: self.make_span(&typed_node.span()), expected: vec![type_hint], received: last_type_id });
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
                    .and_then(|p| p.defined_span.as_ref().map(|s| &s.range)).unwrap_or(&token.get_range())
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
