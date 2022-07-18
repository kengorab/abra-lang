use crate::builtins::native_value_trait::NativeTyp;
use crate::builtins::prelude::{NativeArray, NativeMap, NativeSet, NativeFloat, NativeInt, NativeString};
use crate::common::ast_visitor::AstVisitor;
use crate::lexer::tokens::{Token, Position};
use crate::parser::ast::{AstNode, AstLiteralNode, UnaryNode, BinaryNode, BinaryOp, UnaryOp, ArrayNode, BindingDeclNode, AssignmentNode, IndexingNode, IndexingMode, GroupedNode, IfNode, FunctionDeclNode, InvocationNode, WhileLoopNode, ForLoopNode, TypeDeclNode, MapNode, AccessorNode, LambdaNode, TypeIdentifier, EnumDeclNode, MatchNode, MatchCase, MatchCaseType, SetNode, BindingPattern, TypeDeclField, ImportNode, ModuleId, MatchCaseArgument, ImportKind, TryNode};
use crate::typechecker::types::{Type, StructType, FnType, EnumType, StructTypeField, FieldSpec};
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode, TypedInvocationNode, TypedWhileLoopNode, TypedForLoopNode, TypedTypeDeclNode, TypedMapNode, TypedAccessorNode, TypedInstantiationNode, AssignmentTargetKind, TypedLambdaNode, TypedEnumDeclNode, TypedMatchNode, TypedReturnNode, TypedTupleNode, TypedSetNode, TypedTypeDeclField, TypedImportNode, TypedMatchKind, TypedMatchCaseArgument};
use crate::typechecker::typechecker_error::{TypecheckerErrorKind, InvalidAssignmentTargetReason, TypecheckerError};
use crate::{ModuleLoader, ModuleReader, tokenize_and_parse};
use itertools::Itertools;
use std::collections::{HashSet, HashMap};
use std::iter::FromIterator;

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeBinding(pub /*token:*/ Token, pub /*type:*/ Type, pub /*is_mutable:*/ bool);

#[derive(Debug, Clone, PartialEq)]
pub struct FnScopeKind {
    token: Token,
    name: String,
    is_recursive: bool,
    return_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeKind {
    Root,
    Block,
    Function(FnScopeKind),
    Lambda(/*token: */ Token),
    TypeDef,
    Loop,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub kind: ScopeKind,
    pub bindings: HashMap<String, (ScopeBinding, /* is_import: */ bool)>,
    // Track hoisted fn defs for a scope; upon real visiting, binding will be stored in bindings
    pub fns: HashMap<String, ScopeBinding>,
    pub types: HashMap<String, (Type, /* Must be a TypedAstNode::TypeDecl */ Option<TypedAstNode>, /* is_import: */ bool)>,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Scope { kind, bindings: HashMap::new(), fns: HashMap::new(), types: HashMap::new() }
    }
}

#[derive(Debug, Clone)]
pub enum ExportedValue {
    Binding(Type),
    Type { reference: Option<Type>, backing_type: Type, node: Option<TypedAstNode> },
}

#[derive(Debug, Clone)]
pub struct TypedModule {
    pub module_id: ModuleId,
    pub typed_nodes: Vec<TypedAstNode>,
    pub referencable_types: HashMap<String, Type>,
    pub global_bindings: HashMap<String, ScopeBinding>,
    pub types: HashMap<String, Type>,
    pub exports: HashMap<String, ExportedValue>,
}

pub fn typecheck<R: ModuleReader>(module_id: ModuleId, ast: Vec<AstNode>, loader: &ModuleLoader<R>) -> Result<TypedModule, TypecheckerError> {
    let mut typechecker = Typechecker {
        module_id,
        cur_typedef: None,
        scopes: vec![Scope::new(ScopeKind::Root)],
        num_lambdas: 0,
        referencable_types: HashMap::new(),
        exports: HashMap::new(),
        module_loader: loader,
    };

    // Inject wildcard prelude import
    let prelude_import_nodes = tokenize_and_parse(&typechecker.module_id, &"import * from \"prelude\"".to_string()).unwrap().nodes;
    for node in prelude_import_nodes {
        if let Err(kind) = typechecker.visit(node) {
            return Err(TypecheckerError { module_id: typechecker.module_id, kind });
        }
    }

    let mut results = Vec::new();
    let mut ast_iter = ast.into_iter().peekable();
    while let Some(AstNode::ImportStatement(_, _)) = ast_iter.peek() {
        let node = ast_iter.next().unwrap();
        results.push(match typechecker.visit(node) {
            Ok(node) => node,
            Err(kind) => return Err(TypecheckerError { module_id: typechecker.module_id, kind })
        });
    }

    let ast = ast_iter.collect();
    if let Err(kind) = typechecker.hoist_declarations_in_scope(&ast) {
        return Err(TypecheckerError { module_id: typechecker.module_id, kind });
    }

    let nodes = ast.into_iter()
        .map(|node| typechecker.visit(node))
        .collect::<Result<Vec<_>, TypecheckerErrorKind>>();
    let nodes = match nodes {
        Ok(nodes) => nodes,
        Err(kind) => return Err(TypecheckerError { module_id: typechecker.module_id, kind })
    };

    let typed_nodes = results.into_iter().chain(nodes).collect();

    let scope = typechecker.scopes.pop().expect("There should be a top-level scope");
    let module_name = typechecker.module_loader.get_module_name(&typechecker.module_id);

    let module = TypedModule {
        module_id: typechecker.module_id,
        typed_nodes,
        referencable_types: typechecker.referencable_types.into_iter()
            .filter(|(key, _)| key.starts_with(&module_name))
            .collect(),
        global_bindings: scope.bindings.into_iter()
            .filter_map(|(name, (binding, is_import))| if !is_import { Some((name, binding)) } else { None })
            .collect(),
        types: scope.types.into_iter()
            .filter_map(|(k, (t, _, is_import))| if !is_import { Some((k, t)) } else { None })
            .collect(),
        exports: typechecker.exports,
    };

    Ok(module)
}

pub struct Typechecker<'a, R: ModuleReader> {
    module_id: ModuleId,
    cur_typedef: Option<Type>,
    scopes: Vec<Scope>,
    num_lambdas: usize,
    referencable_types: HashMap<String, Type>,
    exports: HashMap<String, ExportedValue>,
    module_loader: &'a ModuleLoader<'a, R>,
}

impl<'a, R: 'a + ModuleReader> Typechecker<'a, R> {
    fn get_binding(&self, name: &str) -> Option<(&ScopeBinding, usize)> {
        let mut depth = 0;
        let mut fn_depth = 0;
        for scope in self.scopes.iter().rev() {
            match &scope.kind {
                ScopeKind::Function(_) | ScopeKind::Root => fn_depth += 1,
                _ => {}
            }
            match scope.bindings.get(name) {
                None => {
                    // If we've crossed at least 1 fn-scope border, then consider "pre-hoisted" fn defs from that outer scope
                    // A scope's `fns` represent the signatures of all fns in that scope, regardless of ordering. We can only
                    // consider these as valid options for a binding resolution if we've crossed at least 1 fn-scope boundary
                    // or else we allow invocations of fns that haven't been defined yet:
                    //   if true { if true { abc() } func abc() = println("") }        VS
                    //   if true { func def() { if true { abc() } } func abc() = println("") }
                    if fn_depth >= 1 {
                        if let Some(binding) = scope.fns.get(name) {
                            return Some((binding, self.scopes.len() - depth - 1));
                        }
                    }
                    depth += 1;
                    continue;
                }
                Some((binding, _)) => return Some((binding, self.scopes.len() - depth - 1))
            }
        }
        None
    }

    fn get_binding_in_current_scope(&self, name: &str) -> Option<&ScopeBinding> {
        self.scopes.last().and_then(|scope| scope.bindings.get(name).map(|(b, _)| b))
    }

    fn get_fn_binding_in_current_scope(&self, name: &str) -> Option<&ScopeBinding> {
        self.scopes.last().and_then(|scope| scope.fns.get(name))
    }

    fn add_binding(&mut self, name: &str, ident: &Token, typ: &Type, is_mutable: bool) {
        if name == "_" { return; }

        let scope = self.scopes.last_mut().unwrap();
        let binding = ScopeBinding(ident.clone(), typ.clone(), is_mutable);
        scope.bindings.insert(name.to_string(), (binding, false));
    }

    fn add_imported_binding(&mut self, name: &str, ident: &Token, typ: &Type) {
        if name == "_" { return; }

        let scope = self.scopes.last_mut().unwrap();
        let binding = ScopeBinding(ident.clone(), typ.clone(), false);
        scope.bindings.insert(name.to_string(), (binding, true));
    }

    fn add_fn_binding(&mut self, name: &str, ident: &Token, typ: &Type) {
        let scope = self.scopes.last_mut().unwrap();
        let binding = ScopeBinding(ident.clone(), typ.clone(), false);
        scope.fns.insert(name.to_string(), binding);
    }

    fn get_binding_mut(&mut self, name: &str) -> Option<&mut ScopeBinding> {
        self.scopes.iter_mut().rev()
            .flat_map(|scope| scope.bindings.iter_mut())
            .find(|(binding_name, _)| binding_name == &name)
            .map(|(_, (binding, _))| binding)
    }

    fn get_types_in_scope(&self) -> HashMap<String, Type> {
        self.scopes.iter().rev().flat_map(|scope| scope.types.iter())
            .map(|(name, (typ, _, _))| (name.clone(), typ.clone()))
            .collect()
    }

    fn get_generics_in_scope(&self) -> HashSet<String> {
        self.scopes.iter().rev().flat_map(|scope| scope.types.iter())
            .filter_map(|(_, (typ, _, _))| if let Type::Generic(name) = typ { Some(name.clone()) } else { None })
            .collect()
    }

    fn type_from_type_ident(&self, type_ident: &TypeIdentifier, use_placeholder_generics: bool) -> Result<Type, TypecheckerErrorKind> {
        let type_from_ident = Type::from_type_ident(type_ident, &self.get_types_in_scope());
        match type_from_ident {
            Err(tok) => Err(TypecheckerErrorKind::UnknownType { type_ident: tok }),
            Ok(typ) => {
                let mut ret_typ = None;
                let mut placeholder_type_args: Vec<Type> = vec![];
                if let Type::Reference(name, ref_type_args) = &typ {
                    let expected_type_args = match self.resolve_type(name).unwrap() {
                        Type::Struct(StructType { type_args, .. }) |
                        Type::Enum(EnumType { type_args, .. }) => {
                            placeholder_type_args = type_args.iter().map(|(_, t)| t.clone()).collect();
                            type_args.len()
                        }
                        Type::Array(_) | Type::Set(_) => {
                            ret_typ = Some(Type::hydrate_reference_type(name, ref_type_args, &|typ_name| self.resolve_type(typ_name)).unwrap());
                            1
                        }
                        Type::Map(_, _) => {
                            ret_typ = Some(Type::hydrate_reference_type(name, ref_type_args, &|typ_name| self.resolve_type(typ_name)).unwrap());
                            2
                        }
                        _ => unimplemented!()
                    };

                    if ref_type_args.len() != expected_type_args {
                        if !use_placeholder_generics {
                            return Err(TypecheckerErrorKind::InvalidTypeArgumentArity {
                                token: type_ident.get_ident(),
                                actual_type: self.resolve_type(name).unwrap().clone(),
                                actual: ref_type_args.len(),
                                expected: expected_type_args,
                            });
                        } else {
                            debug_assert!(ref_type_args.is_empty());
                            ret_typ = Some(Type::Reference(name.clone(), placeholder_type_args));
                        }
                    }
                }
                Ok(ret_typ.unwrap_or(typ))
            }
        }
    }

    fn add_type(&mut self, name: String, type_decl_node: Option<TypedAstNode>, typ: Type, is_import: bool) {
        let scope = self.scopes.last_mut().unwrap();
        scope.types.insert(name, (typ, type_decl_node, is_import));
    }

    fn get_type(&self, name: &String) -> Option<(Type, Option<TypedAstNode>)> {
        self.scopes.iter().rev()
            .flat_map(|scope| scope.types.iter())
            .find(|(type_name, _)| type_name == &name)
            .map(|(_, (typ, type_decl_node, _))| (typ.clone(), type_decl_node.clone()))
    }

    fn get_type_mut(&mut self, name: &String) -> Option<(&mut Type, &mut Option<TypedAstNode>)> {
        self.scopes.iter_mut().rev()
            .flat_map(|scope| scope.types.iter_mut())
            .find(|(type_name, _)| type_name == &name)
            .map(|(_, (typ, type_decl_node, _))| (typ, type_decl_node))
    }

    fn resolve_ref_type(&self, typ: &Type) -> Type {
        match typ {
            Type::Reference(name, type_args) => {
                Type::hydrate_reference_type(name, type_args, &|typ_name| self.resolve_type(typ_name))
                    .expect(&format!("There should be a type referencable by name '{}'", name))
            }
            t => t.clone()
        }
    }

    fn resolve_type(&self, type_name: &String) -> Option<&Type> {
        self.referencable_types.get(type_name)
            .or_else(|| self.module_loader.resolve_type(type_name))
    }

    fn are_types_equivalent(&mut self, node: &mut TypedAstNode, target_type: &Type) -> Result<bool, TypecheckerErrorKind> {
        // If the node's type has an unknown, we need to descend into it.
        //
        // Consider  `val arr: Int[] = []`; the rhs would be Type::Array(Box::new(Type::Unknown)),
        // iterate over all values (there are none), and if any don't match we return false; if
        // all match (they will, there are none), we update the node's type to be the target_type.
        //
        // Consider  `val fns: ((Int) => Int)[] = [x => x]`; the rhs would be
        // Type::Array(Box::new(Type::Fn(vec![("x", Type::Unknown, false)], Box::new(Type::Unknown)))).
        // Iterate over each of the nodes in the array (recursively calling are_types_equivalent). For lambdas,
        // this will re-typecheck the lambda using the captured scope, and will mutate the node (will rust allow this?).
        // Then when we get "back up to the top", set the type of the array node to be the target_type if nothing goes wrong,
        // and return true.
        let typ = if node.get_type().is_unknown(&|type_name| self.resolve_type(type_name)) {
            match (node, target_type) {
                (TypedAstNode::Lambda(token, lambda_node), Type::Fn(fn_type)) => {
                    let FnType { arg_types: expected_args, ret_type: expected_ret, .. } = fn_type;

                    let mut target_args_iter = expected_args.iter();
                    let mut lambda_args_iter = lambda_node.args.iter();

                    let mut retyped_args = Vec::new();
                    loop {
                        match (target_args_iter.next(), lambda_args_iter.next()) {
                            (Some((_, target_arg_type, _)), Some((lambda_arg_token, _, lambda_arg_default_val))) => {
                                retyped_args.push((lambda_arg_token.clone(), target_arg_type.clone(), lambda_arg_default_val.clone()));
                            }
                            (None, Some((lambda_arg_token, lambda_arg_type, default_value))) => {
                                if default_value.is_none() {
                                    return Err(TypecheckerErrorKind::IncorrectArity { token: token.clone(), expected: expected_args.len(), actual: lambda_node.args.len() });
                                }
                                retyped_args.push((lambda_arg_token.clone(), lambda_arg_type.clone(), default_value.clone()));
                            }
                            (Some(_), None) => {
                                return Err(TypecheckerErrorKind::IncorrectArity { token: token.clone(), expected: expected_args.len(), actual: lambda_node.args.len() });
                            }
                            (None, None) => break
                        }
                    }

                    // We need to re-typecheck the lambda, so load in the original scopes from the lambda's context, and re-visit it
                    let (orig_node, orig_scopes) = lambda_node.orig_node.as_ref().unwrap().clone();
                    let mut scopes = orig_scopes;
                    std::mem::swap(&mut self.scopes, &mut scopes);

                    let retyped_lambda = self.visit_lambda(token.clone(), orig_node, Some((retyped_args, lambda_node.idx)))?;
                    let lambda_type = retyped_lambda.get_type();

                    // After re-typechecking, it _is_ possible that some of the scopes may have been modified, and those modifications
                    // need to bubble up to the real `scopes`. One such example is a function whose recursive call is within a lambda:
                    //   func abc() {
                    //       func def(): Int { // <-- not recognized as recursive
                    //           [1, 2, 3].map(_ => def()).length
                    //       }
                    //   }
                    std::mem::swap(&mut self.scopes, &mut scopes);
                    for Scope { kind, .. } in scopes {
                        if let ScopeKind::Function(FnScopeKind { token: tok, is_recursive: is_rec, .. }) = kind {
                            for s in &mut self.scopes {
                                match &mut s.kind {
                                    ScopeKind::Function(FnScopeKind { token: tok_real, is_recursive: ref mut is_rec_real, .. }) if *tok_real == tok => {
                                        *is_rec_real = is_rec;
                                        break;
                                    }
                                    _ => continue
                                }
                            }
                        }
                    }

                    if let TypedAstNode::Lambda(_, retyped_lambda_node) = retyped_lambda {
                        *lambda_node = retyped_lambda_node;
                    } else { unreachable!() }

                    if **expected_ret == Type::Unit {
                        return Ok(true);
                    } else {
                        if let Type::Fn(FnType { ret_type: ret, .. }) = lambda_type {
                            // If the return type of a fn is Generic, that means we haven't been able to figure out what that
                            // generic value should be yet. Maybe it's the return type annotation for a function (in which case,
                            // we'll only know this at invocation-time); or it's conditional on the re-evaluation of the lambda
                            // node. Return true here, under the assumption that it'll be determined outside of this function.
                            if expected_ret.has_unbound_generic() {
                                return Ok(true);
                            }

                            if !ret.is_equivalent_to(expected_ret, &|typ_name| self.resolve_type(typ_name)) {
                                return Err(TypecheckerErrorKind::Mismatch {
                                    token: token.clone(),
                                    expected: *(*expected_ret).clone(),
                                    actual: *ret,
                                });
                            }

                            return Ok(true);
                        } else { unreachable!() };
                    }
                }
                (TypedAstNode::Array(_, ref mut array_node), Type::Array(ref inner_type)) => {
                    for mut item in &mut array_node.items {
                        if !self.are_types_equivalent(&mut item, inner_type)? {
                            return Ok(false);
                        }
                    }
                    array_node.typ = Type::Array(inner_type.clone());
                    array_node.typ.clone()
                }
                (TypedAstNode::Set(_, ref mut set_node), Type::Set(ref inner_type)) => {
                    for mut item in &mut set_node.items {
                        if !self.are_types_equivalent(&mut item, inner_type)? {
                            return Ok(false);
                        }
                    }
                    set_node.typ = Type::Set(inner_type.clone());
                    set_node.typ.clone()
                }
                (TypedAstNode::Map(_, ref mut map_node), Type::Map(ref key_type, ref value_type)) => {
                    // TODO: Punting on non-String key types in map literals

                    for (_, ref mut v) in &mut map_node.items {
                        if !self.are_types_equivalent(v, value_type)? {
                            return Ok(false);
                        }
                    }
                    map_node.typ = Type::Map(key_type.clone(), value_type.clone());
                    map_node.typ.clone()
                }
                (node, _) => self.resolve_ref_type(&node.get_type())
            }
        } else {
            self.resolve_ref_type(&node.get_type())
        };

        // If the target_type contains an `Unknown` in some shape or form, we should compare by
        // structure; for example:
        //   [].contains("a"), [].concat([1, 3]) // Should both pass
        if target_type.is_unknown(&|type_name| self.resolve_type(type_name)) {
            let is_same_shape = match (typ, target_type) {
                (_, Type::Unknown) => true,
                (Type::Array(_), Type::Array(i2)) if **i2 == Type::Unknown => true,
                (Type::Set(_), Type::Set(i2)) if **i2 == Type::Unknown => true,
                (Type::Map(_, _), Type::Map(_, i2)) if **i2 == Type::Unknown => true,
                _ => false
            };
            Ok(is_same_shape)
        } else {
            Ok(typ.is_equivalent_to(target_type, &|typ_name| self.resolve_type(typ_name)))
        }
    }

    fn visit_block(&mut self, is_stmt: bool, body: Vec<AstNode>) -> Result<Vec<TypedAstNode>, TypecheckerErrorKind> {
        let mut has_terminated = false;
        let len = body.len();
        body.into_iter().enumerate()
            .map(|(idx, mut node)| {
                if has_terminated {
                    return Err(TypecheckerErrorKind::UnreachableCode { token: node.get_token().clone() });
                }

                // If the last node of an if-expression is an if-statement, treat it as an if-expr.
                // This is due to the fact that if-blocks are only treated as expressions in certain
                // situations, namely when an expression is already expected. Otherwise, they're parsed
                // as an if-statement. However, since the last item in an if-expression's block is the
                // "return value" for that block, the last slot is ALSO a valid place for an expression
                // to be. This is much more difficult to represent in the parser, so it's done here.
                if !is_stmt && idx == len - 1 {
                    if let AstNode::IfStatement(token, if_node) = node {
                        node = AstNode::IfExpression(token, if_node)
                    } else if let AstNode::MatchStatement(token, match_node) = node {
                        node = AstNode::MatchExpression(token, match_node)
                    }
                }
                let typed_node = self.visit(node)?;
                has_terminated = typed_node.all_branches_terminate().is_some();

                Ok(typed_node)
            })
            .collect()
    }

    // Called from visit_if_expression and visit_if_statement, but it has to be up here since it's
    // not part of the AstVisitor trait.
    fn visit_if_node(&mut self, is_stmt: bool, node: IfNode) -> Result<TypedIfNode, TypecheckerErrorKind> {
        let IfNode { condition, mut condition_binding, if_block, else_block } = node;

        let condition = self.visit(*condition)?;
        let is_valid_cond_type = match condition.get_type() {
            Type::Option(_) | Type::Bool => true,
            _ => false
        };
        if !is_valid_cond_type {
            let token = condition.get_token().clone();
            return Err(TypecheckerErrorKind::InvalidIfConditionType { token, actual: condition.get_type() });
        }
        let condition = Box::new(condition);

        self.scopes.push(Scope::new(ScopeKind::Block));
        if let Some(pat) = &mut condition_binding {
            let binding_type = match condition.get_type() {
                Type::Bool => Type::Bool,
                Type::Option(inner) if *inner == Type::Bool => Type::Bool,
                Type::Option(inner) => inner.get_opt_unwrapped(),
                _ => unreachable!("No other types should be allowable as conditionals")
            };
            self.visit_binding_pattern(pat, &binding_type, false)?;
        }
        self.hoist_declarations_in_scope(&if_block)?;
        let if_block = self.visit_block(is_stmt, if_block)?;
        self.scopes.pop();

        let else_block = match else_block {
            None => None,
            Some(body) => {
                self.scopes.push(Scope::new(ScopeKind::Block));
                self.hoist_declarations_in_scope(&body)?;
                let else_block = self.visit_block(is_stmt, body)?;
                self.scopes.pop();

                Some(else_block)
            }
        };

        // Temporarily use Type::Unit as a placeholder, if it's an expression it'll be updated later
        Ok(TypedIfNode { typ: Type::Unit, condition, condition_binding, if_block, else_block })
    }

    fn flatten_match_case_types(&self, typ: Type) -> Vec<Type> {
        match typ {
            Type::Union(opts) => opts.into_iter().flat_map(|t| self.flatten_match_case_types(t)).collect(),
            Type::Option(t) => vec![self.flatten_match_case_types(*t), vec![Type::Unknown]].concat(),
            Type::Reference(name, type_args) => vec![
                Type::hydrate_reference_type(&name, &type_args, &|typ_name| self.resolve_type(typ_name)).unwrap()
            ],
            t => vec![t]
        }
    }

    // Called from visit_match_expression and visit_match_statement, but it has to be up here since it's
    // not part of the AstVisitor trait.
    fn visit_match_node(&mut self, is_stmt: bool, match_token: &Token, node: MatchNode) -> Result<TypedMatchNode, TypecheckerErrorKind> {
        let MatchNode { target, branches } = node;

        let target = self.visit(*target)?;
        let mut possibilities = self.flatten_match_case_types(target.get_type());
        let mut enum_variant_possibilities = possibilities.iter()
            .filter_map(|t| match t {
                Type::Enum(EnumType { name, variants, .. }) => Some((name.clone(), variants.iter().map(|(name, _)| name.clone()).collect())),
                _ => None
            })
            .collect::<HashMap<_, Vec<_>>>();
        let mut seen_enum_variant_literal_cases = HashMap::<String, HashMap<Vec<Option<AstLiteralNode>>, Token>>::new();
        let mut seen_wildcard = false;

        let mut typed_branches = Vec::new();
        for (case, block) in branches {
            let MatchCase { token, match_type, case_binding } = case;

            self.scopes.push(Scope::new(ScopeKind::Block));

            if block.is_empty() && !is_stmt {
                let token = match match_type {
                    MatchCaseType::None(ident) => ident,
                    MatchCaseType::Ident(ident, _) => ident,
                    MatchCaseType::Wildcard(token) => token,
                    MatchCaseType::Compound(idents, _) => idents.get(1).expect("There should be at least 2 idents").clone(),
                    MatchCaseType::Constant(expr) => expr.get_token().clone(),
                    MatchCaseType::Tuple(lparen, _) => lparen,
                };
                return Err(TypecheckerErrorKind::EmptyMatchBlock { token });
            }

            let mut binding = None;
            let branch_cond = match match_type {
                MatchCaseType::None(token) => {
                    let idx = possibilities.iter().position(|t| t == &Type::Unknown);
                    if let Some(idx) = idx {
                        possibilities.remove(idx);

                        if let Some(ident) = case_binding {
                            let ident_name = Token::get_ident_name(&ident).clone();
                            self.add_binding(ident_name.as_str(), &ident, &Type::Unknown, false);
                            binding = Some(ident_name)
                        }

                        TypedMatchKind::None
                    } else {
                        return Err(TypecheckerErrorKind::UnreachableMatchCase { token, typ: None, is_unreachable_none: true, prior_covering_case_tok: None });
                    }
                }
                MatchCaseType::Ident(ident, args) => {
                    if seen_wildcard {
                        return Err(TypecheckerErrorKind::UnreachableMatchCase { token: ident, typ: None, is_unreachable_none: false, prior_covering_case_tok: None });
                    }

                    let type_ident = TypeIdentifier::Normal { ident: ident.clone(), type_args: None };
                    let typ = self.type_from_type_ident(&type_ident, true)?;
                    if let Type::Generic(_) = &typ {
                        return Err(TypecheckerErrorKind::Unimplemented(ident, "Cannot match against generic types in match case arms".to_string()));
                    } else if args.is_some() {
                        return Err(TypecheckerErrorKind::InvalidMatchCaseDestructuring { token: ident, typ: Some(typ), enum_variant: None });
                    }

                    let possibility = possibilities.iter().enumerate()
                        .find(|(_, p)| typ.is_equivalent_to(p, &|typ_name| self.resolve_type(typ_name)));
                    if let Some((idx, typ)) = possibility {
                        if let Some(ident) = case_binding {
                            let ident_name = Token::get_ident_name(&ident).clone();
                            self.add_binding(ident_name.as_str(), &ident, &typ, false);
                            binding = Some(ident_name)
                        }

                        possibilities.remove(idx);
                        TypedMatchKind::Type { type_name: Token::get_ident_name(&ident), args: None }
                    } else {
                        return Err(TypecheckerErrorKind::UnreachableMatchCase { token: ident, typ: Some(typ), is_unreachable_none: false, prior_covering_case_tok: None });
                    }
                }
                MatchCaseType::Compound(idents, args) => {
                    let mut idents = idents.into_iter();
                    let ident = idents.next().expect("There should be at least one ident");
                    let enum_name = Token::get_ident_name(&ident);
                    let type_ident = TypeIdentifier::Normal { ident, type_args: None };
                    let typ = self.type_from_type_ident(&type_ident, true)?;
                    let enum_type = match self.resolve_ref_type(&typ) {
                        Type::Enum(enum_type) => enum_type,
                        _ => unreachable!("Unexpected non-enum compound type used as match condition")
                    };

                    let variant_ident = idents.next().expect("There should be at least 2 idents");
                    let variant_name = Token::get_ident_name(&variant_ident);
                    let variant_idx = enum_type.variants.iter().position(|(name, _)| name == &variant_name);
                    if variant_idx.is_none() {
                        return Err(TypecheckerErrorKind::UnknownMember { token: variant_ident, target_type: typ, module_name: None });
                    } else if let Some(token) = idents.next() {
                        return Err(TypecheckerErrorKind::UnknownMember { token, target_type: typ, module_name: None });
                    }
                    let variant_idx = variant_idx.expect("An error is raised if it's None");

                    let possibility = possibilities.iter().enumerate()
                        .find(|(_, p)| typ.is_equivalent_to(p, &|typ_name| self.resolve_type(typ_name)));
                    let generics = match &mut enum_variant_possibilities.get_mut(&enum_type.name) {
                        None => return Err(TypecheckerErrorKind::UnreachableMatchCase { token: variant_ident, typ: Some(typ), is_unreachable_none: false, prior_covering_case_tok: None }),
                        Some(variants) => {
                            match variants.iter().position(|v| *v == variant_name) {
                                None => return Err(TypecheckerErrorKind::DuplicateMatchCase { token: variant_ident }),
                                Some(variant_idx) => {
                                    let (idx, possibility_type) = possibility.unwrap();

                                    let lit_args = MatchCaseType::get_lit_args(&args);
                                    let possibility_type = if let Some(lit_args) = lit_args {
                                        let variant_full_name = format!("{}.{}", &enum_type.name, &variant_name);
                                        match seen_enum_variant_literal_cases.get_mut(&variant_full_name) {
                                            Some(variant_lit_cases) => {
                                                if let Some(prior_covering_case_tok) = variant_lit_cases.get(&lit_args) {
                                                    let prior_covering_case_tok = Some(prior_covering_case_tok.clone());
                                                    return Err(TypecheckerErrorKind::UnreachableMatchCase { token: variant_ident, typ: None, is_unreachable_none: false, prior_covering_case_tok });
                                                } else {
                                                    for (case, case_token) in variant_lit_cases.iter() {
                                                        debug_assert!(case.len() == lit_args.len());
                                                        let mut prior_covering_case_tok = None;
                                                        for (seen_arg, case_arg) in case.iter().zip(lit_args.iter()) {
                                                            match (seen_arg, case_arg) {
                                                                (None, Some(_)) => {
                                                                    prior_covering_case_tok = Some(case_token.clone());
                                                                }
                                                                (Some(_), None) => {
                                                                    prior_covering_case_tok = None;
                                                                    break;
                                                                }
                                                                (Some(v1), Some(v2)) => {
                                                                    if v1 != v2 {
                                                                        prior_covering_case_tok = None;
                                                                        break;
                                                                    }
                                                                }
                                                                (None, None) => { continue; }
                                                            }
                                                        }
                                                        if prior_covering_case_tok.is_some() {
                                                            return Err(TypecheckerErrorKind::UnreachableMatchCase { token: variant_ident, typ: None, is_unreachable_none: false, prior_covering_case_tok });
                                                        }
                                                    }

                                                    variant_lit_cases.insert(lit_args, variant_ident.clone());
                                                }
                                            }
                                            None => {
                                                let mut map = HashMap::new();
                                                map.insert(lit_args, variant_ident.clone());
                                                seen_enum_variant_literal_cases.insert(variant_full_name, map);
                                            }
                                        }
                                        possibility_type.clone()
                                    } else {
                                        // Remove the variant, since we've now seen it. If we've seen all variants, remove the enum itself from consideration
                                        variants.remove(variant_idx);
                                        if variants.is_empty() {
                                            enum_variant_possibilities.remove(&enum_type.name);
                                            possibilities.remove(idx)
                                        } else {
                                            possibility_type.clone()
                                        }
                                    };
                                    if let Type::Enum(EnumType { type_args, .. }) = possibility_type {
                                        type_args.clone().into_iter().collect::<HashMap<_, _>>()
                                    } else { unreachable!() }
                                }
                            }
                        }
                    };

                    if let Some(ident) = case_binding {
                        let ident_name = Token::get_ident_name(&ident).clone();
                        self.add_binding(ident_name.as_str(), &ident, &typ, false);
                        binding = Some(ident_name)
                    }

                    let args = if let Some(destructured_args) = args {
                        match &enum_type.variants[variant_idx].1 {
                            Type::Fn(fn_type) => {
                                let FnType { is_enum_constructor, arg_types, .. } = &fn_type;
                                debug_assert!(is_enum_constructor);
                                if arg_types.len() != destructured_args.len() {
                                    let arg_types_len = arg_types.len();
                                    return Err(TypecheckerErrorKind::InvalidMatchCaseDestructuringArity { token, typ, enum_variant: Some(variant_name), expected: arg_types_len, actual: destructured_args.len() });
                                }

                                let mut args = Vec::new();
                                for ((arg_name, arg_type, _), pat) in arg_types.iter().zip(destructured_args.into_iter()) {
                                    let arg_type = match &arg_type {
                                        Type::Generic(g) => generics.get(g).unwrap(),
                                        t => t
                                    };
                                    let arg = match pat {
                                        MatchCaseArgument::Pattern(mut pat) => {
                                            self.visit_binding_pattern(&mut pat, arg_type, false)?;
                                            TypedMatchCaseArgument::Pattern(pat)
                                        }
                                        MatchCaseArgument::Literal(node) => {
                                            let mut typed_node = self.visit(node.clone())?;
                                            if !self.are_types_equivalent(&mut typed_node, arg_type)? {
                                                return Err(TypecheckerErrorKind::Mismatch { token: typed_node.get_token().clone(), expected: arg_type.clone(), actual: typed_node.get_type() });
                                            }
                                            TypedMatchCaseArgument::Literal(typed_node)
                                        }
                                    };
                                    args.push((arg_name.clone(), arg));
                                }
                                Some(args)
                            }
                            _ => {
                                let variant_name = enum_type.variants[variant_idx].0.clone();
                                return Err(TypecheckerErrorKind::InvalidMatchCaseDestructuring { token: variant_ident, typ: Some(typ), enum_variant: Some(variant_name) });
                            }
                        }
                    } else { None };

                    TypedMatchKind::EnumVariant { enum_name, variant_idx, variant_name, args }
                }
                MatchCaseType::Wildcard(token) => {
                    if seen_wildcard {
                        return Err(TypecheckerErrorKind::DuplicateMatchCase { token });
                    }
                    seen_wildcard = true;
                    let match_type = if possibilities.is_empty() {
                        return Err(TypecheckerErrorKind::UnreachableMatchCase { token, typ: None, is_unreachable_none: false, prior_covering_case_tok: None });
                    } else if possibilities.len() == 1 {
                        possibilities.drain(..).next().unwrap()
                    } else {
                        Type::Union(possibilities.drain(..).collect())
                    };
                    if let Some(ident) = case_binding {
                        let ident_name = Token::get_ident_name(&ident).clone();
                        self.add_binding(ident_name.as_str(), &ident, &match_type, false);
                        binding = Some(ident_name)
                    }

                    TypedMatchKind::Wildcard
                }
                MatchCaseType::Constant(node) => {
                    let typed_node = self.visit(node)?;
                    let typ = typed_node.get_type();
                    if !possibilities.contains(&typ) {
                        return Err(TypecheckerErrorKind::UnreachableMatchCase { token: typed_node.get_token().clone(), typ: Some(typ), is_unreachable_none: false, prior_covering_case_tok: None });
                    }

                    if let Some(ident) = case_binding {
                        let ident_name = Token::get_ident_name(&ident).clone();
                        self.add_binding(ident_name.as_str(), &ident, &typ, false);
                        binding = Some(ident_name)
                    }

                    TypedMatchKind::Constant { node: typed_node }
                }
                MatchCaseType::Tuple(token, nodes) => {
                    let typed_nodes = nodes.into_iter()
                        .map(|n| self.visit(n))
                        .collect::<Result<Vec<_>, _>>()?;
                    let tuple_type = Type::Tuple(typed_nodes.iter().map(|n| n.get_type()).collect());
                    if !possibilities.contains(&tuple_type) {
                        return Err(TypecheckerErrorKind::UnreachableMatchCase { token, typ: Some(tuple_type), is_unreachable_none: false, prior_covering_case_tok: None });
                    }

                    if let Some(ident) = case_binding {
                        let ident_name = Token::get_ident_name(&ident).clone();
                        self.add_binding(ident_name.as_str(), &ident, &tuple_type, false);
                        binding = Some(ident_name)
                    }

                    TypedMatchKind::Tuple { nodes: typed_nodes }
                }
            };

            self.hoist_declarations_in_scope(&block)?;
            let typed_block = self.visit_block(is_stmt, block)?;
            self.scopes.pop();

            typed_branches.push((branch_cond, binding, typed_block));
        }
        if !possibilities.is_empty() {
            return Err(TypecheckerErrorKind::NonExhaustiveMatch { token: match_token.clone() });
        }

        // Temporarily use Type::Unit as a placeholder, if it's an expression it'll be updated later
        Ok(TypedMatchNode { typ: Type::Unit, target: Box::new(target), branches: typed_branches })
    }

    fn visit_fn_args(
        &mut self,
        args: Vec<(Token, Option<TypeIdentifier>, bool, Option<AstNode>)>,
        allow_self_param: bool,
        allow_unknown_arg_types: bool,
        allow_varargs: bool,
    ) -> Result<Vec<(Token, Type, bool, Option<TypedAstNode>)>, TypecheckerErrorKind> {
        let num_args = args.len();
        let mut typed_args = Vec::with_capacity(num_args);
        let mut arg_idents = HashMap::<String, Token>::new();
        let mut seen_optional_arg = false;
        for (idx, (token, type_ident, is_vararg, default_value)) in args.into_iter().enumerate() {
            let arg_name = Token::get_ident_name(&token).clone();

            if let Token::Self_(_) = &token {
                if !allow_self_param {
                    return Err(TypecheckerErrorKind::InvalidSelfParam { token: token.clone() });
                }
                if idx != 0 {
                    return Err(TypecheckerErrorKind::InvalidSelfParamPosition { token: token.clone() });
                }

                let arg_type = match &self.cur_typedef {
                    None => return Err(TypecheckerErrorKind::InvalidSelfParam { token: token.clone() }),
                    Some(cur_type) => cur_type.clone(),
                };

                self.add_binding(&arg_name, &token, &arg_type, false);
                typed_args.push((token.clone(), arg_type, false, None));

                continue;
            }

            if &arg_name != "_" {
                if let Some(arg_tok) = arg_idents.get(&arg_name) {
                    return Err(TypecheckerErrorKind::DuplicateBinding { orig_ident: Some(arg_tok.clone()), ident: token.clone() });
                }
            }
            if is_vararg {
                if !allow_varargs {
                    return Err(TypecheckerErrorKind::InvalidVarargUsage(token));
                }
                if idx != num_args - 1 {
                    return Err(TypecheckerErrorKind::InvalidVarargPosition(token));
                }
            }
            arg_idents.insert(arg_name, token.clone());

            let (arg_tok, arg_type, default_value) = match type_ident {
                Some(type_ident) => {
                    let arg_type = self.type_from_type_ident(&type_ident, false)?;
                    match default_value {
                        Some(default_value) => {
                            seen_optional_arg = true;
                            let mut default_value = self.visit(default_value)?;
                            if self.are_types_equivalent(&mut default_value, &arg_type)? {
                                let arg_name = Token::get_ident_name(&token);
                                self.add_binding(&arg_name, &token, &arg_type, false);
                                (token, arg_type, Some(default_value))
                            } else {
                                return Err(TypecheckerErrorKind::Mismatch { token: default_value.get_token().clone(), expected: arg_type, actual: default_value.get_type() });
                            }
                        }
                        None => {
                            if seen_optional_arg {
                                return Err(TypecheckerErrorKind::InvalidRequiredArgPosition(token));
                            }
                            let arg_name = Token::get_ident_name(&token);
                            self.add_binding(&arg_name, &token, &arg_type, false);
                            (token, arg_type, None)
                        }
                    }
                }
                None => {
                    match default_value {
                        None => {
                            if allow_unknown_arg_types {
                                (token, Type::Unknown, None)
                            } else { unreachable!() /* This should be caught during parsing */ }
                        }
                        Some(default_value) => {
                            seen_optional_arg = true;
                            let default_value = self.visit(default_value)?;
                            let arg_type = default_value.get_type();
                            let arg_name = Token::get_ident_name(&token);
                            self.add_binding(&arg_name, &token, &arg_type, false);
                            (token, arg_type, Some(default_value))
                        }
                    }
                }
            };

            let default_value = if is_vararg {
                match arg_type {
                    Type::Array(_) => {}
                    typ => return Err(TypecheckerErrorKind::VarargMismatch { token: arg_tok, typ })
                }
                // If no default was provided, default the varargs to []
                default_value.or(Some(TypedAstNode::Array(
                    Token::LBrack(arg_tok.get_position(), false),
                    TypedArrayNode { typ: arg_type.clone(), items: vec![] },
                )))
            } else { default_value };

            typed_args.push((arg_tok, arg_type, is_vararg, default_value));
        }

        Ok(typed_args)
    }

    fn visit_fn_body(&mut self, body: Vec<AstNode>) -> Result<Vec<TypedAstNode>, TypecheckerErrorKind> {
        self.hoist_declarations_in_scope(&body)?;

        let mut has_terminated = false;
        let body_len = body.len();
        body.into_iter().enumerate()
            .map(|(idx, node)| {
                if has_terminated {
                    return Err(TypecheckerErrorKind::UnreachableCode { token: node.get_token().clone() });
                }

                if idx == body_len - 1 {
                    // This is sufficiently gross to warrant a comment. This logic is similar to the
                    // if-block logic in `visit_if_node` above, but slightly different. Like in an
                    // if-/else-block, the last slot in a function body could be treated as an
                    // expression. An if-block in this slot will be parsed as an if-statement, and
                    // should be re-counted here as if it were an expression instead. HOWEVER, this
                    // DOES NOT account for functions which return Unit. A function that ends in
                    // such a re-attributed if-expression in which both or either branch is a Unit
                    // type (ie. a full Unit type or a Unit? type) should be treated as if it were
                    // an if-statement instead. This is critical for bytecode generation, as no
                    // POP instruction will be emitted following an if-statement.
                    match node {
                        AstNode::IfStatement(token, if_node) => {
                            let node = AstNode::IfExpression(token.clone(), if_node);
                            let typed_node = self.visit(node)?;

                            let typ = typed_node.get_type().get_opt_unwrapped();
                            match typ {
                                Type::Unit => {
                                    if let TypedAstNode::IfExpression(token, mut typed_if_node) = typed_node {
                                        // Explicitly set the underlying node's type to Unit, in case it had been Unit? (eg. an if-expr missing an else branch)
                                        typed_if_node.typ = Type::Unit;

                                        // Since we attempt to parse the if-stmt as an expr, it will end up with a `None` as its else-branch
                                        // if it was missing one. If the else branch is exactly 1 node and it's the `_Nil` node, treat it as if
                                        // the statement's else_block was None.
                                        // TODO: Think of a more sane way of representing this
                                        if let Some(ref mut else_block) = &mut typed_if_node.else_block {
                                            if else_block.len() == 1 {
                                                if let Some(TypedAstNode::_Nil(_)) = else_block.last() {
                                                    typed_if_node.else_block = None;
                                                }
                                            }
                                        }
                                        Ok(TypedAstNode::IfStatement(token, typed_if_node))
                                    } else { unreachable!() }
                                }
                                _ => Ok(typed_node)
                            }
                        }
                        AstNode::MatchStatement(token, match_node) => {
                            let node = AstNode::MatchExpression(token.clone(), match_node);
                            let typed_node = self.visit(node)?;

                            let typ = typed_node.get_type().get_opt_unwrapped();
                            match typ {
                                Type::Unit => {
                                    if let TypedAstNode::MatchExpression(token, mut typed_match_node) = typed_node {
                                        // Explicitly set the underlying node's type to Unit, in case it had been Unit? (eg. an if-expr missing an else branch)
                                        typed_match_node.typ = Type::Unit;
                                        Ok(TypedAstNode::MatchStatement(token, typed_match_node))
                                    } else { unreachable!() }
                                }
                                _ => Ok(typed_node)
                            }
                        }
                        n => self.visit(n)
                    }
                } else {
                    let typed_node = self.visit(node)?;
                    has_terminated = typed_node.all_branches_terminate().is_some();

                    Ok(typed_node)
                }
            })
            .collect()
    }

    fn get_func_signature(&mut self, type_args: &Vec<Token>, node: &AstNode) -> Result<(bool, Token, Type), TypecheckerErrorKind> {
        let FunctionDeclNode { name, type_args: fn_type_args, ret_type, args, .. } = match &node {
            AstNode::FunctionDecl(_, node) => node,
            _ => unreachable!()
        };

        let func_name = Token::get_ident_name(&name);
        // Check to see if there is already a fn binding; pre-hoisted fns use fn_bindings, not normal bindings
        if let Some(ScopeBinding(orig_ident, _, _)) = self.get_fn_binding_in_current_scope(&func_name) {
            if orig_ident != name {
                let is_prelude = self.module_loader.get_module(&ModuleId::prelude())
                    .exports.contains_key(&func_name);
                let orig_ident = if is_prelude { None } else { Some(orig_ident.clone()) };
                return Err(TypecheckerErrorKind::DuplicateBinding { ident: name.clone(), orig_ident });
            }
        }

        // Create temporary "scope" to capture function's type_args, and eventually...
        let type_args = type_args.iter().map(|t| (Token::get_ident_name(t), t)).collect::<HashMap<String, &Token>>();
        let mut fn_type_arg_names = Vec::new();
        let mut scope = Scope::new(ScopeKind::TypeDef);
        for fn_type_arg in fn_type_args {
            let fn_type_arg_name = Token::get_ident_name(fn_type_arg);
            if let Some(orig_ident) = type_args.get(&fn_type_arg_name) {
                return Err(TypecheckerErrorKind::DuplicateTypeArgument { ident: fn_type_arg.clone(), orig_ident: (*orig_ident).clone() });
            }
            fn_type_arg_names.push(fn_type_arg_name.clone());
            scope.types.insert(fn_type_arg_name.clone(), (Type::Generic(fn_type_arg_name), None, false));
        }
        self.scopes.push(scope);

        let ret_type = ret_type.as_ref()
            .map_or(Ok(Type::Unit), |t| self.type_from_type_ident(&t, false))?;
        let mut is_static = true;
        let mut is_variadic = false;
        let mut arg_types = Vec::new();
        for (ident, type_ident, is_vararg, default_value) in args {
            match ident {
                Token::Self_(_) => is_static = false,
                ident => {
                    is_variadic = is_variadic || *is_vararg;

                    let arg_type = match type_ident {
                        None => match default_value {
                            None => unreachable!(), // This should be caught during parsing
                            Some(default_value) => {
                                let default_value = self.visit(default_value.clone())?;
                                default_value.get_type()
                            }
                        },
                        Some(type_ident) => self.type_from_type_ident(type_ident, false)?,
                    };
                    // Insert arg as binding in throwaway scope, to handle references in other variables' default values
                    let arg_name = Token::get_ident_name(ident);
                    self.add_binding(&arg_name, &ident, &arg_type, false);
                    arg_types.push((arg_name, arg_type, *is_vararg || default_value.is_some()));
                }
            }
        }

        let fn_type = Type::Fn(FnType { arg_types, type_args: fn_type_arg_names, ret_type: Box::new(ret_type.clone()), is_variadic, is_enum_constructor: false });
        let ret = if is_static {
            // TODO: Handle static methods referencing type's type_args
            (true, name.clone(), fn_type)
        } else {
            (false, name.clone(), fn_type)
        };
        // ...pop off the throwaway scope here.
        self.scopes.pop();
        Ok(ret)
    }

    fn hoist_declarations_in_scope(&mut self, body: &Vec<AstNode>) -> Result<(), TypecheckerErrorKind> {
        for n in body {
            match n {
                AstNode::FunctionDecl(_, _) => {
                    let (_, name, func_type) = self.get_func_signature(&vec![], n)?;
                    let func_name = Token::get_ident_name(&name).clone();
                    self.add_fn_binding(&func_name, &name, &func_type);
                }
                AstNode::TypeDecl(_, TypeDeclNode { name, type_args, .. }) |
                AstNode::EnumDecl(_, EnumDeclNode { name, type_args, .. }) => {
                    let type_is_enum = if let AstNode::EnumDecl(_, _) = &n { true } else { false };
                    let new_type_name = Token::get_ident_name(&name).clone();

                    if let Some(ScopeBinding(orig_ident, _, _)) = self.get_binding_in_current_scope(&new_type_name) {
                        if orig_ident != name {
                            let orig_ident = orig_ident.clone();
                            return Err(TypecheckerErrorKind::DuplicateType { ident: name.clone(), orig_ident: Some(orig_ident) });
                        }
                    }

                    // The type embedded in TypedAstNodes of this type will be a Reference - to materialize this
                    // reference we must look it up by name in self.referencable_types. This level of indirection
                    // allows for cyclic/self-referential types.
                    let module_name = self.module_loader.get_module_name(&self.module_id);
                    let typeref_name = format!("{}/{}", module_name, &new_type_name);
                    let typeref = Type::Reference(typeref_name.clone(), vec![]);
                    let binding_type = Type::Type(typeref_name.clone(), Box::new(typeref.clone()), false);
                    self.add_binding(&new_type_name, &name, &binding_type, false);
                    self.add_type(new_type_name.clone(), None, typeref.clone(), false);

                    let type_arg_names = type_args.iter()
                        .map(|name| {
                            let name = Token::get_ident_name(name);
                            (name.clone(), Type::Generic(name))
                        })
                        .collect::<Vec<(String, Type)>>();
                    let referencable_type = if type_is_enum {
                        let typedef = EnumType { name: new_type_name.clone(), type_args: type_arg_names, variants: vec![], static_fields: vec![], methods: vec![] };
                        Type::Enum(typedef)
                    } else {
                        let typedef = StructType { name: new_type_name.clone(), type_args: type_arg_names.clone(), constructable: true, fields: vec![], static_fields: vec![], methods: vec![] };
                        Type::Struct(typedef)
                    };
                    self.referencable_types.insert(typeref_name, referencable_type);
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn typecheck_typedef_methods_phase_1(
        &mut self,
        type_args: &Vec<Token>,
        methods: &Vec<AstNode>,
    ) -> Result<(/* static_fields: */ Vec<(String, Type, bool)>, /* typed_methods: */ Vec<(String, Type)>), TypecheckerErrorKind> {
        let methods = methods.into_iter()
            .map(|node| self.get_func_signature(type_args, node))
            .collect::<Result<Vec<_>, _>>()?;
        let mut static_fields = Vec::new();
        let mut typed_methods = Vec::new();
        for (is_static, name, typ) in methods {
            let fn_name = Token::get_ident_name(&name).clone();
            if is_static {
                static_fields.push((fn_name, typ, true))
            } else {
                if let Type::Fn(FnType { ret_type, .. }) = &typ {
                    // TODO: Proper protocol/interface implementation
                    if fn_name == "toString".to_string() {
                        if **ret_type != Type::String {
                            let expected = Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false });
                            return Err(TypecheckerErrorKind::InvalidProtocolMethod { token: name, fn_name, expected, actual: typ });
                        }
                    }
                } else { unreachable!() }

                typed_methods.push((fn_name, typ))
            }
        }
        // TODO: Proper protocol/interface implementation; interface methods should come first in methods list
        if typed_methods.iter().find(|(name, _)| name == "toString").is_none() {
            typed_methods.insert(0, (
                "toString".to_string(),
                Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String), is_variadic: false, is_enum_constructor: false })
            ));
        }

        Ok((static_fields, typed_methods))
    }

    fn typecheck_typedef_methods_phase_2(
        &mut self,
        is_enum: bool,
        methods: Vec<AstNode>,
        field_names: HashMap<String, Token>,
    ) -> Result<(/* static_fields: */ Vec<(Token, Type, Option<TypedAstNode>)>, /* typed_methods: */ Vec<(String, TypedAstNode)>), TypecheckerErrorKind> {
        let mut method_names = HashMap::<String, Token>::new();
        let mut static_fields = Vec::new();
        let mut typed_methods = Vec::new();
        for func_decl_node in methods {
            let name_tok = match &func_decl_node {
                AstNode::FunctionDecl(_, FunctionDeclNode { name, .. }) => name.clone(),
                _ => unreachable!()
            };
            let name = Token::get_ident_name(&name_tok).clone();

            if let Some(orig_ident) = method_names.get(&name) {
                return Err(TypecheckerErrorKind::DuplicateField { orig_ident: orig_ident.clone(), ident: name_tok.clone(), orig_is_field: false, orig_is_enum_variant: false });
            } else if let Some(orig_ident) = field_names.get(&name) {
                return Err(TypecheckerErrorKind::DuplicateField { orig_ident: orig_ident.clone(), ident: name_tok.clone(), orig_is_field: !is_enum, orig_is_enum_variant: is_enum });
            }

            method_names.insert(name.clone(), name_tok.clone());

            let typed_func_decl = self.visit(func_decl_node)?;
            match &typed_func_decl {
                TypedAstNode::FunctionDecl(tok, TypedFunctionDeclNode { name, args, .. }) => {
                    let name = Token::get_ident_name(&name);
                    let is_static = args.iter()
                        .find(|(ident, _, _, _)| if let Token::Self_(_) = ident { true } else { false })
                        .is_none();
                    if is_static {
                        let cur_typedef = match &self.cur_typedef {
                            Some(Type::Reference(name, _)) => self.referencable_types.get(name),
                            Some(t) => Some(t),
                            None => None
                        };
                        let cur_typedef_static_fields = match cur_typedef {
                            Some(Type::Struct(StructType { static_fields, .. })) |
                            Some(Type::Enum(EnumType { static_fields, .. })) => static_fields,
                            _ => unreachable!("cur_typedef should be present")
                        };
                        let fn_type = cur_typedef_static_fields.iter()
                            .find(|(field_name, _, _)| &name == field_name)
                            .map(|(_, typ, _)| typ.clone())
                            .unwrap();
                        static_fields.push((tok.clone(), fn_type, Some(typed_func_decl)));
                    } else {
                        typed_methods.push((name, typed_func_decl));
                    }
                }
                _ => unreachable!()
            };
        }

        Ok((static_fields, typed_methods))
    }

    fn visit_binding_pattern(&mut self, binding: &mut BindingPattern, typ: &Type, is_mutable: bool) -> Result<Vec<(String, Type)>, TypecheckerErrorKind> {
        match binding {
            BindingPattern::Variable(ident) => {
                let name = Token::get_ident_name(ident);

                if let Some(ScopeBinding(orig_ident, _, _)) = self.get_binding_in_current_scope(&name) {
                    let is_prelude = self.module_loader.get_module(&ModuleId::prelude())
                        .exports.contains_key(&name);
                    let orig_ident = if is_prelude { None } else { Some(orig_ident.clone()) };
                    return Err(TypecheckerErrorKind::DuplicateBinding { ident: ident.clone(), orig_ident });
                }
                self.add_binding(&name, &ident, &typ, is_mutable);
                Ok(vec![(name, typ.clone())])
            }
            BindingPattern::Tuple(_, idents) => {
                match typ.get_opt_unwrapped() {
                    Type::Tuple(opts) if idents.len() == opts.len() => {
                        let mut bindings = Vec::new();
                        for (pat, tuple_elem_typ) in idents.iter_mut().zip(opts) {
                            let typ = if typ.is_opt() { Type::Option(Box::new(tuple_elem_typ)) } else { tuple_elem_typ };
                            let res = self.visit_binding_pattern(pat, &typ, is_mutable)?;
                            bindings.extend(res);
                        }
                        Ok(bindings)
                    }
                    typ => {
                        return Err(TypecheckerErrorKind::InvalidAssignmentDestructuring { binding: binding.clone(), typ: typ.clone() });
                    }
                }
            }
            BindingPattern::Array(_, idents, is_string) => {
                let splats = idents.iter().filter(|(_, is_splat)| *is_splat).collect::<Vec<_>>();
                if splats.len() > 1 {
                    if let Some((BindingPattern::Variable(ident), _)) = splats.last() {
                        return Err(TypecheckerErrorKind::DuplicateSplatDestructuring { token: ident.clone() });
                    } else { unreachable!() }
                }

                let mut bindings = Vec::new();
                match typ.get_opt_unwrapped() {
                    Type::Array(inner_typ) => {
                        for (pat, is_splat) in idents {
                            let typ = if *is_splat {
                                debug_assert!(if let BindingPattern::Variable(_) = &pat { true } else { false });
                                Type::Array(Box::new(*inner_typ.clone()))
                            } else {
                                Type::Option(Box::new(*inner_typ.clone()))
                            };
                            let res = self.visit_binding_pattern(pat, &typ, is_mutable)?;
                            bindings.extend(res);
                        }
                    }
                    Type::String => {
                        *is_string = true;
                        for (pat, _) in idents {
                            let res = self.visit_binding_pattern(pat, &typ, is_mutable)?;
                            bindings.extend(res);
                        }
                    }
                    typ => {
                        return Err(TypecheckerErrorKind::InvalidAssignmentDestructuring { binding: binding.clone(), typ: typ.clone() });
                    }
                };
                Ok(bindings)
            }
        }
    }
}

impl<'a, R: ModuleReader> AstVisitor<TypedAstNode, TypecheckerErrorKind> for Typechecker<'a, R> {
    fn visit_literal(&mut self, token: Token, node: AstLiteralNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        match node {
            AstLiteralNode::IntLiteral(val) =>
                Ok(TypedAstNode::Literal(token, TypedLiteralNode::IntLiteral(val))),
            AstLiteralNode::FloatLiteral(val) =>
                Ok(TypedAstNode::Literal(token, TypedLiteralNode::FloatLiteral(val))),
            AstLiteralNode::StringLiteral(val) =>
                Ok(TypedAstNode::Literal(token, TypedLiteralNode::StringLiteral(val))),
            AstLiteralNode::BoolLiteral(val) =>
                Ok(TypedAstNode::Literal(token, TypedLiteralNode::BoolLiteral(val)))
        }
    }

    fn visit_unary(&mut self, token: Token, node: UnaryNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let expr = *node.expr;
        let typed_expr = self.visit(expr)?;
        let expr_type = typed_expr.get_type();
        match (&node.op, &expr_type) {
            (UnaryOp::Minus, Type::Int) | (UnaryOp::Minus, Type::Float) |
            (UnaryOp::Negate, Type::Bool) | (UnaryOp::Negate, Type::Option(_)) => {
                let node = TypedUnaryNode { typ: expr_type, op: node.op, expr: Box::new(typed_expr) };
                Ok(TypedAstNode::Unary(token, node))
            }
            (op @ UnaryOp::Minus, _) | (op @ UnaryOp::Negate, _) => {
                let expected = if op == &UnaryOp::Minus {
                    Type::Union(vec![Type::Int, Type::Float])
                } else {
                    Type::Union(vec![Type::Bool, Type::Option(Box::new(Type::Any))])
                };
                Err(TypecheckerErrorKind::Mismatch { token, expected, actual: expr_type })
            }
        }
    }

    fn visit_binary(&mut self, token: Token, node: BinaryNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        #[inline]
        fn type_for_op<R: ModuleReader>(
            zelf: &mut Typechecker<R>,
            token: &Token,
            op: &BinaryOp,
            typed_left: &TypedAstNode,
            typed_right: &mut TypedAstNode,
        ) -> Result<Type, TypecheckerErrorKind> {
            let ltype = typed_left.get_type();
            let rtype = typed_right.get_type();

            match op {
                BinaryOp::Add | BinaryOp::AddEq =>
                    match (&ltype, &rtype) {
                        (Type::String, t) | (t, Type::String) if t != &Type::Unit => Ok(Type::String),
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerErrorKind::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Sub | BinaryOp::SubEq | BinaryOp::Mul | BinaryOp::MulEq =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerErrorKind::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Pow =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) | (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerErrorKind::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Div | BinaryOp::DivEq =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) | (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerErrorKind::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Mod | BinaryOp::ModEq =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerErrorKind::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::And | BinaryOp::Or | BinaryOp::Xor =>
                    match (&ltype, &rtype) {
                        (Type::Option(_), Type::Bool) |
                        (Type::Bool, Type::Option(_)) |
                        (Type::Option(_), Type::Option(_)) |
                        (Type::Bool, Type::Bool) => Ok(Type::Bool),
                        (_, _) => Err(TypecheckerErrorKind::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::AndEq | BinaryOp::OrEq =>
                    match (&ltype, &rtype) {
                        (Type::Bool, Type::Bool) => Ok(Type::Bool),
                        (_, _) => Err(TypecheckerErrorKind::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte =>
                    match (&ltype, &rtype) {
                        (Type::String, Type::String) => Ok(Type::Bool),
                        (Type::Int, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) | (Type::Float, Type::Int) => Ok(Type::Bool),
                        (_, _) => Err(TypecheckerErrorKind::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Neq | BinaryOp::Eq => Ok(Type::Bool),
                BinaryOp::Coalesce | BinaryOp::CoalesceEq => {
                    match (&ltype, &rtype) {
                        (Type::Option(ltype), rtype) => {
                            if !zelf.are_types_equivalent(typed_right, ltype)? {
                                let token = typed_right.get_token().clone();
                                Err(TypecheckerErrorKind::Mismatch { token, expected: (**ltype).clone(), actual: rtype.clone() })
                            } else {
                                Ok(rtype.clone())
                            }
                        }
                        (_, _) => Err(TypecheckerErrorKind::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                }
            }
        }

        let pos = &token.get_position();
        let (is_assignment, op, token) = match node.op {
            BinaryOp::AddEq => (true, BinaryOp::Add, Token::Plus(pos.clone())),
            BinaryOp::SubEq => (true, BinaryOp::Sub, Token::Minus(pos.clone())),
            BinaryOp::MulEq => (true, BinaryOp::Mul, Token::Star(pos.clone())),
            BinaryOp::DivEq => (true, BinaryOp::Div, Token::Slash(pos.clone())),
            BinaryOp::ModEq => (true, BinaryOp::Mod, Token::Percent(pos.clone())),
            BinaryOp::AndEq => (true, BinaryOp::And, Token::And(pos.clone())),
            BinaryOp::OrEq => (true, BinaryOp::Or, Token::Or(pos.clone())),
            BinaryOp::CoalesceEq => (true, BinaryOp::Coalesce, Token::Elvis(pos.clone())),
            op => (false, op, token)
        };
        if is_assignment {
            let assignment_node = AstNode::Assignment(
                Token::Assign(pos.clone()),
                AssignmentNode {
                    target: node.left.clone(),
                    expr: Box::new(AstNode::Binary(
                        token,
                        BinaryNode { left: node.left, op, right: node.right },
                    )),
                },
            );
            return self.visit(assignment_node);
        }

        let left = *node.left;
        let typed_left = self.visit(left)?;

        let right = *node.right;
        let mut typed_right = self.visit(right)?;

        let typ = type_for_op(self, &token, &op, &typed_left, &mut typed_right)?;

        let typed_ast_node = match &op {
            op @ BinaryOp::And |
            op @ BinaryOp::Or => {
                #[inline]
                fn bool_lit(pos: &Position, b: bool) -> TypedAstNode {
                    TypedAstNode::Literal(Token::Bool(pos.clone(), b), TypedLiteralNode::BoolLiteral(b))
                }

                let (if_block, else_block) = match op {
                    BinaryOp::And => (typed_right, bool_lit(pos, false)),
                    BinaryOp::Or => (bool_lit(pos, true), typed_right),
                    _ => unreachable!()
                };
                TypedAstNode::IfExpression(Token::If(pos.clone()), TypedIfNode {
                    typ,
                    condition: Box::new(typed_left),
                    condition_binding: None,
                    if_block: vec![if_block],
                    else_block: Some(vec![else_block]),
                })
            }
            _ => {
                let left = Box::new(typed_left);
                let right = Box::new(typed_right);
                TypedAstNode::Binary(token.clone(), TypedBinaryNode { typ, left, op, right })
            }
        };
        Ok(typed_ast_node)
    }

    fn visit_grouped(&mut self, token: Token, node: GroupedNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let GroupedNode { expr } = node;
        let typed_expr = self.visit(*expr)?;
        let typ = typed_expr.get_type();
        let expr = Box::new(typed_expr);
        Ok(TypedAstNode::Grouped(token, TypedGroupedNode { typ, expr }))
    }

    fn visit_array(&mut self, token: Token, node: ArrayNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let items: Result<Vec<TypedAstNode>, TypecheckerErrorKind> = node.items.into_iter()
            .map(|n| self.visit(n))
            .collect();
        let items = items?;

        let item_types: HashSet<Type> = HashSet::from_iter(
            items.iter().map(|node| node.get_type())
        );
        let typ = if item_types.len() == 1 {
            item_types.into_iter()
                .nth(0)
                .expect("We know the size is 1")
        } else if !item_types.is_empty() {
            Type::flatten(item_types.into_iter().collect())
        } else {
            Type::Unknown
        };

        Ok(TypedAstNode::Array(token.clone(), TypedArrayNode { typ: Type::Array(Box::new(typ)), items }))
    }

    fn visit_map_literal(&mut self, token: Token, node: MapNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let MapNode { items } = node;

        let mut typed_fields = Vec::new();
        let mut field_key_types = Vec::new();
        let mut field_value_types = Vec::new();
        let mut seen_fields = Vec::<AstNode>::new();
        for (field_key, field_value) in items {
            let key_token = field_key.get_token();
            let prev_key = seen_fields.iter()
                .find(|f| match (f, &field_key) {
                    (AstNode::Literal(_, l), AstNode::Literal(_, r)) => l == r,
                    _ => false
                });
            if let Some(orig_key) = prev_key {
                return Err(TypecheckerErrorKind::DuplicateMapKey { key: key_token.clone(), orig_key: orig_key.get_token().clone() });
            } else {
                seen_fields.push(field_key.clone());
            }

            let field_key = self.visit(field_key)?;
            field_key_types.push(field_key.get_type());

            let field_value = self.visit(field_value)?;
            field_value_types.push(field_value.get_type());

            typed_fields.push((field_key, field_value));
        }

        let key_type = {
            let mut key_types = field_key_types.into_iter().unique().collect::<Vec<_>>();
            if key_types.is_empty() {
                Type::Unknown
            } else if key_types.len() == 1 {
                key_types.drain(..).next().unwrap()
            } else {
                Type::Union(key_types.drain(..).collect())
            }
        };
        let val_type = {
            let mut value_types = field_value_types.into_iter().unique().collect::<Vec<_>>();
            if value_types.is_empty() {
                Type::Unknown
            } else if value_types.len() == 1 {
                value_types.drain(..).next().unwrap()
            } else {
                Type::Union(value_types.drain(..).collect())
            }
        };

        let typ = Type::Map(Box::new(key_type), Box::new(val_type));
        Ok(TypedAstNode::Map(token, TypedMapNode { typ, items: typed_fields }))
    }

    fn visit_set_literal(&mut self, token: Token, node: SetNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let items: Result<Vec<TypedAstNode>, TypecheckerErrorKind> = node.items.into_iter()
            .map(|n| self.visit(n))
            .collect();
        let items = items?;

        let item_types: HashSet<Type> = HashSet::from_iter(
            items.iter().map(|node| node.get_type())
        );
        let typ = if item_types.len() == 1 {
            item_types.into_iter()
                .nth(0)
                .expect("We know the size is 1")
        } else if !item_types.is_empty() {
            Type::Union(item_types.into_iter().collect())
        } else {
            Type::Unknown
        };

        Ok(TypedAstNode::Set(token.clone(), TypedSetNode { typ: Type::Set(Box::new(typ)), items }))
    }

    fn visit_binding_decl(&mut self, token: Token, node: BindingDeclNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let BindingDeclNode { export_token, is_mutable, mut binding, type_ann, expr } = node;
        if !is_mutable && expr.is_none() {
            if let BindingPattern::Variable(ident) = binding {
                return Err(TypecheckerErrorKind::MissingRequiredAssignment { ident });
            } else { unreachable!("Destructured binding declarations without an `=` will not parse") }
        }

        let ann_type = match type_ann {
            Some(type_ann) => Some(self.type_from_type_ident(&type_ann, false)?),
            None => None
        };

        let mut typed_expr = match expr {
            Some(e) => Some(self.visit(*e)?),
            None => None
        };

        let typ = match (&mut typed_expr, ann_type) {
            (Some(e), None) => {
                let typ = e.get_type();
                if typ.has_unbound_generic() {
                    let available_generics = self.get_generics_in_scope();
                    let generic = typ.extract_unbound_generics().iter()
                        .filter(|generic_name| !available_generics.contains(*generic_name))
                        .map(|g| g.clone())
                        .next();
                    if let Some(unbound_generic) = generic {
                        return Err(TypecheckerErrorKind::UnboundGeneric(binding.get_token().clone(), unbound_generic));
                    }
                }
                typ
            }
            (None, Some(ann_type)) => ann_type,
            (Some(typed_expr), Some(ref ann_type)) => {
                if self.are_types_equivalent(typed_expr, ann_type)? {
                    ann_type.clone()
                } else {
                    return Err(TypecheckerErrorKind::Mismatch {
                        token: typed_expr.get_token().clone(),
                        expected: ann_type.clone(),
                        actual: typed_expr.get_type(),
                    });
                }
            }
            (None, None) => {
                return Err(TypecheckerErrorKind::UnannotatedUninitialized { ident: binding.get_token().clone(), is_mutable });
            }
        };
        if typ.is_unknown(&|type_name| self.resolve_type(type_name)) {
            return Err(TypecheckerErrorKind::ForbiddenVariableType { binding, typ: Type::Unknown });
        } else if typ.is_unit(&|type_name| self.resolve_type(type_name)) {
            return Err(TypecheckerErrorKind::ForbiddenVariableType { binding, typ: Type::Unit });
        }

        let binding_idents = self.visit_binding_pattern(&mut binding, &typ, is_mutable)?;
        if let Some(token) = export_token {
            if self.scopes.len() != 1 {
                return Err(TypecheckerErrorKind::InvalidExportDepth { token });
            }
            for (name, typ) in binding_idents {
                self.exports.insert(name, ExportedValue::Binding(typ));
            }
        }

        let scope_depth = self.scopes.len() - 1;
        let node = TypedBindingDeclNode {
            is_mutable,
            binding,
            expr: typed_expr.map(Box::new),
            scope_depth,
        };
        Ok(TypedAstNode::BindingDecl(token, node))
    }

    fn visit_func_decl(&mut self, token: Token, node: FunctionDeclNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let FunctionDeclNode { export_token, name, type_args, args, ret_type: ret_ann_type, body, .. } = node;

        let func_name = Token::get_ident_name(&name);
        let is_exported = if let Some(ScopeBinding(orig_ident, _, _)) = self.get_binding_in_current_scope(&func_name) {
            let is_prelude = self.module_loader.get_module(&ModuleId::prelude())
                .exports.contains_key(&func_name);
            let orig_ident = if is_prelude { None } else { Some(orig_ident.clone()) };
            return Err(TypecheckerErrorKind::DuplicateBinding { ident: name, orig_ident });
        } else if let Some(token) = export_token {
            if self.scopes.len() != 1 {
                return Err(TypecheckerErrorKind::InvalidExportDepth { token });
            }
            true
        } else { false };

        let mut scope = Scope::new(ScopeKind::Function(FnScopeKind {
            token: name.clone(),
            name: func_name.clone(),
            is_recursive: false,
            // Use Unknown temporarily; return type might be dependent on arguments if there are generics that need to be resolved
            return_type: Type::Unknown,
        }));
        let mut seen = HashMap::<String, Token>::new();
        for type_arg in &type_args {
            let name = Token::get_ident_name(type_arg);
            match seen.get(&name) {
                Some(orig_ident) => {
                    return Err(TypecheckerErrorKind::DuplicateTypeArgument { ident: type_arg.clone(), orig_ident: orig_ident.clone() });
                }
                None => {
                    seen.insert(name.clone(), type_arg.clone());
                    scope.types.insert(name.clone(), (Type::Generic(name.clone()), None, false));
                }
            }
        }
        self.scopes.push(scope);

        let args = self.visit_fn_args(args, true, false, true)?;

        // Store a stub version of the function type, based on what we know so far. Recursive references
        // to the function within its body will be typed according to whatever is saved now. If we cannot
        // determine the return type (due to a missing return type annotation, since we haven't yet
        // typechecked the body, and thus cannot infer it), store it as Unknown. When identifiers are
        // typechecked later in (say, for example, within the function body), a return type of Unknown
        // will signal a recursive reference to a function which is missing a return type. Note: this is
        // a fairly brittle abstraction, and should probably be readdressed.
        // Note also that we need to add this reference to the previous scope, so once we determine the
        // initial return type...
        let mut is_variadic = false;
        let arg_types = args.iter()
            .filter_map(|(ident, typ, is_vararg, default_value)| {
                is_variadic = is_variadic || *is_vararg;

                match ident {
                    Token::Self_(_) => None,
                    ident => Some((Token::get_ident_name(ident).clone(), typ.clone(), default_value.is_some()))
                }
            })
            .collect::<Vec<_>>();
        let ret_type = ret_ann_type.as_ref()
            .map_or(Ok(Type::Unit), |t| self.type_from_type_ident(&t, false))?;
        if ret_type.has_unbound_generic() {
            // The valid type args for a return type are all of the generics available in scope, minus
            // the fn type args that aren't referenced in the arguments (and are thus unbound).
            let arg_type_args = args.iter()
                .flat_map(|(_, t, _, _)| t.extract_unbound_generics().into_iter())
                .collect::<HashSet<_>>();
            let unresolvable_type_arg_names = type_args.iter()
                .map(Token::get_ident_name)
                .filter(|type_arg_name| !arg_type_args.contains(type_arg_name))
                .collect::<HashSet<_>>();
            let generics_in_scope = self.get_generics_in_scope();
            let valid_return_type_args = generics_in_scope.difference(&unresolvable_type_arg_names).collect::<HashSet<_>>();

            let ret_generics = ret_type.extract_unbound_generics();
            for name in ret_generics {
                if !valid_return_type_args.contains(&&name) {
                    let ret_type = ret_ann_type.expect("Should be Some if ret_type is anything but Unknown");
                    let ident_token = ret_type.get_ident();
                    let ret_type_name = Token::get_ident_name(&ident_token);
                    return Err(TypecheckerErrorKind::UnboundGeneric(ident_token, ret_type_name));
                }
            }
        }

        // ...we pop off the scope, add the function there, and then push the scope back on. We also add the
        // return type to the fn's scope (this is used when visiting returns later on).
        let type_args = type_args.iter().map(|t| Token::get_ident_name(t)).collect();
        let fn_type = FnType { arg_types, type_args, ret_type: Box::new(ret_type.clone()), is_variadic, is_enum_constructor: false };
        let mut scope = self.scopes.pop().unwrap();
        self.add_binding(&func_name, &name, &Type::Fn(fn_type.clone()), false);
        if let ScopeKind::Function(fn_scope_kind) = &mut scope.kind {
            fn_scope_kind.return_type = ret_type.clone();
        } else { unreachable!(); }
        self.scopes.push(scope);

        let mut body = self.visit_fn_body(body)?;

        match &ret_type {
            Type::Unit => body.push(TypedAstNode::_Nil(Token::None(Position::new(0, 0)))),
            typ => {
                if let Some(mut node) = body.last_mut() {
                    if node.all_branches_terminate().is_none() {
                        let node_type = node.get_type();

                        if !self.are_types_equivalent(&mut node, &typ)? {
                            let token = body.last().map_or(name.clone(), |node| node.get_token().clone());
                            return Err(TypecheckerErrorKind::ReturnTypeMismatch { token, fn_name: func_name.clone(), bare_return: false, expected: typ.clone(), actual: node_type });
                        }
                    }
                }
            }
        }

        let fn_scope = self.scopes.pop().unwrap();
        let is_recursive = if let ScopeKind::Function(FnScopeKind { is_recursive, .. }) = fn_scope.kind {
            is_recursive
        } else { unreachable!("A function's scope should always be of ScopeKind::Function") };
        let scope_depth = self.scopes.len() - 1;

        if is_exported {
            let (ScopeBinding(_, typ, _), _) = self.get_binding(&func_name).unwrap();
            let export = ExportedValue::Binding(typ.clone());
            self.exports.insert(func_name.clone(), export);
        }

        Ok(TypedAstNode::FunctionDecl(token, TypedFunctionDeclNode { name, args, ret_type, body, scope_depth, is_recursive, fn_type }))
    }

    fn visit_type_decl(&mut self, token: Token, node: TypeDeclNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        if self.scopes.len() != 1 {
            return Err(TypecheckerErrorKind::InvalidTypeDeclDepth { token });
        }

        let TypeDeclNode { export_token, name, type_args, fields, methods, .. } = node;
        let new_type_name = Token::get_ident_name(&name).clone();
        let is_exported = if let Some(token) = export_token {
            if self.scopes.len() != 1 {
                return Err(TypecheckerErrorKind::InvalidExportDepth { token });
            }
            true
        } else { false };

        // // ------------------------ Begin First-pass Type Gathering ------------------------ \\
        // // --- First gather only the type data for each field, method, and static value. --- \\
        // // --- This first pass guarantees that all type data is available for the second --- \\
        // // --- pass. Without doing this, forward-referencing of fields is not possible.  --- \\
        // // --------------------------------------------------------------------------------- \\
        //
        // Note: At this point in time, the type declaration has already been inserted into the
        // referencable_types list, since it was hoisted to the top of the scope. As we progress
        // through this type declaration, we build up this referencable_type value. This value will
        // be used for values of type Type::Reference, when the referenced type is "materialized".

        // Update the Reference type of the type's identifier to include the generics, and establish
        // that Reference as the cur_typedef
        let type_arg_names = type_args.iter().map(|name| Type::Generic(Token::get_ident_name(name))).collect();
        let module_name = self.module_loader.get_module_name(&self.module_id);
        let typeref_name = format!("{}/{}", module_name, &new_type_name);
        let typeref = Type::Reference(typeref_name.clone(), type_arg_names);
        let ScopeBinding(_, binding_type, _) = self.get_binding_mut(&new_type_name).unwrap();
        *binding_type = Type::Type(typeref_name.clone(), Box::new(typeref.clone()), false);
        self.cur_typedef = Some(typeref.clone());

        // Insert Generics for all type_args present
        let mut scope = Scope::new(ScopeKind::TypeDef);
        let mut seen = HashMap::<String, Token>::new();
        for type_arg in &type_args {
            let name = Token::get_ident_name(type_arg);
            match seen.get(&name) {
                Some(orig_ident) => {
                    return Err(TypecheckerErrorKind::DuplicateTypeArgument { ident: type_arg.clone(), orig_ident: orig_ident.clone() });
                }
                None => {
                    seen.insert(name.clone(), type_arg.clone());
                    scope.types.insert(name.clone(), (Type::Generic(name.clone()), None, false));
                }
            }
        }
        self.scopes.push(scope);

        let mut field_names = HashMap::<String, Token>::new();
        let fields = fields.into_iter()
            .map(|TypeDeclField { ident, type_ident, default_value, readonly }| {
                let field_type = self.type_from_type_ident(&type_ident, false)?;
                let field_name_str = Token::get_ident_name(&ident);
                if let Some(orig_ident) = field_names.get(&field_name_str) {
                    return Err(TypecheckerErrorKind::DuplicateField { orig_ident: orig_ident.clone(), ident, orig_is_field: true, orig_is_enum_variant: false });
                } else {
                    field_names.insert(field_name_str.clone(), ident.clone());
                }

                let is_settable = readonly.is_some();
                Ok((ident, field_type, default_value, is_settable))
            })
            .collect::<Result<Vec<_>, _>>();
        let fields = fields?;

        let typedef = if let Some(Type::Struct(typedef)) = self.referencable_types.get_mut(&typeref_name) { typedef } else { unreachable!() };
        typedef.fields = fields.iter()
            .map(|(name, typ, default_value_node, settable)| {
                StructTypeField {
                    name: Token::get_ident_name(name),
                    typ: typ.clone(),
                    has_default_value: default_value_node.is_some(),
                    readonly: *settable,
                }
            })
            .collect();

        let (static_methods, typed_methods) = self.typecheck_typedef_methods_phase_1(&type_args, &methods)?;
        let typedef = if let Some(Type::Struct(typedef)) = self.referencable_types.get_mut(&typeref_name) { typedef } else { unreachable!() };
        typedef.static_fields = static_methods;
        typedef.methods = typed_methods;

        // ------------------------  End First-pass Type Gathering  ------------------------ \\
        // --- Now that the current type has been made available to the environment, we  --- \\
        // --- can make references to this type's own fields/methods/static methods it.  --- \\
        // --------------------------------------------------------------------------------- \\
        // ------------------------ Begin Field/Method Typechecking ------------------------ \\

        let typed_fields = fields.into_iter().map(|(tok, field_type, default_value_node, _)| {
            let default_value = if let Some(default_value) = default_value_node {
                let mut default_value = self.visit(default_value)?;
                let default_value_type = default_value.get_type();
                if !self.are_types_equivalent(&mut default_value, &field_type)? {
                    return Err(TypecheckerErrorKind::Mismatch { token: default_value.get_token().clone(), actual: default_value_type, expected: field_type });
                } else {
                    Some(default_value)
                }
            } else { None };
            let field = TypedTypeDeclField {
                ident: tok,
                typ: field_type,
                default_value,
            };
            Ok(field)
        }).collect::<Result<Vec<_>, _>>();
        let typed_fields = typed_fields?;

        // Record TypeDeclNode for type, registering the freshly-typed fields. Later on, we'll mutate
        // this TypeDeclNode and add static fields and methods, but this allows for methods to reference
        // fields of instances of this current type within their bodies.
        let (_, node) = self.get_type_mut(&new_type_name).unwrap();
        let type_decl_node = TypedAstNode::TypeDecl(token, TypedTypeDeclNode { name, self_type: typeref, fields: typed_fields, static_fields: vec![], methods: vec![] });
        *node = Some(type_decl_node.clone());

        let (static_fields, typed_methods) = self.typecheck_typedef_methods_phase_2(false, methods, field_names)?;
        if let (_, Some(TypedAstNode::TypeDecl(_, type_decl_node))) = self.get_type_mut(&new_type_name).unwrap() {
            type_decl_node.static_fields = static_fields;
            type_decl_node.methods = typed_methods;
        } else { unreachable!("We should have just defined this node up above"); }

        self.scopes.pop();
        self.cur_typedef = None;

        if is_exported {
            let node = if let (_, Some(node)) = self.get_type_mut(&new_type_name).unwrap() { node.clone() } else { unreachable!() };
            let export = ExportedValue::Type {
                reference: self.get_type(&new_type_name).map(|(typ, _)| typ),
                backing_type: self.referencable_types[&typeref_name].clone(),
                node: Some(node),
            };
            self.exports.insert(new_type_name.clone(), export);
        }

        // Return type_decl_node for type
        Ok(self.get_type(&new_type_name).unwrap().1.unwrap())
    }

    fn visit_enum_decl(&mut self, token: Token, node: EnumDeclNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        if self.scopes.len() != 1 {
            return Err(TypecheckerErrorKind::InvalidTypeDeclDepth { token });
        }

        let EnumDeclNode { export_token, name, variants, methods, type_args } = node;
        let new_enum_name = Token::get_ident_name(&name);
        let is_exported = if let Some(token) = export_token {
            if self.scopes.len() != 1 {
                return Err(TypecheckerErrorKind::InvalidExportDepth { token });
            }
            true
        } else { false };

        // // ------------------------ Begin First-pass Type Gathering ------------------------ \\
        // // ---    See comment in visit_type_decl above, the process here is similar      --- \\
        // // --------------------------------------------------------------------------------- \\
        //
        // Note: much like type declarations, enum declarations are also hoisted to the top of their
        // scope. As such, by this point there will already be an entry in self.referencable_types.

        // Update the Reference type of the type's identifier to include the generics, and establish
        // that Reference as the cur_typedef
        let type_arg_names = type_args.iter().map(|name| Type::Generic(Token::get_ident_name(name))).collect();
        let module_name = self.module_loader.get_module_name(&self.module_id);
        let typeref_name = format!("{}/{}", module_name, &new_enum_name);
        let typeref = Type::Reference(typeref_name.clone(), type_arg_names);
        let ScopeBinding(_, binding_type, _) = self.get_binding_mut(&new_enum_name).unwrap();
        *binding_type = Type::Type(typeref_name.clone(), Box::new(typeref.clone()), true);
        self.cur_typedef = Some(typeref);

        let mut scope = Scope::new(ScopeKind::TypeDef);
        let mut seen = HashMap::<String, Token>::new();
        for type_arg in &type_args {
            let name = Token::get_ident_name(type_arg);
            match seen.get(&name) {
                Some(orig_ident) => {
                    return Err(TypecheckerErrorKind::DuplicateTypeArgument { ident: type_arg.clone(), orig_ident: orig_ident.clone() });
                }
                None => {
                    seen.insert(name.clone(), type_arg.clone());
                    scope.types.insert(name.clone(), (Type::Generic(name.clone()), None, false));
                }
            }
        }
        self.scopes.push(scope);

        let mut variant_names = HashMap::<String, Token>::new();
        let mut typed_variants = Vec::new();
        for (name, args) in variants.iter() {
            let variant_name = Token::get_ident_name(name).clone();
            if let Some(orig_ident) = variant_names.get(&variant_name) {
                return Err(TypecheckerErrorKind::DuplicateField { orig_ident: orig_ident.clone(), ident: name.clone(), orig_is_field: false, orig_is_enum_variant: true });
            } else {
                variant_names.insert(variant_name.clone(), name.clone());
            }
            let variant_type = match args {
                None => (variant_name, self.cur_typedef.as_ref().unwrap().clone()),
                Some(args) => {
                    let faked_args = args.clone().into_iter()
                        .map(|(tok, ident, is_vararg, _)| (tok, ident, is_vararg, None))
                        .collect();
                    let faked_args = self.visit_fn_args(faked_args, false, false, false)?;
                    let constructor_type = Type::Fn(FnType {
                        arg_types: faked_args.into_iter().zip(args.iter())
                            .map(|((arg_name, arg_type, _, _), (_, _, _, default_value_node))| {
                                let arg_name = Token::get_ident_name(&arg_name).clone();
                                // Since we forced the default_value_node to be None in `faked_args`, we need to
                                // obtain the `is_some` status from the original arg. This is gross
                                (arg_name, arg_type.clone(), default_value_node.is_some())
                            }).collect(),
                        type_args: type_args.iter().map(|a| Token::get_ident_name(a)).collect(),
                        ret_type: Box::new(self.cur_typedef.as_ref().unwrap().clone()),
                        is_variadic: false,
                        is_enum_constructor: true,
                    });
                    (variant_name, constructor_type)
                }
            };
            typed_variants.push(variant_type);
        }

        let typedef = if let Some(Type::Enum(typedef)) = self.referencable_types.get_mut(&typeref_name) { typedef } else { unreachable!() };
        typedef.variants = typed_variants.clone();

        let (static_fields, typed_methods) = self.typecheck_typedef_methods_phase_1(&type_args, &methods)?;
        let typedef = if let Some(Type::Enum(typedef)) = self.referencable_types.get_mut(&typeref_name) { typedef } else { unreachable!() };
        typedef.static_fields = static_fields;
        typedef.methods = typed_methods;

        // ------------------------  End First-pass Type Gathering  ------------------------ \\
        // --- Now that the current type has been made available to the environment, we  --- \\
        // --- can make references to this type's own fields/methods/static methods it.  --- \\
        // --------------------------------------------------------------------------------- \\
        // ----------------------- Begin Variant/Method Typechecking ----------------------- \\
        let mut variant_nodes = Vec::new();
        for ((name, args), _variant_type) in variants.into_iter().zip(typed_variants) {
            let variant_type = match args {
                None => (self.cur_typedef.as_ref().unwrap().clone(), None),
                Some(args) => {
                    let args = self.visit_fn_args(args, false, false, false)?;
                    let fn_type = Type::Fn(FnType {
                        arg_types: args.iter()
                            .map(|(tok, typ, has_default, _)| {
                                let arg_name = Token::get_ident_name(&tok);
                                (arg_name, typ.clone(), *has_default)
                            })
                            .collect(),
                        type_args: type_args.iter().map(|a| Token::get_ident_name(a)).collect(),
                        ret_type: Box::new(self.cur_typedef.as_ref().unwrap().clone()),
                        is_variadic: false,
                        is_enum_constructor: true,
                    });
                    let default_arg_nodes = args.into_iter().map(|(_, _, _, default_value)| default_value).collect();
                    (fn_type, Some(default_arg_nodes))
                }
            };
            variant_nodes.push((name, variant_type));
        }

        // Record EnumDecl for enum, registering the freshly-typed variants. Later on, we'll mutate
        // this EnumDecl node and add static fields and methods, but this allows for methods to reference
        // variants of this current enum within their bodies.
        let (self_type, node) = self.get_type_mut(&new_enum_name).unwrap();
        let enum_decl_node = TypedAstNode::EnumDecl(token, TypedEnumDeclNode { name, self_type: self_type.clone(), variants: variant_nodes, static_fields: vec![], methods: vec![] });
        *node = Some(enum_decl_node);

        let (static_fields, typed_methods) = self.typecheck_typedef_methods_phase_2(true, methods, variant_names)?;
        if let (_, Some(TypedAstNode::EnumDecl(_, enum_decl_node))) = self.get_type_mut(&new_enum_name).unwrap() {
            enum_decl_node.static_fields = static_fields;
            enum_decl_node.methods = typed_methods;
        } else { unreachable!("We should have just defined this node up above"); }

        self.scopes.pop();
        self.cur_typedef = None;

        if is_exported {
            let node = if let (_, Some(node)) = self.get_type_mut(&new_enum_name).unwrap() { node.clone() } else { unreachable!() };
            let export = ExportedValue::Type {
                reference: self.get_type(&new_enum_name).map(|(typ, _)| typ),
                backing_type: self.referencable_types[&typeref_name].clone(),
                node: Some(node),
            };
            self.exports.insert(new_enum_name.clone(), export);
        }

        // Return enum_decl_node for enum
        Ok(self.get_type(&new_enum_name).unwrap().1.unwrap())
    }

    fn visit_ident(&mut self, token: Token, type_args: Option<Vec<TypeIdentifier>>) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let name = Token::get_ident_name(&token);

        let node = match self.get_binding(&name) {
            None => return Err(TypecheckerErrorKind::UnknownIdentifier { ident: token }),
            Some((ScopeBinding(_, typ, is_mutable), scope_depth)) => {
                let binding_typ = typ.clone();
                let is_mutable = is_mutable.clone();

                if let Type::Fn(_) = typ {
                    for scope in self.scopes.iter_mut().rev() {
                        if let ScopeKind::Function(FnScopeKind { name: func_name, ref mut is_recursive, .. }) = &mut scope.kind {
                            if &name == func_name {
                                *is_recursive = true;
                            }
                        }
                    }
                }
                TypedIdentifierNode { typ: binding_typ, name: name.clone(), is_mutable, scope_depth }
            }
        };

        #[inline]
        fn get_type_generics<R: ModuleReader>(zelf: &mut Typechecker<R>, typ: &Type) -> Vec<String> {
            match zelf.resolve_ref_type(typ) {
                Type::Fn(FnType { type_args, .. }) => type_args,
                Type::Struct(StructType { type_args, .. }) => type_args.iter().map(|a| a.0.clone()).collect(),
                Type::Type(_, typ, _) => get_type_generics(zelf, &*typ),
                _ => vec![]
            }
        }
        let typ_generics = get_type_generics(self, &node.typ);
        let num_provided = type_args.as_ref().map_or(0, Vec::len);
        if typ_generics.len() != 0 && num_provided != 0 && typ_generics.len() != num_provided {
            return Err(TypecheckerErrorKind::InvalidTypeArgumentArity {
                token,
                actual_type: node.typ,
                expected: typ_generics.len(),
                actual: num_provided,
            });
        }

        let mut node = node;
        if let Some(type_args) = type_args {
            let mut available_generics = HashMap::new();
            for (type_ident, type_arg_name) in type_args.iter().zip(typ_generics) {
                let typ = self.type_from_type_ident(&type_ident, false)?;
                available_generics.insert(type_arg_name, typ);
            }
            node.typ = Type::substitute_generics(&node.typ, &available_generics);
        }

        Ok(TypedAstNode::Identifier(token, node))
    }

    fn visit_assignment(&mut self, token: Token, node: AssignmentNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let AssignmentNode { target, expr } = node;
        match *target {
            AstNode::Identifier(ident_tok, _) => {
                let ident = self.visit_ident(ident_tok.clone(), None)?;
                let (typ, is_mutable) = match &ident {
                    TypedAstNode::Identifier(_, TypedIdentifierNode { typ, is_mutable, .. }) => (typ, is_mutable),
                    _ => unreachable!()
                };
                if !is_mutable {
                    let name = Token::get_ident_name(&ident_tok);
                    let orig_ident = match self.get_binding(&name) {
                        Some((ScopeBinding(orig_ident, _, _), _)) => orig_ident.clone(),
                        None => unreachable!()
                    };
                    return Err(TypecheckerErrorKind::AssignmentToImmutable { token, orig_ident });
                }

                let mut typed_expr = self.visit(*expr)?;
                if !self.are_types_equivalent(&mut typed_expr, typ)? {
                    Err(TypecheckerErrorKind::Mismatch {
                        token: typed_expr.get_token().clone(),
                        expected: typ.clone(),
                        actual: typed_expr.get_type(),
                    })
                } else {
                    let node = TypedAssignmentNode {
                        kind: AssignmentTargetKind::Identifier,
                        typ: typ.clone(),
                        target: Box::new(ident),
                        expr: Box::new(typed_expr),
                    };
                    Ok(TypedAstNode::Assignment(token, node))
                }
            }
            AstNode::Indexing(tok, node) => {
                if let IndexingMode::Range(_, _) = &node.index {
                    return Err(TypecheckerErrorKind::InvalidAssignmentTarget { token, typ: None, reason: InvalidAssignmentTargetReason::IndexingMode });
                }

                let mut typed_expr = self.visit(*expr)?;
                let expr_type = typed_expr.get_type();

                let typed_target = self.visit_indexing(tok.clone(), node)?;
                let (index_target_type, kind) = match &typed_target {
                    TypedAstNode::Indexing(_, TypedIndexingNode { target, index, .. }) => {
                        match target.get_type() {
                            Type::Array(inner_type) => (*inner_type, AssignmentTargetKind::ArrayIndex),
                            Type::Tuple(types) => {
                                let idx = match index {
                                    IndexingMode::Index(i) => match **i {
                                        TypedAstNode::Literal(_, TypedLiteralNode::IntLiteral(i)) => i,
                                        _ => unreachable!("The error should already be handled in visit_indexing")
                                    }
                                    _ => unreachable!("The error should already be handled in visit_indexing")
                                };
                                (types[idx as usize].clone(), AssignmentTargetKind::ArrayIndex)
                            }
                            Type::Map(_, value_type) => (*value_type, AssignmentTargetKind::MapIndex),
                            Type::String => {
                                return Err(TypecheckerErrorKind::InvalidAssignmentTarget { token, typ: Some(Type::String), reason: InvalidAssignmentTargetReason::StringTarget });
                            }
                            typ @ Type::Option(_) => {
                                return Err(TypecheckerErrorKind::InvalidAssignmentTarget { token, typ: Some(typ), reason: InvalidAssignmentTargetReason::OptionalTarget });
                            }
                            _ => unreachable!()
                        }
                    }
                    _ => unreachable!()
                };

                if !self.are_types_equivalent(&mut typed_expr, &index_target_type)? {
                    let token = typed_expr.get_token().clone();
                    Err(TypecheckerErrorKind::Mismatch { token, expected: index_target_type, actual: expr_type })
                } else {
                    let node = TypedAssignmentNode {
                        kind,
                        typ: expr_type,
                        target: Box::new(typed_target),
                        expr: Box::new(typed_expr),
                    };
                    Ok(TypedAstNode::Assignment(token, node))
                }
            }
            AstNode::Accessor(tok, node) => {
                let typed_target = self.visit_accessor(tok.clone(), node)?;
                if let TypedAstNode::Accessor(_, TypedAccessorNode { is_method, is_readonly, field_ident, .. }) = &typed_target {
                    if *is_method {
                        return Err(TypecheckerErrorKind::InvalidAssignmentTarget { token, typ: None, reason: InvalidAssignmentTargetReason::MethodTarget });
                    } else if *is_readonly {
                        return Err(TypecheckerErrorKind::InvalidAccess { token: field_ident.clone(), is_field: !*is_method, is_get: false });
                    }
                } else { unreachable!() }
                let mut typed_expr = self.visit(*expr)?;

                let expr_type = typed_expr.get_type();
                let target_type = typed_target.get_type();
                if !self.are_types_equivalent(&mut typed_expr, &target_type)? {
                    let token = typed_expr.get_token().clone();
                    Err(TypecheckerErrorKind::Mismatch { token, expected: target_type, actual: expr_type })
                } else {
                    let node = TypedAssignmentNode {
                        kind: AssignmentTargetKind::Field,
                        typ: expr_type,
                        target: Box::new(typed_target),
                        expr: Box::new(typed_expr),
                    };
                    Ok(TypedAstNode::Assignment(token, node))
                }
            }
            _ => Err(TypecheckerErrorKind::InvalidAssignmentTarget { token, typ: None, reason: InvalidAssignmentTargetReason::IllegalTarget })
        }
    }

    fn visit_indexing(&mut self, token: Token, node: IndexingNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let IndexingNode { target, index } = node;

        let target = self.visit(*target)?;
        let target_type = target.get_type();

        if let Type::Tuple(types) = &target_type {
            return if let IndexingMode::Index(idx) = index {
                let idx = self.visit(*idx)?;
                let index = match &idx {
                    TypedAstNode::Literal(_, TypedLiteralNode::IntLiteral(idx)) => idx,
                    _ => return Err(TypecheckerErrorKind::InvalidTupleIndexingSelector { token: idx.get_token().clone(), types: types.clone(), non_constant: true, index: -1 })
                };
                if *index < 0 || *index > ((types.len() - 1) as i64) {
                    Err(TypecheckerErrorKind::InvalidTupleIndexingSelector { token: idx.get_token().clone(), types: types.clone(), non_constant: false, index: *index })
                } else {
                    Ok(TypedAstNode::Indexing(token, TypedIndexingNode {
                        typ: types[*index as usize].clone(),
                        target: Box::new(target),
                        index: IndexingMode::Index(Box::new(idx)),
                    }))
                }
            } else {
                Err(TypecheckerErrorKind::InvalidIndexingTarget { token: token.clone(), target_type, index_mode: index })
            };
        }

        let target_type = target_type.get_opt_unwrapped();
        let typ = match (target_type.clone(), &index) {
            (Type::Array(inner_type), IndexingMode::Index(_)) => Ok(Type::Option(inner_type)),
            (Type::Array(inner_type), IndexingMode::Range(_, _)) => Ok(Type::Array(inner_type)),
            (Type::String, _) => Ok(Type::String),
            (Type::Map(_, value_type), IndexingMode::Index(_)) => Ok(Type::Option(value_type)),
            (Type::Tuple(_), IndexingMode::Index(_)) => unreachable!("It should have been handled above"),
            (typ, index_mode) => Err(TypecheckerErrorKind::InvalidIndexingTarget { token: token.clone(), target_type: typ, index_mode: index_mode.clone() })
        }?;

        let index = match index {
            IndexingMode::Index(idx) => {
                let idx = self.visit(*idx)?;
                match (&target_type, idx.get_type()) {
                    (Type::Array(_), Type::Int) | (Type::String, Type::Int) => Ok(IndexingMode::Index(Box::new(idx))),
                    (Type::Map(key_type, _), ref selector_type) => {
                        if !selector_type.is_equivalent_to(&key_type, &|typ_name| self.resolve_type(typ_name)) {
                            Err(TypecheckerErrorKind::InvalidIndexingSelector {
                                token: idx.get_token().clone(),
                                target_type: target_type.clone(),
                                selector_type: selector_type.clone(),
                            })
                        } else {
                            Ok(IndexingMode::Index(Box::new(idx)))
                        }
                    }
                    (target_type, selector_type) => Err(TypecheckerErrorKind::InvalidIndexingSelector {
                        token: idx.get_token().clone(),
                        target_type: target_type.clone(),
                        selector_type,
                    })
                }
            }
            IndexingMode::Range(start, end) => {
                #[inline]
                fn visit_endpoint<R: ModuleReader>(tc: &mut Typechecker<R>, node: Option<Box<AstNode>>) -> Result<Option<Box<TypedAstNode>>, TypecheckerErrorKind> {
                    match node {
                        None => Ok(None),
                        Some(node) => {
                            let typed_node = tc.visit(*node)?;
                            let token = typed_node.get_token().clone();
                            match typed_node.get_type() {
                                Type::Int => Ok(Some(Box::new(typed_node))),
                                typ => Err(TypecheckerErrorKind::Mismatch { token, expected: Type::Int, actual: typ })
                            }
                        }
                    }
                }

                let start = visit_endpoint(self, start)?;
                let end = visit_endpoint(self, end)?;
                Ok(IndexingMode::Range(start, end))
            }
        }?;

        Ok(TypedAstNode::Indexing(token, TypedIndexingNode {
            typ,
            target: Box::new(target),
            index,
        }))
    }

    fn visit_if_statement(&mut self, token: Token, node: IfNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let node = self.visit_if_node(true, node)?;
        Ok(TypedAstNode::IfStatement(token, node))
    }

    fn visit_if_expression(&mut self, token: Token, node: IfNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let mut node = self.visit_if_node(false, node)?;

        let mut if_block_terminates = false;
        let if_block_type = match &node.if_block.last() {
            None => Err(TypecheckerErrorKind::MissingIfExprBranch { if_token: token.clone(), is_if_branch: true }),
            Some(expr) => {
                if_block_terminates = expr.all_branches_terminate().is_some();
                Ok(expr.get_type())
            }
        }?;

        let typ = match &node.else_block {
            Some(else_block) => match else_block.last() {
                None => Err(TypecheckerErrorKind::MissingIfExprBranch { if_token: token.clone(), is_if_branch: false }),
                Some(expr) => {
                    let else_block_type = expr.get_type();

                    if if_block_terminates {
                        if expr.all_branches_terminate().is_some() {
                            node.typ = Type::Unit;
                            return Ok(TypedAstNode::IfExpression(token.clone(), node));
                        } else {
                            Ok(else_block_type)
                        }
                    } else if expr.all_branches_terminate().is_some() {
                        node.typ = if_block_type;
                        return Ok(TypedAstNode::IfExpression(token.clone(), node));
                    } else {
                        let mut if_block_last = node.if_block.last_mut().expect("MissingIfExprBranch should be emitted otherwise");
                        if !self.are_types_equivalent(&mut if_block_last, &else_block_type)? {
                            Err(TypecheckerErrorKind::IfExprBranchMismatch {
                                if_token: token.clone(),
                                if_type: if_block_type,
                                else_type: else_block_type,
                            })
                        } else {
                            Ok(if_block_last.get_type())
                        }
                    }
                }
            }
            None => {
                // If missing an else block, the value of this expr should be None, so we modify the AST to add a _Nil node here.
                // This means that there is always a value produced by this expr.
                node.else_block = Some(vec![TypedAstNode::_Nil(Token::Ident(token.get_position(), "nil".to_string()))]);
                Ok(Type::Option(Box::new(if_block_type)))
            }
        }?;

        node.typ = typ.clone();

        Ok(TypedAstNode::IfExpression(token.clone(), node))
    }

    fn visit_match_statement(&mut self, token: Token, node: MatchNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let node = self.visit_match_node(true, &token, node)?;
        Ok(TypedAstNode::MatchStatement(token, node))
    }

    fn visit_match_expression(&mut self, token: Token, node: MatchNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let mut node = self.visit_match_node(false, &token, node)?;

        let mut typ = None;
        for (_, _, ref mut block) in &mut node.branches {
            if block.last().as_ref().map_or(false, |n| n.all_branches_terminate().is_some()) {
                continue;
            }

            if let Some(typ) = &typ {
                let mut block_last = block.last_mut().expect("A case should have a non-empty body");
                let block_typ = block_last.get_type();
                if !self.are_types_equivalent(&mut block_last, &typ)? {
                    return Err(TypecheckerErrorKind::MatchBranchMismatch {
                        token: block_last.get_token().clone(),
                        expected: typ.clone(),
                        actual: block_typ,
                    });
                }
            } else {
                let block_typ = block.last().map(|n| n.get_type()).expect("A case should have a non-empty body");
                typ = Some(block_typ)
            }
        }

        node.typ = typ.unwrap_or(Type::Unit);
        Ok(TypedAstNode::MatchExpression(token, node))
    }

    fn visit_invocation(&mut self, token: Token, node: InvocationNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let InvocationNode { target, args } = node;
        let target = self.visit(*target)?;
        let target_type = target.get_type();

        #[inline]
        fn verify_named_args_invocation<R: ModuleReader>(
            zelf: &mut Typechecker<R>,
            invocation_target: Token,
            args: Vec<(/* arg_name: */ Option<Token>, /* arg_node: */ AstNode)>,
            expected_arg_types: &Vec<(/* arg_name: */ String, /* arg_type: */ Type, /* is_optional: */ bool)>,
            generics: &mut HashMap<String, Type>,
        ) -> Result<Vec<(String, Option<TypedAstNode>)>, TypecheckerErrorKind> {
            // Check for duplicate named parameters
            let mut seen = HashSet::new();
            for (arg_name_tok, _) in args.iter() {
                match arg_name_tok {
                    None => continue,
                    Some(arg_name_tok) => {
                        let arg_name = Token::get_ident_name(arg_name_tok);
                        if seen.contains(&arg_name) {
                            return Err(TypecheckerErrorKind::DuplicateParamName { token: arg_name_tok.clone() });
                        }
                        seen.insert(arg_name);
                    }
                }
            }

            let args = args.into_iter()
                .map(|(arg_name_tok, node)| {
                    let arg_name_tok = arg_name_tok.unwrap();
                    let arg_name = Token::get_ident_name(&arg_name_tok);
                    (arg_name.clone(), (arg_name_tok, node))
                })
                .collect::<HashMap<String, (Token, AstNode)>>();
            let expected_args = expected_arg_types.iter()
                .map(|(arg_name, arg_type, is_optional)| (arg_name, (arg_type, is_optional)))
                .collect::<HashMap<&String, (&Type, &bool)>>();

            // Check for unexpected args
            for (arg_name, (arg_name_token, _)) in args.iter() {
                if !expected_args.contains_key(arg_name) {
                    // Note: this overlaps slightly with the IncorrectArity error; if named args are provided,
                    // this error will be raised for greater clarity, as opposed to IncorrectArity.
                    return Err(TypecheckerErrorKind::UnexpectedParamName { token: arg_name_token.clone() });
                }
            }

            let mut typed_args = Vec::new();

            // Ensure all expected args passed
            let mut missing_params = Vec::new();
            let mut args = args;
            for (arg_name, expected_arg_type, is_optional) in expected_arg_types {
                match args.remove(arg_name) {
                    None => {
                        if !is_optional {
                            missing_params.push(arg_name.clone());
                        } else {
                            typed_args.push((arg_name.clone(), None));
                        }
                    }
                    Some((_, arg)) => {
                        if !missing_params.is_empty() { continue; }

                        let typed_arg = typecheck_arg(zelf, arg, &expected_arg_type, generics)?;
                        typed_args.push((arg_name.clone(), Some(typed_arg)));
                    }
                }
            }

            if !missing_params.is_empty() {
                Err(TypecheckerErrorKind::MissingRequiredParams { token: invocation_target, missing_params })
            } else {
                Ok(typed_args)
            }
        }

        #[inline]
        fn typecheck_arg<R: ModuleReader>(zelf: &mut Typechecker<R>, arg: AstNode, expected_arg_type: &Type, generics: &mut HashMap<String, Type>) -> Result<TypedAstNode, TypecheckerErrorKind> {
            let mut typed_arg = zelf.visit(arg)?;
            let arg_type = typed_arg.get_type();

            let mut generics_need_refit = false;
            let expected_arg_type = if expected_arg_type.has_unbound_generic() {
                let potential_types = Type::try_fit_generics(&arg_type, &expected_arg_type);
                if let Some(pairs) = potential_types {
                    for (name, value) in pairs {
                        if !generics.contains_key(&name) {
                            if value == Type::Unknown {
                                generics_need_refit = true;
                            } else if !generics.contains_key(&name) {
                                generics.insert(name, value);
                            }
                        }
                    }
                }
                Type::substitute_generics(expected_arg_type, &generics)
            } else {
                expected_arg_type.clone()
            };

            if !zelf.are_types_equivalent(&mut typed_arg, &expected_arg_type)? {
                return Err(TypecheckerErrorKind::Mismatch { token: typed_arg.get_token().clone(), expected: expected_arg_type.clone(), actual: arg_type });
            }
            if generics_need_refit { // If it was not possible to determine generic values beforehand (eg. due to lambda return types), try the fit again after are_types_equivalent
                let arg_type = typed_arg.get_type();
                let potential_types = Type::try_fit_generics(&arg_type, &expected_arg_type);
                if let Some(pairs) = potential_types {
                    for (name, value) in pairs {
                        if !generics.contains_key(&name) {
                            generics.insert(name, value);
                        }
                    }
                }
            }
            Ok(typed_arg)
        }

        let mut target_type = self.resolve_ref_type(&target_type);
        let mut is_opt = false;
        match (&target_type, &target) {
            (Type::Option(t), TypedAstNode::Accessor(_, TypedAccessorNode { is_opt_safe, .. })) if *is_opt_safe => {
                if let Type::Fn(_) = &**t {
                    is_opt = true;
                    target_type = target_type.get_opt_unwrapped();
                }
            }
            _ => {}
        };
        match target_type {
            Type::Fn(FnType { arg_types, type_args, ret_type, is_variadic, .. }) => {
                let num_named = args.iter().filter(|(arg, _)| arg.is_some()).count();
                if num_named != 0 && num_named != args.len() {
                    return Err(TypecheckerErrorKind::InvalidMixedParamType { token: target.get_token().clone() });
                }

                let mut generics = HashMap::<String, Type>::with_capacity(type_args.len());

                let typed_args = if num_named == 0 {
                    let num_req_args = arg_types.iter()
                        .take_while(|(_, _, is_optional)| !*is_optional)
                        .count();
                    if args.len() < num_req_args || (!is_variadic && args.len() > arg_types.len()) {
                        return Err(TypecheckerErrorKind::IncorrectArity { token: target.get_token().clone(), expected: num_req_args, actual: args.len() });
                    }

                    let mut typed_args = Vec::new();
                    let mut args_iter = args.into_iter();
                    let num_arg_types = arg_types.len();
                    let mut arg_types_iter = arg_types.into_iter().enumerate();
                    while let Some((idx, (_, expected_arg_type, _))) = arg_types_iter.next() {
                        if is_variadic && idx == num_arg_types - 1 {
                            let remaining_args = args_iter.map(|a| a.1).collect_vec();
                            if remaining_args.is_empty() {
                                typed_args.push(None); // Push None so default value will be used
                                break;
                            }

                            let arg = AstNode::Array(
                                Token::LBrack(remaining_args[0].get_token().get_position(), false),
                                ArrayNode { items: remaining_args },
                            );
                            let typed_arg = typecheck_arg(self, arg, &expected_arg_type, &mut generics)?;
                            typed_args.push(Some(typed_arg));
                            break;
                        } else if let Some((_, arg)) = args_iter.next() {
                            let typed_arg = typecheck_arg(self, arg, &expected_arg_type, &mut generics)?;
                            typed_args.push(Some(typed_arg));
                        } else {
                            // Make sure to fill in any omitted optional positional arguments
                            typed_args.push(None);
                        }
                    }
                    debug_assert!(typed_args.len() == num_arg_types);
                    typed_args
                } else { // num_named should equal args.len()
                    let target_token = target.get_token().clone();
                    verify_named_args_invocation(self, target_token, args, &arg_types, &mut generics)?.into_iter()
                        .map(|(_name, node)| node)
                        .collect()
                };

                let ret_type = if ret_type.has_unbound_generic() {
                    Type::substitute_generics(&ret_type, &generics)
                } else {
                    *ret_type
                };
                let ret_type = if is_opt { Type::Option(Box::new(ret_type)) } else { ret_type };

                Ok(TypedAstNode::Invocation(token, TypedInvocationNode { typ: ret_type, target: Box::new(target), args: typed_args }))
            }
            Type::Type(typeref_name, t, _) => match self.resolve_ref_type(&*t) {
                Type::Struct(struct_type) => {
                    let StructType { name, fields: expected_fields, type_args, constructable, .. } = &struct_type;
                    let target_token = target.get_token().clone();

                    if !constructable {
                        let typ = Type::Struct(struct_type);
                        return Err(TypecheckerErrorKind::InvalidInstantiation { token: target_token, typ });
                    }

                    let num_named = args.iter().filter(|(arg, _)| arg.is_some()).count();
                    if args.len() != num_named {
                        return Err(TypecheckerErrorKind::InvalidTypeFuncInvocation { token: target_token });
                    }

                    let mut generics = HashMap::new();
                    for (type_arg_name, type_arg_type) in type_args {
                        if type_arg_type.has_unbound_generic() { continue; }
                        generics.insert(type_arg_name.clone(), type_arg_type.clone());
                    }
                    let expected_arg_types = expected_fields.into_iter()
                        .map(|f| (f.name.clone(), f.typ.clone(), f.has_default_value))
                        .collect();
                    let typed_args = verify_named_args_invocation(self, target_token, args, &expected_arg_types, &mut generics)?;

                    let default_field_values = match self.get_type(&name).expect(&format!("Type {} should exist", name)) {
                        (_, Some(TypedAstNode::TypeDecl(_, TypedTypeDeclNode { fields, .. }))) => {
                            fields.iter()
                                .map(|TypedTypeDeclField { ident, default_value, .. }| {
                                    let name = Token::get_ident_name(ident).clone();
                                    (name, default_value)
                                })
                                .filter_map(|(name, default_value)| default_value.as_ref().map(|v| (name, v.clone())))
                                .collect::<HashMap<String, TypedAstNode>>()
                        }
                        (typ, None) => {
                            // If there is no typedecl node for a type, then it's a native type; use placeholder _Nil ast nodes for its default
                            // values, since they will be determined in native code.
                            if let Type::Struct(st) = self.resolve_ref_type(&typ) {
                                st.fields.into_iter()
                                    .filter_map(|StructTypeField { name, has_default_value, .. }| {
                                        if has_default_value {
                                            Some((name, TypedAstNode::_Nil(Token::None(Position::new(0, 0)))))
                                        } else { None }
                                    })
                                    .collect::<HashMap<String, TypedAstNode>>()
                            } else { HashMap::new() }
                        }
                        _ => HashMap::new()
                    };

                    let fields = typed_args.into_iter().map(|(name, node)| {
                        match node {
                            Some(node) => (name, node),
                            None => {
                                let default_value = default_field_values.get(&name).unwrap().clone();
                                (name, default_value)
                            }
                        }
                    }).collect();

                    let typ = {
                        let mut pairs = Vec::new();
                        for (type_arg_name, unbound_generic) in type_args {
                            if let Some(resolved_type_arg) = generics.get(type_arg_name) {
                                pairs.push(resolved_type_arg.clone());
                            } else {
                                pairs.push(unbound_generic.clone())
                            }
                        }
                        Type::Reference(typeref_name, pairs)
                    };

                    Ok(TypedAstNode::Instantiation(token, TypedInstantiationNode { typ, target: Box::new(target), fields }))
                }
                _ => Err(TypecheckerErrorKind::InvalidInstantiation { token: target.get_token().clone(), typ: *t }),
            }
            target_type => Err(TypecheckerErrorKind::InvalidInvocationTarget { token: target.get_token().clone(), target_type })
        }
    }

    fn visit_for_loop(&mut self, token: Token, node: ForLoopNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let ForLoopNode { mut binding, index_ident, iterator, body } = node;
        let iterator = self.visit(*iterator)?;
        let (iteratee_type, index_type) = match iterator.get_type() {
            Type::Array(inner) |
            Type::Set(inner) => (*inner, Type::Int),
            Type::Map(key_type, val_type) => (*key_type, *val_type),
            iterator_type => {
                let token = iterator.get_token().clone();
                return Err(TypecheckerErrorKind::InvalidLoopTarget { token, target_type: iterator_type });
            }
        };
        let iterator = Box::new(iterator);

        self.scopes.push(Scope::new(ScopeKind::Block)); // Wrap loop in block where intrinsic variables $idx and $iter will be stored
        self.scopes.push(Scope::new(ScopeKind::Loop));
        self.visit_binding_pattern(&mut binding, &iteratee_type, false)?;
        if let Some(ident) = &index_ident {
            let ident_name = Token::get_ident_name(&ident).clone();
            self.add_binding(ident_name.as_str(), ident, &index_type, false);
        }

        self.hoist_declarations_in_scope(&body)?;
        let body = self.visit_block(true, body)?;
        self.scopes.pop();
        self.scopes.pop(); // Pop loop intrinsic-variables outer block

        Ok(TypedAstNode::ForLoop(token, TypedForLoopNode { binding, index_ident, iterator, body }))
    }

    fn visit_while_loop(&mut self, token: Token, node: WhileLoopNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let WhileLoopNode { condition, condition_binding, body } = node;

        let condition = self.visit(*condition)?;
        let is_valid_cond_type = match condition.get_type() {
            Type::Option(_) | Type::Bool => true,
            _ => false
        };
        if !is_valid_cond_type {
            let token = condition.get_token().clone();
            return Err(TypecheckerErrorKind::InvalidIfConditionType { token, actual: condition.get_type() });
        }
        let condition = Box::new(condition);

        let has_condition_binding = condition_binding.is_some();
        if has_condition_binding {
            self.scopes.push(Scope::new(ScopeKind::Block)); // Wrap loop in block where condition_binding variable will be stored
        }

        self.scopes.push(Scope::new(ScopeKind::Loop));
        if let Some(ident) = &condition_binding {
            let ident_name = Token::get_ident_name(ident).clone();
            let binding_type = match condition.get_type() {
                Type::Bool => Type::Bool,
                Type::Option(inner) if *inner == Type::Bool => Type::Bool,
                Type::Option(inner) => inner.get_opt_unwrapped(),
                _ => unreachable!("No other types should be allowable as conditionals")
            };
            self.add_binding(&ident_name, &ident, &binding_type, false);
        }

        self.hoist_declarations_in_scope(&body)?;
        let body = self.visit_block(true, body)?;
        self.scopes.pop();

        // Pop outer block, if created
        if has_condition_binding { self.scopes.pop(); }

        Ok(TypedAstNode::WhileLoop(token, TypedWhileLoopNode { condition, condition_binding, body }))
    }

    fn visit_break(&mut self, token: Token) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let mut iter = self.scopes.iter().rev();
        let has_loop_parent = loop {
            match iter.next().map(|s| &s.kind) {
                Some(ScopeKind::Loop) => break true,
                Some(ScopeKind::Block) => continue,
                _ => break false
            }
        };

        if has_loop_parent {
            Ok(TypedAstNode::Break(token))
        } else {
            Err(TypecheckerErrorKind::InvalidTerminatorPlacement(token))
        }
    }

    fn visit_continue(&mut self, token: Token) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let mut iter = self.scopes.iter().rev();
        let has_loop_parent = loop {
            match iter.next().map(|s| &s.kind) {
                Some(ScopeKind::Loop) => break true,
                Some(ScopeKind::Block) => continue,
                _ => break false
            }
        };

        if has_loop_parent {
            Ok(TypedAstNode::Continue(token))
        } else {
            Err(TypecheckerErrorKind::InvalidTerminatorPlacement(token))
        }
    }

    fn visit_return(&mut self, token: Token, node: Option<Box<AstNode>>) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let mut iter = self.scopes.iter_mut().rev();
        let parent_fn_scope = loop {
            match iter.next().map(|s| &s.kind) {
                Some(ScopeKind::Function(s)) => break Some((s.name.clone(), s.return_type.clone())),
                Some(ScopeKind::Lambda(_)) => {
                    // TODO: Fix this when we do #336
                    break Some(("lambda".to_string(), Type::Unknown));
                }
                Some(ScopeKind::Root) => break None,
                _ => continue
            }
        };

        if let Some((fn_name, ret_type)) = parent_fn_scope {
            let target = if let Some(target) = node {
                let mut ret_node = self.visit(*target)?;
                let ret_node_typ = ret_node.get_type();
                if !self.are_types_equivalent(&mut ret_node, &ret_type)? {
                    let token = ret_node.get_token().clone();
                    return Err(TypecheckerErrorKind::ReturnTypeMismatch { token, fn_name, bare_return: false, expected: ret_type.clone(), actual: ret_node_typ });
                }
                Some(Box::new(ret_node))
            } else if ret_type != Type::Unit {
                return Err(TypecheckerErrorKind::ReturnTypeMismatch { token, fn_name, bare_return: true, expected: ret_type.clone(), actual: Type::Unit });
            } else { None };

            Ok(TypedAstNode::ReturnStatement(token, TypedReturnNode { typ: Type::Unit, target }))
        } else {
            Err(TypecheckerErrorKind::InvalidTerminatorPlacement(token))
        }
    }

    fn visit_import(&mut self, token: Token, node: ImportNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let module_id = &node.module_id;
        let module = self.module_loader.get_module(&module_id);

        let imports = match &node.kind {
            ImportKind::ImportAll(star_token) => {
                module.exports.iter().map(|(export_name, export_value)| {
                    (export_name.clone(), star_token, export_value)
                }).collect::<Vec<_>>()
            }
            ImportKind::ImportList(imports) => {
                imports.iter().map(|import_token| {
                    let import_name = Token::get_ident_name(&import_token);
                    match module.exports.get(&import_name) {
                        None => Err(TypecheckerErrorKind::InvalidImportValue { ident: import_token.clone() }),
                        Some(export) => Ok((import_name, import_token, export))
                    }
                }).collect::<Result<Vec<_>, _>>()?
            }
            ImportKind::Alias(alias_token) => {
                let alias_name = Token::get_ident_name(alias_token);
                if let Some(ScopeBinding(orig_ident, _, _)) = self.get_binding_in_current_scope(&alias_name) {
                    let ident = (*alias_token).clone();
                    return Err(TypecheckerErrorKind::DuplicateBinding { ident, orig_ident: Some(orig_ident.clone()) });
                }

                let typ = Type::Module(module_id.clone(), alias_name.clone());
                self.add_binding(&alias_name, &alias_token, &typ, false);

                return Ok(TypedAstNode::ImportStatement(token, TypedImportNode { imports: vec![], module_id: module_id.clone(), alias_name: Some(alias_name) }));
            }
        };

        let mut typed_imports = Vec::new();
        for (import_name, import_ident_token, exported_value) in imports {
            if let Some(ScopeBinding(orig_ident, _, _)) = self.get_binding_in_current_scope(&import_name) {
                let orig_ident = if module_id.is_prelude() { None } else { Some(orig_ident.clone()) };
                return Err(TypecheckerErrorKind::DuplicateBinding { ident: import_ident_token.clone(), orig_ident });
            }

            match exported_value {
                ExportedValue::Binding(typ) => {
                    self.add_imported_binding(&import_name, import_ident_token, &typ);
                }
                ExportedValue::Type { reference, backing_type, node } => {
                    match reference {
                        Some(typeref) => {
                            let module_name = self.module_loader.get_module_name(&module_id);
                            let typeref_name = format!("{}/{}", module_name, &import_name);
                            let binding_type = Type::Type(typeref_name.clone(), Box::new(typeref.clone()), false);
                            self.add_imported_binding(&import_name, import_ident_token, &binding_type);
                            self.add_type(import_name.clone(), node.clone(), typeref.clone(), true);

                            self.referencable_types.insert(import_name.clone(), backing_type.clone());
                        }
                        None => {
                            let binding_type = Type::Type(import_name.clone(), Box::new(backing_type.clone()), false);
                            self.add_imported_binding(&import_name, import_ident_token, &binding_type);
                            self.add_type(import_name.clone(), node.clone(), backing_type.clone(), true);
                        }
                    }
                }
            }

            typed_imports.push(import_name);
        }

        Ok(TypedAstNode::ImportStatement(token, TypedImportNode { imports: typed_imports, module_id: module_id.clone(), alias_name: None }))
    }

    fn visit_accessor(&mut self, token: Token, node: AccessorNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let AccessorNode { target, field, is_opt_safe } = node;
        let target = self.visit(*target)?;

        let target_type = target.get_type();
        let (target_type, is_opt) = if is_opt_safe {
            (target_type.get_opt_unwrapped(), target_type.is_opt())
        } else {
            (target_type, false)
        };

        let (field_ident, ident_type_args) = if let AstNode::Identifier(field_ident, type_args) = *field {
            (field_ident, type_args)
        } else { unreachable!("The `field` field on AccessorNode must be an AstNode::Identifier"); };
        let field_name = Token::get_ident_name(&field_ident).clone();

        fn get_field_data<R: ModuleReader>(
            zelf: &mut Typechecker<R>,
            target_type: &Type,
            field_name: &String,
            token: &Token,
        ) -> Result<(Option<FieldSpec>, HashMap<String, Type>), TypecheckerErrorKind> {
            match zelf.resolve_ref_type(&target_type) {
                Type::Struct(StructType { fields, methods, type_args, .. }) => {
                    let generics = type_args.into_iter().collect::<HashMap<String, Type>>();

                    let field_data = fields.iter().enumerate()
                        .find_map(|(idx, StructTypeField { name, typ, readonly, .. })| {
                            if field_name == name {
                                Some(FieldSpec { idx, typ: typ.clone(), is_method: false, readonly: *readonly })
                            } else { None }
                        })
                        .or_else(|| {
                            methods.iter().enumerate().find_map(|(idx, (name, typ))| {
                                if field_name == name {
                                    Some(FieldSpec { idx, typ: typ.clone(), is_method: true, readonly: true })
                                } else { None }
                            })
                        });
                    Ok((field_data, generics))
                }
                Type::String => Ok((NativeString::get_struct_type().get_field_or_method(&field_name), HashMap::new())),
                Type::Float => Ok((NativeFloat::get_struct_type().get_field_or_method(&field_name), HashMap::new())),
                Type::Int => Ok((NativeInt::get_struct_type().get_field_or_method(&field_name), HashMap::new())),
                Type::Array(inner_type) => {
                    let generics = vec![("T".to_string(), *inner_type.clone())].into_iter().collect::<HashMap<String, Type>>();
                    let field_data = NativeArray::get_struct_type().get_field_or_method(field_name);
                    Ok((field_data, generics))
                }
                Type::Set(inner_type) => {
                    let generics = vec![("T".to_string(), *inner_type.clone())].into_iter().collect::<HashMap<String, Type>>();
                    let field_data = NativeSet::get_struct_type().get_field_or_method(field_name);
                    Ok((field_data, generics))
                }
                Type::Map(key_type, value_type) => {
                    let generics = vec![
                        ("K".to_string(), *key_type.clone()),
                        ("V".to_string(), *value_type.clone()),
                    ].into_iter().collect::<HashMap<String, Type>>();
                    let field_data = NativeMap::get_struct_type().get_field_or_method(field_name);
                    Ok((field_data, generics))
                }
                Type::Type(_, typ, _) => match zelf.resolve_ref_type(&*typ) {
                    Type::Struct(StructType { static_fields, .. }) => {
                        let field_data = static_fields.iter().enumerate()
                            .find(|(_, (name, _, _))| field_name == name)
                            .map(|(idx, (_, typ, _))| FieldSpec { idx, typ: typ.clone(), is_method: true, readonly: true }); // All static fields are methods at the moment
                        Ok((field_data, HashMap::new()))
                    }
                    Type::Enum(enum_type) => {
                        let EnumType { variants, static_fields, .. } = enum_type;
                        let field_data = variants.into_iter().enumerate()
                            .find_map(|(idx, (name, typ))| {
                                if *field_name == name {
                                    Some(FieldSpec { idx, typ, is_method: false, readonly: true })
                                } else { None }
                            })
                            .or_else(|| {
                                static_fields.iter().enumerate().find_map(|(idx, (name, typ, _))| {
                                    if field_name == name {
                                        Some(FieldSpec { idx, typ: typ.clone(), is_method: true, readonly: true }) // All static fields are methods at the moment
                                    } else { None }
                                })
                            });
                        Ok((field_data, HashMap::new()))
                    }
                    Type::Array(_) => {
                        let field_data = NativeArray::get_struct_type().get_static_field_or_method(field_name);
                        Ok((field_data, HashMap::new()))
                    }
                    Type::Map(_, _) => {
                        let field_data = NativeMap::get_struct_type().get_static_field_or_method(field_name);
                        Ok((field_data, HashMap::new()))
                    }
                    _ => unimplemented!()
                }
                Type::Enum(enum_type) => {
                    let generics = enum_type.type_args.into_iter().collect::<HashMap<String, Type>>();
                    let field_data = enum_type.methods.iter().enumerate().find_map(|(idx, (name, typ))| {
                        if field_name == name {
                            Some(FieldSpec { idx, typ: typ.clone(), is_method: true, readonly: true })
                        } else { None }
                    });
                    Ok((field_data, generics))
                }
                Type::Union(opts) => {
                    let all_enums_or_variants = opts.iter().all(|o| match zelf.resolve_ref_type(o) {
                        Type::Enum(_) => true,
                        Type::Fn(FnType { is_enum_constructor, .. }) if is_enum_constructor => true,
                        _ => false
                    });
                    if !all_enums_or_variants {
                        return Ok((None, HashMap::new()));
                    }

                    let enum_types = opts.iter().filter_map(|o| {
                        match zelf.resolve_ref_type(o) {
                            t @ Type::Enum(_) => Some(t),
                            Type::Fn(FnType { is_enum_constructor, ret_type, .. }) if is_enum_constructor => Some(*ret_type),
                            _ => None
                        }
                    }).collect::<Vec<_>>();
                    let enum_types = enum_types.into_iter().collect::<HashSet<_>>();
                    if enum_types.len() != 1 {
                        Ok((None, HashMap::new()))
                    } else {
                        let enum_type = enum_types.into_iter().next().unwrap();
                        get_field_data(zelf, &enum_type, field_name, token)
                    }
                }
                Type::Module(module_id, _) => {
                    let module = zelf.module_loader.get_module(&module_id);
                    let field_data = module.exports.get(field_name).map(|export| {
                        match export {
                            ExportedValue::Binding(typ) => FieldSpec { idx: 0, typ: typ.clone(), is_method: false, readonly: true },
                            ExportedValue::Type { reference, backing_type, node } => {
                                let typ = match reference {
                                    Some(typeref) => {
                                        let module_name = zelf.module_loader.get_module_name(&zelf.module_id);
                                        let typeref_name = format!("{}/{}", module_name, &field_name);
                                        let mut backing_type = backing_type.clone();
                                        match &mut backing_type {
                                            Type::Struct(st) => st.name = typeref_name.clone(),
                                            Type::Enum(et) => et.name = typeref_name.clone(),
                                            _ => unreachable!()
                                        }
                                        let binding_type = Type::Type(typeref_name.clone(), Box::new(backing_type.clone()), false);

                                        let scope = zelf.scopes.first_mut().unwrap();
                                        if !scope.types.contains_key(&typeref_name) {
                                            scope.types.insert(typeref_name.clone(), (typeref.clone(), node.clone(), true));
                                        }

                                        zelf.referencable_types.insert(typeref_name.clone(), backing_type.clone());
                                        binding_type
                                    }
                                    None => {
                                        let binding_type = Type::Type(field_name.clone(), Box::new(backing_type.clone()), false);
                                        let scope = zelf.scopes.first_mut().unwrap();
                                        if !scope.types.contains_key(field_name) {
                                            scope.types.insert(field_name.clone(), (backing_type.clone(), node.clone(), true));
                                        }
                                        binding_type
                                    }
                                };

                                FieldSpec { idx: 0, typ, is_method: false, readonly: true }
                            }
                        }
                    });
                    Ok((field_data, HashMap::new()))
                }
                _ => Ok((None, HashMap::new()))
            }
        }

        let (field_data, mut generics) = get_field_data(self, &target_type, &field_name, &token)?;
        let (field_idx, mut typ, is_method, is_readonly) = match field_data {
            Some(FieldSpec { idx, typ, is_method, readonly }) => {
                if let Some(ident_type_args) = &ident_type_args {
                    if let Type::Fn(FnType { type_args: fn_type_args, .. }) = &typ {
                        if ident_type_args.len() != fn_type_args.len() {
                            return Err(TypecheckerErrorKind::InvalidTypeArgumentArity {
                                token: token.clone(),
                                actual_type: typ.clone(),
                                expected: fn_type_args.len(),
                                actual: ident_type_args.len(),
                            });
                        }
                        for (type_arg_name, type_arg_ident) in fn_type_args.iter().zip(ident_type_args) {
                            let type_arg_type = self.type_from_type_ident(type_arg_ident, false)?;
                            generics.insert(type_arg_name.clone(), type_arg_type);
                        }
                    }
                }
                (idx, Type::substitute_generics(&typ, &generics), is_method, readonly)
            }
            None => {
                let module_name = match self.resolve_ref_type(&target_type) {
                    Type::Module(_, module_name) => Some(module_name),
                    _ => None
                };

                return Err(TypecheckerErrorKind::UnknownMember { token: field_ident, target_type: target_type.clone(), module_name });
            }
        };
        if is_opt {
            typ = Type::Option(Box::new(typ))
        }

        let (token, is_opt_safe) = if is_opt_safe && !is_opt {
            if let Token::QuestionDot(pos) = token {
                (Token::Dot(pos), false)
            } else { unreachable!() }
        } else { (token, is_opt_safe) };

        Ok(TypedAstNode::Accessor(token, TypedAccessorNode { typ, target: Box::new(target), field_ident, field_idx, is_opt_safe, is_method, is_readonly }))
    }

    fn visit_try(&mut self, token: Token, node: TryNode) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let mut iter = self.scopes.iter_mut().rev();
        let parent_fn_ctx = loop {
            match iter.next().map(|s| &s.kind) {
                Some(ScopeKind::Function(s)) => break Some((s.token.clone(), s.return_type.clone())),
                Some(ScopeKind::Lambda(_)) => unimplemented!("See #336"),
                Some(ScopeKind::Root) => break None,
                _ => continue
            }
        };
        let parent_fn_return_type = match parent_fn_ctx {
            None => return Err(TypecheckerErrorKind::InvalidTryPlacement { try_token: token, fn_ctx: None }),
            Some((fn_token, return_type)) => if return_type.unwrapped_tryable_type().is_none() {
                return Err(TypecheckerErrorKind::InvalidTryPlacement { try_token: token, fn_ctx: Some((fn_token, return_type)) });
            } else {
                return_type
            }
        };

        let typed_expr = self.visit(*node.expr)?;
        let expr_type = typed_expr.get_type();
        let unwrapped_type = match expr_type.unwrapped_tryable_type() {
            None => return Err(TypecheckerErrorKind::InvalidTryType { try_token: token, typ: expr_type }),
            Some(unwrapped_type) => {
                // TODO: This is very one-off; clean this up once there's a proper `Try` "interface"
                match (&expr_type, &parent_fn_return_type) {
                    (Type::Reference(name1, args1), Type::Reference(name2, args2)) if name1 == name2 && name1 == "Result" => {
                        debug_assert!(args1.len() == 2 && args2.len() == 2);
                        match (args1.get(1), args2.get(1)) {
                            (Some(t1), Some(t2)) => {
                                if !t1.is_equivalent_to(t2, &|typ_name| self.resolve_type(typ_name)) {
                                    return Err(TypecheckerErrorKind::TryMismatch { try_token: token, try_type: expr_type, return_type: parent_fn_return_type });
                                }
                            }
                            _ => unreachable!("Result<V, E> should always have 2 type arguments")
                        }
                    }
                    _ => unimplemented!("Only the Result<V, E> type is tryable")
                }

                unwrapped_type
            }
        };

        let res = TypedAstNode::MatchExpression(
            token.clone(),
            TypedMatchNode {
                typ: unwrapped_type.clone(),
                target: Box::new(typed_expr),
                branches: vec![
                    (
                        TypedMatchKind::EnumVariant {
                            enum_name: "Result".to_string(),
                            variant_idx: 0,
                            variant_name: "Ok".to_string(),
                            args: Some(vec![
                                ("value".to_string(), TypedMatchCaseArgument::Pattern(BindingPattern::Variable(Token::Ident(token.get_position(), "v".to_string()))))
                            ]),
                        },
                        None,
                        vec![
                            TypedAstNode::Identifier(
                                token.clone(),
                                TypedIdentifierNode { typ: unwrapped_type.clone(), name: "v".to_string(), is_mutable: false, scope_depth: 1 },
                            )
                        ]
                    ),
                    (
                        TypedMatchKind::EnumVariant { enum_name: "Result".to_string(), variant_idx: 1, variant_name: "Err".to_string(), args: None },
                        Some("e".to_string()),
                        vec![
                            TypedAstNode::ReturnStatement(
                                token.clone(),
                                TypedReturnNode {
                                    typ: Type::Unit,
                                    target: Some(Box::new(
                                        TypedAstNode::Identifier(
                                            token.clone(),
                                            TypedIdentifierNode { typ: expr_type.clone(), name: "e".to_string(), is_mutable: false, scope_depth: 1 },
                                        )
                                    )),
                                },
                            )
                        ]
                    ),
                ],
            },
        );
        Ok(res)
    }

    fn visit_lambda(
        &mut self,
        token: Token,
        node: LambdaNode,
        retyping_override: Option<( /* retyped_args */ Vec<(Token, Type, Option<TypedAstNode>)>, /* lambda_idx: */ usize)>,
    ) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let orig_node = node.clone();
        let LambdaNode { args, body } = node;

        let orig_scopes = self.scopes.clone();
        self.scopes.push(Scope::new(ScopeKind::Lambda(token.clone())));


        let lambda_idx = if let Some((_, lambda_idx)) = retyping_override {
            lambda_idx
        } else {
            let lambda_idx = self.num_lambdas;
            self.num_lambdas += 1;
            lambda_idx
        };

        let typed_args = if let Some((args, _)) = retyping_override {
            for (arg_tok, arg_type, _) in &args {
                let arg_name = Token::get_ident_name(arg_tok);
                self.add_binding(&arg_name, arg_tok, arg_type, false);
            }
            args
        } else {
            self.visit_fn_args(args, false, true, false)?
                .into_iter()
                .map(|(tok, typ, _, default)| (tok, typ, default))
                .collect()
        };

        // TODO: Filter out anon args (args named `_`), since we don't care about those
        let has_unknown = typed_args.iter().any(|(_, typ, _)| typ == &Type::Unknown);

        let arg_types = typed_args.iter()
            .map(|(ident, typ, default_value)| {
                (Token::get_ident_name(ident).clone(), typ.clone(), default_value.is_some())
            })
            .collect::<Vec<_>>();

        let typed_node = if has_unknown {
            let fn_type = Type::Fn(FnType { arg_types, type_args: vec![], ret_type: Box::new(Type::Unknown), is_variadic: false, is_enum_constructor: false });
            let orig_node = Some((orig_node, orig_scopes));
            let node = TypedLambdaNode { idx: lambda_idx, typ: fn_type, args: typed_args, typed_body: None, orig_node };
            TypedAstNode::Lambda(token, node)
        } else {
            let mut typed_body = self.visit_fn_body(body)?;
            let body_type = typed_body.last().map_or(Type::Unit, |node| node.get_type());
            // TODO: This will generate unnecessary extra bytecode if the last node is an _expression_ which returns Unit; we should only do this if the last item is a _statement_ which returns Unit
            if body_type == Type::Unit {
                typed_body.push(TypedAstNode::_Nil(Token::None(Position::new(0, 0))));
            }

            let fn_type = Type::Fn(FnType { arg_types, type_args: vec![], ret_type: Box::new(body_type), is_variadic: false, is_enum_constructor: false });
            let node = TypedLambdaNode { idx: lambda_idx, typ: fn_type, args: typed_args, typed_body: Some(typed_body), orig_node: None };
            TypedAstNode::Lambda(token, node)
        };

        self.scopes.pop();

        Ok(typed_node)
    }

    fn visit_tuple(&mut self, token: Token, nodes: Vec<AstNode>) -> Result<TypedAstNode, TypecheckerErrorKind> {
        let items = nodes.into_iter()
            .map(|n| { self.visit(n) })
            .collect::<Result<Vec<_>, _>>()?;
        let types = items.iter().map(|i| i.get_type()).collect();
        let node = TypedTupleNode { typ: Type::Tuple(types), items };
        Ok(TypedAstNode::Tuple(token, node))
    }
}
