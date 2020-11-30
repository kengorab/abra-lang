use crate::builtins::native_types::{NativeArray, NativeType, NativeString, NativeFloat, NativeInt};
use crate::common::ast_visitor::AstVisitor;
use crate::lexer::tokens::{Token, Position};
use crate::parser::ast::{AstNode, AstLiteralNode, UnaryNode, BinaryNode, BinaryOp, UnaryOp, ArrayNode, BindingDeclNode, AssignmentNode, IndexingNode, IndexingMode, GroupedNode, IfNode, FunctionDeclNode, InvocationNode, WhileLoopNode, ForLoopNode, TypeDeclNode, MapNode, AccessorNode, LambdaNode, TypeIdentifier, EnumDeclNode, MatchNode, MatchCase, MatchCaseType};
use crate::vm::prelude::PRELUDE;
use crate::typechecker::types::{Type, StructType, FnType, EnumType, EnumVariantType};
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode, TypedInvocationNode, TypedWhileLoopNode, TypedForLoopNode, TypedTypeDeclNode, TypedMapNode, TypedAccessorNode, TypedInstantiationNode, AssignmentTargetKind, TypedLambdaNode, TypedEnumDeclNode, EnumVariantKind, TypedMatchNode};
use crate::typechecker::typechecker_error::{TypecheckerError, InvalidAssignmentTargetReason};
use std::collections::{HashSet, HashMap};
use std::iter::FromIterator;

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeBinding(/*token:*/ Token, /*type:*/ Type, /*is_mutable:*/ bool);

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeKind {
    Root,
    Block,
    Function(/*token: */ Token, /*name: */ String, /*is_recursive: */ bool),
    Lambda(/*token: */ Token),
    TypeDef,
    Loop,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub kind: ScopeKind,
    pub bindings: HashMap<String, ScopeBinding>,
    pub types: HashMap<String, (Type, /* Must be a TypedAstNode::TypeDecl */ Option<TypedAstNode>)>,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Scope { kind, bindings: HashMap::new(), types: HashMap::new() }
    }

    fn root_scope() -> Self {
        let mut scope = Scope::new(ScopeKind::Root);

        PRELUDE.with(|prelude| {
            for (name, typ) in prelude.get_binding_types().into_iter() {
                let token = Token::Ident(Position::new(0, 0), name.clone());
                scope.bindings.insert(name, ScopeBinding(token, typ, false));
            }

            for (name, typ) in prelude.get_typedefs().into_iter() {
                scope.types.insert(name, (typ, None));
            }
        });

        scope
    }
}

pub struct Typechecker {
    pub(crate) cur_typedef: Option<Type>,
    pub(crate) scopes: Vec<Scope>,
    pub(crate) referencable_types: HashMap<String, Type>,
}

impl Typechecker {
    pub fn get_referencable_types(&self) -> &HashMap<String, Type> {
        &self.referencable_types
    }

    fn get_binding(&self, name: &str) -> Option<(&ScopeBinding, usize)> {
        let mut depth = 0;
        for scope in self.scopes.iter().rev() {
            match scope.bindings.get(name) {
                None => {
                    depth += 1;
                    continue;
                }
                Some(binding) => return Some((binding, self.scopes.len() - depth - 1))
            }
        }
        None
    }

    fn get_binding_in_current_scope(&self, name: &str) -> Option<&ScopeBinding> {
        self.scopes.last().and_then(|scope| scope.bindings.get(name))
    }

    fn add_binding(&mut self, name: &str, ident: &Token, typ: &Type, is_mutable: bool) {
        let scope = self.scopes.last_mut().unwrap();
        let binding = ScopeBinding(ident.clone(), typ.clone(), is_mutable);
        scope.bindings.insert(name.to_string(), binding);
    }

    fn get_binding_mut(&mut self, name: &str) -> Option<&mut ScopeBinding> {
        self.scopes.iter_mut().rev()
            .flat_map(|scope| scope.bindings.iter_mut())
            .find(|(binding_name, _)| binding_name == &name)
            .map(|(_, binding)| binding)
    }

    fn get_types_in_scope(&self) -> HashMap<String, Type> {
        self.scopes.iter().rev().flat_map(|scope| scope.types.iter())
            .map(|(name, (typ, _))| (name.clone(), typ.clone()))
            .collect()
    }

    fn get_generics_in_scope(&self) -> HashSet<String> {
        self.scopes.iter().rev().flat_map(|scope| scope.types.iter())
            .filter_map(|(_, (typ, _))| if let Type::Generic(name) = typ { Some(name.clone()) } else { None })
            .collect()
    }

    fn type_from_type_ident(&self, type_ident: &TypeIdentifier) -> Result<Type, TypecheckerError> {
        let type_from_ident = Type::from_type_ident(type_ident, &self.get_types_in_scope());
        match type_from_ident {
            Err(tok) => Err(TypecheckerError::UnknownType { type_ident: tok }),
            Ok(typ) => {
                if let Type::Reference(name, ref_type_args) = &typ {
                    match &self.referencable_types[name] {
                        Type::Struct(StructType { type_args, .. }) => {
                            if ref_type_args.len() != type_args.len() {
                                return Err(TypecheckerError::InvalidTypeArgumentArity {
                                    token: type_ident.get_ident(),
                                    actual_type: self.referencable_types[name].clone(),
                                    actual: ref_type_args.len(),
                                    expected: type_args.len(),
                                });
                            }
                        }
                        Type::Enum(_) => {}
                        _ => unreachable!("Can't reference anything but Struct types")
                    }
                }
                Ok(typ)
            }
        }
    }

    fn add_type(&mut self, name: String, type_decl_node: Option<TypedAstNode>, typ: Type) {
        let scope = self.scopes.last_mut().unwrap();
        scope.types.insert(name, (typ, type_decl_node));
    }

    fn get_type(&self, name: &String) -> Option<(Type, Option<TypedAstNode>)> {
        self.scopes.iter().rev()
            .flat_map(|scope| scope.types.iter())
            .find(|(type_name, _)| type_name == &name)
            .map(|(_, pair)| pair.clone())
    }

    fn get_type_mut(&mut self, name: &String) -> Option<(&mut Type, &mut Option<TypedAstNode>)> {
        self.scopes.iter_mut().rev()
            .flat_map(|scope| scope.types.iter_mut())
            .find(|(type_name, _)| type_name == &name)
            .map(|(_, (typ, type_decl_node))| (typ, type_decl_node))
    }

    fn resolve_ref_type(&self, typ: &Type) -> Type {
        match typ {
            Type::Reference(name, type_args) => {
                Type::hydrate_reference_type(name, type_args, &self.referencable_types)
                    .expect(&format!("There should be a type referencable by name '{}'", name))
            }
            t @ _ => t.clone()
        }
    }

    fn are_types_equivalent(&mut self, node: &mut TypedAstNode, target_type: &Type) -> Result<bool, TypecheckerError> {
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
        let typ = if node.get_type().is_unknown(&self.referencable_types) {
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
                                    return Err(TypecheckerError::IncorrectArity { token: token.clone(), expected: expected_args.len(), actual: lambda_node.args.len() });
                                }
                                retyped_args.push((lambda_arg_token.clone(), lambda_arg_type.clone(), default_value.clone()));
                            }
                            (Some(_), None) => {
                                return Err(TypecheckerError::IncorrectArity { token: token.clone(), expected: expected_args.len(), actual: lambda_node.args.len() });
                            }
                            (None, None) => break
                        }
                    }

                    let (orig_node, orig_scopes) = lambda_node.orig_node.as_ref().unwrap().clone();
                    let mut scopes = orig_scopes;
                    std::mem::swap(&mut self.scopes, &mut scopes);

                    let retyped_lambda = self.visit_lambda(token.clone(), orig_node, Some(retyped_args))?;
                    let lambda_type = retyped_lambda.get_type();

                    std::mem::swap(&mut self.scopes, &mut scopes);

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

                            if !ret.is_equivalent_to(expected_ret, &self.referencable_types) {
                                return Err(TypecheckerError::Mismatch {
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
                (node, _) => self.resolve_ref_type(&node.get_type())
            }
        } else {
            self.resolve_ref_type(&node.get_type())
        };

        if target_type.is_unknown(&self.referencable_types) {
            Ok(true) // TODO: This could be wrong
        } else {
            Ok(typ.is_equivalent_to(target_type, &self.referencable_types))
        }
    }

    fn visit_block(&mut self, is_stmt: bool, body: Vec<AstNode>) -> Result<Vec<TypedAstNode>, TypecheckerError> {
        let len = body.len();
        body.into_iter().enumerate()
            .map(|(idx, mut node)| {
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
                self.visit(node)
            })
            .collect()
    }

    // Called from visit_if_expression and visit_if_statement, but it has to be up here since it's
    // not part of the AstVisitor trait.
    fn visit_if_node(&mut self, is_stmt: bool, node: IfNode) -> Result<TypedIfNode, TypecheckerError> {
        let IfNode { condition, condition_binding, if_block, else_block } = node;

        let condition = self.visit(*condition)?;
        let is_valid_cond_type = match condition.get_type() {
            Type::Option(_) | Type::Bool => true,
            _ => false
        };
        if !is_valid_cond_type {
            let token = condition.get_token().clone();
            return Err(TypecheckerError::InvalidIfConditionType { token, actual: condition.get_type() });
        }
        let condition = Box::new(condition);

        let mut scope = Scope::new(ScopeKind::Block);
        if let Some(ident) = &condition_binding {
            let ident_name = Token::get_ident_name(ident).clone();
            let binding_type = match condition.get_type() {
                Type::Bool => Type::Bool,
                Type::Option(inner) => {
                    let mut typ = *inner;
                    while let Type::Option(inner) = typ { typ = *inner };
                    typ
                }
                _ => unreachable!("No other types should be allowable as conditionals")
            };
            scope.bindings.insert(ident_name, ScopeBinding(ident.clone(), binding_type, false));
        }
        self.scopes.push(scope);
        let if_block = self.visit_block(is_stmt, if_block)?;
        self.scopes.pop();

        let else_block = match else_block {
            None => None,
            Some(body) => {
                self.scopes.push(Scope::new(ScopeKind::Block));
                let else_block = self.visit_block(is_stmt, body)?;
                self.scopes.pop();

                Some(else_block)
            }
        };

        // Temporarily use Type::Unit as a placeholder, if it's an expression it'll be updated later
        Ok(TypedIfNode { typ: Type::Unit, condition, condition_binding, if_block, else_block })
    }

    fn flatten_match_case_types(&self, typ: Type) -> Vec<Type> {
        match self.resolve_ref_type(&typ) {
            Type::Union(opts) => opts.into_iter().flat_map(|t| self.flatten_match_case_types(t)).collect(),
            Type::Option(t) => vec![self.flatten_match_case_types(*t), vec![Type::Unknown]].concat(),
            Type::Enum(enum_type) => {
                let typ = Type::Reference(enum_type.name, vec![]);
                enum_type.variants.into_iter()
                    .map(|evt| Type::EnumVariant(Box::new(typ.clone()), evt, true))
                    .collect()
            }
            _ => vec![typ]
        }
    }

    // Called from visit_match_expression and visit_match_statement, but it has to be up here since it's
    // not part of the AstVisitor trait.
    fn visit_match_node(&mut self, is_stmt: bool, match_token: &Token, node: MatchNode) -> Result<TypedMatchNode, TypecheckerError> {
        let MatchNode { target, branches } = node;

        let target = self.visit(*target)?;
        let target_type = self.resolve_ref_type(&target.get_type());

        let mut possibilities = self.flatten_match_case_types(target_type);
        let mut seen_wildcard = false;

        let mut typed_branches = Vec::new();
        for (case, block) in branches {
            let MatchCase { match_type, case_binding, args } = case;
            let ((match_type, match_type_ident), case_token) = match match_type {
                MatchCaseType::Ident(ident) => {
                    if seen_wildcard {
                        return Err(TypecheckerError::UnreachableMatchCase { token: ident, typ: None, is_unreachable_none: false });
                    }

                    let t = if let Token::None(_) = &ident {
                        if possibilities.contains(&Type::Unknown) {
                            let idx = possibilities.iter().position(|t| t == &Type::Unknown).unwrap();
                            possibilities.remove(idx);
                            (Type::Unknown, None)
                        } else {
                            return Err(TypecheckerError::UnreachableMatchCase { token: ident, typ: None, is_unreachable_none: true });
                        }
                    } else {
                        let type_ident = TypeIdentifier::Normal { ident: ident.clone(), type_args: None };
                        let typ = self.type_from_type_ident(&type_ident)?;
                        if let Type::Generic(_) = &typ {
                            return Err(TypecheckerError::Unimplemented(ident, "Cannot match against generic types in match case arms".to_string()));
                        }
                        if possibilities.contains(&typ) {
                            let idx = possibilities.iter().position(|t| t == &typ).unwrap();
                            possibilities.remove(idx);
                            (typ, Some(type_ident))
                        } else {
                            return Err(TypecheckerError::UnreachableMatchCase { token: ident, typ: Some(typ), is_unreachable_none: false });
                        }
                    };
                    (t, ident)
                }
                MatchCaseType::Compound(idents) => {
                    let mut idents = idents.into_iter();
                    let type_ident = TypeIdentifier::Normal { ident: idents.next().expect("There should be at least one ident"), type_args: None };
                    let typ = self.type_from_type_ident(&type_ident)?;
                    let enum_type = match self.resolve_ref_type(&typ) {
                        Type::Enum(enum_type) => enum_type,
                        _ => unreachable!("Unexpected non-enum compound type used as match condition")
                    };
                    let variants = enum_type.variants.iter().map(|v| v.name.clone()).collect::<Vec<_>>();

                    let variant_ident = idents.next().expect("There should be at least 2 idents");
                    let variant_name = Token::get_ident_name(&variant_ident);
                    let variant_idx = variants.iter().position(|v| v == &variant_name);
                    if variant_idx.is_none() {
                        return Err(TypecheckerError::UnknownMember { token: variant_ident, target_type: typ });
                    } else if let Some(token) = idents.next() {
                        return Err(TypecheckerError::UnknownMember { token, target_type: typ });
                    }
                    let variant_idx = variant_idx.expect("An error is raised if it's None");

                    let idx = possibilities.iter().enumerate().find_map(|(idx, possibility)| {
                        if let Type::EnumVariant(variant_enum_type, variant, _) = possibility {
                            if let Type::Enum(variant_enum_type) = self.resolve_ref_type(&variant_enum_type) {
                                if enum_type == variant_enum_type && variant_idx == variant.variant_idx {
                                    return Some(idx);
                                }
                            }
                        }
                        None
                    });
                    match idx {
                        Some(idx) => {
                            let enum_variant_typ = possibilities.remove(idx);
                            ((enum_variant_typ, Some(type_ident)), variant_ident)
                        }
                        None => {
                            let enum_variant_typ = Type::EnumVariant(Box::new(typ), EnumVariantType { name: variant_name, variant_idx, arg_types: None }, true);
                            return Err(TypecheckerError::UnreachableMatchCase { token: variant_ident, typ: Some(enum_variant_typ), is_unreachable_none: false });
                        }
                    }
                }
                MatchCaseType::Wildcard(token) => {
                    if seen_wildcard {
                        return Err(TypecheckerError::DuplicateMatchCase { token });
                    }
                    seen_wildcard = true;
                    let t = if possibilities.is_empty() {
                        return Err(TypecheckerError::UnreachableMatchCase { token, typ: None, is_unreachable_none: false });
                    } else if possibilities.len() == 1 {
                        (possibilities.drain(..).next().unwrap(), None)
                    } else {
                        (Type::Union(possibilities.drain(..).collect()), None)
                    };
                    (t, token)
                }
            };

            let mut scope = Scope::new(ScopeKind::Block);
            let case_binding_name = if let Some(ident) = case_binding {
                let ident_name = Token::get_ident_name(&ident).clone();
                scope.bindings.insert(ident_name.clone(), ScopeBinding(ident, match_type.clone(), false));
                Some(ident_name)
            } else { None };

            if let Some(destructured_args) = &args {
                match &match_type {
                    Type::EnumVariant(_, variant, _) => {
                        match &variant.arg_types {
                            Some(arg_types) => {
                                if arg_types.len() != destructured_args.len() {
                                    let token = if destructured_args.len() > arg_types.len() {
                                        destructured_args[destructured_args.len() - arg_types.len()].clone()
                                    } else {
                                        destructured_args[destructured_args.len() - 1].clone()
                                    };
                                    return Err(TypecheckerError::InvalidDestructuringArity { token, typ: match_type.clone(), expected: arg_types.len(), actual: destructured_args.len() });
                                }
                                for ((_, arg_type, _), arg_tok) in arg_types.iter().zip(destructured_args.iter()) {
                                    let arg_name = Token::get_ident_name(arg_tok);
                                    scope.bindings.insert(arg_name, ScopeBinding(arg_tok.clone(), arg_type.clone(), false));
                                }
                            }
                            None => return Err(TypecheckerError::InvalidDestructuring { token: case_token, typ: match_type.clone() })
                        }
                    }
                    _ => return Err(TypecheckerError::InvalidDestructuring { token: case_token, typ: match_type.clone() })
                };
            }

            if block.is_empty() && !is_stmt {
                return Err(TypecheckerError::EmptyMatchBlock { token: case_token });
            }
            self.scopes.push(scope);
            let typed_block = self.visit_block(is_stmt, block)?;
            self.scopes.pop();

            typed_branches.push((match_type, match_type_ident, case_binding_name, typed_block, args));
        }
        if !possibilities.is_empty() {
            return Err(TypecheckerError::NonExhaustiveMatch { token: match_token.clone() });
        }

        // Temporarily use Type::Unit as a placeholder, if it's an expression it'll be updated later
        Ok(TypedMatchNode { typ: Type::Unit, target: Box::new(target), branches: typed_branches })
    }

    fn visit_fn_args(
        &mut self,
        args: Vec<(Token, Option<TypeIdentifier>, Option<AstNode>)>,
        allow_self_param: bool,
        allow_unknown_arg_types: bool,
    ) -> Result<Vec<(Token, Type, Option<TypedAstNode>)>, TypecheckerError> {
        let mut typed_args = Vec::<(Token, Type, Option<TypedAstNode>)>::with_capacity(args.len());
        let mut arg_idents = HashMap::<String, Token>::new();
        let mut seen_optional_arg = false;
        for (idx, (token, type_ident, default_value)) in args.into_iter().enumerate() {
            let arg_name = Token::get_ident_name(&token).clone();

            if let Token::Self_(_) = &token {
                if !allow_self_param {
                    return Err(TypecheckerError::InvalidSelfParam { token: token.clone() });
                }
                if idx != 0 {
                    return Err(TypecheckerError::InvalidSelfParamPosition { token: token.clone() });
                }

                let arg_type = match &self.cur_typedef {
                    None => return Err(TypecheckerError::InvalidSelfParam { token: token.clone() }),
                    Some(cur_type) => cur_type.clone(),
                };

                self.add_binding(&arg_name, &token, &arg_type, false);
                typed_args.push((token.clone(), arg_type, None));

                continue;
            }

            if let Some(arg_tok) = arg_idents.get(&arg_name) {
                return Err(TypecheckerError::DuplicateBinding { orig_ident: arg_tok.clone(), ident: token.clone() });
            }
            arg_idents.insert(arg_name, token.clone());

            match type_ident {
                Some(type_ident) => {
                    let arg_type = self.type_from_type_ident(&type_ident)?;
                    match default_value {
                        Some(default_value) => {
                            seen_optional_arg = true;
                            let mut default_value = self.visit(default_value)?;
                            if self.are_types_equivalent(&mut default_value, &arg_type)? {
                                let arg_name = Token::get_ident_name(&token);
                                self.add_binding(&arg_name, &token, &arg_type, false);
                                typed_args.push((token, arg_type, Some(default_value)));
                            } else {
                                return Err(TypecheckerError::Mismatch { token: default_value.get_token().clone(), expected: arg_type, actual: default_value.get_type() });
                            }
                        }
                        None => {
                            if seen_optional_arg {
                                return Err(TypecheckerError::InvalidRequiredArgPosition(token));
                            }
                            let arg_name = Token::get_ident_name(&token);
                            self.add_binding(&arg_name, &token, &arg_type, false);
                            typed_args.push((token, arg_type, None));
                        }
                    }
                }
                None => {
                    match default_value {
                        None => {
                            if allow_unknown_arg_types {
                                typed_args.push((token, Type::Unknown, None));
                            } else { unreachable!() /* This should be caught during parsing */ }
                        }
                        Some(default_value) => {
                            seen_optional_arg = true;
                            let default_value = self.visit(default_value)?;
                            let arg_type = default_value.get_type();
                            let arg_name = Token::get_ident_name(&token);
                            self.add_binding(&arg_name, &token, &arg_type, false);
                            typed_args.push((token, arg_type, Some(default_value)));
                        }
                    }
                }
            }
        }

        Ok(typed_args)
    }

    fn visit_fn_body(&mut self, body: Vec<AstNode>) -> Result<Vec<TypedAstNode>, TypecheckerError> {
        let body_len = body.len();
        body.into_iter().enumerate()
            .map(|(idx, node)| {
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

                            let mut typ = typed_node.get_type();
                            while let Type::Option(inner) = typ { typ = *inner };

                            match typ {
                                Type::Unit => {
                                    if let TypedAstNode::IfExpression(token, mut typed_if_node) = typed_node {
                                        // Explicitly set the underlying node's type to Unit, in case
                                        // it had been Unit? (eg. an if-expr missing an else branch)
                                        typed_if_node.typ = Type::Unit;
                                        Ok(TypedAstNode::IfStatement(token, typed_if_node))
                                    } else { unreachable!() }
                                }
                                _ => Ok(typed_node)
                            }
                        }
                        AstNode::MatchStatement(token, match_node) => {
                            let node = AstNode::MatchExpression(token.clone(), match_node);
                            let typed_node = self.visit(node)?;

                            let mut typ = typed_node.get_type();
                            while let Type::Option(inner) = typ { typ = *inner };

                            match typ {
                                Type::Unit => {
                                    if let TypedAstNode::MatchExpression(token, mut typed_match_node) = typed_node {
                                        // Explicitly set the underlying node's type to Unit, in case
                                        // it had been Unit? (eg. an if-expr missing an else branch)
                                        typed_match_node.typ = Type::Unit;
                                        Ok(TypedAstNode::MatchStatement(token, typed_match_node))
                                    } else { unreachable!() }
                                }
                                _ => Ok(typed_node)
                            }
                        }
                        n @ _ => self.visit(n)
                    }
                } else {
                    self.visit(node)
                }
            })
            .collect()
    }

    fn typecheck_typedef_methods_phase_1(
        &mut self,
        type_args: &Vec<Token>,
        methods: &Vec<AstNode>,
    ) -> Result<(/* static_fields: */ Vec<(String, Type, bool)>, /* typed_methods: */ Vec<(String, Type)>), TypecheckerError> {
        let mut static_fields = Vec::new();
        let mut typed_methods = Vec::new();

        for func_decl_node in methods {
            let FunctionDeclNode { name, type_args: fn_type_args, ret_type, args, .. } = match &func_decl_node {
                AstNode::FunctionDecl(_, node) => node,
                _ => unreachable!()
            };
            let type_args = type_args.iter().map(|t| (Token::get_ident_name(t), t)).collect::<HashMap<String, &Token>>();

            // Create temporary "scope" to capture function's type_args, and...
            let mut fn_type_arg_names = Vec::new();
            let mut scope = Scope::new(ScopeKind::TypeDef);
            for fn_type_arg in fn_type_args {
                let fn_type_arg_name = Token::get_ident_name(fn_type_arg);
                if let Some(orig_ident) = type_args.get(&fn_type_arg_name) {
                    return Err(TypecheckerError::DuplicateTypeArgument { ident: fn_type_arg.clone(), orig_ident: (*orig_ident).clone() });
                }
                fn_type_arg_names.push(fn_type_arg_name.clone());
                scope.types.insert(fn_type_arg_name.clone(), (Type::Generic(fn_type_arg_name), None));
            }
            self.scopes.push(scope);

            let ret_type = match ret_type {
                None => return Err(TypecheckerError::MissingRequiredTypeAnnotation { token: name.clone() }),
                Some(ret_type_ident) => self.type_from_type_ident(ret_type_ident)?,
            };

            let mut is_static = true;
            let mut arg_types = Vec::new();
            for (ident, type_ident, default_value) in args {
                match ident {
                    Token::Self_(_) => is_static = false,
                    ident @ _ => {
                        let arg_type = match type_ident {
                            None => match default_value {
                                None => unreachable!(), // This should be caught during parsing
                                Some(default_value) => {
                                    let default_value = self.visit(default_value.clone())?;
                                    default_value.get_type()
                                }
                            },
                            Some(type_ident) => self.type_from_type_ident(type_ident)?,
                        };
                        arg_types.push((Token::get_ident_name(ident).clone(), arg_type.clone(), default_value.is_some()));
                    }
                }
            }

            let fn_name = Token::get_ident_name(name).clone();
            let fn_type = Type::Fn(FnType { arg_types, type_args: fn_type_arg_names, ret_type: Box::new(ret_type.clone()) });
            // let typedef = if let Some(Type::Struct(typedef)) = self.referencable_types.get_mut(&new_type_name) { typedef } else { unreachable!() };
            if is_static {
                // TODO: Handle static methods referencing type's type_args
                // typedef.static_fields.push((fn_name, fn_type, true))
                static_fields.push((fn_name, fn_type, true))
            } else {
                // typedef.methods.push((fn_name, fn_type))
                typed_methods.push((fn_name, fn_type))
            }
            // ...pop off the temporary scope here.
            self.scopes.pop();
        }

        Ok((static_fields, typed_methods))
    }

    fn typecheck_typedef_methods_phase_2(
        &mut self,
        is_enum: bool,
        methods: Vec<AstNode>,
        field_names: HashMap<String, Token>,
    ) -> Result<(/* static_fields: */ Vec<(Token, Type, Option<TypedAstNode>)>, /* typed_methods: */ Vec<(String, TypedAstNode)>), TypecheckerError> {
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
                return Err(TypecheckerError::DuplicateField { orig_ident: orig_ident.clone(), ident: name_tok.clone(), orig_is_field: false, orig_is_enum_variant: false });
            } else if let Some(orig_ident) = field_names.get(&name) {
                return Err(TypecheckerError::DuplicateField { orig_ident: orig_ident.clone(), ident: name_tok.clone(), orig_is_field: !is_enum, orig_is_enum_variant: is_enum });
            }

            method_names.insert(name.clone(), name_tok.clone());

            let typed_func_decl = self.visit(func_decl_node)?;
            match &typed_func_decl {
                TypedAstNode::FunctionDecl(tok, TypedFunctionDeclNode { name, args, .. }) => {
                    let name = Token::get_ident_name(&name);
                    let is_static = args.iter()
                        .find(|(ident, _, _)| if let Token::Self_(_) = ident { true } else { false })
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
}

pub fn typecheck(ast: Vec<AstNode>) -> Result<(Typechecker, Vec<TypedAstNode>), TypecheckerError> {
    let mut typechecker = Typechecker {
        cur_typedef: None,
        scopes: vec![Scope::root_scope()],
        referencable_types: HashMap::new(),
    };

    let results: Result<Vec<TypedAstNode>, TypecheckerError> = ast.into_iter()
        .map(|node| typechecker.visit(node))
        .collect();
    Ok((typechecker, results?))
}

impl AstVisitor<TypedAstNode, TypecheckerError> for Typechecker {
    fn visit_literal(&mut self, token: Token, node: AstLiteralNode) -> Result<TypedAstNode, TypecheckerError> {
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

    fn visit_unary(&mut self, token: Token, node: UnaryNode) -> Result<TypedAstNode, TypecheckerError> {
        let expr = *node.expr;
        let typed_expr = self.visit(expr)?;
        let expr_type = typed_expr.get_type();
        match (&node.op, &expr_type) {
            (UnaryOp::Minus, Type::Int) | (UnaryOp::Minus, Type::Float) |
            (UnaryOp::Negate, Type::Bool) => {
                let node = TypedUnaryNode { typ: expr_type, op: node.op, expr: Box::new(typed_expr) };
                Ok(TypedAstNode::Unary(token, node))
            }
            (op @ UnaryOp::Minus, _) | (op @ UnaryOp::Negate, _) => {
                let expected = if op == &UnaryOp::Minus {
                    Type::Union(vec![Type::Int, Type::Float])
                } else {
                    Type::Bool
                };
                Err(TypecheckerError::Mismatch { token, expected, actual: expr_type })
            }
        }
    }

    fn visit_binary(&mut self, token: Token, node: BinaryNode) -> Result<TypedAstNode, TypecheckerError> {
        #[inline]
        fn type_for_op(
            zelf: &mut Typechecker,
            token: &Token,
            op: &BinaryOp,
            typed_left: &TypedAstNode,
            typed_right: &mut TypedAstNode,
        ) -> Result<Type, TypecheckerError> {
            let ltype = typed_left.get_type();
            let rtype = typed_right.get_type();

            match op {
                BinaryOp::Add | BinaryOp::AddEq =>
                    match (&ltype, &rtype) {
                        (Type::String, _) | (_, Type::String) => Ok(Type::String),
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Sub | BinaryOp::SubEq | BinaryOp::Mul | BinaryOp::MulEq =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Div | BinaryOp::DivEq =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) | (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Mod | BinaryOp::ModEq =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::And | BinaryOp::AndEq | BinaryOp::Or | BinaryOp::OrEq =>
                    match (&ltype, &rtype) {
                        (Type::Bool, Type::Bool) => Ok(Type::Bool),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte =>
                    match (&ltype, &rtype) {
                        (Type::String, Type::String) => Ok(Type::Bool),
                        (Type::Int, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) | (Type::Float, Type::Int) => Ok(Type::Bool),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Neq | BinaryOp::Eq => Ok(Type::Bool),
                BinaryOp::Coalesce | BinaryOp::CoalesceEq => {
                    match (&ltype, &rtype) {
                        (Type::Option(ltype), rtype @ _) => {
                            if !zelf.are_types_equivalent(typed_right, ltype)? {
                                let token = typed_right.get_token().clone();
                                Err(TypecheckerError::Mismatch { token, expected: (**ltype).clone(), actual: rtype.clone() })
                            } else {
                                Ok(rtype.clone())
                            }
                        }
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
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
            op @ _ => (false, op, token)
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

    fn visit_grouped(&mut self, token: Token, node: GroupedNode) -> Result<TypedAstNode, TypecheckerError> {
        let GroupedNode { expr } = node;
        let typed_expr = self.visit(*expr)?;
        let typ = typed_expr.get_type();
        let expr = Box::new(typed_expr);
        Ok(TypedAstNode::Grouped(token, TypedGroupedNode { typ, expr }))
    }

    fn visit_array(&mut self, token: Token, node: ArrayNode) -> Result<TypedAstNode, TypecheckerError> {
        let items: Result<Vec<TypedAstNode>, TypecheckerError> = node.items.into_iter()
            .map(|n| self.visit(*n))
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

        let items = items.into_iter()
            .map(Box::new)
            .collect();

        Ok(TypedAstNode::Array(token.clone(), TypedArrayNode { typ: Type::Array(Box::new(typ)), items }))
    }

    fn visit_map_literal(&mut self, token: Token, node: MapNode) -> Result<TypedAstNode, TypecheckerError> {
        let MapNode { items } = node;

        let mut fields = Vec::<(Token, TypedAstNode)>::new();
        let mut field_types = Vec::<(String, Type)>::new();
        let mut field_names = HashMap::<String, Token>::new();
        for (field_name_tok, field_value) in items {
            let field_name = Token::get_ident_name(&field_name_tok);
            if let Some(orig_ident) = field_names.get(&field_name) {
                return Err(TypecheckerError::DuplicateBinding { orig_ident: orig_ident.clone(), ident: field_name_tok });
            } else {
                field_names.insert(field_name.clone(), field_name_tok.clone());
            }

            let field_value = self.visit(field_value)?;
            let field_type = field_value.get_type();
            field_types.push((field_name.clone(), field_type));
            fields.push((field_name_tok.clone(), field_value));
        }

        let all_types = field_types.iter()
            .map(|(_, typ)| typ)
            .collect::<HashSet<&Type>>();
        let homogeneous_type = if all_types.len() <= 1 {
            match all_types.into_iter().next() {
                Some(typ) => Some(Box::new(typ.clone())),
                None => Some(Box::new(Type::Any))
            }
        } else { None };
        let typ = Type::Map(field_types, homogeneous_type);
        Ok(TypedAstNode::Map(token, TypedMapNode { typ, items: fields }))
    }

    fn visit_binding_decl(&mut self, token: Token, node: BindingDeclNode) -> Result<TypedAstNode, TypecheckerError> {
        let BindingDeclNode { is_mutable, ident, type_ann, expr } = node;

        if !is_mutable && expr == None {
            return Err(TypecheckerError::MissingRequiredAssignment { ident });
        }

        let name = Token::get_ident_name(&ident);

        if let Some(ScopeBinding(orig_ident, _, _)) = self.get_binding_in_current_scope(&name) {
            let orig_ident = orig_ident.clone();
            return Err(TypecheckerError::DuplicateBinding { ident, orig_ident });
        }

        let mut typed_expr = match expr {
            Some(e) => Some(self.visit(*e)?),
            None => None
        };

        let typ = match (&mut typed_expr, &type_ann) {
            (Some(e), None) => {
                let typ = e.get_type();
                if typ.has_unbound_generic() {
                    let available_generics = self.get_generics_in_scope();
                    let generic = typ.extract_unbound_generics().iter()
                        .filter(|generic_name| !available_generics.contains(*generic_name))
                        .map(|g| g.clone())
                        .next();
                    if let Some(unbound_generic) = generic {
                        return Err(TypecheckerError::UnboundGeneric(ident, unbound_generic));
                    }
                }
                Ok(typ)
            }
            (None, Some(ann)) => self.type_from_type_ident(ann),
            (Some(typed_expr), Some(ann)) => {
                let ann_type = self.type_from_type_ident(ann)?;

                if self.are_types_equivalent(typed_expr, &ann_type)? {
                    Ok(ann_type)
                } else {
                    Err(TypecheckerError::Mismatch {
                        token: typed_expr.get_token().clone(),
                        expected: ann_type.clone(),
                        actual: typed_expr.get_type(),
                    })
                }
            }
            (None, None) => Err(TypecheckerError::UnannotatedUninitialized {
                ident: ident.clone(),
                is_mutable,
            })
        }?;
        if typ.is_unknown(&self.referencable_types) {
            return Err(TypecheckerError::ForbiddenVariableType { token: ident, typ: Type::Unknown });
        } else if typ.is_unit(&self.referencable_types) {
            return Err(TypecheckerError::ForbiddenVariableType { token: ident, typ: Type::Unit });
        }

        self.add_binding(&name, &ident, &typ, is_mutable);
        let scope_depth = self.scopes.len() - 1;

        let node = TypedBindingDeclNode {
            is_mutable,
            ident,
            expr: typed_expr.map(Box::new),
            scope_depth,
        };
        Ok(TypedAstNode::BindingDecl(token, node))
    }

    fn visit_func_decl(&mut self, token: Token, node: FunctionDeclNode) -> Result<TypedAstNode, TypecheckerError> {
        let FunctionDeclNode { name, type_args, args, ret_type, body, } = node;

        let func_name = Token::get_ident_name(&name);
        if let Some(ScopeBinding(orig_ident, _, _)) = self.get_binding_in_current_scope(&func_name) {
            let orig_ident = orig_ident.clone();
            return Err(TypecheckerError::DuplicateBinding { ident: name, orig_ident });
        }

        let mut scope = Scope::new(ScopeKind::Function(name.clone(), func_name.clone(), false));
        let mut seen = HashMap::<String, Token>::new();
        for type_arg in &type_args {
            let name = Token::get_ident_name(type_arg);
            match seen.get(&name) {
                Some(orig_ident) => {
                    return Err(TypecheckerError::DuplicateTypeArgument { ident: type_arg.clone(), orig_ident: orig_ident.clone() });
                }
                None => {
                    seen.insert(name.clone(), type_arg.clone());
                    scope.types.insert(name.clone(), (Type::Generic(name.clone()), None));
                }
            }
        }
        self.scopes.push(scope);

        let args = self.visit_fn_args(args, true, false)?;

        // Store a stub version of the function type, based on what we know so far. Recursive references
        // to the function within its body will be typed according to whatever is saved now. If we cannot
        // determine the return type (due to a missing return type annotation, since we haven't yet
        // typechecked the body, and thus cannot infer it), store it as Unknown. When identifiers are
        // typechecked later in (say, for example, within the function body), a return type of Unknown
        // will signal a recursive reference to a function which is missing a return type. Note: this is
        // a fairly brittle abstraction, and should probably be readdressed.
        // Note also that we need to add this reference to the previous scope, so once we determine the
        // initial return type...
        let arg_types = args.iter()
            .filter_map(|(ident, typ, default_value)| {
                match ident {
                    Token::Self_(_) => None,
                    ident @ _ => {
                        Some((Token::get_ident_name(ident).clone(), typ.clone(), default_value.is_some()))
                    }
                }
            })
            .collect::<Vec<_>>();
        let initial_ret_type = match &ret_type {
            None => Type::Unknown,
            Some(ret_type) => self.type_from_type_ident(ret_type)?,
        };
        if initial_ret_type.has_unbound_generic() {
            // The valid type args for a return type are all of the generics available in scope, minus
            // the fn type args that aren't referenced in the arguments (and are thus unbound).
            let arg_type_args = args.iter()
                .flat_map(|(_, t, _)| t.extract_unbound_generics().into_iter())
                .collect::<HashSet<_>>();
            let unresolvable_type_arg_names = type_args.iter()
                .map(Token::get_ident_name)
                .filter(|type_arg_name| !arg_type_args.contains(type_arg_name))
                .collect::<HashSet<_>>();
            let generics_in_scope = self.get_generics_in_scope();
            let valid_return_type_args = generics_in_scope.difference(&unresolvable_type_arg_names).collect::<HashSet<_>>();

            let ret_generics = initial_ret_type.extract_unbound_generics();
            for name in ret_generics {
                if !valid_return_type_args.contains(&&name) {
                    let ret_type = ret_type.expect("Should be Some if initial_ret_type is anything but Unknown");
                    let ident_token = ret_type.get_ident();
                    let ret_type_name = Token::get_ident_name(&ident_token);
                    return Err(TypecheckerError::UnboundGeneric(ident_token, ret_type_name));
                }
            }
        }

        // ...we pop off the scope, add the function there, and then push the scope back on.
        let type_args = type_args.iter().map(|t| Token::get_ident_name(t)).collect();
        let func_type = Type::Fn(FnType { arg_types, type_args, ret_type: Box::new(initial_ret_type.clone()) });
        let scope = self.scopes.pop().unwrap();
        self.add_binding(&func_name, &name, &func_type, false);
        self.scopes.push(scope);

        let mut body = self.visit_fn_body(body)?;
        let body_type = body.last().map_or(Type::Unit, |node| node.get_type());

        let ret_type = match ret_type {
            None => body_type,
            Some(ret_type) => {
                let typ = self.type_from_type_ident(&ret_type)?;
                match body.last_mut() {
                    None => body_type,
                    Some(mut node) => {
                        if !self.are_types_equivalent(&mut node, &typ)? {
                            let token = body.last().map_or(name.clone(), |node| node.get_token().clone());
                            return Err(TypecheckerError::Mismatch { token, actual: body_type, expected: typ });
                        } else if typ.has_unbound_generic() {
                            typ
                        } else {
                            node.get_type()
                        }
                    }
                }
            }
        };
        let fn_scope = self.scopes.pop().unwrap();
        let is_recursive = if let ScopeKind::Function(_, _, is_recursive) = fn_scope.kind {
            is_recursive
        } else { unreachable!("A function's scope should always be of ScopeKind::Function") };

        // Rewrite the return type of the previously-inserted func_type stub
        let ScopeBinding(_, func_type, _) = self.get_binding_mut(&func_name).unwrap();
        if let Type::Fn(FnType { ret_type: return_type, .. }) = func_type {
            *return_type = Box::new(ret_type.clone());
        }
        let scope_depth = self.scopes.len() - 1;

        Ok(TypedAstNode::FunctionDecl(token, TypedFunctionDeclNode { name, args, ret_type, body, scope_depth, is_recursive }))
    }

    fn visit_type_decl(&mut self, token: Token, node: TypeDeclNode) -> Result<TypedAstNode, TypecheckerError> {
        if self.scopes.len() != 1 {
            return Err(TypecheckerError::InvalidTypeDeclDepth { token });
        }

        let TypeDeclNode { name, type_args, fields, methods, .. } = node;
        let new_type_name = Token::get_ident_name(&name).clone();

        if let Some((_, typed_decl_node)) = self.get_type(&new_type_name) {
            let token = typed_decl_node.map(|node| node.get_token().clone());
            return Err(TypecheckerError::DuplicateType { ident: name.clone(), orig_ident: token });
        }

        // ------------------------ Begin First-pass Type Gathering ------------------------ \\
        // --- First gather only the type data for each field, method, and static value. --- \\
        // --- This first pass guarantees that all type data is available for the second --- \\
        // --- pass. Without doing this, forward-referencing of fields is not possible.  --- \\
        // --------------------------------------------------------------------------------- \\

        // The type embedded in TypedAstNodes of this type will be a Reference - to materialize this
        // reference we must look it up by name in self.referencable_types. This level of indirection
        // allows for cyclic types.
        let typeref = Type::Reference(new_type_name.clone(), vec![]);
        let binding_type = Type::Type(new_type_name.clone(), Box::new(typeref.clone()), false);
        self.add_binding(&new_type_name, &name, &binding_type, false);
        self.add_type(new_type_name.clone(), None, typeref.clone());

        // As we progress through this type declaration, we build up this value in
        // self.referencable_types. This value will be used for values of type Type::Reference, when
        // the referenced type is "materialized".
        let type_arg_names = type_args.iter()
            .map(|name| {
                let name = Token::get_ident_name(name);
                (name.clone(), Type::Generic(name))
            })
            .collect::<Vec<(String, Type)>>();
        let typedef = StructType { name: new_type_name.clone(), type_args: type_arg_names.clone(), fields: vec![], static_fields: vec![], methods: vec![] };
        self.referencable_types.insert(new_type_name.clone(), Type::Struct(typedef));

        // Update the Reference type of the type's identifier to include the generics, and establish
        // that Reference as the cur_typedef
        let ScopeBinding(_, binding_type, _) = self.get_binding_mut(&new_type_name).unwrap();
        let typeref = Type::Reference(new_type_name.clone(), type_arg_names.iter().map(|p| p.1.clone()).collect());
        *binding_type = Type::Type(new_type_name.clone(), Box::new(typeref.clone()), false);
        self.cur_typedef = Some(typeref);

        // Insert Generics for all type_args present
        let mut scope = Scope::new(ScopeKind::TypeDef);
        let mut seen = HashMap::<String, Token>::new();
        for type_arg in &type_args {
            let name = Token::get_ident_name(type_arg);
            match seen.get(&name) {
                Some(orig_ident) => {
                    return Err(TypecheckerError::DuplicateTypeArgument { ident: type_arg.clone(), orig_ident: orig_ident.clone() });
                }
                None => {
                    seen.insert(name.clone(), type_arg.clone());
                    scope.types.insert(name.clone(), (Type::Generic(name.clone()), None));
                }
            }
        }
        self.scopes.push(scope);

        let mut field_names = HashMap::<String, Token>::new();
        let fields = fields.into_iter()
            .map(|(field_name, field_type, default_value)| {
                let field_type = self.type_from_type_ident(&field_type)?;
                let field_name_str = Token::get_ident_name(&field_name);
                if let Some(orig_ident) = field_names.get(&field_name_str) {
                    return Err(TypecheckerError::DuplicateField { orig_ident: orig_ident.clone(), ident: field_name, orig_is_field: true, orig_is_enum_variant: false });
                } else {
                    field_names.insert(field_name_str.clone(), field_name.clone());
                }
                Ok((field_name, field_type, default_value))
            })
            .collect::<Result<Vec<(Token, Type, Option<AstNode>)>, _>>();
        let fields = fields?;

        let typedef = if let Some(Type::Struct(typedef)) = self.referencable_types.get_mut(&new_type_name) { typedef } else { unreachable!() };
        typedef.fields = fields.iter()
            .map(|(name, typ, default_value_node)| (Token::get_ident_name(name).clone(), typ.clone(), default_value_node.is_some()))
            .collect();

        let (static_methods, typed_methods) = self.typecheck_typedef_methods_phase_1(&type_args, &methods)?;
        let typedef = if let Some(Type::Struct(typedef)) = self.referencable_types.get_mut(&new_type_name) { typedef } else { unreachable!() };
        typedef.static_fields = static_methods;
        typedef.methods = typed_methods;

        // ------------------------  End First-pass Type Gathering  ------------------------ \\
        // --- Now that the current type has been made available to the environment, we  --- \\
        // --- can make references to this type's own fields/methods/static methods it.  --- \\
        // --------------------------------------------------------------------------------- \\
        // ------------------------ Begin Field/Method Typechecking ------------------------ \\

        let typed_fields = fields.into_iter().map(|(tok, field_type, default_value_node)| {
            let default_value = if let Some(default_value) = default_value_node {
                let mut default_value = self.visit(default_value)?;
                let default_value_type = default_value.get_type();
                if !self.are_types_equivalent(&mut default_value, &field_type)? {
                    return Err(TypecheckerError::Mismatch { token: default_value.get_token().clone(), actual: default_value_type, expected: field_type });
                } else {
                    Some(default_value)
                }
            } else { None };
            Ok((tok, field_type, default_value))
        }).collect::<Result<Vec<(Token, Type, Option<TypedAstNode>)>, _>>();
        let typed_fields = typed_fields?;

        // Record TypeDeclNode for type, registering the freshly-typed fields. Later on, we'll mutate
        // this TypeDeclNode and add static fields and methods, but this allows for methods to reference
        // fields of instances of this current type within their bodies.
        let (_, node) = self.get_type_mut(&new_type_name).unwrap();
        let type_decl_node = TypedAstNode::TypeDecl(token, TypedTypeDeclNode { name, fields: typed_fields, static_fields: vec![], methods: vec![] });
        *node = Some(type_decl_node);

        let (static_fields, typed_methods) = self.typecheck_typedef_methods_phase_2(false, methods, field_names)?;
        if let (_, Some(TypedAstNode::TypeDecl(_, type_decl_node))) = self.get_type_mut(&new_type_name).unwrap() {
            type_decl_node.static_fields = static_fields;
            type_decl_node.methods = typed_methods;
        } else { unreachable!("We should have just defined this node up above"); }

        self.scopes.pop();
        self.cur_typedef = None;

        // Return type_decl_node for type
        Ok(self.get_type(&new_type_name).unwrap().1.unwrap())
    }

    fn visit_enum_decl(&mut self, token: Token, node: EnumDeclNode) -> Result<TypedAstNode, TypecheckerError> {
        if self.scopes.len() != 1 {
            return Err(TypecheckerError::InvalidTypeDeclDepth { token });
        }

        let EnumDeclNode { name, variants, methods, .. } = node;
        let new_enum_name = Token::get_ident_name(&name);

        if let Some((_, typed_decl_node)) = self.get_type(&new_enum_name) {
            let token = typed_decl_node.map(|node| node.get_token().clone());
            return Err(TypecheckerError::DuplicateType { ident: name.clone(), orig_ident: token });
        }

        // ------------------------ Begin First-pass Type Gathering ------------------------ \\
        // ---    See comment in visit_type_decl above, the process here is similar      --- \\
        // --------------------------------------------------------------------------------- \\

        // The type embedded in TypedAstNodes of this type will be a Reference - to materialize this
        // reference we must look it up by name in self.referencable_types. This level of indirection
        // allows for cyclic types.
        let typeref = Type::Reference(new_enum_name.clone(), vec![]);
        let binding_type = Type::Type(new_enum_name.clone(), Box::new(typeref.clone()), true);
        self.add_binding(&new_enum_name, &name, &binding_type, false);
        self.add_type(new_enum_name.clone(), None, typeref.clone());

        let typedef = EnumType { name: new_enum_name.clone(), variants: vec![], static_fields: vec![], methods: vec![] };
        self.referencable_types.insert(new_enum_name.clone(), Type::Enum(typedef));

        let ScopeBinding(_, binding_type, _) = self.get_binding_mut(&new_enum_name).unwrap();
        let typeref = Type::Reference(new_enum_name.clone(), vec![]);
        *binding_type = Type::Type(new_enum_name.clone(), Box::new(typeref.clone()), true);
        self.cur_typedef = Some(typeref);

        let scope = Scope::new(ScopeKind::TypeDef);
        self.scopes.push(scope);

        let mut variant_names = HashMap::<String, Token>::new();
        let mut typed_variants = Vec::new();
        for (variant_idx, (name, args)) in variants.iter().enumerate() {
            let variant_name = Token::get_ident_name(name).clone();
            if let Some(orig_ident) = variant_names.get(&variant_name) {
                return Err(TypecheckerError::DuplicateField { orig_ident: orig_ident.clone(), ident: name.clone(), orig_is_field: false, orig_is_enum_variant: true });
            } else {
                variant_names.insert(variant_name.clone(), name.clone());
            }
            let variant_type = match args {
                None => EnumVariantType { name: variant_name, variant_idx, arg_types: None },
                Some(args) => {
                    // Handle the case where a variant arg's default value is itself a variant - since
                    // we haven't finished typechecking variants yet, that will fail. This is a dirty hack
                    // to still reuse the `visit_fn_args` method while forcibly not checking args' default values.
                    let faked_args = args.clone().into_iter()
                        .map(|(tok, ident, _)| (tok, ident, None))
                        .collect();
                    let faked_args = self.visit_fn_args(faked_args, false, false)?;
                    EnumVariantType {
                        name: variant_name,
                        variant_idx,
                        arg_types: Some(
                            faked_args.iter().zip(args.iter())
                                .map(|((arg_name, arg_type, _), (_, _, default_value_node))| {
                                    let arg_name = Token::get_ident_name(arg_name).clone();
                                    // Since we forced the default_value_node to be None in `faked_args`, we need to
                                    // obtain the `is_some` status from the original arg. This is gross
                                    (arg_name, arg_type.clone(), default_value_node.is_some())
                                }).collect()
                        ),
                    }
                }
            };
            typed_variants.push(variant_type);
        }

        let typedef = if let Some(Type::Enum(typedef)) = self.referencable_types.get_mut(&new_enum_name) { typedef } else { unreachable!() };
        typedef.variants = typed_variants.clone();

        let type_args = vec![];
        let (static_fields, typed_methods) = self.typecheck_typedef_methods_phase_1(&type_args, &methods)?;
        let typedef = if let Some(Type::Enum(typedef)) = self.referencable_types.get_mut(&new_enum_name) { typedef } else { unreachable!() };
        typedef.static_fields = static_fields;
        typedef.methods = typed_methods;

        // ------------------------  End First-pass Type Gathering  ------------------------ \\
        // --- Now that the current type has been made available to the environment, we  --- \\
        // --- can make references to this type's own fields/methods/static methods it.  --- \\
        // --------------------------------------------------------------------------------- \\
        // ----------------------- Begin Variant/Method Typechecking ----------------------- \\
        let mut variant_nodes = Vec::new();
        for ((name, args), variant_type) in variants.into_iter().zip(typed_variants) {
            let variant_node = match args {
                None => EnumVariantKind::Basic,
                Some(args) => {
                    let args = self.visit_fn_args(args, false, false)?;
                    EnumVariantKind::Constructor(args)
                }
            };
            let enum_type = self.cur_typedef.as_ref().unwrap().clone();
            let variant_type = Type::EnumVariant(Box::new(enum_type), variant_type, false);
            variant_nodes.push((name, variant_type, variant_node));
        }

        // Record EnumDecl for enum, registering the freshly-typed variants. Later on, we'll mutate
        // this EnumDecl node and add static fields and methods, but this allows for methods to reference
        // variants of this current enum within their bodies.
        let (_, node) = self.get_type_mut(&new_enum_name).unwrap();
        let enum_decl_node = TypedAstNode::EnumDecl(token, TypedEnumDeclNode { name, variants: variant_nodes, static_fields: vec![], methods: vec![] });
        *node = Some(enum_decl_node);

        let (static_fields, typed_methods) = self.typecheck_typedef_methods_phase_2(true, methods, variant_names)?;
        if let (_, Some(TypedAstNode::EnumDecl(_, enum_decl_node))) = self.get_type_mut(&new_enum_name).unwrap() {
            enum_decl_node.static_fields = static_fields;
            enum_decl_node.methods = typed_methods;
        } else { unreachable!("We should have just defined this node up above"); }

        self.scopes.pop();
        self.cur_typedef = None;

        // Return enum_decl_node for enum
        Ok(self.get_type(&new_enum_name).unwrap().1.unwrap())
    }

    fn visit_ident(&mut self, token: Token, type_args: Option<Vec<TypeIdentifier>>) -> Result<TypedAstNode, TypecheckerError> {
        let name = Token::get_ident_name(&token);

        let node = match self.get_binding(&name) {
            None => return Err(TypecheckerError::UnknownIdentifier { ident: token }),
            Some((ScopeBinding(_, typ, is_mutable), scope_depth)) => {
                let binding_typ = typ.clone();
                let is_mutable = is_mutable.clone();

                if let Type::Fn(FnType { ret_type, .. }) = typ {
                    // Type::Unknown acts as the sentinel value for a not-fully typechecked function
                    let has_explicit_ret_type = **ret_type != Type::Unknown;

                    for scope in self.scopes.iter_mut().rev() {
                        if let ScopeKind::Function(func_token, func_name, _) = &scope.kind {
                            if &name == func_name {
                                if !has_explicit_ret_type {
                                    // A function can't be referenced recursively unless it has a declared return type
                                    return Err(TypecheckerError::RecursiveRefWithoutReturnType {
                                        orig_token: func_token.clone(),
                                        token: token.clone(),
                                    });
                                } else {
                                    // If it has a declared return type, then everything's cool. We just need to
                                    // mark this function as recursive. This value will be used downstream.
                                    scope.kind = ScopeKind::Function(func_token.clone(), func_name.clone(), true);
                                }
                            }
                        }
                    }
                }
                TypedIdentifierNode { typ: binding_typ, name: name.clone(), is_mutable, scope_depth }
            }
        };

        #[inline]
        fn get_type_generics(zelf: &mut Typechecker, typ: &Type) -> Vec<String> {
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
            return Err(TypecheckerError::InvalidTypeArgumentArity {
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
                let typ = self.type_from_type_ident(&type_ident)?;
                available_generics.insert(type_arg_name, typ);
            }
            node.typ = Type::substitute_generics(&node.typ, &available_generics);
        }

        Ok(TypedAstNode::Identifier(token, node))
    }

    fn visit_assignment(&mut self, token: Token, node: AssignmentNode) -> Result<TypedAstNode, TypecheckerError> {
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
                    return Err(TypecheckerError::AssignmentToImmutable { token, orig_ident });
                }

                let mut typed_expr = self.visit(*expr)?;
                if !self.are_types_equivalent(&mut typed_expr, typ)? {
                    Err(TypecheckerError::Mismatch {
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
                    return Err(TypecheckerError::InvalidAssignmentTarget { token, reason: Some(InvalidAssignmentTargetReason::IndexingMode) });
                }

                let mut typed_expr = self.visit(*expr)?;
                let expr_type = typed_expr.get_type();

                let typed_target = self.visit_indexing(tok.clone(), node)?;
                let (index_target_type, kind) = match &typed_target {
                    TypedAstNode::Indexing(_, TypedIndexingNode { target, .. }) => {
                        match target.get_type() {
                            Type::Array(inner_type) => (*inner_type, AssignmentTargetKind::ArrayIndex),
                            Type::Map(_, homogeneous_type) => {
                                let map_value_type = match homogeneous_type {
                                    Some(map_val_type) => *map_val_type,
                                    None => expr_type.clone(),
                                };
                                (map_value_type, AssignmentTargetKind::MapIndex)
                            }
                            Type::String => {
                                return Err(TypecheckerError::InvalidAssignmentTarget { token, reason: Some(InvalidAssignmentTargetReason::StringTarget) });
                            }
                            _ => unreachable!()
                        }
                    }
                    _ => unreachable!()
                };

                if !self.are_types_equivalent(&mut typed_expr, &index_target_type)? {
                    let token = typed_expr.get_token().clone();
                    Err(TypecheckerError::Mismatch { token, expected: index_target_type, actual: expr_type })
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
                let mut typed_expr = self.visit(*expr)?;

                let expr_type = typed_expr.get_type();
                let target_type = typed_target.get_type();
                if !self.are_types_equivalent(&mut typed_expr, &target_type)? {
                    let token = typed_expr.get_token().clone();
                    Err(TypecheckerError::Mismatch { token, expected: target_type, actual: expr_type })
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
            _ => Err(TypecheckerError::InvalidAssignmentTarget { token, reason: None })
        }
    }

    fn visit_indexing(&mut self, token: Token, node: IndexingNode) -> Result<TypedAstNode, TypecheckerError> {
        let IndexingNode { target, index } = node;

        let target = self.visit(*target)?;
        let target_type = target.get_type();

        let typ = match (target_type.clone(), &index) {
            (Type::Array(inner_type), IndexingMode::Index(_)) => Ok(Type::Option(inner_type)),
            (Type::Array(inner_type), IndexingMode::Range(_, _)) => Ok(Type::Array(inner_type)),
            (Type::String, _) => Ok(Type::String),
            (Type::Map(fields, homogeneous_type), IndexingMode::Index(_)) => {
                match homogeneous_type {
                    Some(typ) => Ok(Type::Option(typ)),
                    None => Err(TypecheckerError::InvalidIndexingTarget { token: token.clone(), target_type: Type::Map(fields, None) })
                }
            }
            (typ, _) => Err(TypecheckerError::InvalidIndexingTarget { token: token.clone(), target_type: typ })
        }?;

        let index = match index {
            IndexingMode::Index(idx) => {
                let idx = self.visit(*idx)?;
                match (&target_type, idx.get_type()) {
                    (Type::Array(_), Type::Int) | (Type::String, Type::Int) => Ok(IndexingMode::Index(Box::new(idx))),
                    (Type::Map(_, _), Type::String) => Ok(IndexingMode::Index(Box::new(idx))),
                    (target_type, selector_type) => Err(TypecheckerError::InvalidIndexingSelector {
                        token: idx.get_token().clone(),
                        target_type: target_type.clone(),
                        selector_type,
                    })
                }
            }
            IndexingMode::Range(start, end) => {
                #[inline]
                fn visit_endpoint(tc: &mut Typechecker, node: Option<Box<AstNode>>) -> Result<Option<Box<TypedAstNode>>, TypecheckerError> {
                    match node {
                        None => Ok(None),
                        Some(node) => {
                            let typed_node = tc.visit(*node)?;
                            let token = typed_node.get_token().clone();
                            match typed_node.get_type() {
                                Type::Int => Ok(Some(Box::new(typed_node))),
                                typ @ _ => Err(TypecheckerError::Mismatch { token, expected: Type::Int, actual: typ })
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

    fn visit_if_statement(&mut self, token: Token, node: IfNode) -> Result<TypedAstNode, TypecheckerError> {
        let node = self.visit_if_node(true, node)?;
        Ok(TypedAstNode::IfStatement(token, node))
    }

    fn visit_if_expression(&mut self, token: Token, node: IfNode) -> Result<TypedAstNode, TypecheckerError> {
        let mut node = self.visit_if_node(false, node)?;

        let if_block_type = match &node.if_block.last() {
            None => Err(TypecheckerError::MissingIfExprBranch { if_token: token.clone(), is_if_branch: true }),
            Some(expr) => Ok(expr.get_type())
        }?;

        let typ = match &node.else_block {
            Some(else_block) => match else_block.last() {
                None => Err(TypecheckerError::MissingIfExprBranch { if_token: token.clone(), is_if_branch: false }),
                Some(expr) => {
                    let else_block_type = expr.get_type();
                    let mut if_block_last = node.if_block.last_mut().expect("MissingIfExprBranch should be emitted otherwise");
                    if !self.are_types_equivalent(&mut if_block_last, &else_block_type)? {
                        Err(TypecheckerError::IfExprBranchMismatch {
                            if_token: token.clone(),
                            if_type: if_block_type,
                            else_type: else_block_type,
                        })
                    } else {
                        Ok(if_block_last.get_type())
                    }
                }
            }
            None => Ok(Type::Option(Box::new(if_block_type)))
        }?;

        node.typ = typ.clone();

        Ok(TypedAstNode::IfExpression(token.clone(), node))
    }

    fn visit_match_statement(&mut self, token: Token, node: MatchNode) -> Result<TypedAstNode, TypecheckerError> {
        let node = self.visit_match_node(true, &token, node)?;
        Ok(TypedAstNode::MatchStatement(token, node))
    }

    fn visit_match_expression(&mut self, token: Token, node: MatchNode) -> Result<TypedAstNode, TypecheckerError> {
        let mut node = self.visit_match_node(false, &token, node)?;

        let mut typ = None;
        for (_, _, _, ref mut block, _) in &mut node.branches {
            if let Some(typ) = &typ {
                let mut block_last = block.last_mut().expect("A case should have a non-empty body");
                let block_typ = block_last.get_type();
                if !self.are_types_equivalent(&mut block_last, &typ)? {
                    return Err(TypecheckerError::MatchBranchMismatch {
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

        node.typ = typ.unwrap();
        Ok(TypedAstNode::MatchExpression(token, node))
    }

    fn visit_invocation(&mut self, token: Token, node: InvocationNode) -> Result<TypedAstNode, TypecheckerError> {
        let InvocationNode { target, args } = node;
        let target = self.visit(*target)?;
        let target_type = target.get_type();

        #[inline]
        fn verify_named_args_invocation(
            zelf: &mut Typechecker,
            invocation_target: Token,
            args: Vec<(/* arg_name: */ Option<Token>, /* arg_node: */ AstNode)>,
            arg_types: &Vec<(/* arg_name: */ String, /* arg_type: */ Type, /* is_optional: */ bool)>,
            generics: &mut HashMap<String, Type>,
        ) -> Result<Vec<(String, Option<TypedAstNode>)>, TypecheckerError> {
            // Check for duplicate named parameters
            let mut seen = HashSet::new();
            for (arg_name_tok, _) in args.iter() {
                match arg_name_tok {
                    None => continue,
                    Some(arg_name_tok) => {
                        let arg_name = Token::get_ident_name(arg_name_tok);
                        if seen.contains(&arg_name) {
                            return Err(TypecheckerError::DuplicateParamName { token: arg_name_tok.clone() });
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
            let expected_args = arg_types.iter()
                .map(|(arg_name, arg_type, is_optional)| (arg_name, (arg_type, is_optional)))
                .collect::<HashMap<&String, (&Type, &bool)>>();

            // Check for unexpected args
            for (arg_name, (arg_name_token, _)) in args.iter() {
                if !expected_args.contains_key(arg_name) {
                    // Note: this overlaps slightly with the IncorrectArity error; if named args are provided,
                    // this error will be raised for greater clarity, as opposed to IncorrectArity.
                    return Err(TypecheckerError::UnexpectedParamName { token: arg_name_token.clone() });
                }
            }

            let mut typed_args = Vec::new();

            // Ensure all expected args passed
            let mut missing_params = Vec::new();
            let mut args = args;
            for (arg_name, expected_arg_type, is_optional) in arg_types {
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
                Err(TypecheckerError::MissingRequiredParams { token: invocation_target, missing_params })
            } else {
                Ok(typed_args)
            }
        }

        #[inline]
        fn typecheck_arg(zelf: &mut Typechecker, arg: AstNode, expected_arg_type: &Type, generics: &mut HashMap<String, Type>) -> Result<TypedAstNode, TypecheckerError> {
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
                return Err(TypecheckerError::Mismatch { token: typed_arg.get_token().clone(), expected: expected_arg_type.clone(), actual: arg_type });
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

        match self.resolve_ref_type(&target_type) {
            t @ Type::Fn(_) | t @ Type::EnumVariant(_, _, false) => {
                let (arg_types, type_args, ret_type) = match t {
                    Type::Fn(FnType { arg_types, type_args, ret_type }) => (arg_types, type_args, ret_type),
                    Type::EnumVariant(enum_type, enum_variant_type, _) => {
                        if let Some(arg_types) = &enum_variant_type.arg_types {
                            let arg_types = arg_types.clone();
                            let type_args = vec![];
                            let ret_type = Box::new(Type::EnumVariant(enum_type, enum_variant_type, true));
                            (arg_types, type_args, ret_type)
                        } else {
                            return Err(TypecheckerError::InvalidInvocationTarget { token: target.get_token().clone(), target_type });
                        }
                    }
                    _ => unreachable!()
                };
                let num_named = args.iter().filter(|(arg, _)| arg.is_some()).count();
                if num_named != 0 && num_named != args.len() {
                    return Err(TypecheckerError::InvalidMixedParamType { token: target.get_token().clone() });
                }

                let mut generics = HashMap::<String, Type>::with_capacity(type_args.len());

                let typed_args = if num_named == 0 {
                    let num_req_args = arg_types.iter()
                        .take_while(|(_, _, is_optional)| !*is_optional)
                        .count();
                    if args.len() < num_req_args || args.len() > arg_types.len() {
                        return Err(TypecheckerError::IncorrectArity { token: target.get_token().clone(), expected: num_req_args, actual: args.len() });
                    }

                    let mut typed_args = Vec::new();
                    for (arg, expected) in args.into_iter().zip(arg_types.iter()) {
                        let (_, arg) = arg;
                        let (_, expected_arg_type, _) = expected;
                        let typed_arg = typecheck_arg(self, arg, expected_arg_type, &mut generics)?;
                        typed_args.push(Some(typed_arg));
                    }

                    // Make sure to fill in any omitted optional positional arguments
                    while typed_args.len() < arg_types.len() {
                        typed_args.push(None);
                    }
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

                Ok(TypedAstNode::Invocation(token, TypedInvocationNode { typ: ret_type, target: Box::new(target), args: typed_args }))
            }
            Type::Type(_, t, _) => match self.resolve_ref_type(&*t) {
                Type::Struct(struct_type) => {
                    let StructType { name, fields: expected_fields, type_args, .. } = &struct_type;
                    let target_token = target.get_token().clone();

                    let num_named = args.iter().filter(|(arg, _)| arg.is_some()).count();
                    if args.len() != num_named {
                        return Err(TypecheckerError::InvalidTypeFuncInvocation { token: target_token });
                    }

                    let mut generics = HashMap::new();
                    for (type_arg_name, type_arg_type) in type_args {
                        if type_arg_type.has_unbound_generic() { continue; }
                        generics.insert(type_arg_name.clone(), type_arg_type.clone());
                    }
                    let typed_args = verify_named_args_invocation(self, target_token, args, expected_fields, &mut generics)?;

                    let default_field_values = match self.get_type(&name).expect(&format!("Type {} should exist", name)) {
                        (_, Some(TypedAstNode::TypeDecl(_, TypedTypeDeclNode { fields, .. }))) => {
                            fields.iter()
                                .map(|(name, _, default_value)| {
                                    let name = Token::get_ident_name(name).clone();
                                    (name, default_value.clone())
                                })
                                .filter_map(|(name, default_value)| default_value.map(|v| (name, v)))
                                .collect::<HashMap<String, TypedAstNode>>()
                        }
                        _ => unreachable!()
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

                    let typ = match *t {
                        Type::Reference(name, _) => {
                            let mut generics_iter = generics.into_iter();
                            let mut struct_type_args_iter = type_args.iter();

                            let mut pairs = Vec::new();
                            loop {
                                match (generics_iter.next(), struct_type_args_iter.next()) {
                                    (Some((_, resolved_type_arg)), _) => pairs.push(resolved_type_arg),
                                    (_, Some((_, unbound_generic))) => pairs.push(unbound_generic.clone()),
                                    (None, None) => break
                                }
                            }
                            Type::Reference(name, pairs)
                        }
                        _ => unreachable!(format!("Unexpected type {:?} used here", t))
                    };

                    Ok(TypedAstNode::Instantiation(token, TypedInstantiationNode { typ, target: Box::new(target), fields }))
                }
                t @ Type::Int | t @ Type::Float | t @ Type::String | t @ Type::Bool => {
                    if args.len() != 1 {
                        return Err(TypecheckerError::IncorrectArity { token: target.get_token().clone(), expected: 1, actual: args.len() });
                    }
                    let mut args = args;
                    let (arg_name, node) = args.remove(0);
                    match arg_name {
                        Some(arg_name_tok) => {
                            return Err(TypecheckerError::UnexpectedParamName { token: arg_name_tok });
                        }
                        None => {
                            let mut typed_arg = self.visit(node)?;
                            let arg_type = typed_arg.get_type();
                            if !self.are_types_equivalent(&mut typed_arg, &t)? {
                                return Err(TypecheckerError::Mismatch { token: typed_arg.get_token().clone(), expected: t.clone(), actual: arg_type });
                            }

                            match typed_arg {
                                lit @ TypedAstNode::Literal(_, _) => Ok(lit),
                                _ => unreachable!()
                            }
                        }
                    }
                }
                typ @ _ => Err(TypecheckerError::InvalidInstantiation { token: target.get_token().clone(), typ }),
            }
            target_type @ _ => Err(TypecheckerError::InvalidInvocationTarget { token: target.get_token().clone(), target_type })
        }
    }

    fn visit_for_loop(&mut self, token: Token, node: ForLoopNode) -> Result<TypedAstNode, TypecheckerError> {
        let ForLoopNode { iteratee, index_ident, iterator, body } = node;
        let iterator = self.visit(*iterator)?;
        let iteratee_type = match iterator.get_type() {
            Type::Array(inner) => {
                inner
            }
            actual @ _ => {
                return Err(TypecheckerError::Mismatch {
                    token: iterator.get_token().clone(),
                    expected: Type::Array(Box::new(Type::Any)),
                    actual,
                });
            }
        };
        let iterator = Box::new(iterator);

        self.scopes.push(Scope::new(ScopeKind::Block)); // Wrap loop in block where intrinsic variables $idx and $iter will be stored
        let mut scope = Scope::new(ScopeKind::Loop);
        let iteratee_name = Token::get_ident_name(&iteratee).clone();
        scope.bindings.insert(iteratee_name, ScopeBinding(iteratee.clone(), *iteratee_type, false));
        if let Some(ident) = &index_ident {
            let ident_name = Token::get_ident_name(&ident).clone();
            scope.bindings.insert(ident_name, ScopeBinding(ident.clone(), Type::Int, false));
        }
        self.scopes.push(scope);

        let body: Result<Vec<_>, _> = body.into_iter()
            .map(|node| self.visit(node))
            .collect();
        let body = body?;
        self.scopes.pop();
        self.scopes.pop(); // Pop loop intrinsic-variables outer block

        Ok(TypedAstNode::ForLoop(token, TypedForLoopNode { iteratee, index_ident, iterator, body }))
    }

    fn visit_while_loop(&mut self, token: Token, node: WhileLoopNode) -> Result<TypedAstNode, TypecheckerError> {
        let WhileLoopNode { condition, condition_binding, body } = node;

        let condition = self.visit(*condition)?;
        let is_valid_cond_type = match condition.get_type() {
            Type::Option(_) | Type::Bool => true,
            _ => false
        };
        if !is_valid_cond_type {
            let token = condition.get_token().clone();
            return Err(TypecheckerError::InvalidIfConditionType { token, actual: condition.get_type() });
        }
        let condition = Box::new(condition);

        let has_condition_binding = condition_binding.is_some();
        if has_condition_binding {
            self.scopes.push(Scope::new(ScopeKind::Block)); // Wrap loop in block where condition_binding variable will be stored
        }

        let mut scope = Scope::new(ScopeKind::Loop);
        if let Some(ident) = &condition_binding {
            let ident_name = Token::get_ident_name(ident).clone();
            let binding_type = match condition.get_type() {
                Type::Bool => Type::Bool,
                Type::Option(inner) => {
                    let mut typ = *inner;
                    while let Type::Option(inner) = typ { typ = *inner };
                    typ
                }
                _ => unreachable!("No other types should be allowable as conditionals")
            };
            scope.bindings.insert(ident_name, ScopeBinding(ident.clone(), binding_type, false));
        }
        self.scopes.push(scope);

        let body: Result<Vec<_>, _> = body.into_iter()
            .map(|node| self.visit(node))
            .collect();
        let body = body?;
        self.scopes.pop();

        // Pop outer block, if created
        if has_condition_binding { self.scopes.pop(); }

        Ok(TypedAstNode::WhileLoop(token, TypedWhileLoopNode { condition, condition_binding, body }))
    }

    fn visit_break(&mut self, token: Token) -> Result<TypedAstNode, TypecheckerError> {
        let mut has_loop_parent = false;
        for Scope { kind, .. } in self.scopes.iter().rev() {
            match kind {
                ScopeKind::Loop => {
                    has_loop_parent = true;
                    break;
                }
                ScopeKind::Block => continue,
                _ => {
                    has_loop_parent = false;
                    break;
                }
            };
        };

        if has_loop_parent {
            Ok(TypedAstNode::Break(token))
        } else {
            Err(TypecheckerError::InvalidBreak(token))
        }
    }

    fn visit_accessor(&mut self, token: Token, node: AccessorNode) -> Result<TypedAstNode, TypecheckerError> {
        let AccessorNode { target, field, is_opt_safe } = node;
        let target = self.visit(*target)?;

        let mut target_type = target.get_type();
        let mut is_opt = false;
        if is_opt_safe {
            // Handle nested Option types (ie. String??? -> String?)
            while let Type::Option(inner_type) = target_type {
                target_type = *inner_type;
                is_opt = true;
            }
        }

        let (field_ident, ident_type_args) = if let AstNode::Identifier(field_ident, type_args) = *field {
            (field_ident, type_args)
        } else { unreachable!("The `field` field on AccessorNode must be an AstNode::Identifier"); };
        let field_name = Token::get_ident_name(&field_ident).clone();

        fn get_field_data(
            zelf: &Typechecker,
            target_type: &Type,
            field_name: &String,
            token: &Token,
        ) -> Result<(Option<(usize, Type)>, HashMap<String, Type>), TypecheckerError> {
            match zelf.resolve_ref_type(&target_type) {
                Type::Struct(StructType { fields, methods, type_args, .. }) => {
                    let generics = type_args.into_iter().collect::<HashMap<String, Type>>();

                    let num_fields = fields.len();
                    let field_data = fields.iter().enumerate()
                        .find_map(|(idx, (name, typ, _))| {
                            if field_name == name { Some((idx, typ.clone())) } else { None }
                        })
                        .or_else(|| {
                            methods.iter().enumerate().find_map(|(idx, (name, typ))| {
                                if field_name == name { Some((idx + num_fields, typ.clone())) } else { None }
                            })
                        });
                    Ok((field_data, generics))
                }
                Type::String => Ok((NativeString::get_field_or_method(&field_name), HashMap::new())),
                Type::Float => Ok((NativeFloat::get_field_or_method(&field_name), HashMap::new())),
                Type::Int => Ok((NativeInt::get_field_or_method(&field_name), HashMap::new())),
                Type::Array(inner_type) => {
                    let generics = vec![("T".to_string(), *inner_type.clone())].into_iter().collect::<HashMap<String, Type>>();
                    let field_data = NativeArray::get_field_or_method(&field_name);
                    Ok((field_data, generics))
                }
                Type::Type(_, typ, _) => match zelf.resolve_ref_type(&*typ) {
                    Type::Struct(StructType { static_fields, .. }) => {
                        let field_data = static_fields.iter().enumerate()
                            .find(|(_, (name, _, _))| field_name == name)
                            .map(|(idx, (_, typ, _))| (idx, typ.clone()));
                        Ok((field_data, HashMap::new()))
                    }
                    Type::Enum(enum_type) => {
                        let EnumType { name: enum_name, variants, static_fields, .. } = &enum_type;
                        let num_variants = variants.len();
                        let field_data = variants.iter().enumerate()
                            .find(|(_, EnumVariantType { name, .. })| field_name == name)
                            .map(|(idx, variant_type)| {
                                let enum_type_ref = Type::Reference(enum_name.clone(), vec![]);
                                (idx, Type::EnumVariant(Box::new(enum_type_ref), variant_type.clone(), false))
                            })
                            .or_else(|| {
                                static_fields.iter().enumerate().find_map(|(idx, (name, typ, _))| {
                                    if field_name == name { Some((idx + num_variants, typ.clone())) } else { None }
                                })
                            });
                        Ok((field_data, HashMap::new()))
                    }
                    _ => unimplemented!()
                }
                Type::Enum(enum_type) => {
                    let field_data = enum_type.methods.iter().enumerate().find_map(|(idx, (name, typ))| {
                        if field_name == name { Some((idx, typ.clone())) } else { None }
                    });
                    Ok((field_data, HashMap::new()))
                }
                Type::EnumVariant(enum_type_ref, EnumVariantType { variant_idx, .. }, is_constructed) => {
                    if let Type::Enum(enum_type) = zelf.resolve_ref_type(&*enum_type_ref) {
                        let variant_type = &enum_type.variants[variant_idx];
                        let field_data = if let Some(arg_types) = &variant_type.arg_types {
                            if is_constructed {
                                let num_methods = enum_type.methods.len();
                                arg_types.iter().enumerate()
                                    .find_map(|(idx, (arg_name, arg_type, _))| {
                                        if field_name == arg_name { Some((idx + num_methods, arg_type.clone())) } else { None }
                                    })
                            } else {
                                return Err(TypecheckerError::InvalidUninitializedEnumVariant { token: token.clone() });
                            }
                        } else { None };
                        match field_data {
                            Some(field_data) => Ok((Some(field_data), HashMap::new())),
                            None => get_field_data(&zelf, &Type::Enum(enum_type), field_name, token)
                        }
                    } else { unreachable!("The enum_type_ref shouldn't be anything other than an Enum type") }
                }
                Type::Union(opts) => {
                    let all_enums_or_variants = opts.iter().all(|o| match zelf.resolve_ref_type(o) {
                        Type::Enum(_) | Type::EnumVariant(_, _, _) => true,
                        _ => false
                    });
                    if !all_enums_or_variants {
                        return Ok((None, HashMap::new()));
                    }

                    let enum_types = opts.iter().filter_map(|o| {
                        match zelf.resolve_ref_type(o) {
                            Type::Enum(enum_type) => Some(enum_type),
                            Type::EnumVariant(enum_type, _, _) => match zelf.resolve_ref_type(&*enum_type) {
                                Type::Enum(enum_type) => Some(enum_type),
                                _ => unreachable!()
                            }
                            _ => None
                        }
                    }).collect::<Vec<_>>();
                    let enum_types = enum_types.into_iter().collect::<HashSet<_>>();
                    if enum_types.len() != 1 {
                        Ok((None, HashMap::new()))
                    } else {
                        let enum_type = enum_types.into_iter().next().unwrap();
                        let enum_type = Type::Enum(enum_type);
                        get_field_data(&zelf, &enum_type, field_name, token)
                    }
                }
                _ => Ok((None, HashMap::new()))
            }
        }

        let (field_data, mut generics) = get_field_data(&self, &target_type, &field_name, &token)?;
        let (field_idx, mut typ) = match field_data {
            Some((field_idx, typ)) => {
                if let Some(ident_type_args) = &ident_type_args {
                    if let Type::Fn(FnType { type_args: fn_type_args, .. }) = &typ {
                        if ident_type_args.len() != fn_type_args.len() {
                            return Err(TypecheckerError::InvalidTypeArgumentArity {
                                token: token.clone(),
                                actual_type: typ.clone(),
                                expected: fn_type_args.len(),
                                actual: ident_type_args.len(),
                            });
                        }
                        for (type_arg_name, type_arg_ident) in fn_type_args.iter().zip(ident_type_args) {
                            let type_arg_type = self.type_from_type_ident(type_arg_ident)?;
                            generics.insert(type_arg_name.clone(), type_arg_type);
                        }
                    }
                }
                (field_idx, Type::substitute_generics(&typ, &generics))
            }
            None => {
                return Err(TypecheckerError::UnknownMember { token: field_ident, target_type: target_type.clone() });
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

        Ok(TypedAstNode::Accessor(token, TypedAccessorNode { typ, target: Box::new(target), field_name, field_idx, is_opt_safe }))
    }

    fn visit_lambda(
        &mut self,
        token: Token,
        node: LambdaNode,
        args_override: Option<Vec<(Token, Type, Option<TypedAstNode>)>>,
    ) -> Result<TypedAstNode, TypecheckerError> {
        let orig_node = node.clone();
        let LambdaNode { args, body } = node;

        let orig_scopes = self.scopes.clone();
        self.scopes.push(Scope::new(ScopeKind::Lambda(token.clone())));

        let typed_args = if let Some(args) = args_override {
            for (arg_tok, arg_type, _) in &args {
                let arg_name = Token::get_ident_name(arg_tok);
                self.add_binding(&arg_name, arg_tok, arg_type, false);
            }
            args
        } else {
            self.visit_fn_args(args, false, true)?
        };

        let has_unknown = typed_args.iter().any(|(_, typ, _)| typ == &Type::Unknown);

        let arg_types = typed_args.iter()
            .map(|(ident, typ, default_value)| {
                (Token::get_ident_name(ident).clone(), typ.clone(), default_value.is_some())
            })
            .collect::<Vec<_>>();

        let typed_node = if has_unknown {
            let fn_type = Type::Fn(FnType { arg_types, type_args: vec![], ret_type: Box::new(Type::Unknown) });
            let orig_node = Some((orig_node, orig_scopes));
            let node = TypedLambdaNode { typ: fn_type, args: typed_args, typed_body: None, orig_node };
            TypedAstNode::Lambda(token, node)
        } else {
            let typed_body = self.visit_fn_body(body)?;
            let body_type = typed_body.last().map_or(Type::Unit, |node| node.get_type());

            let fn_type = Type::Fn(FnType { arg_types, type_args: vec![], ret_type: Box::new(body_type) });
            let node = TypedLambdaNode { typ: fn_type, args: typed_args, typed_body: Some(typed_body), orig_node: None };
            TypedAstNode::Lambda(token, node)
        };

        self.scopes.pop();

        Ok(typed_node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::lexer::tokens::Position;
    use crate::parser::ast::UnaryOp;

    type TestResult = Result<(), TypecheckerError>;

    fn typecheck(input: &str) -> Result<Vec<TypedAstNode>, TypecheckerError> {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();

        let (_, nodes) = super::typecheck(ast)?;
        Ok(nodes)
    }

    fn typecheck_get_typechecker(input: &str) -> (Typechecker, Vec<TypedAstNode>) {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();

        super::typecheck(ast).unwrap()
    }

    #[test]
    fn typecheck_literals() -> TestResult {
        let typed_ast = typecheck("1 2.34 \"hello\"")?;
        let expected = vec![
            int_literal!((1, 1), 1),
            float_literal!((1, 3), 2.34),
            string_literal!((1, 8), "hello")
        ];
        Ok(assert_eq!(expected, typed_ast))
    }

    #[test]
    fn typecheck_unary() -> TestResult {
        let typed_ast = typecheck("-1")?;
        let expected = vec![
            TypedAstNode::Unary(
                Token::Minus(Position::new(1, 1)),
                TypedUnaryNode {
                    typ: Type::Int,
                    op: UnaryOp::Minus,
                    expr: Box::new(int_literal!((1, 2), 1)),
                },
            ),
        ];
        assert_eq!(expected, typed_ast);

        let typed_ast = typecheck("-2.34")?;
        let expected = vec![
            TypedAstNode::Unary(
                Token::Minus(Position::new(1, 1)),
                TypedUnaryNode {
                    typ: Type::Float,
                    op: UnaryOp::Minus,
                    expr: Box::new(float_literal!((1, 2), 2.34)),
                },
            ),
        ];
        assert_eq!(expected, typed_ast);

        let typed_ast = typecheck("!true")?;
        let expected = vec![
            TypedAstNode::Unary(
                Token::Bang(Position::new(1, 1)),
                TypedUnaryNode {
                    typ: Type::Bool,
                    op: UnaryOp::Negate,
                    expr: Box::new(bool_literal!((1, 2), true)),
                },
            ),
        ];
        Ok(assert_eq!(expected, typed_ast))
    }

    #[test]
    fn typecheck_unary_failure() -> TestResult {
        let err = typecheck("-\"bad\"").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Minus(Position::new(1, 1)),
            expected: Type::Union(vec![Type::Int, Type::Float]),
            actual: Type::String,
        };
        assert_eq!(expected, err);

        let err = typecheck("-false").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Minus(Position::new(1, 1)),
            expected: Type::Union(vec![Type::Int, Type::Float]),
            actual: Type::Bool,
        };
        assert_eq!(expected, err);

        let err = typecheck("!4.5").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Bang(Position::new(1, 1)),
            expected: Type::Bool,
            actual: Type::Float,
        };
        assert_eq!(expected, err);

        let err = typecheck("!\"abc\"").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Bang(Position::new(1, 1)),
            expected: Type::Bool,
            actual: Type::String,
        };
        Ok(assert_eq!(expected, err))
    }

    #[test]
    fn typecheck_binary_arithmetic() -> TestResult {
        let typed_ast = typecheck("1 + 2")?;
        let expected = TypedAstNode::Binary(
            Token::Plus(Position::new(1, 3)),
            TypedBinaryNode {
                typ: Type::Int,
                left: Box::new(int_literal!((1, 1), 1)),
                op: BinaryOp::Add,
                right: Box::new(int_literal!((1, 5), 2)),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("1 % 2")?;
        let expected = TypedAstNode::Binary(
            Token::Percent(Position::new(1, 3)),
            TypedBinaryNode {
                typ: Type::Int,
                left: Box::new(int_literal!((1, 1), 1)),
                op: BinaryOp::Mod,
                right: Box::new(int_literal!((1, 5), 2)),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("1.1 % 2")?;
        let expected = TypedAstNode::Binary(
            Token::Percent(Position::new(1, 5)),
            TypedBinaryNode {
                typ: Type::Float,
                left: Box::new(float_literal!((1, 1), 1.1)),
                op: BinaryOp::Mod,
                right: Box::new(int_literal!((1, 7), 2)),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("1 % 2.3")?;
        let expected = TypedAstNode::Binary(
            Token::Percent(Position::new(1, 3)),
            TypedBinaryNode {
                typ: Type::Float,
                left: Box::new(int_literal!((1, 1), 1)),
                op: BinaryOp::Mod,
                right: Box::new(float_literal!((1, 5), 2.3)),
            },
        );
        Ok(assert_eq!(expected, typed_ast[0]))
    }

    #[test]
    fn typecheck_binary_arithmetic_nested() -> TestResult {
        let typed_ast = typecheck("1 + 2.3 - -4.5")?;
        let expected = TypedAstNode::Binary(
            Token::Minus(Position::new(1, 9)),
            TypedBinaryNode {
                typ: Type::Float,
                left: Box::new(
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(1, 3)),
                        TypedBinaryNode {
                            typ: Type::Float,
                            left: Box::new(int_literal!((1, 1), 1)),
                            op: BinaryOp::Add,
                            right: Box::new(float_literal!((1, 5), 2.3)),
                        },
                    )
                ),
                op: BinaryOp::Sub,
                right: Box::new(
                    TypedAstNode::Unary(
                        Token::Minus(Position::new(1, 11)),
                        TypedUnaryNode {
                            typ: Type::Float,
                            op: UnaryOp::Minus,
                            expr: Box::new(float_literal!((1, 12), 4.5)),
                        },
                    )
                ),
            },
        );
        Ok(assert_eq!(expected, typed_ast[0]))
    }

    #[test]
    fn typecheck_binary_str_concat() -> TestResult {
        let typed_ast = typecheck("\"hello \" + \"world\"")?;
        let expected = TypedAstNode::Binary(
            Token::Plus(Position::new(1, 10)),
            TypedBinaryNode {
                typ: Type::String,
                left: Box::new(string_literal!((1, 1), "hello ")),
                op: BinaryOp::Add,
                right: Box::new(string_literal!((1, 12), "world")),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("\"hello \" + 3")?;
        let expected = TypedAstNode::Binary(
            Token::Plus(Position::new(1, 10)),
            TypedBinaryNode {
                typ: Type::String,
                left: Box::new(string_literal!((1, 1), "hello ")),
                op: BinaryOp::Add,
                right: Box::new(int_literal!((1, 12), 3)),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("3.14 + \"world\"")?;
        let expected = TypedAstNode::Binary(
            Token::Plus(Position::new(1, 6)),
            TypedBinaryNode {
                typ: Type::String,
                left: Box::new(float_literal!((1, 1), 3.14)),
                op: BinaryOp::Add,
                right: Box::new(string_literal!((1, 8), "world")),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("false + \" world\"")?;
        let expected = TypedAstNode::Binary(
            Token::Plus(Position::new(1, 7)),
            TypedBinaryNode {
                typ: Type::String,
                left: Box::new(bool_literal!((1, 1), false)),
                op: BinaryOp::Add,
                right: Box::new(string_literal!((1, 9), " world")),
            },
        );
        Ok(assert_eq!(expected, typed_ast[0]))
    }

    #[test]
    fn typecheck_binary_assignment_operator() {
        assert!(typecheck("var a = 1\na += 3").is_ok());
        assert!(typecheck("var a = \"abc\"\na += 123").is_ok());
        assert!(typecheck("var a = \"abc\"\na += \"def\"").is_ok());

        assert!(typecheck("var a = 1\na -= 3").is_ok());
        assert!(typecheck("var a = 1\na *= 3").is_ok());
        assert!(typecheck("var a = 1.0\na /= 3").is_ok());
        assert!(typecheck("var a = 1\na %= 3").is_ok());
        assert!(typecheck("var a = true\na ||= false").is_ok());
        assert!(typecheck("var a = true\na &&= false").is_ok());

        assert!(typecheck("var a = None\na ?:= false").is_ok());
    }

    #[test]
    fn typecheck_binary_assignment_operator_errors() {
        assert!(typecheck("var a = 123\na += \"def\"").is_err());
        assert!(typecheck("val a = 1\na *= 3").is_err());

        assert!(typecheck("var a = true\na ||= 123").is_err());
        assert!(typecheck("var a = \"asdf\"\na &&= false").is_err());

        assert!(typecheck("true &&= false").is_err());
    }

    #[test]
    fn typecheck_binary_arithmetic_failures() {
        let cases = vec![
            ("3 - \"str\"", Token::Minus(Position::new(1, 3)), BinaryOp::Sub, Type::Int, Type::String),
            ("3.2 - \"str\"", Token::Minus(Position::new(1, 5)), BinaryOp::Sub, Type::Float, Type::String),
            ("3 * \"str\"", Token::Star(Position::new(1, 3)), BinaryOp::Mul, Type::Int, Type::String),
            ("3.2 * \"str\"", Token::Star(Position::new(1, 5)), BinaryOp::Mul, Type::Float, Type::String),
            ("3 / \"str\"", Token::Slash(Position::new(1, 3)), BinaryOp::Div, Type::Int, Type::String),
            ("3.2 / \"str\"", Token::Slash(Position::new(1, 5)), BinaryOp::Div, Type::Float, Type::String),
            ("3.2 % \"str\"", Token::Percent(Position::new(1, 5)), BinaryOp::Mod, Type::Float, Type::String),
            //
            ("\"str\" - 3", Token::Minus(Position::new(1, 7)), BinaryOp::Sub, Type::String, Type::Int),
            ("\"str\" - 3.2", Token::Minus(Position::new(1, 7)), BinaryOp::Sub, Type::String, Type::Float),
            ("\"str\" * 3", Token::Star(Position::new(1, 7)), BinaryOp::Mul, Type::String, Type::Int),
            ("\"str\" * 3.2", Token::Star(Position::new(1, 7)), BinaryOp::Mul, Type::String, Type::Float),
            ("\"str\" / 3", Token::Slash(Position::new(1, 7)), BinaryOp::Div, Type::String, Type::Int),
            ("\"str\" / 3.2", Token::Slash(Position::new(1, 7)), BinaryOp::Div, Type::String, Type::Float),
            ("\"str\" % 3.2", Token::Percent(Position::new(1, 7)), BinaryOp::Mod, Type::String, Type::Float),
            //
            ("true + 1", Token::Plus(Position::new(1, 6)), BinaryOp::Add, Type::Bool, Type::Int),
            ("true + 1.0", Token::Plus(Position::new(1, 6)), BinaryOp::Add, Type::Bool, Type::Float),
            ("true + false", Token::Plus(Position::new(1, 6)), BinaryOp::Add, Type::Bool, Type::Bool),
            ("true - 1", Token::Minus(Position::new(1, 6)), BinaryOp::Sub, Type::Bool, Type::Int),
            ("true - 1.0", Token::Minus(Position::new(1, 6)), BinaryOp::Sub, Type::Bool, Type::Float),
            ("true - \"str\"", Token::Minus(Position::new(1, 6)), BinaryOp::Sub, Type::Bool, Type::String),
            ("true - false", Token::Minus(Position::new(1, 6)), BinaryOp::Sub, Type::Bool, Type::Bool),
            ("true * 1", Token::Star(Position::new(1, 6)), BinaryOp::Mul, Type::Bool, Type::Int),
            ("true * 1.0", Token::Star(Position::new(1, 6)), BinaryOp::Mul, Type::Bool, Type::Float),
            ("true * \"str\"", Token::Star(Position::new(1, 6)), BinaryOp::Mul, Type::Bool, Type::String),
            ("true * false", Token::Star(Position::new(1, 6)), BinaryOp::Mul, Type::Bool, Type::Bool),
            ("true / 1", Token::Slash(Position::new(1, 6)), BinaryOp::Div, Type::Bool, Type::Int),
            ("true / 1.0", Token::Slash(Position::new(1, 6)), BinaryOp::Div, Type::Bool, Type::Float),
            ("true / \"str\"", Token::Slash(Position::new(1, 6)), BinaryOp::Div, Type::Bool, Type::String),
            ("true / false", Token::Slash(Position::new(1, 6)), BinaryOp::Div, Type::Bool, Type::Bool),
            ("true % false", Token::Percent(Position::new(1, 6)), BinaryOp::Mod, Type::Bool, Type::Bool),
            //
            ("[1, 2][0] + 1", Token::Plus(Position::new(1, 11)), BinaryOp::Add, Type::Option(Box::new(Type::Int)), Type::Int),
            ("[0][1] + [2][3]", Token::Plus(Position::new(1, 8)), BinaryOp::Add, Type::Option(Box::new(Type::Int)), Type::Option(Box::new(Type::Int))),
            ("[\"a\", \"b\"][0] - [\"c\"][0]", Token::Minus(Position::new(1, 15)), BinaryOp::Sub, Type::Option(Box::new(Type::String)), Type::Option(Box::new(Type::String))),
        ];

        for (input, token, op, ltype, rtype) in cases {
            let expected = TypecheckerError::InvalidOperator { token, op, ltype, rtype };
            let msg = format!("Typechecking `{}` should result in the error {:?}", input, expected);

            let err = typecheck(input).expect_err(&*msg);
            assert_eq!(expected, err, "{}", msg);
        }
    }

    #[test]
    fn typecheck_binary_boolean() -> TestResult {
        let typed_ast = typecheck("true && true || false")?;

        let expected = TypedAstNode::IfExpression(Token::If(Position::new(1, 14)), TypedIfNode {
            typ: Type::Bool,
            condition: Box::new(
                TypedAstNode::IfExpression(Token::If(Position::new(1, 6)), TypedIfNode {
                    typ: Type::Bool,
                    condition: Box::new(bool_literal!((1, 1), true)),
                    condition_binding: None,
                    if_block: vec![
                        bool_literal!((1, 9), true),
                    ],
                    else_block: Some(vec![
                        bool_literal!((1, 6), false), // <- pos is derived from && position
                    ]),
                }),
            ),
            condition_binding: None,
            if_block: vec![
                bool_literal!((1, 14), true), // <- pos is derived from || position
            ],
            else_block: Some(vec![
                bool_literal!((1, 17), false),
            ]),
        });
        Ok(assert_eq!(expected, typed_ast[0]))
    }

    #[test]
    fn typecheck_binary_boolean_errors() {
        let cases = vec![
            ("true && 1", Token::And(Position::new(1, 6)), BinaryOp::And, Type::Bool, Type::Int),
            ("true && 3.14", Token::And(Position::new(1, 6)), BinaryOp::And, Type::Bool, Type::Float),
            ("true && \"str\"", Token::And(Position::new(1, 6)), BinaryOp::And, Type::Bool, Type::String),
            ("false && 1", Token::And(Position::new(1, 7)), BinaryOp::And, Type::Bool, Type::Int),
            ("false && 3.14", Token::And(Position::new(1, 7)), BinaryOp::And, Type::Bool, Type::Float),
            ("false && \"str\"", Token::And(Position::new(1, 7)), BinaryOp::And, Type::Bool, Type::String),
            //
            ("true || 1", Token::Or(Position::new(1, 6)), BinaryOp::Or, Type::Bool, Type::Int),
            ("true || 3.14", Token::Or(Position::new(1, 6)), BinaryOp::Or, Type::Bool, Type::Float),
            ("true || \"str\"", Token::Or(Position::new(1, 6)), BinaryOp::Or, Type::Bool, Type::String),
            ("false || 1", Token::Or(Position::new(1, 7)), BinaryOp::Or, Type::Bool, Type::Int),
            ("false || 3.14", Token::Or(Position::new(1, 7)), BinaryOp::Or, Type::Bool, Type::Float),
            ("false || \"str\"", Token::Or(Position::new(1, 7)), BinaryOp::Or, Type::Bool, Type::String),
        ];

        for (input, token, op, ltype, rtype) in cases {
            let expected = TypecheckerError::InvalidOperator { token, op, ltype, rtype };
            let msg = format!("Typechecking `{}` should result in the error {:?}", input, expected);

            let err = typecheck(input).expect_err(&*msg);
            assert_eq!(expected, err, "{}", msg);
        }
    }

    #[test]
    fn typecheck_binary_comparisons() -> TestResult {
        let cases: Vec<(&str, Box<dyn Fn(Position) -> Token>, BinaryOp)> = vec![
            ("1 <  2", Box::new(Token::LT), BinaryOp::Lt),
            ("1 <= 2", Box::new(Token::LTE), BinaryOp::Lte),
            ("1 >  2", Box::new(Token::GT), BinaryOp::Gt),
            ("1 >= 2", Box::new(Token::GTE), BinaryOp::Gte),
            ("1 != 2", Box::new(Token::Neq), BinaryOp::Neq),
            ("1 == 2", Box::new(Token::Eq), BinaryOp::Eq),
        ];
        for (input, token, op) in cases {
            let typed_ast = typecheck(input)?;
            let expected = TypedAstNode::Binary(
                token(Position::new(1, 3)),
                TypedBinaryNode {
                    typ: Type::Bool,
                    left: Box::new(int_literal!((1, 1), 1)),
                    op,
                    right: Box::new(int_literal!((1, 6), 2)),
                },
            );
            assert_eq!(expected, typed_ast[0]);
        };

        let cases: Vec<(&str, Box<dyn Fn(Position) -> Token>, BinaryOp)> = vec![
            ("\"abc\" <  \"def\"", Box::new(Token::LT), BinaryOp::Lt),
            ("\"abc\" <= \"def\"", Box::new(Token::LTE), BinaryOp::Lte),
            ("\"abc\" >  \"def\"", Box::new(Token::GT), BinaryOp::Gt),
            ("\"abc\" >= \"def\"", Box::new(Token::GTE), BinaryOp::Gte),
            ("\"abc\" == \"def\"", Box::new(Token::Eq), BinaryOp::Eq),
            ("\"abc\" != \"def\"", Box::new(Token::Neq), BinaryOp::Neq),
        ];
        for (input, token, op) in cases {
            let typed_ast = typecheck(input)?;
            let expected = TypedAstNode::Binary(
                token(Position::new(1, 7)),
                TypedBinaryNode {
                    typ: Type::Bool,
                    left: Box::new(string_literal!((1, 1), "abc")),
                    op,
                    right: Box::new(string_literal!((1, 10), "def")),
                },
            );
            assert_eq!(expected, typed_ast[0]);
        }

        let cases: Vec<(&str, Box<dyn Fn(Position) -> Token>, BinaryOp)> = vec![
            ("\"abc\" == 3", Box::new(Token::Eq), BinaryOp::Eq),
            ("\"abc\" != 3", Box::new(Token::Neq), BinaryOp::Neq),
        ];
        for (input, token, op) in cases {
            let typed_ast = typecheck(input)?;
            let expected = TypedAstNode::Binary(
                token(Position::new(1, 7)),
                TypedBinaryNode {
                    typ: Type::Bool,
                    left: Box::new(string_literal!((1, 1), "abc")),
                    op,
                    right: Box::new(int_literal!((1, 10), 3)),
                },
            );
            assert_eq!(expected, typed_ast[0]);
        }

        Ok(())
    }

    #[test]
    fn typecheck_binary_comparison_errors() {
        let cases = vec![
            ("\"str\" <  3", Token::LT(Position::new(1, 7)), BinaryOp::Lt, Type::String, Type::Int),
            ("\"str\" <  3.0", Token::LT(Position::new(1, 7)), BinaryOp::Lt, Type::String, Type::Float),
            ("\"str\" <= 3", Token::LTE(Position::new(1, 7)), BinaryOp::Lte, Type::String, Type::Int),
            ("\"str\" <= 3.0", Token::LTE(Position::new(1, 7)), BinaryOp::Lte, Type::String, Type::Float),
            ("\"str\" >  3", Token::GT(Position::new(1, 7)), BinaryOp::Gt, Type::String, Type::Int),
            ("\"str\" >  3.0", Token::GT(Position::new(1, 7)), BinaryOp::Gt, Type::String, Type::Float),
            ("\"str\" >= 3", Token::GTE(Position::new(1, 7)), BinaryOp::Gte, Type::String, Type::Int),
            ("\"str\" >= 3.0", Token::GTE(Position::new(1, 7)), BinaryOp::Gte, Type::String, Type::Float),
            //
            ("[1, 2] < 3", Token::LT(Position::new(1, 8)), BinaryOp::Lt, Type::Array(Box::new(Type::Int)), Type::Int),
            ("[1, 2] <= 3", Token::LTE(Position::new(1, 8)), BinaryOp::Lte, Type::Array(Box::new(Type::Int)), Type::Int),
            ("[1, 2] > 3", Token::GT(Position::new(1, 8)), BinaryOp::Gt, Type::Array(Box::new(Type::Int)), Type::Int),
            ("[1, 2] >= 3", Token::GTE(Position::new(1, 8)), BinaryOp::Gte, Type::Array(Box::new(Type::Int)), Type::Int),
        ];

        for (input, token, op, ltype, rtype) in cases {
            let expected = TypecheckerError::InvalidOperator { token, op, ltype, rtype };
            let msg = format!("Typechecking `{}` should result in the error {:?}", input, expected);

            let err = typecheck(input).expect_err(&*msg);
            assert_eq!(expected, err, "{}", msg);
        }
    }

    #[test]
    fn typecheck_binary_coalesce_operation() -> TestResult {
        let cases = vec![
            ("[1][0] ?: 2", Type::Int),
            ("[[0, 1]][0] ?: [1, 2]", Type::Array(Box::new(Type::Int))),
            ("[[0, 1][0]][0] ?: [1, 2][1]", Type::Option(Box::new(Type::Int))),
            ("[][0] ?: 0", Type::Int),
            ("None ?: 0", Type::Int),
        ];

        for (input, expected_type) in cases {
            let typed_ast = typecheck(input)?;
            assert_eq!(expected_type, typed_ast[0].get_type());
        };

        Ok(())
    }

    #[test]
    fn typecheck_binary_coalesce_errors() {
        let cases = vec![
            ("[1][0] ?: \"a\"", Token::String(Position::new(1, 11), "a".to_string()), Type::Int, Type::String),
            ("[1][0] ?: 1.0", Token::Float(Position::new(1, 11), 1.0), Type::Int, Type::Float),
        ];
        for (input, token, expected, actual) in cases {
            let expected = TypecheckerError::Mismatch { token, expected, actual };
            let msg = format!("Typechecking `{}` should result in the error {:?}", input, expected);

            let err = typecheck(input).expect_err(&*msg);
            assert_eq!(expected, err, "{}", msg);
        }

        let input = "\"abc\" ?: 12";
        let err = typecheck(input).expect_err("Error expected");
        let expected = TypecheckerError::InvalidOperator {
            token: Token::Elvis(Position::new(1, 7)),
            op: BinaryOp::Coalesce,
            ltype: Type::String,
            rtype: Type::Int,
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn typecheck_grouped() -> TestResult {
        let typed_ast = typecheck("(1 + 2)")?;
        let expected = TypedAstNode::Grouped(
            Token::LParen(Position::new(1, 1), false),
            TypedGroupedNode {
                typ: Type::Int,
                expr: Box::new(
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(1, 4)),
                        TypedBinaryNode {
                            typ: Type::Int,
                            left: Box::new(int_literal!((1, 2), 1)),
                            op: BinaryOp::Add,
                            right: Box::new(int_literal!((1, 6), 2)),
                        },
                    )
                ),
            },
        );
        Ok(assert_eq!(expected, typed_ast[0]))
    }

    #[test]
    fn typecheck_array_empty() -> TestResult {
        let typed_ast = typecheck("[]")?;
        let expected = TypedAstNode::Array(
            Token::LBrack(Position::new(1, 1), false),
            TypedArrayNode { typ: Type::Array(Box::new(Type::Unknown)), items: vec![] },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_array_homogeneous() -> TestResult {
        let typed_ast = typecheck("[1, 2, 3]")?;
        let expected = TypedAstNode::Array(
            Token::LBrack(Position::new(1, 1), false),
            TypedArrayNode {
                typ: Type::Array(Box::new(Type::Int)),
                items: vec![
                    Box::new(int_literal!((1, 2), 1)),
                    Box::new(int_literal!((1, 5), 2)),
                    Box::new(int_literal!((1, 8), 3))
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("[\"a\", \"b\"]")?;
        let expected = TypedAstNode::Array(
            Token::LBrack(Position::new(1, 1), false),
            TypedArrayNode {
                typ: Type::Array(Box::new(Type::String)),
                items: vec![
                    Box::new(string_literal!((1, 2), "a")),
                    Box::new(string_literal!((1, 7), "b"))
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("[true, false]")?;
        let expected = TypedAstNode::Array(
            Token::LBrack(Position::new(1, 1), false),
            TypedArrayNode {
                typ: Type::Array(Box::new(Type::Bool)),
                items: vec![
                    Box::new(bool_literal!((1, 2), true)),
                    Box::new(bool_literal!((1, 8), false))
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_array_nested() -> TestResult {
        let typed_ast = typecheck("[[1, 2], [3, 4]]")?;
        let expected_type = Type::Array(Box::new(Type::Array(Box::new(Type::Int))));
        assert_eq!(expected_type, typed_ast[0].get_type());

        // TODO: Handle edge cases, like [[1, 2.3], [3.4, 5]], which should be (Int | Float)[][]

        Ok(())
    }

    #[test]
    fn typecheck_map_empty() -> TestResult {
        let typed_ast = typecheck("{}")?;
        let expected = TypedAstNode::Map(
            Token::LBrace(Position::new(1, 1)),
            TypedMapNode {
                typ: Type::Map(vec![], Some(Box::new(Type::Any))),
                items: vec![],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_map() -> TestResult {
        // Homogeneous (simple)
        let typed_ast = typecheck("{ a: 1, b: 2 }")?;
        let expected = TypedAstNode::Map(
            Token::LBrace(Position::new(1, 1)),
            TypedMapNode {
                typ: Type::Map(vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)], Some(Box::new(Type::Int))),
                items: vec![
                    (ident_token!((1, 3), "a"), int_literal!((1, 6), 1)),
                    (ident_token!((1, 9), "b"), int_literal!((1, 12), 2))
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        // Homogeneous (complex)
        let typed_ast = typecheck("{ a: { c: true }, b: { c: false } }")?;
        let nested_map_type = Type::Map(vec![("c".to_string(), Type::Bool)], Some(Box::new(Type::Bool)));
        let expected = TypedAstNode::Map(
            Token::LBrace(Position::new(1, 1)),
            TypedMapNode {
                typ: Type::Map(
                    vec![("a".to_string(), nested_map_type.clone()), ("b".to_string(), nested_map_type.clone())],
                    Some(Box::new(nested_map_type.clone())),
                ),
                items: vec![
                    (ident_token!((1, 3), "a"), TypedAstNode::Map(
                        Token::LBrace(Position::new(1, 6)),
                        TypedMapNode {
                            typ: nested_map_type.clone(),
                            items: vec![(ident_token!((1, 8), "c"), bool_literal!((1, 11), true))],
                        },
                    )),
                    (ident_token!((1, 19), "b"), TypedAstNode::Map(
                        Token::LBrace(Position::new(1, 22)),
                        TypedMapNode {
                            typ: nested_map_type.clone(),
                            items: vec![(ident_token!((1, 24), "c"), bool_literal!((1, 27), false))],
                        },
                    ))
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        // Non-homogeneous
        let typed_ast = typecheck("{ a: 1, b: true, c: \"hello\" }")?;
        let expected_type = Type::Map(
            vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Bool), ("c".to_string(), Type::String)],
            None,
        );
        assert_eq!(expected_type, typed_ast[0].get_type());

        Ok(())
    }

    #[test]
    fn typecheck_binding_decl() -> TestResult {
        let (typechecker, typed_ast) = typecheck_get_typechecker("val abc = 123");
        let expected = TypedAstNode::BindingDecl(
            Token::Val(Position::new(1, 1)),
            TypedBindingDeclNode {
                is_mutable: false,
                ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                expr: Some(Box::new(int_literal!((1, 11), 123))),
                scope_depth: 0,
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let (binding, scope_depth) = typechecker.get_binding("abc").unwrap();
        let expected_binding = ScopeBinding(
            Token::Ident(Position::new(1, 5), "abc".to_string()),
            Type::Int,
            false,
        );
        assert_eq!(&expected_binding, binding);
        assert_eq!(0, scope_depth);

        let (typechecker, typed_ast) = typecheck_get_typechecker("var abc: Int");
        let expected = TypedAstNode::BindingDecl(
            Token::Var(Position::new(1, 1)),
            TypedBindingDeclNode {
                is_mutable: true,
                ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                expr: None,
                scope_depth: 0,
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let (binding, scope_depth) = typechecker.get_binding("abc").unwrap();
        let expected_binding = ScopeBinding(
            Token::Ident(Position::new(1, 5), "abc".to_string()),
            Type::Int,
            true,
        );
        assert_eq!(&expected_binding, binding);
        assert_eq!(0, scope_depth);

        assert!(typecheck("val arr: Int[] = []").is_ok());

        Ok(())
    }

    #[test]
    fn typecheck_type_annotation() {
        let cases = vec![
            // Simple cases
            ("var abc: Int", Type::Int),
            ("var abc: Int[]", Type::Array(Box::new(Type::Int))),
            ("var abc: Int?", Type::Option(Box::new(Type::Int))),
            // Complex cases
            ("var abc: Int[][]", Type::Array(Box::new(Type::Array(Box::new(Type::Int))))),
            ("var abc: Int?[]", Type::Array(Box::new(Type::Option(Box::new(Type::Int))))),
            ("var abc: Int[]?", Type::Option(Box::new(Type::Array(Box::new(Type::Int))))),
        ];

        for (input, expected_binding_type) in cases {
            let (typechecker, _) = typecheck_get_typechecker(input);
            let (ScopeBinding(_, typ, _), scope_depth) = typechecker.get_binding("abc").unwrap();
            assert_eq!(expected_binding_type, *typ);
            assert_eq!(0, scope_depth);
        }
    }

    #[test]
    fn typecheck_binding_decl_errors() {
        let err = typecheck("val abc").unwrap_err();
        let expected = TypecheckerError::MissingRequiredAssignment {
            ident: Token::Ident(Position::new(1, 5), "abc".to_string())
        };
        assert_eq!(expected, err);

        let err = typecheck("val abc = 1\nval abc = 2").unwrap_err();
        let expected = TypecheckerError::DuplicateBinding {
            ident: Token::Ident(Position::new(2, 5), "abc".to_string()),
            orig_ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
        };
        assert_eq!(expected, err);

        let err = typecheck("val abc: Int[] = 1").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Int(Position::new(1, 18), 1),
            expected: Type::Array(Box::new(Type::Int)),
            actual: Type::Int,
        };
        assert_eq!(expected, err);

        let err = typecheck("val abc: NonExistentType = true").unwrap_err();
        let expected = TypecheckerError::UnknownType {
            type_ident: Token::Ident(Position::new(1, 10), "NonExistentType".to_string())
        };
        assert_eq!(expected, err);

        let err = typecheck("var abc: NonExistentType").unwrap_err();
        let expected = TypecheckerError::UnknownType {
            type_ident: Token::Ident(Position::new(1, 10), "NonExistentType".to_string())
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn typecheck_function_decl() -> TestResult {
        let (typechecker, typed_ast) = typecheck_get_typechecker("func abc() = 123");
        let expected = TypedAstNode::FunctionDecl(
            Token::Func(Position::new(1, 1)),
            TypedFunctionDeclNode {
                name: Token::Ident(Position::new(1, 6), "abc".to_string()),
                args: vec![],
                ret_type: Type::Int,
                body: vec![
                    int_literal!((1, 14), 123)
                ],
                scope_depth: 0,
                is_recursive: false,
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let (ScopeBinding(_, typ, _), scope_depth) = typechecker.get_binding("abc")
            .expect("The function abc should be defined");
        let expected_type = Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::Int) });
        assert_eq!(&expected_type, typ);
        assert_eq!(0, scope_depth);

        let typed_ast = typecheck("func abc(a: Int) = a + 1")?;
        let expected = TypedAstNode::FunctionDecl(
            Token::Func(Position::new(1, 1)),
            TypedFunctionDeclNode {
                name: Token::Ident(Position::new(1, 6), "abc".to_string()),
                args: vec![(ident_token!((1, 10), "a"), Type::Int, None)],
                ret_type: Type::Int,
                body: vec![
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(1, 22)),
                        TypedBinaryNode {
                            typ: Type::Int,
                            left: Box::new(identifier!((1, 20), "a", Type::Int, 1)),
                            op: BinaryOp::Add,
                            right: Box::new(int_literal!((1, 24), 1)),
                        })
                ],
                scope_depth: 0,
                is_recursive: false,
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let (typechecker, typed_ast) = typecheck_get_typechecker("func abc() { val a = [1, 2] a }");
        let expected = TypedAstNode::FunctionDecl(
            Token::Func(Position::new(1, 1)),
            TypedFunctionDeclNode {
                name: Token::Ident(Position::new(1, 6), "abc".to_string()),
                args: vec![],
                ret_type: Type::Array(Box::new(Type::Int)),
                body: vec![
                    TypedAstNode::BindingDecl(
                        Token::Val(Position::new(1, 14)),
                        TypedBindingDeclNode {
                            ident: Token::Ident(Position::new(1, 18), "a".to_string()),
                            is_mutable: false,
                            expr: Some(Box::new(
                                TypedAstNode::Array(
                                    Token::LBrack(Position::new(1, 22), false),
                                    TypedArrayNode {
                                        typ: Type::Array(Box::new(Type::Int)),
                                        items: vec![
                                            Box::new(int_literal!((1, 23), 1)),
                                            Box::new(int_literal!((1, 26), 2)),
                                        ],
                                    },
                                )
                            )),
                            scope_depth: 1,
                        },
                    ),
                    identifier!((1, 29), "a", Type::Array(Box::new(Type::Int)), 1)
                ],
                scope_depth: 0,
                is_recursive: false,
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let (ScopeBinding(_, typ, _), scope_depth) = typechecker.get_binding("abc")
            .expect("The function abc should be defined");
        let expected_type = Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::Array(Box::new(Type::Int))) });
        assert_eq!(&expected_type, typ);
        assert_eq!(0, scope_depth);

        let typed_ast = typecheck("func abc(): Int = 123")?;
        let ret_type = match typed_ast.first().unwrap() {
            TypedAstNode::FunctionDecl(_, TypedFunctionDeclNode { ret_type, .. }) => ret_type,
            _ => panic!("Node must be a FunctionDecl")
        };
        assert_eq!(&Type::Int, ret_type);

        // Test that bindings assigned to functions have the proper type
        let (typechecker, _) = typecheck_get_typechecker("func abc(a: Int): Bool = a == 1\nval def = abc");
        let (ScopeBinding(_, typ, _), _) = typechecker.get_binding("def").unwrap();
        assert_eq!(&Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::Int, false)], type_args: vec![], ret_type: Box::new(Type::Bool) }), typ);

        Ok(())
    }

    #[test]
    fn typecheck_function_decl_args() -> TestResult {
        let typed_ast = typecheck("func abc(a: Int) = 123")?;
        let args = match typed_ast.first().unwrap() {
            TypedAstNode::FunctionDecl(_, TypedFunctionDeclNode { args, .. }) => args,
            _ => panic!("Node must be a FunctionDecl")
        };
        let expected = vec![
            (ident_token!((1, 10), "a"), Type::Int, None)
        ];
        assert_eq!(&expected, args);

        let typed_ast = typecheck("func abc(a: Int, b: Bool?, c: Int[]) = 123")?;
        let args = match typed_ast.first().unwrap() {
            TypedAstNode::FunctionDecl(_, TypedFunctionDeclNode { args, .. }) => args,
            _ => panic!("Node must be a FunctionDecl")
        };
        let expected = vec![
            (ident_token!((1, 10), "a"), Type::Int, None),
            (ident_token!((1, 18), "b"), Type::Option(Box::new(Type::Bool)), None),
            (ident_token!((1, 28), "c"), Type::Array(Box::new(Type::Int)), None),
        ];
        assert_eq!(&expected, args);

        let typed_ast = typecheck("func abc(a: Int = 1, b = [1, 2, 3]) = 123")?;
        let args = match typed_ast.first().unwrap() {
            TypedAstNode::FunctionDecl(_, TypedFunctionDeclNode { args, .. }) => args,
            _ => panic!("Node must be a FunctionDecl")
        };
        let expected = vec![
            (ident_token!((1, 10), "a"), Type::Int, Some(int_literal!((1, 19), 1))),
            (ident_token!((1, 22), "b"), Type::Array(Box::new(Type::Int)), Some(
                TypedAstNode::Array(
                    Token::LBrack(Position::new(1, 26), false),
                    TypedArrayNode {
                        typ: Type::Array(Box::new(Type::Int)),
                        items: vec![
                            Box::new(int_literal!((1, 27), 1)),
                            Box::new(int_literal!((1, 30), 2)),
                            Box::new(int_literal!((1, 33), 3)),
                        ],
                    },
                )
            )),
        ];
        assert_eq!(&expected, args);

        // A function with default-valued arguments beyond those required should still be acceptable
        let typed_ast = typecheck("\
          func call(fn: (Int) => Int, value: Int) = fn(value)\n\
          func incr(v: Int, incBy = 3) = v + incBy\n\
          call(incr, 21)\n\
        ");
        assert!(typed_ast.is_ok());

        Ok(())
    }

    #[test]
    fn typecheck_function_decl_args_error() {
        let error = typecheck("func abc(a: Int, a: Bool) = 123").unwrap_err();
        let expected = TypecheckerError::DuplicateBinding {
            orig_ident: ident_token!((1, 10), "a"),
            ident: ident_token!((1, 18), "a"),
        };
        assert_eq!(expected, error);

        let error = typecheck("func abc(a: Int, b: Bool = \"hello\") = 123").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(1, 28), "hello".to_string()),
            expected: Type::Bool,
            actual: Type::String,
        };
        assert_eq!(expected, error);

        let error = typecheck("func abc(a: Int, b = 1, c: Int) = 123").unwrap_err();
        let expected = TypecheckerError::InvalidRequiredArgPosition(ident_token!((1, 25), "c"));
        assert_eq!(expected, error);

        let error = typecheck("func abc(self, a: Int, b = 1, c: Int) = 123").unwrap_err();
        let expected = TypecheckerError::InvalidSelfParam { token: Token::Self_(Position::new(1, 10)) };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_function_decl_errors() {
        let err = typecheck("func println() = 123").unwrap_err();
        let expected = TypecheckerError::DuplicateBinding {
            ident: Token::Ident(Position::new(1, 6), "println".to_string()),
            orig_ident: Token::Ident(Position::new(0, 0), "println".to_string()),
        };
        assert_eq!(expected, err);

        let err = typecheck("func myFunc() = 123 + true").unwrap_err();
        let expected = TypecheckerError::InvalidOperator {
            token: Token::Plus(Position::new(1, 21)),
            ltype: Type::Int,
            op: BinaryOp::Add,
            rtype: Type::Bool,
        };
        assert_eq!(expected, err);

        let error = typecheck("func abc(a: Int): Bool = 123").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Int(Position::new(1, 26), 123),
            expected: Type::Bool,
            actual: Type::Int,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_function_decl_recursion() {
        let (typechecker, typed_ast) = typecheck_get_typechecker("func abc(): Int {\nabc()\n}");
        let (ScopeBinding(_, typ, _), _) = typechecker.get_binding("abc")
            .expect("The function abc should be defined");
        let expected_type = Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::Int) });
        assert_eq!(&expected_type, typ);

        let is_recursive = match typed_ast.first().unwrap() {
            TypedAstNode::FunctionDecl(_, TypedFunctionDeclNode { is_recursive, .. }) => is_recursive,
            _ => panic!("Node must be a FunctionDecl")
        };
        assert_eq!(&true, is_recursive);
    }

    #[test]
    fn typecheck_function_decl_recursion_error() {
        let error = typecheck("func abc() {\nabc()\n}").unwrap_err();
        let expected = TypecheckerError::RecursiveRefWithoutReturnType {
            orig_token: ident_token!((1, 6), "abc"),
            token: ident_token!((2, 1), "abc"),
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_function_decl_inner_function() -> TestResult {
        let typed_ast = typecheck("func a(): Int {\nfunc b() { 1 }\n b()\n}")?;
        let func = match typed_ast.first().unwrap() {
            TypedAstNode::FunctionDecl(_, func) => func,
            _ => panic!("Node must be a FunctionDecl")
        };
        assert_eq!(Type::Int, func.ret_type);

        Ok(())
    }

    #[test]
    fn typecheck_function_decl_inner_function_err() {
        let error = typecheck("func a(): Int {\nfunc b() { 1 }\n b + 1\n}").unwrap_err();
        let expected = TypecheckerError::InvalidOperator {
            token: Token::Plus(Position::new(3, 4)),
            op: BinaryOp::Add,
            ltype: Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::Int) }),
            rtype: Type::Int,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_function_decl_generics() -> TestResult {
        let typed_ast = typecheck("\
          func abc<T>(t: T): T { t }\n\
          val a = abc(123)\n\
          a\
        ")?;
        let typ = typed_ast.last().unwrap().get_type();
        assert_eq!(Type::Int, typ);

        let typed_ast = typecheck("\
          func abc<T, U>(t: T, u: U): U { u }\n\
          val a = abc(123, \"asdf\")\n\
          a\
        ")?;
        let typ = typed_ast.last().unwrap().get_type();
        assert_eq!(Type::String, typ);

        let typed_ast = typecheck("\
          func map<T, U>(arr: T[], fn: (T) => U): U[] { [] }\n\
          val a = map([0, 1, 2], x => x + \"!\")\n\
          a\
        ")?;
        let typ = typed_ast.last().unwrap().get_type();
        assert_eq!(Type::Array(Box::new(Type::String)), typ);

        let typed_ast = typecheck("\
          func map<T, U>(arr: T[], fn: (T) => U): U[] { [] }\n\
          val a = map([[0, 1], [2, 3]], a => a.length)\n\
          a\
        ")?;
        let typ = typed_ast.last().unwrap().get_type();
        assert_eq!(Type::Array(Box::new(Type::Int)), typ);

        let typed_ast = typecheck("\
          func map<T, U>(arr: T[], fn: (T) => U): (T) => U[] { t => [] }\n\
          val a = map([[0, 1], [2, 3]], a => a.length)\n\
          a\
        ")?;
        let typ = typed_ast.last().unwrap().get_type();
        let expected = Type::Fn(FnType {
            arg_types: vec![("_".to_string(), Type::Array(Box::new(Type::Int)), false)],
            type_args: vec![],
            ret_type: Box::new(Type::Array(Box::new(Type::Int))),
        });
        assert_eq!(expected, typ);

        // Verify generic resolution works for named arguments too
        let typed_ast = typecheck("\
          func map<T, U>(arr: T[], fn: (T) => U): (T) => U[] { t => [] }\n\
          val a = map(fn: a => a.length, arr: [[0, 1], [2, 3]])\n\
          a\
        ")?;
        let typ = typed_ast.last().unwrap().get_type();
        let expected = Type::Fn(FnType {
            arg_types: vec![("_".to_string(), Type::Array(Box::new(Type::Int)), false)],
            type_args: vec![],
            ret_type: Box::new(Type::Array(Box::new(Type::Int))),
        });
        assert_eq!(expected, typ);

        // Verify generic resolution works for union types
        let typed_ast = typecheck("\
          func cos<T>(angle: T | Float) = angle\n\
          cos(1.23)\
        ")?;
        let typ = typed_ast.last().unwrap().get_type();
        let expected = Type::Union(vec![Type::Float, Type::Float]); // <- Redundant, I know
        assert_eq!(expected, typ);
        let typed_ast = typecheck("\
          func cos<T>(angle: T | Float) = angle\n\
          cos(12)\
        ")?;
        let typ = typed_ast.last().unwrap().get_type();
        let expected = Type::Union(vec![Type::Int, Type::Float]);
        assert_eq!(expected, typ);

        Ok(())
    }

    #[test]
    fn typecheck_function_decl_generics_errors() {
        let error = typecheck("\
          func map<T, U, T>(arr: T[], fn: (T) => U): U[] { [] }\n\
        ").unwrap_err();
        let expected = TypecheckerError::DuplicateTypeArgument {
            ident: ident_token!((1, 16), "T"),
            orig_ident: ident_token!((1, 10), "T"),
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          func map<T, U, V>(arr: T[], fn: (T) => U): V[] { [] }\n\
        ").unwrap_err();
        let expected = TypecheckerError::UnboundGeneric(ident_token!((1, 44), "V"), "V".to_string());
        assert_eq!(expected, error);

        let error = typecheck("\
          func map<T, U>(arr: T[], fn: (T) => U): (T) => U[] { () => [] }\n\
        ").unwrap_err();
        let expected = TypecheckerError::IncorrectArity {
            token: Token::Arrow(Position::new(1, 57)),
            expected: 1,
            actual: 0,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_type_decl() -> TestResult {
        let (typechecker, typed_ast) = typecheck_get_typechecker("type Person { name: String }");
        let expected = TypedAstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypedTypeDeclNode {
                name: ident_token!((1, 6), "Person"),
                fields: vec![
                    (ident_token!((1, 15), "name"), Type::String, None)
                ],
                static_fields: vec![],
                methods: vec![],
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let (typ, _) = typechecker.get_type(&"Person".to_string()).unwrap();
        assert_eq!(Type::Reference("Person".to_string(), vec![]), typ);
        let expected_type = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![("name".to_string(), Type::String, false)],
            static_fields: vec![],
            methods: vec![],
        });
        assert_eq!(expected_type, typechecker.referencable_types["Person"]);

        let (typechecker, typed_ast) = typecheck_get_typechecker("type Person { name: String, age: Int = 0 }");
        let expected = TypedAstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypedTypeDeclNode {
                name: ident_token!((1, 6), "Person"),
                fields: vec![
                    (ident_token!((1, 15), "name"), Type::String, None),
                    (ident_token!((1, 29), "age"), Type::Int, Some(int_literal!((1, 40), 0)))
                ],
                static_fields: vec![],
                methods: vec![],
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let (typ, _) = typechecker.get_type(&"Person".to_string()).unwrap();
        assert_eq!(Type::Reference("Person".to_string(), vec![]), typ);
        let expected_type = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![
                ("name".to_string(), Type::String, false),
                ("age".to_string(), Type::Int, true),
            ],
            static_fields: vec![],
            methods: vec![],
        });
        assert_eq!(expected_type, typechecker.referencable_types["Person"]);

        Ok(())
    }

    #[test]
    fn typecheck_type_decl_self_referencing() -> TestResult {
        let (typechecker, typed_ast) = typecheck_get_typechecker("\
          type Node {\n\
            value: Int\n\
            next: Node? = None\n\
          }\n\
          val node = Node(value: 1, next: Node(value: 2))\n\
          node\n\
        ");

        let expected = identifier!((6, 1), "node", Type::Reference("Node".to_string(), vec![]), 0);
        assert_eq!(expected, typed_ast[2]);
        let expected_type = Type::Struct(StructType {
            name: "Node".to_string(),
            type_args: vec![],
            fields: vec![
                ("value".to_string(), Type::Int, false),
                (
                    "next".to_string(),
                    Type::Option(Box::new(Type::Reference("Node".to_string(), vec![]))),
                    true
                ),
            ],
            static_fields: vec![],
            methods: vec![],
        });
        assert_eq!(expected_type, typechecker.referencable_types["Node"]);

        Ok(())
    }

    #[test]
    fn typecheck_type_decl_errors() {
        let error = typecheck("type Person { name: Huh }").unwrap_err();
        let expected = TypecheckerError::UnknownType { type_ident: ident_token!((1, 21), "Huh") };
        assert_eq!(expected, error);

        let error = typecheck("type Person { age: Int, age: String }").unwrap_err();
        let expected = TypecheckerError::DuplicateField { orig_ident: ident_token!((1, 15), "age"), ident: ident_token!((1, 25), "age"), orig_is_field: true, orig_is_enum_variant: false };
        assert_eq!(expected, error);

        let error = typecheck("type Person { age: String = true }").unwrap_err();
        let expected = TypecheckerError::Mismatch { token: Token::Bool(Position::new(1, 29), true), expected: Type::String, actual: Type::Bool };
        assert_eq!(expected, error);

        let error = typecheck("\
          func abc() {\n\
            type Person { age: String }\n\
          }\
        ").unwrap_err();
        let expected = TypecheckerError::InvalidTypeDeclDepth { token: Token::Type(Position::new(2, 1)) };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_type_decl_methods() -> TestResult {
        let input = "\
          type Person {\n\
            name: String\n\
            func getName(self): String = self.name\n\
            func getName2(self): String = self.getName()\n\
          }
        ";
        let (typechecker, typed_ast) = typecheck_get_typechecker(input);
        let person_type = Type::Reference("Person".to_string(), vec![]);
        let expected = TypedAstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypedTypeDeclNode {
                name: ident_token!((1, 6), "Person"),
                fields: vec![
                    (ident_token!((2, 1), "name"), Type::String, None),
                ],
                static_fields: vec![],
                methods: vec![
                    (
                        "getName".to_string(),
                        TypedAstNode::FunctionDecl(
                            Token::Func(Position::new(3, 1)),
                            TypedFunctionDeclNode {
                                name: Token::Ident(Position::new(3, 6), "getName".to_string()),
                                args: vec![
                                    (Token::Self_(Position { line: 3, col: 14 }), person_type.clone(), None)
                                ],
                                ret_type: Type::String,
                                body: vec![
                                    TypedAstNode::Accessor(
                                        Token::Dot(Position::new(3, 34)),
                                        TypedAccessorNode {
                                            typ: Type::String,
                                            target: Box::new(TypedAstNode::Identifier(
                                                Token::Self_(Position::new(3, 30)),
                                                TypedIdentifierNode {
                                                    typ: person_type.clone(),
                                                    name: "self".to_string(),
                                                    scope_depth: 2,
                                                    is_mutable: false,
                                                },
                                            )),
                                            field_name: "name".to_string(),
                                            field_idx: 0,
                                            is_opt_safe: false,
                                        },
                                    )
                                ],
                                scope_depth: 1,
                                is_recursive: false,
                            },
                        ),
                    ),
                    (
                        "getName2".to_string(),
                        TypedAstNode::FunctionDecl(
                            Token::Func(Position::new(4, 1)),
                            TypedFunctionDeclNode {
                                name: Token::Ident(Position::new(4, 6), "getName2".to_string()),
                                args: vec![
                                    (Token::Self_(Position { line: 4, col: 15 }), person_type.clone(), None)
                                ],
                                ret_type: Type::String,
                                body: vec![
                                    TypedAstNode::Invocation(
                                        Token::LParen(Position::new(4, 43), false),
                                        TypedInvocationNode {
                                            typ: Type::String,
                                            args: vec![],
                                            target: Box::new(
                                                TypedAstNode::Accessor(
                                                    Token::Dot(Position::new(4, 35)),
                                                    TypedAccessorNode {
                                                        typ: Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String) }),
                                                        target: Box::new(TypedAstNode::Identifier(
                                                            Token::Self_(Position::new(4, 31)),
                                                            TypedIdentifierNode {
                                                                typ: person_type.clone(),
                                                                name: "self".to_string(),
                                                                scope_depth: 2,
                                                                is_mutable: false,
                                                            },
                                                        )),
                                                        field_name: "getName".to_string(),
                                                        field_idx: 1,
                                                        is_opt_safe: false,
                                                    },
                                                )
                                            ),
                                        },
                                    )
                                ],
                                scope_depth: 1,
                                is_recursive: false,
                            },
                        ),
                    ),
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let expected_type = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![
                ("name".to_string(), Type::String, false)
            ],
            static_fields: vec![],
            methods: vec![
                ("getName".to_string(), Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String) })),
                ("getName2".to_string(), Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String) }))
            ],
        });
        Ok(assert_eq!(expected_type, typechecker.referencable_types["Person"]))
    }

    #[test]
    fn typecheck_type_decl_static_methods() -> TestResult {
        let input = "\
          type Person {\n\
            name: String\n\
            func getName(): String = \"hello\"\n\
          }
        ";
        let (typechecker, typed_ast) = typecheck_get_typechecker(input);
        let expected = TypedAstNode::TypeDecl(
            Token::Type(Position::new(1, 1)),
            TypedTypeDeclNode {
                name: ident_token!((1, 6), "Person"),
                fields: vec![
                    (ident_token!((2, 1), "name"), Type::String, None),
                ],
                static_fields: vec![
                    (
                        Token::Func(Position::new(3, 1)),
                        Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String) }),
                        Some(TypedAstNode::FunctionDecl(
                            Token::Func(Position::new(3, 1)),
                            TypedFunctionDeclNode {
                                name: Token::Ident(Position::new(3, 6), "getName".to_string()),
                                args: vec![],
                                ret_type: Type::String,
                                body: vec![
                                    string_literal!((3, 26), "hello")
                                ],
                                scope_depth: 1,
                                is_recursive: false,
                            },
                        )),
                    ),
                ],
                methods: vec![],
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let expected_type = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![
                ("name".to_string(), Type::String, false)
            ],
            static_fields: vec![
                ("getName".to_string(), Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String) }), true),
            ],
            methods: vec![],
        });
        Ok(assert_eq!(expected_type, typechecker.referencable_types["Person"]))
    }

    #[test]
    fn typecheck_type_decl_methods_errors() {
        let input = "\
          type Person {\n\
            func hello(self): String = \"hello\"\n\
            func hello(self): String = \"hello\"\n\
          }
        ";
        let error = typecheck(input).unwrap_err();
        let expected = TypecheckerError::DuplicateField {
            orig_ident: ident_token!((2, 6), "hello"),
            ident: ident_token!((3, 6), "hello"),
            orig_is_field: false,
            orig_is_enum_variant: false,
        };
        assert_eq!(expected, error);

        let input = "\
          type Person {\n\
            func hello(self, self): String = \"hello\"\n\
          }
        ";
        let error = typecheck(input).unwrap_err();
        let expected = TypecheckerError::InvalidSelfParamPosition { token: Token::Self_(Position::new(2, 18)) };
        assert_eq!(expected, error);

        let input = "\
          type Person {\n\
            func hello(self) = \"hello\"\n\
          }
        ";
        let error = typecheck(input).unwrap_err();
        let expected = TypecheckerError::MissingRequiredTypeAnnotation { token: ident_token!((2, 6), "hello") };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_type_decl_generics() -> TestResult {
        let input = "type List<T> { items: T[] }";
        let (typechecker, _) = typecheck_get_typechecker(input);
        let expected_type = Type::Struct(StructType {
            name: "List".to_string(),
            type_args: vec![("T".to_string(), Type::Generic("T".to_string()))],
            fields: vec![("items".to_string(), Type::Array(Box::new(Type::Generic("T".to_string()))), false)],
            static_fields: vec![],
            methods: vec![],
        });
        assert_eq!(expected_type, typechecker.referencable_types["List"]);

        let input = r#"
          type List<T> { items: T[] }
          val l = List(items: [1, 2, 3])
          l
        "#;
        let typed_ast = typecheck(input)?;
        let actual_type = typed_ast.last().unwrap().get_type();
        let expected_type = Type::Reference("List".to_string(), vec![Type::Int]);
        assert_eq!(expected_type, actual_type);

        let input = r#"
          type List<T> { items: T[] }
          val l: List<Int> = List(items: [1, 2, 3])
        "#;
        let res = typecheck(input);
        assert!(res.is_ok());

        let input = r#"
          type List<T> { items: T[] }
          val l = List<Int>(items: [1, 2, 3])
        "#;
        let res = typecheck(input);
        assert!(res.is_ok());

        let input = r#"
          type List<T> { items: T[] }
          val l: List<Int> = List(items: [])
        "#;
        let res = typecheck(input);
        assert!(res.is_ok());

        let input = r#"
          type List<T> { items: T[] }
          val l = List<Int>(items: [])
        "#;
        let res = typecheck(input);
        assert!(res.is_ok());

        Ok(())
    }

    #[test]
    fn typecheck_type_decl_generics_errors() {
        let input = "type List<T, U, T> { items: T[] }";
        let error = typecheck(input).unwrap_err();
        let expected = TypecheckerError::DuplicateTypeArgument {
            ident: ident_token!((1, 17), "T"),
            orig_ident: ident_token!((1, 11), "T"),
        };
        assert_eq!(expected, error);

        let input = "\
          type List<T> { items: T[] }\n\
          val l = List(items: [])\
        ";
        let error = typecheck(input).unwrap_err();
        let expected = TypecheckerError::UnboundGeneric(ident_token!((2, 5), "l"), "T".to_string());
        assert_eq!(expected, error);

        let input = "\
          type List<T> { items: T[] }\n\
          val l: List<String> = List(items: [1, 2, 3])\
        ";
        let error = typecheck(input).unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::LParen(Position::new(2, 27), false),
            expected: Type::Reference("List".to_string(), vec![Type::String]),
            actual: Type::Reference("List".to_string(), vec![Type::Int]),
        };
        assert_eq!(expected, error);

        let input = "\
          type List<T> { items: T[] }\n\
          val l = List<String>(items: [1, 2, 3])\
        ";
        let error = typecheck(input).unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::LBrack(Position::new(2, 29), false),
            expected: Type::Array(Box::new(Type::String)),
            actual: Type::Array(Box::new(Type::Int)),
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_type_decl_generics_fields_and_methods() -> TestResult {
        let type_def = r#"
          type List<T> {
            items: T[] = []
            func push(self, item: T): List<T> {
              self.items.push(item)
              self
            }
            func map<U>(self, fn: (T) => U): List<U> {
              List(items: self.items.map(fn))
            }
            func concat(self, other: List<T>): List<T> {
              List(items: self.items.concat(other.items))
            }
            func reduce<U>(self, initialValue: U, fn: (U, T) => U): U {
              var acc = initialValue
              for item in self.items { acc = fn(acc, item) }
              acc
            }
          }
        "#;

        let input = format!(r#"{}
          val l: List<Int> = List(items: [1, 2, 3])
          val items: Int[] = l.items
        "#, type_def);
        let res = typecheck(&input);
        assert!(res.is_ok());

        let input = format!(r#"{}
          val l: List<Int> = List(items: [1, 2, 3])
          l.push(4)
        "#, type_def);
        let res = typecheck(&input);
        assert!(res.is_ok());

        let input = format!(r#"{}
          val l: List<String> = List(items: [])
          l.push("abc")
        "#, type_def);
        let res = typecheck(&input);
        assert!(res.is_ok());

        let input = format!(r#"{}
          val l: List<String> = List()
          l.push("abc")
        "#, type_def);
        let res = typecheck(&input);
        assert!(res.is_ok());

        let input = format!(r#"{}
          val l: List<String> = List(items: [])
          val lengths: List<Int> = l.map(s => s.length)
        "#, type_def);
        let res = typecheck(&input);
        assert!(res.is_ok());

        let input = format!(r#"{}
          List(items: [1, 2, 3]).concat(List(items: [4, 5, 6]))
        "#, type_def);
        let res = typecheck(&input);
        assert!(res.is_ok());

        let input = format!(r#"{}
          List(items: [1, 2, 3]).reduce<String[]>([], (acc, i) => {{
            acc.push(i + "!")
            acc
          }})
        "#, type_def);
        let res = typecheck(&input);
        assert!(res.is_ok());

        Ok(())
    }

    #[test]
    fn typecheck_type_decl_generics_fields_and_methods_errors() {
        let type_def = "\
          type List<T> {\n\
            items: T[]\n\
            func push(self, item: T): T[] {\n\
              self.items.push(item)\n\
              self.items\n\
            }\n\
            func reduce<U>(self, initialValue: U, fn: (U, T) => U): U {\n\
              initialValue\n\
            }\n\
          }\
        ";

        let input = format!("{}\n\
          val l: List<Int> = List(items: [1, 2, 3])\n\
          l.items.push(\"abcd\")\
        ", type_def);
        let error = typecheck(&input).unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(12, 14), "abcd".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, error);

        let input = format!("{}\n\
          val l: List<Int> = List(items: [1, 2, 3])\n\
          l.push(\"abcd\")\
        ", type_def);
        let error = typecheck(&input).unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(12, 8), "abcd".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, error);

        let input = format!("{}\n\
          val l = List(items: [1, 2, 3])\n\
          l.reduce([], (acc, i) => acc.concat([i]))\
        ", type_def);
        let error = typecheck(&input).unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Arrow(Position::new(12, 23)),
            expected: Type::Array(Box::new(Type::Unknown)),
            actual: Type::Array(Box::new(Type::Unknown)),
        };
        assert_eq!(expected, error);

        let input = format!("{}\n\
          val l = List(items: [1, 2, 3])\n\
          l.reduce<Int>(\"\", (acc, i) => acc + i)\
        ", type_def);
        let error = typecheck(&input).unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(12, 15), "".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, error);

        let list_struct_type = Type::Struct(StructType {
            name: "List".to_string(),
            type_args: vec![("T".to_string(), Type::Generic("T".to_string()))],
            fields: vec![("items".to_string(), Type::Array(Box::new(Type::Generic("T".to_string()))), false)],
            static_fields: vec![],
            methods: vec![],
        });
        let input = "\
          type List<T> { items: T[] }\n\
          val l: List = List(items: [1, 2, 3])\
        ";
        let error = typecheck(&input).unwrap_err();
        let expected = TypecheckerError::InvalidTypeArgumentArity {
            token: ident_token!((2, 8), "List"),
            actual_type: list_struct_type.clone(),
            expected: 1,
            actual: 0,
        };
        assert_eq!(expected, error);

        let input = "\
          type List<T> { items: T[] }\n\
          val l: List<Int, Int> = List(items: [1, 2, 3])\
        ";
        let error = typecheck(&input).unwrap_err();
        let expected = TypecheckerError::InvalidTypeArgumentArity {
            token: ident_token!((2, 8), "List"),
            actual_type: list_struct_type,
            expected: 1,
            actual: 2,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_enum_decl() -> TestResult {
        let typed_ast = typecheck("enum Temp { Hot, Cold }")?;
        let expected = TypedAstNode::EnumDecl(
            Token::Enum(Position::new(1, 1)),
            TypedEnumDeclNode {
                name: ident_token!((1, 6), "Temp"),
                static_fields: vec![],
                methods: vec![],
                variants: vec![
                    (
                        ident_token!((1, 13), "Hot"),
                        Type::EnumVariant(Box::new(Type::Reference("Temp".to_string(), vec![])), EnumVariantType { name: "Hot".to_string(), variant_idx: 0, arg_types: None }, false),
                        EnumVariantKind::Basic
                    ),
                    (
                        ident_token!((1, 18), "Cold"),
                        Type::EnumVariant(Box::new(Type::Reference("Temp".to_string(), vec![])), EnumVariantType { name: "Cold".to_string(), variant_idx: 1, arg_types: None }, false),
                        EnumVariantKind::Basic
                    ),
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("\
          enum AngleMode { Radians(rads: Float), Degrees(degs: Float) }\
        ")?;
        let expected = TypedAstNode::EnumDecl(
            Token::Enum(Position::new(1, 1)),
            TypedEnumDeclNode {
                name: ident_token!((1, 6), "AngleMode"),
                static_fields: vec![],
                methods: vec![],
                variants: vec![
                    (
                        ident_token!((1, 18), "Radians"),
                        Type::EnumVariant(
                            Box::new(Type::Reference("AngleMode".to_string(), vec![])),
                            EnumVariantType { name: "Radians".to_string(), variant_idx: 0, arg_types: Some(vec![("rads".to_string(), Type::Float, false)]) },
                            false,
                        ),
                        EnumVariantKind::Constructor(vec![
                            (ident_token!((1, 26), "rads"), Type::Float, None)
                        ])
                    ),
                    (
                        ident_token!((1, 40), "Degrees"),
                        Type::EnumVariant(
                            Box::new(Type::Reference("AngleMode".to_string(), vec![])),
                            EnumVariantType { name: "Degrees".to_string(), variant_idx: 1, arg_types: Some(vec![("degs".to_string(), Type::Float, false)]) },
                            false,
                        ),
                        EnumVariantKind::Constructor(vec![
                            (ident_token!((1, 48), "degs"), Type::Float, None)
                        ])
                    ),
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        // Verify that constructor variants can have default arg values that are themselves enum variants
        let typed_ast = typecheck("\
          enum Color { Red, Darken(base: Color = Color.Red, amount: Float = 10.0) }\
        ")?;
        let expected = TypedAstNode::EnumDecl(
            Token::Enum(Position::new(1, 1)),
            TypedEnumDeclNode {
                name: ident_token!((1, 6), "Color"),
                methods: vec![],
                static_fields: vec![],
                variants: vec![
                    (
                        ident_token!((1, 14), "Red"),
                        Type::EnumVariant(
                            Box::new(Type::Reference("Color".to_string(), vec![])),
                            EnumVariantType { name: "Red".to_string(), variant_idx: 0, arg_types: None },
                            false,
                        ),
                        EnumVariantKind::Basic
                    ),
                    (
                        ident_token!((1, 19), "Darken"),
                        Type::EnumVariant(
                            Box::new(Type::Reference("Color".to_string(), vec![])),
                            EnumVariantType {
                                name: "Darken".to_string(),
                                variant_idx: 1,
                                arg_types: Some(vec![
                                    ("base".to_string(), Type::Reference("Color".to_string(), vec![]), true),
                                    ("amount".to_string(), Type::Float, true),
                                ]),
                            },
                            false,
                        ),
                        EnumVariantKind::Constructor(vec![
                            (
                                ident_token!((1, 26), "base"),
                                Type::Reference("Color".to_string(), vec![]),
                                Some(TypedAstNode::Accessor(
                                    Token::Dot(Position::new(1, 45)),
                                    TypedAccessorNode {
                                        typ: Type::EnumVariant(
                                            Box::new(Type::Reference("Color".to_string(), vec![])),
                                            EnumVariantType { name: "Red".to_string(), variant_idx: 0, arg_types: None },
                                            false,
                                        ),
                                        target: Box::new(identifier!(
                                            (1, 40),
                                            "Color",
                                            Type::Type(
                                                "Color".to_string(),
                                                Box::new(Type::Reference("Color".to_string(), vec![])),
                                                true,
                                            ),
                                            0
                                        )),
                                        field_idx: 0,
                                        field_name: "Red".to_string(),
                                        is_opt_safe: false,
                                    },
                                ))
                            ),
                            (
                                ident_token!((1, 51), "amount"),
                                Type::Float,
                                Some(float_literal!((1, 67), 10.0))
                            )
                        ])
                    ),
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_enum_decl_errors() {
        let error = typecheck("enum Temp { Hot, Hot }").unwrap_err();
        let expected = TypecheckerError::DuplicateField {
            ident: ident_token!((1, 18), "Hot"),
            orig_ident: ident_token!((1, 13), "Hot"),
            orig_is_field: false,
            orig_is_enum_variant: true,
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          enum Temp { Hot, Cold }\n\
          enum Temp { Hot, Cold, Tepid }\
        ").unwrap_err();
        let expected = TypecheckerError::DuplicateType {
            ident: ident_token!((2, 6), "Temp"),
            orig_ident: Some(Token::Enum(Position::new(1, 1))),
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          enum Temp { Hot(temp: Int, temp: Int), Cold }\n\
        ").unwrap_err();
        let expected = TypecheckerError::DuplicateBinding {
            ident: ident_token!((1, 28), "temp"),
            orig_ident: ident_token!((1, 17), "temp"),
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          enum Temp { Hot(self), Cold }\n\
        ").unwrap_err();
        let expected = TypecheckerError::InvalidSelfParam {
            token: Token::Self_(Position::new(1, 17))
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_enum_decl_variants() {
        let is_ok = typecheck("\
          enum Scale { Fahrenheit, Celsius }\n\
          func isBoiling(amount: Float, scale: Scale) = amount\n\
          isBoiling(212.0, Scale.Fahrenheit)\
        ").is_ok();
        assert!(is_ok);

        let is_ok = typecheck("\
          enum Scale { Fahrenheit(degs: Float), Celsius(degs: Float) }\n\
          func isBoiling(scale: Scale) = true\n\
          isBoiling(Scale.Fahrenheit(degs: 212.0))\
        ").is_ok();
        assert!(is_ok);
    }

    #[test]
    fn typecheck_enum_decl_variants_errors() {
        let error = typecheck("\
          enum Scale { Fahrenheit, Celsius }\n\
          func isBoiling(amount: Float, scale: Scale = 123) = amount\
        ").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Int(Position::new(2, 46), 123),
            actual: Type::Int,
            expected: Type::Reference("Scale".to_string(), vec![]),
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          enum Scale { Fahrenheit(degs: Float), Celsius(degs: Float) }\n\
          func isBoiling(scale: Scale) = true\n\
          isBoiling(Scale.Fahrenheit(degs: \"212.0\"))\
        ").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(3, 34), "212.0".to_string()),
            actual: Type::String,
            expected: Type::Float,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_enum_decl_methods() {
        let is_ok = typecheck("\
          enum Color {\n\
            Red\n\
            Green\n\
            func hexCode(self): String = \"0xffffff\"\n\
          }\n\
          val hex: String = Color.Red.hexCode()\
        ").is_ok();
        assert!(is_ok);

        // Test static methods
        let is_ok = typecheck("\
          enum Color {\n\
            Red\n\
            Green\n\
            func gray(): String = \"0x777\"\n\
          }\n\
          val hex: String = Color.gray()\
        ").is_ok();
        assert!(is_ok);
    }

    #[test]
    fn typecheck_enum_decl_methods_errors() {
        let error = typecheck("\
          enum Color {\n\
            Red\n\
            Green\n\
            func hexCode(self): String = \"0xffffff\"\n\
          }\n\
          Color.Red.hex()\
        ").unwrap_err();
        let expected = TypecheckerError::UnknownMember {
            token: ident_token!((6, 11), "hex"),
            target_type: Type::EnumVariant(
                Box::new(Type::Reference("Color".to_string(), vec![])),
                EnumVariantType { name: "Red".to_string(), variant_idx: 0, arg_types: None },
                false,
            ),
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          enum Color {\n\
            Red\n\
            Green\n\
            func black(self): String = \"0x000000\"\n\
          }\n\
          Color.white()\
        ").unwrap_err();
        let expected = TypecheckerError::UnknownMember {
            token: ident_token!((6, 7), "white"),
            target_type: Type::Type(
                "Color".to_string(),
                Box::new(Type::Reference("Color".to_string(), vec![])),
                true,
            ),
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          enum Color {\n\
            Red\n\
            func Red(): String = \"0xFF0000\"\n\
          }\
        ").unwrap_err();
        let expected = TypecheckerError::DuplicateField {
            ident: ident_token!((3, 6), "Red"),
            orig_ident: ident_token!((2, 1), "Red"),
            orig_is_field: false,
            orig_is_enum_variant: true,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_ident() -> TestResult {
        let typed_ast = typecheck("val abc = 123\nabc")?;
        let expected = vec![
            TypedAstNode::BindingDecl(
                Token::Val(Position::new(1, 1)),
                TypedBindingDeclNode {
                    is_mutable: false,
                    ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                    expr: Some(Box::new(int_literal!((1, 11), 123))),
                    scope_depth: 0,
                },
            ),
            identifier!((2, 1), "abc", Type::Int, 0)
        ];
        assert_eq!(expected, typed_ast);
        Ok(())
    }

    #[test]
    fn typecheck_ident_errors() {
        let err = typecheck("abc").unwrap_err();
        let expected = TypecheckerError::UnknownIdentifier {
            ident: Token::Ident(Position::new(1, 1), "abc".to_string())
        };
        assert_eq!(expected, err);

        let err = typecheck("var abc\nabc").unwrap_err();
        let expected = TypecheckerError::UnannotatedUninitialized {
            ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
            is_mutable: true,
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn typecheck_assignment_identifier() -> TestResult {
        let typed_ast = typecheck("var abc = 123\nabc = 456")?;
        let expected = vec![
            TypedAstNode::BindingDecl(
                Token::Var(Position::new(1, 1)),
                TypedBindingDeclNode {
                    is_mutable: true,
                    ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
                    expr: Some(Box::new(int_literal!((1, 11), 123))),
                    scope_depth: 0,
                },
            ),
            TypedAstNode::Assignment(
                Token::Assign(Position::new(2, 5)),
                TypedAssignmentNode {
                    kind: AssignmentTargetKind::Identifier,
                    typ: Type::Int,
                    target: Box::new(identifier_mut!((2, 1), "abc", Type::Int, 0)),
                    expr: Box::new(int_literal!((2, 7), 456)),
                },
            )
        ];
        assert_eq!(expected, typed_ast);
        Ok(())
    }

    #[test]
    fn typecheck_assignment_indexing() -> TestResult {
        let typed_ast = typecheck("var abc = [1, 2]\nabc[0] = 2")?;
        let expected = TypedAstNode::Assignment(
            Token::Assign(Position::new(2, 8)),
            TypedAssignmentNode {
                kind: AssignmentTargetKind::ArrayIndex,
                typ: Type::Int,
                target: Box::new(TypedAstNode::Indexing(
                    Token::LBrack(Position::new(2, 4), false),
                    TypedIndexingNode {
                        typ: Type::Option(Box::new(Type::Int)),
                        target: Box::new(identifier_mut!((2, 1), "abc", Type::Array(Box::new(Type::Int)), 0)),
                        index: IndexingMode::Index(Box::new(int_literal!((2, 5), 0))),
                    },
                )),
                expr: Box::new(int_literal!((2, 10), 2)),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        let typed_ast = typecheck("var abc = {a: 2}\nabc[\"a\"] = 3")?;
        let expected = TypedAstNode::Assignment(
            Token::Assign(Position::new(2, 10)),
            TypedAssignmentNode {
                kind: AssignmentTargetKind::MapIndex,
                typ: Type::Int,
                target: Box::new(TypedAstNode::Indexing(
                    Token::LBrack(Position::new(2, 4), false),
                    TypedIndexingNode {
                        typ: Type::Option(Box::new(Type::Int)),
                        target: Box::new(identifier_mut!((2, 1), "abc", Type::Map(vec![("a".to_string(), Type::Int)], Some(Box::new(Type::Int))), 0)),
                        index: IndexingMode::Index(Box::new(string_literal!((2, 5), "a"))),
                    },
                )),
                expr: Box::new(int_literal!((2, 12), 3)),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        Ok(())
    }

    #[test]
    fn typecheck_assignment_field() -> TestResult {
        let (typechecker, typed_ast) = typecheck_get_typechecker("\
          type Person { name: String }\n\
          val a = Person(name: \"abc\")\n\
          a.name = \"qwer\"\n\
        ");
        let expected = TypedAstNode::Assignment(
            Token::Assign(Position::new(3, 8)),
            TypedAssignmentNode {
                kind: AssignmentTargetKind::Field,
                typ: Type::String,
                target: Box::new(TypedAstNode::Accessor(
                    Token::Dot(Position::new(3, 2)),
                    TypedAccessorNode {
                        typ: Type::String,
                        target: Box::new(identifier!((3, 1), "a", Type::Reference("Person".to_string(), vec![]), 0)),
                        field_idx: 0,
                        field_name: "name".to_string(),
                        is_opt_safe: false,
                    },
                )),
                expr: Box::new(string_literal!((3, 10), "qwer")),
            },
        );
        assert_eq!(expected, typed_ast[2]);
        let expected_type = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![("name".to_string(), Type::String, false)],
            static_fields: vec![],
            methods: vec![],
        });
        assert_eq!(expected_type, typechecker.referencable_types["Person"]);

        Ok(())
    }

    #[test]
    fn typecheck_assignment_errors_with_target() {
        let err = typecheck("true = 345").unwrap_err();
        let expected = TypecheckerError::InvalidAssignmentTarget { token: Token::Assign(Position::new(1, 6)), reason: None };
        assert_eq!(expected, err);

        let err = typecheck("val abc = 345\nabc = 67").unwrap_err();
        let expected = TypecheckerError::AssignmentToImmutable {
            token: Token::Assign(Position::new(2, 5)),
            orig_ident: Token::Ident(Position::new(1, 5), "abc".to_string()),
        };
        assert_eq!(expected, err);

        let err = typecheck("var abc = 345\nabc = \"str\"").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(2, 7), "str".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, err);

        let err = typecheck("val a = [1, 2]\na[0:1] = \"str\"").unwrap_err();
        let expected = TypecheckerError::InvalidAssignmentTarget {
            token: Token::Assign(Position::new(2, 8)),
            reason: Some(InvalidAssignmentTargetReason::IndexingMode),
        };
        assert_eq!(expected, err);

        let err = typecheck("val a = \"abc\"\na[0] = \"qwer\"").unwrap_err();
        let expected = TypecheckerError::InvalidAssignmentTarget {
            token: Token::Assign(Position::new(2, 6)),
            reason: Some(InvalidAssignmentTargetReason::StringTarget),
        };
        assert_eq!(expected, err);

        let err = typecheck("\
          type Person { name: String }\n\
          val a = Person(name: \"abc\")\n\
          a.bogusField = \"qwer\"\
        ").unwrap_err();
        let expected = TypecheckerError::UnknownMember {
            token: ident_token!((3, 3), "bogusField"),
            target_type: Type::Reference("Person".to_string(), vec![]),
        };
        assert_eq!(expected, err);

        let err = typecheck("\
          type Person { name: String }\n\
          val a = Person(name: \"abc\")\n\
          a.name = 123\
        ").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Int(Position::new(3, 10), 123),
            expected: Type::String,
            actual: Type::Int,
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn typecheck_assignment_errors_with_type() {
        let err = typecheck("val abc = [1, 2]\nabc[2] = \"7\"").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(2, 10), "7".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, err);

        let err = typecheck("val abc = {a: 2, b: 3}\nabc[\"b\"] = \"7\"").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(2, 12), "7".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn typecheck_indexing() -> TestResult {
        let typed_ast = typecheck("val abc = [1, 2, 3]\nabc[1]")?;
        let expected = TypedAstNode::Indexing(
            Token::LBrack(Position::new(2, 4), false),
            TypedIndexingNode {
                typ: Type::Option(Box::new(Type::Int)),
                target: Box::new(identifier!((2, 1), "abc", Type::Array(Box::new(Type::Int)), 0)),
                index: IndexingMode::Index(Box::new(int_literal!((2, 5), 1))),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        let typed_ast = typecheck("val idx = 1\nval abc = [1, 2, 3]\nabc[idx:]")?;
        let expected = TypedAstNode::Indexing(
            Token::LBrack(Position::new(3, 4), false),
            TypedIndexingNode {
                typ: Type::Array(Box::new(Type::Int)),
                target: Box::new(identifier!((3, 1), "abc", Type::Array(Box::new(Type::Int)), 0)),
                index: IndexingMode::Range(
                    Some(Box::new(identifier!((3, 5), "idx", Type::Int, 0))),
                    None,
                ),
            },
        );
        assert_eq!(expected, typed_ast[2]);

        let typed_ast = typecheck("val idx = 1\n\"abc\"[:idx * 2]")?;
        let expected = TypedAstNode::Indexing(
            Token::LBrack(Position::new(2, 6), false),
            TypedIndexingNode {
                typ: Type::String,
                target: Box::new(string_literal!((2, 1), "abc")),
                index: IndexingMode::Range(
                    None,
                    Some(Box::new(
                        TypedAstNode::Binary(
                            Token::Star(Position::new(2, 12)),
                            TypedBinaryNode {
                                typ: Type::Int,
                                op: BinaryOp::Mul,
                                left: Box::new(identifier!((2, 8), "idx", Type::Int, 0)),
                                right: Box::new(int_literal!((2, 14), 2)),
                            },
                        ),
                    )),
                ),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        let typed_ast = typecheck("val map = { a: 1, b: 2 }\nmap[\"a\"]")?;
        let expected = TypedAstNode::Indexing(
            Token::LBrack(Position::new(2, 4), false),
            TypedIndexingNode {
                typ: Type::Option(Box::new(Type::Int)),
                target: Box::new(identifier!(
                    (2, 1),
                    "map",
                    Type::Map(
                        vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
                        Some(Box::new(Type::Int)),
                    ),
                    0
                )),
                index: IndexingMode::Index(Box::new(string_literal!((2, 5), "a"))),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        Ok(())
    }

    #[test]
    fn typecheck_indexing_errors() {
        let err = typecheck("[1, 2, 3][\"a\"]").unwrap_err();
        let expected = TypecheckerError::InvalidIndexingSelector {
            token: Token::String(Position::new(1, 11), "a".to_string()),
            target_type: Type::Array(Box::new(Type::Int)),
            selector_type: Type::String,
        };
        assert_eq!(expected, err);

        let err = typecheck("\"abcd\"[[1, 2]]").unwrap_err();
        let expected = TypecheckerError::InvalidIndexingSelector {
            token: Token::LBrack(Position::new(1, 8), false),
            target_type: Type::String,
            selector_type: Type::Array(Box::new(Type::Int)),
        };
        assert_eq!(expected, err);

        let err = typecheck("[1, 2, 3][\"a\":]").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(1, 11), "a".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, err);

        let err = typecheck("[1, 2, 3][:\"a\"]").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::String(Position::new(1, 12), "a".to_string()),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, err);

        let err = typecheck("123[0]").unwrap_err();
        let expected = TypecheckerError::InvalidIndexingTarget {
            token: Token::LBrack(Position::new(1, 4), false),
            target_type: Type::Int,
        };
        assert_eq!(expected, err);

        let err = typecheck("val a: Int = [1, 2, 3][0]").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::LBrack(Position::new(1, 23), false),
            expected: Type::Int,
            actual: Type::Option(Box::new(Type::Int)),
        };
        assert_eq!(expected, err);

        let err = typecheck("{ a: 1, b: 2 }[3]").unwrap_err();
        let expected = TypecheckerError::InvalidIndexingSelector {
            token: Token::Int(Position::new(1, 16), 3),
            target_type: Type::Map(
                vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
                Some(Box::new(Type::Int)),
            ),
            selector_type: Type::Int,
        };
        assert_eq!(expected, err);

        let err = typecheck("{ a: true, b: 2 }[\"a\"]").unwrap_err();
        let expected = TypecheckerError::InvalidIndexingTarget {
            token: Token::LBrack(Position::new(1, 18), false),
            target_type: Type::Map(
                vec![("a".to_string(), Type::Bool), ("b".to_string(), Type::Int)],
                None,
            ),
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn typecheck_if_statement() -> TestResult {
        let typed_ast = typecheck("if 1 < 2 1234")?;
        let expected = TypedAstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            TypedIfNode {
                typ: Type::Unit,
                condition: Box::new(
                    TypedAstNode::Binary(
                        Token::LT(Position::new(1, 6)),
                        TypedBinaryNode {
                            typ: Type::Bool,
                            left: Box::new(int_literal!((1, 4), 1)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 8), 2)),
                        },
                    )
                ),
                condition_binding: None,
                if_block: vec![int_literal!((1, 10), 1234)],
                else_block: None,
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("if 1 < 2 1234 else 1 + 2")?;
        let expected = TypedAstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            TypedIfNode {
                typ: Type::Unit,
                condition: Box::new(
                    TypedAstNode::Binary(
                        Token::LT(Position::new(1, 6)),
                        TypedBinaryNode {
                            typ: Type::Bool,
                            left: Box::new(int_literal!((1, 4), 1)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 8), 2)),
                        },
                    )
                ),
                condition_binding: None,
                if_block: vec![int_literal!((1, 10), 1234)],
                else_block: Some(vec![
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(1, 22)),
                        TypedBinaryNode {
                            typ: Type::Int,
                            left: Box::new(int_literal!((1, 20), 1)),
                            op: BinaryOp::Add,
                            right: Box::new(int_literal!((1, 24), 2)),
                        },
                    )
                ]),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("\
          val i: Int? = 0\n\
          if i 1 else 2\
        ")?;
        let expected = TypedAstNode::IfStatement(
            Token::If(Position::new(2, 1)),
            TypedIfNode {
                typ: Type::Unit,
                condition: Box::new(identifier!((2, 4), "i", Type::Option(Box::new(Type::Int)), 0)),
                condition_binding: None,
                if_block: vec![int_literal!((2, 6), 1)],
                else_block: Some(vec![int_literal!((2, 13), 2)]),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        Ok(())
    }

    #[test]
    fn typecheck_if_statement_condition_binding() -> TestResult {
        let typed_ast = typecheck("\
          val i: Int? = 0\n\
          if i |i| i else 2\
        ")?;
        let expected = TypedAstNode::IfStatement(
            Token::If(Position::new(2, 1)),
            TypedIfNode {
                typ: Type::Unit,
                condition: Box::new(identifier!((2, 4), "i", Type::Option(Box::new(Type::Int)), 0)),
                condition_binding: Some(ident_token!((2, 7), "i")),
                if_block: vec![
                    identifier!((2, 10), "i", Type::Int, 1)
                ],
                else_block: Some(vec![int_literal!((2, 17), 2)]),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        let typed_ast = typecheck("if true |v| v else false")?;
        let expected = TypedAstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            TypedIfNode {
                typ: Type::Unit,
                condition: Box::new(bool_literal!((1, 4), true)),
                condition_binding: Some(ident_token!((1, 10), "v")),
                if_block: vec![
                    identifier!((1, 13), "v", Type::Bool, 1)
                ],
                else_block: Some(vec![bool_literal!((1, 20), false)]),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_if_statement_errors() {
        let error = typecheck("if 4 { val a = \"hello\" a }").unwrap_err();
        let expected = TypecheckerError::InvalidIfConditionType {
            token: Token::Int(Position::new(1, 4), 4),
            actual: Type::Int,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_if_statement_scopes() -> TestResult {
        let typed_ast = typecheck("if 1 < 2 { val a = \"hello\" a }")?;
        let expected = TypedAstNode::IfStatement(
            Token::If(Position::new(1, 1)),
            TypedIfNode {
                typ: Type::Unit,
                condition: Box::new(
                    TypedAstNode::Binary(
                        Token::LT(Position::new(1, 6)),
                        TypedBinaryNode {
                            typ: Type::Bool,
                            left: Box::new(int_literal!((1, 4), 1)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((1, 8), 2)),
                        },
                    )
                ),
                condition_binding: None,
                if_block: vec![
                    TypedAstNode::BindingDecl(
                        Token::Val(Position::new(1, 12)),
                        TypedBindingDeclNode {
                            ident: Token::Ident(Position::new(1, 16), "a".to_string()),
                            is_mutable: false,
                            expr: Some(Box::new(string_literal!((1, 20), "hello"))),
                            scope_depth: 1,
                        },
                    ),
                    identifier!((1, 28), "a", Type::String, 1)
                ],
                else_block: None,
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck(
            "val a = \"hello\"\nif 1 < 2 { val b = \"world\" a + b } else { a + \"!\" }"
        )?;
        let expected = TypedAstNode::IfStatement(
            Token::If(Position::new(2, 1)),
            TypedIfNode {
                typ: Type::Unit,
                condition: Box::new(
                    TypedAstNode::Binary(
                        Token::LT(Position::new(2, 6)),
                        TypedBinaryNode {
                            typ: Type::Bool,
                            left: Box::new(int_literal!((2, 4), 1)),
                            op: BinaryOp::Lt,
                            right: Box::new(int_literal!((2, 8), 2)),
                        },
                    )
                ),
                condition_binding: None,
                if_block: vec![
                    TypedAstNode::BindingDecl(
                        Token::Val(Position::new(2, 12)),
                        TypedBindingDeclNode {
                            ident: Token::Ident(Position::new(2, 16), "b".to_string()),
                            is_mutable: false,
                            expr: Some(Box::new(string_literal!((2, 20), "world"))),
                            scope_depth: 1,
                        },
                    ),
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(2, 30)),
                        TypedBinaryNode {
                            typ: Type::String,
                            left: Box::new(identifier!((2, 28), "a", Type::String, 0)),
                            op: BinaryOp::Add,
                            right: Box::new(identifier!((2, 32), "b", Type::String, 1)),
                        },
                    )
                ],
                else_block: Some(vec![
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(2, 45)),
                        TypedBinaryNode {
                            typ: Type::String,
                            left: Box::new(identifier!((2, 43), "a", Type::String, 0)),
                            op: BinaryOp::Add,
                            right: Box::new(string_literal!((2, 47), "!")),
                        },
                    ),
                ]),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        Ok(())
    }

    #[test]
    fn typecheck_if_statement_scopes_errors() {
        let error = typecheck("if (1 < 2) { a }").unwrap_err();
        let expected = TypecheckerError::UnknownIdentifier { ident: Token::Ident(Position::new(1, 14), "a".to_string()) };
        assert_eq!(expected, error);

        let error = typecheck("val num = 1\nif (1 < 2) { num + true }").unwrap_err();
        let expected = TypecheckerError::InvalidOperator {
            token: Token::Plus(Position::new(2, 18)),
            ltype: Type::Int,
            op: BinaryOp::Add,
            rtype: Type::Bool,
        };
        assert_eq!(expected, error);

        let error = typecheck("if (1 < 2) { val num = 1 }\nnum + 2").unwrap_err();
        let expected = TypecheckerError::UnknownIdentifier { ident: Token::Ident(Position::new(2, 1), "num".to_string()) };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_if_expression_conversion_to_statement() -> TestResult {
        let typed_ast = typecheck("\
          func abc() {\n\
            if true { println(\"hello\") }\n\
          }\
        ")?;
        let expected = TypedAstNode::FunctionDecl(
            Token::Func(Position::new(1, 1)),
            TypedFunctionDeclNode {
                name: ident_token!((1, 6), "abc"),
                args: vec![],
                body: vec![TypedAstNode::IfStatement(
                    Token::If(Position::new(2, 1)),
                    TypedIfNode {
                        typ: Type::Unit,
                        condition: Box::new(bool_literal!((2, 4), true)),
                        condition_binding: None,
                        if_block: vec![
                            TypedAstNode::Invocation(
                                Token::LParen(Position::new(2, 18), false),
                                TypedInvocationNode {
                                    typ: Type::Unit,
                                    target: Box::new(identifier!((2, 11), "println", Type::Fn(FnType { arg_types: vec![("_".to_string(), Type::Any, false)], type_args: vec![], ret_type: Box::new(Type::Unit) }), 0)),
                                    args: vec![
                                        Some(string_literal!((2, 19), "hello"))
                                    ],
                                },
                            )
                        ],
                        else_block: None,
                    },
                )],
                ret_type: Type::Unit,
                scope_depth: 0,
                is_recursive: false,
            },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_if_expression() {
        let (typechecker, _) = typecheck_get_typechecker("val a = if (1 < 2) 123 else 456");
        match typechecker.get_binding("a") {
            Some((ScopeBinding(_, typ, _), scope_depth)) => {
                assert_eq!(&Type::Int, typ);
                assert_eq!(0, scope_depth);
            }
            _ => panic!("There should be a binding named 'a'"),
        }

        let (typechecker, _) = typecheck_get_typechecker("val a = if (1 < 2) 123");
        match typechecker.get_binding("a") {
            Some((ScopeBinding(_, typ, _), scope_depth)) => {
                assert_eq!(&Type::Option(Box::new(Type::Int)), typ);
                assert_eq!(0, scope_depth);
            }
            _ => panic!("There should be a binding named 'a'"),
        }

        let (typechecker, _) = typecheck_get_typechecker("val a = if (1 < 2) { if (true) 123 } else if (false) 456");
        match typechecker.get_binding("a") {
            Some((ScopeBinding(_, typ, _), scope_depth)) => {
                assert_eq!(&Type::Option(Box::new(Type::Int)), typ);
                assert_eq!(0, scope_depth);
            }
            _ => panic!("There should be a binding named 'a'"),
        }
    }

    #[test]
    fn typecheck_if_expression_errors() {
        let error = typecheck("val a = if (1 < 2) {} else 456").unwrap_err();
        let expected = TypecheckerError::MissingIfExprBranch { if_token: Token::If(Position::new(1, 9)), is_if_branch: true };
        assert_eq!(expected, error);

        let error = typecheck("val a = if (1 < 2) 123 else {}").unwrap_err();
        let expected = TypecheckerError::MissingIfExprBranch { if_token: Token::If(Position::new(1, 9)), is_if_branch: false };
        assert_eq!(expected, error);

        let error = typecheck("val a = if (1 < 2) 123 else true").unwrap_err();
        let expected = TypecheckerError::IfExprBranchMismatch {
            if_token: Token::If(Position::new(1, 9)),
            if_type: Type::Int,
            else_type: Type::Bool,
        };
        assert_eq!(expected, error);

        let error = typecheck("val a = if (1 < 2) { if (true) 123 } else 456").unwrap_err();
        let expected = TypecheckerError::IfExprBranchMismatch {
            if_token: Token::If(Position::new(1, 9)),
            if_type: Type::Option(Box::new(Type::Int)),
            else_type: Type::Int,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_invocation() -> TestResult {
        let ast = typecheck("func abc() {}\nabc()")?;
        let node = ast.get(1).unwrap();

        let expected = TypedAstNode::Invocation(
            Token::LParen(Position::new(2, 4), false),
            TypedInvocationNode {
                typ: Type::Unit,
                target: Box::new(
                    identifier!((2, 1), "abc", Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::Unit) }), 0)
                ),
                args: vec![],
            },
        );
        assert_eq!(node, &expected);

        let ast = typecheck("\
          func abc(a: Int, b: String) { b }\n\
          abc(1, \"2\")\
        ")?;
        let node = ast.get(1).unwrap();

        let expected = TypedAstNode::Invocation(
            Token::LParen(Position::new(2, 4), false),
            TypedInvocationNode {
                typ: Type::String,
                target: Box::new(identifier!(
                    (2, 1),
                    "abc",
                    Type::Fn(FnType {
                        arg_types: vec![("a".to_string(), Type::Int, false), ("b".to_string(), Type::String, false)],
                        type_args: vec![],
                        ret_type: Box::new(Type::String),
                    }),
                    0
                )),
                args: vec![
                    Some(int_literal!((2, 5), 1)),
                    Some(string_literal!((2, 8), "2")),
                ],
            },
        );
        assert_eq!(node, &expected);

        let ast = typecheck("\
          func abc(a: Int, b = \"hello\") { b }\n\
          abc(1)\
        ");
        assert_eq!(ast.is_ok(), true);

        Ok(())
    }

    #[test]
    fn typecheck_invocation_instantiation() -> TestResult {
        let (typechecker, typed_ast) = typecheck_get_typechecker("\
          type Person { name: String }\n\
          Person(name: \"Ken\")\
        ");
        let expected = TypedAstNode::Instantiation(
            Token::LParen(Position::new(2, 7), false),
            TypedInstantiationNode {
                typ: Type::Reference("Person".to_string(), vec![]),
                target: Box::new(
                    identifier!((2, 1), "Person", Type::Type("Person".to_string(), Box::new(Type::Reference("Person".to_string(), vec![])), false), 0)
                ),
                fields: vec![
                    ("name".to_string(), string_literal!((2, 14), "Ken"))
                ],
            },
        );
        assert_eq!(expected, typed_ast[1]);
        let expected_type = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![("name".to_string(), Type::String, false)],
            static_fields: vec![],
            methods: vec![],
        });
        assert_eq!(expected_type, typechecker.referencable_types["Person"]);

        // Test with default parameters
        let (typechecker, typed_ast) = typecheck_get_typechecker("\
          type Person { name: String, age: Int = 0 }\n\
          Person(name: \"Ken\")\
        ");
        let expected = TypedAstNode::Instantiation(
            Token::LParen(Position::new(2, 7), false),
            TypedInstantiationNode {
                typ: Type::Reference("Person".to_string(), vec![]),
                target: Box::new(
                    identifier!((2, 1), "Person", Type::Type("Person".to_string(), Box::new(Type::Reference("Person".to_string(), vec![])), false), 0)
                ),
                fields: vec![
                    ("name".to_string(), string_literal!((2, 14), "Ken")),
                    ("age".to_string(), int_literal!((1, 40), 0)),
                ],
            },
        );
        assert_eq!(expected, typed_ast[1]);
        let expected_type = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![
                ("name".to_string(), Type::String, false),
                ("age".to_string(), Type::Int, true),
            ],
            static_fields: vec![],
            methods: vec![],
        });
        assert_eq!(expected_type, typechecker.referencable_types["Person"]);

        Ok(())
    }

    #[test]
    fn typecheck_invocation_errors() {
        let error = typecheck("func abc() {}\nabc(1, 2)").unwrap_err();
        let expected = TypecheckerError::IncorrectArity {
            token: ident_token!((2, 1), "abc"),
            expected: 0,
            actual: 2,
        };
        assert_eq!(error, expected);

        let error = typecheck("func abc(a: Int) {}\nabc(z: false)").unwrap_err();
        let expected = TypecheckerError::UnexpectedParamName {
            token: ident_token!((2, 5), "z"),
        };
        assert_eq!(error, expected);

        let error = typecheck("func abc(a: Int, b: Int) {}\nabc()").unwrap_err();
        let expected = TypecheckerError::IncorrectArity {
            token: ident_token!((2, 1), "abc"),
            expected: 2,
            actual: 0,
        };
        assert_eq!(error, expected);

        let error = typecheck("func abc(a: Int, b: Int) {}\nabc(a: 3)").unwrap_err();
        let expected = TypecheckerError::MissingRequiredParams {
            token: ident_token!((2, 1), "abc"),
            missing_params: vec!["b".to_string()],
        };
        assert_eq!(error, expected);

        let error = typecheck("func abc(a: Int) {}\nabc(false)").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Bool(Position::new(2, 5), false),
            expected: Type::Int,
            actual: Type::Bool,
        };
        assert_eq!(error, expected);

        let error = typecheck("func abc(a: Int) {}\nabc(a: false)").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Bool(Position::new(2, 8), false),
            expected: Type::Int,
            actual: Type::Bool,
        };
        assert_eq!(error, expected);

        let error = typecheck("val abc = [1, 2]\nabc()").unwrap_err();
        let expected = TypecheckerError::InvalidInvocationTarget {
            token: ident_token!((2, 1), "abc"),
            target_type: Type::Array(Box::new(Type::Int)),
        };
        assert_eq!(error, expected);
    }

    #[test]
    fn typecheck_invocation_struct_instantiation_error() {
        let error = typecheck("\
          type Person { name: String }\n\
          Person()\
        ").unwrap_err();
        let expected = TypecheckerError::MissingRequiredParams {
            token: ident_token!((2, 1), "Person"),
            missing_params: vec!["name".to_string()],
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          type Person { name: String }\n\
          Person(args: 1)\
        ").unwrap_err();
        let expected = TypecheckerError::UnexpectedParamName {
            token: ident_token!((2, 8), "args"),
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          type Person { name: String }\n\
          Person(1)\
        ").unwrap_err();
        let expected = TypecheckerError::InvalidTypeFuncInvocation {
            token: ident_token!((2, 1), "Person"),
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          type Person { name: String }\n\
          Person(name: 123)\
        ").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Int(Position::new(2, 14), 123),
            expected: Type::String,
            actual: Type::Int,
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          type Person { name: String }\n\
          Person(age: 123, name: \"Ken\")\
        ").unwrap_err();
        let expected = TypecheckerError::UnexpectedParamName {
            token: ident_token!((2, 8), "age"),
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          type Person { name: String }\n\
          Person(name: \"Meg\", name: \"Ken\")\
        ").unwrap_err();
        let expected = TypecheckerError::DuplicateParamName {
            token: ident_token!((2, 21), "name"),
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          type Person { name: String }\n\
          Person()\
        ").unwrap_err();
        let expected = TypecheckerError::MissingRequiredParams {
            token: ident_token!((2, 1), "Person"),
            missing_params: vec!["name".to_string()],
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_invocation_primitive_instantiation_error() {
        let error = typecheck("Int(1.2)").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Float(Position::new(1, 5), 1.2),
            expected: Type::Int,
            actual: Type::Float,
        };
        assert_eq!(expected, error);

        let error = typecheck("String(1.2)").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Float(Position::new(1, 8), 1.2),
            expected: Type::String,
            actual: Type::Float,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_while_loop() -> TestResult {
        let ast = typecheck("while true 1 + 1")?;
        let expected = TypedAstNode::WhileLoop(
            Token::While(Position::new(1, 1)),
            TypedWhileLoopNode {
                condition: Box::new(bool_literal!((1, 7), true)),
                condition_binding: None,
                body: vec![
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(1, 14)),
                        TypedBinaryNode {
                            typ: Type::Int,
                            left: Box::new(int_literal!((1, 12), 1)),
                            op: BinaryOp::Add,
                            right: Box::new(int_literal!((1, 16), 1)),
                        },
                    )
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = typecheck("while true { break }")?;
        let expected = TypedAstNode::WhileLoop(
            Token::While(Position::new(1, 1)),
            TypedWhileLoopNode {
                condition: Box::new(bool_literal!((1, 7), true)),
                condition_binding: None,
                body: vec![
                    TypedAstNode::Break(Token::Break(Position::new(1, 14)))
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = typecheck("while true {\nval a = 1\na + 1 }")?;
        let expected = TypedAstNode::WhileLoop(
            Token::While(Position::new(1, 1)),
            TypedWhileLoopNode {
                condition: Box::new(bool_literal!((1, 7), true)),
                condition_binding: None,
                body: vec![
                    TypedAstNode::BindingDecl(
                        Token::Val(Position::new(2, 1)),
                        TypedBindingDeclNode {
                            scope_depth: 1,
                            is_mutable: false,
                            ident: ident_token!((2, 5), "a"),
                            expr: Some(Box::new(int_literal!((2, 9), 1))),
                        },
                    ),
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(3, 3)),
                        TypedBinaryNode {
                            typ: Type::Int,
                            left: Box::new(identifier!((3, 1), "a", Type::Int, 1)),
                            op: BinaryOp::Add,
                            right: Box::new(int_literal!((3, 5), 1)),
                        },
                    )
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = typecheck("while [1, 2][0] { break }")?;
        let expected = TypedAstNode::WhileLoop(
            Token::While(Position::new(1, 1)),
            TypedWhileLoopNode {
                condition: Box::new(
                    TypedAstNode::Indexing(
                        Token::LBrack(Position::new(1, 13), false),
                        TypedIndexingNode {
                            typ: Type::Option(Box::new(Type::Int)),
                            index: IndexingMode::Index(Box::new(int_literal!((1, 14), 0))),
                            target: Box::new(
                                TypedAstNode::Array(
                                    Token::LBrack(Position::new(1, 7), false),
                                    TypedArrayNode {
                                        typ: Type::Array(Box::new(Type::Int)),
                                        items: vec![
                                            Box::new(int_literal!((1, 8), 1)),
                                            Box::new(int_literal!((1, 11), 2)),
                                        ],
                                    },
                                )
                            ),
                        },
                    )
                ),
                condition_binding: None,
                body: vec![
                    TypedAstNode::Break(Token::Break(Position::new(1, 19)))
                ],
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn typecheck_while_loop_condition_binding() -> TestResult {
        let ast = typecheck("while true |v| { v }")?;
        let expected = TypedAstNode::WhileLoop(
            Token::While(Position::new(1, 1)),
            TypedWhileLoopNode {
                condition: Box::new(bool_literal!((1, 7), true)),
                condition_binding: Some(ident_token!((1, 13), "v")),
                body: vec![
                    identifier!((1, 18), "v", Type::Bool, 2)
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = typecheck("\
          val item = [1, 2, 3][0]\n\
          while item |item| { item }\
        ")?;
        let expected = TypedAstNode::WhileLoop(
            Token::While(Position::new(2, 1)),
            TypedWhileLoopNode {
                condition: Box::new(
                    identifier!((2, 7), "item", Type::Option(Box::new(Type::Int)), 0)
                ),
                condition_binding: Some(ident_token!((2, 13), "item")),
                body: vec![
                    identifier!((2, 21), "item", Type::Int, 2)
                ],
            },
        );
        assert_eq!(expected, ast[1]);

        Ok(())
    }

    #[test]
    fn typecheck_while_loop_error() {
        let error = typecheck("while 1 + 1 { println(123) }").unwrap_err();
        let expected = TypecheckerError::InvalidIfConditionType {
            token: Token::Plus(Position::new(1, 9)),
            actual: Type::Int,
        };
        assert_eq!(expected, error);

        let error = typecheck("while \"asdf\" |str| { println(123) }").unwrap_err();
        let expected = TypecheckerError::InvalidIfConditionType {
            token: Token::String(Position::new(1, 7), "asdf".to_string()),
            actual: Type::String,
        };
        assert_eq!(expected, error)
    }

    #[test]
    fn typecheck_break_statement_error() {
        let error = typecheck("if true { break }").unwrap_err();
        let expected = TypecheckerError::InvalidBreak(Token::Break(Position::new(1, 11)));
        assert_eq!(expected, error);

        let error = typecheck("func abc() { break }").unwrap_err();
        let expected = TypecheckerError::InvalidBreak(Token::Break(Position::new(1, 14)));
        assert_eq!(expected, error)
    }

    #[test]
    fn typecheck_for_loop() -> TestResult {
        let ast = typecheck("val arr = [1, 2, 3]\nfor a in arr {\na + 1 }")?;
        let expected = TypedAstNode::ForLoop(
            Token::For(Position::new(2, 1)),
            TypedForLoopNode {
                iteratee: ident_token!((2, 5), "a"),
                index_ident: None,
                iterator: Box::new(identifier!((2, 10), "arr", Type::Array(Box::new(Type::Int)), 0)),
                body: vec![
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(3, 3)),
                        TypedBinaryNode {
                            typ: Type::Int,
                            left: Box::new(identifier!((3, 1), "a", Type::Int, 2)),
                            op: BinaryOp::Add,
                            right: Box::new(int_literal!((3, 5), 1)),
                        },
                    )
                ],
            },
        );
        assert_eq!(expected, ast[1]);

        let ast = typecheck("val arr = [1, 2, 3]\nfor a, i in arr {\na + i }")?;
        let expected = TypedAstNode::ForLoop(
            Token::For(Position::new(2, 1)),
            TypedForLoopNode {
                iteratee: ident_token!((2, 5), "a"),
                index_ident: Some(ident_token!((2, 8), "i")),
                iterator: Box::new(identifier!((2, 13), "arr", Type::Array(Box::new(Type::Int)), 0)),
                body: vec![
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(3, 3)),
                        TypedBinaryNode {
                            typ: Type::Int,
                            left: Box::new(identifier!((3, 1), "a", Type::Int, 2)),
                            op: BinaryOp::Add,
                            right: Box::new(identifier!((3, 5), "i", Type::Int, 2)),
                        },
                    )
                ],
            },
        );
        Ok(assert_eq!(expected, ast[1]))
    }

    #[test]
    fn typecheck_for_loop_error() {
        let error = typecheck("for a in 123 { a }").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Int(Position::new(1, 10), 123),
            expected: Type::Array(Box::new(Type::Any)),
            actual: Type::Int,
        };
        assert_eq!(expected, error)
    }

    #[test]
    fn typecheck_accessor_instance() -> TestResult {
        // Getting fields off structs
        let (typechecker, typed_ast) = typecheck_get_typechecker("\
          type Person { name: String }\n\
          val p: Person = { name: \"Sam\" }\n\
          p.name\n\
        ");
        let expected = TypedAstNode::Accessor(
            Token::Dot(Position::new(3, 2)),
            TypedAccessorNode {
                typ: Type::String,
                target: Box::new(identifier!((3, 1), "p", Type::Reference("Person".to_string(), vec![]), 0)),
                field_name: "name".to_string(),
                field_idx: 0,
                is_opt_safe: false,
            },
        );
        assert_eq!(expected, typed_ast[2]);
        let expected_typ = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![("name".to_string(), Type::String, false)],
            static_fields: vec![],
            methods: vec![],
        });
        assert_eq!(expected_typ, typechecker.referencable_types["Person"]);

        // Getting fields off structs with default field values
        let (typechecker, typed_ast) = typecheck_get_typechecker("\
          type Person { name: String, age: Int = 0 }\n\
          val p: Person = { name: \"Sam\" }\n\
          p.age\n\
        ");
        let expected = TypedAstNode::Accessor(
            Token::Dot(Position::new(3, 2)),
            TypedAccessorNode {
                typ: Type::Int,
                target: Box::new(identifier!((3, 1), "p", Type::Reference("Person".to_string(), vec![]), 0)),
                field_name: "age".to_string(),
                field_idx: 1,
                is_opt_safe: false,
            },
        );
        assert_eq!(expected, typed_ast[2]);
        let expected_type = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![
                ("name".to_string(), Type::String, false),
                ("age".to_string(), Type::Int, true),
            ],
            static_fields: vec![],
            methods: vec![],
        });
        assert_eq!(expected_type, typechecker.referencable_types["Person"]);

        // Getting field of builtin Array type
        let typed_ast = typecheck("[1, 2, 3].length")?;
        let expected = TypedAstNode::Accessor(
            Token::Dot(Position::new(1, 10)),
            TypedAccessorNode {
                typ: Type::Int,
                target: Box::new(TypedAstNode::Array(
                    Token::LBrack(Position::new(1, 1), false),
                    TypedArrayNode {
                        typ: Type::Array(Box::new(Type::Int)),
                        items: vec![
                            Box::new(int_literal!((1, 2), 1)),
                            Box::new(int_literal!((1, 5), 2)),
                            Box::new(int_literal!((1, 8), 3)),
                        ],
                    },
                )),
                field_name: "length".to_string(),
                field_idx: 0,
                is_opt_safe: false,
            },
        );
        assert_eq!(expected, typed_ast[0]);

        // Getting field of builtin String type
        let typed_ast = typecheck("\"hello\".length")?;
        let expected = TypedAstNode::Accessor(
            Token::Dot(Position::new(1, 8)),
            TypedAccessorNode {
                typ: Type::Int,
                target: Box::new(string_literal!((1, 1), "hello")),
                field_name: "length".to_string(),
                field_idx: 0,
                is_opt_safe: false,
            },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_accessor_static() -> TestResult {
        // Getting static fields off structs
        let (typechecker, typed_ast) = typecheck_get_typechecker("\
          type Person { func getName(): String = \"Sam\" }\n\
          Person.getName\n\
        ");
        let expected = TypedAstNode::Accessor(
            Token::Dot(Position::new(2, 7)),
            TypedAccessorNode {
                typ: Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String) }),
                target: Box::new(identifier!((2, 1), "Person", Type::Type("Person".to_string(), Box::new(Type::Reference("Person".to_string(), vec![])), false), 0)),
                field_name: "getName".to_string(),
                field_idx: 0,
                is_opt_safe: false,
            },
        );
        assert_eq!(expected, typed_ast[1]);
        let expected_type = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![],
            static_fields: vec![("getName".to_string(), Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String) }), true)],
            methods: vec![],
        });
        assert_eq!(expected_type, typechecker.referencable_types["Person"]);

        Ok(())
    }

    #[test]
    fn typecheck_accessor_optional_safe() -> TestResult {
        let (typechecker, typed_ast) = typecheck_get_typechecker("\
          type Person { name: String? = None }\n\
          val p = Person()\n\
          p.name?.length\n\
        ");
        let expected = TypedAstNode::Accessor(
            Token::QuestionDot(Position::new(3, 7)),
            TypedAccessorNode {
                typ: Type::Option(Box::new(Type::Int)),
                target: Box::new(TypedAstNode::Accessor(
                    Token::Dot(Position::new(3, 2)),
                    TypedAccessorNode {
                        typ: Type::Option(Box::new(Type::String)),
                        target: Box::new(identifier!((3, 1), "p", Type::Reference("Person".to_string(), vec![]), 0)),
                        field_name: "name".to_string(),
                        field_idx: 0,
                        is_opt_safe: false,
                    },
                )),
                field_name: "length".to_string(),
                field_idx: 0,
                is_opt_safe: true,
            },
        );
        assert_eq!(expected, typed_ast[2]);
        let expected_type = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![("name".to_string(), Type::Option(Box::new(Type::String)), true)],
            static_fields: vec![],
            methods: vec![],
        });
        assert_eq!(expected_type, typechecker.referencable_types["Person"]);

        // Verify that it also works for non-optional fields, converting QuestionDot to just Dot
        let (typechecker, typed_ast) = typecheck_get_typechecker("\
          type Person { name: String = \"\" }\n\
          val p = Person()\n\
          p?.name\n\
        ");
        let expected = TypedAstNode::Accessor(
            Token::Dot(Position::new(3, 2)),
            TypedAccessorNode {
                typ: Type::String,
                target: Box::new(identifier!((3, 1), "p", Type::Reference("Person".to_string(), vec![]), 0)),
                field_name: "name".to_string(),
                field_idx: 0,
                is_opt_safe: false,
            },
        );
        assert_eq!(expected, typed_ast[2]);
        let expected_type = Type::Struct(StructType {
            name: "Person".to_string(),
            type_args: vec![],
            fields: vec![("name".to_string(), Type::String, true)],
            static_fields: vec![],
            methods: vec![],
        });
        assert_eq!(expected_type, typechecker.referencable_types["Person"]);

        Ok(())
    }

    #[test]
    fn typecheck_accessor_error() {
        let error = typecheck("\
          type Person { name: String }\n\
          val p: Person = { name: \"Sam\" }\n\
          p.firstName\n\
        ").unwrap_err();
        let expected = TypecheckerError::UnknownMember {
            token: ident_token!((3, 3), "firstName"),
            target_type: Type::Reference("Person".to_string(), vec![]),
        };
        assert_eq!(expected, error);

        let error = typecheck("true.value").unwrap_err();
        let expected = TypecheckerError::UnknownMember {
            token: ident_token!((1, 6), "value"),
            target_type: Type::Bool,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_lambda() -> TestResult {
        let typed_ast = typecheck("() => \"hello\"")?;
        let expected = TypedAstNode::Lambda(
            Token::Arrow(Position::new(1, 4)),
            TypedLambdaNode {
                typ: Type::Fn(FnType { arg_types: vec![], type_args: vec![], ret_type: Box::new(Type::String) }),
                args: vec![],
                typed_body: Some(vec![string_literal!((1, 7), "hello")]),
                orig_node: None,
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("a => \"hello\"")?;
        let expected = TypedAstNode::Lambda(
            Token::Arrow(Position::new(1, 3)),
            TypedLambdaNode {
                typ: Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::Unknown, false)], type_args: vec![], ret_type: Box::new(Type::Unknown) }),
                args: vec![(ident_token!((1, 1), "a"), Type::Unknown, None)],
                typed_body: None,
                orig_node: Some((
                    LambdaNode {
                        args: vec![(ident_token!((1, 1), "a"), None, None)],
                        body: vec![
                            AstNode::Literal(
                                Token::String(Position::new(1, 6), "hello".to_string()),
                                AstLiteralNode::StringLiteral("hello".to_string()),
                            )
                        ],
                    },
                    vec![Scope::root_scope()]
                )),
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("(a, b = \"b\") => \"hello\"")?;
        let expected = TypedAstNode::Lambda(
            Token::Arrow(Position::new(1, 14)),
            TypedLambdaNode {
                typ: Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::Unknown, false), ("b".to_string(), Type::String, true)], type_args: vec![], ret_type: Box::new(Type::Unknown) }),
                args: vec![
                    (ident_token!((1, 2), "a"), Type::Unknown, None),
                    (ident_token!((1, 5), "b"), Type::String, Some(string_literal!((1, 9), "b"))),
                ],
                orig_node: Some((
                    LambdaNode {
                        args: vec![
                            (ident_token!((1, 2), "a"), None, None),
                            (
                                ident_token!((1, 5), "b"),
                                None,
                                Some(AstNode::Literal(
                                    Token::String(Position::new(1, 9), "b".to_string()),
                                    AstLiteralNode::StringLiteral("b".to_string()),
                                ))
                            ),
                        ],
                        body: vec![
                            AstNode::Literal(
                                Token::String(Position::new(1, 17), "hello".to_string()),
                                AstLiteralNode::StringLiteral("hello".to_string()),
                            )
                        ],
                    },
                    vec![Scope::root_scope()]
                )),
                typed_body: None,
            },
        );
        assert_eq!(expected, typed_ast[0]);

        let typed_ast = typecheck("(a: String) => \"hello\"")?;
        let expected = TypedAstNode::Lambda(
            Token::Arrow(Position::new(1, 13)),
            TypedLambdaNode {
                typ: Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::String, false)], type_args: vec![], ret_type: Box::new(Type::String) }),
                args: vec![
                    (ident_token!((1, 2), "a"), Type::String, None),
                ],
                typed_body: Some(vec![
                    string_literal!((1, 16), "hello")
                ]),
                orig_node: None,
            },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_lambda_closure() -> TestResult {
        let typed_ast = typecheck("\
          func getAdder(x: Int): (Int) => Int {\n\
            y => x + y\n\
          }\
        ")?;
        let expected = TypedAstNode::FunctionDecl(
            Token::Func(Position::new(1, 1)),
            TypedFunctionDeclNode {
                name: ident_token!((1, 6), "getAdder"),
                args: vec![
                    (ident_token!((1, 15), "x"), Type::Int, None)
                ],
                ret_type: Type::Fn(FnType { arg_types: vec![("y".to_string(), Type::Int, false)], type_args: vec![], ret_type: Box::new(Type::Int) }),
                body: vec![
                    TypedAstNode::Lambda(
                        Token::Arrow(Position::new(2, 3)),
                        TypedLambdaNode {
                            typ: Type::Fn(FnType {
                                arg_types: vec![("y".to_string(), Type::Int, false)],
                                type_args: vec![],
                                ret_type: Box::new(Type::Int),
                            }),
                            args: vec![
                                (ident_token!((2, 1), "y"), Type::Int, None)
                            ],
                            orig_node: None,
                            typed_body: Some(vec![
                                TypedAstNode::Binary(
                                    Token::Plus(Position::new(2, 8)),
                                    TypedBinaryNode {
                                        typ: Type::Int,
                                        op: BinaryOp::Add,
                                        left: Box::new(identifier!((2, 6), "x", Type::Int, 1)),
                                        right: Box::new(identifier!((2, 10), "y", Type::Int, 2)),
                                    },
                                )
                            ]),
                        },
                    )
                ],
                scope_depth: 0,
                is_recursive: false,
            },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_lambda_errors() {
        let error = typecheck("(a: Int) => a.toUpper()").unwrap_err();
        let expected = TypecheckerError::UnknownMember {
            token: ident_token!((1, 15), "toUpper"),
            target_type: Type::Int,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_lambda_inference() -> TestResult {
        let typed_ast = typecheck("\
          var fn = (a: String) => a\n\
          fn = a => a\n\
        ")?;
        let expected = TypedAstNode::Assignment(
            Token::Assign(Position::new(2, 4)),
            TypedAssignmentNode {
                kind: AssignmentTargetKind::Identifier,
                typ: Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::String, false)], type_args: vec![], ret_type: Box::new(Type::String) }),
                target: Box::new(identifier_mut!((2, 1), "fn", Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::String, false)], type_args: vec![], ret_type: Box::new(Type::String) }), 0)),
                expr: Box::new(TypedAstNode::Lambda(
                    Token::Arrow(Position::new(2, 8)),
                    TypedLambdaNode {
                        typ: Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::String, false)], type_args: vec![], ret_type: Box::new(Type::String) }),
                        args: vec![
                            (ident_token!((2, 6), "a"), Type::String, None)
                        ],
                        typed_body: Some(vec![
                            identifier!((2, 11), "a", Type::String, 1)
                        ]),
                        orig_node: None,
                    },
                )),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        let typed_ast = typecheck("\
          var fn = (a: String) => a\n\
          fn = (a, b = \"a\") => \"hello\"\n\
        ")?;
        let expected = TypedAstNode::Assignment(
            Token::Assign(Position::new(2, 4)),
            TypedAssignmentNode {
                kind: AssignmentTargetKind::Identifier,
                typ: Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::String, false)], type_args: vec![], ret_type: Box::new(Type::String) }),
                target: Box::new(identifier_mut!((2, 1), "fn", Type::Fn(FnType { arg_types: vec![("a".to_string(), Type::String, false)], type_args: vec![], ret_type: Box::new(Type::String) }), 0)),
                expr: Box::new(TypedAstNode::Lambda(
                    Token::Arrow(Position::new(2, 19)),
                    TypedLambdaNode {
                        typ: Type::Fn(FnType {
                            arg_types: vec![("a".to_string(), Type::String, false), ("b".to_string(), Type::String, true)],
                            type_args: vec![],
                            ret_type: Box::new(Type::String),
                        }),
                        args: vec![
                            (ident_token!((2, 7), "a"), Type::String, None),
                            (ident_token!((2, 10), "b"), Type::String, Some(string_literal!((2, 14), "a"))),
                        ],
                        typed_body: Some(vec![string_literal!((2, 22), "hello")]),
                        orig_node: None,
                    },
                )),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        let typed_ast = typecheck("\
          var fn = (a: String) => a\n\
          func abc(str: String) = str\n\
          fn = abc\n\
          fn(\"abc\")\n\
        ");
        assert!(typed_ast.is_ok());

        let typed_ast = typecheck("\
          var fn: (String) => String = a => a\n\
          type Person {\n\
            name: String\n\
            func greet(self, greeting: String): String = greeting + \", \" + self.name\n\
          }\n\
          fn = Person(name: \"Ken\").greet\n\
          fn(\"Hello\")\n\
        ");
        assert!(typed_ast.is_ok());

        let typed_ast = typecheck("\
          func call(fn: (String) => String, value: String) = fn(value)\n\
          call((x, b = \"hello\") => b, \"hello\")\n\
        ");
        assert!(typed_ast.is_ok());

        // A lambda which returns Unit can accept anything as a response (which will just be thrown out)
        let typed_ast = typecheck("\
          func call(fn: (String) => Unit, value: String) = fn(value)\n\
          call((x, b = \"hello\") => b, \"hello\")\n\
        ");
        assert!(typed_ast.is_ok());

        Ok(())
    }

    #[test]
    fn typecheck_lambda_inference_errors() {
        let error = typecheck("\
          var fn: (String) => Int = a => a\
        ").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Arrow(Position::new(1, 29)),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, error);

        let error = typecheck("\
          val fns: ((Int) => Int)[] = [\n\
            x => x * 2,\n\
            x => x + \"!\",\n\
          ]\
        ").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Arrow(Position::new(3, 3)),
            expected: Type::Int,
            actual: Type::String,
        };
        assert_eq!(expected, error);
    }

    #[test]
    fn typecheck_match_statements() -> TestResult {
        // Verify branches for Int? type
        let typed_ast = typecheck("\
          val i = [1, 2][2]\n\
          match i {\n\
            Int i => i\n\
            None => 0\n\
          }
        ")?;
        let expected = TypedAstNode::MatchStatement(
            Token::Match(Position::new(2, 1)),
            TypedMatchNode {
                typ: Type::Unit,
                target: Box::new(identifier!((2, 7), "i", Type::Option(Box::new(Type::Int)), 0)),
                branches: vec![
                    (Type::Int, Some(TypeIdentifier::Normal { ident: ident_token!((3, 1), "Int"), type_args: None }), Some("i".to_string()), vec![identifier!((3, 10), "i", Type::Int, 1)], None),
                    (Type::Unknown, None, None, vec![int_literal!((4, 9), 0)], None)
                ],
            },
        );
        assert_eq!(expected, typed_ast[1]);

        // Verify branches for (Int | String)? type
        let typed_ast = typecheck("\
          val i: (Int | String)? = [1, 2][2]\n\
          match i {\n\
            Int i => i\n\
            _ v => v\n\
          }
        ")?;
        let expected = TypedAstNode::MatchStatement(
            Token::Match(Position::new(2, 1)),
            TypedMatchNode {
                typ: Type::Unit,
                target: Box::new(
                    identifier!((2, 7), "i", Type::Option(Box::new(Type::Union(vec![Type::Int, Type::String]))), 0)
                ),
                branches: vec![
                    (Type::Int, Some(TypeIdentifier::Normal { ident: ident_token!((3, 1), "Int"), type_args: None }), Some("i".to_string()), vec![identifier!((3, 10), "i", Type::Int, 1)], None),
                    (
                        Type::Union(vec![Type::String, Type::Unknown]),
                        None,
                        Some("v".to_string()),
                        vec![identifier!((4, 8), "v", Type::Union(vec![Type::String, Type::Unknown]), 1)],
                        None,
                    )
                ],
            },
        );
        assert_eq!(expected, typed_ast[1]);

        // Verify branches for enum type
        let typed_ast = typecheck("\
          enum Direction { Left, Right, Up, Down }\n\
          val d: Direction = Direction.Left\n\
          match d {\n\
            Direction.Left => 0\n\
            Direction.Right => 1\n\
            Direction.Up => 2\n\
            Direction.Down => 3\n\
          }
        ")?;
        let enum_ref_type = Type::Reference("Direction".to_string(), vec![]);
        let expected = TypedAstNode::MatchStatement(
            Token::Match(Position::new(3, 1)),
            TypedMatchNode {
                typ: Type::Unit,
                target: Box::new(identifier!((3, 7), "d", enum_ref_type.clone(), 0)),
                branches: vec![
                    (
                        Type::EnumVariant(Box::new(enum_ref_type.clone()), EnumVariantType { name: "Left".to_string(), variant_idx: 0, arg_types: None }, true),
                        Some(TypeIdentifier::Normal { ident: ident_token!((4, 1), "Direction"), type_args: None }),
                        None,
                        vec![int_literal!((4, 19), 0)],
                        None,
                    ),
                    (
                        Type::EnumVariant(Box::new(enum_ref_type.clone()), EnumVariantType { name: "Right".to_string(), variant_idx: 1, arg_types: None }, true),
                        Some(TypeIdentifier::Normal { ident: ident_token!((5, 1), "Direction"), type_args: None }),
                        None,
                        vec![int_literal!((5, 20), 1)],
                        None,
                    ),
                    (
                        Type::EnumVariant(Box::new(enum_ref_type.clone()), EnumVariantType { name: "Up".to_string(), variant_idx: 2, arg_types: None }, true),
                        Some(TypeIdentifier::Normal { ident: ident_token!((6, 1), "Direction"), type_args: None }),
                        None,
                        vec![int_literal!((6, 17), 2)],
                        None,
                    ),
                    (
                        Type::EnumVariant(Box::new(enum_ref_type.clone()), EnumVariantType { name: "Down".to_string(), variant_idx: 3, arg_types: None }, true),
                        Some(TypeIdentifier::Normal { ident: ident_token!((7, 1), "Direction"), type_args: None }),
                        None,
                        vec![int_literal!((7, 19), 3)],
                        None,
                    )
                ],
            },
        );
        assert_eq!(expected, typed_ast[2]);

        let typed_ast = typecheck("\
          enum Foo { Bar(baz: Int) }\n\
          val f: Foo = Foo.Bar(baz: 24)\n\
          val i: Int = match f {\n\
            Foo.Bar(z) => z\n\
          }
        ");
        assert!(typed_ast.is_ok());

        Ok(())
    }

    #[test]
    fn typecheck_match_expressions() -> TestResult {
        let typed_ast = typecheck("\
          val i = [1, 2][2]\n\
          val j = match i {\n\
            Int i => i\n\
            None => {\n\
              println(\"Got nothing!\")\n\
              0\n\
            }\n\
          }\n\
          j
        ")?;
        let expected = identifier!((9, 1), "j", Type::Int, 0);
        assert_eq!(expected, typed_ast[2]);

        let typed_ast = typecheck("\
          val i: String | Int = 123\n\
          val j = match i {\n\
            Int i => i\n\
            String s => s.length\n\
          }\n\
          j
        ")?;
        let expected = identifier!((6, 1), "j", Type::Int, 0);
        assert_eq!(expected, typed_ast[2]);

        Ok(())
    }

    #[test]
    fn typecheck_match_statements_errors() {
        // Verify branches for (Int | String)? type
        let err = typecheck("\
          val i: (Int | String)? = [1, 2][2]\n\
          match i {\n\
            Int i => i\n\
            None => 0\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::NonExhaustiveMatch { token: Token::Match(Position::new(2, 1)) };
        assert_eq!(expected, err);

        // Verify branches for (Int? | String?) type
        let err = typecheck("\
          val i: Int? | String? = [1, 2][2]\n\
          match i {\n\
            Int i => i\n\
            None => 0\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::NonExhaustiveMatch { token: Token::Match(Position::new(2, 1)) };
        assert_eq!(expected, err);

        let err = typecheck("\
          val i = 0\n\
          match i {\n\
            _ i => i\n\
            Int => 0\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::UnreachableMatchCase {
            token: ident_token!((4, 1), "Int"),
            typ: None,
            is_unreachable_none: false,
        };
        assert_eq!(expected, err);

        let err = typecheck("\
          val i = 0\n\
          match i {\n\
            Int i => i\n\
            String => 0\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::UnreachableMatchCase {
            token: ident_token!((4, 1), "String"),
            typ: Some(Type::String),
            is_unreachable_none: false,
        };
        assert_eq!(expected, err);

        let err = typecheck("\
          val i = 0\n\
          match i {\n\
            _ i => i\n\
            _ x => x\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::DuplicateMatchCase { token: ident_token!((4, 1), "_") };
        assert_eq!(expected, err);

        let err = typecheck("\
          val i = 0\n\
          match i {\n\
            BogusType i => i\n\
            _ x => x\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::UnknownType { type_ident: ident_token!((3, 1), "BogusType") };
        assert_eq!(expected, err);

        let err = typecheck("\
          enum Direction { Left, Right, Up, Down }\n\
          val d = Direction.Left\n\
          match d {\n\
            Direction.Sideways => 0
            _ x => x\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::UnknownMember {
            token: ident_token!((4, 11), "Sideways"),
            target_type: Type::Reference("Direction".to_string(), vec![]),
        };
        assert_eq!(expected, err);

        let err = typecheck("\
          enum Direction { Left, Right, Up, Down }\n\
          val d = Direction.Left\n\
          match d {\n\
            Direction.Left.A => 0
            _ x => x\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::UnknownMember {
            token: ident_token!((4, 16), "A"),
            target_type: Type::Reference("Direction".to_string(), vec![]),
        };
        assert_eq!(expected, err);

        let err = typecheck("\
          enum Direction { Left, Right, Up, Down }\n\
          val d = Direction.Left\n\
          match d {\n\
            Direction.Left => 0
            Direction.Left => 1
            _ x => x\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::UnreachableMatchCase {
            token: ident_token!((5, 23), "Left"),
            typ: Some(Type::EnumVariant(Box::new(Type::Reference("Direction".to_string(), vec![])), EnumVariantType { name: "Left".to_string(), variant_idx: 0, arg_types: None }, true)),
            is_unreachable_none: false,
        };
        assert_eq!(expected, err);

        let err = typecheck("\
          enum Foo { Bar(baz: Int) }\n\
          val f: Foo = Foo.Bar(baz: 24)\n\
          val i: Int = match f {\n\
            Foo.Bar(z, x) => z\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::InvalidDestructuringArity {
            token: ident_token!((4, 12), "x"),
            typ: Type::EnumVariant(
                Box::new(Type::Reference("Foo".to_string(), vec![])),
                EnumVariantType {
                    name: "Bar".to_string(),
                    variant_idx: 0,
                    arg_types: Some(vec![
                        ("baz".to_string(), Type::Int, false)
                    ]),
                },
                true,
            ),
            expected: 1,
            actual: 2,
        };
        assert_eq!(expected, err);

        let err = typecheck("\
          enum Foo { Bar(baz: Int, qux: Int) }\n\
          val f: Foo = Foo.Bar(baz: 6, qux: 24)\n\
          val i: Int = match f {\n\
            Foo.Bar(z) => z\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::InvalidDestructuringArity {
            token: ident_token!((4, 9), "z"),
            typ: Type::EnumVariant(
                Box::new(Type::Reference("Foo".to_string(), vec![])),
                EnumVariantType {
                    name: "Bar".to_string(),
                    variant_idx: 0,
                    arg_types: Some(vec![
                        ("baz".to_string(), Type::Int, false),
                        ("qux".to_string(), Type::Int, false),
                    ]),
                },
                true,
            ),
            expected: 2,
            actual: 1,
        };
        assert_eq!(expected, err);

        let err = typecheck("\
          enum Foo { Bar }\n\
          val f: Foo = Foo.Bar\n\
          val i: Int = match f {\n\
            Foo.Bar(a, b) => 0\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::InvalidDestructuring {
            token: ident_token!((4, 5), "Bar"),
            typ: Type::EnumVariant(
                Box::new(Type::Reference("Foo".to_string(), vec![])),
                EnumVariantType {
                    name: "Bar".to_string(),
                    variant_idx: 0,
                    arg_types: None,
                },
                true,
            ),
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn typecheck_match_expressions_errors() {
        let err = typecheck("\
          val i = [1, 2][2]\n\
          val j = match i {\n\
            Int i => i\n\
            None => false\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::MatchBranchMismatch {
            token: Token::Bool(Position::new(4, 9), false),
            expected: Type::Int,
            actual: Type::Bool,
        };
        assert_eq!(expected, err);

        let err = typecheck("\
          val i = [1, 2][2]\n\
          val j = match i {\n\
            Int i => println(\"\")\n\
            None => println(\"\")\n\
          }
        ").unwrap_err();
        let expected = TypecheckerError::ForbiddenVariableType {
            token: ident_token!((2, 5), "j"),
            typ: Type::Unit,
        };
        assert_eq!(expected, err);
    }
}
