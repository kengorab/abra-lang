use crate::builtins::native_fns::{NATIVE_FNS, NativeFn};
use crate::common::ast_visitor::AstVisitor;
use crate::lexer::tokens::{Token, Position};
use crate::parser::ast::{AstNode, AstLiteralNode, UnaryNode, BinaryNode, BinaryOp, UnaryOp, ArrayNode, BindingDeclNode, AssignmentNode, IndexingNode, IndexingMode, GroupedNode, IfNode, FunctionDeclNode, InvocationNode, WhileLoopNode, ForLoopNode, TypeDeclNode, MapNode, AccessorNode};
use crate::typechecker::types::Type;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode, TypedInvocationNode, TypedWhileLoopNode, TypedForLoopNode, TypedTypeDeclNode, TypedMapNode, TypedAccessorNode};
use crate::typechecker::typechecker_error::TypecheckerError;
use std::collections::{HashSet, HashMap};
use std::iter::FromIterator;
use std::slice::Iter;
use crate::builtins::native_types::fields_for_type;

#[derive(Debug, PartialEq)]
pub(crate) struct ScopeBinding(/*token:*/ Token, /*type:*/ Type, /*is_mutable:*/ bool);

#[derive(Debug, PartialEq)]
pub(crate) enum ScopeKind {
    Block,
    Function(/*token: */ Token, /*name: */ String),
    Loop,
}

pub(crate) struct Scope {
    pub(crate) kind: ScopeKind,
    pub(crate) bindings: HashMap<String, ScopeBinding>,
    pub(crate) types: HashMap<String, (Type, Option<Token>)>,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Scope { kind, bindings: HashMap::new(), types: HashMap::new() }
    }

    fn root_scope() -> Self {
        let mut scope = Scope::new(ScopeKind::Block);

        // Populate root scope with root-level builtin functions
        let native_fns: Iter<NativeFn> = NATIVE_FNS.iter();
        for NativeFn { name, args, return_type, .. } in native_fns {
            let token = Token::Ident(Position::new(0, 0), name.clone());
            let args: Vec<(String, Type, bool)> = (0..args.len()).zip(args.iter())
                .map(|(idx, (arg, default_value))| {
                    (format!("_{}", idx), arg.clone(), default_value.is_some())
                })
                .collect();
            let typ = Type::Fn(args, Box::new(return_type.clone()));
            scope.bindings.insert(name.to_string(), ScopeBinding(token, typ, false));
        }

        scope.types.insert("Int".to_string(), (Type::Int, None));
        scope.types.insert("Float".to_string(), (Type::Float, None));
        scope.types.insert("Bool".to_string(), (Type::Bool, None));
        scope.types.insert("String".to_string(), (Type::String, None));

        scope
    }
}

pub struct Typechecker {
    pub(crate) scopes: Vec<Scope>
}

impl Typechecker {
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
        let scope = self.scopes.last_mut().unwrap();
        scope.bindings.get_mut(name)
    }

    fn get_types_in_scope(&self) -> HashMap<String, Type> {
        let mut types = HashMap::<String, Type>::new();
        for scope in self.scopes.iter().rev() {
            scope.types.iter().for_each(|(key, (typ, _))| {
                types.insert(key.clone(), typ.clone());
            });
        }
        types
    }

    fn add_type(&mut self, name: String, ident: Token, typ: Type) {
        let scope = self.scopes.last_mut().unwrap();
        scope.types.insert(name, (typ, Some(ident)));
    }

    fn get_type(&self, name: &String) -> Option<(Type, Option<Token>)> {
        for scope in self.scopes.iter().rev() {
            for (type_name, (typ, tok)) in scope.types.iter() {
                if type_name == name {
                    return Some((typ.clone(), tok.clone()));
                }
            }
        }
        None
    }

    // Called from visit_if_expression and visit_if_statement, but it has to be up here since it's
    // not part of the AstVisitor trait.
    fn visit_if_node(&mut self, is_stmt: bool, node: IfNode) -> Result<TypedIfNode, TypecheckerError> {
        let IfNode { condition, if_block, else_block } = node;

        let condition = self.visit(*condition)?;
        if !condition.get_type().is_equivalent_to(&Type::Bool) {
            let token = condition.get_token().clone();
            return Err(TypecheckerError::Mismatch { token, expected: Type::Bool, actual: condition.get_type() });
        }
        let condition = Box::new(condition);

        self.scopes.push(Scope::new(ScopeKind::Block));
        let if_block_len = if_block.len();
        let if_block: Result<Vec<_>, _> = (0..if_block_len).zip(if_block.into_iter())
            .map(|(idx, mut node)| {
                // If the last node of an if-expression is an if-statement, treat it as an if-expr.
                // This is due to the fact that if-blocks are only treated as expressions in certain
                // situations, namely when an expression is already expected. Otherwise, they're parsed
                // as an if-statement. However, since the last item in an if-expression's block is the
                // "return value" for that block, the last slot is ALSO a valid place for an expression
                // to be. This is much more difficult to represent in the parser, so it's done here.
                if !is_stmt && idx == if_block_len - 1 {
                    if let AstNode::IfStatement(token, if_node) = node {
                        node = AstNode::IfExpression(token, if_node)
                    }
                }
                self.visit(node)
            })
            .collect();
        let if_block = if_block?;
        self.scopes.pop();

        self.scopes.push(Scope::new(ScopeKind::Block));
        let else_block = match else_block {
            None => None,
            Some(nodes) => {
                let else_block_len = nodes.len();
                let else_block: Result<Vec<_>, _> = (0..else_block_len).zip(nodes.into_iter())
                    .map(|(idx, mut node)| {
                        if !is_stmt && idx == else_block_len - 1 {
                            if let AstNode::IfStatement(token, if_node) = node {
                                node = AstNode::IfExpression(token, if_node)
                            }
                        }
                        self.visit(node)
                    })
                    .collect();
                Some(else_block?)
            }
        };
        self.scopes.pop();

        Ok(TypedIfNode { typ: Type::Unit, condition, if_block, else_block })
    }
}

pub fn typecheck(ast: Vec<AstNode>) -> Result<(Typechecker, Vec<TypedAstNode>), TypecheckerError> {
    let mut typechecker = Typechecker { scopes: vec![Scope::root_scope()] };

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
                    Type::Or(vec![Type::Int, Type::Float])
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
            token: &Token,
            op: &BinaryOp,
            typed_left: &TypedAstNode,
            typed_right: &TypedAstNode,
        ) -> Result<Type, TypecheckerError> {
            let ltype = typed_left.get_type();
            let rtype = typed_right.get_type();

            match op {
                BinaryOp::Add =>
                    match (&ltype, &rtype) {
                        (Type::String, _) | (_, Type::String) => Ok(Type::String),
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Sub | BinaryOp::Mul =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Div =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) | (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::Mod =>
                    match (&ltype, &rtype) {
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Int) | (Type::Int, Type::Float) | (Type::Float, Type::Float) => Ok(Type::Float),
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                BinaryOp::And | BinaryOp::Or =>
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
                BinaryOp::Coalesce => {
                    match (&ltype, &rtype) {
                        (Type::Option(ltype), rtype @ _) => {
                            if !ltype.is_equivalent_to(rtype) {
                                let token = typed_right.get_token().clone();
                                Err(TypecheckerError::Mismatch { token, expected: (**ltype).clone(), actual: rtype.clone() })
                            } else {
                                Ok((**ltype).clone())
                            }
                        }
                        (_, _) => Err(TypecheckerError::InvalidOperator { token: token.clone(), op: op.clone(), ltype, rtype })
                    }
                }
            }
        }

        let left = *node.left;
        let typed_left = self.visit(left)?;

        let right = *node.right;
        let typed_right = self.visit(right)?;

        let typ = type_for_op(&token, &node.op, &typed_left, &typed_right)?;

        Ok(TypedAstNode::Binary(token.clone(), TypedBinaryNode {
            typ,
            left: Box::new(typed_left),
            op: node.op,
            right: Box::new(typed_right),
        }))
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
            Type::Or(item_types.into_iter().collect())
        } else {
            Type::Any
        };

        let items = items.into_iter()
            .map(Box::new)
            .collect();

        Ok(TypedAstNode::Array(token.clone(), TypedArrayNode { typ: Type::Array(Box::new(typ)), items }))
    }

    fn visit_map_literal(&mut self, token: Token, node: MapNode) -> Result<TypedAstNode, TypecheckerError> {
        let MapNode { items } = node;

        let mut fields = Vec::<(String, TypedAstNode)>::new();
        let mut field_types = Vec::<(String, Type)>::new();
        let mut field_names = HashMap::<String, Token>::new();
        for (field_name_tok, field_value) in items {
            let field_name = Token::get_ident_name(&field_name_tok);
            if let Some(orig_ident) = field_names.get(field_name) {
                return Err(TypecheckerError::DuplicateBinding { orig_ident: orig_ident.clone(), ident: field_name_tok });
            } else {
                field_names.insert(field_name.clone(), field_name_tok.clone());
            }

            let field_value = self.visit(field_value)?;
            let field_type = field_value.get_type();
            field_types.push((field_name.clone(), field_type));
            fields.push((field_name.clone(), field_value));
        }

        let all_types = field_types.iter()
            .map(|(_, typ)| typ)
            .collect::<HashSet<&Type>>();
        let homogeneous_type = if all_types.len() <= 1 {
            match all_types.into_iter().next() {
                Some(typ) => Some(Box::new(typ.clone())),
                None => Some(Box::new(Type::Any))
            }
        } else {
            None
        };
        let typ = Type::Map(field_types, homogeneous_type);
        Ok(TypedAstNode::Map(token, TypedMapNode { typ, items: fields }))
    }

    fn visit_binding_decl(&mut self, token: Token, node: BindingDeclNode) -> Result<TypedAstNode, TypecheckerError> {
        let BindingDeclNode { is_mutable, ident, type_ann, expr } = node;

        if !is_mutable && expr == None {
            return Err(TypecheckerError::MissingRequiredAssignment { ident });
        }

        let name = Token::get_ident_name(&ident);

        if let Some(ScopeBinding(orig_ident, _, _)) = self.get_binding_in_current_scope(name) {
            let orig_ident = orig_ident.clone();
            return Err(TypecheckerError::DuplicateBinding { ident, orig_ident });
        }

        let typed_expr = match expr {
            Some(e) => Some(self.visit(*e)?),
            None => None
        };

        let typ = match (&typed_expr, &type_ann) {
            (Some(e), None) => Ok(e.get_type()),
            (typed_expr @ _, Some(ann)) => {
                let ann_type = Type::from_type_ident(ann, &self.get_types_in_scope())
                    .ok_or(TypecheckerError::UnknownType { type_ident: ann.get_ident() })?;

                match typed_expr {
                    None => Ok(ann_type),
                    Some(e) => {
                        if e.get_type().is_equivalent_to(&ann_type) {
                            Ok(ann_type)
                        } else {
                            Err(TypecheckerError::Mismatch {
                                token: e.get_token().clone(),
                                expected: ann_type.clone(),
                                actual: e.get_type(),
                            })
                        }
                    }
                }
            }
            (None, None) => Err(TypecheckerError::UnannotatedUninitialized {
                ident: ident.clone(),
                is_mutable,
            })
        }?;

        self.add_binding(name, &ident, &typ, is_mutable);
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
        let FunctionDeclNode { name, args, ret_type, body } = node;

        let func_name = Token::get_ident_name(&name);
        if let Some(ScopeBinding(orig_ident, _, _)) = self.get_binding_in_current_scope(func_name) {
            let orig_ident = orig_ident.clone();
            return Err(TypecheckerError::DuplicateBinding { ident: name, orig_ident });
        }

        self.scopes.push(Scope::new(ScopeKind::Function(name.clone(), func_name.clone())));
        let mut typed_args = Vec::<(Token, Type, Option<TypedAstNode>)>::with_capacity(args.len());
        let mut arg_idents = HashMap::<String, Token>::new();
        let mut seen_optional_arg = false;
        for (token, type_ident, default_value) in args {
            let arg_name = Token::get_ident_name(&token).clone();
            if let Some(arg_tok) = arg_idents.get(&arg_name) {
                return Err(TypecheckerError::DuplicateBinding { orig_ident: arg_tok.clone(), ident: token.clone() });
            }
            arg_idents.insert(arg_name, token.clone());

            match type_ident {
                Some(type_ident) => {
                    let arg_type = Type::from_type_ident(&type_ident, &self.get_types_in_scope());
                    match arg_type {
                        None => return Err(TypecheckerError::UnknownType { type_ident: type_ident.get_ident() }),
                        Some(arg_type) => {
                            match default_value {
                                Some(default_value) => {
                                    seen_optional_arg = true;
                                    let default_value = self.visit(default_value)?;
                                    if default_value.get_type().is_equivalent_to(&arg_type) {
                                        let arg_name = Token::get_ident_name(&token);
                                        self.add_binding(arg_name, &token, &arg_type, false);
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
                                    self.add_binding(arg_name, &token, &arg_type, false);
                                    typed_args.push((token, arg_type, None));
                                }
                            }
                        }
                    }
                }
                None => {
                    match default_value {
                        None => unreachable!(), // This should be caught during parsing
                        Some(default_value) => {
                            seen_optional_arg = true;
                            let default_value = self.visit(default_value)?;
                            let arg_type = default_value.get_type();
                            let arg_name = Token::get_ident_name(&token);
                            self.add_binding(arg_name, &token, &arg_type, false);
                            typed_args.push((token, arg_type, Some(default_value)));
                        }
                    }
                }
            }
        }
        let args = typed_args;

        // Store a stub version of the function type, based on what we know so far. Recursive references
        // to the function within its body will be typed according to whatever is saved now. If we cannot
        // determine the return type (due to a missing return type annotation, since we haven't yet
        // typechecked the body, and thus cannot infer it), store it as Unknown. When identifiers are
        // typechecked later in (say, for example, within the function body), a return type of Unknown
        // will signal a recursive reference to a function which is missing a return type. Note: this is
        // a fairly brittle abstraction, and should probably be readdressed.
        // Note also that we need to add this reference to the previous scope, which is achieved via
        // this pop/push.
        let scope = self.scopes.pop().unwrap();
        let arg_types = args.iter()
            .map(|(ident, typ, default_value)| {
                (Token::get_ident_name(ident).clone(), typ.clone(), default_value.is_some())
            })
            .collect::<Vec<_>>();
        let initial_ret_type = match &ret_type {
            None => Type::Unknown,
            Some(ret_type) => {
                match Type::from_type_ident(ret_type, &self.get_types_in_scope()) {
                    None => Err(TypecheckerError::UnknownType { type_ident: ret_type.get_ident() }),
                    Some(typ) => Ok(typ)
                }?
            }
        };

        let func_type = Type::Fn(arg_types, Box::new(initial_ret_type.clone()));
        self.add_binding(func_name, &name, &func_type, false);
        self.scopes.push(scope);

        // Typecheck function body
        let body_len = body.len();
        let body: Result<Vec<TypedAstNode>, _> = (0..body_len).zip(body.into_iter())
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
                            match typed_node.get_type() {
                                Type::Unit => {
                                    if let TypedAstNode::IfExpression(token, typed_if_node) = typed_node {
                                        Ok(TypedAstNode::IfStatement(token, typed_if_node))
                                    } else {
                                        unreachable!()
                                    }
                                }
                                // Kind of annoying, but the box_patterns feature is hidden behind a
                                // nightly feature, so code duplication is unavoidable here
                                Type::Option(ref t) if *t == Box::new(Type::Unit) => {
                                    if let TypedAstNode::IfExpression(token, typed_if_node) = typed_node {
                                        Ok(TypedAstNode::IfStatement(token, typed_if_node))
                                    } else {
                                        unreachable!()
                                    }
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
            .collect();
        let body = body?;
        let body_type = body.last().map_or(Type::Unit, |node| node.get_type());
        self.scopes.pop();

        let ret_type = match ret_type {
            None => body_type,
            Some(ret_type) => {
                match Type::from_type_ident(&ret_type, &self.get_types_in_scope()) {
                    None => Err(TypecheckerError::UnknownType { type_ident: ret_type.get_ident() }),
                    Some(typ) => {
                        if !body_type.is_equivalent_to(&typ) {
                            Err(TypecheckerError::Mismatch {
                                token: body.last().map_or(
                                    name.clone(),
                                    |node| node.get_token().clone(),
                                ),
                                actual: body_type,
                                expected: typ,
                            })
                        } else {
                            Ok(body_type)
                        }
                    }
                }?
            }
        };
        // Rewrite the return type of the previously-inserted func_type stub
        let ScopeBinding(_, func_type, _) = self.get_binding_mut(func_name).unwrap();
        if let Type::Fn(_, return_type) = func_type {
            *return_type = Box::new(ret_type.clone());
        }
        let scope_depth = self.scopes.len() - 1;

        Ok(TypedAstNode::FunctionDecl(token, TypedFunctionDeclNode { name, args, ret_type, body, scope_depth }))
    }

    fn visit_type_decl(&mut self, token: Token, node: TypeDeclNode) -> Result<TypedAstNode, TypecheckerError> {
        let TypeDeclNode { name, fields } = node;
        let new_type_name = Token::get_ident_name(&name).clone();
        let all_types = self.get_types_in_scope();

        if let Some((_, token)) = self.get_type(&new_type_name) {
            return Err(TypecheckerError::DuplicateType { ident: name.clone(), orig_ident: token });
        }

        let mut field_names = HashMap::<String, Token>::new();
        let fields = fields.into_iter()
            .map(|(field_name, field_type)| {
                let field_type = Type::from_type_ident(&field_type, &all_types)
                    .ok_or(TypecheckerError::UnknownType { type_ident: field_type.get_ident() })?;
                let field_name_str = Token::get_ident_name(&field_name);
                if let Some(orig_ident) = field_names.get(field_name_str) {
                    return Err(TypecheckerError::DuplicateBinding { orig_ident: orig_ident.clone(), ident: field_name });
                } else {
                    field_names.insert(field_name_str.clone(), field_name.clone());
                }
                Ok((field_name, field_type))
            })
            .collect::<Result<Vec<(Token, Type)>, _>>();
        let fields = fields?;

        let new_type = Type::Struct {
            name: new_type_name.clone(),
            fields: fields.iter()
                .map(|(name, typ)| {
                    (Token::get_ident_name(name).clone(), typ.clone())
                })
                .collect(),
        };
        let binding_type = Type::Type(new_type_name.clone(), Box::new(new_type.clone()));
        self.add_binding(&new_type_name, &name, &binding_type, false);
        self.add_type(new_type_name, name.clone(), new_type);

        Ok(TypedAstNode::TypeDecl(token, TypedTypeDeclNode { name, fields }))
    }

    fn visit_ident(&mut self, token: Token) -> Result<TypedAstNode, TypecheckerError> {
        let name = Token::get_ident_name(&token);

        match self.get_binding(name) {
            None => Err(TypecheckerError::UnknownIdentifier { ident: token }),
            Some((ScopeBinding(_, typ, is_mutable), scope_depth)) => {
                if let Type::Fn(_, ret_type) = typ {
                    // Type::Unknown acts as the sentinel value for a not-fully typechecked function
                    if **ret_type == Type::Unknown {
                        for Scope { kind, .. } in self.scopes.iter().rev() {
                            if let ScopeKind::Function(func_token, func_name) = kind {
                                if name == func_name {
                                    // A function can't be referenced recursively unless it has a declared return type
                                    return Err(TypecheckerError::RecursiveRefWithoutReturnType {
                                        orig_token: func_token.clone(),
                                        token: token.clone(),
                                    });
                                }
                            }
                        }
                    }
                }
                let node = TypedIdentifierNode {
                    typ: typ.clone(),
                    is_mutable: is_mutable.clone(),
                    scope_depth,
                };
                Ok(TypedAstNode::Identifier(token, node))
            }
        }
    }

    fn visit_assignment(&mut self, token: Token, node: AssignmentNode) -> Result<TypedAstNode, TypecheckerError> {
        let AssignmentNode { target, expr } = node;
        if let AstNode::Identifier(ident_tok) = *target {
            let ident = self.visit_ident(ident_tok.clone())?;
            let (typ, is_mutable) = match &ident {
                TypedAstNode::Identifier(_, TypedIdentifierNode { typ, is_mutable, .. }) => (typ, is_mutable),
                _ => unreachable!()
            };
            if !is_mutable {
                let name = Token::get_ident_name(&ident_tok);
                let orig_ident = match self.get_binding(name) {
                    Some((ScopeBinding(orig_ident, _, _), _)) => orig_ident.clone(),
                    None => unreachable!()
                };
                return Err(TypecheckerError::AssignmentToImmutable { token, orig_ident });
            }

            let expr = self.visit(*expr)?;
            let expr_type = expr.get_type();
            if !expr_type.is_equivalent_to(typ) {
                Err(TypecheckerError::Mismatch {
                    token: expr.get_token().clone(),
                    expected: typ.clone(),
                    actual: expr_type,
                })
            } else {
                let node = TypedAssignmentNode {
                    typ: expr_type,
                    target: Box::new(ident),
                    expr: Box::new(expr),
                };
                Ok(TypedAstNode::Assignment(token, node))
            }
        } else {
            Err(TypecheckerError::InvalidAssignmentTarget { token })
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
                    if !if_block_type.is_equivalent_to(&else_block_type) {
                        Err(TypecheckerError::IfExprBranchMismatch {
                            if_token: token.clone(),
                            if_type: if_block_type,
                            else_type: else_block_type,
                        })
                    } else {
                        Ok(if_block_type)
                    }
                }
            }
            None => Ok(Type::Option(Box::new(if_block_type)))
        }?;

        node.typ = typ;

        Ok(TypedAstNode::IfExpression(token, node))
    }

    fn visit_invocation(&mut self, token: Token, node: InvocationNode) -> Result<TypedAstNode, TypecheckerError> {
        let InvocationNode { target, args } = node;
        let target = self.visit(*target)?;
        let target_type = target.get_type();
        let (args, ret_type) = if let Type::Fn(arg_types, ret_type) = target_type {
            let num_req_args = arg_types.iter()
                .take_while(|(_, _, is_optional)| !*is_optional)
                .count();
            if args.len() < num_req_args || args.len() > arg_types.len() {
                return Err(TypecheckerError::IncorrectArity { token: target.get_token().clone(), expected: num_req_args, actual: args.len() });
            }

            let mut typed_args = Vec::<TypedAstNode>::new();
            for (arg, expected) in args.into_iter().zip(arg_types.iter()) {
                let (arg_name, arg) = arg;
                let (expected_name, expected_arg_type, _) = expected;

                if let Some(arg_name) = arg_name {
                    let passed_name = Token::get_ident_name(&arg_name);
                    if passed_name != expected_name {
                        return Err(TypecheckerError::ParamNameMismatch { token: arg_name.clone(), expected: expected_name.clone(), actual: passed_name.clone() });
                    }
                }

                let arg = self.visit(arg)?;
                let arg_type = arg.get_type();
                if !arg_type.is_equivalent_to(expected_arg_type) {
                    return Err(TypecheckerError::Mismatch { token: arg.get_token().clone(), expected: expected_arg_type.clone(), actual: arg_type });
                }
                typed_args.push(arg);
            }
            (typed_args, *ret_type)
        } else {
            return Err(TypecheckerError::InvalidInvocationTarget { token: target.get_token().clone() });
        };

        let node = TypedInvocationNode {
            typ: ret_type,
            target: Box::new(target),
            args,
        };
        Ok(TypedAstNode::Invocation(token, node))
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
        let WhileLoopNode { condition, body } = node;

        let condition = self.visit(*condition)?;
        if !condition.get_type().is_equivalent_to(&Type::Bool) {
            let token = condition.get_token().clone();
            return Err(TypecheckerError::Mismatch { token, expected: Type::Bool, actual: condition.get_type() });
        }
        let condition = Box::new(condition);

        self.scopes.push(Scope::new(ScopeKind::Loop));
        let body: Result<Vec<_>, _> = body.into_iter()
            .map(|node| self.visit(node))
            .collect();
        let body = body?;
        self.scopes.pop();

        Ok(TypedAstNode::WhileLoop(token, TypedWhileLoopNode { condition, body }))
    }

    fn visit_break(&mut self, token: Token) -> Result<TypedAstNode, TypecheckerError> {
        let mut depth = None;
        for (idx, s) in (0..self.scopes.len()).zip(self.scopes.iter()).rev() {
            if s.kind == ScopeKind::Loop {
                depth = Some(idx);
            }
        }
        match depth {
            Some(depth) => Ok(TypedAstNode::Break(token, depth)),
            None => Err(TypecheckerError::InvalidBreak(token))
        }
    }

    fn visit_accessor(&mut self, token: Token, node: AccessorNode) -> Result<TypedAstNode, TypecheckerError> {
        let AccessorNode { target, field } = node;
        let target = self.visit(*target)?;

        let field_name = Token::get_ident_name(&field);

        let target_type = target.get_type();
        let typ = match &target_type {
            Type::Struct { fields, .. } => {
                match fields.iter().find(|(name, _)| field_name == name) {
                    Some((_, typ)) => Ok(typ.clone()),
                    None => Err(TypecheckerError::UnknownMember { token: field.clone(), target_type: target_type.clone() })
                }
            }
            typ @ _ => {
                let field_type = fields_for_type(typ)
                    .and_then(|fields| {
                        fields.get(field_name.as_str())
                            .map(|t| t.clone())
                    });
                match field_type {
                    Some(typ) => Ok(typ.clone()),
                    None => Err(TypecheckerError::UnknownMember { token: field.clone(), target_type: typ.clone() })
                }
            }
        }?;

        Ok(TypedAstNode::Accessor(token, TypedAccessorNode { typ, target: Box::new(target), field }))
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
            expected: Type::Or(vec![Type::Int, Type::Float]),
            actual: Type::String,
        };
        assert_eq!(expected, err);

        let err = typecheck("-false").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Minus(Position::new(1, 1)),
            expected: Type::Or(vec![Type::Int, Type::Float]),
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
        let expected = TypedAstNode::Binary(
            Token::Or(Position::new(1, 14)),
            TypedBinaryNode {
                typ: Type::Bool,
                left: Box::new(
                    TypedAstNode::Binary(
                        Token::And(Position::new(1, 6)),
                        TypedBinaryNode {
                            typ: Type::Bool,
                            left: Box::new(bool_literal!((1, 1), true)),
                            op: BinaryOp::And,
                            right: Box::new(bool_literal!((1, 9), true)),
                        },
                    ),
                ),
                op: BinaryOp::Or,
                right: Box::new(bool_literal!((1, 17), false)),
            },
        );
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
            Token::LParen(Position::new(1, 1)),
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
            Token::LBrack(Position::new(1, 1)),
            TypedArrayNode { typ: Type::Array(Box::new(Type::Any)), items: vec![] },
        );
        assert_eq!(expected, typed_ast[0]);

        Ok(())
    }

    #[test]
    fn typecheck_array_homogeneous() -> TestResult {
        let typed_ast = typecheck("[1, 2, 3]")?;
        let expected = TypedAstNode::Array(
            Token::LBrack(Position::new(1, 1)),
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
            Token::LBrack(Position::new(1, 1)),
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
            Token::LBrack(Position::new(1, 1)),
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
                    ("a".to_string(), int_literal!((1, 6), 1)),
                    ("b".to_string(), int_literal!((1, 12), 2))
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
                    ("a".to_string(), TypedAstNode::Map(
                        Token::LBrace(Position::new(1, 6)),
                        TypedMapNode {
                            typ: nested_map_type.clone(),
                            items: vec![("c".to_string(), bool_literal!((1, 11), true))],
                        },
                    )),
                    ("b".to_string(), TypedAstNode::Map(
                        Token::LBrace(Position::new(1, 22)),
                        TypedMapNode {
                            typ: nested_map_type.clone(),
                            items: vec![("c".to_string(), bool_literal!((1, 27), false))],
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
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let (ScopeBinding(_, typ, _), scope_depth) = typechecker.get_binding("abc")
            .expect("The function abc should be defined");
        let expected_type = Type::Fn(vec![], Box::new(Type::Int));
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
                            left: Box::new(TypedAstNode::Identifier(
                                ident_token!((1, 20), "a"),
                                TypedIdentifierNode { typ: Type::Int, is_mutable: false, scope_depth: 1 },
                            )),
                            op: BinaryOp::Add,
                            right: Box::new(int_literal!((1, 24), 1)),
                        })
                ],
                scope_depth: 0,
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
                                    Token::LBrack(Position::new(1, 22)),
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
                    TypedAstNode::Identifier(
                        Token::Ident(Position::new(1, 29), "a".to_string()),
                        TypedIdentifierNode {
                            typ: Type::Array(Box::new(Type::Int)),
                            is_mutable: false,
                            scope_depth: 1,
                        },
                    )
                ],
                scope_depth: 0,
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let (ScopeBinding(_, typ, _), scope_depth) = typechecker.get_binding("abc")
            .expect("The function abc should be defined");
        let expected_type = Type::Fn(vec![], Box::new(Type::Array(Box::new(Type::Int))));
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
        assert_eq!(&Type::Fn(vec![("a".to_string(), Type::Int, false)], Box::new(Type::Bool)), typ);

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
                    Token::LBrack(Position::new(1, 26)),
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
        let (typechecker, _) = typecheck_get_typechecker("func abc(): Int {\nabc()\n}");
        let (ScopeBinding(_, typ, _), _) = typechecker.get_binding("abc")
            .expect("The function abc should be defined");
        let expected_type = Type::Fn(vec![], Box::new(Type::Int));
        assert_eq!(&expected_type, typ);

        let error = typecheck("func abc() {\nabc()\n}").unwrap_err();
        let expected = TypecheckerError::RecursiveRefWithoutReturnType {
            orig_token: ident_token!((1, 6), "abc"),
            token: ident_token!((2, 1), "abc"),
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
                    (ident_token!((1, 15), "name"), Type::String)
                ],
            },
        );
        assert_eq!(expected, typed_ast[0]);
        let (typ, _) = typechecker.get_type(&"Person".to_string()).unwrap();
        let expected_type = Type::Struct {
            name: "Person".to_string(),
            fields: vec![("name".to_string(), Type::String)],
        };
        assert_eq!(expected_type, typ);

        Ok(())
    }

    #[test]
    fn typecheck_type_decl_errors() {
        let error = typecheck("type Person { name: Huh }").unwrap_err();
        let expected = TypecheckerError::UnknownType { type_ident: ident_token!((1, 21), "Huh") };
        assert_eq!(expected, error);

        let error = typecheck("type Person { age: Int, age: String }").unwrap_err();
        let expected = TypecheckerError::DuplicateBinding { orig_ident: ident_token!((1, 15), "age"), ident: ident_token!((1, 25), "age") };
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
            TypedAstNode::Identifier(
                Token::Ident(Position::new(2, 1), "abc".to_string()),
                TypedIdentifierNode {
                    typ: Type::Int,
                    is_mutable: false,
                    scope_depth: 0,
                },
            )
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
    fn typecheck_assignment() -> TestResult {
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
                    typ: Type::Int,
                    target: Box::new(TypedAstNode::Identifier(
                        Token::Ident(Position::new(2, 1), "abc".to_string()),
                        TypedIdentifierNode {
                            typ: Type::Int,
                            is_mutable: true,
                            scope_depth: 0,
                        },
                    )),
                    expr: Box::new(int_literal!((2, 7), 456)),
                },
            )
        ];
        assert_eq!(expected, typed_ast);
        Ok(())
    }

    #[test]
    fn typecheck_assignment_errors() {
        let err = typecheck("true = 345").unwrap_err();
        let expected = TypecheckerError::InvalidAssignmentTarget { token: Token::Assign(Position::new(1, 6)) };
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
    }

    #[test]
    fn typecheck_indexing() -> TestResult {
        let typed_ast = typecheck("val abc = [1, 2, 3]\nabc[1]")?;
        let expected = TypedAstNode::Indexing(
            Token::LBrack(Position::new(2, 4)),
            TypedIndexingNode {
                typ: Type::Option(Box::new(Type::Int)),
                target: Box::new(
                    TypedAstNode::Identifier(
                        Token::Ident(Position::new(2, 1), "abc".to_string()),
                        TypedIdentifierNode {
                            typ: Type::Array(Box::new(Type::Int)),
                            is_mutable: false,
                            scope_depth: 0,
                        },
                    )
                ),
                index: IndexingMode::Index(Box::new(int_literal!((2, 5), 1))),
            },
        );
        assert_eq!(expected, typed_ast[1]);

        let typed_ast = typecheck("val idx = 1\nval abc = [1, 2, 3]\nabc[idx:]")?;
        let expected = TypedAstNode::Indexing(
            Token::LBrack(Position::new(3, 4)),
            TypedIndexingNode {
                typ: Type::Array(Box::new(Type::Int)),
                target: Box::new(
                    TypedAstNode::Identifier(
                        Token::Ident(Position::new(3, 1), "abc".to_string()),
                        TypedIdentifierNode {
                            typ: Type::Array(Box::new(Type::Int)),
                            is_mutable: false,
                            scope_depth: 0,
                        },
                    )
                ),
                index: IndexingMode::Range(
                    Some(Box::new(
                        TypedAstNode::Identifier(
                            Token::Ident(Position::new(3, 5), "idx".to_string()),
                            TypedIdentifierNode {
                                typ: Type::Int,
                                is_mutable: false,
                                scope_depth: 0,
                            },
                        )
                    )),
                    None,
                ),
            },
        );
        assert_eq!(expected, typed_ast[2]);

        let typed_ast = typecheck("val idx = 1\n\"abc\"[:idx * 2]")?;
        let expected = TypedAstNode::Indexing(
            Token::LBrack(Position::new(2, 6)),
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
                                left: Box::new(
                                    TypedAstNode::Identifier(
                                        Token::Ident(Position::new(2, 8), "idx".to_string()),
                                        TypedIdentifierNode {
                                            typ: Type::Int,
                                            is_mutable: false,
                                            scope_depth: 0,
                                        },
                                    )
                                ),
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
            Token::LBrack(Position::new(2, 4)),
            TypedIndexingNode {
                typ: Type::Option(Box::new(Type::Int)),
                target: Box::new(TypedAstNode::Identifier(
                    ident_token!((2, 1), "map"),
                    TypedIdentifierNode {
                        typ: Type::Map(
                            vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
                            Some(Box::new(Type::Int)),
                        ),
                        scope_depth: 0,
                        is_mutable: false,
                    },
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
            token: Token::LBrack(Position::new(1, 8)),
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
            token: Token::LBrack(Position::new(1, 4)),
            target_type: Type::Int,
        };
        assert_eq!(expected, err);

        let err = typecheck("val a: Int = [1, 2, 3][0]").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::LBrack(Position::new(1, 23)),
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
            token: Token::LBrack(Position::new(1, 18)),
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

        Ok(())
    }

    #[test]
    fn typecheck_if_statement_errors() {
        let error = typecheck("if 4 { val a = \"hello\" a }").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Int(Position::new(1, 4), 4),
            actual: Type::Int,
            expected: Type::Bool,
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
                    TypedAstNode::Identifier(
                        Token::Ident(Position::new(1, 28), "a".to_string()),
                        TypedIdentifierNode {
                            typ: Type::String,
                            is_mutable: false,
                            scope_depth: 1,
                        },
                    )
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
                            left: Box::new(
                                TypedAstNode::Identifier(
                                    Token::Ident(Position::new(2, 28), "a".to_string()),
                                    TypedIdentifierNode {
                                        typ: Type::String,
                                        is_mutable: false,
                                        scope_depth: 0,
                                    },
                                )
                            ),
                            op: BinaryOp::Add,
                            right: Box::new(
                                TypedAstNode::Identifier(
                                    Token::Ident(Position::new(2, 32), "b".to_string()),
                                    TypedIdentifierNode {
                                        typ: Type::String,
                                        is_mutable: false,
                                        scope_depth: 1,
                                    },
                                )
                            ),
                        },
                    )
                ],
                else_block: Some(vec![
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(2, 45)),
                        TypedBinaryNode {
                            typ: Type::String,
                            left: Box::new(
                                TypedAstNode::Identifier(
                                    Token::Ident(Position::new(2, 43), "a".to_string()),
                                    TypedIdentifierNode {
                                        typ: Type::String,
                                        is_mutable: false,
                                        scope_depth: 0,
                                    },
                                )
                            ),
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
            Token::LParen(Position::new(2, 4)),
            TypedInvocationNode {
                typ: Type::Unit,
                target: Box::new(TypedAstNode::Identifier(
                    ident_token!((2, 1), "abc"),
                    TypedIdentifierNode {
                        typ: Type::Fn(vec![], Box::new(Type::Unit)),
                        is_mutable: false,
                        scope_depth: 0,
                    },
                )),
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
            Token::LParen(Position::new(2, 4)),
            TypedInvocationNode {
                typ: Type::String,
                target: Box::new(TypedAstNode::Identifier(
                    ident_token!((2, 1), "abc"),
                    TypedIdentifierNode {
                        typ: Type::Fn(
                            vec![("a".to_string(), Type::Int, false), ("b".to_string(), Type::String, false)],
                            Box::new(Type::String),
                        ),
                        is_mutable: false,
                        scope_depth: 0,
                    },
                )),
                args: vec![
                    int_literal!((2, 5), 1),
                    string_literal!((2, 8), "2"),
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
    fn typecheck_invocation_errors() {
        let error = typecheck("func abc() {}\nabc(1, 2)").unwrap_err();
        let expected = TypecheckerError::IncorrectArity {
            token: ident_token!((2, 1), "abc"),
            expected: 0,
            actual: 2,
        };
        assert_eq!(error, expected);

        let error = typecheck("func abc(a: Int) {}\nabc(z: false)").unwrap_err();
        let expected = TypecheckerError::ParamNameMismatch {
            token: ident_token!((2, 5), "z"),
            expected: "a".to_string(),
            actual: "z".to_string(),
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
        };
        assert_eq!(error, expected);
    }

    #[test]
    fn typecheck_while_loop() -> TestResult {
        let ast = typecheck("while true 1 + 1")?;
        let expected = TypedAstNode::WhileLoop(
            Token::While(Position::new(1, 1)),
            TypedWhileLoopNode {
                condition: Box::new(bool_literal!((1, 7), true)),
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
                body: vec![
                    TypedAstNode::Break(
                        Token::Break(Position::new(1, 14)),
                        1,
                    )
                ],
            },
        );
        assert_eq!(expected, ast[0]);

        let ast = typecheck("while true {\nval a = 1\na + 1 }")?;
        let expected = TypedAstNode::WhileLoop(
            Token::While(Position::new(1, 1)),
            TypedWhileLoopNode {
                condition: Box::new(bool_literal!((1, 7), true)),
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
                            left: Box::new(TypedAstNode::Identifier(
                                ident_token!((3, 1), "a"),
                                TypedIdentifierNode {
                                    typ: Type::Int,
                                    is_mutable: false,
                                    scope_depth: 1,
                                },
                            )),
                            op: BinaryOp::Add,
                            right: Box::new(int_literal!((3, 5), 1)),
                        },
                    )
                ],
            },
        );
        Ok(assert_eq!(expected, ast[0]))
    }

    #[test]
    fn typecheck_while_loop_error() {
        let error = typecheck("while 1 + 1 { println(123) }").unwrap_err();
        let expected = TypecheckerError::Mismatch {
            token: Token::Plus(Position::new(1, 9)),
            expected: Type::Bool,
            actual: Type::Int,
        };
        assert_eq!(expected, error)
    }

    #[test]
    fn typecheck_break_statement_error() {
        let error = typecheck("if true { break }").unwrap_err();
        let expected = TypecheckerError::InvalidBreak(Token::Break(Position::new(1, 11)));
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
                iterator: Box::new(TypedAstNode::Identifier(
                    ident_token!((2, 10), "arr"),
                    TypedIdentifierNode {
                        is_mutable: false,
                        scope_depth: 0,
                        typ: Type::Array(Box::new(Type::Int)),
                    },
                )),
                body: vec![
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(3, 3)),
                        TypedBinaryNode {
                            typ: Type::Int,
                            left: Box::new(TypedAstNode::Identifier(
                                ident_token!((3, 1), "a"),
                                TypedIdentifierNode {
                                    is_mutable: false,
                                    scope_depth: 2, // Depth is 2 because of intrinsic wrapper scope
                                    typ: Type::Int,
                                },
                            )),
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
                iterator: Box::new(TypedAstNode::Identifier(
                    ident_token!((2, 13), "arr"),
                    TypedIdentifierNode {
                        is_mutable: false,
                        scope_depth: 0,
                        typ: Type::Array(Box::new(Type::Int)),
                    },
                )),
                body: vec![
                    TypedAstNode::Binary(
                        Token::Plus(Position::new(3, 3)),
                        TypedBinaryNode {
                            typ: Type::Int,
                            left: Box::new(TypedAstNode::Identifier(
                                ident_token!((3, 1), "a"),
                                TypedIdentifierNode {
                                    is_mutable: false,
                                    scope_depth: 2, // Depth is 2 because of intrinsic wrapper scope
                                    typ: Type::Int,
                                },
                            )),
                            op: BinaryOp::Add,
                            right: Box::new(TypedAstNode::Identifier(
                                ident_token!((3, 5), "i"),
                                TypedIdentifierNode {
                                    is_mutable: false,
                                    scope_depth: 2, // Depth is 2 because of intrinsic wrapper scope
                                    typ: Type::Int,
                                },
                            )),
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
    fn typecheck_accessor() -> TestResult {
        // Getting fields off structs
        let typed_ast = typecheck("\
          type Person { name: String }\n\
          val p: Person = { name: \"Sam\" }\n\
          p.name\n\
        ")?;
        let expected = TypedAstNode::Accessor(
            Token::Dot(Position::new(3, 2)),
            TypedAccessorNode {
                typ: Type::String,
                target: Box::new(TypedAstNode::Identifier(
                    ident_token!((3, 1), "p"),
                    TypedIdentifierNode {
                        typ: Type::Struct { name: "Person".to_string(), fields: vec![("name".to_string(), Type::String)] },
                        scope_depth: 0,
                        is_mutable: false,
                    },
                )),
                field: ident_token!((3, 3), "name"),
            },
        );
        assert_eq!(expected, typed_ast[2]);

        // Getting field of builtin Array type
        let typed_ast = typecheck("[1, 2, 3].length")?;
        let expected = TypedAstNode::Accessor(
            Token::Dot(Position::new(1, 10)),
            TypedAccessorNode {
                typ: Type::Int,
                target: Box::new(TypedAstNode::Array(
                    Token::LBrack(Position::new(1, 1)),
                    TypedArrayNode {
                        typ: Type::Array(Box::new(Type::Int)),
                        items: vec![
                            Box::new(int_literal!((1, 2), 1)),
                            Box::new(int_literal!((1, 5), 2)),
                            Box::new(int_literal!((1, 8), 3)),
                        ],
                    },
                )),
                field: ident_token!((1, 11), "length"),
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
                field: ident_token!((1, 9), "length"),
            },
        );
        assert_eq!(expected, typed_ast[0]);

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
            target_type: Type::Struct { name: "Person".to_string(), fields: vec![("name".to_string(), Type::String)] },
        };
        assert_eq!(expected, error);

        let error = typecheck("true.value").unwrap_err();
        let expected = TypecheckerError::UnknownMember {
            token: ident_token!((1, 6), "value"),
            target_type: Type::Bool,
        };
        assert_eq!(expected, error);
    }
}
