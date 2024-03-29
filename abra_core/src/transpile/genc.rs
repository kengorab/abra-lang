use std::collections::{HashMap, HashSet, VecDeque};
use std::path::PathBuf;
use itertools::Itertools;
use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::common::util::random_string;
use crate::lexer::tokens::Token;
use crate::{ModuleId, ModuleLoader, ModuleReader};
use crate::parser::ast::{BinaryOp, BindingPattern, IndexingMode, UnaryOp};
use crate::typechecker::typechecker::ExportedValue;
use crate::typechecker::typed_ast::{TypedAccessorNode, TypedArrayNode, TypedAssignmentNode, TypedAstNode, TypedBinaryNode, TypedBindingDeclNode, TypedEnumDeclNode, TypedForLoopNode, TypedFunctionDeclNode, TypedGroupedNode, TypedIdentifierNode, TypedIfNode, TypedImportNode, TypedIndexingNode, TypedInstantiationNode, TypedInvocationNode, TypedLambdaNode, TypedLiteralNode, TypedMapNode, TypedMatchCaseArgument, TypedMatchKind, TypedMatchNode, TypedReturnNode, TypedSetNode, TypedTupleNode, TypedTypeDeclNode, TypedUnaryNode, TypedWhileLoopNode};
use crate::typechecker::types::Type;

#[derive(Clone)]
enum BufferType {
    MainFn,
    FwdDecls,
    Body,
}

#[derive(Debug)]
struct Scope {
    name: String,
    bindings: HashMap<String, String>,
}

pub struct CCompiler<'a, R: ModuleReader> {
    includes_buf: String,
    main_fn_buf: String,
    fwd_decls_buf: String,
    body_buf: String,
    buf_type: BufferType,
    module_name: String,
    scopes: Vec<Scope>,
    c_type_names: HashMap<String, String>,
    module_loader: &'a ModuleLoader<'a, R>,
    root_dir: &'a PathBuf,

    if_result_var_names_stack: Vec<VecDeque<String>>,
    match_result_var_names_stack: Vec<VecDeque<String>>,
    array_literal_var_names_stack: Vec<VecDeque<String>>,
    tuple_literal_var_names_stack: Vec<VecDeque<String>>,
    map_literal_var_names_stack: Vec<VecDeque<String>>,
    set_literal_var_names_stack: Vec<VecDeque<String>>,
    invocation_var_names_queue: VecDeque<VecDeque<String>>,
    accessor_var_names_stack: Vec<VecDeque<String>>,
    upvalues_by_fn_cname: HashMap<String, HashSet<String>>,
}

#[derive(Clone, Debug)]
enum FnKind {
    Fn,
    Method(/* type_name: */ String),
    StaticMethod(/* type_name: */ String),
}

#[derive(Clone, Debug)]
enum FunctionLike {
    FunctionDecl(FnKind, TypedFunctionDeclNode),
    Lambda(TypedLambdaNode),
}

fn lambda_name(lambda_node: &TypedLambdaNode) -> String {
    format!("lambda_{}", lambda_node.idx)
}

fn extract_functions(
    node: &TypedAstNode,
    known_vars: &Vec<String>,
    path: Vec<String>,
    fn_kind: FnKind,
    seen_fns: &mut Vec<(Vec<String>, String, FunctionLike, HashSet<String>)>,
) {
    #[inline]
    fn walk_and_find_vars(node: &TypedAstNode, vars: &mut Vec<String>) {
        match node {
            TypedAstNode::Literal(_, _) => {}
            TypedAstNode::Unary(_, n) => walk_and_find_vars(&n.expr, vars),
            TypedAstNode::Binary(_, n) => {
                walk_and_find_vars(&n.left, vars);
                walk_and_find_vars(&n.right, vars);
            }
            TypedAstNode::Grouped(_, n) => walk_and_find_vars(&n.expr, vars),
            TypedAstNode::Array(_, n) => {
                for item in &n.items {
                    walk_and_find_vars(&item, vars);
                }
            }
            TypedAstNode::Map(_, n) => {
                for (key, val) in &n.items {
                    walk_and_find_vars(&key, vars);
                    walk_and_find_vars(&val, vars);
                }
            }
            TypedAstNode::Set(_, n) => {
                for item in &n.items {
                    walk_and_find_vars(&item, vars);
                }
            }
            TypedAstNode::Tuple(_, n) => {
                for item in &n.items {
                    walk_and_find_vars(&item, vars);
                }
            }
            TypedAstNode::Lambda(_, _) => {}
            TypedAstNode::BindingDecl(_, n) => {
                if let Some(expr) = &n.expr {
                    walk_and_find_vars(&expr, vars);
                }
            }
            TypedAstNode::FunctionDecl(_, _) => {}
            TypedAstNode::TypeDecl(_, _) => {}
            TypedAstNode::EnumDecl(_, _) => {}
            TypedAstNode::Identifier(_, n) => {
                if let Type::Fn(_) = &n.typ {
                    // Do nothing
                } else {
                    vars.push(n.name.clone())
                }
            }
            TypedAstNode::Assignment(_, n) => {
                walk_and_find_vars(&n.target, vars);
                walk_and_find_vars(&n.expr, vars);
            }
            TypedAstNode::Indexing(_, n) => {
                walk_and_find_vars(&n.target, vars);
                match &n.index {
                    IndexingMode::Index(i) => walk_and_find_vars(&i, vars),
                    IndexingMode::Range(s, e) => {
                        if let Some(s) = s {
                            walk_and_find_vars(&s, vars);
                        }
                        if let Some(e) = e {
                            walk_and_find_vars(&e, vars);
                        }
                    }
                }
            }
            TypedAstNode::IfStatement(_, n) |
            TypedAstNode::IfExpression(_, n) => {
                walk_and_find_vars(&n.condition, vars);

                fn visit_binding_pattern(vars: &mut Vec<String>, pat: &BindingPattern) {
                    match pat {
                        BindingPattern::Variable(v) => {
                            vars.push(Token::get_ident_name(v));
                        }
                        BindingPattern::Tuple(_, pats) => {
                            for pat in pats {
                                visit_binding_pattern(vars, pat);
                            }
                        }
                        BindingPattern::Array(_, pats, _) => {
                            for (pat, _) in pats {
                                visit_binding_pattern(vars, pat);
                            }
                        }
                    }
                }

                let mut binding_vars = vec![];
                if let Some(binding) = &n.condition_binding {
                    visit_binding_pattern(&mut binding_vars, &binding);
                }
                for node in &n.if_block {
                    walk_and_find_vars(&node, vars);
                    for v in &binding_vars {
                        if let Some((idx, _)) = vars.iter().find_position(|var| var == &v) {
                            vars.remove(idx);
                        }
                    }
                }
                if let Some(else_block) = &n.else_block {
                    for node in else_block {
                        walk_and_find_vars(&node, vars);
                    }
                }
            }
            TypedAstNode::Invocation(_, n) => {
                walk_and_find_vars(&n.target, vars);
                for arg in &n.args {
                    if let Some(arg) = arg {
                        walk_and_find_vars(&arg, vars);
                    }
                }
            }
            TypedAstNode::Accessor(_, n) => walk_and_find_vars(&n.target, vars),
            TypedAstNode::ForLoop(_, n) => {
                walk_and_find_vars(&n.iterator, vars);
            }
            TypedAstNode::Instantiation(_, _) |
            TypedAstNode::WhileLoop(_, _) |
            TypedAstNode::Break(_) |
            TypedAstNode::Continue(_) |
            TypedAstNode::ReturnStatement(_, _) |
            TypedAstNode::MatchStatement(_, _) |
            TypedAstNode::MatchExpression(_, _) |
            TypedAstNode::ImportStatement(_, _) |
            TypedAstNode::_Nil(_) => {}
        }
    }

    let (fn_name, args, body, f) = match node {
        TypedAstNode::FunctionDecl(_, decl_node) => {
            let fn_name = Token::get_ident_name(&decl_node.name);
            (fn_name, decl_node.args.clone(), &decl_node.body, FunctionLike::FunctionDecl(fn_kind, decl_node.clone()))
        }
        TypedAstNode::Lambda(_, lambda_node) => {
            let args = lambda_node.args.clone().into_iter()
                .map(|(tok, typ, default_value)| (tok, typ, false, default_value))
                .collect();
            let body = lambda_node.typed_body
                .as_ref()
                .expect("Any lambdas requiring retyping should have already been retyped");
            (lambda_name(&lambda_node), args, body, FunctionLike::Lambda(lambda_node.clone()))
        }
        TypedAstNode::BindingDecl(_, n) => {
            if let Some(expr) = &n.expr {
                extract_functions(&expr, known_vars, path, FnKind::Fn, seen_fns);
            }
            return;
        }
        TypedAstNode::Unary(_, n) => {
            extract_functions(&n.expr, known_vars, path, FnKind::Fn, seen_fns);
            return;
        }
        TypedAstNode::Binary(_, n) => {
            extract_functions(&n.left, known_vars, path.clone(), FnKind::Fn, seen_fns);
            extract_functions(&n.right, known_vars, path, FnKind::Fn, seen_fns);
            return;
        }
        TypedAstNode::Grouped(_, n) => {
            extract_functions(&n.expr, known_vars, path, FnKind::Fn, seen_fns);
            return;
        }
        TypedAstNode::Array(_, n) => {
            for item in &n.items {
                extract_functions(&item, known_vars, path.clone(), FnKind::Fn, seen_fns);
            }
            return;
        }
        TypedAstNode::Map(_, n) => {
            for (key, val) in &n.items {
                extract_functions(&key, known_vars, path.clone(), FnKind::Fn, seen_fns);
                extract_functions(&val, known_vars, path.clone(), FnKind::Fn, seen_fns);
            }
            return;
        }
        TypedAstNode::Set(_, n) => {
            for item in &n.items {
                extract_functions(&item, known_vars, path.clone(), FnKind::Fn, seen_fns);
            }
            return;
        }
        TypedAstNode::Tuple(_, n) => {
            for item in &n.items {
                extract_functions(&item, known_vars, path.clone(), FnKind::Fn, seen_fns);
            }
            return;
        }
        TypedAstNode::TypeDecl(_, n) => {
            for field in &n.fields {
                if let Some(default_value) = &field.default_value {
                    extract_functions(&default_value, known_vars, path.clone(), FnKind::Fn, seen_fns);
                }
            }
            for (_, typ, node) in &n.static_fields {
                if let Type::Fn(_) = &typ {
                    let method_node = node.as_ref().unwrap();
                    extract_functions(method_node, known_vars, path.clone(), FnKind::StaticMethod(Token::get_ident_name(&n.name)), seen_fns);
                } else { todo!() }
            }
            for (_, method_node) in &n.methods {
                extract_functions(&method_node, known_vars, path.clone(), FnKind::Method(Token::get_ident_name(&n.name)), seen_fns);
            }
            return;
        }
        TypedAstNode::EnumDecl(_, n) => {
            for (_, typ, node) in &n.static_fields {
                if let Type::Fn(_) = &typ {
                    let method_node = node.as_ref().unwrap();
                    extract_functions(method_node, known_vars, path.clone(), FnKind::StaticMethod(Token::get_ident_name(&n.name)), seen_fns);
                } else { todo!() }
            }
            for (_, method_node) in &n.methods {
                extract_functions(&method_node, known_vars, path.clone(), FnKind::Method(Token::get_ident_name(&n.name)), seen_fns);
            }
            return;
        }
        TypedAstNode::Identifier(_, _) => { return; }
        TypedAstNode::Assignment(_, n) => {
            extract_functions(&n.expr, known_vars, path, FnKind::Fn, seen_fns);
            return;
        }
        TypedAstNode::Indexing(_, n) => {
            extract_functions(&n.target, known_vars, path.clone(), FnKind::Fn, seen_fns);
            match &n.index {
                IndexingMode::Index(i) => {
                    extract_functions(&i, known_vars, path, FnKind::Fn, seen_fns);
                }
                IndexingMode::Range(s, e) => {
                    if let Some(s) = s {
                        extract_functions(&s, known_vars, path.clone(), FnKind::Fn, seen_fns);
                    }
                    if let Some(e) = e {
                        extract_functions(&e, known_vars, path, FnKind::Fn, seen_fns);
                    }
                }
            }
            return;
        }
        TypedAstNode::IfStatement(_, n) |
        TypedAstNode::IfExpression(_, n) => {
            extract_functions(&n.condition, known_vars, path.clone(), FnKind::Fn, seen_fns);
            for node in &n.if_block {
                extract_functions(&node, known_vars, path.clone(), FnKind::Fn, seen_fns);
            }
            if let Some(else_block) = &n.else_block {
                for node in else_block {
                    extract_functions(&node, known_vars, path.clone(), FnKind::Fn, seen_fns);
                }
            }
            return;
        }
        TypedAstNode::Invocation(_, n) => {
            extract_functions(&n.target, known_vars, path.clone(), FnKind::Fn, seen_fns);
            for arg in &n.args {
                if let Some(arg) = arg {
                    extract_functions(&arg, known_vars, path.clone(), FnKind::Fn, seen_fns);
                }
            }
            return;
        }
        TypedAstNode::ReturnStatement(_, n) => {
            if let Some(n) = &n.target {
                extract_functions(&n, known_vars, path, FnKind::Fn, seen_fns);
            }
            return;
        }
        TypedAstNode::Accessor(_, n) => {
            extract_functions(&n.target, known_vars, path, FnKind::Fn, seen_fns);
            return;
        }
        TypedAstNode::MatchStatement(_, n) |
        TypedAstNode::MatchExpression(_, n) => {
            extract_functions(&n.target, known_vars, path.clone(), FnKind::Fn, seen_fns);
            for (_kind, _, body) in &n.branches {
                for n in body {
                    extract_functions(&n, known_vars, path.clone(), FnKind::Fn, seen_fns);
                }
            }
            return;
        }
        TypedAstNode::WhileLoop(_, n) => {
            extract_functions(&n.condition, known_vars, path.clone(), FnKind::Fn, seen_fns);
            for n in &n.body {
                extract_functions(&n, known_vars, path.clone(), FnKind::Fn, seen_fns);
            }
            return;
        }
        TypedAstNode::Instantiation(_, n) => {
            for (_, n) in &n.fields {
                extract_functions(&n, known_vars, path.clone(), FnKind::Fn, seen_fns);
            }
            return;
        }
        TypedAstNode::ForLoop(_, _) |
        TypedAstNode::Literal(_, _) |
        TypedAstNode::Break(_) |
        TypedAstNode::Continue(_) |
        TypedAstNode::ImportStatement(_, _) |
        TypedAstNode::_Nil(_) => return
    };

    let mut current_known_vars = known_vars.clone();
    current_known_vars.push(fn_name.clone());

    let mut seen_vars = Vec::new();
    for (name, _, _, default_value) in args {
        current_known_vars.push(Token::get_ident_name(&name));
        if let Some(default_value_node) = default_value {
            walk_and_find_vars(&default_value_node, &mut seen_vars);
        }
    }

    fn visit_binding_pattern(current_known_vars: &mut Vec<String>, pat: &BindingPattern) {
        match pat {
            BindingPattern::Variable(v) => {
                current_known_vars.push(Token::get_ident_name(v));
            }
            BindingPattern::Tuple(_, pats) => {
                for pat in pats {
                    visit_binding_pattern(current_known_vars, pat);
                }
            }
            BindingPattern::Array(_, pats, _) => {
                for (pat, _) in pats {
                    visit_binding_pattern(current_known_vars, pat);
                }
            }
        }
    }

    for node in body {
        match &node {
            TypedAstNode::FunctionDecl(_, _) |
            TypedAstNode::Lambda(_, _) => {
                let mut sub_path = path.clone();
                sub_path.push(fn_name.clone());
                extract_functions(&node, &known_vars, sub_path, FnKind::Fn, seen_fns);

                let (_, _, _, seen_vars_in_fn) = seen_fns.last().unwrap();
                let mut seen_vars_in_fn = seen_vars_in_fn.clone().into_iter().collect();
                seen_vars.append(&mut seen_vars_in_fn);
            }
            TypedAstNode::BindingDecl(_, n) => {
                extract_functions(&node, &known_vars, path.clone(), FnKind::Fn, seen_fns);
                visit_binding_pattern(&mut current_known_vars, &n.binding);
                if let Some(expr) = &n.expr {
                    walk_and_find_vars(&expr, &mut seen_vars);
                }
            }
            TypedAstNode::IfStatement(_, if_node) |
            TypedAstNode::IfExpression(_, if_node) => {
                walk_and_find_vars(&node, &mut seen_vars);
                extract_functions(&if_node.condition, &seen_vars, path.clone(), FnKind::Fn, seen_fns);

                if let Some(condition_binding) = &if_node.condition_binding {
                    visit_binding_pattern(&mut current_known_vars, condition_binding);
                }

                for node in &if_node.if_block {
                    walk_and_find_vars(&node, &mut seen_vars);
                    extract_functions(&node, &seen_vars, path.clone(), FnKind::Fn, seen_fns);
                }
                if let Some(else_block) = &if_node.else_block {
                    for node in else_block {
                        walk_and_find_vars(&node, &mut seen_vars);
                        extract_functions(&node, &seen_vars, path.clone(), FnKind::Fn, seen_fns);
                    }
                }
            }
            _ => {
                walk_and_find_vars(&node, &mut seen_vars);
                extract_functions(&node, &known_vars, path.clone(), FnKind::Fn, seen_fns);
            }
        }
    }

    let closed_over_variables = seen_vars.iter()
        .filter(|var| !current_known_vars.contains(var))
        .map(|var| var.clone())
        .collect::<HashSet<_>>();
    seen_fns.push((path, fn_name, f, closed_over_variables))
}

pub fn normalize_module_name(abs_path: &String, root_dir: &PathBuf) -> String {
    let non_normalized = PathBuf::from(abs_path).strip_prefix(root_dir).unwrap().to_str().unwrap().to_string();
    non_normalized
        .replace(".", "dot")
        .replace("/", "__")
        .replace("-", "_")
}

impl<'a, R: ModuleReader> CCompiler<'a, R> {
    fn new(module_loader: &'a mut ModuleLoader<R>, root_dir: &'a PathBuf, module_name: String) -> Self {
        let root_scope = Scope {
            name: module_name.clone(),
            bindings: {
                let mut m = HashMap::new();
                m.insert("print".to_string(), "std__print_val".to_string());
                m.insert("println".to_string(), "std__println_val".to_string());
                m.insert("range".to_string(), "std__range_val".to_string());
                m.insert("None".to_string(), "ABRA_NONE".to_string());

                // TODO: Fix when implementing type values
                m.insert("String".to_string(), "ABRA_NONE".to_string());
                m.insert("Array".to_string(), "ABRA_NONE".to_string());
                m.insert("Map".to_string(), "ABRA_NONE".to_string());
                m.insert("Result".to_string(), "ABRA_NONE".to_string());

                m
            },
        };

        CCompiler {
            includes_buf: "".to_string(),
            main_fn_buf: "".to_string(),
            fwd_decls_buf: "".to_string(),
            body_buf: "".to_string(),
            buf_type: BufferType::MainFn,
            module_name,
            scopes: vec![root_scope],
            c_type_names: {
                let mut m = HashMap::new();
                m.insert("std__Array".to_string(), "std__Array".to_string());
                m.insert("std__Map".to_string(), "std__Map".to_string());
                m.insert("std__Result".to_string(), "std__Result".to_string());
                m
            },
            module_loader,
            root_dir,

            if_result_var_names_stack: vec![VecDeque::new()],
            match_result_var_names_stack: vec![VecDeque::new()],
            array_literal_var_names_stack: vec![VecDeque::new()],
            tuple_literal_var_names_stack: vec![VecDeque::new()],
            map_literal_var_names_stack: vec![VecDeque::new()],
            set_literal_var_names_stack: vec![VecDeque::new()],
            invocation_var_names_queue: {
                let mut q = VecDeque::new();
                q.push_back(VecDeque::new());
                q
            },
            accessor_var_names_stack: vec![VecDeque::new()],
            upvalues_by_fn_cname: HashMap::new(),
        }
    }

    pub fn gen_c(module_loader: &mut ModuleLoader<R>, root: &PathBuf, module_name: &String, ast: Vec<TypedAstNode>) -> Result<String, ()> {
        let mut compiler = CCompiler::new(module_loader, root, module_name.clone());

        let (imports, non_imports) = ast.into_iter()
            .partition(|node| if let TypedAstNode::ImportStatement(_, _) = node { true } else { false });
        let ast = non_imports;

        compiler.switch_buf(BufferType::MainFn);
        compiler.emit_line(format!("int init_module_{}() {{", &module_name));

        for import_node in imports {
            compiler.visit(import_node)?;
        }

        compiler.preregister_global_bindings(&ast);
        compiler.lift_types(&ast)?;

        compiler.switch_buf(BufferType::Body);
        compiler.lift_fns(&ast)?;
        compiler.switch_buf(BufferType::MainFn);

        let ast_len = ast.len();
        for (idx, node) in ast.into_iter().enumerate() {
            compiler.lift(&node)?;

            let should_print = idx == ast_len - 1 && node.get_type() != Type::Unit;
            if should_print {
                let salt = random_string(10);
                compiler.emit(format!("const char* last_expr_{} = std__to_string(", salt));
                compiler.visit(node)?;
                compiler.emit_line(");");
                compiler.emit_line(format!("printf(\"%s\\n\", last_expr_{});", salt));
            } else {
                compiler.visit(node)?;
                compiler.emit_line(";");
            }
        }

        compiler.emit_line("  return 0;\n}");

        let output = format!(
            "{}\n{}\n{}\n{}",
            compiler.includes_buf,
            compiler.fwd_decls_buf,
            compiler.body_buf,
            compiler.main_fn_buf,
        );
        Ok(output)
    }

    fn find_c_var_name<S: AsRef<str>>(&self, name: S) -> Option<String> {
        self.scopes.iter().rev().find_map(|s| s.bindings.get(name.as_ref()).map(|n| n.clone()))
    }

    fn find_c_var_name_trim_suffix<S1: AsRef<str>, S2: AsRef<str>>(&self, name: S1, suffix: S2) -> Option<String> {
        let c_name = self.scopes.iter().rev().find_map(|s| s.bindings.get(name.as_ref()).map(|n| n.clone()));
        c_name.map(|mut c_name| {
            debug_assert!(c_name.ends_with(suffix.as_ref()));
            if c_name.ends_with(suffix.as_ref()) {
                c_name.truncate(c_name.len() - suffix.as_ref().len());
            }
            c_name
        })
    }

    fn add_c_var_name<S: AsRef<str>>(&mut self, name: S) -> String {
        self.add_c_var_name_with_suffix(name, "")
    }

    fn add_c_var_name_with_suffix<S1: AsRef<str>, S2: AsRef<str>>(&mut self, name: S1, suffix: S2) -> String {
        let prefix = self.scopes.iter().map(|Scope { name, .. }| name).join("_");

        let c_name = format!("{}__{}{}", prefix, name.as_ref(), suffix.as_ref());
        self.insert_binding(name, &c_name);
        c_name
    }

    fn insert_binding<S1: AsRef<str>, S2: AsRef<str>>(&mut self, name: S1, binding_name: S2) {
        self.scopes.last_mut().expect("There's always at least 1 scope")
            .bindings
            .insert(name.as_ref().to_string(), binding_name.as_ref().to_string());
    }

    fn c_name_for_type<S: AsRef<str>>(&self, type_name: S) -> String {
        format!("{}__{}", &self.module_name, type_name.as_ref())
    }

    fn c_name_for_type_in_module<S1: AsRef<str>, S2: AsRef<str>>(mod_name: S1, type_name: S2) -> String {
        format!("{}__{}", mod_name.as_ref(), type_name.as_ref())
    }

    fn c_name_for_builtin_type(typ: &Type) -> String {
        let name = match typ {
            Type::Int => "Int",
            Type::Float => "Float",
            Type::String => "String",
            Type::Array(_) => "Array",
            Type::Map(_, _) => "Map",
            Type::Set(_) => "Set",
            _ => unreachable!()
        };
        format!("std__{}", name)
    }

    fn switch_buf(&mut self, buf_type: BufferType) -> BufferType {
        let mut old_buf = buf_type;
        std::mem::swap(&mut self.buf_type, &mut old_buf);
        old_buf
    }

    fn buf(&mut self) -> &mut String {
        match self.buf_type {
            BufferType::MainFn => &mut self.main_fn_buf,
            BufferType::FwdDecls => &mut self.fwd_decls_buf,
            BufferType::Body => &mut self.body_buf,
        }
    }

    fn emit<S: AsRef<str>>(&mut self, code: S) {
        self.buf().push_str(code.as_ref())
    }

    fn emit_line<S: AsRef<str>>(&mut self, code: S) {
        self.buf().push_str(code.as_ref());
        self.buf().push('\n');
    }

    fn emit_finalize_function_value(&mut self, arity: usize, fn_name: &String, c_name: &String) {
        let ctx_type_name = format!("callable_ctx__{}_t", arity);
        self.emit_line(format!("{}* {}_env_ctx = GC_MALLOC(sizeof({}));", &ctx_type_name, &c_name, &ctx_type_name));
        self.emit_line(format!("{}_env_ctx->fn = &{};", &c_name, &c_name));

        let upvalues = self.upvalues_by_fn_cname.get(c_name).map(|uvs| uvs.clone());
        match &upvalues {
            Some(upvalues) if !upvalues.is_empty() => {
                self.emit_line(format!("{}_env_t* {}_env = GC_MALLOC(sizeof({}_env_t));", &c_name, &c_name, &c_name));
                for upvalue_name in upvalues.iter() {
                    self.emit_line(format!("{}_env->{} = {};", &c_name, upvalue_name, self.find_c_var_name(&upvalue_name).expect(&format!("{}", upvalue_name))));
                }
                self.emit_line(format!("{}_env_ctx->env = {}_env;", &c_name, &c_name));
            }
            _ => {
                self.emit_line(format!("{}_env_ctx->env = NULL;", &c_name));
            }
        }
        self.emit_line(format!("{}_val = alloc_function(\"{}\", \"{}\", (void*) {}_env_ctx);", &c_name, &fn_name, &c_name, &c_name));
    }

    fn preregister_global_bindings(&mut self, ast: &Vec<TypedAstNode>) {
        #[inline]
        fn extract_binding_names(pat: &BindingPattern, names: &mut Vec<String>) {
            match pat {
                BindingPattern::Variable(t) => names.push(Token::get_ident_name(t)),
                BindingPattern::Tuple(_, pats) => {
                    pats.iter().for_each(|pat| extract_binding_names(pat, names))
                }
                BindingPattern::Array(_, pats, _) => {
                    pats.iter().for_each(|(pat, _)| extract_binding_names(pat, names))
                }
            }
        }

        for node in ast {
            if let TypedAstNode::BindingDecl(_, n) = &node {
                let mut names = vec![];
                extract_binding_names(&n.binding, &mut names);
                for name in names {
                    self.add_c_var_name(name);
                }
            }
        }
    }

    fn lift_types(&mut self, ast: &Vec<TypedAstNode>) -> Result<(), ()> {
        let prev_buf = self.switch_buf(BufferType::Body);

        for node in ast {
            match &node {
                TypedAstNode::EnumDecl(_, n) => {
                    let enum_name = Token::get_ident_name(&n.name);
                    let c_name = self.add_c_var_name(&enum_name);
                    self.c_type_names.insert(c_name.clone(), c_name.clone()); // A type defined within the current module "aliases" to itself

                    self.emit_line(format!("// Begin declare {}", &c_name));
                    let var_names = n.variants.iter().map(|(tok, _)| Token::get_ident_name(tok)).join(",");
                    for clause in &["INIT", "DEFINE"] {
                        self.emit_line(format!("ABRA_{}_ENUM({}, {}, {})", &clause, &self.module_name, &enum_name, &var_names));
                        for (var_name, (var_type, _)) in &n.variants {
                            let var_name = Token::get_ident_name(var_name);

                            if let Type::Fn(fn_type) = var_type {
                                let variant_field_names = fn_type.arg_types.iter().map(|(arg_name, _, _)| arg_name).join(",");
                                self.emit_line(format!(
                                    "ABRA_{}_ENUM_VARIANT({}, {}, {}, {})", &clause,
                                    &self.module_name, &enum_name, &var_name, variant_field_names
                                ));
                            } else {
                                self.emit_line(format!(
                                    "ABRA_{}_ENUM_VARIANT_0({}, {}, {})", &clause,
                                    &self.module_name, &enum_name, &var_name
                                ));
                            }
                        }
                    }
                    self.emit_line(format!("// End declare {}\n", &c_name));

                    self.switch_buf(BufferType::MainFn);
                    self.emit_line(format!("TYPE_SETUP({}, {});", &self.module_name, &enum_name));
                    for (var_name, (var_type, _)) in &n.variants {
                        let var_name = Token::get_ident_name(var_name);
                        let arity = if let Type::Fn(fn_type) = var_type { fn_type.arg_types.len() } else { 0 };
                        self.emit_line(format!(
                           "ENUM_VARIANT_SETUP({}, {}, {}, {})",
                            &self.module_name, &enum_name, var_name, arity
                        ));
                    }
                    self.switch_buf(BufferType::Body);
                }
                TypedAstNode::TypeDecl(_, node) => {
                    let type_name = Token::get_ident_name(&node.name);
                    let c_name = self.add_c_var_name(&type_name);
                    self.c_type_names.insert(c_name.clone(), c_name.clone()); // A type defined within the current module "aliases" to itself

                    if node.fields.is_empty() {
                        self.emit_line(format!(
                            "ABRA_DEFINE_TYPE_0({}, {});",
                            &self.module_name, &type_name,
                        ));
                    } else {
                        self.emit_line(format!(
                            "ABRA_DEFINE_TYPE({}, {}, {});",
                            &self.module_name, &type_name,
                            node.fields.iter()
                                .map(|f| Token::get_ident_name(&f.ident))
                                .join(","),
                        ));
                    }

                    self.switch_buf(BufferType::MainFn);
                    self.emit_line(format!("TYPE_SETUP({}, {});", &self.module_name, &type_name));
                    self.switch_buf(BufferType::Body);
                },
                _ => continue,
            }
        }

        self.switch_buf(prev_buf);

        Ok(())
    }

    fn lift_fns(&mut self, ast: &Vec<TypedAstNode>) -> Result<(), ()> {
        let mut seen_fns = Vec::new();
        for node in ast {
            let mut known_vars = self.scopes.first().unwrap().bindings.keys().map(|v| v.clone()).collect();
            extract_functions(&node, &mut known_vars, vec![], FnKind::Fn, &mut seen_fns);
        }

        let mut fns_at_path = HashMap::new();
        for (path, fn_name, f, closed_over_vars) in &seen_fns {
            let c_name = match f {
                FunctionLike::FunctionDecl(fn_kind, _) => {
                    let mod_name = self.scopes.first().unwrap().name.clone();
                    let (fn_ident, c_name) = match fn_kind {
                        FnKind::Fn => {
                            let fn_ident = fn_name.clone();
                            let mut c_name = vec![vec![mod_name], path.clone()].concat().iter().join("_");
                            c_name.push_str("__");
                            c_name.push_str(&fn_ident);
                            (fn_ident, c_name)
                        }
                        FnKind::Method(type_name) => {
                            let fn_ident = format!("{}__method_{}", type_name, &fn_name);
                            let c_name = format!("{}__{}", mod_name, &fn_ident);
                            (fn_ident, c_name)
                        }
                        FnKind::StaticMethod(type_name) => {
                            let fn_ident = format!("{}__static_method_{}", type_name, &fn_name);
                            let c_name = format!("{}__{}", mod_name, &fn_ident);
                            (fn_ident, c_name)
                        }
                    };
                    fns_at_path.entry(path.clone()).or_insert(vec![]).push(fn_ident);
                    c_name
                }
                FunctionLike::Lambda(lambda) => {
                    let name = lambda_name(&lambda);
                    fns_at_path.entry(vec![]).or_insert(vec![]).push(name.clone());
                    format!("{}__{}", self.scopes.first().unwrap().name, &name)
                }
            };

            self.upvalues_by_fn_cname.insert(c_name, closed_over_vars.clone());
        }

        // Add cvar name for all root-level fns
        if let Some(fns_at_path) = fns_at_path.get(&vec![]) {
            for name in fns_at_path {
                self.add_c_var_name_with_suffix(&name, "_val");
            }
        }

        // Emit code for each fn, making sure to set the proper intermediate scopes, as well as
        // pre-define a cvar for each other fn in that scope (to handle out-of-order references).
        for (path, fn_name, f, vars) in seen_fns {
            for scope_name in &path {
                self.scopes.push(Scope { name: scope_name.clone(), bindings: HashMap::new() });
            }

            if let Some(fns_at_path) = fns_at_path.get(&path) {
                for name in fns_at_path {
                    self.add_c_var_name_with_suffix(&name, "_val");
                }
            }

            self.lift_fn(&fn_name, &f, &vars)?;

            for _ in 0..path.len() {
                self.scopes.pop();
            }
        }

        Ok(())
    }

    fn lift_fn(&mut self, fn_name: &String, node: &FunctionLike, closed_over_vars: &HashSet<String>) -> Result<(), ()> {
        let node = node.clone(); // :/
        let (fn_name, c_name, args, body) = match node {
            FunctionLike::FunctionDecl(fn_kind, node) => {
                let fn_name = match fn_kind {
                    FnKind::Fn => fn_name.clone(),
                    FnKind::Method(type_name) => format!("{}__method_{}", type_name, &fn_name),
                    FnKind::StaticMethod(type_name) => format!("{}__static_method_{}", type_name, &fn_name),
                };
                let c_name = self.find_c_var_name_trim_suffix(&fn_name, "_val").expect(&format!("Could not find c-variable name for {}", fn_name));
                (fn_name.clone(), c_name, node.args, node.body)
            }
            FunctionLike::Lambda(node) => {
                let fn_name = lambda_name(&node);
                let c_name = format!("{}__{}", self.scopes.first().unwrap().name, &fn_name);
                let args = node.args.clone().into_iter()
                    .map(|(tok, typ, default_value)| (tok, typ, false, default_value))
                    .collect();
                let body = node.typed_body
                    .expect("Any lambdas requiring retyping should have already been retyped");
                (fn_name, c_name, args, body)
            }
        };

        self.scopes.push(Scope { name: fn_name.clone(), bindings: HashMap::new() });

        let prev_buf = self.switch_buf(BufferType::FwdDecls);
        let env_struct_name = if !closed_over_vars.is_empty() {
            let env_struct_name = format!("{}_env_t", &c_name);
            self.emit_line(format!("typedef struct {} {{", &env_struct_name));
            for var in closed_over_vars {
                self.emit_line(format!("  AbraValue {};", var));
            }
            self.emit_line(format!("}} {};", &env_struct_name));
            Some(env_struct_name)
        } else { None };

        let sig_args = args.iter()
            .map(|(name, _, _, _)| self.add_c_var_name(Token::get_ident_name(name)))
            .map(|name| format!("AbraValue {}", name))
            .join(", ");
        let sig = format!("AbraValue {}(void* _env{}{})", &c_name, if args.is_empty() { "" } else { "," }, sig_args);
        self.emit_line(format!("{};", &sig));
        self.emit_line(format!("AbraValue {}_val;", &c_name));

        self.switch_buf(BufferType::Body);
        self.emit_line(format!("{} {{", sig));
        if let Some(env_struct_name) = &env_struct_name {
            self.emit_line(format!("{}* env = ({}*)_env;", env_struct_name, env_struct_name));
        }

        for var_name in closed_over_vars {
            self.scopes.last_mut().expect("There's always at least 1 scope")
                .bindings
                .insert(var_name.clone(), format!("env->{}", var_name));
        }

        for (name, _, _, default_value) in args {
            let arg_name = Token::get_ident_name(&name);
            let c_name = self.add_c_var_name(&arg_name);

            if let Some(default_value_node) = default_value {
                self.emit_line(format!("if (IS_NONE({})) {{", &c_name));
                self.lift(&default_value_node)?;
                self.emit(format!("{} = ", c_name));
                self.visit(default_value_node)?;
                self.emit_line(";");
                self.emit_line("}");
            }
        }

        let len = body.len();
        for (idx, node) in body.into_iter().enumerate() {
            self.lift(&node)?;

            if idx == len - 1 && node.is_expression() {
                self.emit("return ");
            }

            // If we see a nested fn, we add a cvar to handle references within this fn's body.
            if let TypedAstNode::FunctionDecl(_, n) = &node {
                let nested_fn_name = Token::get_ident_name(&n.name);
                let c_name = self.add_c_var_name(&nested_fn_name);
                self.scopes.last_mut().unwrap().bindings.insert(nested_fn_name.clone(), format!("{}_val", c_name));
                self.visit(node)?;
            } else {
                self.visit(node)?;
                self.emit_line(";");
            };
        }

        self.emit_line("}");

        self.scopes.pop();
        self.switch_buf(prev_buf);

        Ok(())
    }

    fn lift(&mut self, node: &TypedAstNode) -> Result<(), ()> {
        match node {
            TypedAstNode::Unary(_, node) => {
                self.lift(&node.expr)?;
            }
            TypedAstNode::Binary(_, node) => {
                self.lift(&node.left)?;
                self.lift(&node.right)?;
            }
            TypedAstNode::Grouped(_, node) => {
                self.lift(&node.expr)?;
            }
            TypedAstNode::Array(_, node) => {
                let node = node.clone(); // :/
                let size = node.items.len();

                let salt = random_string(10);
                let items_ident = format!("arr_items_{}", &salt);
                self.emit_line(format!("AbraValue* {} = GC_MALLOC(sizeof(AbraValue) * {});", items_ident, size));
                for (idx, item) in node.items.into_iter().enumerate() {
                    self.array_literal_var_names_stack.push(VecDeque::new());
                    self.lift(&item)?;
                    self.emit(format!("{}[{}] = ", items_ident, idx));
                    self.visit(item)?;
                    self.array_literal_var_names_stack.pop();
                    self.emit_line(";");
                }

                let arr_ident = format!("arr_{}", salt);
                self.array_literal_var_names_stack.last_mut().unwrap().push_back(arr_ident.clone());
                self.emit_line(format!("AbraValue {} = alloc_array({}, {});", &arr_ident, items_ident, size));
            }
            TypedAstNode::Tuple(_, node) => {
                let node = node.clone(); // :/
                let size = node.items.len();

                let salt = random_string(10);
                let items_ident = format!("tuple_items_{}", &salt);
                self.emit_line(format!("AbraValue* {} = GC_MALLOC(sizeof(AbraValue) * {});", items_ident, size));
                for (idx, item) in node.items.into_iter().enumerate() {
                    self.tuple_literal_var_names_stack.push(VecDeque::new());
                    self.lift(&item)?;
                    self.emit(format!("{}[{}] = ", items_ident, idx));
                    self.visit(item)?;
                    self.tuple_literal_var_names_stack.pop();
                    self.emit_line(";");
                }

                let tuple_ident = format!("tuple_{}", salt);
                self.tuple_literal_var_names_stack.last_mut().unwrap().push_back(tuple_ident.clone());
                self.emit_line(format!("AbraValue {} = alloc_tuple({}, {});", &tuple_ident, items_ident, size));
            }
            TypedAstNode::Map(_, node) => {
                let node = node.clone(); // :/
                let salt = random_string(10);
                let map_ident = format!("map_{}", salt);
                self.emit_line(format!("AbraValue {} = alloc_map();", map_ident));

                for (idx, (key, val)) in node.items.into_iter().enumerate() {
                    let key_ident = format!("{}_k{}", map_ident, idx);
                    self.map_literal_var_names_stack.push(VecDeque::new());
                    self.lift(&key)?;
                    self.emit(format!("AbraValue {} = ", &key_ident));
                    self.visit(key)?;
                    self.emit_line(";");
                    self.map_literal_var_names_stack.pop();

                    let val_ident = format!("{}_v{}", map_ident, idx);
                    self.map_literal_var_names_stack.push(VecDeque::new());
                    self.lift(&val)?;
                    self.emit(format!("AbraValue {} = ", &val_ident));
                    self.visit(val)?;
                    self.emit_line(";");
                    self.map_literal_var_names_stack.pop();

                    self.emit_line(format!(
                        "std_map__insert(AS_OBJ({}), {}, {});",
                        map_ident, key_ident, val_ident
                    ));
                }
                self.map_literal_var_names_stack.last_mut().unwrap().push_back(map_ident.clone());
            }
            TypedAstNode::Set(_, node) => {
                let node = node.clone(); // :/
                let salt = random_string(10);
                let set_ident = format!("set_{}", salt);
                self.emit_line(format!("AbraValue {} = alloc_set();", set_ident));

                for val in node.items {
                    self.set_literal_var_names_stack.push(VecDeque::new());
                    self.lift(&val)?;
                    self.set_literal_var_names_stack.pop();

                    self.emit(format!("std_set__insert(AS_OBJ({}), ", set_ident));
                    self.visit(val)?;
                    self.emit_line(");");
                }
                self.set_literal_var_names_stack.last_mut().unwrap().push_back(set_ident.clone());
            }
            TypedAstNode::BindingDecl(_, node) => {
                if let Some(expr) = &node.expr {
                    self.lift(expr)?;
                }
            }
            TypedAstNode::Assignment(_, node) => {
                if let TypedAstNode::Accessor(_, TypedAccessorNode { target, .. }) = &*node.target {
                    self.lift(&*target)?;
                } else {
                    self.lift(&node.target)?;
                }

                self.lift(&node.expr)?;
            }
            TypedAstNode::Indexing(_, node) => {
                self.lift(&node.target)?;
                match &node.index {
                    IndexingMode::Index(i) => self.lift(i)?,
                    IndexingMode::Range(start, end) => {
                        if let Some(start) = start {
                            self.lift(start)?;
                        }
                        if let Some(end) = end {
                            self.lift(end)?;
                        }
                    }
                }
            }
            TypedAstNode::IfExpression(_, node) => {
                self.visit_if_node(node.clone(), true)?;
            }
            TypedAstNode::Invocation(_, node) => {
                let node = node.clone(); // :/
                let arity = node.args.len();

                let is_opt_safe_call = if let Type::Option(_) = node.target.get_type() { true } else { false };

                let salt = random_string(10);
                let result_ident_name = format!("r_inv__{}", &salt);
                self.lift(&node.target)?;
                self.invocation_var_names_queue.back_mut().unwrap().push_back(result_ident_name.clone());

                self.emit_line(format!("AbraValue {} = ABRA_NONE;", result_ident_name));
                let target_ident_name = format!("inv_target__{}", &salt);
                self.emit(format!("AbraValue {} =", &target_ident_name));
                self.visit(*node.target)?;
                self.emit_line(";");

                if is_opt_safe_call {
                    self.emit_line(format!("if (!IS_NONE({})) {{", &target_ident_name));
                }
                let ctx_type_name = format!("callable_ctx__{}_t", arity);
                self.emit_line(format!(
                    "{}* {}_ctx = ({}*) ((AbraFunction*)AS_OBJ({}))->ctx;",
                    &ctx_type_name, &result_ident_name, &ctx_type_name, &target_ident_name
                ));

                self.invocation_var_names_queue.push_back(VecDeque::new());
                for arg in &node.args {
                    if let Some(arg) = arg {
                        self.lift(&arg)?;
                    }
                }

                self.emit(format!("{} = call_fn_{}({}_ctx", result_ident_name, arity, result_ident_name));
                if arity > 0 { self.emit(","); }

                for (idx, arg) in node.args.into_iter().enumerate() {
                    if let Some(arg) = arg {
                        self.visit(arg)?;
                    } else {
                        self.emit("ABRA_NONE");
                    }
                    if idx < arity - 1 {
                        self.emit(",");
                    }
                }
                self.invocation_var_names_queue.pop_back();

                self.emit_line(");");

                if is_opt_safe_call { self.emit_line("}"); }
            }
            TypedAstNode::Instantiation(_, node) => {
                self.lift(&node.target)?;
                for (_, field) in &node.fields {
                    self.lift(&field)?;
                }
            }
            TypedAstNode::ReturnStatement(_, node) => {
                if let Some(target) = &node.target {
                    self.lift(&target)?;
                }
            }
            TypedAstNode::Accessor(_, node) => {
                let node = node.clone();
                let node_typ = node.target.get_type().get_opt_unwrapped();

                let salt = random_string(10);
                let ident_name = format!("acc__{}", &salt);

                let mut is_typeref = false;
                let mut is_static = false;
                let mut enum_variant_init_code = None;
                let prefix= match node_typ {
                    Type::Int | Type::Float | Type::String | Type::Array(_) |
                    Type::Map(_, _) | Type::Set(_) => Self::c_name_for_builtin_type(&node_typ),
                    Type::Type(name, _, _) => {
                        let type_name = name.split("/").last().unwrap();
                        let type_c_name = if name.starts_with("prelude/") {
                            Self::c_name_for_type_in_module("std", &type_name)
                        } else {
                            self.c_name_for_type(&type_name)
                        };
                        let c_name = match self.c_type_names.get(&type_c_name) {
                            Some(c_name) => c_name,
                            None => unimplemented!("Accessing values of type {} not implemented", type_name)
                        };

                        if let Some(Type::Enum(enum_type)) = self.module_loader.resolve_type(&name) {
                            let variant_name = Token::get_ident_name(&node.field_ident);
                            enum_variant_init_code = enum_type.variants.iter()
                                .find_map(|(v_name, v_type)| if v_name == &variant_name { Some(v_type) } else { None })
                                .map(|typ| {
                                    if let Type::Fn(_) = typ {
                                        format!("{}__new_{}_val", &c_name, &variant_name)
                                    } else {
                                        format!("{}__new_{}()", &c_name, &variant_name)
                                    }
                                })
                        }

                        is_static = true;
                        c_name.clone()
                    }
                    Type::Reference(t, _) => {
                        let type_name = t.split("/").last().unwrap();
                        let type_c_name = if type_name == "Result" {
                            Self::c_name_for_type_in_module("std", &type_name)
                        } else {
                            self.c_name_for_type(&type_name)
                        };
                        let c_name = self.c_type_names.get(&type_c_name).expect(&format!("{}", type_c_name));
                        is_typeref = true;
                        c_name.clone()
                    }
                    Type::Struct(t) => {
                        is_typeref = true;
                        let type_c_name = self.c_name_for_type(&t.name);
                        let c_name = self.c_type_names.get(&type_c_name).unwrap();
                        c_name.clone()
                    }
                    t => {
                        dbg!(&t);
                        todo!()
                    },
                };

                self.lift(&node.target)?;
                self.accessor_var_names_stack.last_mut().unwrap().push_back(ident_name.clone());

                if let Some(code) = enum_variant_init_code {
                    self.emit_line(format!("AbraValue {} = {};", &ident_name, code));
                    return Ok(());
                }

                let target_ident_name = format!("target__{}", &salt);
                if !is_static {
                    self.emit(format!("AbraValue {} = ", &target_ident_name));
                    self.visit(*node.target)?;
                    self.emit_line(";");
                }

                self.emit_line(format!("AbraValue {} = ABRA_NONE;", &ident_name));
                if node.is_opt_safe {
                    // TODO: This adds a lot of flat !IS_NONE checks which could otherwise be skipped if they were nested
                    self.emit_line(format!("if (!IS_NONE({})) {{", &target_ident_name));
                }

                self.emit(format!("{} =", &ident_name));
                if node.is_method {
                    let arity = if let Type::Fn(fn_type) = node.typ.get_opt_unwrapped() {
                        fn_type.arg_types.len()
                    } else { unreachable!() };
                    let fn_name = Token::get_ident_name(&node.field_ident);
                    if is_static {
                        let static_method_fn_name = format!("{}__static_method_{}", prefix, &fn_name);
                        self.emit(format!("init_fn_{}(&{}, \"{}\", \"{}\")", arity, &static_method_fn_name, &fn_name, &static_method_fn_name));
                    } else {
                        let method_fn_name = format!("{}__method_{}", prefix, &fn_name);
                        self.emit(format!("bind_fn_{}(&{}, \"{}\", \"{}\", {})", arity, &method_fn_name, &fn_name, &method_fn_name, &target_ident_name));
                    }
                } else {
                    if is_typeref {
                        let field_fn_name = Token::get_ident_name(&node.field_ident);
                        self.emit(format!("(({}*)AS_OBJ({}))->{}", &prefix, &target_ident_name, field_fn_name));
                    } else {
                        let field_fn_name = format!("{}__field_{}", prefix, Token::get_ident_name(&node.field_ident));
                        self.emit(format!("{}({})", field_fn_name, &target_ident_name));
                    }
                }
                self.emit_line(";");

                if node.is_opt_safe { self.emit_line("}"); }
            }
            TypedAstNode::Lambda(_, node) => {
                let node = node.clone(); // :/
                let arity = node.args.len();
                let fn_name = lambda_name(&node);
                let c_name = format!("{}__{}", self.scopes.first().unwrap().name, fn_name);
                self.emit_finalize_function_value(arity, &fn_name, &c_name);
            }
            TypedAstNode::MatchExpression(_, node) => {
                self.visit_match_node(node.clone(), true)?;
            }
            // The following node types cannot contain expressions that need lifting
            TypedAstNode::Literal(_, _) |
            TypedAstNode::FunctionDecl(_, _) |
            TypedAstNode::TypeDecl(_, _) |
            TypedAstNode::EnumDecl(_, _) |
            TypedAstNode::Identifier(_, _) |
            TypedAstNode::ForLoop(_, _) |
            TypedAstNode::WhileLoop(_, _) |
            TypedAstNode::Break(_) |
            TypedAstNode::Continue(_) |
            TypedAstNode::IfStatement(_, _) |
            TypedAstNode::MatchStatement(_, _) |
            TypedAstNode::ImportStatement(_, _) |
            TypedAstNode::_Nil(_) => {}
        };

        Ok(())
    }

    fn visit_if_node(&mut self, node: TypedIfNode, is_expr: bool) -> Result<(), ()> {
        let salt = random_string(10);

        let if_expr_res_ident_name = format!("r_ifexp__{}", salt);
        if is_expr {
            self.if_result_var_names_stack.last_mut().unwrap().push_back(if_expr_res_ident_name.clone());
            self.emit_line(format!("AbraValue {};", if_expr_res_ident_name));
            self.if_result_var_names_stack.push(VecDeque::new());
        }

        self.lift(&node.condition)?;
        self.emit(format!("AbraValue if_cond_{} = ", salt));
        self.visit(*node.condition)?;
        self.emit_line(";");
        if is_expr {
            self.if_result_var_names_stack.pop();
        }

        self.emit_line(format!("if (!IS_FALSY(if_cond_{})) {{", salt));
        if let Some(condition_binding) = node.condition_binding {
            self.visit_binding_pattern(&condition_binding, format!("if_cond_{}", &salt), false);
        }

        if is_expr {
            self.if_result_var_names_stack.push(VecDeque::new());
        }
        let len = node.if_block.len();
        for (idx, node) in node.if_block.into_iter().enumerate() {
            self.lift(&node)?;
            if idx == len - 1 && is_expr && node.all_branches_terminate().is_none() {
                self.emit(format!("{} = ", if_expr_res_ident_name));
            }
            self.visit(node)?;
            self.emit_line(";");
        }
        if is_expr {
            self.if_result_var_names_stack.pop();
        }

        if let Some(else_block) = node.else_block {
            self.emit_line("} else {");
            if is_expr {
                self.if_result_var_names_stack.push(VecDeque::new());
            }
            let len = else_block.len();
            for (idx, node) in else_block.into_iter().enumerate() {
                self.lift(&node)?;
                if idx == len - 1 && is_expr && node.all_branches_terminate().is_none() {
                    self.emit(format!("{} = ", if_expr_res_ident_name));
                }
                self.visit(node)?;
                self.emit_line(";");
            }
        }
        self.emit_line("}");
        if is_expr {
            self.if_result_var_names_stack.pop();
        }

        Ok(())
    }

    fn visit_match_node(&mut self, node: TypedMatchNode, is_expr: bool) -> Result<(), ()> {
        let salt = random_string(10);
        let match_end_label = format!("match_{}_end", &salt);

        let match_expr_res_ident_name = format!("r_matchexp__{}", salt);
        if is_expr {
            self.match_result_var_names_stack.last_mut().unwrap().push_back(match_expr_res_ident_name.clone());
            self.emit_line(format!("AbraValue {};", match_expr_res_ident_name));
            self.match_result_var_names_stack.push(VecDeque::new());
        }

        let match_target_ident = format!("match_target_{}", &salt);
        self.lift(&node.target)?;
        self.emit(format!("AbraValue {} =", &match_target_ident));
        self.visit(*node.target)?;
        self.emit_line(";");
        if is_expr {
            self.match_result_var_names_stack.pop();
        }

        if is_expr {
            self.match_result_var_names_stack.push(VecDeque::new());
        }

        let num_branches = node.branches.len();
        for (idx, (match_kind, binding, body)) in node.branches.into_iter().enumerate() {
            match &match_kind {
                TypedMatchKind::Constant { node } => self.lift(node)?,
                TypedMatchKind::Tuple { nodes } => {
                    for node in nodes {
                        self.lift(node)?;
                    }
                }
                _ => {}
            }

            self.emit_line(format!("match_{}_case_{}:", &salt, idx));
            self.emit("if (");

            let mut enum_variant_match_case_ctx = None;
            match match_kind {
                TypedMatchKind::Wildcard => self.emit("true"),
                TypedMatchKind::None => self.emit(format!("IS_NONE({})", &match_target_ident)),
                TypedMatchKind::Type { type_name, .. } => {
                    let type_id_var = match type_name.as_ref() {
                        "Int" | "Float" | "Bool" | "String" => format!("std__{}__type_id", &type_name),
                        _ => {
                            let type_c_name = self.c_name_for_type(&type_name);
                            format!("{}__type_id", self.c_type_names.get(&type_c_name).unwrap())
                        }
                    };
                    self.emit(format!("std_type_eq({}, {})", &match_target_ident, type_id_var));
                }
                TypedMatchKind::EnumVariant { enum_name, variant_idx, variant_name, args } => {
                    let type_id_var = match enum_name.as_ref() {
                        "Result" => format!("std__{}__type_id", &enum_name),
                        _ => {
                            let type_c_name = self.c_name_for_type(&enum_name);
                            format!("{}__type_id", self.c_type_names.get(&type_c_name).unwrap())
                        }
                    };
                    self.emit(format!("std_enum_variant_eq({}, {}, {})", &match_target_ident, type_id_var, variant_idx));

                    if let Some(args) = args {
                        enum_variant_match_case_ctx = Some((enum_name, variant_name, args));
                    }
                }
                TypedMatchKind::Constant { node } => {
                    self.emit(format!("std__eq({}, ", &match_target_ident));
                    self.visit(node)?;
                    self.emit(")");
                }
                TypedMatchKind::Tuple { nodes } => {
                    self.emit(format!("std_type_is_tuple({}) && ", &match_target_ident));
                    let len = nodes.len();
                    for (idx, node) in nodes.into_iter().enumerate() {
                        self.emit("std__eq(");
                        self.emit(format!("std_tuple__index(AS_OBJ({}), {})", &match_target_ident, idx));
                        self.emit(",");
                        self.visit(node)?;
                        self.emit(")");

                        if idx < len - 1 {
                            self.emit(" && ");
                        }
                    }
                }
            }
            self.emit_line(") {");
            self.scopes.push(Scope { name: "__match_branch".to_string(), bindings: HashMap::new() });
            if let Some(binding) = binding {
                let c_name = self.add_c_var_name(&binding);
                self.emit_line(format!("AbraValue {} = {};", c_name, &match_target_ident));
            }
            if let Some((enum_name, variant_name, args)) = enum_variant_match_case_ctx {
                for (arg_idx, (arg_name, arg)) in args.into_iter().enumerate() {
                    let type_c_name = if &enum_name == "Result" {
                        Self::c_name_for_type_in_module("std", &enum_name)
                    } else {
                        self.c_name_for_type(&enum_name)
                    };
                    let c_name = self.c_type_names.get(&type_c_name).expect(&format!("{}, {:#?}", type_c_name, &self.scopes));
                    let arg_var_name = format!(
                        "(({}*)AS_OBJ({}))->{}.{}",
                        &c_name, &match_target_ident, &variant_name, &arg_name
                    );

                    match arg {
                        TypedMatchCaseArgument::Pattern(pat) => {
                            self.visit_binding_pattern(&pat, arg_var_name, false);
                        }
                        TypedMatchCaseArgument::Literal(ast_node) => {
                            self.lift(&ast_node)?;
                            let lit_ident = format!("match_{}_case_{}_lit_{}", &salt, &idx, &arg_idx);
                            self.emit(format!("AbraValue {} = ", &lit_ident));
                            self.visit(ast_node)?;
                            self.emit_line(";");

                            if idx < num_branches {
                                let next_match_case_label = format!("match_{}_case_{}", &salt, idx + 1);
                                self.emit_line(format!(
                                    "if (!std__eq({}, {})) {{ goto {}; }}",
                                    &arg_var_name, &lit_ident, &next_match_case_label
                                ));
                            } else {
                                unreachable!("The last match case should never include a literal")
                            }
                        }
                    }
                }

            }
            let len = body.len();
            for (idx, node) in body.into_iter().enumerate() {
                self.lift(&node)?;
                if idx == len - 1 && is_expr && node.all_branches_terminate().is_none() {
                    self.emit(format!("{} = ", match_expr_res_ident_name));
                }
                self.visit(node)?;
                self.emit_line(";");
            }

            self.scopes.pop();
            self.emit_line(format!("goto {};", &match_end_label));
            self.emit_line("}");
        }
        if is_expr {
            self.match_result_var_names_stack.pop();
        }
        self.emit_line(format!("{}:;", match_end_label));

        Ok(())
    }

    fn visit_and_convert(&mut self, node: TypedAstNode) -> Result<(), ()> {
        match node.get_type() {
            Type::Int => self.emit("AS_INT("),
            Type::Float => self.emit("AS_FLOAT("),
            Type::Bool => self.emit("AS_BOOL("),
            _ => self.emit("AS_OBJ("),
        }
        self.visit(node)?;
        self.emit(")");
        Ok(())
    }

    fn visit_binding_pattern(&mut self, pat: &BindingPattern, var_name: String, is_exported: bool) {
        match pat {
            BindingPattern::Variable(tok) => {
                let name = Token::get_ident_name(&tok);
                let c_name = self.add_c_var_name(name.clone());

                if is_exported {
                    let prev = self.switch_buf(BufferType::FwdDecls);
                    self.emit_line(format!("AbraValue {};", &c_name));
                    self.switch_buf(prev);

                    self.emit_line(format!("{} = {};", c_name, var_name));
                } else {
                    self.emit_line(format!("AbraValue {} = {};", c_name, var_name));
                }
            }
            BindingPattern::Tuple(_, pats) => {
                let salt = random_string(10);
                for (idx, pat) in pats.iter().enumerate() {
                    let tuple_var_name = format!("tuple_item_{}_{}", &salt, &idx);
                    self.emit_line(format!("AbraValue {} = std_tuple__index(AS_OBJ({}), {});", &tuple_var_name, &var_name, &idx));
                    self.visit_binding_pattern(pat, tuple_var_name, is_exported);
                }
            }
            BindingPattern::Array(_, pats, is_string) => {
                let salt = random_string(10);
                let num_pats = pats.len();
                let mut seen_splat = false;
                for (idx, (pat, is_splat)) in pats.iter().enumerate() {
                    let array_var_name = format!("array_item_{}_{}", &salt, &idx).replace("-", "neg");
                    if *is_splat {
                        if (idx as usize) == num_pats - 1 {
                            let f = if *is_string { "std_string__range_to_end" } else { "std_array__range_to_end" };
                            self.emit_line(format!("AbraValue {} = {}(AS_OBJ({}), {});", &array_var_name, f, &var_name, &idx));
                        } else {
                            let f = if *is_string { "std_string__range" } else { "std_array__range" };
                            let end_idx = num_pats - (idx as usize) - 1;
                            self.emit_line(format!("AbraValue {} = {}(AS_OBJ({}), {}, -{});", &array_var_name, f, &var_name, &idx, end_idx));
                            self.emit_line(format!("int64_t cur_idx_{} = {} + ((AbraArray*)AS_OBJ({}))->size;", salt, idx, array_var_name));
                            seen_splat = true;
                        };
                    } else {
                        let f = if *is_string { "std_string__index" } else { "std_array__index" };
                        if seen_splat {
                            self.emit_line(format!("AbraValue {} = {}(AS_OBJ({}), cur_idx_{}++);", &array_var_name, f, &var_name, salt));
                        } else {
                            self.emit_line(format!("AbraValue {} = {}(AS_OBJ({}), {});", &array_var_name, f, &var_name, &idx));
                        }
                    }
                    self.visit_binding_pattern(pat, array_var_name, is_exported);
                }
            }
        }
    }
}

impl<'a, R: ModuleReader> TypedAstVisitor<(), ()> for CCompiler<'a, R> {
    fn visit_literal(&mut self, _token: Token, node: TypedLiteralNode) -> Result<(), ()> {
        match node {
            TypedLiteralNode::IntLiteral(i) => self.emit(format!("NEW_INT({})", i)),
            TypedLiteralNode::FloatLiteral(f) => self.emit(format!("NEW_FLOAT({})", f)),
            TypedLiteralNode::BoolLiteral(b) => self.emit(if b { "ABRA_TRUE" } else { "ABRA_FALSE" }),
            TypedLiteralNode::StringLiteral(s) => {
                // TODO: Unicode escape sequences
                let len = s.len();
                let s = s.replace("\\", "\\\\");
                let s = s.replace("\n", "\\n");
                let s = s.replace("\r", "\\r");
                let s = s.replace("\t", "\\t");
                let s = s.replace("\"", "\\\"");

                self.emit(format!("alloc_string(\"{}\", {})", s, len));
            }
        }

        Ok(())
    }

    fn visit_unary(&mut self, _token: Token, node: TypedUnaryNode) -> Result<(), ()> {
        match &node.typ {
            Type::Int => self.emit("NEW_INT("),
            Type::Float => self.emit("NEW_FLOAT("),
            Type::Bool => self.emit("NEW_BOOL("),
            Type::Option(_) => todo!(),
            _ => unreachable!("No other types currently have unary operators")
        }

        let op = match node.op {
            UnaryOp::Minus => "-",
            UnaryOp::Negate => "!",
        };
        self.emit(op);
        self.visit_and_convert(*node.expr)?;

        self.emit(")");

        Ok(())
    }

    fn visit_binary(&mut self, _token: Token, node: TypedBinaryNode) -> Result<(), ()> {
        let is_coalesce = node.op == BinaryOp::Coalesce || node.op == BinaryOp::CoalesceEq;
        let requires_conversion = !is_coalesce && match &node.typ {
            Type::Int => {
                self.emit("NEW_INT(");
                true
            }
            Type::Float => {
                self.emit("NEW_FLOAT(");
                true
            }
            Type::Bool => {
                self.emit("NEW_BOOL(");
                true
            }
            Type::String => false,
            _ => todo!()
        };

        match node.op {
            BinaryOp::Add => {
                if node.typ == Type::String {
                    self.emit("std_string__concat(");
                    self.visit(*node.left)?;
                    self.emit(", ");
                    self.visit(*node.right)?;
                    self.emit(")");

                    return Ok(());
                }

                self.visit_and_convert(*node.left)?;
                self.emit("+");
                self.visit_and_convert(*node.right)?;
            }
            BinaryOp::Sub => {
                self.visit_and_convert(*node.left)?;
                self.emit("-");
                self.visit_and_convert(*node.right)?;
            }
            BinaryOp::Mul => {
                self.visit_and_convert(*node.left)?;
                self.emit("*");
                self.visit_and_convert(*node.right)?;
            }
            BinaryOp::Div => {
                let needs_cast = node.left.get_type() == Type::Int && node.right.get_type() == Type::Int;
                if needs_cast { self.emit("((double)"); }
                self.visit_and_convert(*node.left)?;
                if needs_cast { self.emit(")"); }

                self.emit("/");
                self.visit_and_convert(*node.right)?;
            }
            BinaryOp::Mod => {
                if node.left.get_type() == Type::Float || node.right.get_type() == Type::Float {
                    self.emit("fmod(");
                    self.visit_and_convert(*node.left)?;
                    self.emit(",");
                    self.visit_and_convert(*node.right)?;
                    self.emit(")");
                } else {
                    self.visit_and_convert(*node.left)?;
                    self.emit("%");
                    self.visit_and_convert(*node.right)?;
                }
            }
            BinaryOp::And | BinaryOp::Or => unreachable!("&& and || get transformed to if-exprs"),
            BinaryOp::Xor => {
                self.emit("!");
                self.visit_and_convert(*node.left)?;
                self.emit("!=");
                self.emit("!");
                self.visit_and_convert(*node.right)?;
            }
            BinaryOp::Coalesce => {
                self.emit("std_option__coalesce(");
                self.visit(*node.left)?;
                self.emit(",");
                self.visit(*node.right)?;
                self.emit(")");
            }
            BinaryOp::Lt => {
                self.visit_and_convert(*node.left)?;
                self.emit("<");
                self.visit_and_convert(*node.right)?;
            }
            BinaryOp::Lte => {
                self.visit_and_convert(*node.left)?;
                self.emit("<=");
                self.visit_and_convert(*node.right)?;
            }
            BinaryOp::Gt => {
                self.visit_and_convert(*node.left)?;
                self.emit(">");
                self.visit_and_convert(*node.right)?;
            }
            BinaryOp::Gte => {
                self.visit_and_convert(*node.left)?;
                self.emit(">=");
                self.visit_and_convert(*node.right)?;
            }
            op @ BinaryOp::Neq | op @ BinaryOp::Eq => {
                if op == BinaryOp::Neq { self.emit("!"); }
                self.emit("std__eq(");
                self.visit(*node.left)?;
                self.emit(", ");
                self.visit(*node.right)?;
                self.emit(")");
            }
            BinaryOp::Pow => {
                self.emit("pow(");
                self.visit_and_convert(*node.left)?;
                self.emit(",");
                self.visit_and_convert(*node.right)?;
                self.emit(")");
            }
            BinaryOp::AddEq | BinaryOp::SubEq | BinaryOp::MulEq | BinaryOp::DivEq | BinaryOp::ModEq |
            BinaryOp::AndEq | BinaryOp::OrEq | BinaryOp::CoalesceEq => unreachable!("Assignment operators get transformed into Assignment nodes"),
            _ => unimplemented!()
        }

        if requires_conversion { self.emit(")"); }

        Ok(())
    }

    fn visit_grouped(&mut self, _token: Token, node: TypedGroupedNode) -> Result<(), ()> {
        self.emit("(");
        self.visit(*node.expr)?;
        self.emit(")");

        Ok(())
    }

    fn visit_array(&mut self, _token: Token, _node: TypedArrayNode) -> Result<(), ()> {
        let ident_name = self.array_literal_var_names_stack.last_mut().unwrap().pop_front().expect("We shouldn't reach an array literal without having visited it previously");
        self.emit(ident_name);

        Ok(())
    }

    fn visit_tuple(&mut self, _token: Token, _node: TypedTupleNode) -> Result<(), ()> {
        let ident_name = self.tuple_literal_var_names_stack.last_mut().unwrap().pop_front().expect("We shouldn't reach a tuple literal without having visited it previously");
        self.emit(ident_name);

        Ok(())
    }

    fn visit_map(&mut self, _token: Token, _node: TypedMapNode) -> Result<(), ()> {
        let ident_name = self.map_literal_var_names_stack.last_mut().unwrap().pop_front().expect("We shouldn't reach a map literal without having visited it previously");
        self.emit(ident_name);

        Ok(())
    }

    fn visit_set(&mut self, _token: Token, _node: TypedSetNode) -> Result<(), ()> {
        let ident_name = self.set_literal_var_names_stack.last_mut().unwrap().pop_front().expect("We shouldn't reach a set literal without having visited it previously");
        self.emit(ident_name);

        Ok(())
    }

    fn visit_lambda(&mut self, _token: Token, node: TypedLambdaNode) -> Result<(), ()> {
        let fn_name = lambda_name(&node);
        let c_name = format!("{}__{}_val", self.scopes.first().unwrap().name, fn_name);
        self.emit(c_name);

        Ok(())
    }

    fn visit_binding_decl(&mut self, _token: Token, node: TypedBindingDeclNode) -> Result<(), ()> {
        let TypedBindingDeclNode { binding, expr, .. } = node;

        let var_name = format!("bind_destr_{}", random_string(10));
        self.emit(format!("AbraValue {} = ", var_name));
        if let Some(expr) = expr {
            self.visit(*expr)?;
        } else {
            self.emit("ABRA_NONE");
        }
        self.emit_line(";");

        let is_top_level = self.scopes.len() == 1;
        self.visit_binding_pattern(&binding, var_name, is_top_level);

        Ok(())
    }

    fn visit_function_decl(&mut self, _token: Token, node: TypedFunctionDeclNode) -> Result<(), ()> {
        let arity = node.args.len();
        let fn_name = Token::get_ident_name(&node.name);
        let c_name = self.find_c_var_name_trim_suffix(&fn_name, "_val").expect(&format!("Could not find c-variable name for {}", fn_name));
        self.emit_finalize_function_value(arity, &fn_name, &c_name);
        self.scopes.last_mut().unwrap().bindings.insert(fn_name, format!("{}_val", c_name));

        Ok(())
    }

    fn visit_type_decl(&mut self, _token: Token, _node: TypedTypeDeclNode) -> Result<(), ()> {
        // All code necessary for type declaration was emitted when lifting types
        Ok(())
    }

    fn visit_enum_decl(&mut self, _token: Token, _node: TypedEnumDeclNode) -> Result<(), ()> {
        // All code necessary for enum declaration was emitted when lifting types
        Ok(())
    }

    fn visit_identifier(&mut self, _token: Token, node: TypedIdentifierNode) -> Result<(), ()> {
        let c_var_name = self.find_c_var_name(&node.name)
            .expect(&format!("Could not find c-variable name for binding {}", &node.name));
        self.emit(c_var_name);
        Ok(())
    }

    fn visit_assignment(&mut self, _token: Token, node: TypedAssignmentNode) -> Result<(), ()> {
        match *node.target {
            TypedAstNode::Indexing(_, TypedIndexingNode { target, index, .. }) => {
                match target.get_type() {
                    Type::Array(_) => self.emit("std_array__index_assign("),
                    Type::Tuple(_) => self.emit("std_tuple__index_assign("),
                    Type::Map(_, _) => self.emit("std_map__index_assign("),
                    _ => unreachable!()
                }

                self.visit_and_convert(*target)?;
                self.emit(",");
                match index {
                    IndexingMode::Index(idx) => self.visit(*idx)?,
                    _ => unreachable!()
                };
                self.emit(",");
                self.visit(*node.expr)?;
                self.emit(")");
            }
            TypedAstNode::Accessor(_, TypedAccessorNode { target, field_ident, .. }) => {
                let field_name = Token::get_ident_name(&field_ident);

                let type_c_name = match target.get_type() {
                    Type::Int | Type::Float | Type::String | Type::Array(_) | Type::Map(_, _) | Type::Set(_) => unimplemented!("Builtins have no mutable static members"),
                    Type::Type(name, _, _) => {
                        let type_name = name.split("/").last().unwrap();
                        if name.starts_with("prelude/") {
                            Self::c_name_for_type_in_module("std", &type_name)
                        } else {
                            self.c_name_for_type(&type_name)
                        }
                    }
                    Type::Reference(t, _) => {
                        let type_name = t.split("/").last().unwrap();
                        self.c_name_for_type(&type_name)
                    }
                    Type::Struct(t) => self.c_name_for_type(&t.name),
                    _ => unimplemented!()
                };
                let c_name = self.c_type_names.get(&type_c_name).unwrap();
                let line = format!("(({}*)AS_OBJ(", &c_name);
                self.emit(line);
                self.visit(*target)?;
                self.emit(format!("))->{} = ", field_name));
                self.visit(*node.expr)?;
            },
            target @ TypedAstNode::Identifier(_, _) => {
                self.visit(target)?;
                self.emit("=");
                self.visit(*node.expr)?;
            }
            _ => unreachable!("All other node types are caught in typechecking")
        };

        Ok(())
    }

    fn visit_indexing(&mut self, _token: Token, node: TypedIndexingNode) -> Result<(), ()> {
        let target_type = node.target.get_type();
        match (&target_type, &node.index) {
            (Type::String, IndexingMode::Index(_)) => self.emit("std_string__index("),
            (Type::String, IndexingMode::Range(start, end)) => {
                match (&start, &end) {
                    (Some(_), Some(_)) => self.emit("std_string__range("),
                    (None, Some(_)) => self.emit("std_string__range_from_start("),
                    (Some(_), None) => self.emit("std_string__range_to_end("),
                    (None, None) => unreachable!("Forbidden"),
                }
            }
            (Type::Array(_), IndexingMode::Index(_)) => self.emit("std_array__index("),
            (Type::Array(_), IndexingMode::Range(start, end)) => {
                match (&start, &end) {
                    (Some(_), Some(_)) => self.emit("std_array__range("),
                    (None, Some(_)) => self.emit("std_array__range_from_start("),
                    (Some(_), None) => self.emit("std_array__range_to_end("),
                    (None, None) => unreachable!("Forbidden"),
                }
            }
            (Type::Tuple(_), IndexingMode::Index(_)) => self.emit("std_tuple__index("),
            (Type::Map(_, _), IndexingMode::Index(_)) => self.emit("std_map__index("),
            (Type::Option(_), _) => todo!("Indexing should be chainable for optional types"),
            _ => unreachable!("No other indexing modes")
        }

        self.visit_and_convert(*node.target)?;
        self.emit(", ");

        match node.index {
            IndexingMode::Index(i) => {
                if let Type::Map(_, _) = target_type {
                    self.visit(*i)?;
                } else {
                    self.visit_and_convert(*i)?
                }
            }
            IndexingMode::Range(start, end) => {
                if let Some(start) = start {
                    self.visit_and_convert(*start)?;
                    if end.is_some() {
                        self.emit(", ");
                    }
                }
                if let Some(end) = end {
                    self.visit_and_convert(*end)?;
                }
            }
        }

        self.emit(")");
        Ok(())
    }

    fn visit_if_statement(&mut self, _is_stmt: bool, _token: Token, node: TypedIfNode) -> Result<(), ()> {
        self.visit_if_node(node.clone(), false)?;

        Ok(())
    }

    fn visit_if_expression(&mut self, _token: Token, _node: TypedIfNode) -> Result<(), ()> {
        let ident_name = self.if_result_var_names_stack.last_mut().unwrap().pop_front().expect("We shouldn't reach an if-expr without having visited it previously");
        self.emit(ident_name);

        Ok(())
    }

    fn visit_invocation(&mut self, _token: Token, node: TypedInvocationNode) -> Result<(), ()> {
        let ident_name = self.invocation_var_names_queue.back_mut().unwrap().pop_front().expect("We shouldn't reach an invocation without having visited it previously");

        // No need to emit invocation result var, since nothing will access it (verified by typechecker).
        if node.typ != Type::Unit {
            self.emit(ident_name);
        }

        Ok(())
    }

    fn visit_instantiation(&mut self, _token: Token, node: TypedInstantiationNode) -> Result<(), ()> {
        let target_c_name = match &*node.target {
            TypedAstNode::Identifier(_, TypedIdentifierNode { name, .. }) => {
                let type_c_name = self.c_name_for_type(name);
                self.c_type_names.get(&type_c_name).unwrap().clone()
            }
            _ => todo!()
        };
        self.emit(format!("{}__new(", &target_c_name));
        let num_fields = node.fields.len();
        for (idx, (_, field_node)) in node.fields.into_iter().enumerate() {
            self.visit(field_node)?;
            if idx < num_fields - 1 {
                self.emit(",");
            }
        }
        self.emit(")");

        Ok(())
    }

    fn visit_accessor(&mut self, _token: Token, _node: TypedAccessorNode) -> Result<(), ()> {
        let ident_name = self.accessor_var_names_stack.last_mut().unwrap().pop_front().expect("We shouldn't reach an accessor without having visited it previously");
        self.emit(ident_name);

        Ok(())
    }

    fn visit_for_loop(&mut self, _token: Token, node: TypedForLoopNode) -> Result<(), ()> {
        let salt = random_string(10);

        let iter_ident = format!("iter_{}", &salt);
        self.lift(&node.iterator)?;
        self.emit(format!("AbraValue {} =", &iter_ident));
        let enumerate_method_name = match node.iterator.get_type() {
            Type::Array(_) => "std__Array__method_enumerate",
            Type::Set(_) => "std__Set__method_enumerate",
            Type::Map(_, _) => "std__Map__method_enumerate",
            _ => unreachable!("Un-enumerable type, should have been caught during typechecking")
        };
        self.emit(format!("{}(NULL, ", enumerate_method_name));
        self.visit(*node.iterator)?;
        self.emit_line(");");
        self.emit_line(format!("int64_t iter_idx_{} = ((AbraArray*)AS_OBJ(iter_{}))->size;", &salt, &salt));
        self.emit_line(format!("AbraValue* iter_items_{} = ((AbraArray*)AS_OBJ(iter_{}))->items;", &salt, &salt));

        self.emit_line(format!("for (int64_t i = 0; i < iter_idx_{}; ++i) {{", &salt));
        self.scopes.push(Scope { name: "__for_loop".to_string(), bindings: HashMap::new() });
        self.emit_line(format!("AbraValue iter_item_{} = iter_items_{}[i];", &salt, &salt));
        let var_name = format!("bind_destr_{}", random_string(10));
        self.emit_line(format!("AbraValue {} = std_tuple__index(AS_OBJ(iter_item_{}), 0);", var_name, &salt));
        self.visit_binding_pattern(&node.binding, var_name, false);
        if let Some(index_ident) = node.index_ident {
            let c_name = self.add_c_var_name(Token::get_ident_name(&index_ident));
            self.emit_line(format!("AbraValue {} = std_tuple__index(AS_OBJ(iter_item_{}), 1);", c_name, &salt));
        }
        for node in node.body {
            self.lift(&node)?;
            self.visit(node)?;
            self.emit_line(";");
        }
        self.scopes.pop();
        self.emit_line("}");

        Ok(())
    }

    fn visit_while_loop(&mut self, _token: Token, node: TypedWhileLoopNode) -> Result<(), ()> {
        let salt = random_string(10);

        let while_cond_ident = format!("while_cond_{}", &salt);

        self.emit_line("while (true) {");
        self.scopes.push(Scope { name: "__while_loop".to_string(), bindings: HashMap::new() });

        self.lift(&node.condition)?;
        self.emit(format!("AbraValue {} =", &while_cond_ident));
        self.visit(*node.condition)?;
        self.emit_line(";");
        self.emit_line(format!("if (IS_FALSY({})) {{ break; }}", &while_cond_ident));

        if let Some(condition_binding) = node.condition_binding {
            let c_name = self.add_c_var_name(Token::get_ident_name(&condition_binding));
            self.emit_line(format!("AbraValue {} = {};", c_name, &while_cond_ident));
        }

        for node in node.body {
            self.lift(&node)?;
            self.visit(node)?;
            self.emit_line(";");
        }

        self.scopes.pop();
        self.emit_line("}");

        Ok(())
    }

    fn visit_break(&mut self, _token: Token) -> Result<(), ()> {
        self.emit_line("break;");
        Ok(())
    }

    fn visit_continue(&mut self, _token: Token) -> Result<(), ()> {
        self.emit_line("continue;");
        Ok(())
    }

    fn visit_return(&mut self, _token: Token, node: TypedReturnNode) -> Result<(), ()> {
        self.emit("return ");
        if let Some(target) = node.target {
            self.visit(*target)?;
        } else {
            self.emit("ABRA_NONE");
        }
        self.emit_line(";");

        Ok(())
    }

    fn visit_match_statement(&mut self, _is_stmt: bool, _token: Token, node: TypedMatchNode) -> Result<(), ()> {
        self.visit_match_node(node, false)?;

        Ok(())
    }

    fn visit_match_expression(&mut self, _token: Token, _node: TypedMatchNode) -> Result<(), ()> {
        let ident_name = self.match_result_var_names_stack.last_mut().unwrap().pop_front().expect("We shouldn't reach a match without having visited it previously");
        self.emit(ident_name);

        Ok(())
    }

    fn visit_import_statement(&mut self, _token: Token, node: TypedImportNode) -> Result<(), ()> {
        let TypedImportNode { module_id, imports, .. } = node;
        let module = self.module_loader.get_module(&module_id);

        let module_name = match module_id {
            ModuleId::External(module_name) => module_name,
            ModuleId::Internal(_) => {
                let module_name = self.module_loader.get_module_name(&module_id);
                normalize_module_name(&module_name, &self.root_dir)
            }
        };

        for import_name in imports {
            let export = module.exports.get(&import_name).unwrap();
            match export {
                ExportedValue::Binding(t) => {
                    self.switch_buf(BufferType::Body);
                    let c_name = self.add_c_var_name_with_suffix(&import_name, "_val");
                    self.emit_line(format!("AbraValue {};", c_name));

                    self.switch_buf(BufferType::MainFn);

                    let import_c_name = if let Type::Fn(_) = t {
                        format!("{}__{}_val", module_name, &import_name)
                    } else {
                        format!("{}__{}", module_name, &import_name)
                    };
                    self.emit_line(format!("{} = {};", c_name, import_c_name));
                },
                ExportedValue::Type { .. } => {
                    let imported_type_c_name = Self::c_name_for_type_in_module(&module_name, &import_name);
                    let alias_type_name = self.c_name_for_type(&import_name);
                    self.insert_binding(&import_name, &alias_type_name);
                    self.c_type_names.insert(alias_type_name, imported_type_c_name);
                }
            };
        }

        Ok(())
    }

    fn visit_nil(&mut self, _token: Token) -> Result<(), ()> {
        self.emit("ABRA_NONE");
        Ok(())
    }
}
