use std::collections::{HashMap, HashSet, VecDeque};
use itertools::Itertools;
use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::common::util::random_string;
use crate::lexer::tokens::Token;
use crate::parser::ast::{BinaryOp, BindingPattern, IndexingMode, UnaryOp};
use crate::typechecker::typed_ast::{AssignmentTargetKind, TypedAccessorNode, TypedArrayNode, TypedAssignmentNode, TypedAstNode, TypedBinaryNode, TypedBindingDeclNode, TypedEnumDeclNode, TypedForLoopNode, TypedFunctionDeclNode, TypedGroupedNode, TypedIdentifierNode, TypedIfNode, TypedImportNode, TypedIndexingNode, TypedInstantiationNode, TypedInvocationNode, TypedLambdaNode, TypedLiteralNode, TypedMapNode, TypedMatchNode, TypedReturnNode, TypedSetNode, TypedTupleNode, TypedTypeDeclNode, TypedUnaryNode, TypedWhileLoopNode};
use crate::typechecker::types::Type;

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

pub struct CCompiler {
    main_fn_buf: String,
    fwd_decls_buf: String,
    body_buf: String,
    buf_type: BufferType,
    scopes: Vec<Scope>,
    if_result_var_names_stack: Vec<VecDeque<String>>,
    array_literal_var_names_stack: Vec<VecDeque<String>>,
    tuple_literal_var_names_stack: Vec<VecDeque<String>>,
    map_literal_var_names_stack: Vec<VecDeque<String>>,
    invocation_var_names_stack: Vec<VecDeque<String>>,
}

fn extract_functions(
    node: &TypedAstNode,
    known_vars: &Vec<String>,
    path: Vec<String>,
    seen_fns: &mut Vec<(Vec<String>, TypedFunctionDeclNode, HashSet<String>)>,
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
            TypedAstNode::Lambda(_, _) => todo!(),
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
                for node in &n.if_block {
                    walk_and_find_vars(&node, vars);
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
            TypedAstNode::Instantiation(_, _) |
            TypedAstNode::ForLoop(_, _) |
            TypedAstNode::WhileLoop(_, _) |
            TypedAstNode::Break(_) |
            TypedAstNode::Continue(_) |
            TypedAstNode::ReturnStatement(_, _) |
            TypedAstNode::Accessor(_, _) |
            TypedAstNode::MatchStatement(_, _) |
            TypedAstNode::MatchExpression(_, _) |
            TypedAstNode::ImportStatement(_, _) |
            TypedAstNode::_Nil(_) => {}
        }
    }

    if let TypedAstNode::FunctionDecl(_, decl_node) = node {
        let mut current_known_vars = known_vars.clone();
        let fn_name = Token::get_ident_name(&decl_node.name);
        current_known_vars.push(fn_name.clone());

        let mut seen_vars = Vec::new();
        for (name, _, _, default_value) in &decl_node.args {
            current_known_vars.push(Token::get_ident_name(name));
            if let Some(default_value_node) = default_value {
                walk_and_find_vars(&default_value_node, &mut seen_vars);
            }
        }

        for node in &decl_node.body {
            if let TypedAstNode::FunctionDecl(_, _) = &node {
                let mut sub_path = path.clone();
                sub_path.push(fn_name.clone());
                extract_functions(&node, &known_vars, sub_path, seen_fns);

                let (_, _, seen_vars_in_fn) = seen_fns.last().unwrap();
                let mut seen_vars_in_fn = seen_vars_in_fn.clone().into_iter().collect();
                seen_vars.append(&mut seen_vars_in_fn);
            } else {
                walk_and_find_vars(&node, &mut seen_vars);
            }
        }

        let closed_over_variables = seen_vars.iter()
            .filter(|var| !current_known_vars.contains(var))
            .map(|var| var.clone())
            .collect::<HashSet<_>>();
        seen_fns.push((path, decl_node.clone(), closed_over_variables))
    }
}

impl CCompiler {
    fn new() -> Self {
        let root_scope = Scope {
            name: "example".to_string(),
            bindings: {
                let mut m = HashMap::new();
                m.insert("print".to_string(), "std__print".to_string());
                m.insert("println".to_string(), "std__println".to_string());
                m.insert("None".to_string(), "ABRA_NONE".to_string());
                m
            },
        };

        CCompiler {
            main_fn_buf: "".to_string(),
            fwd_decls_buf: "".to_string(),
            body_buf: "".to_string(),
            buf_type: BufferType::MainFn,
            scopes: vec![root_scope],
            if_result_var_names_stack: vec![VecDeque::new()],
            array_literal_var_names_stack: vec![VecDeque::new()],
            tuple_literal_var_names_stack: vec![VecDeque::new()],
            map_literal_var_names_stack: vec![VecDeque::new()],
            invocation_var_names_stack: vec![VecDeque::new()],
        }
    }

    pub fn gen_c(ast: Vec<TypedAstNode>) -> Result<String, ()> {
        let mut compiler = CCompiler::new();

        compiler.switch_buf(BufferType::FwdDecls);
        compiler.emit_line("#include \"abra_std.h\"\n");

        compiler.switch_buf(BufferType::MainFn);
        compiler.emit_line("int main(int argc, char** argv) {");
        compiler.emit_line("abra_init();");

        compiler.switch_buf(BufferType::Body);
        compiler.lift_fns(&ast)?;
        compiler.switch_buf(BufferType::MainFn);

        let ast_len = ast.len();
        for (idx, node) in ast.into_iter().enumerate() {
            compiler.lift(&node)?;

            let should_print = idx == ast_len - 1 && node.get_type() != Type::Unit;
            if should_print {
                let ident = random_string(10);
                compiler.emit(format!("const char* last_expr_{} = std__to_string(", ident));
                compiler.visit(node)?;
                compiler.emit_line(");");
                compiler.emit_line(format!("printf(\"%s\\n\", last_expr_{});", ident));
            } else {
                compiler.visit(node)?;
                compiler.emit_line(";");
            }
        }

        compiler.emit_line("  return 0;\n}");

        let output = format!(
            "{}\n{}\n{}",
            compiler.fwd_decls_buf,
            compiler.body_buf,
            compiler.main_fn_buf,
        );
        Ok(output)
    }

    fn find_c_var_name<S: AsRef<str>>(&self, name: S) -> Option<String> {
        self.scopes.iter().rev().find_map(|s| s.bindings.get(name.as_ref()).map(|n| n.clone()))
    }

    fn add_c_var_name<S: AsRef<str>>(&mut self, name: S) -> String {
        self.add_c_var_name_with_suffix(name, "")
    }

    fn add_c_var_name_with_suffix<S1: AsRef<str>, S2: AsRef<str>>(&mut self, name: S1, suffix: S2) -> String {
        let prefix = self.scopes.iter().map(|Scope { name, .. }| name).join("_");

        let c_name = format!("{}__{}{}", prefix, name.as_ref(), suffix.as_ref());
        self.scopes.last_mut().expect("There's always at least 1 scope")
            .bindings
            .insert(name.as_ref().to_string(), c_name.clone());
        c_name
    }

    fn switch_buf(&mut self, buf_type: BufferType) {
        self.buf_type = buf_type;
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

    fn lift_fns(&mut self, ast: &Vec<TypedAstNode>) -> Result<(), ()> {
        let mut seen_fns = Vec::new();
        for node in ast {
            let mut known_vars = self.scopes.first().unwrap().bindings.keys().map(|v| v.clone()).collect();
            extract_functions(&node, &mut known_vars, vec![], &mut seen_fns);
        }

        let mut fns_at_path = HashMap::new();
        let mut upvalues_at_path = HashMap::new();
        for (path, f, closed_over_vars) in &seen_fns {
            fns_at_path.entry(path.clone()).or_insert(vec![]).push(Token::get_ident_name(&f.name));

            let mut path = path.clone();
            path.push(Token::get_ident_name(&f.name));
            upvalues_at_path.insert(path, closed_over_vars.clone());
        }

        // Add cvar name for all root-level fns
        if let Some(fns_at_path) = fns_at_path.get(&vec![]) {
            for name in fns_at_path {
                self.add_c_var_name_with_suffix(&name, "_val");
            }
        }

        // Emit code for each fn, making sure to set the proper intermediate scopes, as well as
        // pre-define a cvar for each other fn in that scope (to handle out-of-order references).
        for (path, f, vars) in seen_fns {
            for scope_name in &path {
                self.scopes.push(Scope { name: scope_name.clone(), bindings: HashMap::new() });
            }

            if let Some(fns_at_path) = fns_at_path.get(&path) {
                for name in fns_at_path {
                    self.add_c_var_name_with_suffix(&name, "_val");
                }
            }

            self.lift_fn(&f, &vars, &upvalues_at_path)?;

            for _ in 0..path.len() {
                self.scopes.pop();
            }
        }

        Ok(())
    }

    fn lift_fn(&mut self, node: &TypedFunctionDeclNode, closed_over_vars: &HashSet<String>, all_upvalues: &HashMap<Vec<String>, HashSet<String>>) -> Result<(), ()> {
        let node = node.clone(); // :/
        let fn_name = Token::get_ident_name(&node.name);
        let mut c_name = self.find_c_var_name(&fn_name).expect(&format!("Could not find c-variable name for {}", fn_name));
        debug_assert!(c_name.ends_with("_val"));
        if c_name.ends_with("_val") {
            c_name.truncate(c_name.len() - 4);
        }
        let c_name = c_name; // Remove temporary mutability

        self.scopes.push(Scope { name: fn_name.clone(), bindings: HashMap::new() });

        self.switch_buf(BufferType::FwdDecls);
        let env_struct_name = if !closed_over_vars.is_empty() {
            let env_struct_name = format!("{}_env_t", &c_name);
            self.emit_line(format!("typedef struct {} {{", &env_struct_name));
            for var in closed_over_vars {
                self.emit_line(format!("  AbraValue {};", var));
            }
            self.emit_line(format!("}} {};", &env_struct_name));
            Some(env_struct_name)
        } else { None };

        let args = node.args.iter()
            .map(|(name, _, _, _)| self.add_c_var_name(Token::get_ident_name(name)))
            .map(|name| format!("AbraValue {}", name))
            .join(", ");
        let sig = format!("AbraValue {}(void* _env{}{})", &c_name, if args.is_empty() { "" } else {","}, args);
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

        let arity = node.args.len();
        for (name, _, _, default_value) in node.args {
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

        let len = node.body.len();
        for (idx, node) in node.body.into_iter().enumerate() {
            self.lift(&node)?;

            if idx == len - 1 {
                self.emit("return ");
            }

            // If we see a nested fn, we add a cvar to handle references within this fn's body.
            if let TypedAstNode::FunctionDecl(_, n) = &node {
                let nested_fn_name = Token::get_ident_name(&n.name);
                let c_name = self.add_c_var_name(&nested_fn_name);
                self.scopes.last_mut().unwrap().bindings.insert(nested_fn_name.clone(), format!("{}_val", c_name));
                let arity = n.args.len();
                self.visit(node)?;

                let ctx_type_name = format!("callable_ctx__{}_t", arity);
                self.emit_line(format!("{}* {}_env_ctx = GC_MALLOC(sizeof({}));", &ctx_type_name, &c_name, &ctx_type_name));
                self.emit_line(format!("{}_env_ctx->fn = &{};", &c_name, &c_name));

                let mut path = self.scopes.iter().skip(1).map(|s| s.name.clone()).collect::<Vec<_>>();
                path.push(nested_fn_name.clone());
                match all_upvalues.get(&path) {
                    Some(upvalues) if !upvalues.is_empty() => {
                        self.emit_line(format!("{}_env_t* {}_env = GC_MALLOC(sizeof({}_env_t));", &c_name, &c_name, &c_name));
                        for upvalue_name in upvalues {
                            self.emit_line(format!("{}_env->{} = {};", &c_name, upvalue_name, self.find_c_var_name(&upvalue_name).unwrap()));
                        }
                        self.emit_line(format!("{}_env_ctx->env = {}_env;", &c_name, &c_name));
                    }
                    _ => {
                        self.emit_line(format!("{}_env_ctx->env = NULL;", &c_name));
                    }
                }
                self.emit_line(format!("{}_val = alloc_function(\"{}\", \"{}\", (void*) {}_env_ctx);", &c_name, &nested_fn_name, &c_name, &c_name));
            } else {
                self.visit(node)?;
                self.emit_line(";");
            };
        }

        self.emit_line("}");

        self.scopes.pop();
        self.switch_buf(BufferType::MainFn);

        if self.scopes.len() == 1 {
            let ctx_type_name = format!("callable_ctx__{}_t", arity);
            self.emit_line(format!("{}* {}_env_ctx = GC_MALLOC(sizeof({}));", &ctx_type_name, &c_name, &ctx_type_name));
            self.emit_line(format!("{}_env_ctx->fn = &{};", &c_name, &c_name));
            // todo: store ->env
            self.emit_line(format!("{}_val = alloc_function(\"{}\", \"{}\", (void*) {}_env_ctx);", &c_name, &c_name, &fn_name, &c_name));
            self.scopes.last_mut().unwrap().bindings.insert(fn_name, format!("{}_val", c_name));
        }

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

                let ident = random_string(10);
                let items_ident = format!("arr_items_{}", &ident);
                self.emit_line(format!("AbraValue* {} = GC_MALLOC(sizeof(AbraValue) * {});", items_ident, size));
                for (idx, item) in node.items.into_iter().enumerate() {
                    self.array_literal_var_names_stack.push(VecDeque::new());
                    self.lift(&item)?;
                    self.emit(format!("{}[{}] = ", items_ident, idx));
                    self.visit(item)?;
                    self.array_literal_var_names_stack.pop();
                    self.emit_line(";");
                }

                let arr_ident = format!("arr_{}", ident);
                self.array_literal_var_names_stack.last_mut().unwrap().push_back(arr_ident.clone());
                self.emit_line(format!("AbraValue {} = alloc_array({}, {});", &arr_ident, items_ident, size));
            }
            TypedAstNode::Tuple(_, node) => {
                let node = node.clone(); // :/
                let size = node.items.len();

                let ident = random_string(10);
                let items_ident = format!("tuple_items_{}", &ident);
                self.emit_line(format!("AbraValue* {} = GC_MALLOC(sizeof(AbraValue) * {});", items_ident, size));
                for (idx, item) in node.items.into_iter().enumerate() {
                    self.tuple_literal_var_names_stack.push(VecDeque::new());
                    self.lift(&item)?;
                    self.emit(format!("{}[{}] = ", items_ident, idx));
                    self.visit(item)?;
                    self.tuple_literal_var_names_stack.pop();
                    self.emit_line(";");
                }

                let tuple_ident = format!("tuple_{}", ident);
                self.tuple_literal_var_names_stack.last_mut().unwrap().push_back(tuple_ident.clone());
                self.emit_line(format!("AbraValue {} = alloc_tuple({}, {});", &tuple_ident, items_ident, size));
            }
            TypedAstNode::Map(_, node) => {
                let node = node.clone(); // :/
                let ident = random_string(10);
                let map_ident = format!("map_{}", ident);
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
            TypedAstNode::Set(_, _) => todo!("These will need to be lifted"),
            TypedAstNode::BindingDecl(_, node) => {
                if let Some(expr) = &node.expr {
                    self.lift(expr)?;
                }
            }
            TypedAstNode::Assignment(_, node) => {
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
                let node = node.clone(); // :/
                let ident_name = format!("r_ifexp__{}", random_string(10));
                self.if_result_var_names_stack.last_mut().unwrap().push_back(ident_name.clone());

                self.emit_line(format!("AbraValue {};", ident_name));
                self.if_result_var_names_stack.push(VecDeque::new());
                self.lift(&node.condition)?;
                self.emit("if (");
                self.visit_and_convert(*node.condition)?;
                self.if_result_var_names_stack.pop();
                self.emit_line(") {");

                self.if_result_var_names_stack.push(VecDeque::new());
                let len = node.if_block.len();
                for (idx, node) in node.if_block.into_iter().enumerate() {
                    self.lift(&node)?;
                    if idx == len - 1 {
                        self.emit(format!("{} = ", ident_name));
                    }
                    self.visit(node)?;
                    self.emit_line(";");
                }
                self.if_result_var_names_stack.pop();

                self.emit_line("} else {");
                self.if_result_var_names_stack.push(VecDeque::new());
                let else_block = node.else_block.unwrap();
                let len = else_block.len();
                for (idx, node) in else_block.into_iter().enumerate() {
                    self.lift(&node)?;
                    if idx == len - 1 {
                        self.emit(format!("{} = ", ident_name));
                    }
                    self.visit(node)?;
                    self.emit_line(";");
                }
                self.emit_line("}");
                self.if_result_var_names_stack.pop();
            }
            TypedAstNode::Invocation(_, node) => {
                let node = node.clone(); // :/
                let is_builtin = match &*node.target {
                    TypedAstNode::Identifier(_, TypedIdentifierNode { name, .. }) => name == "println" || name == "print",
                    _ => false
                };
                let arity = node.args.len();

                let ident_name = if is_builtin {
                    // Push an empty string; the invocation code will be emitted here during lifting,
                    // but there's nothing we need to emit when the node is visited later on.
                    self.invocation_var_names_stack.last_mut().unwrap().push_back("".to_string());
                    None
                } else {
                    let ident_name = format!("r_inv__{}", random_string(10));
                    self.lift(&node.target)?;
                    self.invocation_var_names_stack.last_mut().unwrap().push_back(ident_name.clone());
                    Some(ident_name)
                };

                for arg in &node.args {
                    if let Some(arg) = arg {
                        self.invocation_var_names_stack.push(VecDeque::new());
                        self.lift(&arg)?;
                        self.invocation_var_names_stack.pop();
                    }
                }

                if let Some(ident_name) = &ident_name {
                    let ctx_type_name = format!("callable_ctx__{}_t", arity);
                    self.emit(format!("{}* {}_ctx = ({}*) ((AbraFunction*)AS_OBJ(", &ctx_type_name, &ident_name, &ctx_type_name));
                    self.visit(*node.target)?;
                    self.emit_line("))->ctx;");

                    self.emit(format!("AbraValue {} = {}_ctx->fn({}_ctx->env", ident_name, ident_name, ident_name));
                    if arity > 0 { self.emit(","); }
                } else {
                    self.visit(*node.target)?;
                    self.emit("(");
                }

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

                self.emit_line(");");
            }
            TypedAstNode::Instantiation(_, node) => {
                for (_, field) in &node.fields {
                    self.lift(&field)?;
                }
            }
            TypedAstNode::ReturnStatement(_, node) => {
                if let Some(target) = &node.target {
                    self.lift(&target)?;
                }
            }
            TypedAstNode::MatchExpression(_, _) => todo!("This will also need to be lifted"),
            // The following node types cannot contain expressions that need lifting
            TypedAstNode::Literal(_, _) |
            TypedAstNode::Lambda(_, _) |
            TypedAstNode::FunctionDecl(_, _) |
            TypedAstNode::TypeDecl(_, _) |
            TypedAstNode::EnumDecl(_, _) |
            TypedAstNode::Identifier(_, _) |
            TypedAstNode::ForLoop(_, _) |
            TypedAstNode::WhileLoop(_, _) |
            TypedAstNode::Break(_) |
            TypedAstNode::Continue(_) |
            TypedAstNode::Accessor(_, _) |
            TypedAstNode::IfStatement(_, _) |
            TypedAstNode::MatchStatement(_, _) |
            TypedAstNode::ImportStatement(_, _) |
            TypedAstNode::_Nil(_) => {}
        };

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
}

impl TypedAstVisitor<(), ()> for CCompiler {
    fn visit_literal(&mut self, _token: Token, node: TypedLiteralNode) -> Result<(), ()> {
        match node {
            TypedLiteralNode::IntLiteral(i) => self.emit(format!("NEW_INT({})", i)),
            TypedLiteralNode::FloatLiteral(f) => self.emit(format!("NEW_FLOAT({})", f)),
            TypedLiteralNode::BoolLiteral(b) => self.emit(format!("NEW_BOOL({})", b)),
            TypedLiteralNode::StringLiteral(s) => {
                let len = s.len();
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
        match &node.typ {
            Type::Int => self.emit("NEW_INT("),
            Type::Float => self.emit("NEW_FLOAT("),
            Type::Bool => self.emit("NEW_BOOL("),
            Type::String => {} // Do nothing, it's handled below
            _ => todo!()
        }

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
            BinaryOp::Coalesce => {}
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
            BinaryOp::AndEq | BinaryOp::OrEq | BinaryOp::CoalesceEq => unreachable!("Assignment operators get transformed into Assignment nodes")
        }

        self.emit(")");

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
        todo!()
    }

    fn visit_lambda(&mut self, _token: Token, _node: TypedLambdaNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_binding_decl(&mut self, _token: Token, node: TypedBindingDeclNode) -> Result<(), ()> {
        let TypedBindingDeclNode { binding, expr, .. } = node;

        match binding {
            BindingPattern::Variable(tok) => {
                let c_name = self.add_c_var_name(Token::get_ident_name(&tok));

                self.emit(format!("AbraValue {} = ", c_name));

                if let Some(expr) = expr {
                    self.visit(*expr)?;
                } else {
                    self.emit("ABRA_NONE");
                }
            }
            BindingPattern::Tuple(_, _) => todo!(),
            BindingPattern::Array(_, _, _) => todo!()
        }

        Ok(())
    }

    fn visit_function_decl(&mut self, _token: Token, _node: TypedFunctionDeclNode) -> Result<(), ()> {
        Ok(())
    }

    fn visit_type_decl(&mut self, _token: Token, _node: TypedTypeDeclNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_enum_decl(&mut self, _token: Token, _node: TypedEnumDeclNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_identifier(&mut self, _token: Token, node: TypedIdentifierNode) -> Result<(), ()> {
        let c_var_name = self.find_c_var_name(&node.name)
            .expect(&format!("Could not find c-variable name for binding {}", &node.name));
        self.emit(c_var_name);
        Ok(())
    }

    fn visit_assignment(&mut self, _token: Token, node: TypedAssignmentNode) -> Result<(), ()> {
        match node.kind {
            AssignmentTargetKind::Identifier => {
                self.visit(*node.target)?;
                self.emit("=");
                self.visit(*node.expr)?;
            }
            AssignmentTargetKind::ArrayIndex |
            AssignmentTargetKind::MapIndex |
            AssignmentTargetKind::Field => todo!()
        }

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

    fn visit_if_statement(&mut self, _is_stmt: bool, _token: Token, _node: TypedIfNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_if_expression(&mut self, _token: Token, _node: TypedIfNode) -> Result<(), ()> {
        let ident_name = self.if_result_var_names_stack.last_mut().unwrap().pop_front().expect("We shouldn't reach an if-expr without having visited it previously");
        self.emit(ident_name);

        Ok(())
    }

    fn visit_invocation(&mut self, _token: Token, _node: TypedInvocationNode) -> Result<(), ()> {
        let ident_name = self.invocation_var_names_stack.last_mut().unwrap().pop_front().expect("We shouldn't reach an invocation without having visited it previously");
        self.emit(ident_name);

        Ok(())
    }

    fn visit_instantiation(&mut self, _token: Token, _node: TypedInstantiationNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_accessor(&mut self, _token: Token, _node: TypedAccessorNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_for_loop(&mut self, _token: Token, _node: TypedForLoopNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_while_loop(&mut self, _token: Token, _node: TypedWhileLoopNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_break(&mut self, _token: Token) -> Result<(), ()> {
        todo!()
    }

    fn visit_continue(&mut self, _token: Token) -> Result<(), ()> {
        todo!()
    }

    fn visit_return(&mut self, _token: Token, _node: TypedReturnNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_match_statement(&mut self, _is_stmt: bool, _token: Token, _node: TypedMatchNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_match_expression(&mut self, _token: Token, _node: TypedMatchNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_import_statement(&mut self, _token: Token, _node: TypedImportNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_nil(&mut self, _token: Token) -> Result<(), ()> {
        self.emit("ABRA_NONE");
        Ok(())
    }
}
