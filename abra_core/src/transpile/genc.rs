use std::collections::{HashMap, VecDeque};
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
}

impl CCompiler {
    fn new() -> Self {
        let root_scope = Scope {
            name: "example".to_string(),
            bindings: {
                let mut m = HashMap::new();
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
        }
    }

    pub fn gen_c(ast: Vec<TypedAstNode>) -> Result<String, ()> {
        let mut compiler = CCompiler::new();

        compiler.switch_buf(BufferType::FwdDecls);
        compiler.emit_line("#include \"abra_std.h\"\n");

        compiler.switch_buf(BufferType::Body);
        compiler.lift_fns(&ast)?;

        compiler.switch_buf(BufferType::MainFn);
        compiler.emit_line("int main(int argc, char** argv) {");
        compiler.emit_line("abra_init();");

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
        let prefix = self.scopes.iter().map(|Scope { name, .. }| name).join("_");

        let c_name = format!("{}__{}", prefix, name.as_ref());
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

    fn lift_fns(&mut self, nodes: &Vec<TypedAstNode>) -> Result<(), ()> {
        let fn_decl_nodes = nodes.iter().filter_map(|ast| if let TypedAstNode::FunctionDecl(_, node) = ast { Some(node) } else { None }).collect::<Vec<_>>();

        // Forward-declare all functions, emitting signatures
        let mut sigs = Vec::new();
        for node in &fn_decl_nodes {
            let fn_name = Token::get_ident_name(&node.name);
            let c_name = self.add_c_var_name(&fn_name);

            self.scopes.push(Scope { name: fn_name, bindings: HashMap::new() });
            // TODO: Visit fn body's nodes, looking for variables closed over from outer scope. These need to be built into a *_env_t struct and passed as a parameter to fn
            let args = node.args.iter()
                .map(|(name, _, _, _)| self.add_c_var_name(Token::get_ident_name(name)))
                .map(|name| format!("AbraValue {}", name))
                .join(", ");
            let sig = format!("AbraValue {}({})", c_name, args);
            self.switch_buf(BufferType::FwdDecls);
            self.emit_line(format!("{};", &sig));
            sigs.push(sig);
            self.scopes.pop();
        }

        for (node, sig) in fn_decl_nodes.iter().zip(sigs) {
            self.lift_fn(&node, sig)?;
        }
        Ok(())
    }

    fn lift_fn(&mut self, node: &TypedFunctionDeclNode, sig: String) -> Result<(), ()> {
        let node = node.clone(); // :/
        let fn_name = Token::get_ident_name(&node.name);

        self.scopes.push(Scope { name: fn_name, bindings: HashMap::new() });
        self.lift_fns(&node.body)?;

        self.switch_buf(BufferType::Body);
        self.emit_line(format!("{} {{", sig));
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

            self.visit(node)?;
            self.emit_line(";");
        }

        self.emit_line("}");

        self.scopes.pop();
        self.switch_buf(BufferType::MainFn);
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
                self.lift(&node.target)?;
                for arg in &node.args {
                    if let Some(arg) = arg {
                        self.lift(&arg)?;
                    }
                }
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

    fn visit_invocation(&mut self, _token: Token, node: TypedInvocationNode) -> Result<(), ()> {
        self.visit(*node.target)?;
        self.emit("(");

        let num_args = node.args.len();
        for (idx, arg) in node.args.into_iter().enumerate() {
            if let Some(arg) = arg {
                self.visit(arg)?;
            } else {
                self.emit("ABRA_NONE");
            }

            if idx < num_args - 1 {
                self.emit(", ");
            }
        }

        self.emit(")");
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
