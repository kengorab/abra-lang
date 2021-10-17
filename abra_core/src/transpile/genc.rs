use std::collections::VecDeque;
use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::common::util::random_string;
use crate::lexer::tokens::Token;
use crate::parser::ast::{BinaryOp, BindingPattern, UnaryOp};
use crate::typechecker::typed_ast::{AssignmentTargetKind, TypedAccessorNode, TypedArrayNode, TypedAssignmentNode, TypedAstNode, TypedBinaryNode, TypedBindingDeclNode, TypedEnumDeclNode, TypedForLoopNode, TypedFunctionDeclNode, TypedGroupedNode, TypedIdentifierNode, TypedIfNode, TypedImportNode, TypedIndexingNode, TypedInstantiationNode, TypedInvocationNode, TypedLambdaNode, TypedLiteralNode, TypedMapNode, TypedMatchNode, TypedReturnNode, TypedSetNode, TypedTupleNode, TypedTypeDeclNode, TypedUnaryNode, TypedWhileLoopNode};
use crate::typechecker::types::Type;

pub struct CCompiler {
    buf: String,
    module_name: String,
    if_result_var_names_stack: Vec<VecDeque<String>>,
    array_literal_var_names_stack: VecDeque<String>,
}

impl CCompiler {
    fn new() -> Self {
        CCompiler {
            buf: "".to_string(),
            module_name: "example".to_string(),
            if_result_var_names_stack: vec![VecDeque::new()],
            array_literal_var_names_stack: VecDeque::new(),
        }
    }

    pub fn gen_c(ast: Vec<TypedAstNode>) -> Result<String, ()> {
        let mut compiler = CCompiler::new();

        compiler.emit_line("#include \"abra_std.h\"\n");

        compiler.emit_line("int main(int argc, char** argv) {");
        compiler.emit_line("GC_INIT();");

        let ast_len = ast.len();
        for (idx, node) in ast.into_iter().enumerate() {
            compiler.lift(&node)?;

            let should_print = idx == ast_len - 1 && node.get_type() != Type::Unit;
            if should_print { compiler.emit("  std__println("); }

            compiler.visit(node)?;

            if should_print { compiler.emit(")"); }
            compiler.emit_line(";");
        }

        compiler.emit_line("  return 0;\n}");

        Ok(compiler.buf)
    }

    fn emit<S: AsRef<str>>(&mut self, code: S) {
        self.buf.push_str(code.as_ref())
    }

    fn emit_line<S: AsRef<str>>(&mut self, code: S) {
        self.buf.push_str(code.as_ref());
        self.buf.push('\n');
    }

    fn c_ident_name<S: AsRef<str>>(&self, name: S) -> String {
        format!("{}__{}", self.module_name, name.as_ref())
    }

    fn lift(&mut self, node: &TypedAstNode) -> Result<(), ()> {
        match node {
            TypedAstNode::Unary(_, node) => {
                self.lift(&node.expr)?;
            },
            TypedAstNode::Binary(_, node) => {
                self.lift(&node.left)?;
                self.lift(&node.right)?;
            }
            TypedAstNode::Grouped(_, node) => {
                self.lift(&node.expr)?;
            },
            TypedAstNode::Array(_, node) => {
                let node = node.clone(); // :/
                let size = node.items.len();

                let ident = random_string(10);
                let items_ident = format!("items_{}", &ident);
                self.emit_line(format!("AbraValue* {} = GC_MALLOC(sizeof(AbraValue) * {});", items_ident, size));
                for (idx, item) in node.items.into_iter().enumerate() {
                    self.lift(&item)?;
                    self.emit(format!("{}[{}] = ", items_ident, idx));
                    self.visit(item)?;
                    self.emit_line(";");
                }

                let arr_ident = format!("arr_{}", ident);
                self.emit_line(format!("AbraValue {} = alloc_array({}, {});", &arr_ident, items_ident, size));
                self.array_literal_var_names_stack.push_back(arr_ident);
            }
            TypedAstNode::Map(_, _) |
            TypedAstNode::Set(_, _) |
            TypedAstNode::Tuple(_, _) => todo!("These will need to be lifted"),
            TypedAstNode::BindingDecl(_, node) => {
                if let Some(expr) = &node.expr {
                    self.lift(expr)?;
                }
            }
            TypedAstNode::Assignment(_, node) => {
                self.lift(&node.expr)?;
            },
            TypedAstNode::Indexing(_, _) => todo!(),
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
            _ => todo!()
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

                    return Ok(())
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
            BinaryOp::Neq => {}
            BinaryOp::Eq => {}
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
        let ident_name = self.array_literal_var_names_stack.pop_front().expect("We shouldn't reach an array literal without having visited it previously");
        self.emit(ident_name);

        Ok(())
    }

    fn visit_tuple(&mut self, _token: Token, _node: TypedTupleNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_map(&mut self, _token: Token, _node: TypedMapNode) -> Result<(), ()> {
        todo!()
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
                if let Some(expr) = expr {
                    let name = Token::get_ident_name(&tok);
                    let c_name = self.c_ident_name(name);

                    self.emit(format!("AbraValue {} = ", c_name));
                    self.visit(*expr)?;
                } else {
                    todo!()
                }
            }
            BindingPattern::Tuple(_, _) |
            BindingPattern::Array(_, _, _) => todo!()
        }

        Ok(())
    }

    fn visit_function_decl(&mut self, _token: Token, _node: TypedFunctionDeclNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_type_decl(&mut self, _token: Token, _node: TypedTypeDeclNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_enum_decl(&mut self, _token: Token, _node: TypedEnumDeclNode) -> Result<(), ()> {
        todo!()
    }

    fn visit_identifier(&mut self, _token: Token, node: TypedIdentifierNode) -> Result<(), ()> {
        if &node.name == "println" {
            self.emit("std__println");
        } else {
            self.emit(self.c_ident_name(node.name));
        }

        Ok(())
    }

    fn visit_assignment(&mut self, _token: Token, node: TypedAssignmentNode) -> Result<(), ()> {
        match node.kind {
            AssignmentTargetKind::Identifier => {
                self.visit(*node.target)?;
                self.emit("=");
                self.visit(*node.expr)?;
            }
            _ => todo!()
        }

        Ok(())
    }

    fn visit_indexing(&mut self, _token: Token, _node: TypedIndexingNode) -> Result<(), ()> {
        todo!()
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
