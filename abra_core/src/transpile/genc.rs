use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::lexer::tokens::Token;
use crate::parser::ast::{BinaryOp, BindingPattern, UnaryOp};
use crate::typechecker::typed_ast::{AssignmentTargetKind, TypedAccessorNode, TypedArrayNode, TypedAssignmentNode, TypedAstNode, TypedBinaryNode, TypedBindingDeclNode, TypedEnumDeclNode, TypedForLoopNode, TypedFunctionDeclNode, TypedGroupedNode, TypedIdentifierNode, TypedIfNode, TypedImportNode, TypedIndexingNode, TypedInstantiationNode, TypedInvocationNode, TypedLambdaNode, TypedLiteralNode, TypedMapNode, TypedMatchNode, TypedReturnNode, TypedSetNode, TypedTupleNode, TypedTypeDeclNode, TypedUnaryNode, TypedWhileLoopNode};
use crate::typechecker::types::Type;

pub struct CCompiler {
    buf: String,
    module_name: String,
}

impl CCompiler {
    fn new() -> Self {
        CCompiler {
            buf: "".to_string(),
            module_name: "example".to_string(),
        }
    }

    pub fn gen_c(ast: Vec<TypedAstNode>) -> Result<String, ()> {
        let mut compiler = CCompiler::new();

        compiler.emit_line("#include \"abra_std.h\"\n");

        compiler.emit_line("int main(int argc, char** argv) {");

        let ast_len = ast.len();
        for (idx, node) in ast.into_iter().enumerate() {
            let should_print = idx == ast_len - 1 && node.get_type() != Type::Unit;
            if should_print { compiler.emit("  std__println("); }

            compiler.indent(1);
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

    fn indent(&mut self, level: usize) {
        self.buf.push_str(&"  ".repeat(level))
    }

    fn c_ident_name<S: AsRef<str>>(&self, name: S) -> String {
        format!("{}__{}", self.module_name, name.as_ref())
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
            TypedLiteralNode::StringLiteral(_) => todo!(),
        }

        Ok(())
    }

    fn visit_unary(&mut self, _token: Token, node: TypedUnaryNode) -> Result<(), ()> {
        match &node.typ {
            Type::Int => self.emit("NEW_INT("),
            Type::Float => self.emit("NEW_FLOAT("),
            Type::Bool => self.emit("NEW_BOOL("),
            _ => todo!()
        }

        match node.op {
            UnaryOp::Minus => {
                self.emit("-");
                self.visit_and_convert(*node.expr)?;
            }
            UnaryOp::Negate => {
                self.emit("!");
                self.visit_and_convert(*node.expr)?;
            }
        }

        self.emit(")");

        Ok(())
    }

    fn visit_binary(&mut self, _token: Token, node: TypedBinaryNode) -> Result<(), ()> {
        match &node.typ {
            Type::Int => self.emit("NEW_INT("),
            Type::Float => self.emit("NEW_FLOAT("),
            Type::Bool => self.emit("NEW_BOOL("),
            _ => todo!()
        }

        match node.op {
            BinaryOp::Add => {
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

    fn visit_array(&mut self, _token: Token, node: TypedArrayNode) -> Result<(), ()> {
        let first = node.items.into_iter().next().unwrap();
        self.visit(first)?;
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

    fn visit_if_expression(&mut self, _token: Token, node: TypedIfNode) -> Result<(), ()> {
        // TODO: Properly handling if-exprs will require them to be lifted to the top-level
        self.visit_and_convert(*node.condition)?;
        self.emit("?");

        let if_block = node.if_block.into_iter().next().unwrap();
        self.visit(if_block)?;
        self.emit(":");

        let else_block = node.else_block.unwrap().into_iter().next().unwrap();
        self.visit(else_block)?;

        Ok(())
    }

    fn visit_invocation(&mut self, _token: Token, node: TypedInvocationNode) -> Result<(), ()> {
        self.visit(*node.target)?;
        self.emit("(");

        let num_args = node.args.len();
        for (idx, arg) in node.args.into_iter().enumerate() {
            if let Some(arg) = arg {
                self.visit(arg)?;

                if idx < num_args - 1 {
                    self.emit(", ");
                }
            } else {
                todo!("Implement default-valued arguments")
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
        todo!()
    }
}
