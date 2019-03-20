use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode};
use crate::vm::chunk::{Chunk, Value};
use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::lexer::tokens::Token;
use crate::vm::opcode::Opcode;

pub struct Compiler<'a> {
    chunk: &'a mut Chunk
}

//impl<'a> Compiler<'a> {
pub fn compile(ast: Vec<TypedAstNode>) -> Result<Chunk, ()> {
    let mut chunk = Chunk::new();

    let mut compiler = Compiler { chunk: &mut chunk };

    let last_line = ast.into_iter()
        .map(|node| {
            let line = node.get_token().get_position().line;
            compiler.visit(node);
            line
        })
        .last()
        .expect("There should be a last line");

    chunk.write(Opcode::Return.into(), last_line + 1);

    Ok(chunk)
}
//}

impl<'a> TypedAstVisitor<(), ()> for Compiler<'a> {
    fn visit_literal(&mut self, token: Token, node: TypedLiteralNode) -> Result<(), ()> {
        let const_idx = match node {
            TypedLiteralNode::IntLiteral(val) => self.chunk.add_constant(Value::Int(val)),
            TypedLiteralNode::FloatLiteral(val) => self.chunk.add_constant(Value::Float(val))
        };

        let line = token.get_position().line;

        self.chunk.write(Opcode::Constant.into(), line);
        self.chunk.write(const_idx, line);
        Ok(())
    }

    fn visit_unary(&mut self, token: Token, node: TypedUnaryNode) -> Result<(), ()> {
        unimplemented!()
    }

    fn visit_binary(&mut self, token: Token, node: TypedBinaryNode) -> Result<(), ()> {
        unimplemented!()
    }
}
