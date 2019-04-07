use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode};
use crate::vm::chunk::Chunk;
use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::lexer::tokens::Token;
use crate::vm::opcode::Opcode;
use crate::parser::ast::{UnaryOp, BinaryOp};
use crate::typechecker::types::Type;
use crate::vm::value::{Value, Obj};

pub struct Compiler<'a> {
    chunk: &'a mut Chunk
}

pub fn compile(ast: Vec<TypedAstNode>) -> Result<Chunk, ()> {
    let mut chunk = Chunk::new();

    let mut compiler = Compiler { chunk: &mut chunk };

    let last_line = ast.into_iter()
        .map(|node| {
            let line = node.get_token().get_position().line;
            compiler.visit(node).unwrap();
            line
        })
        .last()
        .unwrap_or(0);

    chunk.write(Opcode::Return as u8, last_line + 1);

    Ok(chunk)
}

impl<'a> TypedAstVisitor<(), ()> for Compiler<'a> {
    fn visit_literal(&mut self, token: Token, node: TypedLiteralNode) -> Result<(), ()> {
        let line = token.get_position().line;

        if let TypedLiteralNode::BoolLiteral(val) = node {
            let opcode = if val { Opcode::T } else { Opcode::F } as u8;
            self.chunk.write(opcode, line);
            return Ok(());
        }

        let const_idx = match node {
            TypedLiteralNode::IntLiteral(val) =>
                self.chunk.add_constant(Value::Int(val)),
            TypedLiteralNode::FloatLiteral(val) =>
                self.chunk.add_constant(Value::Float(val)),
            TypedLiteralNode::StringLiteral(val) =>
                self.chunk.add_constant(Value::Obj(Obj::StringObj { value: Box::new(val) })),
            TypedLiteralNode::BoolLiteral(_) => unreachable!() // Handled in if-let above
        };

        self.chunk.write(Opcode::Constant as u8, line);
        self.chunk.write(const_idx, line);

        Ok(())
    }

    fn visit_unary(&mut self, token: Token, node: TypedUnaryNode) -> Result<(), ()> {
        let line = token.get_position().line;

        self.visit(*node.expr)?;
        match node.op {
            UnaryOp::Minus => self.chunk.write(Opcode::Invert as u8, line),
            UnaryOp::Negate => self.chunk.write(Opcode::Negate as u8, line),
        }
        Ok(())
    }

    fn visit_binary(&mut self, token: Token, node: TypedBinaryNode) -> Result<(), ()> {
        let node_type = &node.typ;

        let opcode = match (node.op, node_type) {
            (BinaryOp::Add, Type::String) => Opcode::StrConcat,
            (BinaryOp::And, Type::Bool) => Opcode::And,
            (BinaryOp::Or, Type::Bool) => Opcode::Or,
            (BinaryOp::Lt, Type::Bool) => Opcode::LT,
            (BinaryOp::Lte, Type::Bool) => Opcode::LTE,
            (BinaryOp::Gt, Type::Bool) => Opcode::GT,
            (BinaryOp::Gte, Type::Bool) => Opcode::GTE,
            (BinaryOp::Eq, _) => Opcode::Eq,
            (BinaryOp::Neq, _) => Opcode::Neq,

            (BinaryOp::Add, Type::Int) => Opcode::IAdd,
            (BinaryOp::Add, Type::Float) => Opcode::FAdd,
            (BinaryOp::Sub, Type::Int) => Opcode::ISub,
            (BinaryOp::Sub, Type::Float) => Opcode::FSub,
            (BinaryOp::Mul, Type::Int) => Opcode::IMul,
            (BinaryOp::Mul, Type::Float) => Opcode::FMul,
            (BinaryOp::Div, Type::Int) => Opcode::IDiv,
            (BinaryOp::Div, Type::Float) => Opcode::FDiv,
            _ => unreachable!()
        };

        let left = *node.left;
        let right = *node.right;

        let line = left.get_token().get_position().line;
        let ltype = left.get_type();
        self.visit(left)?;
        match (node_type, ltype) {
            (Type::Int, Type::Float) => self.chunk.write(Opcode::F2I as u8, line),
            (Type::Float, Type::Int) => self.chunk.write(Opcode::I2F as u8, line),
            _ => {}
        };

        let line = right.get_token().get_position().line;
        let rtype = right.get_type();
        self.visit(right)?;
        match (node_type, rtype) {
            (Type::Int, Type::Float) => self.chunk.write(Opcode::F2I as u8, line),
            (Type::Float, Type::Int) => self.chunk.write(Opcode::I2F as u8, line),
            _ => {}
        };

        self.chunk.write(opcode as u8, token.get_position().line);

        Ok(())
    }

    fn visit_array(&mut self, token: Token, node: TypedArrayNode) -> Result<(), ()> {
        let num_items = node.items.len();
        for arr_item in node.items {
            self.visit(*arr_item);
        }

        let line = token.get_position().line;

        let const_idx = self.chunk.add_constant(Value::Int(num_items as i64));
        self.chunk.write(Opcode::Constant as u8, line);
        self.chunk.write(const_idx, line);

        self.chunk.write(Opcode::MkArr as u8, line);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::typechecker::typechecker::typecheck;

    fn compile(input: &str) -> Chunk {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = typecheck(ast).unwrap();

        super::compile(typed_ast).unwrap()
    }

    #[test]
    fn compile_empty() {
        let chunk = compile("");
        let expected = Chunk {
            lines: vec![1],
            constants: vec![],
            code: vec![
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_literals() {
        let chunk = compile("1 2.3 4 5.6 \"hello\" true false");
        let expected = Chunk {
            lines: vec![12, 1],
            constants: vec![
                Value::Int(1),
                Value::Float(2.3),
                Value::Int(4),
                Value::Float(5.6),
                Value::Obj(Obj::StringObj { value: Box::new("hello".to_string()) })
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 2,
                Opcode::Constant as u8, 3,
                Opcode::Constant as u8, 4,
                Opcode::T as u8,
                Opcode::F as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_unary() {
        let chunk = compile("-1");
        let expected = Chunk {
            lines: vec![3, 1],
            constants: vec![Value::Int(1)],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Invert as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("-2.3");
        let expected = Chunk {
            lines: vec![3, 1],
            constants: vec![Value::Float(2.3)],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Invert as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("!false");
        let expected = Chunk {
            lines: vec![2, 1],
            constants: vec![],
            code: vec![
                Opcode::F as u8,
                Opcode::Negate as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_numeric() {
        let chunk = compile("1 + 2");
        let expected = Chunk {
            lines: vec![5, 1],
            constants: vec![Value::Int(1), Value::Int(2)],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::IAdd as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);

        // Testing i2f and order of ops
        let chunk = compile("1 - -2 * 3.4 / 5");
        let expected = Chunk {
            lines: vec![15, 1],
            constants: vec![Value::Int(1), Value::Int(2), Value::Float(3.4), Value::Int(5)],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::I2F as u8,
                Opcode::Constant as u8, 1,
                Opcode::Invert as u8,
                Opcode::I2F as u8,
                Opcode::Constant as u8, 2,
                Opcode::FMul as u8,
                Opcode::Constant as u8, 3,
                Opcode::I2F as u8,
                Opcode::FDiv as u8,
                Opcode::FSub as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_str_concat() {
        let chunk = compile("\"abc\" + \"def\"");
        let expected = Chunk {
            lines: vec![5, 1],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("def".to_string()) }),
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::StrConcat as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("1 + \"a\" + 3.4");
        let expected = Chunk {
            lines: vec![8, 1],
            constants: vec![
                Value::Int(1),
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Float(3.4)
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::StrConcat as u8,
                Opcode::Constant as u8, 2,
                Opcode::StrConcat as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_boolean() {
        let chunk = compile("true && true || false");
        let expected = Chunk {
            lines: vec![5, 1],
            constants: vec![],
            code: vec![
                Opcode::T as u8,
                Opcode::T as u8,
                Opcode::And as u8,
                Opcode::F as u8,
                Opcode::Or as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_comparisons() {
        let chunk = compile("1 <= 2 == 3.4 >= 5.6");
        let expected = Chunk {
            lines: vec![11, 1],
            constants: vec![Value::Int(1), Value::Int(2), Value::Float(3.4), Value::Float(5.6)],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::LTE as u8,
                Opcode::Constant as u8, 2,
                Opcode::Constant as u8, 3,
                Opcode::GTE as u8,
                Opcode::Eq as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"a\" < \"b\" != 4");
        let expected = Chunk {
            lines: vec![8, 1],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Int(4)
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::LT as u8,
                Opcode::Constant as u8, 2,
                Opcode::Neq as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_array_primitives() {
        let chunk = compile("[1, 2]");
        let expected = Chunk {
            lines: vec![7, 1],
            constants: vec![Value::Int(1), Value::Int(2), Value::Int(2)],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 2,
                Opcode::MkArr as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("[\"a\", \"b\", \"c\"]");
        let expected = Chunk {
            lines: vec![9, 1],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
                Value::Int(3),
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 2,
                Opcode::Constant as u8, 3,
                Opcode::MkArr as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_array_nested() {
        let chunk = compile("[[1, 2], [3, 4]]");
        let expected = Chunk {
            lines: vec![17, 1],
            constants: vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(2),
                Value::Int(3),
                Value::Int(4),
                Value::Int(2),
                Value::Int(2),
           ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 2,
                Opcode::MkArr as u8,
                Opcode::Constant as u8, 3,
                Opcode::Constant as u8, 4,
                Opcode::Constant as u8, 5,
                Opcode::MkArr as u8,
                Opcode::Constant as u8, 6,
                Opcode::MkArr as u8,
                Opcode::Return as u8
            ],
        };
        assert_eq!(expected, chunk);
    }
}
