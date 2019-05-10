use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode};
use crate::vm::chunk::Chunk;
use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::lexer::tokens::Token;
use crate::vm::opcode::Opcode;
use crate::parser::ast::{UnaryOp, BinaryOp, IndexingMode};
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

impl<'a> Compiler<'a> {
    fn write_int_constant(&mut self, number: u32, line: usize) {
        if number <= 4 {
            let opcode = match number {
                0 => Opcode::IConst0,
                1 => Opcode::IConst1,
                2 => Opcode::IConst2,
                3 => Opcode::IConst3,
                4 => Opcode::IConst4,
                _ => unreachable!(), // Values greater than 4 are handled in the else-block
            };
            self.chunk.write(opcode as u8, line);
        } else {
            let const_idx = self.chunk.add_constant(Value::Int(number as i64));
            self.chunk.write(Opcode::Constant as u8, line);
            self.chunk.write(const_idx, line);
        }
    }

    fn write_store_instr(&mut self, binding_idx: u32, line: usize) {
        if binding_idx <= 4 {
            let opcode = match binding_idx {
                0 => Opcode::Store0,
                1 => Opcode::Store1,
                2 => Opcode::Store2,
                3 => Opcode::Store3,
                4 => Opcode::Store4,
                _ => unreachable!(), // Values greater than 4 are handled in the else-block
            };
            self.chunk.write(opcode as u8, line);
        } else {
            self.write_int_constant(binding_idx, line);
            self.chunk.write(Opcode::Store as u8, line);
        }
    }

    fn write_load_instr(&mut self, binding_idx: u32, line: usize) {
        if binding_idx <= 4 {
            let opcode = match binding_idx {
                0 => Opcode::Load0,
                1 => Opcode::Load1,
                2 => Opcode::Load2,
                3 => Opcode::Load3,
                4 => Opcode::Load4,
                _ => unreachable!(), // Values greater than 4 are handled in the else-block
            };
            self.chunk.write(opcode as u8, line);
        } else {
            self.write_int_constant(binding_idx, line);
            self.chunk.write(Opcode::Load as u8, line);
        }
    }
}


impl<'a> TypedAstVisitor<(), ()> for Compiler<'a> {
    fn visit_literal(&mut self, token: Token, node: TypedLiteralNode) -> Result<(), ()> {
        let line = token.get_position().line;

        if let TypedLiteralNode::BoolLiteral(val) = node {
            let opcode = if val { Opcode::T } else { Opcode::F } as u8;
            self.chunk.write(opcode, line);
            return Ok(());
        } else if let TypedLiteralNode::IntLiteral(val) = node {
            self.write_int_constant(val as u32, line);
            return Ok(());
        }

        let const_idx = match node {
            TypedLiteralNode::FloatLiteral(val) =>
                self.chunk.add_constant(Value::Float(val)),
            TypedLiteralNode::StringLiteral(val) =>
                self.chunk.add_constant(Value::Obj(Obj::StringObj { value: Box::new(val) })),
            TypedLiteralNode::IntLiteral(_) | TypedLiteralNode::BoolLiteral(_) => unreachable!() // Handled in if-let above
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
            (BinaryOp::Coalesce, _) => Opcode::Coalesce,

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
            self.visit(*arr_item)?;
        }

        let line = token.get_position().line;

        self.write_int_constant(num_items as u32, line);
        self.chunk.write(Opcode::ArrMk as u8, line);

        Ok(())
    }

    fn visit_binding_decl(&mut self, token: Token, node: TypedBindingDeclNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedBindingDeclNode { ident, expr, .. } = node;
        let ident = Token::get_ident_name(&ident);

        let binding_idx = self.chunk.bindings.len();
        self.chunk.bindings.insert(ident.clone(), binding_idx);

        if let Some(node) = expr {
            self.visit(*node)?;

            self.write_store_instr(binding_idx as u32, line);
        }

        Ok(())
    }

    fn visit_identifier(&mut self, token: Token, _typ: Type, _is_mutable: bool) -> Result<(), ()> {
        let line = token.get_position().line;

        let ident = Token::get_ident_name(&token);

        if let Some(binding_idx) = self.chunk.bindings.get(ident) {
            self.write_load_instr(*binding_idx as u32, line);
        }

        Ok(())
    }

    fn visit_assignment(&mut self, token: Token, node: TypedAssignmentNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedAssignmentNode { target, expr, .. } = node;
        let ident = match *target {
            TypedAstNode::Identifier(ident, _, _) => Token::get_ident_name(&ident).clone(),
            _ => unreachable!() // We can assume it's an Identifier; typechecking would have failed otherwise
        };

        self.visit(*expr)?;

        let binding_idx = self.chunk.bindings.get(&ident).unwrap().clone();
        self.write_store_instr(binding_idx as u32, line);
        self.write_load_instr(binding_idx as u32, line);

        Ok(())
    }

    fn visit_indexing(&mut self, token: Token, node: TypedIndexingNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedIndexingNode { target, index, .. } = node;

        self.visit(*target)?;

        match index {
            IndexingMode::Index(idx) => {
                self.visit(*idx)?;
                self.chunk.write(Opcode::ArrLoad as u8, line);
            }
            IndexingMode::Range(start, end) => {
                if let Some(start) = start {
                    self.visit(*start)?;
                } else {
                    self.write_int_constant(0, line);
                }
                if let Some(end) = end {
                    self.visit(*end)?;
                } else {
                    // Jank: Use the Nil opcode as a placeholder to signify that there is no value,
                    // and that the end of the range will need to be determined at runtime
                    self.chunk.write(Opcode::Nil as u8, line);
                }

                self.chunk.write(Opcode::ArrSlc as u8, line);
            }
        };

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::typechecker::typechecker::typecheck;
    use std::collections::HashMap;

    fn compile(input: &str) -> Chunk {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let (_, typed_ast) = typecheck(ast).unwrap();

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
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_literals() {
        let chunk = compile("1 2.3 4 5.6 \"hello\" true false");
        let expected = Chunk {
            lines: vec![10, 1],
            constants: vec![
                Value::Float(2.3),
                Value::Float(5.6),
                Value::Obj(Obj::StringObj { value: Box::new("hello".to_string()) })
            ],
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 0,
                Opcode::IConst4 as u8,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 2,
                Opcode::T as u8,
                Opcode::F as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_unary() {
        let chunk = compile("-5");
        let expected = Chunk {
            lines: vec![3, 1],
            constants: vec![Value::Int(5)],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Invert as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
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
            bindings: HashMap::new(),
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
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_numeric() {
        let chunk = compile("5 + 6");
        let expected = Chunk {
            lines: vec![5, 1],
            constants: vec![Value::Int(5), Value::Int(6)],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::IAdd as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);

        // Testing i2f and order of ops
        let chunk = compile("1 - -5 * 3.4 / 5");
        let expected = Chunk {
            lines: vec![14, 1],
            constants: vec![Value::Int(5), Value::Float(3.4)],
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::I2F as u8,
                Opcode::Constant as u8, 0,
                Opcode::Invert as u8,
                Opcode::I2F as u8,
                Opcode::Constant as u8, 1,
                Opcode::FMul as u8,
                Opcode::Constant as u8, 0,
                Opcode::I2F as u8,
                Opcode::FDiv as u8,
                Opcode::FSub as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
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
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("1 + \"a\" + 3.4");
        let expected = Chunk {
            lines: vec![7, 1],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Float(3.4)
            ],
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 0,
                Opcode::StrConcat as u8,
                Opcode::Constant as u8, 1,
                Opcode::StrConcat as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
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
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_comparisons() {
        let chunk = compile("1 <= 5 == 3.4 >= 5.6");
        let expected = Chunk {
            lines: vec![10, 1],
            constants: vec![Value::Int(5), Value::Float(3.4), Value::Float(5.6)],
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 0,
                Opcode::LTE as u8,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 2,
                Opcode::GTE as u8,
                Opcode::Eq as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"a\" < \"b\" != 4");
        let expected = Chunk {
            lines: vec![7, 1],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) })
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::LT as u8,
                Opcode::IConst4 as u8,
                Opcode::Neq as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_coalesce() {
        let chunk = compile("[\"a\", \"b\"][2] ?: \"c\"");
        let expected = Chunk {
            lines: vec![11, 1],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrLoad as u8,
                Opcode::Constant as u8, 2,
                Opcode::Coalesce as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_array_primitives() {
        let chunk = compile("[1, 2]");
        let expected = Chunk {
            lines: vec![4, 1],
            constants: vec![],
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("[\"a\", \"b\", \"c\"]");
        let expected = Chunk {
            lines: vec![8, 1],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 2,
                Opcode::IConst3 as u8,
                Opcode::ArrMk as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_array_nested() {
        let chunk = compile("[[1, 2], [3, 4, 5]]");
        let expected = Chunk {
            lines: vec![12, 1],
            constants: vec![Value::Int(5)],
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8,
                Opcode::IConst3 as u8,
                Opcode::IConst4 as u8,
                Opcode::Constant as u8, 0,
                Opcode::IConst3 as u8,
                Opcode::ArrMk as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binding_decl() {
        let chunk = compile("val abc = 123");
        let expected = Chunk {
            lines: vec![3, 1],
            constants: vec![Value::Int(123)],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Store0 as u8,
                Opcode::Return as u8
            ],
            bindings: {
                let mut bindings = HashMap::<String, usize>::new();
                bindings.insert("abc".to_string(), 0);
                bindings
            },
        };
        assert_eq!(expected, chunk);

        let chunk = compile("var unset: Bool\nvar set = true");
        let expected = Chunk {
            lines: vec![0, 2, 1],
            constants: vec![],
            code: vec![
                Opcode::T as u8,
                Opcode::Store1 as u8,
                Opcode::Return as u8
            ],
            bindings: {
                let mut bindings = HashMap::<String, usize>::new();
                bindings.insert("unset".to_string(), 0);
                bindings.insert("set".to_string(), 1);
                bindings
            },
        };
        assert_eq!(expected, chunk);

        let chunk = compile("val abc = \"a\" + \"b\"\nval def = 5");
        let expected = Chunk {
            lines: vec![6, 3, 1],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Int(5),
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::StrConcat as u8,
                Opcode::Store0 as u8,
                Opcode::Constant as u8, 2,
                Opcode::Store1 as u8,
                Opcode::Return as u8
            ],
            bindings: {
                let mut bindings = HashMap::<String, usize>::new();
                bindings.insert("abc".to_string(), 0);
                bindings.insert("def".to_string(), 1);
                bindings
            },
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_ident() {
        let chunk = compile("val abc = 123\nabc");
        let expected = Chunk {
            lines: vec![3, 1, 1],
            constants: vec![Value::Int(123)],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Store0 as u8,
                Opcode::Load0 as u8,
                Opcode::Return as u8
            ],
            bindings: {
                let mut bindings = HashMap::<String, usize>::new();
                bindings.insert("abc".to_string(), 0);
                bindings
            },
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment() {
        let chunk = compile("var a = 1\nvar b = 2\nval c = b = a = 3");
        let expected = Chunk {
            lines: vec![2, 2, 6, 1],
            constants: vec![],
            code: vec![
                // var a = 1
                Opcode::IConst1 as u8,
                Opcode::Store0 as u8,
                // var b = 2
                Opcode::IConst2 as u8,
                Opcode::Store1 as u8,

                // val c = b = a = 3
                //   a = 3
                Opcode::IConst3 as u8,
                Opcode::Store0 as u8,
                Opcode::Load0 as u8,
                //  b = <a = 3>
                Opcode::Store1 as u8,
                Opcode::Load1 as u8,
                //  c = <b = <a = 3>>
                Opcode::Store2 as u8,
                Opcode::Return as u8
            ],
            bindings: {
                let mut bindings = HashMap::<String, usize>::new();
                bindings.insert("a".to_string(), 0);
                bindings.insert("b".to_string(), 1);
                bindings.insert("c".to_string(), 2);
                bindings
            },
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_indexing() {
        let chunk = compile("[1, 2, 3, 4, 5][3 + 1]");
        let expected = Chunk {
            lines: vec![13, 1],
            constants: vec![Value::Int(5)],
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::IConst3 as u8,
                Opcode::IConst4 as u8,
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 0,
                Opcode::ArrMk as u8,
                Opcode::IConst3 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::ArrLoad as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[1 + 1:]");
        let expected = Chunk {
            lines: vec![7, 1],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::IConst1 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::Nil as u8,
                Opcode::ArrSlc as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[-1:4]");
        let expected = Chunk {
            lines: vec![6, 1],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::IConst1 as u8,
                Opcode::Invert as u8,
                Opcode::IConst4 as u8,
                Opcode::ArrSlc as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[:1 + 1]");
        let expected = Chunk {
            lines: vec![7, 1],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::IConst0 as u8,
                Opcode::IConst1 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::ArrSlc as u8,
                Opcode::Return as u8
            ],
            bindings: HashMap::new(),
        };
        assert_eq!(expected, chunk);
    }
}
