use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode};
use crate::vm::chunk::{CompiledModule, Chunk, BindingDescriptor};
use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::lexer::tokens::Token;
use crate::vm::opcode::Opcode;
use crate::parser::ast::{UnaryOp, BinaryOp, IndexingMode};
use crate::typechecker::types::Type;
use crate::vm::value::{Value, Obj};

pub struct Compiler<'a> {
    current_chunk: String,
    module: CompiledModule<'a>,
}

pub const MAIN_CHUNK_NAME: &str = "main";

pub fn compile(module_name: &str, ast: Vec<TypedAstNode>) -> Result<CompiledModule, ()> {
    let mut module = CompiledModule::new(module_name);
    let main_chunk = Chunk::new();
    module.add_chunk(MAIN_CHUNK_NAME.to_string(), main_chunk);

    let mut compiler = Compiler { module, current_chunk: MAIN_CHUNK_NAME.to_string() };

    let last_line = ast.into_iter()
        .map(|node| {
            let line = node.get_token().get_position().line;
            compiler.visit(node).unwrap();
            line
        })
        .last()
        .unwrap_or(0);

    let mut module = compiler.module;
    module.get_chunk(MAIN_CHUNK_NAME.to_string())
        .unwrap()
        .write(Opcode::Return as u8, last_line + 1);
    Ok(module)
}

impl<'a> Compiler<'a> {
    #[inline]
    fn get_current_chunk(&mut self) -> &mut Chunk {
        let name = self.current_chunk.clone();
        self.module.get_chunk(name.to_string())
            .expect(&format!("Expected chunk named {} to exist", self.current_chunk))
    }

    #[inline]
    fn write_opcode(&mut self, opcode: Opcode, line: usize) {
        self.write_byte(opcode as u8, line);
    }

    #[inline]
    fn write_byte(&mut self, byte: u8, line: usize) {
        self.get_current_chunk().write(byte, line);
    }

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
            self.write_opcode(opcode, line);
        } else {
            let const_idx = self.module.add_constant(Value::Int(number as i64));
            self.write_opcode(Opcode::Constant, line);
            self.write_byte(const_idx, line);
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
            self.write_opcode(opcode, line);
        } else {
            self.write_int_constant(binding_idx, line);
            self.write_opcode(Opcode::Store, line);
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
            self.write_opcode(opcode, line);
        } else {
            self.write_int_constant(binding_idx, line);
            self.write_opcode(Opcode::Load, line);
        }
    }

    fn get_binding_index(&self, binding_name: &String) -> usize {
        let mut binding_idx = self.module.bindings.len() - 1;
        while binding_idx > 0 {
            if let Some(BindingDescriptor { name, .. }) = self.module.bindings.get(binding_idx) {
                if name == binding_name {
                    break;
                }
            }
            binding_idx -= 1;
        }
        binding_idx
    }
}


impl<'a> TypedAstVisitor<(), ()> for Compiler<'a> {
    fn visit_literal(&mut self, token: Token, node: TypedLiteralNode) -> Result<(), ()> {
        let line = token.get_position().line;

        if let TypedLiteralNode::BoolLiteral(val) = node {
            let opcode = if val { Opcode::T } else { Opcode::F };
            self.write_opcode(opcode, line);
            return Ok(());
        } else if let TypedLiteralNode::IntLiteral(val) = node {
            self.write_int_constant(val as u32, line);
            return Ok(());
        }

        let const_idx = match node {
            TypedLiteralNode::FloatLiteral(val) =>
                self.module.add_constant(Value::Float(val)),
            TypedLiteralNode::StringLiteral(val) =>
                self.module.add_constant(Value::Obj(Obj::StringObj { value: Box::new(val) })),
            TypedLiteralNode::IntLiteral(_) | TypedLiteralNode::BoolLiteral(_) => unreachable!() // Handled in if-let above
        };

        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);

        Ok(())
    }

    fn visit_unary(&mut self, token: Token, node: TypedUnaryNode) -> Result<(), ()> {
        let line = token.get_position().line;

        self.visit(*node.expr)?;
        match node.op {
            UnaryOp::Minus => self.write_opcode(Opcode::Invert, line),
            UnaryOp::Negate => self.write_opcode(Opcode::Negate, line),
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
            (Type::Int, Type::Float) => self.write_opcode(Opcode::F2I, line),
            (Type::Float, Type::Int) => self.write_opcode(Opcode::I2F, line),
            _ => {}
        };

        let line = right.get_token().get_position().line;
        let rtype = right.get_type();
        self.visit(right)?;
        match (node_type, rtype) {
            (Type::Int, Type::Float) => self.write_opcode(Opcode::F2I, line),
            (Type::Float, Type::Int) => self.write_opcode(Opcode::I2F, line),
            _ => {}
        };

        self.write_opcode(opcode, token.get_position().line);

        Ok(())
    }

    fn visit_grouped(&mut self, _token: Token, node: TypedGroupedNode) -> Result<(), ()> {
        let TypedGroupedNode { expr, .. } = node;
        self.visit(*expr)
    }

    fn visit_array(&mut self, token: Token, node: TypedArrayNode) -> Result<(), ()> {
        let num_items = node.items.len();
        for arr_item in node.items {
            self.visit(*arr_item)?;
        }

        let line = token.get_position().line;

        self.write_int_constant(num_items as u32, line);
        self.write_opcode(Opcode::ArrMk, line);

        Ok(())
    }

    fn visit_binding_decl(&mut self, token: Token, node: TypedBindingDeclNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedBindingDeclNode { ident, expr, scope_depth, .. } = node;
        let ident = Token::get_ident_name(&ident);

        let binding_idx = self.module.bindings.len();
        self.module.bindings.push(BindingDescriptor { name: ident.clone(), scope_depth });
        self.get_current_chunk().num_bindings += 1;

        if let Some(node) = expr {
            self.visit(*node)?;

            self.write_store_instr(binding_idx as u32, line);
        }

        Ok(())
    }

    fn visit_function_decl(&mut self, _token: Token, node: TypedFunctionDeclNode) -> Result<(), ()> {
        let TypedFunctionDeclNode { name, body, scope_depth, .. } = node;
        let func_name = Token::get_ident_name(&name);
        self.module.bindings.push(BindingDescriptor { name: func_name.clone(), scope_depth });
        self.get_current_chunk().num_bindings += 1;

        self.module.add_chunk(func_name.to_owned(), Chunk::new());
        let prev_chunk = self.current_chunk.clone();
        self.current_chunk = func_name.to_owned();

        let len = body.len();
        let mut idx = 0;
        let mut line = 0;
        for node in body {
            idx += 1;
            if idx == len {
                line = node.get_token().get_position().line;
            }
            self.visit(node)?;
        }
        self.write_opcode(Opcode::Return, line);

        self.current_chunk = prev_chunk;

        Ok(())
    }

    fn visit_identifier(&mut self, token: Token, _node: TypedIdentifierNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let ident = Token::get_ident_name(&token);
        let binding_idx = self.get_binding_index(ident);
        self.write_load_instr(binding_idx as u32, line);

        Ok(())
    }

    fn visit_assignment(&mut self, token: Token, node: TypedAssignmentNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedAssignmentNode { target, expr, .. } = node;
        let ident = match *target {
            TypedAstNode::Identifier(ident, _) => Token::get_ident_name(&ident).clone(),
            _ => unreachable!() // We can assume it's an Identifier; typechecking would have failed otherwise
        };

        self.visit(*expr)?;

        let binding_idx = self.get_binding_index(&ident);
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
                self.write_opcode(Opcode::ArrLoad, line);
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
                    self.write_opcode(Opcode::Nil, line);
                }

                self.write_opcode(Opcode::ArrSlc, line);
            }
        };

        Ok(())
    }

    fn visit_if_statement(&mut self, token: Token, node: TypedIfNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedIfNode { condition, if_block, else_block, .. } = node;

        self.visit(*condition)?;
        self.write_opcode(Opcode::JumpIfF, line);
        self.write_byte(0, line); // <- Replaced after compiling if-block
        let jump_offset_slot_idx = self.get_current_chunk().code.len();

        // TODO: Purge useless bindings after if/else-blocks exit

        for node in if_block {
            self.visit(node)?;
        }
        if else_block.is_some() {
            self.write_opcode(Opcode::Jump, line);
            self.write_byte(0, line); // <- Replaced after compiling else-block
        }

        let chunk = self.get_current_chunk();
        let if_block_len = chunk.code.len().checked_sub(jump_offset_slot_idx)
            .expect("jump offset slot should be <= end of if-block");
        *chunk.code.get_mut(jump_offset_slot_idx - 1).unwrap() = if_block_len as u8;

        let jump_offset_slot_idx = chunk.code.len();

        if let Some(else_block) = else_block {
            for node in else_block {
                self.visit(node)?;
            }
            let chunk = self.get_current_chunk();
            let else_block_len = chunk.code.len().checked_sub(jump_offset_slot_idx)
                .expect("jump offset slot should be <= end of else-block");
            *chunk.code.get_mut(jump_offset_slot_idx - 1).unwrap() = else_block_len as u8;
        }

        Ok(())
    }

    fn visit_if_expression(&mut self, token: Token, node: TypedIfNode) -> Result<(), ()> {
        self.visit_if_statement(token, node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::typechecker::typechecker::typecheck;
    use std::collections::HashMap;

    const MODULE_NAME: &str = "<test_module>";

    fn compile(input: &str) -> CompiledModule {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let (_, typed_ast) = typecheck(ast).unwrap();

        super::compile(MODULE_NAME, typed_ast).unwrap()
    }

    fn with_main_chunk(chunk: Chunk) -> HashMap<String, Chunk> {
        let mut chunks = HashMap::new();
        chunks.insert(MAIN_CHUNK_NAME.to_string(), chunk);
        chunks
    }

    #[test]
    fn compile_empty() {
        let chunk = compile("");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(
                Chunk {
                    lines: vec![1],
                    code: vec![
                        Opcode::Return as u8
                    ],
                    num_bindings: 0,
                }
            ),
            constants: vec![],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_literals() {
        let chunk = compile("1 2.3 4 5.6 \"hello\" true false");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![10, 1],
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
                num_bindings: 0,
            }),
            constants: vec![
                Value::Float(2.3),
                Value::Float(5.6),
                Value::Obj(Obj::StringObj { value: Box::new("hello".to_string()) })
            ],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_unary() {
        let chunk = compile("-5");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![3, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Invert as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![Value::Int(5)],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("-2.3");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![3, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Invert as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![Value::Float(2.3)],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("!false");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![2, 1],
                code: vec![
                    Opcode::F as u8,
                    Opcode::Negate as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_numeric() {
        let chunk = compile("5 + 6");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![5, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::IAdd as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![Value::Int(5), Value::Int(6)],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        // Testing i2f and order of ops
        let chunk = compile("1 - -5 * 3.4 / 5");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![14, 1],
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
                num_bindings: 0,
            }),
            constants: vec![Value::Int(5), Value::Float(3.4)],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_grouped() {
        let chunk = compile("(1 + 2) * 3");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![5, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::IAdd as u8,
                    Opcode::IConst3 as u8,
                    Opcode::IMul as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_str_concat() {
        let chunk = compile("\"abc\" + \"def\"");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![5, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::StrConcat as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("def".to_string()) }),
            ],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("1 + \"a\" + 3.4");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![7, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::StrConcat as u8,
                    Opcode::Constant as u8, 1,
                    Opcode::StrConcat as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Float(3.4)
            ],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_boolean() {
        let chunk = compile("true && true || false");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![5, 1],
                code: vec![
                    Opcode::T as u8,
                    Opcode::T as u8,
                    Opcode::And as u8,
                    Opcode::F as u8,
                    Opcode::Or as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_comparisons() {
        let chunk = compile("1 <= 5 == 3.4 >= 5.6");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![10, 1],
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
                num_bindings: 0,
            }),
            constants: vec![Value::Int(5), Value::Float(3.4), Value::Float(5.6)],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"a\" < \"b\" != 4");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![7, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::LT as u8,
                    Opcode::IConst4 as u8,
                    Opcode::Neq as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) })
            ],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_coalesce() {
        let chunk = compile("[\"a\", \"b\"][2] ?: \"c\"");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![11, 1],
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
                num_bindings: 0,
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
            ],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_array_primitives() {
        let chunk = compile("[1, 2]");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![4, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::ArrMk as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("[\"a\", \"b\", \"c\"]");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![8, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::Constant as u8, 2,
                    Opcode::IConst3 as u8,
                    Opcode::ArrMk as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
            ],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_array_nested() {
        let chunk = compile("[[1, 2], [3, 4, 5]]");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![12, 1],
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
                num_bindings: 0,
            }),
            constants: vec![Value::Int(5)],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binding_decl() {
        let chunk = compile("val abc = 123");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![3, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Store0 as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 1,
            }),
            constants: vec![Value::Int(123)],
            bindings: vec![BindingDescriptor { name: "abc".to_string(), scope_depth: 0 }],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("var unset: Bool\nvar set = true");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![0, 2, 1],
                code: vec![
                    Opcode::T as u8,
                    Opcode::Store1 as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 2,
            }),
            constants: vec![],
            bindings: vec![
                BindingDescriptor { name: "unset".to_string(), scope_depth: 0 },
                BindingDescriptor { name: "set".to_string(), scope_depth: 0 },
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("val abc = \"a\" + \"b\"\nval def = 5");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![6, 3, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::StrConcat as u8,
                    Opcode::Store0 as u8,
                    Opcode::Constant as u8, 2,
                    Opcode::Store1 as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 2,
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Int(5),
            ],
            bindings: vec![
                BindingDescriptor { name: "abc".to_string(), scope_depth: 0 },
                BindingDescriptor { name: "def".to_string(), scope_depth: 0 },
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_ident() {
        let chunk = compile("val abc = 123\nabc");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![3, 1, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Store0 as u8,
                    Opcode::Load0 as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 1,
            }),
            constants: vec![Value::Int(123)],
            bindings: vec![BindingDescriptor { name: "abc".to_string(), scope_depth: 0 }],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment() {
        let chunk = compile("var a = 1\nvar b = 2\nval c = b = a = 3");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![2, 2, 6, 1],
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
                num_bindings: 3,
            }),
            constants: vec![],
            bindings: vec![
                BindingDescriptor { name: "a".to_string(), scope_depth: 0 },
                BindingDescriptor { name: "b".to_string(), scope_depth: 0 },
                BindingDescriptor { name: "c".to_string(), scope_depth: 0 },
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_indexing() {
        let chunk = compile("[1, 2, 3, 4, 5][3 + 1]");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![13, 1],
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
                num_bindings: 0,
            }),
            constants: vec![Value::Int(5)],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[1 + 1:]");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![7, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::IConst1 as u8,
                    Opcode::IConst1 as u8,
                    Opcode::IAdd as u8,
                    Opcode::Nil as u8,
                    Opcode::ArrSlc as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[-1:4]");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![6, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::IConst1 as u8,
                    Opcode::Invert as u8,
                    Opcode::IConst4 as u8,
                    Opcode::ArrSlc as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[:1 + 1]");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![7, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::IConst0 as u8,
                    Opcode::IConst1 as u8,
                    Opcode::IConst1 as u8,
                    Opcode::IAdd as u8,
                    Opcode::ArrSlc as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_if_else_statements() {
        let chunk = compile("if (1 == 2) 123 else 456");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![11, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::Eq as u8,
                    Opcode::JumpIfF as u8, 4,
                    Opcode::Constant as u8, 0,
                    Opcode::Jump as u8, 2,
                    Opcode::Constant as u8, 1,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![Value::Int(123), Value::Int(456)],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("if (1 == 2) 123");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![7, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::Eq as u8,
                    Opcode::JumpIfF as u8, 2,
                    Opcode::Constant as u8, 0,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![Value::Int(123)],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("if (1 == 2) { } else { 456 }");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![9, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::Eq as u8,
                    Opcode::JumpIfF as u8, 2,
                    Opcode::Jump as u8, 2,
                    Opcode::Constant as u8, 0,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![Value::Int(456)],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("if (1 == 2) 123 else if (3 < 4) 456 else 789");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![20, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::Eq as u8,
                    Opcode::JumpIfF as u8, 4,
                    Opcode::Constant as u8, 0,
                    Opcode::Jump as u8, 11,
                    Opcode::IConst3 as u8,
                    Opcode::IConst4 as u8,
                    Opcode::LT as u8,
                    Opcode::JumpIfF as u8, 4,
                    Opcode::Constant as u8, 1,
                    Opcode::Jump as u8, 2,
                    Opcode::Constant as u8, 2,
                    Opcode::Return as u8
                ],
                num_bindings: 0,
            }),
            constants: vec![Value::Int(123), Value::Int(456), Value::Int(789)],
            bindings: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\
          val a = 123
          if (true) {\
            val a = 456\
            a + 1\
          }\
        ");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![3, 9, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Store0 as u8,
                    Opcode::T as u8,
                    Opcode::JumpIfF as u8, 6,
                    Opcode::Constant as u8, 1,
                    Opcode::Store1 as u8,
                    Opcode::Load1 as u8,
                    Opcode::IConst1 as u8,
                    Opcode::IAdd as u8,
                    Opcode::Return as u8
                ],
                num_bindings: 2,
            }),
            constants: vec![Value::Int(123), Value::Int(456)],
            bindings: vec![
                BindingDescriptor { name: "a".to_string(), scope_depth: 0 },
                BindingDescriptor { name: "a".to_string(), scope_depth: 1 },
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_declaration() {
        let chunk = compile("func abc() = 1 + 2");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: {
                let mut chunks = HashMap::new();
                chunks.insert("abc".to_string(), Chunk {
                    lines: vec![4],
                    code: vec![
                        Opcode::IConst1 as u8,
                        Opcode::IConst2 as u8,
                        Opcode::IAdd as u8,
                        Opcode::Return as u8,
                    ],
                    num_bindings: 0,
                });
                chunks.insert(MAIN_CHUNK_NAME.to_string(), Chunk {
                    lines: vec![0, 1],
                    code: vec![
                        Opcode::Return as u8
                    ],
                    num_bindings: 1,
                });
                chunks
            },
            constants: vec![],
            bindings: vec![BindingDescriptor { name: "abc".to_string(), scope_depth: 0 }],
        };
        assert_eq!(expected, chunk);
    }
}
