use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode, TypedInvocationNode, TypedWhileLoopNode, TypedForLoopNode};
use crate::vm::chunk::{CompiledModule, Chunk};
use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::lexer::tokens::Token;
use crate::vm::opcode::Opcode;
use crate::parser::ast::{UnaryOp, BinaryOp, IndexingMode};
use crate::typechecker::types::Type;
use crate::vm::value::{Value, Obj};

#[derive(Debug, PartialEq)]
pub struct Local(/* name: */ String, /* scope_depth: */ usize);

pub struct Compiler<'a> {
    current_chunk: String,
    module: CompiledModule<'a>,
    depth: usize,
    locals: Vec<Local>,
    interrupt_offset_slots: Vec<usize>,
}

pub const MAIN_CHUNK_NAME: &str = "main";

pub fn compile(module_name: &str, ast: Vec<TypedAstNode>) -> Result<CompiledModule, ()> {
    let mut module = CompiledModule::new(module_name);
    let main_chunk = Chunk::new();
    module.add_chunk(MAIN_CHUNK_NAME.to_string(), main_chunk);

    let mut compiler = Compiler {
        module,
        current_chunk: MAIN_CHUNK_NAME.to_string(),
        depth: 0,
        locals: Vec::new(),
        interrupt_offset_slots: Vec::new(),
    };

    let len = ast.len();
    let mut last_line = 0;
    for (idx, node) in (0..len).zip(ast.into_iter()) {
        let line = node.get_token().get_position().line;
        let should_pop = should_pop_after_node(&node);
        compiler.visit(node).unwrap();

        if idx != len - 1 && should_pop {
            compiler.write_opcode(Opcode::Pop, line);
        }
        last_line = line
    }

    let mut module = compiler.module;
    module.get_chunk(MAIN_CHUNK_NAME.to_string())
        .unwrap()
        .write(Opcode::Return as u8, last_line + 1);
    Ok(module)
}

fn should_pop_after_node(node: &TypedAstNode) -> bool {
    // Really this function could be `is_expression`
    match node {
        TypedAstNode::BindingDecl(_, _) |
        TypedAstNode::FunctionDecl(_, _) |
        TypedAstNode::IfStatement(_, _) |
        TypedAstNode::Break(_, _) | // This is here for completeness; the return type for this node should never matter
        TypedAstNode::ForLoop(_, _) |
        TypedAstNode::WhileLoop(_, _) => false,
        _ => true
    }
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

    fn write_constant(&mut self, value: Value, line: usize) -> u8 {
        let const_idx = self.module.add_constant(value);
        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);
        const_idx
    }

    fn write_invoke(&mut self, fn_name: String, num_args: usize, line: usize) {
        let fn_name = Value::Obj(Obj::StringObj { value: Box::new(fn_name.to_owned()) });
        self.write_constant(fn_name, line);
        self.write_opcode(Opcode::Invoke, line);
        self.write_byte(num_args as u8, line);
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
            self.write_constant(Value::Int(number as i64), line);
        }
    }

    fn write_store_local_instr(&mut self, stack_slot: usize, line: usize) {
        if stack_slot <= 4 {
            let opcode = match stack_slot {
                0 => Opcode::LStore0,
                1 => Opcode::LStore1,
                2 => Opcode::LStore2,
                3 => Opcode::LStore3,
                4 => Opcode::LStore4,
                _ => unreachable!(), // Values greater than 4 are handled in the else-block
            };
            self.write_opcode(opcode, line);
        } else {
            self.write_opcode(Opcode::LStore, line);
            self.write_byte(stack_slot as u8, line);
        }
    }

    fn write_load_local_instr(&mut self, stack_slot: usize, line: usize) {
        if stack_slot <= 4 {
            let opcode = match stack_slot {
                0 => Opcode::LLoad0,
                1 => Opcode::LLoad1,
                2 => Opcode::LLoad2,
                3 => Opcode::LLoad3,
                4 => Opcode::LLoad4,
                _ => unreachable!(), // Values greater than 4 are handled in the else-block
            };
            self.write_opcode(opcode, line);
        } else {
            self.write_opcode(Opcode::LLoad, line);
            self.write_byte(stack_slot as u8, line);
        }
    }

    fn get_num_locals_at_depth(&self, target_depth: &usize) -> usize {
        self.locals.iter().rev()
            .filter(|Local(_, depth)| depth >= target_depth)
            .count()
    }

    fn get_first_local_at_depth(&self, target_depth: &usize) -> Option<usize> {
        (0..self.locals.len()).zip(self.locals.iter()).rev()
            .filter(|(_, Local(_, depth))| depth == target_depth)
            .map(|(idx, _)| idx)
            .min()
    }

    fn get_binding_index(&self, ident: &String) -> (/* local_idx: */ usize, /* is_global: */ bool) {
        for idx in (0..self.locals.len()).rev() {
            let Local(local_name, _) = self.locals.get(idx).unwrap();
            if local_name == ident {
                return (idx, false);
            }
        }
        return (0, true);
    }

    // Called from visit_for_loop and visit_while_loop, but it has to be up here since it's
    // not part of the TypedAstVisitor trait.
    fn visit_loop_body(
        &mut self,
        body: Vec<TypedAstNode>,
        cond_slot_idx: usize, // The slot representing the start of the loop conditional
        cond_jump_offset_slot_idx: usize, // The slot representing the end of the loop
    ) -> Result<usize, ()> {
        let body_depth = self.depth;
        let body_len = body.len();
        let mut last_line = 0;
        for (idx, node) in (0..body_len).zip(body.into_iter()) {
            let line = node.get_token().get_position().line;
            last_line = line;
            let is_last_node = idx == body_len - 1;

            let should_pop = should_pop_after_node(&node);
            let is_interrupt = match &node {
                TypedAstNode::Break(_, _) => true,
                _ => false
            };
            self.visit(node)?;

            if should_pop {
                self.write_opcode(Opcode::Pop, line);
            }

            // In an interrupt (ie. a break) the local-popping and jumping bytecode
            // will already have been emitted in `visit_break`; all that needs to happen here is
            // for the compiler to no longer care about the locals in this current scope.
            // We also break out of the loop, since it's unnecessary to compile further than a break.
            if is_interrupt {
                let num_locals_to_pop = self.get_num_locals_at_depth(&body_depth);
                for _ in 0..num_locals_to_pop {
                    self.locals.pop(); // Remove from compiler's locals vector
                }
                break;
            }

            if is_last_node {
                let num_locals_to_pop = self.get_num_locals_at_depth(&body_depth);

                for _ in 0..num_locals_to_pop {
                    self.locals.pop(); // Remove from compiler's locals vector
                    self.write_opcode(Opcode::Pop, line); // TODO: PopN
                }
            }
        }

        // Calculate the number of bytes needed to jump back in order to evaluate the cond again
        let offset_to_cond = self.get_current_chunk().code.len()
            .checked_sub(cond_slot_idx)
            .expect("conditional offset slot should be <= end of loop body");
        let offset_to_cond = offset_to_cond + 2; // Account for JumpB <imm>
        self.write_opcode(Opcode::JumpB, last_line);
        self.write_byte(offset_to_cond as u8, last_line);

        // Calculate the number of bytes needed to initially skip over body, if cond was false
        let chunk = self.get_current_chunk();
        let body_len_bytes = chunk.code.len()
            .checked_sub(cond_jump_offset_slot_idx)
            .expect("jump offset slot should be <= end of loop body");
        *chunk.code.get_mut(cond_jump_offset_slot_idx - 1).unwrap() = body_len_bytes as u8;

        // Fill in any break-jump slots that have been accrued during compilation of this loop
        // Note: for nested loops, break-jumps will break out of inner loop only
        let interrupt_offset_slots = self.interrupt_offset_slots.drain(..).collect::<Vec<usize>>();
        for slot_idx in interrupt_offset_slots {
            let chunk = self.get_current_chunk();
            let break_jump_offset = chunk.code.len()
                .checked_sub(slot_idx)
                .expect("break jump offset slots should be <= end of loop body");
            *chunk.code.get_mut(slot_idx - 1).unwrap() = break_jump_offset as u8;
        }
        Ok(last_line)
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
            (BinaryOp::Mod, Type::Int) => Opcode::IMod,
            (BinaryOp::Mod, Type::Float) => Opcode::FMod,
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

        self.write_opcode(Opcode::ArrMk, line);
        self.write_byte(num_items as u8, line);
        Ok(())
    }

    fn visit_binding_decl(&mut self, token: Token, node: TypedBindingDeclNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedBindingDeclNode { ident, expr, .. } = node;
        let ident = Token::get_ident_name(&ident).clone();

        if self.depth == 0 { // If it's a global...
            if let Some(node) = expr {
                self.visit(*node)?;
            } else {
                self.write_opcode(Opcode::Nil, line);
            }
            self.write_constant(Value::Obj(Obj::StringObj { value: Box::new(ident) }), line);
            self.write_opcode(Opcode::GStore, line);
        } else { // ...otherwise, it's a local
            let local = Local(ident, self.depth);
            if let Some(node) = expr {
                self.visit(*node)?;
            } else {
                self.write_opcode(Opcode::Nil, line);
            }
            self.locals.push(local);
        }
        Ok(())
    }

    fn visit_function_decl(&mut self, token: Token, node: TypedFunctionDeclNode) -> Result<(), ()> {
        let TypedFunctionDeclNode { name, args, body, .. } = node;
        let func_name = Token::get_ident_name(&name);

        let line = token.get_position().line;
        let const_idx = self.module.add_constant(Value::Fn(func_name.clone()));
        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);
        self.write_constant(Value::Obj(Obj::StringObj { value: Box::new(func_name.clone()) }), line);
        self.write_opcode(Opcode::GStore, line);

        self.module.add_chunk(func_name.to_owned(), Chunk::new());
        let prev_chunk = self.current_chunk.clone();
        self.current_chunk = func_name.to_owned();

        self.depth += 1;
        let func_depth = self.depth;

        // Pop function arguments off stack and store in local bindings
        for (arg_token, _) in args {
            let ident = Token::get_ident_name(&arg_token);

            let local = Local(ident.clone(), self.depth);
            self.locals.push(local);
        }

        let body_len = body.len();
        let mut last_line = 0;
        for (idx, node) in (0..body_len).zip(body.into_iter()) {
            last_line = node.get_token().get_position().line;
            let is_last_line = idx == body_len - 1;
            let should_pop = should_pop_after_node(&node);
            self.visit(node)?;

            if !is_last_line && should_pop {
                self.write_opcode(Opcode::Pop, line);
            }
            if is_last_line {
                let mut num_locals_to_pop = self.get_num_locals_at_depth(&func_depth);

                if let Some(idx) = self.get_first_local_at_depth(&func_depth) {
                    self.write_store_local_instr(idx, line);
                }
                for _ in 0..num_locals_to_pop {
                    self.locals.pop();
                }
                if num_locals_to_pop != 0 {
                    num_locals_to_pop -= 1;
                }
                for _ in 0..num_locals_to_pop {
                    self.write_opcode(Opcode::Pop, line);
                }
            }
        }
        self.write_opcode(Opcode::Return, last_line);
        self.depth -= 1;

        self.current_chunk = prev_chunk;
        Ok(())
    }

    fn visit_identifier(&mut self, token: Token, _node: TypedIdentifierNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let ident = Token::get_ident_name(&token);

        let (local_idx, is_global) = self.get_binding_index(ident);
        if is_global {
            let const_idx = self.module.get_constant_index(&Value::Obj(Obj::StringObj { value: Box::new(ident.clone()) }));
            let const_idx = const_idx.unwrap();

            self.write_opcode(Opcode::Constant, line);
            self.write_byte(const_idx, line);
            self.write_opcode(Opcode::GLoad, line);
        } else {
            self.write_load_local_instr(local_idx, line);
        }
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

        let (local_idx, is_global) = self.get_binding_index(&ident);
        if is_global {
            let const_idx = self.module.get_constant_index(&Value::Obj(Obj::StringObj { value: Box::new(ident.clone()) }));
            let const_idx = const_idx.unwrap();

            self.write_opcode(Opcode::Constant, line);
            self.write_byte(const_idx, line);
            self.write_opcode(Opcode::GStore, line);
            self.write_opcode(Opcode::Constant, line);
            self.write_byte(const_idx, line);
            self.write_opcode(Opcode::GLoad, line);
        } else {
            self.write_store_local_instr(local_idx, line);
            self.write_load_local_instr(local_idx, line);
        }
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
                self.write_opcode(Opcode::OptMk, line);
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

    fn visit_if_statement(&mut self, is_stmt: bool, token: Token, node: TypedIfNode) -> Result<(), ()> {
        #[inline]
        fn compile_block(compiler: &mut Compiler, block: Vec<TypedAstNode>, is_stmt: bool) -> Result<(), ()> {
            compiler.depth += 1;
            let if_block_depth = compiler.depth;

            let block_len = block.len();
            for (idx, node) in (0..block_len).zip(block.into_iter()) {
                let line = node.get_token().get_position().line;
                let is_last_line = idx == block_len - 1;

                let should_pop = should_pop_after_node(&node);
                let is_interrupt = match &node {
                    TypedAstNode::Break(_, _) => true,
                    _ => false
                };
                compiler.visit(node)?;

                // If we're in a statement and we should pop, then pop
                // If we're in an expression and we should pop AND IT'S NOT THE LAST LINE, then pop
                if (is_stmt && should_pop) || (!is_stmt && !is_last_line && should_pop) {
                    compiler.write_opcode(Opcode::Pop, line);
                }

                // In an interrupt (ie. a break within a loop) the local-popping and jumping bytecode
                // will already have been emitted in `visit_break`; all that needs to happen here is
                // for the compiler to no longer care about the locals in this current scope.
                // We also break out of the loop, since it's unnecessary to compile further than a break.
                if is_interrupt {
                    let num_locals_to_pop = compiler.get_num_locals_at_depth(&if_block_depth);
                    for _ in 0..num_locals_to_pop {
                        compiler.locals.pop();
                    }
                    break;
                }

                // This is documented in #35
                if is_last_line {
                    let mut num_locals_to_pop = compiler.get_num_locals_at_depth(&if_block_depth);

                    if !is_stmt {
                        if let Some(idx) = compiler.get_first_local_at_depth(&if_block_depth) {
                            compiler.write_store_local_instr(idx, line);
                        }
                        for _ in 0..num_locals_to_pop {
                            compiler.locals.pop();
                        }
                        if num_locals_to_pop != 0 {
                            num_locals_to_pop -= 1;
                        }
                    } else {
                        for _ in 0..num_locals_to_pop {
                            compiler.locals.pop();
                        }
                    }
                    for _ in 0..num_locals_to_pop {
                        compiler.write_opcode(Opcode::Pop, line);
                    }
                }
            }

            compiler.depth -= 1;
            Ok(())
        }

        let line = token.get_position().line;

        let TypedIfNode { condition, if_block, else_block, .. } = node;

        self.visit(*condition)?;
        self.write_opcode(Opcode::JumpIfF, line);
        self.write_byte(0, line); // <- Replaced after compiling if-block
        let jump_offset_slot_idx = self.get_current_chunk().code.len();

        compile_block(self, if_block, is_stmt)?;
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
            compile_block(self, else_block, is_stmt)?;
            let chunk = self.get_current_chunk();
            let else_block_len = chunk.code.len().checked_sub(jump_offset_slot_idx)
                .expect("jump offset slot should be <= end of else-block");
            *chunk.code.get_mut(jump_offset_slot_idx - 1).unwrap() = else_block_len as u8;
        }
        Ok(())
    }

    fn visit_if_expression(&mut self, token: Token, node: TypedIfNode) -> Result<(), ()> {
        self.visit_if_statement(false, token, node)
    }

    fn visit_invocation(&mut self, token: Token, node: TypedInvocationNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedInvocationNode { target, args, .. } = node;

        let num_args = args.len();
        for arg in args {
            self.visit(arg)?;
        }

        let name = match *target {
            TypedAstNode::Identifier(ref token, _) => Token::get_ident_name(token),
            _ => unreachable!() // TODO: Support other, non-identifier, invokable ast notes
        };
        self.write_invoke(name.to_owned(), num_args, line);
        Ok(())
    }

    fn visit_for_loop(&mut self, token: Token, node: TypedForLoopNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedForLoopNode { iteratee, index_ident, iterator, body } = node;

        // Push intrinsic variables $idx and $iter
        self.depth += 1; // Create wrapper scope to hold invisible variables
        self.write_opcode(Opcode::IConst0, line); // Local 0 is iterator index ($idx)
        self.locals.push(Local("$idx".to_string(), self.depth));
        self.visit(*iterator)?; // Local 1 is the iterator
        self.locals.push(Local("$iter".to_string(), self.depth));

        #[inline]
        fn load_intrinsic(compiler: &mut Compiler, name: &str, line: usize) {
            let (slot, _) = compiler.get_binding_index(&name.to_string());
            compiler.write_load_local_instr(slot, line);
        }

        #[inline]
        fn store_intrinsic(compiler: &mut Compiler, name: &str, line: usize) {
            let (slot, _) = compiler.get_binding_index(&name.to_string());
            compiler.write_store_local_instr(slot, line);
        }

        // Essentially: if $idx >= arrayLen($iter) { break }
        let cond_slot_idx = self.get_current_chunk().code.len();
        load_intrinsic(self, "$idx", line);
        load_intrinsic(self, "$iter", line);
        self.write_invoke("arrayLen".to_string(), 1, line);
        self.write_opcode(Opcode::LT, line);
        self.write_opcode(Opcode::JumpIfF, line);
        self.write_byte(0, line); // <- Replaced after compiling loop body
        let cond_jump_offset_slot_idx = self.get_current_chunk().code.len();

        // Insert iteratee (bound to $iter[$idx]) and index bindings (if indexer expected) into loop scope
        self.depth += 1;
        load_intrinsic(self, "$iter", line);
        load_intrinsic(self, "$idx", line);
        self.write_opcode(Opcode::ArrLoad, line);
        self.locals.push(Local(Token::get_ident_name(&iteratee).clone(), self.depth));
        if let Some(ident) = index_ident {
            let (slot, _) = self.get_binding_index(&"$idx".to_string());
            self.write_load_local_instr(slot, line); // Load $idx
            self.locals.push(Local(Token::get_ident_name(&ident).clone(), self.depth));
        }
        load_intrinsic(self, "$idx", line);
        self.write_opcode(Opcode::IConst1, line);
        self.write_opcode(Opcode::IAdd, line);
        store_intrinsic(self, "$idx", line);

        let last_line = self.visit_loop_body(body, cond_slot_idx, cond_jump_offset_slot_idx)?;

        self.depth -= 1;
        self.write_opcode(Opcode::Pop, last_line); // Pop $iter
        self.locals.pop(); // Remove $iter from compiler's locals vector
        self.write_opcode(Opcode::Pop, last_line); // Pop $idx
        self.locals.pop(); // Remove $idx from compiler's locals vector
        self.depth -= 1;
        Ok(())
    }

    fn visit_while_loop(&mut self, token: Token, node: TypedWhileLoopNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedWhileLoopNode { condition, body } = node;
        let cond_slot_idx = self.get_current_chunk().code.len();
        self.visit(*condition)?;

        self.write_opcode(Opcode::JumpIfF, line);
        self.write_byte(0, line); // <- Replaced after compiling loop body
        let cond_jump_offset_slot_idx = self.get_current_chunk().code.len();

        self.depth += 1;
        self.visit_loop_body(body, cond_slot_idx, cond_jump_offset_slot_idx)?;
        self.depth -= 1;

        Ok(())
    }

    fn visit_break(&mut self, token: Token, loop_depth: usize) -> Result<(), ()> {
        let line = token.get_position().line;

        // Emit bytecode to pop locals from stack. The scope in which the break statement lives
        // takes care of making sure the compiler's `locals` vec is in the correct state; here we
        // just need to emit the runtime popping
        let num_locals_to_pop = self.get_num_locals_at_depth(&loop_depth);
        for _ in 0..num_locals_to_pop {
            self.write_opcode(Opcode::Pop, line); // TODO: PopN
        }

        self.write_opcode(Opcode::Jump, line);
        self.write_byte(0, line); // <- Replaced after compiling loop body (see visit_while_loop)
        let offset_slot = self.get_current_chunk().code.len();
        self.interrupt_offset_slots.push(offset_slot);
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
                }
            ),
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_literals() {
        let chunk = compile("1 2.3 4 5.6 \"hello\" true false");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![16, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::Pop as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::Pop as u8,
                    Opcode::IConst4 as u8,
                    Opcode::Pop as u8,
                    Opcode::Constant as u8, 1,
                    Opcode::Pop as u8,
                    Opcode::Constant as u8, 2,
                    Opcode::Pop as u8,
                    Opcode::T as u8,
                    Opcode::Pop as u8,
                    Opcode::F as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![
                Value::Float(2.3),
                Value::Float(5.6),
                Value::Obj(Obj::StringObj { value: Box::new("hello".to_string()) })
            ],
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
            }),
            constants: vec![Value::Int(5)],
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
            }),
            constants: vec![Value::Float(2.3)],
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
            }),
            constants: vec![],
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
            }),
            constants: vec![Value::Int(5), Value::Int(6)],
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
            }),
            constants: vec![Value::Int(5), Value::Float(3.4)],
        };
        assert_eq!(expected, chunk);

        // Testing %, along with i2f
        let chunk = compile("3.4 % 2.4 % 5");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![9, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::FMod as u8,
                    Opcode::Constant as u8, 2,
                    Opcode::I2F as u8,
                    Opcode::FMod as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![Value::Float(3.4), Value::Float(2.4), Value::Int(5)],
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
            }),
            constants: vec![],
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
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("def".to_string()) }),
            ],
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
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Float(3.4)
            ],
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
            }),
            constants: vec![],
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
            }),
            constants: vec![Value::Int(5), Value::Float(3.4), Value::Float(5.6)],
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
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) })
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_coalesce() {
        let chunk = compile("[\"a\", \"b\"][2] ?: \"c\"");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![12, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::ArrMk as u8, 2,
                    Opcode::IConst2 as u8,
                    Opcode::ArrLoad as u8,
                    Opcode::OptMk as u8,
                    Opcode::Constant as u8, 2,
                    Opcode::Coalesce as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
            ],
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
                    Opcode::ArrMk as u8, 2,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![],
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
                    Opcode::ArrMk as u8, 3,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
            ],
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
                    Opcode::ArrMk as u8, 2,
                    Opcode::IConst3 as u8,
                    Opcode::IConst4 as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::ArrMk as u8, 3,
                    Opcode::ArrMk as u8, 2,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![Value::Int(5)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binding_decl() {
        let chunk = compile("val abc = 123");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![5, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::GStore as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![Value::Int(123), Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) })],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("var unset: Bool\nvar set = true");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![4, 4, 1],
                code: vec![
                    Opcode::Nil as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GStore as u8,
                    Opcode::T as u8,
                    Opcode::Constant as u8, 1,
                    Opcode::GStore as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("unset".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("set".to_string()) })
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("val abc = \"a\" + \"b\"\nval def = 5");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![8, 5, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::StrConcat as u8,
                    Opcode::Constant as u8, 2,
                    Opcode::GStore as u8,
                    Opcode::Constant as u8, 3,
                    Opcode::Constant as u8, 4,
                    Opcode::GStore as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
                Value::Int(5),
                Value::Obj(Obj::StringObj { value: Box::new("def".to_string()) }),
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
                lines: vec![5, 3, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::GStore as u8,
                    Opcode::Constant as u8, 1,
                    Opcode::GLoad as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![Value::Int(123), Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) })],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment() {
        let chunk = compile("var a = 1\nvar b = 2\nval c = b = a = 3");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![4, 4, 16, 1],
                code: vec![
                    // var a = 1
                    Opcode::IConst1 as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GStore as u8,
                    // var b = 2
                    Opcode::IConst2 as u8,
                    Opcode::Constant as u8, 1,
                    Opcode::GStore as u8,

                    // val c = b = a = 3
                    //   a = 3
                    Opcode::IConst3 as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GStore as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GLoad as u8,
                    //  b = <a = 3>
                    Opcode::Constant as u8, 1,
                    Opcode::GStore as u8,
                    Opcode::Constant as u8, 1,
                    Opcode::GLoad as u8,
                    //  c = <b = <a = 3>>
                    Opcode::Constant as u8, 2,
                    Opcode::GStore as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("var a = 1\na = 2\nval b = 3");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![4, 8, 4, 1],
                code: vec![
                    // var a = 1
                    Opcode::IConst1 as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GStore as u8,
                    // a = 2
                    Opcode::IConst2 as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GStore as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GLoad as u8,
                    Opcode::Pop as u8, // <- This test verifies that the intermediate 2 gets popped
                    // val b = 3
                    Opcode::IConst3 as u8,
                    Opcode::Constant as u8, 1,
                    Opcode::GStore as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment_scopes() {
        let chunk = compile("var a = 1\nfunc abc() { a = 3 }");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: {
                let mut chunks = HashMap::new();
                chunks.insert(MAIN_CHUNK_NAME.to_owned(), Chunk {
                    lines: vec![4, 5, 1],
                    code: vec![
                        Opcode::IConst1 as u8,
                        Opcode::Constant as u8, 0,
                        Opcode::GStore as u8,
                        Opcode::Constant as u8, 1,
                        Opcode::Constant as u8, 2,
                        Opcode::GStore as u8,
                        Opcode::Return as u8
                    ],
                });

                chunks.insert("abc".to_owned(), Chunk {
                    lines: vec![0, 8],
                    code: vec![
                        Opcode::IConst3 as u8,
                        Opcode::Constant as u8, 0,
                        Opcode::GStore as u8,
                        Opcode::Constant as u8, 0,
                        Opcode::GLoad as u8,
                        Opcode::Return as u8
                    ],
                });

                chunks
            },
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Fn("abc".to_string()),
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
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
                    Opcode::ArrMk as u8, 5,
                    Opcode::IConst3 as u8,
                    Opcode::IConst1 as u8,
                    Opcode::IAdd as u8,
                    Opcode::ArrLoad as u8,
                    Opcode::OptMk as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![Value::Int(5)],
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
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
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
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
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
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_if_else_statements() {
        let chunk = compile("if (1 == 2) 123 else 456");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![13, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::Eq as u8,
                    Opcode::JumpIfF as u8, 5,
                    Opcode::Constant as u8, 0,
                    Opcode::Pop as u8,
                    Opcode::Jump as u8, 3,
                    Opcode::Constant as u8, 1,
                    Opcode::Pop as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![Value::Int(123), Value::Int(456)],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("if (1 == 2) 123");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![8, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::Eq as u8,
                    Opcode::JumpIfF as u8, 3,
                    Opcode::Constant as u8, 0,
                    Opcode::Pop as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![Value::Int(123)],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("if (1 == 2) { } else { 456 }");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![10, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::Eq as u8,
                    Opcode::JumpIfF as u8, 2,
                    Opcode::Jump as u8, 3,
                    Opcode::Constant as u8, 0,
                    Opcode::Pop as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![Value::Int(456)],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("if (1 == 2) 123 else if (3 < 4) 456 else 789");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![23, 1],
                code: vec![
                    Opcode::IConst1 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::Eq as u8,
                    Opcode::JumpIfF as u8, 5,
                    Opcode::Constant as u8, 0,
                    Opcode::Pop as u8,
                    Opcode::Jump as u8, 13,
                    Opcode::IConst3 as u8,
                    Opcode::IConst4 as u8,
                    Opcode::LT as u8,
                    Opcode::JumpIfF as u8, 5,
                    Opcode::Constant as u8, 1,
                    Opcode::Pop as u8,
                    Opcode::Jump as u8, 3,
                    Opcode::Constant as u8, 2,
                    Opcode::Pop as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![Value::Int(123), Value::Int(456), Value::Int(789)],
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
                lines: vec![5, 10, 1],
                code: vec![
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::GStore as u8,
                    Opcode::T as u8,
                    Opcode::JumpIfF as u8, 7,
                    Opcode::Constant as u8, 2,
                    Opcode::LLoad0 as u8,
                    Opcode::IConst1 as u8,
                    Opcode::IAdd as u8,
                    Opcode::Pop as u8,
                    Opcode::Pop as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![
                Value::Int(123),
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Int(456),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_declaration() {
        let chunk = compile("\
          val a = 1\n\
          val b = 2\n\
          val c = 3\n\
          func abc(b: Int) {\n\
            val a1 = a\n\
            val c = b + a1\n\
            c\n\
          }\
        ");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: {
                let mut chunks = HashMap::new();
                chunks.insert("abc".to_string(), Chunk {
                    lines: vec![0, 0, 0, 0, 3, 3, 1, 1, 1, 1, 1], // TODO: Fix how messed up line-counting is (#32)
                    code: vec![
                        Opcode::Constant as u8, 0,
                        Opcode::GLoad as u8,
                        Opcode::LLoad0 as u8,
                        Opcode::LLoad1 as u8,
                        Opcode::IAdd as u8,
                        Opcode::LLoad2 as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Pop as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                });
                chunks.insert(MAIN_CHUNK_NAME.to_string(), Chunk {
                    lines: vec![4, 4, 4, 5, 1],
                    code: vec![
                        Opcode::IConst1 as u8,
                        Opcode::Constant as u8, 0,
                        Opcode::GStore as u8,
                        Opcode::IConst2 as u8,
                        Opcode::Constant as u8, 1,
                        Opcode::GStore as u8,
                        Opcode::IConst3 as u8,
                        Opcode::Constant as u8, 2,
                        Opcode::GStore as u8,
                        Opcode::Constant as u8, 3,
                        Opcode::Constant as u8, 4,
                        Opcode::GStore as u8,
                        Opcode::Return as u8
                    ],
                });
                chunks
            },
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
                Value::Fn("abc".to_string()),
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_invocation() {
        let chunk = compile("\
          val one = 1\n\
          func inc(number: Int) {\n\
            number + 1\n\
          }\n
          val two = inc(number: one)\
        ");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: {
                let mut chunks = HashMap::new();
                chunks.insert("inc".to_string(), Chunk {
                    lines: vec![0, 0, 3, 1, 1],
                    code: vec![
                        Opcode::LLoad0 as u8,
                        Opcode::IConst1 as u8,
                        Opcode::IAdd as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Return as u8,
                    ],
                });
                chunks.insert(MAIN_CHUNK_NAME.to_string(), Chunk {
                    lines: vec![4, 5, 0, 0, 0, 10, 1],
                    code: vec![
                        Opcode::IConst1 as u8,
                        Opcode::Constant as u8, 0,
                        Opcode::GStore as u8,
                        Opcode::Constant as u8, 1,
                        Opcode::Constant as u8, 2,
                        Opcode::GStore as u8,
                        Opcode::Constant as u8, 0,
                        Opcode::GLoad as u8,
                        Opcode::Constant as u8, 2,
                        Opcode::Invoke as u8, 1,
                        Opcode::Constant as u8, 3,
                        Opcode::GStore as u8,
                        Opcode::Return as u8
                    ],
                });
                chunks
            },
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("one".to_string()) }),
                Value::Fn("inc".to_string()),
                Value::Obj(Obj::StringObj { value: Box::new("inc".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("two".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop() {
        let chunk = compile("\
          var i = 0\n\
          while i < 1 {\n\
            i = i + 1\n\
          }\
        ");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![4, 7, 15],
                code: vec![
                    Opcode::IConst0 as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GStore as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GLoad as u8,
                    Opcode::IConst1 as u8,
                    Opcode::LT as u8,
                    Opcode::JumpIfF as u8, 14,
                    Opcode::Constant as u8, 0,
                    Opcode::GLoad as u8,
                    Opcode::IConst1 as u8,
                    Opcode::IAdd as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GStore as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GLoad as u8,
                    Opcode::Pop as u8,
                    Opcode::JumpB as u8, 21,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("i".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop_with_local() {
        let chunk = compile("\
          var i = 0\n\
          while i < 1 {\n\
            val newI = i + 1\n\
            i = newI\n\
          }\
        ");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![4, 7, 5, 11, 1],
                code: vec![
                    Opcode::IConst0 as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GStore as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GLoad as u8,
                    Opcode::IConst1 as u8,
                    Opcode::LT as u8,
                    Opcode::JumpIfF as u8, 16,
                    Opcode::Constant as u8, 0,
                    Opcode::GLoad as u8,
                    Opcode::IConst1 as u8,
                    Opcode::IAdd as u8,
                    Opcode::LLoad0 as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GStore as u8,
                    Opcode::Constant as u8, 0,
                    Opcode::GLoad as u8,
                    Opcode::Pop as u8,
                    Opcode::Pop as u8,
                    Opcode::JumpB as u8, 23,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("i".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop_with_break() {
        let chunk = compile("\
          while true {\n\
            val i = 1\n\
            break\n\
          }\
        ");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![3, 1, 5, 1],
                code: vec![
                    Opcode::T as u8,
                    Opcode::JumpIfF as u8, 6,
                    Opcode::IConst1 as u8,
                    Opcode::Pop as u8,      // <
                    Opcode::Jump as u8, 2,  // < These 3 instrs are generated by the break
                    Opcode::JumpB as u8, 9, // These 2 get falsely attributed to the break, because of #32
                    Opcode::Return as u8
                ],
            }),
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\
          while true {\n\
            val i = 1\n\
            if i == 1 {\n\
              val a = 2\n\
              break\n\
            }\n\
          }\
        ");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![3, 1, 5, 1, 4, 1, 1, 1, 1],
                code: vec![
                    Opcode::T as u8,
                    Opcode::JumpIfF as u8, 14,
                    Opcode::IConst1 as u8,
                    Opcode::LLoad0 as u8,
                    Opcode::IConst1 as u8,
                    Opcode::Eq as u8,
                    Opcode::JumpIfF as u8, 5,
                    Opcode::IConst2 as u8,
                    Opcode::Pop as u8,      // <
                    Opcode::Pop as u8,      // <
                    Opcode::Jump as u8, 3,  // < These 4 instrs are generated by the break
                    Opcode::Pop as u8,      // This instr is where the if jumps to if false (we still need to clean up locals in the loop)
                    Opcode::JumpB as u8, 17,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_for_loop() {
        let chunk = compile("\
          val msg = \"Row: \"\n\
          val arr = [1, 2]\n\
          for a in arr {\n\
            println(msg + a)\n\
          }\
        ");
        let expected = CompiledModule {
            name: MODULE_NAME,
            chunks: with_main_chunk(Chunk {
                lines: vec![5, 7, 20, 16],
                code: vec![
                    // val msg = "Row: "
                    Opcode::Constant as u8, 0,
                    Opcode::Constant as u8, 1,
                    Opcode::GStore as u8,

                    // val arr = [1, 2]
                    Opcode::IConst1 as u8,
                    Opcode::IConst2 as u8,
                    Opcode::ArrMk as u8, 2,
                    Opcode::Constant as u8, 2,
                    Opcode::GStore as u8,

                    // val $idx = 0
                    // val $iter = arr
                    // if $idx < arrayLen($iter) {
                    Opcode::IConst0 as u8,
                    Opcode::Constant as u8, 2,
                    Opcode::GLoad as u8,
                    Opcode::LLoad0 as u8,
                    Opcode::LLoad1 as u8,
                    Opcode::Constant as u8, 3,
                    Opcode::Invoke as u8, 1,
                    Opcode::LT as u8,
                    Opcode::JumpIfF as u8, 20,

                    // a = $iter[$idx]
                    Opcode::LLoad1 as u8,
                    Opcode::LLoad0 as u8,
                    Opcode::ArrLoad as u8,

                    // $idx += 1
                    Opcode::LLoad0 as u8,
                    Opcode::IConst1 as u8,
                    Opcode::IAdd as u8,
                    Opcode::LStore0 as u8,

                    // println(msg + a)
                    // <recur>
                    Opcode::Constant as u8, 1,
                    Opcode::GLoad as u8,
                    Opcode::LLoad2 as u8,
                    Opcode::StrConcat as u8,
                    Opcode::Constant as u8, 4,
                    Opcode::Invoke as u8, 1,
                    Opcode::Pop as u8,
                    Opcode::Pop as u8,
                    Opcode::JumpB as u8, 29,

                    // Cleanup/end
                    Opcode::Pop as u8,
                    Opcode::Pop as u8,
                    Opcode::Return as u8
                ],
            }),
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("Row: ".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("msg".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("arr".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("arrayLen".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("println".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }
}
