use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::lexer::tokens::{Token, Position};
use crate::parser::ast::{UnaryOp, BinaryOp, IndexingMode, BindingPattern, ModuleId};
use crate::vm::opcode::Opcode;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode, TypedInvocationNode, TypedWhileLoopNode, TypedForLoopNode, TypedTypeDeclNode, TypedMapNode, TypedAccessorNode, TypedInstantiationNode, AssignmentTargetKind, TypedLambdaNode, TypedEnumDeclNode, TypedMatchNode, TypedReturnNode, TypedTupleNode, TypedSetNode, TypedImportNode, TypedMatchKind, TypedMatchCaseArgument};
use crate::typechecker::types::{Type, FnType};
use crate::vm::value::{Value, FnValue, TypeValue, EnumValue, NativeFn, EnumInstanceObj};
use crate::builtins::prelude::{NativeArray, NativeMap, NativeSet, NativeString};
use crate::builtins::common::default_to_string_method;
use crate::builtins::native_value_trait::NativeTyp;
use crate::common::util::random_string;
use crate::typechecker::typechecker::TypedModule;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Local {
    name: String,
    depth: usize,
    is_captured: bool,
    is_closed: bool,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd)]
pub enum UpvalueCaptureKind {
    Local { local_idx: usize },
    Upvalue { upvalue_idx: usize },
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd)]
pub struct Upvalue {
    pub capture_kind: UpvalueCaptureKind,
    depth: usize,
}

#[derive(Clone, Debug, PartialEq)]
enum ScopeKind { Root, If, Func, ForLoop, WhileLoop, Block }

#[derive(Clone, Debug, PartialEq)]
struct Scope {
    kind: ScopeKind,
    num_locals: usize,
    first_local_idx: Option<usize>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Metadata {
    pub loads: Vec<String>,
    pub stores: Vec<String>,
    pub uv_loads: Vec<String>,
    pub uv_stores: Vec<String>,
    pub field_gets: Vec<String>,
    pub local_marks: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub is_native: bool,
    pub num_globals: usize,
    pub constants: Vec<Value>,
    pub code: Vec<Opcode>,
}

#[derive(Clone)]
struct JumpHandle {
    // If it's a jump-forward, this holds the index of the Jump(IfF)? instruction in the code vec,
    // which needs to be replaced when the handle is `closed`; if it's a jump-backwards, this holds
    // the index into `code` that the JumpB instruction should jump back to.
    instr_slot: usize,
}

pub struct Compiler<'a> {
    module_id: ModuleId,
    module_idx: usize,
    code: Vec<Opcode>,
    constants: Vec<Value>,
    scopes: Vec<Scope>,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    globals: &'a mut HashMap<ModuleId, HashMap<String, usize>>,
    interrupt_handles: Vec<JumpHandle>,
    loop_start_handle: Option<JumpHandle>,
    return_handles: Vec<JumpHandle>,
    metadata: Metadata,
    anon_idx: usize,
    temp_idx: usize,
}

impl<'a> Compiler<'a> {
    pub fn new(module_id: ModuleId, module_idx: usize, globals: &'a mut HashMap<ModuleId, HashMap<String, usize>>) -> Self {
        let root_scope = Scope { kind: ScopeKind::Root, num_locals: 0, first_local_idx: None };

        Self {
            module_id,
            module_idx,
            code: vec![],
            constants: vec![],
            scopes: vec![root_scope],
            locals: vec![],
            upvalues: vec![],
            globals,
            interrupt_handles: vec![],
            loop_start_handle: None,
            return_handles: vec![],
            metadata: Metadata::default(),
            anon_idx: 0,
            temp_idx: 0,
        }
    }

    fn compile_node(&mut self, node: TypedAstNode, is_last: bool) {
        let line = node.get_token().get_position().line;
        let is_expr = is_expression(&node);
        self.visit(node).unwrap();

        if !is_last && is_expr {
            self.write_opcode(Opcode::Pop(1), line);
        }
    }

    fn build_module_final(self) -> Module {
        let num_globals = self.globals.get(&self.module_id).unwrap().len();
        Module { name: self.module_id.get_name(), is_native: false, num_globals, constants: self.constants, code: self.code }
    }
}

pub fn compile(
    module: TypedModule,
    module_idx: usize,
    globals: &mut HashMap<ModuleId, HashMap<String, usize>>,
) -> Result<(Module, Metadata), ()> {
    let mut compiler = Compiler::new(module.module_id, module_idx, globals);

    let ast = module.typed_nodes;
    compiler.hoist_fn_defs(&ast)?;

    let len = ast.len();
    let mut last_line = 0;
    for (idx, node) in ast.into_iter().enumerate() {
        let line = node.get_token().get_position().line;
        compiler.compile_node(node, idx == len - 1);
        last_line = line
    }
    compiler.write_opcode(Opcode::Return, last_line + 1);

    let metadata = compiler.metadata.clone();
    let module = compiler.build_module_final();
    Ok((module, metadata))
}

fn is_expression(node: &TypedAstNode) -> bool {
    match node {
        TypedAstNode::BindingDecl(_, _) |
        TypedAstNode::FunctionDecl(_, _) |
        TypedAstNode::TypeDecl(_, _) |
        TypedAstNode::EnumDecl(_, _) |
        TypedAstNode::IfStatement(_, _) |
        TypedAstNode::MatchStatement(_, _) |
        TypedAstNode::Break(_) | // This is here for completeness; the return type for this node should never matter
        TypedAstNode::ForLoop(_, _) |
        TypedAstNode::WhileLoop(_, _) |
        TypedAstNode::ImportStatement(_, _) => false,
        _ => true
    }
}

impl<'a> Compiler<'a> {
    #[inline]
    fn write_opcode(&mut self, opcode: Opcode, _line: usize) {
        self.code.push(opcode);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.get_constant_index(&value).unwrap_or_else(|| {
            self.constants.push(value);
            self.constants.len() - 1
        })
    }

    fn get_constant_index(&self, value: &Value) -> Option<usize> {
        self.constants.iter().position(|v| v == value)
    }

    fn get_type_constant_index(&self, type_name: &String) -> usize {
        match self.get_global_idx(&self.module_id.clone(), type_name) {
            Some(idx) => *idx,
            None => *self.get_global_idx(&ModuleId::from_name("prelude"), type_name)
                .expect(&format!("There should be a designated global slot for name {}", type_name))
        }
    }

    fn add_and_write_constant(&mut self, value: Value, line: usize) -> usize {
        let const_idx = self.add_constant(value);
        self.write_constant(self.module_idx, const_idx, line);
        const_idx
    }

    fn write_constant(&mut self, module_idx: usize, const_idx: usize, line: usize) {
        self.write_opcode(Opcode::Constant(module_idx, const_idx), line);
    }

    fn write_pops(&mut self, popped_locals: &Vec<Local>, line: usize) {
        let ops = popped_locals.iter()
            .map(|local| if local.is_captured && !local.is_closed { Opcode::CloseUpvalueAndPop } else { Opcode::Pop(1) })
            .collect::<Vec<_>>();

        let mut num_conseq_pops = 0;
        for op in ops {
            if let Opcode::Pop(_) = op {
                num_conseq_pops += 1;
            } else { // op = CloseUpvalueAndPop
                self.write_opcode(Opcode::Pop(num_conseq_pops), line);
                num_conseq_pops = 0;

                self.write_opcode(Opcode::CloseUpvalueAndPop, line);
            }
        }
        if num_conseq_pops != 0 {
            self.write_opcode(Opcode::Pop(num_conseq_pops), line);
        }
    }

    fn write_pops_for_closure(&mut self, popped_locals: &Vec<Local>, line: usize) {
        for local in popped_locals.iter().rev() {
            if local.name == "<ret>".to_string() { continue; }

            if local.is_captured && !local.is_closed {
                self.write_opcode(Opcode::CloseUpvalueAndPop, line);
            } else {
                self.write_opcode(Opcode::Pop(1), line);
            }
        }
    }

    fn int_constant_op(&mut self, number: u32) -> Opcode {
        let ops = vec![Opcode::IConst0, Opcode::IConst1, Opcode::IConst2, Opcode::IConst3, Opcode::IConst4];
        match ops.get(number as usize) {
            Some(op) => *op,
            None => Opcode::Constant(self.module_idx, self.add_constant(Value::Int(number as i64)))
        }
    }

    fn write_int_constant(&mut self, number: u32, line: usize) {
        let op = self.int_constant_op(number);
        self.write_opcode(op, line);
    }

    fn write_store_local_instr<S: AsRef<str>>(&mut self, name: S, stack_slot: usize, line: usize) {
        let name = name.as_ref();
        self.write_opcode(Opcode::LStore(stack_slot), line);
        self.metadata.stores.push(name.to_string());
    }

    fn write_load_local_instr<S: AsRef<str>>(&mut self, name: S, stack_slot: usize, line: usize) {
        let name = name.as_ref();
        self.write_opcode(Opcode::LLoad(stack_slot), line);
        self.metadata.loads.push(name.to_string());
    }

    fn write_store_upvalue_instr<S: AsRef<str>>(&mut self, name: S, upvalue_idx: usize, line: usize) {
        let name = name.as_ref();
        self.write_opcode(Opcode::UStore(upvalue_idx), line);
        self.metadata.uv_stores.push(name.to_string());
    }

    fn write_load_upvalue_instr<S: AsRef<str>>(&mut self, name: S, upvalue_idx: usize, line: usize) {
        let name = name.as_ref();
        self.write_opcode(Opcode::ULoad(upvalue_idx), line);
        self.metadata.uv_loads.push(name.to_string());
    }

    fn get_global_idx<S: AsRef<str>>(&self, module_id: &ModuleId, name: S) -> Option<&usize> {
        match self.globals.get(module_id) {
            None => unreachable!(format!("No module found for name {}", module_id)),
            Some(module_globals) => module_globals.get(name.as_ref())
        }
    }

    fn add_global_idx<S: AsRef<str>>(&mut self, name: S) -> usize {
        let num_globals = self.globals.values().map(|m| m.len()).sum();

        match self.globals.get_mut(&self.module_id) {
            None => unreachable!(format!("No module found for name {}", self.module_id)),
            Some(module_globals) => {
                let idx = num_globals;
                module_globals.insert(name.as_ref().to_string(), idx);
                idx
            }
        }
    }

    fn write_store_global_instr<S: AsRef<str>>(&mut self, name: S, line: usize) {
        let global_idx = match self.get_global_idx(&self.module_id, &name) {
            Some(idx) => *idx,
            None => self.add_global_idx(name),
        };
        self.write_opcode(Opcode::GStore(global_idx), line);
    }

    fn write_load_global_instr<S: AsRef<str>>(&mut self, name: S, line: usize) {
        let global_idx = self.get_global_idx(&self.module_id, &name);
        let global_idx = *global_idx.expect(&format!("There should be a designated global slot for name {}", name.as_ref()));
        self.write_opcode(Opcode::GLoad(global_idx), line);
    }

    // A local is resolved wrt the current function scope - any other notion of 'depth'
    // (ie. loops/ifs/etc) is irrelevant. If a local is present in an upper func scope, then it
    // should be captured as an `upvalue`.
    fn resolve_local<S: AsRef<str>>(&mut self, ident: S, depth: usize) -> Option<(&mut Local, usize)> {
        let locals_at_depth = self.locals.iter_mut()
            .filter(|Local { depth: local_depth, .. }| local_depth == &depth)
            .rev()
            .collect::<Vec<&mut Local>>();

        let mut idx = locals_at_depth.len();
        for local in locals_at_depth {
            idx -= 1;
            if local.name == ident.as_ref().to_string() {
                return Some((local, idx));
            }
        }

        return None;
    }

    fn resolve_upvalue<S: AsRef<str>>(&mut self, ident: S, depth: i64) -> Option<usize> {
        if depth < 0 { return None; }
        let depth = depth as usize;

        #[inline]
        fn add_upvalue(zelf: &mut Compiler, depth: usize, kind: UpvalueCaptureKind) -> usize {
            for (idx, upvalue) in zelf.upvalues.iter().enumerate() {
                if upvalue.capture_kind == kind && upvalue.depth == depth {
                    return match upvalue.capture_kind {
                        UpvalueCaptureKind::Local { .. } => idx,
                        UpvalueCaptureKind::Upvalue { upvalue_idx } => upvalue_idx
                    };
                }
            }

            zelf.upvalues.push(Upvalue { capture_kind: kind, depth });
            zelf.upvalues.iter().filter(|uv| uv.depth == depth).count() - 1
        }

        match self.resolve_local(&ident, depth) {
            Some((local, local_idx)) => {
                local.is_captured = true;

                let capture_kind = UpvalueCaptureKind::Local { local_idx };
                let upvalue_idx = add_upvalue(self, depth, capture_kind);
                Some(upvalue_idx)
            }
            None => match self.resolve_upvalue(ident, depth as i64 - 1) {
                None => None,
                Some(upvalue_idx) => {
                    let capture_kind = UpvalueCaptureKind::Upvalue { upvalue_idx };
                    let upvalue_idx = add_upvalue(self, depth, capture_kind);
                    Some(upvalue_idx)
                }
            }
        }
    }

    fn resolve_identifier<S: AsRef<str>>(&mut self, ident: S, line: usize) {
        let scope_depth = self.get_fn_depth();
        let ident_str = ident.as_ref().to_string();

        // Search for name among locals in current scope
        if let Some((_, local_idx)) = self.resolve_local(&ident, scope_depth) {
            self.write_load_local_instr(ident_str, local_idx, line);
            return;
        }

        // Search for name among upvalues in outer scope
        let upper_scope_depth = (scope_depth as i64) - 1;
        if let Some(upvalue_idx) = self.resolve_upvalue(&ident, upper_scope_depth) {
            self.write_load_upvalue_instr(ident_str, upvalue_idx, line);
            return;
        }

        // Search for name among this module's globals
        if let Some(global_idx) = self.get_global_idx(&self.module_id, &ident_str) {
            let global_idx = *global_idx;
            self.write_opcode(Opcode::GLoad(global_idx), line);
            return;
        }

        // Search for name among the prelude's globals
        let global_idx = self.get_global_idx(&ModuleId::from_name("prelude"), &ident_str);
        let global_idx = *global_idx.expect(&format!("There should be a designated global slot for name {}", ident_str));
        self.write_opcode(Opcode::GLoad(global_idx), line);
    }

    #[inline]
    fn get_anon_name(&mut self) -> String {
        self.anon_idx += 1;
        format!("$anon_{}", self.anon_idx - 1)
    }

    #[inline]
    fn get_temp_name(&mut self) -> String {
        self.temp_idx += 1;
        format!("$temp_{}", self.temp_idx - 1)
    }

    fn get_fn_depth(&self) -> usize {
        self.scopes.iter().filter(|s| s.kind == ScopeKind::Func).count()
    }

    fn push_local<S: AsRef<str>>(&mut self, name: S, line: usize, mark_local: bool) {
        let depth = self.get_fn_depth();
        self.locals.push(Local {
            name: name.as_ref().to_string(),
            depth,
            is_captured: false,
            is_closed: false,
        });

        let mut scope = self.scopes.last_mut().unwrap();
        scope.num_locals += 1;
        if scope.first_local_idx == None {
            let first_local_idx = self.locals.iter().filter(|l| l.depth == depth).count();
            scope.first_local_idx = Some(first_local_idx - 1);
        }

        if mark_local {
            let (_, local_idx) = self.resolve_local(name.as_ref(), depth).unwrap();
            self.metadata.local_marks.push(name.as_ref().to_string());
            self.write_opcode(Opcode::MarkLocal(local_idx), line);
        }
    }

    fn push_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope { kind, num_locals: 0, first_local_idx: None });
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn pop_scope_locals(&mut self) -> Vec<Local> {
        let &Scope { num_locals, .. } = self.current_scope();
        let split_idx = ((self.locals.len() as i64) - (num_locals as i64)) as usize;
        self.locals.split_off(split_idx)
    }

    fn current_scope(&self) -> &Scope {
        self.scopes.last().expect("There should be at least 1 scope")
    }

    fn begin_jump(&mut self, opcode: Opcode, line: usize) -> JumpHandle {
        self.write_opcode(opcode, line);
        let instr_slot = self.code.len() - 1;
        JumpHandle { instr_slot }
    }

    fn begin_jump_back(&mut self) -> JumpHandle {
        let instr_slot = self.code.len();
        JumpHandle { instr_slot }
    }

    fn close_jump(&mut self, jump_handle: JumpHandle) {
        let jump_offset = self.code.len()
            .checked_sub(jump_handle.instr_slot)
            .expect("jump offset slot should be <= current position");
        let code = &mut self.code;
        match code.get_mut(jump_handle.instr_slot) {
            Some(Opcode::Jump(offset)) => *offset = jump_offset - 1,
            Some(Opcode::JumpIfF(offset)) => *offset = jump_offset - 1,
            Some(op) => unreachable!(format!("Expected Jump/JumpIfF, got {:?}", op)),
            None => unreachable!(format!("Expected opcode at index {}, but there was none", jump_handle.instr_slot)),
        }
    }

    fn close_jump_back(&mut self, jump_handle: JumpHandle, line: usize) {
        let jump_offset = self.code.len()
            .checked_sub(jump_handle.instr_slot)
            .expect("jump offset slot should be <= current position");
        self.write_opcode(Opcode::JumpB(jump_offset + 1), line); // +1 to account for the JumpB itself
    }

    fn hoist_fn_defs(&mut self, body: &Vec<TypedAstNode>) -> Result<(), ()> {
        for n in body {
            if let TypedAstNode::FunctionDecl(tok, TypedFunctionDeclNode { name, .. }) = n {
                let line = tok.get_position().line;
                let func_name = Token::get_ident_name(&name);

                if self.current_scope().kind == ScopeKind::Root { // If it's a global...
                    self.write_int_constant(0, line);
                    self.write_store_global_instr(func_name, line);
                } else {
                    self.write_int_constant(0, line);
                    self.push_local(func_name, line, true);
                }
            }
        }
        Ok(())
    }

    // Called from visit_for_loop and visit_while_loop, but it has to be up here since it's
    // not part of the TypedAstVisitor trait.
    fn visit_loop_body(&mut self, body: Vec<TypedAstNode>, loop_end_jump_handle: JumpHandle) -> Result<usize, ()> {
        self.hoist_fn_defs(&body)?;

        let body_len = body.len();
        let mut last_line = 0;
        for (idx, node) in body.into_iter().enumerate() {
            let line = node.get_token().get_position().line;
            last_line = line;
            let is_last_node = idx == body_len - 1;

            let is_expr = is_expression(&node);
            let is_interrupt = match &node {
                TypedAstNode::Break(_) => true,
                _ => false
            };
            self.visit(node)?;

            if is_expr {
                self.write_opcode(Opcode::Pop(1), line);
            }

            // In an interrupt (ie. a break) the local-popping and jumping bytecode
            // will already have been emitted in `visit_break`; all that needs to happen here is
            // for the compiler to no longer care about the locals in this current scope.
            // We also break out of the loop, since it's unnecessary to compile further than a break.
            if is_interrupt {
                self.pop_scope_locals();
                break;
            }

            if is_last_node {
                let pops = self.pop_scope_locals();
                self.write_pops(&pops, line);
            }
        }

        // Write the instrs needed to jump back in order to evaluate the cond again
        let loop_start_handle = self.loop_start_handle.as_ref().expect("There must be a loop_start_handle if we're within a loop body").clone();
        self.close_jump_back(loop_start_handle, last_line);

        // Write the instrs needed to initially skip over body, if cond was false
        self.close_jump(loop_end_jump_handle);
        Ok(last_line)
    }

    fn compile_function_decl(
        &mut self,
        token: Token,
        name: Option<Token>,
        args: Vec<(Token, Type, bool, Option<TypedAstNode>)>,
        ret_type: Type,
        body: Vec<TypedAstNode>,
        scope_depth: usize,
    ) -> Result<FnValue, ()> {
        let func_name = match name {
            Some(name) => Token::get_ident_name(&name),
            None => self.get_anon_name()
        };

        let line = token.get_position().line;

        let mut old_code = vec![];
        std::mem::swap(&mut self.code, &mut old_code);

        let mut old_return_handles = vec![];
        std::mem::swap(&mut self.return_handles, &mut old_return_handles);

        self.push_scope(ScopeKind::Func);

        let returns_fn = if let Type::Fn(_) = &ret_type { true } else { false };

        // If the function being called returns a function, there will have been a return slot pre-allocated
        // for this function. This is necessary since the returned function could capture the first local as
        // an upvalue, and we can't overwrite that value. So we need a "first slot" that's guaranteed to not
        // be closed over. This slot must also be marked as the first local in this scope so that the return
        // value later on will be stored into this slot.
        if returns_fn {
            self.push_local("<ret>", line, false);
        }

        // Track function arguments in local bindings, also track # optional args.
        // Argument values will already be on the stack.
        for (arg_token, _, _is_vararg, default_value) in args.iter() {
            let ident = Token::get_ident_name(arg_token);

            // See comment above about not marking locals
            self.push_local(&ident, line, false);

            // This basically adds, for each default-valued parameter `p`:
            // if (p == nil) { p = defaultValueForP }
            match default_value {
                None => continue,
                Some(default_value_node) => {
                    let pos = default_value_node.get_token().get_position();
                    let typ = default_value_node.get_type();

                    let ident_node = TypedAstNode::Identifier(
                        arg_token.clone(),
                        TypedIdentifierNode { typ: typ.clone(), name: ident.clone(), is_mutable: true, scope_depth },
                    );
                    let nil_expr = TypedAstNode::_Nil(Token::Ident(pos.clone(), "nil".to_string()));
                    let default_param_value_node = TypedAstNode::IfStatement(
                        Token::If(pos.clone()),
                        TypedIfNode {
                            typ: Type::Unit,
                            condition: Box::new(
                                TypedAstNode::Binary(
                                    Token::Eq(pos.clone()),
                                    TypedBinaryNode {
                                        typ: Type::Bool,
                                        right: Box::new(ident_node.clone()),
                                        op: BinaryOp::Eq,
                                        left: Box::new(nil_expr),
                                    },
                                )
                            ),
                            condition_binding: None,
                            if_block: vec![
                                TypedAstNode::Assignment(
                                    Token::Assign(pos.clone()),
                                    TypedAssignmentNode {
                                        kind: AssignmentTargetKind::ArrayIndex,
                                        typ: typ.clone(),
                                        target: Box::new(ident_node),
                                        expr: Box::new(default_value_node.clone()),
                                    },
                                )
                            ],
                            else_block: None,
                        },
                    );
                    self.visit(default_param_value_node)?;
                }
            }
        }

        self.hoist_fn_defs(&body)?;

        let body_len = body.len();
        let mut last_line = 0;
        for (idx, node) in body.into_iter().enumerate() {
            last_line = node.get_token().get_position().line;
            let is_last_line = idx == body_len - 1;
            let is_expr = is_expression(&node);
            self.visit(node)?;

            // Handle bare expressions
            if !is_last_line && is_expr {
                self.write_opcode(Opcode::Pop(1), line);
            }
            if is_last_line {
                let mut pops = self.pop_scope_locals();

                // If there is a first local declared in this scope, store the return value into
                // that slot so it will be on top of stack when this call frame ends. If there
                // are no locals in this scope, don't pop the value and just leave it on top of stack.
                let first_local_idx = self.current_scope().first_local_idx;
                if let Some(idx) = first_local_idx {
                    self.write_store_local_instr("<ret>", idx, line);
                }
                if pops.len() != 0 {
                    pops.remove(0);
                }
                self.write_pops_for_closure(&pops, line);
            }
        }

        std::mem::swap(&mut self.return_handles, &mut old_return_handles);
        let mut return_handles = old_return_handles;
        for handle in return_handles.drain(..) {
            self.close_jump(handle);
        }

        self.write_opcode(Opcode::Return, last_line);
        self.pop_scope();

        std::mem::swap(&mut self.code, &mut old_code);
        let code = old_code;

        // Since upvalues could be transitively added in containing function scopes (to capture a
        // value from an outer scope), the upvalues for this function will be those at the given
        // fn_depth (in reverse order). The ones from other depths must remain.
        let fn_depth = self.get_fn_depth();
        let mut fn_uvs = Vec::new();
        let mut remaining_uvs = Vec::new();
        for uv in self.upvalues.drain(..).rev() {
            if uv.depth == fn_depth {
                fn_uvs.push(uv);
            } else {
                remaining_uvs.push(uv)
            }
        }
        remaining_uvs.reverse();
        self.upvalues = remaining_uvs;

        Ok(FnValue { name: func_name.clone(), code, upvalues: fn_uvs, receiver: None })
    }

    #[inline]
    fn visit_block(&mut self, block: Vec<TypedAstNode>, block_is_stmt: bool) -> Result<(), ()> {
        self.hoist_fn_defs(&block)?;

        let block_len = block.len();
        for (idx, node) in block.into_iter().enumerate() {
            let line = node.get_token().get_position().line;
            let is_last_line = idx == block_len - 1;

            let node_is_expr = is_expression(&node);
            let is_interrupt = match &node {
                TypedAstNode::Break(_) => true,
                _ => false
            };
            self.visit(node)?;

            // If we're in a statement and we should pop, then pop
            // If we're in an expression and we should pop AND IT'S NOT THE LAST LINE, then pop
            if (block_is_stmt && node_is_expr) || (!block_is_stmt && !is_last_line && node_is_expr) {
                self.write_opcode(Opcode::Pop(1), line);
            }

            // In an interrupt (ie. a break within a loop) the local-popping and jumping bytecode
            // will already have been emitted in `visit_break`; all that needs to happen here is
            // for the compiler to no longer care about the locals in this current scope.
            // We also break out of the loop, since it's unnecessary to compile further than a break.
            if is_interrupt {
                self.pop_scope_locals();
                break;
            }

            // This is documented in #35
            if is_last_line {
                let mut pops = self.pop_scope_locals();

                if !block_is_stmt {
                    let first_local_idx = self.current_scope().first_local_idx;
                    if let Some(idx) = first_local_idx {
                        // Push an empty string into metadata since this isn't a "real" store
                        self.write_store_local_instr("", idx, line);
                    }
                    if pops.len() != 0 {
                        pops.remove(0);
                    }
                }
                self.write_pops(&pops, line);
            }
        }
        Ok(())
    }

    // This is a pretty complex function; given a binding pattern, it will emit bytecode to destructure
    // the TOS value according to that pattern. There may be intermediate $temp_X bindings (globals or
    // locals, depending on context) created in scope in this process.
    // Assumptions:
    //   - For any pattern other than a plain Variable, the TOS must not be nil
    fn visit_pattern(&mut self, binding: BindingPattern) {
        #[inline]
        fn store(zelf: &mut Compiler, is_root_scope: bool, ident: String, line: usize) -> Option<usize> {
            if is_root_scope {
                zelf.write_store_global_instr(ident, line);
                None
            } else {
                zelf.push_local(ident.clone(), line, true);
                let scope_depth = zelf.get_fn_depth();
                let (_, temp_idx) = zelf.resolve_local(&ident, scope_depth).unwrap();
                Some(temp_idx)
            }
        }

        #[inline]
        fn load(zelf: &mut Compiler, is_root_scope: bool, ident: String, line: usize) {
            if is_root_scope {
                zelf.write_load_global_instr(&ident, line);
            } else {
                let scope_depth = zelf.get_fn_depth();
                let (_, temp_idx) = zelf.resolve_local(&ident, scope_depth).unwrap();
                zelf.write_load_local_instr(ident, temp_idx, line);
            }
        }

        let is_root_scope = self.current_scope().kind == ScopeKind::Root;

        match binding {
            BindingPattern::Variable(ident) => {
                // If pattern is a variable, this is a root node in the tree; write store instruction
                // (depending on whether it's a global or not) for top-of-stack.
                let line = ident.get_position().line;
                store(self, is_root_scope, Token::get_ident_name(&ident), line);
            }
            BindingPattern::Tuple(lparen_tok, patterns) => {
                // Store tuple as temp variable, then we recursively destructure each element
                let line = lparen_tok.get_position().line;
                let temp_var_name = self.get_temp_name();
                store(self, is_root_scope, temp_var_name.clone(), line);

                for (idx, pat) in patterns.into_iter().enumerate() {
                    load(self, is_root_scope, temp_var_name.clone(), line);
                    self.write_int_constant(idx as u32, line);
                    self.write_opcode(Opcode::TupleLoad, line);
                    self.visit_pattern(pat);
                }
            }
            BindingPattern::Array(lbrack_tok, patterns, is_string) => {
                // Store array as temp variable, then we recursively destructure each element
                let line = lbrack_tok.get_position().line;
                let temp_var_name = self.get_temp_name();
                store(self, is_root_scope, temp_var_name.clone(), line);

                let mut idx = 0;
                let num_pats = patterns.len();
                let mut patterns_iter = patterns.into_iter();
                while let Some((pat, is_splat)) = patterns_iter.next() {
                    let is_last = idx == num_pats - 1;

                    // If the element is a splat (and it's not the last element), push arguments for eventual splitAt invocation
                    // if is_splat && !is_last {
                    //     let line = pat.get_token().get_position().line;
                    //
                    //     self.write_opcode(Opcode::Nil, line);
                    //     self.write_int_constant((num_pats - 1 - idx) as u32, line);
                    //     self.write_opcode(Opcode::Invert, line);
                    // }

                    load(self, is_root_scope, temp_var_name.clone(), line);

                    // If the destructured pattern is a `*splat`, perform some special setup. Eg, for
                    //   val $temp_0 = [1, 2, 3, 4, 5, 6, 7]
                    //   val [v1, *mid, v2, v3] = $temp_0
                    if is_splat {
                        let line = pat.get_token().get_position().line;

                        // Get the tail of the array from X until the end, where X is the number of elements in the
                        // pattern before the `*splat`
                        //   $temp_0[1:]
                        self.write_int_constant(idx as u32, line);
                        self.write_opcode(Opcode::Nil, line);
                        self.write_opcode(Opcode::ArrSlc, line);

                        // If the splat is the last element in the destructuring pattern, there's no need to run the splitting logic
                        if is_last {
                            self.visit_pattern(pat);
                            continue;
                        }

                        // Call splitAt on that tail, passing in `-N`, where N is the
                        // number of elements in the destructuring pattern following the `*splat`. From the example above:
                        //   $temp_0[1:].splitAt(-2)
                        let split_at_method_idx = if is_string {
                            NativeString::get_struct_type().get_method_idx("splitAt").expect("String is missing required splitAt method")
                        } else {
                            NativeArray::get_struct_type().get_method_idx("splitAt").expect("Array is missing required splitAt method")
                        };
                        self.write_opcode(Opcode::GetMethod(split_at_method_idx), line);
                        self.metadata.field_gets.push("splitAt".to_string());

                        let line = pat.get_token().get_position().line;

                        self.write_int_constant((num_pats - 1 - idx) as u32, line);
                        self.write_opcode(Opcode::Invert, line);

                        self.write_opcode(Opcode::Invoke(1), line);

                        // Store that previous intermediate result (splitAt returns (T[], T[])); the first
                        // element of that tuple gets stored as the splat ident...
                        //   val $temp_1 = $temp_0[1:].splitAt(-2)
                        //   val mid = $temp_1[0]
                        let splat_temp_ident_name = self.get_temp_name();
                        store(self, is_root_scope, splat_temp_ident_name.clone(), line);
                        load(self, is_root_scope, splat_temp_ident_name.clone(), line);
                        self.write_opcode(Opcode::IConst0, line);
                        self.write_opcode(Opcode::TupleLoad, line);
                        self.visit_pattern(pat);

                        // ...and the second element of the tuple gets assigned to the original temporary binding,
                        // so that the remaining patterns in the destructuring are applied wrt that now (hence setting idx = 0):
                        //   $temp_0 = $temp_1[1]
                        load(self, is_root_scope, splat_temp_ident_name.clone(), line);
                        self.write_opcode(Opcode::IConst1, line);
                        self.write_opcode(Opcode::TupleLoad, line);
                        store(self, is_root_scope, temp_var_name.clone(), line);
                        idx = 0;
                    } else { // otherwise, it's a simple ArrLoad, then recurse into the pattern
                        self.write_int_constant(idx as u32, line);
                        self.write_opcode(Opcode::ArrLoad, line);
                        self.visit_pattern(pat);
                        idx += 1
                    }
                }
            }
        };
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

        let value = match node {
            TypedLiteralNode::FloatLiteral(val) => Value::Float(val),
            TypedLiteralNode::StringLiteral(val) => Value::new_string_obj(val),
            TypedLiteralNode::IntLiteral(_) | TypedLiteralNode::BoolLiteral(_) => unreachable!() // Handled in if-let above
        };
        self.add_and_write_constant(value, line);
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
        if node.op == BinaryOp::Coalesce {
            let left = *node.left;
            let right = *node.right;

            // When compiling a coalesce, expand it out to an if-expr with condition binding:
            //   a ?: b  =>  if a |$left_rand| $left_rand else b
            // This means that the compilation of coalescence and if-exprs generates similar bytecode
            let name = format!("$left_{}", random_string(3));
            let if_expr = TypedAstNode::IfExpression(
                Token::If(token.get_position()),
                TypedIfNode {
                    typ: Type::Placeholder, // Type doesn't matter
                    condition: Box::new(left),
                    condition_binding: Some(BindingPattern::Variable(Token::Ident(token.get_position(), name.clone()))),
                    if_block: vec![
                        TypedAstNode::Identifier(
                            Token::Ident(token.get_position(), name.clone()),
                            TypedIdentifierNode {
                                typ: Type::Placeholder, // Type doesn't matter
                                name,
                                is_mutable: false,
                                scope_depth: 0, // Doesn't matter
                            },
                        )
                    ],
                    else_block: Some(vec![right]),
                },
            );

            self.visit(if_expr)?;
            return Ok(());
        }

        let node_type = &node.typ;

        let opcode = match (&node.op, node_type) {
            (BinaryOp::Add, Type::String) => Opcode::StrConcat,
            (BinaryOp::And, Type::Bool) |
            (BinaryOp::Or, Type::Bool) => unreachable!("&& and || get transformed to if-exprs"),
            (BinaryOp::Lt, Type::Bool) => Opcode::LT,
            (BinaryOp::Lte, Type::Bool) => Opcode::LTE,
            (BinaryOp::Gt, Type::Bool) => Opcode::GT,
            (BinaryOp::Gte, Type::Bool) => Opcode::GTE,
            (BinaryOp::Xor, _) => Opcode::Xor,
            (BinaryOp::Eq, _) => Opcode::Eq,
            (BinaryOp::Neq, _) => Opcode::Neq,
            (BinaryOp::Coalesce, _) => unreachable!("Coalesce is handled in the if-block above"),

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
            (BinaryOp::Pow, _) => Opcode::Pow,
            _ => unreachable!()
        };

        let left = *node.left;
        let right = *node.right;

        let line = left.get_token().get_position().line;
        let ltype = left.get_type();
        self.visit(left)?;
        if node.op != BinaryOp::Pow {
            match (node_type, ltype) {
                (Type::Int, Type::Float) => self.write_opcode(Opcode::F2I, line),
                (Type::Float, Type::Int) => self.write_opcode(Opcode::I2F, line),
                _ => {}
            };
        }

        let line = right.get_token().get_position().line;
        let rtype = right.get_type();
        self.visit(right)?;
        if node.op != BinaryOp::Pow {
            match (node_type, rtype) {
                (Type::Int, Type::Float) => self.write_opcode(Opcode::F2I, line),
                (Type::Float, Type::Int) => self.write_opcode(Opcode::I2F, line),
                _ => {}
            };
        }

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
            self.visit(arr_item)?;
        }

        let line = token.get_position().line;

        self.write_opcode(Opcode::ArrMk(num_items), line);
        Ok(())
    }

    fn visit_tuple(&mut self, token: Token, node: TypedTupleNode) -> Result<(), ()> {
        let num_items = node.items.len();
        for item in node.items {
            self.visit(item)?;
        }

        let line = token.get_position().line;

        self.write_opcode(Opcode::TupleMk(num_items), line);
        Ok(())
    }

    fn visit_map(&mut self, token: Token, node: TypedMapNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let num_items = node.items.len();
        for (key, value) in node.items {
            self.visit(key)?;
            self.visit(value)?;
        }

        self.write_opcode(Opcode::MapMk(num_items), line);
        Ok(())
    }

    fn visit_set(&mut self, token: Token, node: TypedSetNode) -> Result<(), ()> {
        let num_items = node.items.len();
        for arr_item in node.items {
            self.visit(arr_item)?;
        }

        let line = token.get_position().line;

        self.write_opcode(Opcode::SetMk(num_items), line);
        Ok(())
    }

    fn visit_lambda(&mut self, token: Token, node: TypedLambdaNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let ret_type = if let Type::Fn(FnType { ret_type, .. }) = node.typ { *ret_type } else { unreachable!() };
        let body = node.typed_body.unwrap();
        let scope_depth = self.get_fn_depth();
        let args = node.args.into_iter()
            .map(|(tok, typ, default)| (tok, typ, false, default))
            .collect();
        let fn_value = self.compile_function_decl(token, None, args, ret_type, body, scope_depth)?;

        let has_upvalues = !&fn_value.upvalues.is_empty();
        self.add_and_write_constant(Value::Fn(fn_value), line);
        if has_upvalues {
            self.write_opcode(Opcode::ClosureMk, line);
        }

        Ok(())
    }

    fn visit_binding_decl(&mut self, token: Token, node: TypedBindingDeclNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedBindingDeclNode { binding, expr, .. } = node;

        if let Some(node) = expr {
            self.visit(*node)?;
        } else {
            self.write_opcode(Opcode::Nil, line);
        }

        // Note: for assignments, binding patterns other than Variable are guaranteed to have an initializer.
        // It will fail to parse without it.
        self.visit_pattern(binding);

        Ok(())
    }

    fn visit_nil(&mut self, token: Token) -> Result<(), ()> {
        self.write_opcode(Opcode::Nil, token.get_position().line);
        Ok(())
    }

    fn visit_function_decl(&mut self, token: Token, node: TypedFunctionDeclNode) -> Result<(), ()> {
        let func_name = Token::get_ident_name(&node.name);
        let line = token.get_position().line;

        let fn_value = self.compile_function_decl(
            token,
            Some(node.name),
            node.args,
            node.ret_type,
            node.body,
            node.scope_depth,
        )?;

        let has_upvalues = !&fn_value.upvalues.is_empty();
        self.add_and_write_constant(Value::Fn(fn_value), line);
        if has_upvalues {
            self.write_opcode(Opcode::ClosureMk, line);
        }

        // Fill in the stub allocated during the hoisting process (see hoist_fn_defs).
        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_store_global_instr(&func_name, line);
        } else {
            // For non-global fns, we must store and push the created fn/closure as a local, then
            // store (duplicate) that value into the stub created at the top of the block. This is
            // how functions can reference as-of-yet-undefined fns in their bodies. Essentially, the
            // following happens:
            //   func wrapper() {
            //     var $b: () => Unit
            //     func a() = $b()
            //     func b() = println("hello")
            //     $b = b
            //   }
            let scope_depth = self.get_fn_depth();
            let (_, stub_fn_local_idx) = self.resolve_local(&func_name, scope_depth)
                .expect("There should have been a function stub pre-defined with this name");
            self.push_local(func_name.clone(), line, true);
            let (_, real_fn_local_idx) = self.resolve_local(&func_name, scope_depth)
                .expect("There should have been a function pre-defined with this name");
            self.write_load_local_instr(&func_name, real_fn_local_idx, line);
            self.write_store_local_instr(&func_name, stub_fn_local_idx, line);
        }

        Ok(())
    }

    fn visit_type_decl(&mut self, token: Token, node: TypedTypeDeclNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedTypeDeclNode { name, fields, methods, static_fields } = node;

        let type_name = Token::get_ident_name(&name);

        // To handle self-referencing types, initially store `0` as a placeholder
        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_int_constant(0, line);
            self.write_store_global_instr(&type_name, line);
        } else { // ...otherwise, it's a local
            unreachable!("Type declarations are only allowed at the root scope");
        }

        let mut compiled_methods = Vec::with_capacity(methods.len());
        if methods.iter().find(|(name, _)| name == "toString").is_none() {
            let to_string_method = Value::NativeFn(NativeFn {
                name: "toString",
                receiver: None,
                native_fn: default_to_string_method,
            });
            compiled_methods.push(("toString".to_string(), to_string_method));
        }

        for (method_name, method_node) in methods {
            let (method_tok, method_node) = match method_node {
                TypedAstNode::FunctionDecl(tok, node) => (tok, node),
                _ => unreachable!()
            };

            let method = self.compile_function_decl(
                method_tok,
                Some(method_node.name),
                method_node.args,
                method_node.ret_type,
                method_node.body,
                method_node.scope_depth,
            )?;
            compiled_methods.push((method_name, Value::Fn(method)));
        }

        let mut compiled_static_fields = Vec::new();
        for (_, _, value) in static_fields {
            if let Some(TypedAstNode::FunctionDecl(method_tok, method_node)) = value {
                let method_name = Token::get_ident_name(&method_node.name).clone();
                let method = self.compile_function_decl(
                    method_tok,
                    Some(method_node.name),
                    method_node.args,
                    method_node.ret_type,
                    method_node.body,
                    method_node.scope_depth,
                )?;
                compiled_static_fields.push((method_name, Value::Fn(method)));
            }
        }

        let type_value = Value::Type(TypeValue {
            name: type_name.clone(),
            module_name: self.module_id.get_name(),
            fields: fields.iter().map(|f| Token::get_ident_name(&f.ident)).collect(),
            constructor: None,
            methods: compiled_methods,
            static_fields: compiled_static_fields,
        });
        self.add_and_write_constant(type_value, line);

        // Overwrite placeholder created at start
        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_store_global_instr(&type_name, line);
        } else { // ...otherwise, it's a local
            unreachable!("Type declarations are only allowed at the root scope");
        }

        Ok(())
    }

    fn visit_enum_decl(&mut self, token: Token, node: TypedEnumDeclNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedEnumDeclNode { name, variants, static_fields, methods, .. } = node;

        let enum_name = Token::get_ident_name(&name);

        // To handle self-referencing enums, initially store `0` as a placeholder
        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_int_constant(0, line);
            self.write_store_global_instr(&enum_name, line);
        } else { // ...otherwise, it's a local
            unreachable!("Enum declarations are only allowed at the root scope");
        }

        let global_idx = self.get_global_idx(&self.module_id, &enum_name);
        let type_id = *global_idx.expect(&format!("There should be a designated global slot for name {}", enum_name));

        let variants = variants.into_iter().enumerate()
            .map(|(idx, (ident_tok, (typ, variant_args)))| {
                let variant_name = Token::get_ident_name(&ident_tok);

                match typ {
                    Type::Fn(FnType { is_enum_constructor, arg_types, ret_type, .. }) => {
                        debug_assert!(is_enum_constructor);

                        // Jank: for enum variants with arguments, we need to generate a function. To generate the bytecode for the
                        // function preamble (default argument handling, which pushes locals), use `compile_function_decl`. Then,
                        // remove the final Opcode::Return, and append additional bytecode to construct the enum instance.
                        let num_args = arg_types.len();
                        let variant_args = match variant_args {
                            Some(args) => args,
                            None => std::iter::repeat(None).take(num_args).collect()
                        };
                        let args = arg_types.into_iter().zip(variant_args)
                            .map(|(at, va)| {
                                (Token::Ident(Position::new(0, 0), at.0), at.1, at.2, va)
                            })
                            .collect();
                        let constructor_fn_name = format!("{}.{}", &enum_name, &variant_name);
                        let fn_name_ident = Token::Ident(Position::new(0, 0), constructor_fn_name);
                        let mut fn_value = self.compile_function_decl(ident_tok, Some(fn_name_ident), args, *ret_type, vec![], self.get_fn_depth())?;
                        for _ in 0..num_args {
                            // When compiling the function declaration, a local is pushed for each argument; we need to pop these off so we don't pollute
                            self.locals.pop();
                        }
                        fn_value.code.pop(); // Remove Opcode::Return

                        // Extend fn with code to initialize enum variant
                        fn_value.code.extend(vec![
                            vec![self.int_constant_op(idx as u32)],
                            (0..num_args).rev().map(|i| Opcode::LLoad(i)).collect(),
                            vec![
                                Opcode::New(type_id, num_args),
                                Opcode::LStore(0),
                                Opcode::Pop(num_args - 1),
                                Opcode::Return,
                            ],
                        ].concat());
                        Ok((variant_name, Value::Fn(fn_value)))
                    }
                    Type::Reference(_, _) => {
                        let inst = EnumInstanceObj { idx, type_id, values: None };
                        Ok((variant_name, Value::new_enum_instance_obj(inst)))
                    }
                    _ => unreachable!()
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut compiled_methods = Vec::with_capacity(methods.len());
        if methods.iter().find(|(name, _)| name == "toString").is_none() {
            let to_string_method = Value::NativeFn(NativeFn {
                name: "toString",
                receiver: None,
                native_fn: default_to_string_method,
            });
            compiled_methods.push(("toString".to_string(), to_string_method));
        }

        for (method_name, method_node) in methods {
            let (method_tok, method_node) = match method_node {
                TypedAstNode::FunctionDecl(tok, node) => (tok, node),
                _ => unreachable!()
            };

            let method = self.compile_function_decl(
                method_tok,
                Some(method_node.name),
                method_node.args,
                method_node.ret_type,
                method_node.body,
                method_node.scope_depth,
            )?;
            compiled_methods.push((method_name, Value::Fn(method)));
        }

        let mut compiled_static_fields = Vec::new();
        for (_, _, value) in static_fields {
            if let Some(TypedAstNode::FunctionDecl(method_tok, method_node)) = value {
                let method_name = Token::get_ident_name(&method_node.name).clone();
                let method = self.compile_function_decl(
                    method_tok,
                    Some(method_node.name),
                    method_node.args,
                    method_node.ret_type,
                    method_node.body,
                    method_node.scope_depth,
                )?;
                compiled_static_fields.push((method_name, Value::Fn(method)));
            }
        }

        let enum_value = Value::Enum(EnumValue {
            name: enum_name.clone(),
            module_name: self.module_id.get_name(),
            variants,
            methods: compiled_methods,
            static_fields: compiled_static_fields,
        });
        self.add_and_write_constant(enum_value, line);

        // Overwrite placeholder created at start
        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_store_global_instr(&enum_name, line);
        } else { // ...otherwise, it's a local
            unreachable!("Type declarations are only allowed at the root scope");
        }

        Ok(())
    }

    fn visit_identifier(&mut self, token: Token, node: TypedIdentifierNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let ident = node.name;

        self.resolve_identifier(ident, line);
        Ok(())
    }

    fn visit_assignment(&mut self, token: Token, node: TypedAssignmentNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedAssignmentNode { target, expr, .. } = node;
        match *target {
            TypedAstNode::Indexing(_, TypedIndexingNode { target, index, .. }) => {
                let typ = target.get_type();

                self.visit(*target)?;
                match index {
                    IndexingMode::Index(idx) => self.visit(*idx)?,
                    _ => unreachable!()
                };
                self.visit(*expr)?;

                let opcode = match typ {
                    Type::Array(_) => Opcode::ArrStore,
                    Type::Tuple(_) => Opcode::TupleStore,
                    Type::Map(_, _) => Opcode::MapStore,
                    _ => unreachable!()
                };
                self.write_opcode(opcode, line);
            }
            TypedAstNode::Accessor(_, TypedAccessorNode { target, field_idx, .. }) => {
                self.visit(*expr)?;
                self.visit(*target)?;

                self.write_opcode(Opcode::SetField(field_idx), line);
            }
            TypedAstNode::Identifier(ident, _) => {
                let ident = Token::get_ident_name(&ident).clone();

                self.visit(*expr)?;

                let scope_depth = self.get_fn_depth();
                match self.resolve_local(&ident, scope_depth) {
                    Some((_, local_idx)) => { // Store to local at index
                        self.write_store_local_instr(&ident, local_idx, line);
                        self.write_load_local_instr(&ident, local_idx, line);
                    }
                    None => {
                        let upper_scope_depth = (scope_depth as i64) - 1;
                        match self.resolve_upvalue(&ident, upper_scope_depth) {
                            Some(upvalue_idx) => { // Store to upvalue at index
                                self.write_store_upvalue_instr(&ident, upvalue_idx, line);
                                self.write_load_upvalue_instr(&ident, upvalue_idx, line);
                            }
                            None => { // Store to global by name
                                self.write_store_global_instr(&ident, line);
                                self.write_load_global_instr(&ident, line);
                            }
                        }
                    }
                }
            }
            _ => todo!()
        };

        Ok(())
    }

    fn visit_indexing(&mut self, token: Token, node: TypedIndexingNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedIndexingNode { target, index, .. } = node;

        let mut target_type = target.get_type();
        if let Type::Option(inner) = target_type { target_type = *inner };

        let opcode = match &target_type {
            Type::Map(_, _) => Opcode::MapLoad,
            Type::Array(_) | Type::String => Opcode::ArrLoad,
            Type::Tuple(_) => Opcode::TupleLoad,
            _ => unreachable!()
        };
        self.visit(*target)?;

        match index {
            IndexingMode::Index(idx) => {
                self.visit(*idx)?;
                self.write_opcode(opcode, line);
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
        let line = token.get_position().line;

        let TypedIfNode { condition, condition_binding, if_block, else_block, .. } = node;

        let is_opt = match condition.get_type() {
            Type::Option(inner) => *inner != Type::Bool,
            _ => false
        };
        self.visit(*condition)?;
        if let Some(_) = &condition_binding {
            // If there is a condition binding, duplicate the condition value. After the condition
            // is evaluated in the JumpIfF (passing through the != nil check if it's an option type)
            // it will remain on the stack. Once the If scope is the current scope...
            self.write_opcode(Opcode::Dup, line);
        }
        if is_opt {
            self.write_opcode(Opcode::Nil, line);
            self.write_opcode(Opcode::Neq, line);
        }

        let if_block_jump_handle = self.begin_jump(Opcode::JumpIfF(0), line);

        self.push_scope(ScopeKind::If);
        let has_cond_binding = condition_binding.is_some();
        if let Some(pat) = condition_binding {
            // ...this value becomes the condition binding local. Since it's pushed within the If
            // scope, it'll be properly popped when the scope ends. Note that there is that value
            // floating on the stack, which is fine here since it's a local, but when we instead go
            // to the else branch...
            self.visit_pattern(pat);
        }
        self.visit_block(if_block, is_stmt)?;
        self.pop_scope();

        // ...we need to pop that floating value off of the stack. Each block correctly cleans
        // up its locals (via the pop_scope() call), but since this value is placed on the stack
        // _before_ the blocks (and only (optionally) captured as a local in _one_ of them), we need
        // to make sure we pop it within the else-block too.
        // But if there IS no else-block to compile, we still need to properly emit this pop, and we
        // need to ensure this pop is only reachable if the if-block isn't taken; let's make a dummy
        // else-block so the jumps are handled properly.
        let else_block = match else_block {
            Some(block) => Some(block),
            None => if has_cond_binding { Some(vec![]) } else { None }
        };
        let else_block_jump_handle = if else_block.is_some() {
            Some(self.begin_jump(Opcode::Jump(0), line))
        } else { None };

        self.close_jump(if_block_jump_handle);

        if let Some(else_block) = else_block {
            // Pop the floating condition binding value off the stack, if present. See comment above
            // for how we know we can always do this here (tl;dr there will _always_ be an else-block
            // if there is a condition binding).
            if has_cond_binding {
                self.write_opcode(Opcode::Pop(1), line);
            }

            self.push_scope(ScopeKind::If);
            self.visit_block(else_block, is_stmt)?;
            self.pop_scope();

            self.close_jump(else_block_jump_handle.expect("Should exist, from above"));
        }
        Ok(())
    }

    fn visit_if_expression(&mut self, token: Token, node: TypedIfNode) -> Result<(), ()> {
        self.visit_if_statement(false, token, node)
    }

    fn visit_match_statement(&mut self, is_stmt: bool, token: Token, node: TypedMatchNode) -> Result<(), ()> {
        let TypedMatchNode { target, branches, .. } = node;

        self.visit(*target)?;

        let mut end_match_jumps = Vec::new();
        let mut next_case_jump_handles = Vec::new();

        for (match_kind, binding, block) in branches {
            self.push_scope(ScopeKind::Block);

            let args = match match_kind {
                TypedMatchKind::Wildcard => {
                    if let Some(binding_name) = &binding {
                        self.push_local(binding_name, token.get_position().line, true);
                    } else {
                        // Pop match target if it's not bound as a local
                        self.write_opcode(Opcode::Pop(1), token.get_position().line);
                    }
                    self.visit_block(block, is_stmt)?;

                    self.pop_scope();
                    continue;
                }
                TypedMatchKind::None => {
                    self.write_opcode(Opcode::Dup, token.get_position().line);
                    self.write_opcode(Opcode::Nil, token.get_position().line);
                    self.write_opcode(Opcode::Eq, token.get_position().line);
                    next_case_jump_handles.push(self.begin_jump(Opcode::JumpIfF(0), token.get_position().line));

                    None
                }
                TypedMatchKind::Type { type_name, args } => {
                    let type_global_idx = self.get_type_constant_index(&type_name);

                    self.write_opcode(Opcode::Dup, token.get_position().line);
                    self.write_opcode(Opcode::Typeof, token.get_position().line);
                    self.write_opcode(Opcode::GLoad(type_global_idx), token.get_position().line);
                    self.write_opcode(Opcode::Eq, token.get_position().line);
                    next_case_jump_handles.push(self.begin_jump(Opcode::JumpIfF(0), token.get_position().line));

                    args
                }
                TypedMatchKind::EnumVariant { variant_idx, args } => {
                    self.write_opcode(Opcode::Dup, token.get_position().line);
                    self.write_int_constant(variant_idx as u32, token.get_position().line);

                    self.write_opcode(Opcode::Eq, token.get_position().line);
                    next_case_jump_handles.push(self.begin_jump(Opcode::JumpIfF(0), token.get_position().line));

                    if let Some(args) = &args {
                        for (idx, arg) in args.iter().enumerate() {
                            match arg {
                                TypedMatchCaseArgument::Pattern(_) => { continue; }
                                TypedMatchCaseArgument::Literal(arg) => {
                                    self.write_opcode(Opcode::Dup, token.get_position().line);
                                    self.metadata.field_gets.push(format!("_{}", idx));
                                    self.write_opcode(Opcode::GetField(idx), token.get_position().line);
                                    self.visit(arg.clone())?;
                                    self.write_opcode(Opcode::Eq, token.get_position().line);
                                    next_case_jump_handles.push(self.begin_jump(Opcode::JumpIfF(0), token.get_position().line));
                                }
                            }
                        }
                    }

                    args
                }
                TypedMatchKind::Constant { node, } => {
                    self.write_opcode(Opcode::Dup, token.get_position().line);
                    self.visit(node)?;
                    self.write_opcode(Opcode::Eq, token.get_position().line);
                    next_case_jump_handles.push(self.begin_jump(Opcode::JumpIfF(0), token.get_position().line));

                    None
                }
                TypedMatchKind::Tuple { nodes, } => {
                    self.write_opcode(Opcode::Dup, token.get_position().line);
                    let tuple_node = TypedTupleNode { typ: Type::Placeholder, items: nodes };
                    self.visit_tuple(token.clone(), tuple_node)?;
                    self.write_opcode(Opcode::Eq, token.get_position().line);
                    next_case_jump_handles.push(self.begin_jump(Opcode::JumpIfF(0), token.get_position().line));

                    None
                }
            };

            // Push a local representing the match target still left on the stack (from the previous DUP).
            // If there is a name supplied in source to ascribe to that value we use it, otherwise use the intrinsic $match_target.
            // This value will be inaccessible if there is no label for it in source, but is necessary either way
            // to facilitate destructuring (if destructured_args are present in this match case).
            let binding_name = binding.unwrap_or("$match_target".to_string());
            self.push_local(&binding_name, token.get_position().line, true);
            if let Some(destructured_args) = args {
                let depth = self.get_fn_depth();
                for (idx, pat) in destructured_args.into_iter().enumerate() {
                    match pat {
                        TypedMatchCaseArgument::Pattern(pat) => {
                            let (_, slot) = self.resolve_local(&binding_name, depth).unwrap();
                            self.write_load_local_instr(&binding_name, slot, token.get_position().line);

                            self.metadata.field_gets.push(format!("_{}", idx));
                            self.write_opcode(Opcode::GetField(idx), token.get_position().line);
                            self.visit_pattern(pat);
                        }
                        TypedMatchCaseArgument::Literal(_) => { continue; }
                    }
                }
            }

            self.visit_block(block, is_stmt)?;

            let end_match_jump_handle = self.begin_jump(Opcode::Jump(0), token.get_position().line);
            end_match_jumps.push(end_match_jump_handle);

            for handle in next_case_jump_handles.drain(..) {
                self.close_jump(handle);
            }

            self.pop_scope();
        }

        for jump_holder in end_match_jumps {
            self.close_jump(jump_holder);
        }

        Ok(())
    }

    fn visit_match_expression(&mut self, token: Token, node: TypedMatchNode) -> Result<(), ()> {
        self.visit_match_statement(false, token, node)
    }

    fn visit_invocation(&mut self, token: Token, node: TypedInvocationNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedInvocationNode { target, args, .. } = node;

        let typ = target.get_type();
        let (arity, returns_fn) = match typ.get_opt_unwrapped() {
            Type::Fn(FnType { arg_types, ret_type, .. }) => {
                let returns_fn = if let Type::Fn(_) = &*ret_type { true } else { false };
                (arg_types.len(), returns_fn)
            }
            _ => unreachable!() // This should have been caught during typechecking
        };

        if typ.is_opt() {
            #[inline]
            fn store(zelf: &mut Compiler, is_root_scope: bool, ident: String, line: usize) -> Option<usize> {
                if is_root_scope {
                    zelf.write_store_global_instr(&ident, line);
                    None
                } else {
                    zelf.push_local(ident.clone(), line, true);
                    let scope_depth = zelf.get_fn_depth();
                    let (_, temp_idx) = zelf.resolve_local(&ident, scope_depth).unwrap();
                    Some(temp_idx)
                }
            }

            #[inline]
            fn load(zelf: &mut Compiler, is_root_scope: bool, ident: String, line: usize) {
                if is_root_scope {
                    zelf.write_load_global_instr(&ident, line);
                } else {
                    let scope_depth = zelf.get_fn_depth();
                    let (_, temp_idx) = zelf.resolve_local(&ident, scope_depth).unwrap();
                    zelf.write_load_local_instr(ident, temp_idx, line);
                }
            }

            self.visit(*target)?;
            let tmp_name = self.get_temp_name();
            let is_root_scope = self.current_scope().kind == ScopeKind::Root;
            store(self, is_root_scope, tmp_name.clone(), line);
            load(self, is_root_scope, tmp_name.clone(), line);
            self.write_opcode(Opcode::Nil, line);
            self.write_opcode(Opcode::Eq, line);
            let else_jump_handle = self.begin_jump(Opcode::JumpIfF(0), line);

            self.write_opcode(Opcode::Nil, line);
            let if_end_jump_handle = self.begin_jump(Opcode::Jump(0), line);

            self.close_jump(else_jump_handle);

            load(self, is_root_scope, tmp_name, line);

            // If the function returns a function, we need to pre-allocated a return value slot for the function.
            // See self.compile_function_decl
            if returns_fn {
                self.write_opcode(Opcode::Nil, line);
            }

            for arg in args {
                match arg {
                    None => self.write_opcode(Opcode::Nil, line),
                    Some(arg) => self.visit(arg)?
                }
            }

            let arity = arity + if returns_fn { 1 } else { 0 };
            self.write_opcode(Opcode::Invoke(arity), line);
            self.close_jump(if_end_jump_handle);

            return Ok(());
        }

        self.visit(*target)?;

        // If the function returns a function, we need to pre-allocated a return value slot for the function.
        // See self.compile_function_decl
        if returns_fn {
            self.write_opcode(Opcode::Nil, line);
        }

        for arg in args {
            match arg {
                None => self.write_opcode(Opcode::Nil, line),
                Some(arg) => self.visit(arg)?
            }
        }

        // If we've pre-allocated a return value slot, then we need to make sure that slot is captured
        // as a local when the function's call frame is handled.
        let arity = arity + if returns_fn { 1 } else { 0 };
        self.write_opcode(Opcode::Invoke(arity), line);
        Ok(())
    }

    fn visit_instantiation(&mut self, token: Token, node: TypedInstantiationNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedInstantiationNode { target, fields, .. } = node;

        let type_global_idx = if let TypedAstNode::Identifier(_, TypedIdentifierNode { name, .. }) = *target {
            *self.get_global_idx(&self.module_id, &name)
                .expect(&format!("There should be a designated global slot for name {}", name))
        } else if let TypedAstNode::Accessor(_, TypedAccessorNode { target, field_ident, .. }) = *target {
            if let Type::Module(module_id) = target.get_type() {
                let name = Token::get_ident_name(&field_ident);
                *self.get_global_idx(&module_id, &name)
                    .expect(&format!("There should be a designated global slot for name {}/{}", module_id.get_name(), name))
            } else {
                unimplemented!("Unsupported instantiation on line {}", line)
            }
        } else {
            unimplemented!("Unsupported instantiation on line {}", line)
        };

        let num_fields = fields.len();
        for (_, field_value) in fields.into_iter().rev() {
            self.visit(field_value)?;
        }

        self.write_opcode(Opcode::New(type_global_idx, num_fields), line);
        Ok(())
    }

    fn visit_accessor(&mut self, token: Token, node: TypedAccessorNode) -> Result<(), ()> {
        let line = token.get_position().line;

        if node.is_opt_safe {
            // If we're in an opt-safe accessor (usage of the `?.` operator), we emit bytecode
            // for a transformed AST. We transform an expression like `a.b?.c?.d` to
            //   if a.b |$0| {
            //     if $0.c |$1| {
            //       if $1.d |$2| {
            //         $2
            //       }
            //     }
            //   }
            // to take advantage of the if-stmt/expr's condition binding. To achieve this, we first
            // must "unwind" the expression until we reach a non-opt-safe accessor, and then...
            let mut unwound_path = vec![];

            let this_node = Box::new(TypedAstNode::Accessor(token.clone(), node));
            let mut next = &this_node;
            loop {
                match &**next {
                    node @ TypedAstNode::Accessor(_, _) => {
                        if let TypedAstNode::Accessor(_, TypedAccessorNode { target, is_opt_safe, .. }) = &node {
                            unwound_path.push(node);
                            if *is_opt_safe { next = target } else { break; }
                        }
                    }
                    node => break unwound_path.push(node)
                }
            }

            fn make_dummy_ident_node(tok: &Token, name: String) -> TypedAstNode {
                let ident = Token::Ident(tok.get_position(), name.clone());
                // The `typ` and `scope_depth` fields don't matter here
                TypedAstNode::Identifier(ident, TypedIdentifierNode { typ: Type::Placeholder, name, is_mutable: false, scope_depth: 0 })
            }

            // ...we iterate through that path, starting with the innermost if-expr, building outward
            // until we reach the first non-opt accessor expr.
            let path_len = unwound_path.len();
            let mut layer_number = path_len;
            let mut if_node = None;
            for (idx, segment) in unwound_path.into_iter().map(|n| n.clone()).enumerate() {
                let is_last = idx == path_len - 1;
                let condition = if is_last {
                    segment
                } else {
                    match segment {
                        TypedAstNode::Accessor(tok, node) => {
                            let prev_cond_binding_name = format!("${}", layer_number - 1);
                            let target = Box::new(make_dummy_ident_node(&token, prev_cond_binding_name));
                            TypedAstNode::Accessor(tok, TypedAccessorNode { target, is_opt_safe: false, ..node })
                        }
                        _ => unimplemented!()
                    }
                };

                let cond_binding_name = format!("${}", layer_number);
                if_node = Some(TypedAstNode::IfExpression(
                    Token::If(token.get_position()),
                    TypedIfNode {
                        typ: Type::Placeholder, // Type doesn't matter
                        condition: Box::new(condition),
                        condition_binding: Some(BindingPattern::Variable(Token::Ident(token.get_position(), cond_binding_name.clone()))),
                        if_block: vec![
                            match if_node {
                                None => make_dummy_ident_node(&token, cond_binding_name),
                                Some(if_node) => if_node
                            }
                        ],
                        else_block: Some(vec![make_dummy_ident_node(&token, "None".to_string())]),
                    },
                ));
                layer_number -= 1;
            }
            let if_node = if_node.expect("It should be Some after the unwinding is through");

            self.visit(if_node)?;

            return Ok(());
        }

        let TypedAccessorNode { target, field_ident, field_idx, is_method, .. } = node;
        let field_name = Token::get_ident_name(&field_ident);

        if let Type::Module(module_id) = target.get_type() {
            let global_idx = self.get_global_idx(&module_id, &field_name);
            let global_idx = *global_idx.expect(&format!("There should be a designated global slot for name {}", field_name));
            self.write_opcode(Opcode::GLoad(global_idx), line);

            return Ok(());
        }

        self.visit(*target)?;
        if is_method {
            self.write_opcode(Opcode::GetMethod(field_idx), line);
        } else {
            self.write_opcode(Opcode::GetField(field_idx), line);
        }
        self.metadata.field_gets.push(field_name);

        Ok(())
    }

    fn visit_for_loop(&mut self, token: Token, node: TypedForLoopNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedForLoopNode { binding, index_ident, iterator, body } = node;
        let iterator_type = iterator.get_type();

        // Push intrinsic variable $idx, to track position in $iter
        self.push_scope(ScopeKind::ForLoop); // Create wrapper scope to hold invisible variables
        self.write_opcode(Opcode::IConst0, line); // Local 0 is iterator index ($idx)
        self.push_local("$idx", line, true);

        // $iter = <iterator>.enumerate()
        self.visit(*iterator)?;
        let enumerate_method_idx = match iterator_type {
            Type::Array(_) => NativeArray::get_struct_type().get_method_idx("enumerate").expect("Array is missing required enumerate method"),
            Type::Set(_) => NativeSet::get_struct_type().get_method_idx("enumerate").expect("Set is missing required enumerate method"),
            Type::Map(_, _) => NativeMap::get_struct_type().get_method_idx("enumerate").expect("Map is missing required enumerate method"),
            _ => unreachable!("Should have been caught during typechecking")
        };
        self.write_opcode(Opcode::GetMethod(enumerate_method_idx), line);
        self.metadata.field_gets.push("enumerate".to_string());
        self.write_opcode(Opcode::Invoke(0), line);
        self.push_local("$iter", line, true); // Local 1 is the iterator

        #[inline]
        fn load_intrinsic<'a>(compiler: &mut Compiler<'a>, name: &str, line: usize) {
            let (_, slot) = compiler.resolve_local(&name.to_string(), compiler.get_fn_depth()).unwrap();
            compiler.write_load_local_instr(name, slot, line);
        }

        #[inline]
        fn store_intrinsic<'a>(compiler: &mut Compiler<'a>, name: &str, line: usize) {
            let (_, slot) = compiler.resolve_local(&name.to_string(), compiler.get_fn_depth()).unwrap();
            compiler.write_store_local_instr(name, slot, line);
        }

        // Place marker to point the JumpB to later, to jump to start of loop. Preserve any prior loop_start_handle
        let mut old_loop_start_handle = Some(self.begin_jump_back());
        std::mem::swap(&mut self.loop_start_handle, &mut old_loop_start_handle);

        // Essentially: if $idx >= $iter.length { break }
        load_intrinsic(self, "$idx", line);
        load_intrinsic(self, "$iter", line);
        let length_idx = NativeArray::get_struct_type().get_field_idx("length").expect("Array is missing required length field");
        self.write_opcode(Opcode::GetField(length_idx), line);
        self.metadata.field_gets.push("length".to_string());
        self.write_opcode(Opcode::LT, line);
        let loop_end_jump_handle = self.begin_jump(Opcode::JumpIfF(0), line);

        // Insert iteratee and index bindings (if indexer expected) into loop scope
        //   $iter[$idx] is a tuple, of (<iteratee>, <index>)
        //   So, iteratee = $iter[$idx][0]
        // This value will then be destructured, if a pattern is used
        self.push_scope(ScopeKind::Block);
        load_intrinsic(self, "$iter", line);
        load_intrinsic(self, "$idx", line);
        self.write_opcode(Opcode::ArrLoad, line);
        self.write_opcode(Opcode::IConst0, line);
        self.write_opcode(Opcode::TupleLoad, line);
        self.visit_pattern(binding);
        if let Some(ident) = index_ident {
            // If present, index = $iter[$idx][1]
            load_intrinsic(self, "$iter", line);
            load_intrinsic(self, "$idx", line);
            self.write_opcode(Opcode::ArrLoad, line);
            self.write_opcode(Opcode::IConst1, line);
            self.write_opcode(Opcode::TupleLoad, line);
            self.push_local(Token::get_ident_name(&ident), line, true);
        }
        load_intrinsic(self, "$idx", line);
        self.write_opcode(Opcode::IConst1, line);
        self.write_opcode(Opcode::IAdd, line);
        store_intrinsic(self, "$idx", line);

        let mut old_interrupt_handles = vec![];
        std::mem::swap(&mut self.interrupt_handles, &mut old_interrupt_handles);

        let last_line = self.visit_loop_body(body, loop_end_jump_handle)?;
        // Restore any prior loop_start_handle
        std::mem::swap(&mut self.loop_start_handle, &mut old_loop_start_handle);

        self.pop_scope();
        self.write_opcode(Opcode::Pop(2), last_line); // Pop $iter and $idx
        self.locals.pop(); // <
        self.locals.pop(); // < Remove $iter and $idx from compiler's locals vector

        // Fill in any break-jump slots that have been accrued during compilation of this loop
        // Note: for nested loops, break-jumps will break out of inner loop only
        std::mem::swap(&mut self.interrupt_handles, &mut old_interrupt_handles);
        let mut interrupt_handles = old_interrupt_handles;
        for handle in interrupt_handles.drain(..) {
            self.close_jump(handle);
        }

        self.pop_scope();
        Ok(())
    }

    fn visit_while_loop(&mut self, token: Token, node: TypedWhileLoopNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedWhileLoopNode { condition, condition_binding, body } = node;

        // Place marker to point the JumpB to later, to jump to start of loop. Preserve any prior loop_start_handle
        let mut old_loop_start_handle = Some(self.begin_jump_back());
        std::mem::swap(&mut self.loop_start_handle, &mut old_loop_start_handle);

        self.push_scope(ScopeKind::WhileLoop);
        let is_opt = match condition.get_type() {
            Type::Option(inner) => *inner != Type::Bool,
            _ => false
        };
        if let Some(ident) = &condition_binding {
            // If there is a condition binding, create a new local with initial value of Nil
            self.write_opcode(Opcode::Nil, line);
            self.push_local(Token::get_ident_name(ident), line, true);
        }

        self.visit(*condition)?;
        if let Some(ident) = &condition_binding {
            // If there is a condition binding, duplicate the condition value and store into the reserved local.
            self.write_opcode(Opcode::Dup, line);

            let ident = Token::get_ident_name(ident);
            let (_, slot) = self.resolve_local(&ident, self.get_fn_depth()).unwrap();
            self.write_store_local_instr(ident, slot, line); // Store into condition binding
        }
        if is_opt {
            self.write_opcode(Opcode::Nil, line);
            self.write_opcode(Opcode::Neq, line);
        }

        let loop_end_jump_handle = self.begin_jump(Opcode::JumpIfF(0), line);

        let mut old_interrupt_handles = vec![];
        std::mem::swap(&mut self.interrupt_handles, &mut old_interrupt_handles);
        self.visit_loop_body(body, loop_end_jump_handle)?;
        // Restore any prior loop_start_handle
        std::mem::swap(&mut self.loop_start_handle, &mut old_loop_start_handle);

        if let Some(_) = condition_binding {
            // If there was a condition binding, we need to pop it off the stack
            self.write_opcode(Opcode::Pop(1), line);
        }

        // Fill in any break-jump slots that have been accrued during compilation of this loop
        // Note: for nested loops, break-jumps will break out of inner loop only
        std::mem::swap(&mut self.interrupt_handles, &mut old_interrupt_handles);
        let mut interrupt_handles = old_interrupt_handles;
        for handle in interrupt_handles.drain(..) {
            self.close_jump(handle);
        }

        self.pop_scope();

        Ok(())
    }

    fn visit_break(&mut self, token: Token) -> Result<(), ()> {
        let line = token.get_position().line;

        // Emit bytecode to pop locals from stack. The scope in which the break statement lives
        // takes care of making sure the compiler's `locals` vec is in the correct state; here we
        // just need to emit the runtime popping. It's worth noting that locals in scopes deeper than
        // the loop also need to be popped
        let mut scopes_iter = self.scopes.iter().rev();
        let mut split_idx = self.locals.len() as i64;
        loop {
            let Scope { num_locals, kind, .. } = scopes_iter.next().unwrap();
            split_idx -= *num_locals as i64;
            if kind == &ScopeKind::ForLoop || kind == &ScopeKind::WhileLoop {
                break;
            }
        }
        let mut locals_to_pop = self.locals.split_off(split_idx as usize);
        self.write_pops(&locals_to_pop, line);
        self.locals.append(&mut locals_to_pop);

        let interrupt_jump_handle = self.begin_jump(Opcode::Jump(0), line);
        self.interrupt_handles.push(interrupt_jump_handle);
        Ok(())
    }

    fn visit_continue(&mut self, token: Token) -> Result<(), ()> {
        let line = token.get_position().line;

        // Emit bytecode to pop locals from stack. The scope in which the continue statement lives
        // takes care of making sure the compiler's `locals` vec is in the correct state; here we
        // just need to emit the runtime popping. It's worth noting that locals in scopes deeper than
        // the loop also need to be popped, depending on the kind of loop we're in
        let mut scopes_iter = self.scopes.iter().rev();
        let mut split_idx = self.locals.len() as i64;
        loop {
            let Scope { num_locals, kind, .. } = scopes_iter.next().unwrap();

            // If the continue occurred within a for-loop, we do _not_ want to pop off the $iter and $idx bindings
            if kind == &ScopeKind::ForLoop { break; }
            split_idx -= *num_locals as i64;
            if kind == &ScopeKind::WhileLoop { break; }
        }
        let mut locals_to_pop = self.locals.split_off(split_idx as usize);
        self.write_pops(&locals_to_pop, line);
        self.locals.append(&mut locals_to_pop);

        let loop_start_handle = self.loop_start_handle.as_ref().expect("There must be a loop_start_handle if we're within a loop body").clone();
        self.close_jump_back(loop_start_handle, line);
        Ok(())
    }

    fn visit_return(&mut self, token: Token, node: TypedReturnNode) -> Result<(), ()> {
        let line = token.get_position().line;
        if let Some(target) = node.target {
            self.visit(*target)?;
        } else {
            self.write_opcode(Opcode::Nil, line);
        }

        let mut scopes_iter = self.scopes.iter().rev();
        let mut split_idx = self.locals.len() as i64;
        loop {
            let Scope { num_locals, kind, .. } = scopes_iter.next().unwrap();
            split_idx -= *num_locals as i64;
            if kind == &ScopeKind::Func {
                break;
            }
        }

        let mut locals_to_pop = self.locals.split_off(split_idx as usize);
        let mut opt_first_local = None;
        // Store the return value into slot 0 (the return slot). Then, we need to make sure we don't
        // emit a pop instruction for this local, so we temporarily remove it from the locals_to_pop
        // vec, then re-push it afterwards.
        self.write_store_local_instr("<ret>", 0, line);
        if locals_to_pop.len() != 0 {
            opt_first_local = Some(locals_to_pop.remove(0));
        }

        // Emit the pop instructions for the locals in scope, but don't permanently remove them from self.locals;
        // this will be handled in the function compilation step. As such, if we are in the second case above (in which
        // there is no first local to use as the return slot), we need to make sure that we reinsert that first local
        // back into self.locals, before the rest of `locals_to_pop`.
        self.write_pops_for_closure(&locals_to_pop, line);
        if let Some(first_local) = opt_first_local {
            self.locals.push(first_local);
        }
        self.locals.append(&mut locals_to_pop);

        let return_jump_handle = self.begin_jump(Opcode::Jump(0), line);
        self.return_handles.push(return_jump_handle);
        Ok(())
    }

    fn visit_import_statement(&mut self, token: Token, node: TypedImportNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedImportNode { imports, module_id, alias_name } = node;

        if let Some(alias_name) = alias_name {
            self.add_and_write_constant(Value::Module(module_id), line);
            self.write_store_global_instr(alias_name, line);

            return Ok(());
        }

        for import_name in imports {
            let global_idx = self.get_global_idx(&module_id, &import_name);
            let global_idx = *global_idx.expect(&format!("There should be a designated global slot for name {}", import_name));
            self.write_opcode(Opcode::GLoad(global_idx), line);

            self.write_store_global_instr(import_name, line);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtins::prelude::{PRELUDE_PRINTLN_INDEX, PRELUDE_STRING_INDEX, PRELUDE_RANGE_INDEX, NUM_PRELUDE_BINDINGS};
    use crate::common::test_utils::MockModuleReader;
    use crate::parser::ast::ModuleId;
    use itertools::Itertools;

    fn new_string_obj(string: &str) -> Value {
        Value::new_string_obj(string.to_string())
    }

    fn test_compile(input: &str) -> Module {
        let mock_reader = MockModuleReader::default();
        let module_id = ModuleId::from_name("_test");
        let modules = crate::compile(module_id, &input.to_string(), &mock_reader).unwrap();
        let mut modules = modules.into_iter();
        assert_eq!(modules.next().unwrap().name, "prelude".to_string());

        modules.next().unwrap()
    }

    fn test_compile_with_modules(input: &str, modules: Vec<(&str, &str)>) -> Module {
        let mock_reader = MockModuleReader::new(modules);
        let module_id = ModuleId::from_name("_test");
        let modules = crate::compile(module_id, &input.to_string(), &mock_reader).unwrap();
        let mut modules = modules.into_iter();
        assert_eq!(modules.next().unwrap().name, "prelude".to_string());

        modules.last().unwrap()
    }

    fn to_string_method() -> (String, Value) {
        ("toString".to_string(), Value::NativeFn(NativeFn {
            name: "toString",
            receiver: None,
            native_fn: default_to_string_method,
        }))
    }

    fn global_idx(idx: usize) -> usize {
        NUM_PRELUDE_BINDINGS + idx
    }

    #[test]
    fn compile_empty() {
        let chunk = test_compile("");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![Opcode::Return],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_literals() {
        let chunk = test_compile("1 2.3 4 5.6 \"hello\" true false");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::Pop(1),
                Opcode::Constant(1, 0),
                Opcode::Pop(1),
                Opcode::IConst4,
                Opcode::Pop(1),
                Opcode::Constant(1, 1),
                Opcode::Pop(1),
                Opcode::Constant(1, 2),
                Opcode::Pop(1),
                Opcode::T,
                Opcode::Pop(1),
                Opcode::F,
                Opcode::Return,
            ],
            constants: vec![
                Value::Float(2.3),
                Value::Float(5.6),
                new_string_obj("hello"),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_unary() {
        let chunk = test_compile("-5");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Invert,
                Opcode::Return,
            ],
            constants: vec![Value::Int(5)],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("-2.3");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Invert,
                Opcode::Return,
            ],
            constants: vec![Value::Float(2.3)],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("!false");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::F,
                Opcode::Negate,
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_numeric() {
        let chunk = test_compile("5 + 6");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Return,
            ],
            constants: vec![Value::Int(5), Value::Int(6)],
        };
        assert_eq!(expected, chunk);

        // Testing i2f and order of ops
        let chunk = test_compile("1 - -5 * 3.4 / 5");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::I2F,
                Opcode::Constant(1, 0),
                Opcode::Invert,
                Opcode::I2F,
                Opcode::Constant(1, 1),
                Opcode::FMul,
                Opcode::Constant(1, 0),
                Opcode::I2F,
                Opcode::FDiv,
                Opcode::FSub,
                Opcode::Return,
            ],
            constants: vec![Value::Int(5), Value::Float(3.4)],
        };
        assert_eq!(expected, chunk);

        // Testing %, along with i2f
        let chunk = test_compile("3.4 % 2.4 % 5");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Constant(1, 1),
                Opcode::FMod,
                Opcode::Constant(1, 2),
                Opcode::I2F,
                Opcode::FMod,
                Opcode::Return,
            ],
            constants: vec![Value::Float(3.4), Value::Float(2.4), Value::Int(5)],
        };
        assert_eq!(expected, chunk);

        // Testing **
        let chunk = test_compile("3.4 ** 5");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Constant(1, 1),
                Opcode::Pow,
                Opcode::Return,
            ],
            constants: vec![Value::Float(3.4), Value::Int(5)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_grouped() {
        let chunk = test_compile("(1 + 2) * 3");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IMul,
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_str_concat() {
        let chunk = test_compile("\"abc\" + \"def\"");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Constant(1, 1),
                Opcode::StrConcat,
                Opcode::Return,
            ],
            constants: vec![new_string_obj("abc"), new_string_obj("def")],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("1 + \"a\" + 3.4");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::Constant(1, 0),
                Opcode::StrConcat,
                Opcode::Constant(1, 1),
                Opcode::StrConcat,
                Opcode::Return,
            ],
            constants: vec![new_string_obj("a"), Value::Float(3.4)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_boolean() {
        let chunk = test_compile("true && true || false");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::T,
                Opcode::JumpIfF(2),
                Opcode::T,
                Opcode::Jump(1),
                Opcode::F,
                Opcode::JumpIfF(2),
                Opcode::T,
                Opcode::Jump(1),
                Opcode::F,
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        // Testing xor
        let chunk = test_compile("true ^ false");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::T,
                Opcode::F,
                Opcode::Xor,
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_comparisons() {
        let chunk = test_compile("1 <= 5 == 3.4 >= 5.6");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::Constant(1, 0),
                Opcode::LTE,
                Opcode::Constant(1, 1),
                Opcode::Constant(1, 2),
                Opcode::GTE,
                Opcode::Eq,
                Opcode::Return,
            ],
            constants: vec![Value::Int(5), Value::Float(3.4), Value::Float(5.6)],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\"a\" < \"b\" != 4");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Constant(1, 1),
                Opcode::LT,
                Opcode::IConst4,
                Opcode::Neq,
                Opcode::Return,
            ],
            constants: vec![new_string_obj("a"), new_string_obj("b")],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_coalesce() {
        let chunk = test_compile("[\"a\", \"b\"][2] ?: \"c\"");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Constant(1, 1),
                Opcode::ArrMk(2),
                Opcode::IConst2,
                Opcode::ArrLoad,
                Opcode::Dup,
                Opcode::Nil,
                Opcode::Neq,
                Opcode::JumpIfF(4),
                Opcode::MarkLocal(0),
                Opcode::LLoad(0),
                Opcode::LStore(0),
                Opcode::Jump(2),
                Opcode::Pop(1),
                Opcode::Constant(1, 2),
                Opcode::Return,
            ],
            constants: vec![new_string_obj("a"), new_string_obj("b"), new_string_obj("c")],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_array_literal() {
        let chunk = test_compile("[1, 2]");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::ArrMk(2),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("[\"a\", \"b\", \"c\"]");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Constant(1, 1),
                Opcode::Constant(1, 2),
                Opcode::ArrMk(3),
                Opcode::Return,
            ],
            constants: vec![new_string_obj("a"), new_string_obj("b"), new_string_obj("c")],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_array_nested() {
        let chunk = test_compile("[[1, 2], [3, 4, 5]]");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::ArrMk(2),
                Opcode::IConst3,
                Opcode::IConst4,
                Opcode::Constant(1, 0),
                Opcode::ArrMk(3),
                Opcode::ArrMk(2),
                Opcode::Return,
            ],
            constants: vec![Value::Int(5)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_set_literal() {
        let chunk = test_compile("#{1, 2}");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::SetMk(2),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("#{\"a\", \"b\", \"c\"}");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Constant(1, 1),
                Opcode::Constant(1, 2),
                Opcode::SetMk(3),
                Opcode::Return,
            ],
            constants: vec![new_string_obj("a"), new_string_obj("b"), new_string_obj("c")],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_map_literal() {
        let chunk = test_compile("{ a: 1, b: \"c\", d: true }");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::IConst1,
                Opcode::Constant(1, 1),
                Opcode::Constant(1, 2),
                Opcode::Constant(1, 3),
                Opcode::T,
                Opcode::MapMk(3),
                Opcode::Return,
            ],
            constants: vec![
                new_string_obj("a"),
                new_string_obj("b"),
                new_string_obj("c"),
                new_string_obj("d"),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binding_decl() {
        let chunk = test_compile("val abc = 123");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![Value::Int(123)],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("var unset: Bool\nvar set = true");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::Nil,
                Opcode::GStore(global_idx(0)),
                Opcode::T,
                Opcode::GStore(global_idx(1)),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("val abc = \"a\" + \"b\"\nval def = 5");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Constant(1, 1),
                Opcode::StrConcat,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 2),
                Opcode::GStore(global_idx(1)),
                Opcode::Return,
            ],
            constants: vec![
                new_string_obj("a"),
                new_string_obj("b"),
                Value::Int(5),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binding_decl_struct_type() {
        let chunk = test_compile("\
          type Person { name: String }\n\
          val meg = Person(name: \"Meg\")\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 1),
                Opcode::New(global_idx(0), 1),
                Opcode::GStore(global_idx(1)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Type(TypeValue {
                    name: "Person".to_string(),
                    module_name: "_test".to_string(),
                    fields: vec!["name".to_string()],
                    constructor: None,
                    methods: vec![to_string_method()],
                    static_fields: vec![],
                }),
                new_string_obj("Meg"),
            ],
        };
        assert_eq!(expected, chunk);

        // Test assignment with default field values
        let chunk = test_compile("\
          type Person { name: String, age: Int = 0 }\n\
          val someBaby = Person(name: \"Unnamed\")\n\
          val anAdult = Person(name: \"Some Name\", age: 29)\n\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 3,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::IConst0,
                Opcode::Constant(1, 1),
                Opcode::New(global_idx(0), 2),
                Opcode::GStore(global_idx(1)),
                Opcode::Constant(1, 2),
                Opcode::Constant(1, 3),
                Opcode::New(global_idx(0), 2),
                Opcode::GStore(global_idx(2)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Type(TypeValue {
                    name: "Person".to_string(),
                    module_name: "_test".to_string(),
                    fields: vec!["name".to_string(), "age".to_string()],
                    constructor: None,
                    methods: vec![to_string_method()],
                    static_fields: vec![],
                }),
                new_string_obj("Unnamed"),
                Value::Int(29),
                new_string_obj("Some Name"),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binding_decl_destructuring_tuples() {
        let chunk = test_compile("val (a, b) = (1, 2)");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 3,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::TupleMk(2),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst0,
                Opcode::TupleLoad,
                Opcode::GStore(global_idx(1)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst1,
                Opcode::TupleLoad,
                Opcode::GStore(global_idx(2)),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\
          func abc() {\n\
            val (a, b) = (1, 2)\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Fn(FnValue {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::IConst1,
                        Opcode::IConst2,
                        Opcode::TupleMk(2),
                        Opcode::MarkLocal(0),
                        Opcode::LLoad(0),
                        Opcode::IConst0,
                        Opcode::TupleLoad,
                        Opcode::MarkLocal(1),
                        Opcode::LLoad(0),
                        Opcode::IConst1,
                        Opcode::TupleLoad,
                        Opcode::MarkLocal(2),
                        Opcode::Nil,
                        Opcode::LStore(0),
                        Opcode::Pop(1),
                        Opcode::Pop(1),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binding_decl_destructuring_arrays() {
        let chunk = test_compile("val [a, b] = [1, 2]");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 3, // Account for $temp_0
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::ArrMk(2),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst0,
                Opcode::ArrLoad,
                Opcode::GStore(global_idx(1)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst1,
                Opcode::ArrLoad,
                Opcode::GStore(global_idx(2)),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\
          func abc() {\n\
            val [a, b] = [1, 2]\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Fn(FnValue {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::IConst1,
                        Opcode::IConst2,
                        Opcode::ArrMk(2),
                        Opcode::MarkLocal(0),
                        Opcode::LLoad(0),
                        Opcode::IConst0,
                        Opcode::ArrLoad,
                        Opcode::MarkLocal(1),
                        Opcode::LLoad(0),
                        Opcode::IConst1,
                        Opcode::ArrLoad,
                        Opcode::MarkLocal(2),
                        Opcode::Nil,
                        Opcode::LStore(0),
                        Opcode::Pop(1),
                        Opcode::Pop(1),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binding_decl_destructuring_strings() {
        let chunk = test_compile("val [a, b] = \"hello\"");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 3,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst0,
                Opcode::ArrLoad,
                Opcode::GStore(global_idx(1)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst1,
                Opcode::ArrLoad,
                Opcode::GStore(global_idx(2)),
                Opcode::Return,
            ],
            constants: vec![new_string_obj("hello")],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_ident() {
        let chunk = test_compile("val abc = 123\nabc");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![Value::Int(123)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_ident_upvalues() {
        let chunk = test_compile("func a(i: Int) {\nval b = 3\nfunc c(): Int { b + 1 }\n}");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 1),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Fn(FnValue {
                    name: "c".to_string(),
                    code: vec![
                        Opcode::ULoad(0),
                        Opcode::IConst1,
                        Opcode::IAdd,
                        Opcode::Return,
                    ],
                    upvalues: vec![
                        Upvalue {
                            capture_kind: UpvalueCaptureKind::Local { local_idx: 2 },
                            depth: 1,
                        }
                    ],
                    receiver: None,
                }),
                Value::Fn(FnValue {
                    name: "a".to_string(),
                    code: vec![
                        Opcode::IConst0,
                        Opcode::MarkLocal(1),
                        Opcode::IConst3,
                        Opcode::MarkLocal(2),
                        Opcode::Constant(1, 0),
                        Opcode::ClosureMk,
                        Opcode::MarkLocal(3),
                        Opcode::LLoad(3),
                        Opcode::LStore(1),
                        Opcode::Nil,
                        Opcode::LStore(0),
                        Opcode::Pop(1),
                        Opcode::CloseUpvalueAndPop,
                        Opcode::Pop(1),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_ident_upvalues_skip_level() {
        let chunk = test_compile("func a(i: Int) {\nval b = 3\nfunc c() { func d(): Int { b + 1 }\n}\n}");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 2),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Fn(FnValue {
                    name: "d".to_string(),
                    code: vec![
                        Opcode::ULoad(0),
                        Opcode::IConst1,
                        Opcode::IAdd,
                        Opcode::Return,
                    ],
                    upvalues: vec![
                        Upvalue {
                            capture_kind: UpvalueCaptureKind::Upvalue { upvalue_idx: 0 },
                            depth: 2,
                        }
                    ],
                    receiver: None,
                }),
                Value::Fn(FnValue {
                    name: "c".to_string(),
                    code: vec![
                        Opcode::IConst0,
                        Opcode::MarkLocal(0),
                        Opcode::Constant(1, 0),
                        Opcode::ClosureMk,
                        Opcode::MarkLocal(1),
                        Opcode::LLoad(1),
                        Opcode::LStore(0),
                        Opcode::Nil,
                        Opcode::LStore(0),
                        Opcode::Pop(1),
                        Opcode::Return,
                    ],
                    upvalues: vec![
                        Upvalue {
                            capture_kind: UpvalueCaptureKind::Local { local_idx: 2 },
                            depth: 1,
                        }
                    ],
                    receiver: None,
                }),
                Value::Fn(FnValue {
                    name: "a".to_string(),
                    code: vec![
                        Opcode::IConst0,
                        Opcode::MarkLocal(1),
                        Opcode::IConst3,
                        Opcode::MarkLocal(2),
                        Opcode::Constant(1, 1),
                        Opcode::ClosureMk,
                        Opcode::MarkLocal(3),
                        Opcode::LLoad(3),
                        Opcode::LStore(1),
                        Opcode::Nil,
                        Opcode::LStore(0),
                        Opcode::Pop(1),
                        Opcode::CloseUpvalueAndPop,
                        Opcode::Pop(1),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment() {
        let chunk = test_compile("var a = 1\nvar b = 2\nval c = b = a = 3");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 3,
            code: vec![
                // var a = 1
                Opcode::IConst1,
                Opcode::GStore(global_idx(0)),
                // var b = 2
                Opcode::IConst2,
                Opcode::GStore(global_idx(1)),

                // val c = b = a = 3
                //   a = 3
                Opcode::IConst3,
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                //  b = <a = 3>
                Opcode::GStore(global_idx(1)),
                Opcode::GLoad(global_idx(1)),
                //  c = <b = <a = 3>>
                Opcode::GStore(global_idx(2)),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("var a = 1\na = 2\nval b = 3");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                // var a = 1
                Opcode::IConst1,
                Opcode::GStore(global_idx(0)),
                // a = 2
                Opcode::IConst2,
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::Pop(1), // <- This test verifies that the intermediate 2 gets popped
                // val b = 3
                Opcode::IConst3,
                Opcode::GStore(global_idx(1)),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment_globals() {
        let chunk = test_compile("var a = 1\nfunc abc(): Int { a = 3 }");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::IConst1,
                Opcode::GStore(global_idx(1)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Fn(FnValue {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::IConst3,
                        Opcode::GStore(global_idx(1)),
                        Opcode::GLoad(global_idx(1)),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment_upvalues() {
        let chunk = test_compile("func outer() {\nvar a = 1\nfunc inner(): Int { a = 3 }\n}");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 1),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Fn(FnValue {
                    name: "inner".to_string(),
                    code: vec![
                        Opcode::IConst3,
                        Opcode::UStore(0),
                        Opcode::ULoad(0),
                        Opcode::Return,
                    ],
                    upvalues: vec![
                        Upvalue {
                            capture_kind: UpvalueCaptureKind::Local { local_idx: 1 },
                            depth: 1,
                        }
                    ],
                    receiver: None,
                }),
                Value::Fn(FnValue {
                    name: "outer".to_string(),
                    code: vec![
                        Opcode::IConst0,
                        Opcode::MarkLocal(0), // Stub for inner
                        Opcode::IConst1,
                        Opcode::MarkLocal(1), // var a = 1
                        Opcode::Constant(1, 0),
                        Opcode::ClosureMk,
                        Opcode::MarkLocal(2),
                        Opcode::LLoad(2),
                        Opcode::LStore(0), // Store real fn in stub slot
                        Opcode::Nil, // Store nil return value for unit-returning fn
                        Opcode::LStore(0),
                        Opcode::Pop(1),
                        Opcode::CloseUpvalueAndPop,
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment_indexing() {
        let chunk = test_compile("val a = [1]\na[0] = 0");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst1,
                Opcode::ArrMk(1),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst0,
                Opcode::IConst0,
                Opcode::ArrStore,
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("val a = {b:1}\na[\"b\"] = 0");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::IConst1,
                Opcode::MapMk(1),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::IConst0,
                Opcode::MapStore,
                Opcode::Return,
            ],
            constants: vec![new_string_obj("b")],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("val a = (1, 2)\na[0] = 0");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::TupleMk(2),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst0,
                Opcode::IConst0,
                Opcode::TupleStore,
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment_field_accessor() {
        let chunk = test_compile("\
          type Person { name: String }\n\
          val p = Person(name: \"Ken\")\n\
          p.name = \"Meg\"\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 1),
                Opcode::New(global_idx(0), 1),
                Opcode::GStore(global_idx(1)),
                Opcode::Constant(1, 2),
                Opcode::GLoad(global_idx(1)),
                Opcode::SetField(0),
                Opcode::Return,
            ],
            constants: vec![
                Value::Type(TypeValue {
                    name: "Person".to_string(),
                    module_name: "_test".to_string(),
                    fields: vec!["name".to_string()],
                    constructor: None,
                    methods: vec![to_string_method()],
                    static_fields: vec![],
                }),
                new_string_obj("Ken"),
                new_string_obj("Meg"),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_indexing() {
        let chunk = test_compile("[1, 2, 3, 4, 5][3 + 1]");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IConst3,
                Opcode::IConst4,
                Opcode::Constant(1, 0),
                Opcode::ArrMk(5),
                Opcode::IConst3,
                Opcode::IConst1,
                Opcode::IAdd,
                Opcode::ArrLoad,
                Opcode::Return,
            ],
            constants: vec![Value::Int(5)],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\"some string\"[1 + 1:]");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::IConst1,
                Opcode::IConst1,
                Opcode::IAdd,
                Opcode::Nil,
                Opcode::ArrSlc,
                Opcode::Return,
            ],
            constants: vec![new_string_obj("some string")],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\"some string\"[-1:4]");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::IConst1,
                Opcode::Invert,
                Opcode::IConst4,
                Opcode::ArrSlc,
                Opcode::Return,
            ],
            constants: vec![new_string_obj("some string")],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\"some string\"[:1 + 1]");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::IConst0,
                Opcode::IConst1,
                Opcode::IConst1,
                Opcode::IAdd,
                Opcode::ArrSlc,
                Opcode::Return,
            ],
            constants: vec![new_string_obj("some string")],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("{ a: 1, b: 2 }[\"a\"]");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::IConst1,
                Opcode::Constant(1, 1),
                Opcode::IConst2,
                Opcode::MapMk(2),
                Opcode::Constant(1, 0),
                Opcode::MapLoad,
                Opcode::Return,
            ],
            constants: vec![new_string_obj("a"), new_string_obj("b")],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("(1, true, 3)[2]");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::T,
                Opcode::IConst3,
                Opcode::TupleMk(3),
                Opcode::IConst2,
                Opcode::TupleLoad,
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_if_else_statements() {
        let chunk = test_compile("if (1 == 2) 123 else 456");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::Eq,
                Opcode::JumpIfF(3),
                Opcode::Constant(1, 0),
                Opcode::Pop(1),
                Opcode::Jump(2),
                Opcode::Constant(1, 1),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![Value::Int(123), Value::Int(456)],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("if (1 == 2) 123");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::Eq,
                Opcode::JumpIfF(2),
                Opcode::Constant(1, 0),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![Value::Int(123)],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("if (1 == 2) { } else { 456 }");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::Eq,
                Opcode::JumpIfF(1),
                Opcode::Jump(2),
                Opcode::Constant(1, 0),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![Value::Int(456)],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("if (1 == 2) 123 else if (3 < 4) 456 else 789");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::Eq,
                Opcode::JumpIfF(3),
                Opcode::Constant(1, 0),
                Opcode::Pop(1),
                Opcode::Jump(9),
                Opcode::IConst3,
                Opcode::IConst4,
                Opcode::LT,
                Opcode::JumpIfF(3),
                Opcode::Constant(1, 1),
                Opcode::Pop(1),
                Opcode::Jump(2),
                Opcode::Constant(1, 2),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![Value::Int(123), Value::Int(456), Value::Int(789)],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\
          val a = 123
          if (true) {\
            val a = 456\
            a + 1\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::T,
                Opcode::JumpIfF(7),
                Opcode::Constant(1, 1),
                Opcode::MarkLocal(0),
                Opcode::LLoad(0),
                Opcode::IConst1,
                Opcode::IAdd,
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![
                Value::Int(123),
                Value::Int(456),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_if_else_statements_option_condition() {
        let chunk = test_compile("if ([1, 2][0]) 123 else 456");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::ArrMk(2),
                Opcode::IConst0,
                Opcode::ArrLoad,
                Opcode::Nil,
                Opcode::Neq,
                Opcode::JumpIfF(3),
                Opcode::Constant(1, 0),
                Opcode::Pop(1),
                Opcode::Jump(2),
                Opcode::Constant(1, 1),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![Value::Int(123), Value::Int(456)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_if_else_statements_with_condition_binding() {
        let chunk = test_compile("if [1, 2][0] |item| item else 456");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::ArrMk(2),
                Opcode::IConst0,
                Opcode::ArrLoad,
                Opcode::Dup,
                Opcode::Nil,
                Opcode::Neq,
                Opcode::JumpIfF(5),
                Opcode::MarkLocal(0),
                Opcode::LLoad(0),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Jump(3),
                Opcode::Pop(1),
                Opcode::Constant(1, 0),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![Value::Int(456)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_declaration() {
        let chunk = test_compile(r#"
          val a = 1
          val b = 2
          val c = 3
          func abc(b: Int): Int {
            val a1 = a
            val c = b + a1
            c
          }
        "#);
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 4,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::IConst1,
                Opcode::GStore(global_idx(1)),
                Opcode::IConst2,
                Opcode::GStore(global_idx(2)),
                Opcode::IConst3,
                Opcode::GStore(global_idx(3)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Fn(FnValue {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::GLoad(global_idx(1)),
                        Opcode::MarkLocal(1),
                        Opcode::LLoad(0),
                        Opcode::LLoad(1),
                        Opcode::IAdd,
                        Opcode::MarkLocal(2),
                        Opcode::LLoad(2),
                        Opcode::LStore(0),
                        Opcode::Pop(1),
                        Opcode::Pop(1),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                })
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_declaration_returns_unit_type() {
        let chunk = test_compile("\
          func abc() {\n\
            val a = 1\n\
            println(\"hello\")\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 1),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                new_string_obj("hello"),
                Value::Fn(FnValue {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::IConst1,
                        Opcode::MarkLocal(0),
                        Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                        Opcode::Constant(1, 0),
                        Opcode::ArrMk(1),
                        Opcode::Invoke(1),
                        Opcode::Pop(1), // Pop off `a`
                        Opcode::Nil,
                        Opcode::LStore(0),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_declaration_default_args() {
        let chunk = test_compile("func add(a: Int, b = 2): Int = a + b\nadd(1)\nadd(1, 2)");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst1,
                Opcode::Nil,
                Opcode::Invoke(2),
                Opcode::Pop(1),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::Invoke(2),
                Opcode::Return,
            ],
            constants: vec![
                Value::Fn(FnValue {
                    name: "add".to_string(),
                    code: vec![
                        Opcode::Nil,
                        Opcode::LLoad(1),
                        Opcode::Eq,
                        Opcode::JumpIfF(4),
                        Opcode::IConst2,
                        Opcode::LStore(1),
                        Opcode::LLoad(1),
                        Opcode::Pop(1),
                        Opcode::LLoad(0),
                        Opcode::LLoad(1),
                        Opcode::IAdd,
                        Opcode::LStore(0),
                        Opcode::Pop(1),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_declaration_with_inner() {
        let chunk = test_compile(r#"
          func abc(b: Int): Int {
            func def(g: Int): Int { g + 1 }
            val c = b + def(b)
            c
          }
        "#);
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 1),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Fn(FnValue {
                    name: "def".to_string(),
                    code: vec![
                        Opcode::LLoad(0),
                        Opcode::IConst1,
                        Opcode::IAdd,
                        Opcode::LStore(0),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
                Value::Fn(FnValue {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::IConst0,
                        Opcode::MarkLocal(1),
                        Opcode::Constant(1, 0),
                        Opcode::MarkLocal(2),
                        Opcode::LLoad(2),
                        Opcode::LStore(1),
                        Opcode::LLoad(0),
                        Opcode::LLoad(2),
                        Opcode::LLoad(0),
                        Opcode::Invoke(1),
                        Opcode::IAdd,
                        Opcode::MarkLocal(3),
                        Opcode::LLoad(3),
                        Opcode::LStore(0),
                        Opcode::Pop(1),
                        Opcode::Pop(1),
                        Opcode::Pop(1),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_type_decl_struct_type_methods() {
        let chunk = test_compile("\
          type Person {\n\
            name: String\n\
            func getName(self): String = self.name\n\
            func getName2(self): String = self.getName()\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Type(TypeValue {
                    name: "Person".to_string(),
                    module_name: "_test".to_string(),
                    fields: vec!["name".to_string()],
                    constructor: None,
                    methods: vec![
                        to_string_method(),
                        ("getName".to_string(), Value::Fn(FnValue {
                            name: "getName".to_string(),
                            code: vec![
                                Opcode::LLoad(0),
                                Opcode::GetField(0),
                                Opcode::LStore(0),
                                Opcode::Return,
                            ],
                            upvalues: vec![],
                            receiver: None,
                        })),
                        ("getName2".to_string(), Value::Fn(FnValue {
                            name: "getName2".to_string(),
                            code: vec![
                                Opcode::LLoad(0),
                                Opcode::GetMethod(1),
                                Opcode::Invoke(0),
                                Opcode::LStore(0),
                                Opcode::Return,
                            ],
                            upvalues: vec![],
                            receiver: None,
                        })),
                    ],
                    static_fields: vec![],
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_enum_decl_variants() {
        let chunk = test_compile("enum Status { On, Off }");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Enum(EnumValue {
                    name: "Status".to_string(),
                    module_name: "_test".to_string(),
                    methods: vec![to_string_method()],
                    static_fields: vec![],
                    variants: vec![
                        (
                            "On".to_string(),
                            Value::new_enum_instance_obj(EnumInstanceObj { type_id: global_idx(0), idx: 0, values: None })
                        ),
                        (
                            "Off".to_string(),
                            Value::new_enum_instance_obj(EnumInstanceObj { type_id: global_idx(0), idx: 1, values: None })
                        ),
                    ],
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_invocation() {
        let chunk = test_compile(r#"
          val one = 1
          func inc(number: Int): Int {
            number + 1
          }
          val two = inc(number: one)
        "#);
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 3,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::IConst1,
                Opcode::GStore(global_idx(1)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::GLoad(global_idx(1)),
                Opcode::Invoke(1),
                Opcode::GStore(global_idx(2)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Fn(FnValue {
                    name: "inc".to_string(),
                    code: vec![
                        Opcode::LLoad(0),
                        Opcode::IConst1,
                        Opcode::IAdd,
                        Opcode::LStore(0),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop() {
        let chunk = test_compile("\
          var i = 0\n\
          while i < 1 {\n\
            i = i + 1\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst1,
                Opcode::LT,
                Opcode::JumpIfF(7),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst1,
                Opcode::IAdd,
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::Pop(1),
                Opcode::JumpB(11),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("while ([1, 2][0]) { 123 }");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::ArrMk(2),
                Opcode::IConst0,
                Opcode::ArrLoad,
                Opcode::Nil,
                Opcode::Neq,
                Opcode::JumpIfF(3),
                Opcode::Constant(1, 0),
                Opcode::Pop(1),
                Opcode::JumpB(11),
                Opcode::Return,
            ],
            constants: vec![Value::Int(123)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop_with_condition_binding() {
        let chunk = test_compile("while ([1, 2][0]) |item| { item }");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Nil,
                Opcode::MarkLocal(0),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::ArrMk(2),
                Opcode::IConst0,
                Opcode::ArrLoad,
                Opcode::Dup,
                Opcode::LStore(0),
                Opcode::Nil,
                Opcode::Neq,
                Opcode::JumpIfF(4),
                Opcode::LLoad(0),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::JumpB(16),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop_with_local() {
        let chunk = test_compile("\
          var i = 0\n\
          while i < 1 {\n\
            val newI = i + 1\n\
            i = newI\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst1,
                Opcode::LT,
                Opcode::JumpIfF(10),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst1,
                Opcode::IAdd,
                Opcode::MarkLocal(0),
                Opcode::LLoad(0),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::JumpB(14),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop_with_break() {
        let chunk = test_compile("\
          while true {\n\
            val i = 1\n\
            break\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::T,
                Opcode::JumpIfF(5),
                Opcode::IConst1,
                Opcode::MarkLocal(0),
                Opcode::Pop(1),      // <
                Opcode::Jump(1),     // < These 3 instrs are generated by the break
                Opcode::JumpB(7),    // These 2 get falsely attributed to the break, because of #32
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\
          while true |t| {\n\
            val i = 1\n\
            break\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Nil,
                Opcode::MarkLocal(0),
                Opcode::T,
                Opcode::Dup,
                Opcode::LStore(0),
                Opcode::JumpIfF(5),
                Opcode::IConst1,
                Opcode::MarkLocal(1),
                Opcode::Pop(2),      // <
                Opcode::Jump(2),     // < These 3 instrs are generated by the break
                Opcode::JumpB(11),   // These 2 get falsely attributed to the break, because of #32
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\
          while true {\n\
            val i = 1\n\
            if i == 1 {\n\
              val a = 2\n\
              break\n\
            }\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::T,
                Opcode::JumpIfF(12),
                Opcode::IConst1,
                Opcode::MarkLocal(0),
                Opcode::LLoad(0),
                Opcode::IConst1,
                Opcode::Eq,
                Opcode::JumpIfF(4),
                Opcode::IConst2,
                Opcode::MarkLocal(1),
                Opcode::Pop(2),     // <
                Opcode::Jump(2),  // < These 3 instrs are generated by the break
                Opcode::Pop(1),         // This instr is where the if jumps to if false (we still need to clean up locals in the loop)
                Opcode::JumpB(14),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop_with_continue() {
        let chunk = test_compile("\
          while true |t| {\n\
            val i = 1\n\
            continue\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Nil,
                Opcode::MarkLocal(0),
                Opcode::T,
                Opcode::Dup,
                Opcode::LStore(0),
                Opcode::JumpIfF(7),
                Opcode::IConst1,
                Opcode::MarkLocal(1),
                Opcode::Pop(2),      // <
                Opcode::JumpB(10),   // < These 2 are emitted by the continue, pop i & t, and jump back to start of loop
                Opcode::Pop(1),      // <<
                Opcode::Pop(2),      // << These two are actually unreachable, but everything is still popped properly
                Opcode::JumpB(13),   // This is the end of the loop body
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_for_loop() {
        let chunk = test_compile("\
          val msg = \"Row: \"\n\
          val arr = [1, 2]\n\
          for a, i in arr {\n\
            println(msg + a + i)\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                // val msg = "Row: "
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),

                // val arr = [1, 2]
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::ArrMk(2),
                Opcode::GStore(global_idx(1)),

                // val $idx = 0
                // val $iter = arr.enumerate()
                // if $idx < $iter.length {
                Opcode::IConst0,
                Opcode::MarkLocal(0),
                Opcode::GLoad(global_idx(1)),
                Opcode::GetMethod(2), // .enumerate
                Opcode::Invoke(0),
                Opcode::MarkLocal(1),
                Opcode::LLoad(0),
                Opcode::LLoad(1),
                Opcode::GetField(0), // .length
                Opcode::LT,
                Opcode::JumpIfF(27),

                // a = $iter[$idx][0]
                Opcode::LLoad(1),
                Opcode::LLoad(0),
                Opcode::ArrLoad,
                Opcode::IConst0,
                Opcode::TupleLoad,
                Opcode::MarkLocal(2),

                // i = $iter[$idx][1]
                Opcode::LLoad(1),
                Opcode::LLoad(0),
                Opcode::ArrLoad,
                Opcode::IConst1,
                Opcode::TupleLoad,
                Opcode::MarkLocal(3),

                // $idx += 1
                Opcode::LLoad(0),
                Opcode::IConst1,
                Opcode::IAdd,
                Opcode::LStore(0),

                // println(msg + a + i)
                // <recur>
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::GLoad(global_idx(0)),
                Opcode::LLoad(2),
                Opcode::StrConcat,
                Opcode::LLoad(3),
                Opcode::StrConcat,
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(2),
                Opcode::JumpB(32),

                // Cleanup/end
                Opcode::Pop(2),
                Opcode::Return,
            ],
            constants: vec![new_string_obj("Row: ")],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_for_loop_with_continue() {
        let chunk = test_compile("\
          val msg = \"Row: \"\n\
          val arr = [1, 2]\n\
          for a, i in arr {\n\
            if true println(msg + a + i)\n\
            else { continue }\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                // val msg = "Row: "
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),

                // val arr = [1, 2]
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::ArrMk(2),
                Opcode::GStore(global_idx(1)),

                // val $idx = 0
                // val $iter = arr.enumerate()
                // if $idx < $iter.length {
                Opcode::IConst0,
                Opcode::MarkLocal(0),
                Opcode::GLoad(global_idx(1)),
                Opcode::GetMethod(2), // .enumerate
                Opcode::Invoke(0),
                Opcode::MarkLocal(1),
                Opcode::LLoad(0),
                Opcode::LLoad(1),
                Opcode::GetField(0), // .length
                Opcode::LT,
                Opcode::JumpIfF(33),

                // a = $iter[$idx][0]
                Opcode::LLoad(1),
                Opcode::LLoad(0),
                Opcode::ArrLoad,
                Opcode::IConst0,
                Opcode::TupleLoad,
                Opcode::MarkLocal(2),

                // i = $iter[$idx][1]
                Opcode::LLoad(1),
                Opcode::LLoad(0),
                Opcode::ArrLoad,
                Opcode::IConst1,
                Opcode::TupleLoad,
                Opcode::MarkLocal(3),

                // $idx += 1
                Opcode::LLoad(0),
                Opcode::IConst1,
                Opcode::IAdd,
                Opcode::LStore(0),

                // if true println(msg + a + i)
                Opcode::T,
                Opcode::JumpIfF(10),
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::GLoad(global_idx(0)),
                Opcode::LLoad(2),
                Opcode::StrConcat,
                Opcode::LLoad(3),
                Opcode::StrConcat,
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Jump(3),

                // else { continue } (jump back to loop start)
                Opcode::Pop(2),
                Opcode::JumpB(35),
                Opcode::Pop(1),

                // <recur> (from if-branch)
                Opcode::Pop(2),
                Opcode::JumpB(38),

                // Cleanup/end
                Opcode::Pop(2),
                Opcode::Return,
            ],
            constants: vec![new_string_obj("Row: ")],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_jump_bigger_than_u8() {
        let input = r#"
          for i in range(0, 1) {
            if true && true && true && true {
              val a = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
              val b = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
              val c = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
              val d = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
              val e = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
              val f = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
            } else {
              val a = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
              val b = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
              val c = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
              val d = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
              val e = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
              val f = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11
            }
          }
        "#;
        let chunk = test_compile(input);
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            constants: vec![
                Value::Int(5), Value::Int(6), Value::Int(7), Value::Int(8), Value::Int(9), Value::Int(10), Value::Int(11),
            ],
            code: vec![
                Opcode::IConst0,
                Opcode::MarkLocal(0),
                Opcode::GLoad(PRELUDE_RANGE_INDEX),
                Opcode::IConst0,
                Opcode::IConst1,
                Opcode::Nil,
                Opcode::Invoke(3),
                Opcode::GetMethod(2), // .enumerate
                Opcode::Invoke(0),
                Opcode::MarkLocal(1),
                Opcode::LLoad(0),
                Opcode::LLoad(1),
                Opcode::GetField(0), // .length
                Opcode::LT,
                Opcode::JumpIfF(293),
                Opcode::LLoad(1),
                Opcode::LLoad(0),
                Opcode::ArrLoad,
                Opcode::IConst0,
                Opcode::TupleLoad,
                Opcode::MarkLocal(2),
                Opcode::LLoad(0),
                Opcode::IConst1,
                Opcode::IAdd,
                Opcode::LStore(0),
                Opcode::T,
                Opcode::JumpIfF(2),
                Opcode::T,
                Opcode::Jump(1),
                Opcode::F,
                Opcode::JumpIfF(2),
                Opcode::T,
                Opcode::Jump(1),
                Opcode::F,
                Opcode::JumpIfF(2),
                Opcode::T,
                Opcode::Jump(1),
                Opcode::F,
                Opcode::JumpIfF(134),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(3),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(4),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(5),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(6),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(7),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(8),
                Opcode::Pop(6),
                Opcode::Jump(133),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(3),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(4),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(5),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(6),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(7),
                Opcode::IConst1,
                Opcode::IConst2,
                Opcode::IAdd,
                Opcode::IConst3,
                Opcode::IAdd,
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::Constant(1, 0),
                Opcode::IAdd,
                Opcode::Constant(1, 1),
                Opcode::IAdd,
                Opcode::Constant(1, 2),
                Opcode::IAdd,
                Opcode::Constant(1, 3),
                Opcode::IAdd,
                Opcode::Constant(1, 4),
                Opcode::IAdd,
                Opcode::Constant(1, 5),
                Opcode::IAdd,
                Opcode::Constant(1, 6),
                Opcode::IAdd,
                Opcode::MarkLocal(8),
                Opcode::Pop(6),
                Opcode::Pop(1),
                Opcode::JumpB(298),
                Opcode::Pop(2),
                Opcode::Return,
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_num_constants_bigger_than_u8() {
        // This test is a bit gnarly, but it's better than having hundreds of lines of bytecode in the test file
        let input = (0..150).into_iter()
            .map(|i| format!("val v{} = \"{}\"", i, i))
            .join("\n");
        let chunk = test_compile(input.as_str());
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 150,
            constants: (0..150).into_iter()
                .map(|i| Value::new_string_obj(format!("{}", i)))
                .collect(),
            code: vec![
                (0..150).into_iter()
                    .flat_map(|i| vec![
                        Opcode::Constant(1, i),
                        Opcode::GStore(global_idx(i)),
                    ])
                    .collect(),
                vec![Opcode::Return],
            ].concat(),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_accessor() {
        // Accessing fields of structs
        let chunk = test_compile("\
          type Person { name: String }\n\
          val ken = Person(name: \"Ken\")\n\
          ken.name\n\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 1),
                Opcode::New(global_idx(0), 1),
                Opcode::GStore(global_idx(1)),
                Opcode::GLoad(global_idx(1)),
                Opcode::GetField(0),
                Opcode::Return,
            ],
            constants: vec![
                Value::Type(TypeValue {
                    name: "Person".to_string(),
                    module_name: "_test".to_string(),
                    fields: vec!["name".to_string()],
                    constructor: None,
                    methods: vec![to_string_method()],
                    static_fields: vec![],
                }),
                new_string_obj("Ken"),
            ],
        };
        assert_eq!(expected, chunk);

        // Accessing fields of structs
        let chunk = test_compile("\"hello\".length");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 0,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::GetField(0),
                Opcode::Return,
            ],
            constants: vec![new_string_obj("hello")],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_lambda_declaration_returns_unit_type() {
        let chunk = test_compile("\
          val abc = () => println(\"hello\")\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::Constant(1, 1),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                new_string_obj("hello"),
                Value::Fn(FnValue {
                    name: "$anon_0".to_string(),
                    code: vec![
                        Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                        Opcode::Constant(1, 0),
                        Opcode::ArrMk(1),
                        Opcode::Invoke(1),
                        Opcode::Pop(1),
                        Opcode::Nil,
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_match_statement() {
        let chunk = test_compile("\
          val a: (String | Int)? = \"woo\"\n\
          match a {\n\
            None x => println(x)\n\
            _ x => println(x)\n\
          }
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::Dup,
                Opcode::Nil,
                Opcode::Eq,
                Opcode::JumpIfF(8),
                Opcode::MarkLocal(0), // x
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Jump(7),
                Opcode::MarkLocal(0), // x
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![new_string_obj("woo")],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\
          val a: (String | Int)? = \"woo\"\n\
          match a {\n\
            None => println(4)\n\
            _ x => println(x)\n\
          }
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::Dup,
                Opcode::Nil,
                Opcode::Eq,
                Opcode::JumpIfF(8),
                Opcode::MarkLocal(0),
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::IConst4,
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Jump(7),
                Opcode::MarkLocal(0), // x
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![new_string_obj("woo")],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\
          type Person { name: String }\n\
          val a: String | Person = \"woo\"\n\
          match a {\n\
            Person p => println(p)\n\
            String s => println(s)\n\
          }
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 1),
                Opcode::GStore(global_idx(1)),
                Opcode::GLoad(global_idx(1)),
                Opcode::Dup,
                Opcode::Typeof,
                Opcode::GLoad(global_idx(0)),
                Opcode::Eq,
                Opcode::JumpIfF(8),
                Opcode::MarkLocal(0), // p
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Jump(13),
                Opcode::Dup,
                Opcode::Typeof,
                Opcode::GLoad(PRELUDE_STRING_INDEX),
                Opcode::Eq,
                Opcode::JumpIfF(8),
                Opcode::MarkLocal(0), // s
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Jump(0),
                Opcode::Return,
            ],
            constants: vec![
                Value::Type(TypeValue {
                    name: "Person".to_string(),
                    module_name: "_test".to_string(),
                    fields: vec!["name".to_string()],
                    constructor: None,
                    methods: vec![to_string_method()],
                    static_fields: vec![],
                }),
                new_string_obj("woo"),
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\
          enum Direction { Left, Right }\n\
          val d: Direction = Direction.Left\n\
          match d {\n\
            Direction.Left => println(\"Left\")\n\
            _ x => println(x)\n\
          }
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::GetField(0),
                Opcode::GStore(global_idx(1)),
                Opcode::GLoad(global_idx(1)),
                Opcode::Dup,
                Opcode::IConst0,
                Opcode::Eq,
                Opcode::JumpIfF(8),
                Opcode::MarkLocal(0),
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::Constant(1, 1),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Jump(7),
                Opcode::MarkLocal(0), // x
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![
                Value::Enum(EnumValue {
                    name: "Direction".to_string(),
                    module_name: "_test".to_string(),
                    variants: vec![
                        (
                            "Left".to_string(),
                            Value::new_enum_instance_obj(EnumInstanceObj { type_id: global_idx(0), idx: 0, values: None })
                        ),
                        (
                            "Right".to_string(),
                            Value::new_enum_instance_obj(EnumInstanceObj { type_id: global_idx(0), idx: 1, values: None })
                        ),
                    ],
                    methods: vec![to_string_method()],
                    static_fields: vec![],
                }),
                new_string_obj("Left"),
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\
          enum Foo { Bar(baz: Int) }\n\
          val f: Foo = Foo.Bar(baz: 24)\n\
          match f {\n\
            Foo.Bar(baz) => println(baz)
          }
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::GetField(0),
                Opcode::Constant(1, 1),
                Opcode::Invoke(1),
                Opcode::GStore(global_idx(1)),
                Opcode::GLoad(global_idx(1)),
                Opcode::Dup,
                Opcode::IConst0,
                Opcode::Eq,
                Opcode::JumpIfF(11),
                Opcode::MarkLocal(0), // $match_target
                Opcode::LLoad(0),
                Opcode::GetField(0),
                Opcode::MarkLocal(1), // baz
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(1),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(2),
                Opcode::Jump(0),
                Opcode::Return,
            ],
            constants: vec![
                Value::Enum(EnumValue {
                    name: "Foo".to_string(),
                    module_name: "_test".to_string(),
                    variants: vec![
                        (
                            "Bar".to_string(),
                            Value::Fn(FnValue {
                                name: "Foo.Bar".to_string(),
                                code: vec![
                                    Opcode::IConst0,
                                    Opcode::LLoad(0),
                                    Opcode::New(global_idx(0), 1),
                                    Opcode::LStore(0),
                                    Opcode::Pop(0),
                                    Opcode::Return,
                                ],
                                upvalues: vec![],
                                receiver: None,
                            })
                        ),
                    ],
                    methods: vec![to_string_method()],
                    static_fields: vec![],
                }),
                Value::Int(24),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_match_statement_constants() {
        let chunk = test_compile("\
          val s = \"hello\"\n\
          match s {\n\
            \"asdf\" x => println(x)\n\
            \"hello\" x => println(x)\n\
            _ x => println(x)\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::Dup,
                Opcode::Constant(1, 1),
                Opcode::Eq,
                Opcode::JumpIfF(8),
                Opcode::MarkLocal(0), // x
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Jump(19),
                Opcode::Dup,
                Opcode::Constant(1, 0),
                Opcode::Eq,
                Opcode::JumpIfF(8),
                Opcode::MarkLocal(0), // x
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Jump(7),
                Opcode::MarkLocal(0), // x
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![
                new_string_obj("hello"),
                new_string_obj("asdf"),
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\
          val s = (\"hello\", 12)\n\
          match s {\n\
            (\"hello\", 12) x => println(x)\n\
            _ x => println(x)\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::Constant(1, 0),
                Opcode::Constant(1, 1),
                Opcode::TupleMk(2),
                Opcode::GStore(global_idx(0)),
                Opcode::GLoad(global_idx(0)),
                Opcode::Dup,
                Opcode::Constant(1, 0),
                Opcode::Constant(1, 1),
                Opcode::TupleMk(2),
                Opcode::Eq,
                Opcode::JumpIfF(8),
                Opcode::MarkLocal(0), // x
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Jump(7),
                Opcode::MarkLocal(0), // x
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![
                new_string_obj("hello"),
                Value::Int(12),
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = test_compile("\
          enum Foo { Bar(baz: String, qux: Int) }\n\
          val foo = Foo.Bar(baz: \"asdf\", qux: 24)\n\
          match foo {\n\
            Foo.Bar(\"asdf\", 12) => println(1)
            Foo.Bar(\"qwer\", 24) => println(2)
            _ x => println(x)\n\
          }\
        ");
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                // enum Foo { Bar(baz: String, qux: Int) }
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 0),
                Opcode::GStore(global_idx(0)),

                // val foo = Foo.Bar(baz: "asdf", qux: 24)
                Opcode::GLoad(global_idx(0)),
                Opcode::GetField(0),
                Opcode::Constant(1, 1),
                Opcode::Constant(1, 2),
                Opcode::Invoke(2),
                Opcode::GStore(global_idx(1)),

                // match foo {
                Opcode::GLoad(global_idx(1)),

                //   Foo.Bar("asdf", 12) => println(1)
                Opcode::Dup,
                Opcode::IConst0,
                Opcode::Eq,
                Opcode::JumpIfF(18),
                Opcode::Dup,
                Opcode::GetField(0),
                Opcode::Constant(1, 1),
                Opcode::Eq,
                Opcode::JumpIfF(13),
                Opcode::Dup,
                Opcode::GetField(1),
                Opcode::Constant(1, 3),
                Opcode::Eq,
                Opcode::JumpIfF(8),
                Opcode::MarkLocal(0), // $match_target
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::IConst1,
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Jump(29),

                //   Foo.Bar("qwer", 24) => println(2)
                Opcode::Dup,
                Opcode::IConst0,
                Opcode::Eq,
                Opcode::JumpIfF(18),
                Opcode::Dup,
                Opcode::GetField(0),
                Opcode::Constant(1, 4),
                Opcode::Eq,
                Opcode::JumpIfF(13),
                Opcode::Dup,
                Opcode::GetField(1),
                Opcode::Constant(1, 2),
                Opcode::Eq,
                Opcode::JumpIfF(8),
                Opcode::MarkLocal(0), // $match_target
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::IConst2,
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Jump(7),

                // _ x => println(x)
                Opcode::MarkLocal(0), // x
                Opcode::GLoad(PRELUDE_PRINTLN_INDEX),
                Opcode::LLoad(0),
                Opcode::ArrMk(1),
                Opcode::Invoke(1),
                Opcode::Pop(1),
                Opcode::Pop(1),
                Opcode::Return,
            ],
            constants: vec![
                Value::Enum(EnumValue {
                    name: "Foo".to_string(),
                    module_name: "_test".to_string(),
                    variants: vec![
                        (
                            "Bar".to_string(),
                            Value::Fn(FnValue {
                                name: "Foo.Bar".to_string(),
                                code: vec![
                                    Opcode::IConst0,
                                    Opcode::LLoad(1),
                                    Opcode::LLoad(0),
                                    Opcode::New(global_idx(0), 2),
                                    Opcode::LStore(0),
                                    Opcode::Pop(1),
                                    Opcode::Return,
                                ],
                                upvalues: vec![],
                                receiver: None,
                            })
                        ),
                    ],
                    methods: vec![to_string_method()],
                    static_fields: vec![],
                }),
                new_string_obj("asdf"),
                Value::Int(24),
                Value::Int(12),
                new_string_obj("qwer"),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_return_statement() {
        let chunk = test_compile(r#"
          func f(): Int {
            if true { return 24 }
            return 6
          }
        "#);
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 1,
            code: vec![
                Opcode::IConst0,
                Opcode::GStore(global_idx(0)),
                Opcode::Constant(1, 2),
                Opcode::GStore(global_idx(0)),
                Opcode::Return,
            ],
            constants: vec![
                Value::Int(24),
                Value::Int(6),
                Value::Fn(FnValue {
                    name: "f".to_string(),
                    code: vec![
                        Opcode::T,
                        Opcode::JumpIfF(4),
                        Opcode::Constant(1, 0),
                        Opcode::LStore(0),
                        Opcode::Jump(4),
                        Opcode::Pop(1),
                        Opcode::Constant(1, 1),
                        Opcode::LStore(0),
                        Opcode::Jump(0),
                        Opcode::Return,
                    ],
                    upvalues: vec![],
                    receiver: None,
                }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_imports() {
        // Importing types
        let mod1 = "\
          import Person from .person\n\
          val p = Person(name: \"Ken\")\
        ";
        let modules = vec![
            (".person", "export type Person { name: String }"),
        ];
        let chunk = test_compile_with_modules(mod1, modules);
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::GLoad(global_idx(0)),
                Opcode::GStore(global_idx(1)),
                Opcode::Constant(2, 0),
                Opcode::New(global_idx(1), 1),
                Opcode::GStore(global_idx(2)),
                Opcode::Return,
            ],
            constants: vec![new_string_obj("Ken")],
        };
        assert_eq!(expected, chunk);

        // Importing enums
        let mod1 = "\
          import Direction from .direction\n\
          val d = Direction.Up\
        ";
        let modules = vec![
            (".direction", "export enum Direction { Up, Down }"),
        ];
        let chunk = test_compile_with_modules(mod1, modules);
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::GLoad(global_idx(0)),
                Opcode::GStore(global_idx(1)),
                Opcode::GLoad(global_idx(1)),
                Opcode::GetField(0),
                Opcode::GStore(global_idx(2)),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        // Importing bindings
        let mod1 = "\
          import x from .constants\n\
          val y = x + 4\
        ";
        let modules = vec![
            (".constants", "export val x = 123"),
        ];
        let chunk = test_compile_with_modules(mod1, modules);
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::GLoad(global_idx(0)),
                Opcode::GStore(global_idx(1)),
                Opcode::GLoad(global_idx(1)),
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::GStore(global_idx(2)),
                Opcode::Return,
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_import_alias() {
        let mod1 = "\
          import .constants as constants\n\
          val y = constants.x + 4\
        ";
        let modules = vec![
            (".constants", "export val x = 123"),
        ];
        let chunk = test_compile_with_modules(mod1, modules);
        let expected = Module {
            name: "_test".to_string(),
            is_native: false,
            num_globals: 2,
            code: vec![
                Opcode::Constant(2, 0),
                Opcode::GStore(global_idx(1)),
                Opcode::GLoad(global_idx(0)),
                Opcode::IConst4,
                Opcode::IAdd,
                Opcode::GStore(global_idx(2)),
                Opcode::Return,
            ],
            constants: vec![Value::Module(ModuleId::from_name(".constants"))],
        };
        assert_eq!(expected, chunk);
    }
}
