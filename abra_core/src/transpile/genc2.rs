use itertools::Itertools;
use crate::lexer::tokens::Token;
use crate::parser::ast::{BinaryOp, BindingPattern, IndexingMode, UnaryOp};
use crate::typechecker::typechecker2::{AssignmentKind, Enum, EnumId, FuncId, Function, ModuleId, PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_MODULE_ID, PRELUDE_STRING_TYPE_ID, PRELUDE_UNIT_TYPE_ID, PrimitiveType, Project, ScopeId, Struct, StructId, Type, TypedLiteral, TypedModule, TypedNode, TypeId, TypeKind, Variable, VariableAlias};

fn function_name(func: &Function) -> String {
    let FuncId(ScopeId(ModuleId(m_id), s_id), f_id) = func.id;
    format!("_{}_{}_{}__{}", m_id, s_id, f_id, func.name)
}

fn closure_name(func: &Function) -> String {
    format!("{}_v_", function_name(func))
}

pub struct CCompiler2<W: std::io::Write> {
    output: W,
    temp_idx: usize,
}

impl<W: std::io::Write> CCompiler2<W> {
    pub fn new(output: W) -> Self {
        CCompiler2 { output, temp_idx: 0 }
    }

    fn next_temp_variable(&mut self) -> String {
        let temp_idx = self.temp_idx;
        self.temp_idx += 1;
        format!("_tmp__{}", temp_idx)
    }

    fn get_type_name_by_id(&self, project: &Project, type_id: &TypeId) -> String {
        let ty = project.get_type_by_id(type_id);
        match ty {
            Type::Primitive(PrimitiveType::Unit) => "AbraUnit".to_string(),
            Type::Primitive(PrimitiveType::Any) => "AbraAny".to_string(),
            Type::Primitive(PrimitiveType::Int) => "AbraInt".to_string(),
            Type::Primitive(PrimitiveType::Float) => "AbraFloat".to_string(),
            Type::Primitive(PrimitiveType::Bool) => "AbraBool".to_string(),
            Type::Primitive(PrimitiveType::String) => "AbraString".to_string(),
            Type::Generic(_, _) => todo!(),
            Type::GenericInstance(struct_id, generic_ids) => {
                if *struct_id == project.prelude_array_struct_id {
                    "AbraArray".to_string()
                } else if *struct_id == project.prelude_tuple_struct_id {
                    "AbraTuple".to_string()
                } else if generic_ids.is_empty() {
                    self.get_struct_enum_name(project, &TypeKind::Struct(*struct_id))
                } else {
                    todo!()
                }
            }
            Type::GenericEnumInstance(_, _, _) |
            Type::Function(_, _, _, _) => "AbraFn".to_string(),
            Type::Type(kind) => self.get_struct_enum_name(project, kind),
            Type::ModuleAlias => todo!(),
        }
    }

    fn get_struct_enum_name(&self, project: &Project, kind: &TypeKind) -> String {
        match kind {
            TypeKind::Struct(struct_id) => {
                let StructId(ModuleId(m_id), s_id) = struct_id;
                let struct_ = project.get_struct_by_id(struct_id);
                format!("_{}_0_{}__{}", m_id, s_id, struct_.name)
            }
            TypeKind::Enum(enum_id) => {
                let EnumId(ModuleId(m_id), e_id) = enum_id;
                let enum_ = project.get_enum_by_id(enum_id);
                format!("_{}_0_{}__{}", m_id, e_id, enum_.name)
            }
        }
    }

    fn get_module_entrypoint_name(&self, module: &TypedModule) -> String {
        format!("entrypoint__{}", module.id.0)
    }

    // --- EMITTER LOGIC START ---

    fn emit<S: AsRef<str>>(&mut self, str: S) {
        write!(self.output, "{}", str.as_ref()).expect("Output should be able to be written to")
    }

    fn emit_line<S: AsRef<str>>(&mut self, str: S) {
        writeln!(self.output, "{}", str.as_ref()).expect("Output should be able to be written to")
    }

    fn emit_newline(&mut self) {
        writeln!(self.output).expect("Output should be able to be written to");
    }

    fn emit_comment<S: AsRef<str>>(&mut self, comment: S) {
        writeln!(self.output, "// {}", comment.as_ref()).expect("Output should be able to be written to");
    }

    fn emit_fn_signature(&mut self, project: &Project, func_id: &FuncId) {
        let function = project.get_func_by_id(func_id);
        let ret_type = self.get_type_name_by_id(project, &function.return_type_id);
        let ret_type = if function.return_type_id == PRELUDE_UNIT_TYPE_ID { ret_type } else { format!("{}*", ret_type) };
        let fn_name = function_name(&function);
        let params = if function.captured_vars.is_empty() {
            vec!["size_t nargs".to_string()]
        } else {
            vec!["size_t nargs".to_string(), "AbraAny** __captures".to_string()]
        };
        let params = params.into_iter()
            .chain(function.params.iter().map(|p| {
                let type_name = if p.is_variadic {
                    "AbraArray".to_string()
                } else {
                    self.get_type_name_by_id(project, &p.type_id)
                };
                format!("{}* {}", type_name, &p.name)
            }))
            .join(", ");

        write!(self.output, "{} {}({})", ret_type, fn_name, params).expect("Output should be able to be written to");
    }

    fn emit_fn_predecl(&mut self, project: &Project, func_id: &FuncId) {
        self.emit_fn_signature(project, func_id);
        writeln!(self.output, ";").expect("Output should be able to be written to");
    }

    fn emit_struct_predecl(&mut self, project: &Project, struct_: &Struct) {
        writeln!(self.output, "struct {};", self.get_struct_enum_name(project, &TypeKind::Struct(struct_.id))).expect("Output should be able to be written to");
    }

    fn emit_struct_decl(&mut self, project: &Project, struct_: &Struct) {
        writeln!(self.output, "typedef struct {} {{", self.get_struct_enum_name(project, &TypeKind::Struct(struct_.id))).expect("Output should be able to be written to");
        for field in &struct_.fields {
            writeln!(self.output, "  {} {};", self.get_type_name_by_id(project, &field.type_id), &field.name).expect("Output should be able to be written to");
        }
        writeln!(self.output, "}} {};", self.get_struct_enum_name(project, &TypeKind::Struct(struct_.id))).expect("Output should be able to be written to");

        for func_id in &struct_.methods {
            let function = project.get_func_by_id(func_id);
            if !function.captured_vars.is_empty() { todo!("Need to implement closures") }
            self.emit_fn_predecl(project, func_id);
        }
    }

    fn emit_enum_predecl(&mut self, project: &Project, enum_: &Enum) {
        writeln!(self.output, "enum {};", self.get_struct_enum_name(project, &TypeKind::Enum(enum_.id))).expect("Output should be able to be written to");
    }

    fn emit_main_function(&mut self, project: &Project) {
        self.emit_line("int main(int argc, char** argv) {");
        self.emit_comment("Runtime initialization");
        self.emit_line(format!("init_vtable({});", project.modules.iter().map(|m| m.structs.len()).sum::<usize>()));

        self.emit_newline();
        self.emit_comment("Load and execute module code");
        for module in &project.modules {
            self.emit_comment(&module.name);
            writeln!(self.output, "{}();", self.get_module_entrypoint_name(&module)).expect("Output should be able to be written to");
        }
        writeln!(self.output, "}}").expect("Output should be able to be written to");
    }

    // --- EMITTER LOGIC END ---

    pub fn generate(&mut self, project: Project) {
        self.emit_line("#include \"math.h\"\n");
        self.emit_line("#include \"prelude.h\"\n");

        self.emit_comment("Supply extern type_id constants for builtin prelude types");
        self.emit_line(format!("const size_t TYPE_ID_TUPLE = {};", project.prelude_tuple_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_INT = {};", project.prelude_int_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_FLOAT = {};", project.prelude_float_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_BOOL = {};", project.prelude_bool_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_STRING = {};", project.prelude_string_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_ARRAY = {};", project.prelude_array_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_NONE = {};", project.prelude_none_type_id.1));
        self.emit_newline();

        for module in &project.modules {
            if module.id == PRELUDE_MODULE_ID { continue; }

            self.emit_comment(&module.name);
            self.compile_forward_declarations(&project, &module);

            self.compile_toplevel_code(&project, &module);
        }

        self.emit_newline();
        self.emit_comment("Entrypoint begins here");
        self.emit_main_function(&project);
    }

    fn compile_forward_declarations(&mut self, project: &Project, module: &TypedModule) {
        for struct_ in &module.structs {
            self.emit_struct_predecl(project, struct_);
        }
        if !module.structs.is_empty() { self.emit_newline(); }

        for enum_ in &module.enums {
            self.emit_enum_predecl(project, enum_);
        }
        if !module.enums.is_empty() { self.emit_newline(); }

        for scope in &module.scopes {
            for function in &scope.funcs {
                if function.has_self { continue; }

                self.emit_fn_predecl(project, &function.id);
                if !function.captured_vars.is_empty() {
                    self.emit_comment(format!("Closure value for function {}", function_name(function)));
                    self.emit_line(format!("AbraFn* {} = NULL;", closure_name(function)));
                }
            }
        }
        for scope in &module.scopes {
            for function in &scope.funcs {
                self.compile_function(project, &function.id, false);
            }
        }

        for struct_ in &module.structs {
            self.emit_struct_decl(project, struct_);
            self.emit_newline();
        }
    }

    fn compile_function(&mut self, project: &Project, func_id: &FuncId, allow_method: bool) {
        let function = project.get_func_by_id(func_id);
        if function.has_self && !allow_method { return; }

        self.emit_fn_signature(project, func_id);
        self.emit_line("{");

        if !function.captured_vars.is_empty() {
            for (idx, var_id) in function.captured_vars.iter().enumerate() {
                let variable = project.get_var_by_id(var_id);
                let var_type_name = self.get_type_name_by_id(project, &variable.type_id);
                self.emit_line(format!("{}* {} = ({}*)__captures[{}];", var_type_name, variable.name, var_type_name, idx));
            }
        }

        let mut param_idx = 0;
        for param in &function.params {
            param_idx += 1;
            let Some(default_value) = &param.default_value else { continue; };
            self.emit_line(format!("if (nargs < {} || IS_NONE({})) {{", param_idx, &param.name));
            let handle = self.compile_expression(project, default_value);
            self.emit_line(format!("{} = {};", &param.name, handle));
            self.emit_line("}");
        }

        let body_len = function.body.len();
        for (idx, node) in function.body.iter().enumerate() {
            if idx == body_len - 1 && function.return_type_id != PRELUDE_UNIT_TYPE_ID {
                let expr_handle = self.compile_expression(project, node);
                self.emit_line(format!("return {};", expr_handle));
            } else {
                self.compile_statement(project, node);
            }
        }

        self.emit_line("}");
    }

    fn compile_toplevel_code(&mut self, project: &Project, module: &TypedModule) {
        self.emit_line(&format!("void {}() {{", self.get_module_entrypoint_name(&module)));
        for node in &module.code {
            self.compile_statement(project, node);
        }
        self.emit_line("}");
    }

    fn compile_statement(&mut self, project: &Project, node: &TypedNode) {
        match node {
            TypedNode::If { .. } => {}
            TypedNode::Match { .. } => {}
            TypedNode::FuncDeclaration(func_id) => {
                let function = project.get_func_by_id(func_id);
                if function.captured_vars.is_empty() { return; }

                let wrapped = self.wrap_function(project, &function.id);
                self.emit_line(format!("{} = {};", closure_name(function), wrapped));
            }
            TypedNode::BindingDeclaration { expr, vars, pattern, .. } => {
                if let Some(expr) = expr {
                    let expr_handle = self.compile_expression(project, expr);
                    let vars = vars.iter().map(|var_id| project.get_var_by_id(var_id)).collect_vec();
                    self.compile_pattern_destructuring(project, &expr_handle, expr.as_ref().type_id(), pattern, &vars);
                } else {
                    for var_id in vars {
                        let var = project.get_var_by_id(var_id);
                        let type_name = self.get_type_name_by_id(project, &var.type_id);
                        self.emit_line(format!("{}* {} = ({}*)AbraNone_make();", type_name, &var.name, type_name));
                        if var.is_captured {
                            self.emit_line(format!("MOVE_TO_HEAP({}, {}*)", &var.name, type_name));
                        }
                    }
                }
            }
            TypedNode::ForLoop { .. } => {}
            TypedNode::WhileLoop { .. } => {}
            TypedNode::Break { .. } => {}
            TypedNode::Continue { .. } => {}
            TypedNode::Return { .. } => {}
            TypedNode::Import { .. } => {}
            n => {
                self.compile_expression(project, n);
            }
        }
    }

    fn compile_pattern_destructuring(&mut self, project: &Project, expr_handle: &String, expr_type: &TypeId, pattern: &BindingPattern, vars: &Vec<&Variable>) {
        match pattern {
            BindingPattern::Variable(tok) => {
                let var_name = Token::get_ident_name(tok);
                let var = vars.iter().find(|var| var.name == var_name).expect("There should be a variable by this name in the vars list");
                let type_name = self.get_type_name_by_id(project, expr_type);
                self.emit_line(format!("{}* {} = {};", type_name, var_name, expr_handle));
                if var.is_captured {
                    self.emit_line(format!("MOVE_TO_HEAP({}, {}*);", var_name, type_name));
                }
            }
            BindingPattern::Tuple(_, patterns) => {
                let Type::GenericInstance(struct_id, generic_ids) = project.get_type_by_id(expr_type) else {
                    unreachable!("The expression must be a tuple to enter here");
                };
                debug_assert!(*struct_id == project.prelude_tuple_struct_id);
                debug_assert!(generic_ids.len() == patterns.len());

                for (idx, (pattern, tuple_item_type_id)) in patterns.iter().zip(generic_ids).enumerate() {
                    let tuple_item_type_name = self.get_type_name_by_id(project, tuple_item_type_id);

                    match pattern {
                        BindingPattern::Variable(tok) => {
                            let var_name = Token::get_ident_name(tok);
                            self.compile_opt_chaining(&var_name, &tuple_item_type_name, &expr_handle, |_| {
                                format!("AbraTuple_get({}, {})", expr_handle, idx)
                            })
                        }
                        _ => {
                            let item_handle = self.next_temp_variable();
                            self.emit_line(format!("{}* {} = ({}*)AbraTuple_get({}, {});", tuple_item_type_name, item_handle, tuple_item_type_name, expr_handle, idx));
                            self.compile_pattern_destructuring(project, &item_handle, tuple_item_type_id, pattern, vars);
                        }
                    }
                }
            }
            BindingPattern::Array(_, patterns, is_string) => {
                let item_type_id;
                let expr_type_name;
                if *is_string {
                    item_type_id = PRELUDE_STRING_TYPE_ID;
                    expr_type_name = self.get_type_name_by_id(project, expr_type);
                } else {
                    let Type::GenericInstance(struct_id, generic_ids) = project.get_type_by_id(expr_type) else { unreachable!("The expression must be an array to enter here"); };
                    debug_assert!(*struct_id == project.prelude_array_struct_id);
                    item_type_id = *generic_ids.first().expect("The array must have a type");
                    expr_type_name = self.get_type_name_by_id(project, expr_type);
                }
                let item_type_name = self.get_type_name_by_id(project, &item_type_id);

                let num_patterns = patterns.len();
                let mut expr_handle = expr_handle.to_string();
                let mut idx = 0;
                for (_, (pattern, is_splat)) in patterns.iter().enumerate() {
                    match pattern {
                        BindingPattern::Variable(tok) => {
                            let var_name = Token::get_ident_name(tok);

                            let dest_type_name = if *is_splat { &expr_type_name } else { &item_type_name };
                            self.compile_opt_chaining(&var_name, &dest_type_name, &expr_handle, |_| {
                                if *is_splat {
                                    if *is_string {
                                        format!("AbraString_get_range((AbraString*){}, {}, ((AbraString*){})->length)", &expr_handle, idx, &expr_handle)
                                    } else {
                                        format!("AbraArray_get_range((AbraArray*){}, {}, ((AbraArray*){})->length)", &expr_handle, idx, &expr_handle)
                                    }
                                } else {
                                    if *is_string {
                                        format!("AbraString_get((AbraString*){}, {})", &expr_handle, idx)
                                    } else {
                                        format!("AbraArray_get((AbraArray*){}, {})", &expr_handle, idx)
                                    }
                                }
                            });

                            if *is_splat {
                                let tmp = self.next_temp_variable();
                                let num_remaining = num_patterns - idx - 1;
                                if *is_string {
                                    self.emit_line(format!("AbraString* {} = AbraString_slice({}, {}->length - {});", &tmp, &var_name, &var_name, num_remaining));
                                } else {
                                    self.emit_line(format!("AbraArray* {} = AbraArray_slice({}, {}->length - {});", &tmp, &var_name, &var_name, num_remaining));
                                }
                                expr_handle = tmp;
                                idx = 0;
                                continue;
                            }
                        }
                        _ => {
                            let item_handle = self.next_temp_variable();
                            self.compile_opt_chaining(&item_handle, &item_type_name, &expr_handle, |_| {
                                if *is_string {
                                    format!("AbraString_get((AbraString*){}, {})", &expr_handle, idx)
                                } else {
                                    format!("AbraArray_get((AbraArray*){}, {})", &expr_handle, idx)
                                }
                            });
                            self.compile_pattern_destructuring(project, &item_handle, &item_type_id, pattern, vars);
                        }
                    }

                    idx += 1;
                }
            }
        }
    }

    fn compile_opt_chaining<F>(&mut self, var_handle: &String, var_type_name: &String, target_handle: &String, continuation: F)
        where F: Fn(&mut CCompiler2<W>) -> String
    {
        self.emit_line(format!("{}* {};", var_type_name, var_handle));
        self.emit_line(format!("if (IS_NONE({})) {{", target_handle));
        self.emit_line(format!("  {} = ({}*)AbraNone_make();", var_handle, var_type_name));
        self.emit_line("} else {");

        let continuation_handle = continuation(self);
        self.emit_line(format!("  {} = ({}*){};", var_handle, var_type_name, continuation_handle));
        self.emit_line("}");
    }

    fn compile_expression(&mut self, project: &Project, node: &TypedNode) -> String {
        match node {
            TypedNode::Literal { value, .. } => match value {
                TypedLiteral::Int(i) => format!("AbraInt_make({})", i),
                TypedLiteral::Float(f) => format!("AbraFloat_make({})", f),
                TypedLiteral::Bool(b) => format!("AbraBool_make({})", b),
                TypedLiteral::String(s) => format!("AbraString_make({}, \"{}\")", s.len(), s),
            }
            TypedNode::Unary { expr, op, .. } => {
                let type_id = *expr.as_ref().type_id();

                let fn_name = if type_id == PRELUDE_INT_TYPE_ID {
                    "AbraInt_make"
                } else if type_id == PRELUDE_FLOAT_TYPE_ID {
                    "AbraFloat_make"
                } else if type_id == PRELUDE_BOOL_TYPE_ID {
                    "AbraBool_make"
                } else {
                    unreachable!()
                };

                let fn_arg = match (op, expr.as_ref()) {
                    (UnaryOp::Minus, TypedNode::Literal { value: TypedLiteral::Int(value), .. }) => format!("-{}", value),
                    (UnaryOp::Minus, TypedNode::Literal { value: TypedLiteral::Float(value), .. }) => format!("-{}", value),
                    (UnaryOp::Negate, TypedNode::Literal { value: TypedLiteral::Bool(value), .. }) => format!("!{}", value),
                    (UnaryOp::Minus, expr) => format!("-({})->value", self.compile_expression(project, expr)),
                    (UnaryOp::Negate, expr) => format!("!({})->value", self.compile_expression(project, expr)),
                };

                format!("{}({})", fn_name, fn_arg)
            }
            TypedNode::Binary { left, right, op, .. } => {
                let type_id = *node.type_id();

                let left_handle = self.compile_expression(project, left);
                if *op == BinaryOp::And || *op == BinaryOp::Or || *op == BinaryOp::Coalesce {
                    let handle = self.next_temp_variable();

                    if *op == BinaryOp::And {
                        self.emit_line(format!("AbraBool* {};", handle));
                        self.emit_line(format!("if (!{}->value) {{", left_handle));
                        self.emit_line(format!("{} = {};", handle, left_handle));
                        self.emit_line("} else {");
                    } else if *op == BinaryOp::Or {
                        self.emit_line(format!("AbraBool* {};", handle));
                        self.emit_line(format!("if ({}->value) {{", left_handle));
                        self.emit_line(format!("{} = {};", handle, left_handle));
                        self.emit_line("} else {");
                    } else {
                        let type_name = self.get_type_name_by_id(project, &type_id);
                        self.emit_line(format!("{}* {} = ({}*){};", type_name, handle, type_name, left_handle));
                        self.emit_line(format!("if (IS_NONE({})) {{", handle));
                    }

                    let right_handle = self.compile_expression(project, right);
                    self.emit_line(format!("{} = {};", handle, right_handle));
                    self.emit_line("}");

                    return handle;
                }

                let right_handle = self.compile_expression(project, right);

                let compile_arithmetic_op = |op: &str| {
                    if type_id == PRELUDE_INT_TYPE_ID {
                        format!("AbraInt_make({}->value {} {}->value)", left_handle, op, right_handle)
                    } else if type_id == PRELUDE_FLOAT_TYPE_ID {
                        format!("AbraFloat_make((double){}->value {} (double){}->value)", left_handle, op, right_handle)
                    } else {
                        unreachable!("No other resultant types are possible for the {} operation", op);
                    }
                };

                let compile_comparison_op = |op: &str| {
                    format!("AbraBool_make({}->value {} {}->value)", left_handle, op, right_handle)
                };

                match op {
                    BinaryOp::Add => {
                        if type_id == PRELUDE_STRING_TYPE_ID {
                            if *left.type_id() == PRELUDE_STRING_TYPE_ID {
                                format!("AbraString__concat(2, {}, (AbraAny*){})", left_handle, right_handle)
                            } else if *right.type_id() == PRELUDE_STRING_TYPE_ID {
                                format!("AbraString__concat(2, prelude__tostring((AbraAny*){}), (AbraAny*){})", left_handle, right_handle)
                            } else {
                                unreachable!("Either the left or right node must be a string")
                            }
                        } else {
                            compile_arithmetic_op("+")
                        }
                    }
                    BinaryOp::Sub => compile_arithmetic_op("-"),
                    BinaryOp::Mul => compile_arithmetic_op("*"),
                    BinaryOp::Div => compile_arithmetic_op("/"),
                    BinaryOp::Mod => {
                        if *left.type_id() == PRELUDE_FLOAT_TYPE_ID || *right.type_id() == PRELUDE_FLOAT_TYPE_ID {
                            format!("AbraFloat_make(fmod((double){}->value, (double){}->value))", left_handle, right_handle)
                        } else {
                            format!("AbraInt_make({}->value % {}->value)", left_handle, right_handle)
                        }
                    }
                    BinaryOp::Pow => {
                        format!("AbraFloat_make(pow((double){}->value, (double){}->value))", left_handle, right_handle)
                    }
                    BinaryOp::And | BinaryOp::Or | BinaryOp::Coalesce => unreachable!("Handled above"),
                    BinaryOp::Xor => {
                        format!("AbraBool_make(!({}->value) != !({}->value))", left_handle, right_handle)
                    }
                    BinaryOp::Lt => compile_comparison_op("<"),
                    BinaryOp::Lte => compile_comparison_op("<="),
                    BinaryOp::Gt => compile_comparison_op(">"),
                    BinaryOp::Gte => compile_comparison_op(">="),
                    BinaryOp::Neq | BinaryOp::Eq => {
                        format!("prelude__eq((AbraAny*)({}), (AbraAny*)({}), {})", left_handle, right_handle, *op == BinaryOp::Neq)
                    }
                    BinaryOp::AddEq |
                    BinaryOp::SubEq |
                    BinaryOp::MulEq |
                    BinaryOp::DivEq |
                    BinaryOp::ModEq |
                    BinaryOp::AndEq |
                    BinaryOp::OrEq |
                    BinaryOp::CoalesceEq => todo!()
                }
            }
            TypedNode::Grouped { expr, .. } => self.compile_expression(project, expr),
            TypedNode::Array { items, .. } => {
                let capacity = if items.is_empty() { 0 } else { items.len().next_power_of_two() };

                let handle = self.next_temp_variable();

                self.emit_line(format!("AbraArray* {} = AbraArray_make_with_capacity(/*length:*/ {}, /*capacity:*/{});", handle, items.len(), capacity));
                for (idx, item) in items.iter().enumerate() {
                    let expr_handle = self.compile_expression(project, &item);
                    self.emit_line(format!("AbraArray_set(/*self:*/{}, /*idx:*/{}, /*item:*/(AbraAny*){});", handle, idx, expr_handle));
                }

                handle
            }
            TypedNode::Tuple { items, .. } => {
                let item_handles = items.iter().map(|item| self.compile_expression(project, item)).join(", ");
                format!("AbraTuple_make({}, {})", items.len(), item_handles)
            }
            TypedNode::Set { .. } => unimplemented!(),
            TypedNode::Map { .. } => unimplemented!(),
            TypedNode::Identifier { var_id, .. } => {
                let variable = project.get_var_by_id(var_id);
                match variable.alias {
                    VariableAlias::None => {
                        if variable.is_captured {
                            let var_type_name = self.get_type_name_by_id(project, &variable.type_id);
                            format!("HEAP_DEREF({}, {}*)", &variable.name, var_type_name)
                        } else {
                            variable.name.clone()
                        }
                    },
                    VariableAlias::Function(func_id) => {
                        // If a function is being referenced by identifier, it could be for the purpose of invocation, or simply for
                        // passing around a function as a value. If the former, the underlying c function will simply be invoked (see
                        // implementation in the TypedNode::Invocation compilation step); if the latter, then we need to wrap up the
                        // c function in an AbraFn so it can be passed around as a value.
                        self.wrap_function(project, &func_id)
                    }
                    VariableAlias::Type(_) => unimplemented!(),
                }
            }
            TypedNode::NoneValue { .. } => "AbraNone_make()".to_string(),
            TypedNode::Invocation { target, arguments, type_id, .. } => {
                let Type::Function(parameter_type_ids, _, _, return_type_id) = project.get_type_by_id(&target.as_ref().type_id()) else { unreachable!() };
                debug_assert!(parameter_type_ids.len() == arguments.len());
                let return_type_name = format!("{}*", self.get_type_name_by_id(project, return_type_id));

                let num_arguments = arguments.len();
                let mut arg_values = Vec::with_capacity(num_arguments);
                for (argument, parameter_type_id) in arguments.iter().zip(parameter_type_ids) {
                    let handle = if let Some(argument) = argument {
                        self.compile_expression(project, &argument)
                    } else {
                        format!("(({}*)AbraNone_make())", self.get_type_name_by_id(project, parameter_type_id))
                    };
                    arg_values.push(handle);
                }

                if let TypedNode::Identifier { var_id, .. } = &**target {
                    let variable = project.get_var_by_id(var_id);
                    if let VariableAlias::Function(func_id) = &variable.alias {
                        let handle = if *type_id != PRELUDE_UNIT_TYPE_ID {
                            let handle = self.next_temp_variable();
                            self.emit(format!("{}* {} = ", self.get_type_name_by_id(project, type_id), handle));
                            Some(handle)
                        } else {
                            None
                        };

                        let function = project.get_func_by_id(func_id);
                        if !function.captured_vars.is_empty() {
                            self.emit(format!("ABRA_CL_CALL_{}({}, ", function.params.len(), closure_name(function)));
                            self.emit(vec![num_arguments.to_string(), return_type_name].into_iter().chain(arg_values).join(", "));
                        } else {
                            self.emit(format!("{}(", function_name(function)));
                            self.emit(vec![num_arguments.to_string()].into_iter().chain(arg_values).join(", "));
                        }
                        self.emit_line(");");

                        return handle.unwrap_or_default();
                    }
                }

                let target_handle = self.compile_expression(project, &*target);
                let arg_types = parameter_type_ids.iter().map(|param_type_id| {
                    format!("{}*", self.get_type_name_by_id(project, param_type_id))
                }).collect_vec();

                let handle = if *type_id != PRELUDE_UNIT_TYPE_ID {
                    let handle = self.next_temp_variable();
                    self.emit(format!("{}* {} = ", self.get_type_name_by_id(project, type_id), handle));
                    Some(handle)
                } else {
                    None
                };
                self.emit(format!("ABRA_FN_CALL_{}((AbraFn*){}, {}, ", num_arguments, target_handle, num_arguments));
                self.emit(vec![return_type_name].into_iter()
                    .chain(arg_values.iter().zip(arg_types).map(|(v, ty)| format!("{ty}, {v}")))
                    .join(",")
                );
                self.emit_line(");");

                handle.unwrap_or_default()
            }
            TypedNode::Accessor { .. } => unimplemented!(),
            TypedNode::Indexing { target, index, .. } => {
                let target_type_id = target.as_ref().type_id();
                let target_type = project.get_type_by_id(target_type_id);

                let target_handle = self.compile_expression(project, target);

                let fn_name = match (target_type, &index) {
                    (Type::Primitive(PrimitiveType::String), IndexingMode::Index(_)) => "AbraString_get",
                    (Type::Primitive(PrimitiveType::String), IndexingMode::Range(_, _)) => "AbraString_get_range",
                    (Type::GenericInstance(struct_id, _), IndexingMode::Index(_)) if *struct_id == project.prelude_array_struct_id => "AbraArray_get",
                    (Type::GenericInstance(struct_id, _), IndexingMode::Range(_, _)) if *struct_id == project.prelude_array_struct_id => "AbraArray_get_range",
                    (Type::GenericInstance(struct_id, _), IndexingMode::Index(_)) if *struct_id == project.prelude_tuple_struct_id => "AbraTuple_get",
                    _ => unimplemented!(),
                };

                let fn_args = match index {
                    IndexingMode::Index(idx_expr) => {
                        let idx_expr_handle = self.compile_expression(project, idx_expr);
                        match target_type {
                            Type::Primitive(PrimitiveType::String) => format!("{}->value", idx_expr_handle),
                            Type::GenericInstance(struct_id, _) if *struct_id == project.prelude_array_struct_id => format!("{}->value", idx_expr_handle),
                            Type::GenericInstance(struct_id, _) if *struct_id == project.prelude_tuple_struct_id => format!("{}->value", idx_expr_handle),
                            _ => idx_expr_handle
                        }
                    }
                    IndexingMode::Range(start_expr, end_expr) => {
                        match (start_expr, end_expr) {
                            (Some(start_expr), Some(end_expr)) => {
                                let start_expr_handle = self.compile_expression(project, start_expr);
                                let end_expr_handle = self.compile_expression(project, end_expr);
                                format!("{}->value, {}->value", start_expr_handle, end_expr_handle)
                            }
                            (None, Some(end_expr)) => {
                                let end_expr_handle = self.compile_expression(project, end_expr);
                                format!("0, {}->value", end_expr_handle)
                            }
                            (Some(start_expr), None) => {
                                let start_expr_handle = self.compile_expression(project, start_expr);
                                format!("{}->value, {}->length", start_expr_handle, target_handle)
                            }
                            (None, None) => unreachable!("foo[:] is invalid syntax at the moment")
                        }
                    }
                };

                format!("{}({}, {})", fn_name, target_handle, fn_args)
            }
            TypedNode::Lambda { func_id, .. } => {
                self.wrap_function(project, func_id)
            }
            TypedNode::Assignment { kind, expr, .. } => {
                let expr_handle = self.compile_expression(project, expr);
                match kind {
                    AssignmentKind::Identifier { var_id } => {
                        let variable = project.get_var_by_id(var_id);
                        if variable.is_captured {
                            let var_type_name = self.get_type_name_by_id(project, &variable.type_id);
                            self.emit_line(format!("HEAP_DEREF({}, {}*) = {};", &variable.name, var_type_name, expr_handle));
                        } else {
                            self.emit_line(format!("{} = {};", &variable.name, expr_handle));
                        }
                        variable.name.clone()
                    }
                    AssignmentKind::Accessor { .. } => todo!(),
                    AssignmentKind::Indexing { index, target } => {
                        let target_type_id = target.as_ref().type_id();
                        let target_type = project.get_type_by_id(target_type_id);
                        let Type::GenericInstance(struct_id, _) = target_type else { unreachable!() };
                        debug_assert!(*struct_id == project.prelude_array_struct_id);
                        let target_handle = self.compile_expression(project, target);
                        let index_handle = self.compile_expression(project, index);
                        self.emit_line(format!("AbraArray_set(/*self:*/{}, /*idx:*/{}->value, /*item:*/(AbraAny*){});", target_handle, index_handle, expr_handle));
                        expr_handle
                    }
                }
            }
            n => unreachable!("Internal error: node is not an expression: {:?}", n),
        }
    }

    fn wrap_function(&mut self, project: &Project, func_id: &FuncId) -> String {
        let function = project.get_func_by_id(func_id);
        let (opt_params, req_params): (Vec<_>, Vec<_>) = function.params.iter().partition(|p| p.default_value.is_some());
        let min_arity = req_params.len();
        let max_arity = opt_params.len() + req_params.len();

        let fn_name = function_name(&function);
        if !function.captured_vars.is_empty() {
            let num_captures = function.captured_vars.len();
            let captures = function.captured_vars.iter()
                .map(|var_id| format!("(AbraAny**){}", &project.get_var_by_id(var_id).name))
                .join(",");
            // TODO: pre-allocate maybe, rather than passing captures as varargs?
            format!("AbraFn_make_closure((Fn)&{}, {}, {}, {}, {})", fn_name, min_arity, max_arity, num_captures, captures)
        } else {
            format!("AbraFn_make((Fn)&{}, {}, {})", fn_name, min_arity, max_arity)
        }
    }
}

#[cfg(test)]
mod test {
    use std::process::Command;
    use assert_cmd::cargo::CommandCargoExt;
    use itertools::{EitherOrBoth, Itertools};
    use crate::transpile::get_project_root::get_project_root;

    fn run_test_file(file_name: &str) {
        let rust_project_root = get_project_root().unwrap();

        let test_file_path = rust_project_root.join("abra_core").join("src").join("transpile").join("testv2").join(file_name);
        let test_file = std::fs::read_to_string(&test_file_path).unwrap();

        let mut cmd = Command::cargo_bin("abra").unwrap();
        cmd.arg("compile2").arg(&test_file_path);
        let output = cmd.output().unwrap();
        assert!(output.stderr.is_empty(), "Compilation error: {}", String::from_utf8(output.stderr).unwrap());

        let output = String::from_utf8(output.stdout).unwrap();
        let output = output.lines();

        let prefix = "/// Expect: ";
        let expectations = test_file.lines()
            .map(|line| line.trim())
            .enumerate()
            .filter(|(_, line)| line.starts_with(prefix))
            .map(|(line_num, line)| (line_num + 1, line.replace(prefix, "")))
            .collect_vec();

        for pair in expectations.iter().zip_longest(output) {
            match pair {
                EitherOrBoth::Both((line_num, expected), actual) => {
                    assert_eq!(expected, actual, "Expectation mismatch at {}:{}", test_file_path.to_str().unwrap(), line_num);
                }
                EitherOrBoth::Left((line_num, expected)) => {
                    assert!(false, "Expected: {} (line {}), but reached end of output", expected, line_num);
                }
                EitherOrBoth::Right(actual) => {
                    assert!(false, "Received line: {}, but there were no more expectations", actual);
                }
            }
        }
    }

    #[test]
    fn builtin_values() {
        run_test_file("builtinValues.abra");
    }

    #[test]
    fn unary_ops() {
        run_test_file("unaryOps.abra");
    }

    #[test]
    fn binary_ops() {
        run_test_file("binaryOps.abra");
    }

    #[test]
    fn variable_declaration() {
        run_test_file("variableDeclaration.abra");
    }

    #[test]
    fn assignment() {
        run_test_file("assignment.abra");
    }

    #[test]
    fn indexing() {
        run_test_file("indexing.abra");
    }

    #[test]
    fn functions() {
        run_test_file("functions.abra");
    }

    #[test]
    fn lambdas() {
        run_test_file("lambdas.abra");
    }

    #[test]
    fn closures() {
        run_test_file("closures.abra");
    }
}
