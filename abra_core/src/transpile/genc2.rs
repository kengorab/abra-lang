use std::fmt::{Display, Formatter};
use itertools::Itertools;
use crate::lexer::tokens::Token;
use crate::parser::ast::{BinaryOp, BindingPattern, IndexingMode, UnaryOp};
use crate::typechecker::typechecker2::{AccessorKind, AssignmentKind, Enum, EnumId, FuncId, Function, ModuleId, PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_MODULE_ID, PRELUDE_STRING_TYPE_ID, PRELUDE_UNIT_TYPE_ID, PrimitiveType, Project, ScopeId, Struct, StructId, Type, TypedLiteral, TypedModule, TypedNode, TypeId, TypeKind, Variable, VariableAlias};

#[derive(Clone)]
struct SSAHandle(String);

impl Default for SSAHandle {
    fn default() -> Self {
        SSAHandle("".to_string())
    }
}

impl Display for SSAHandle {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

pub struct CCompiler2<W: std::io::Write> {
    output: W,
    temp_idx: usize,
    current_fn: Option<FuncId>,
}

impl<W: std::io::Write> CCompiler2<W> {
    pub fn new(output: W) -> Self {
        CCompiler2 { output, temp_idx: 0, current_fn: None }
    }

    fn next_ssa_handle(&mut self) -> SSAHandle {
        let temp_idx = self.temp_idx;
        self.temp_idx += 1;
        SSAHandle(format!("_tmp__{}", temp_idx))
    }

    fn get_type_name_by_id(&self, project: &Project, type_id: &TypeId) -> String {
        let ty = project.get_type_by_id(type_id);
        self.get_type_name(project, ty)
    }

    fn get_type_name(&self, project: &Project, ty: &Type) -> String {
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
                } else if *struct_id == project.prelude_set_struct_id {
                    "AbraSet".to_string()
                } else if *struct_id == project.prelude_map_struct_id {
                    "AbraMap".to_string()
                } else if generic_ids.is_empty() {
                    self.get_struct_or_enum_name(project, &TypeKind::Struct(*struct_id))
                } else {
                    let struct_ = project.get_struct_by_id(struct_id);
                    todo!("Unimplemented for struct {struct_:?}")
                }
            }
            Type::GenericEnumInstance(_, _, _) |
            Type::Function(_, _, _, _) => "AbraFn".to_string(),
            Type::Type(kind) => self.get_struct_or_enum_name(project, kind),
            Type::ModuleAlias => todo!(),
        }
    }

    fn get_struct_or_enum_name(&self, project: &Project, kind: &TypeKind) -> String {
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

    fn function_name(&self, project: &Project, func: &Function, container_type: Option<&Type>) -> String {
        let FuncId(ScopeId(ModuleId(m_id), s_id), f_id) = func.id;

        if let Some(ty) = container_type {
            self.method_name(project, ty, &func.name)
        } else {
            format!("_{}_{}_{}__{}", m_id, s_id, f_id, func.name)
        }
    }

    fn method_name<S: AsRef<str>>(&self, project: &Project, ty: &Type, name: S) -> String {
        let type_name = self.get_type_name(project, ty);
        format!("{}__{}", type_name, name.as_ref())
    }

    fn closure_name(&self, project: &Project, func: &Function, container_type: Option<&Type>) -> String {
        format!("{}_v_", self.function_name(project, func, container_type))
    }

    fn get_ctor_fn_name(&self, project: &Project, struct_id: &StructId) -> String {
        format!("{}_make", self.get_struct_or_enum_name(project, &TypeKind::Struct(*struct_id)))
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

    fn emit_fn_signature(&mut self, project: &Project, function: &Function, container_type: Option<&Type>) {
        let ret_type = self.get_type_name_by_id(project, &function.return_type_id);
        let fn_name = self.function_name(project, &function, container_type);
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
                format!("{} {}", type_name, &p.name)
            }))
            .join(", ");

        write!(self.output, "{} {}({})", ret_type, fn_name, params).expect("Output should be able to be written to");
    }

    fn emit_fn_predecl(&mut self, project: &Project, function: &Function, container_type: Option<&Type>) {
        self.emit_fn_signature(project, function, container_type);
        self.emit_line(";");

        if !function.captured_vars.is_empty() {
            self.emit_comment(format!("Closure value for function {}", self.function_name(project, function, container_type)));
            self.emit_line(format!("AbraFn {};", self.closure_name(project, function, container_type)));
        }
    }

    fn emit_struct_predecl(&mut self, project: &Project, struct_: &Struct) {
        let type_kind = TypeKind::Struct(struct_.id);
        let ty = Type::Type(type_kind);

        let type_name = self.get_struct_or_enum_name(project, &type_kind);
        let scalar_type_id = self.compute_scalar_type_id(project, &struct_.id);
        self.emit_line(format!("const size_t TYPE_ID_{} = {};", &type_name, scalar_type_id));
        self.emit_line(format!("struct {}_Inner;", &type_name));
        self.emit_line(format!("typedef struct {} {{\nsize_t type_id;\nstruct {}_Inner* value;\n}} {};", &type_name, &type_name, &type_name));
        self.emit_ctor_fn_signature(project, struct_);
        self.emit_line(";");
        self.emit_tostring_fn_signature(project, &ty);
        self.emit_line(";");
        self.emit_eq_fn_signature(project, &ty);
        self.emit_line(";");
        self.emit_hash_fn_signature(project, &ty);
        self.emit_line(";");
        for func_id in &struct_.methods {
            let function = project.get_func_by_id(func_id);
            self.emit_fn_predecl(project, function, Some(&ty));
        }
    }

    fn compute_scalar_type_id(&self, project: &Project, struct_id: &StructId) -> usize {
        let StructId(ModuleId(m_id), s_id) = struct_id;
        let mut id = 0usize;
        for i in 0..*m_id {
            id += project.modules[i].structs.len();
        }
        id + s_id
    }

    fn emit_ctor_fn_signature(&mut self, project: &Project, struct_: &Struct) {
        let struct_type_name = self.get_struct_or_enum_name(project, &TypeKind::Struct(struct_.id));
        self.emit(format!("{} {}_make(", &struct_type_name, &struct_type_name));
        for (idx, field) in struct_.fields.iter().enumerate() {
            self.emit(format!("{} {}", self.get_type_name_by_id(project, &field.type_id), &field.name));
            if idx != struct_.fields.len() - 1 {
                self.emit(", ");
            }
        }
        self.emit(")");
    }

    fn emit_tostring_fn_signature(&mut self, project: &Project, ty: &Type) {
        let type_name = self.get_type_name(project, ty);
        self.emit(format!("AbraString {}(size_t nargs, {} self)", self.method_name(project, ty, "toString"), &type_name));
    }

    fn emit_eq_fn_signature(&mut self, project: &Project, ty: &Type) {
        let type_name = self.get_type_name(project, ty);
        self.emit(format!("AbraBool {}(size_t nargs, {} self, AbraAny _other)", self.method_name(project, ty, "eq"), &type_name));
    }

    fn emit_hash_fn_signature(&mut self, project: &Project, ty: &Type) {
        let type_name = self.get_type_name(project, ty);
        self.emit(format!("AbraInt {}(size_t nargs, {} self)", self.method_name(project, ty, "hash"), &type_name));
    }

    fn emit_struct_decl(&mut self, project: &Project, struct_: &Struct) {
        let type_kind = TypeKind::Struct(struct_.id);
        let ty = Type::Type(type_kind);
        let struct_type_name = self.get_struct_or_enum_name(project, &type_kind);
        self.emit_line(format!("struct {}_Inner {{", struct_type_name));
        for field in &struct_.fields {
            self.emit_line(format!("  {} {};", self.get_type_name_by_id(project, &field.type_id), &field.name));
        }
        self.emit_line("};");

        self.emit_comment(format!("{} methods", &struct_.name));
        self.emit_line(format!("AbraFnObj {}_METHODS[] = {{", &struct_type_name.to_uppercase()));
        self.emit_line(format!("METHOD({}, 1, 1)", self.method_name(project, &ty, "toString")));
        self.emit_line("};");

        // Emit constructor
        self.emit_ctor_fn_signature(project, struct_);
        self.emit_line("{");
        self.emit_line(format!("struct {}_Inner* value = malloc(sizeof(struct {}_Inner));", &struct_type_name, &struct_type_name));
        for field in &struct_.fields {
            if let Some(default_value_node) = &field.default_value {
                self.emit_line(format!("if (!IS_NONE({})) {{\nvalue->{} = {};\n}}\nelse\n{{", &field.name, &field.name, &field.name));
                let handle = self.compile_expression(project, default_value_node);
                self.emit_line(format!("value->{} = {};\n}}", &field.name, &handle));
            } else {
                self.emit_line(format!("value->{} = {};", &field.name, &field.name));
            }
        }
        self.emit_line(format!("return ({}){{ .type_id=TYPE_ID_{}, .value=value }};", &struct_type_name, &struct_type_name));
        self.emit_line("}");

        // Emit toString
        self.emit_tostring_fn_signature(project, &ty);
        self.emit_line("{");
        self.emit_line(format!("assert(self.type_id == TYPE_ID_{});\nassert(nargs == 1);", &struct_type_name));
        for field in &struct_.fields {
            self.emit_line(format!("AbraString {}_repr = PRELUDE_TOSTRING(self.value->{});", &field.name, &field.name));
        }
        let repr_len = struct_.name.len()
            + 2 // account for '(' and ')'
            + struct_.fields.iter().map(|f| f.name.len() + 2 /* account for ": " */ + if f.type_id == PRELUDE_STRING_TYPE_ID { 2 /* account for '""' */ } else { 0 }).sum::<usize>()
            + (struct_.fields.len() - 1) * 2; // account for ", " between fields
        self.emit_line(format!("size_t len = {} + {};", repr_len, struct_.fields.iter().map(|f| format!("{}_repr.value->length", &f.name)).join(" + ")));
        self.emit_line("char* chars = malloc(len + 1);");
        self.emit_line(format!(
            "snprintf(chars, len + 1, \"{}({})\", {});",
            &struct_.name,
            struct_.fields.iter().map(|f| if f.type_id == PRELUDE_STRING_TYPE_ID { format!("{}: \\\"%s\\\"", &f.name) } else { format!("{}: %s", &f.name) }).join(", "),
            struct_.fields.iter().map(|f| format!("{}_repr.value->chars", &f.name)).join(", ")
        ));
        self.emit_line("return AbraString_make(len, chars);");
        self.emit_line("}");

        // Emit eq
        self.emit_eq_fn_signature(project, &ty);
        self.emit_line("{");
        self.emit_line(format!("assert(self.type_id == TYPE_ID_{});\nassert(nargs == 2);", &struct_type_name));
        self.emit_line(format!("if (_other.type_id != TYPE_ID_{}) return ABRA_BOOL_FALSE;", &struct_type_name));
        self.emit_line(format!("{} other = REINTERPRET_CAST(_other, {});", &struct_type_name, &struct_type_name));
        for field in &struct_.fields {
            self.emit_line(format!("if (!PRELUDE_EQ(self.value->{}, other.value->{}).value) return ABRA_BOOL_FALSE;", &field.name, &field.name));
        }
        self.emit_line("return ABRA_BOOL_TRUE;");
        self.emit_line("}");

        // Emit hash
        self.emit_hash_fn_signature(project, &ty);
        self.emit_line("{");
        self.emit_line(format!("assert(self.type_id == TYPE_ID_{});\nassert(nargs == 1);", &struct_type_name));
        self.emit_line("size_t hash = 5381;");
        self.emit_line(format!("hash = ((hash << 5) + hash) ^ TYPE_ID_{};", &struct_type_name));
        for field in &struct_.fields {
            self.emit_line(format!("hash = ((hash << 5) + hash) ^ PRELUDE_HASH(self.value->{}).value;", &field.name));
        }
        self.emit_line("return AbraInt_make((int64_t) hash);");
        self.emit_line("}");

        for func_id in &struct_.methods {
            self.compile_function(project, func_id, Some(&ty));
        }
    }

    fn emit_enum_predecl(&mut self, project: &Project, enum_: &Enum) {
        self.emit_line(format!("enum {};", self.get_struct_or_enum_name(project, &TypeKind::Enum(enum_.id))));
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
        self.emit_line("#include \"math.h\"");
        self.emit_line("#include \"assert.h\"\n");
        self.emit_line("#include \"prelude.h\"\n");

        self.emit_comment("Supply extern type_id constants for builtin prelude types");
        self.emit_line(format!("const size_t TYPE_ID_TUPLE = {};", project.prelude_tuple_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_INT = {};", project.prelude_int_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_FLOAT = {};", project.prelude_float_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_BOOL = {};", project.prelude_bool_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_STRING = {};", project.prelude_string_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_ARRAY = {};", project.prelude_array_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_SET = {};", project.prelude_set_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_MAP = {};", project.prelude_map_struct_id.1));
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
                if function.has_self() { continue; }

                // Passing container_type=None since functions with `self` are skipped above
                self.emit_fn_predecl(project, &function, None);
            }
        }
        for scope in &module.scopes {
            for function in &scope.funcs {
                if function.has_self() { continue; }

                // Passing container_type=None since functions with `self` are skipped above
                self.compile_function(project, &function.id, None);
            }
        }

        for struct_ in &module.structs {
            self.emit_struct_decl(project, struct_);
            self.emit_newline();
        }
    }

    fn compile_function(&mut self, project: &Project, func_id: &FuncId, container_type: Option<&Type>) {
        let function = project.get_func_by_id(func_id);
        let prev_fn = self.current_fn.replace(*func_id);

        self.emit_fn_signature(project, function, container_type);
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
        self.current_fn = prev_fn;
    }

    fn compile_toplevel_code(&mut self, project: &Project, module: &TypedModule) {
        self.emit_line(&format!("void {}() {{", self.get_module_entrypoint_name(&module)));
        self.emit_comment("Assert generated types are of the proper expected size");
        for struct_ in &module.structs {
            self.emit_line(format!("assert(sizeof({}) == REQUIRED_VALUE_SIZE);", self.get_struct_or_enum_name(project, &TypeKind::Struct(struct_.id))));
        }
        self.emit_line("");

        for struct_ in &module.structs {
            let type_kind = TypeKind::Struct(struct_.id);
            let struct_type_name = self.get_struct_or_enum_name(project, &type_kind);

            let ty = Type::Type(type_kind);
            self.emit_line(format!(
                "VTABLE[TYPE_ID_{}] = (VTableEntry){{ .fn_eq=METHOD({}, 2, 2), .fn_hash=METHOD({}, 1, 1), .methods={}_METHODS }};",
                struct_type_name,
                self.method_name(project, &ty, "eq"),
                self.method_name(project, &ty, "hash"),
                struct_type_name.to_uppercase(),
            ))
        }
        self.emit_line("");

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

                let wrapped = self.wrap_function(project, &function.id, None);
                self.emit_line(format!("{} = {};", self.closure_name(project, function, None), wrapped));
            }
            TypedNode::TypeDeclaration(struct_id) => {
                let struct_ = project.get_struct_by_id(struct_id);
                let ty = Type::Type(TypeKind::Struct(*struct_id));
                self.emit_comment("here");
                for func_id in &struct_.methods {
                    let function = project.get_func_by_id(func_id);
                    if function.captured_vars.is_empty() { continue; }

                    let wrapped = self.wrap_function(project, &function.id, Some(&ty));
                    self.emit_line(format!("{} = {};", self.closure_name(project, function, Some(&ty)), wrapped));
                }
            }
            TypedNode::EnumDeclaration(_enum_id) => todo!(),
            TypedNode::BindingDeclaration { expr, vars, pattern, .. } => {
                if let Some(expr) = expr {
                    let expr_handle = self.compile_expression(project, expr);
                    let vars = vars.iter().map(|var_id| project.get_var_by_id(var_id)).collect_vec();
                    self.compile_pattern_destructuring(project, &expr_handle, expr.as_ref().type_id(), pattern, &vars);
                } else {
                    for var_id in vars {
                        let var = project.get_var_by_id(var_id);
                        let type_name = self.get_type_name_by_id(project, &var.type_id);
                        self.emit_line(format!("{} {} = REINTERPRET_CAST(AbraNone, {});", type_name, &var.name, type_name));
                        if var.is_captured {
                            self.emit_line(format!("{}* {}_ref = ({}*)copy_to_heap((AbraAny*)&{});", type_name, &var.name, type_name, &var.name));
                        }
                    }
                }
            }
            TypedNode::ForLoop { .. } => {}
            TypedNode::WhileLoop { .. } => {}
            TypedNode::Break { .. } => {}
            TypedNode::Continue { .. } => {}
            TypedNode::Return { .. } => {}
            // TypedNode::Import { .. } => {}
            n => {
                self.compile_expression(project, n);
            }
        }
    }

    fn compile_pattern_destructuring(&mut self, project: &Project, expr_handle: &SSAHandle, expr_type: &TypeId, pattern: &BindingPattern, vars: &Vec<&Variable>) {
        match pattern {
            BindingPattern::Variable(tok) => {
                let var_name = Token::get_ident_name(tok);
                let var = vars.iter().find(|var| var.name == var_name).expect("There should be a variable by this name in the vars list");
                let type_name = self.get_type_name_by_id(project, expr_type);
                self.emit_line(format!("{} {} = {};", type_name, &var.name, expr_handle));
                if var.is_captured {
                    self.emit_line(format!("{}* {}_ref = ({}*)copy_to_heap((AbraAny*)&{});", type_name, &var.name, type_name, &var.name));
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
                            self.compile_opt_chaining(&SSAHandle(var_name), &tuple_item_type_name, &expr_handle, |zelf| {
                                let temp_handle = zelf.next_ssa_handle();
                                zelf.emit_line(format!("AbraAny {} = AbraTuple_get({}, {});", temp_handle, expr_handle, idx));
                                let handle = zelf.next_ssa_handle();
                                zelf.emit_line(format!("{} {} = REINTERPRET_CAST({}, {});", tuple_item_type_name, handle, temp_handle, tuple_item_type_name));
                                handle
                            })
                        }
                        _ => {
                            let temp_handle = self.next_ssa_handle();
                            self.emit_line(format!("AbraAny {} = AbraTuple_get({}, {});", temp_handle, expr_handle, idx));
                            let item_handle = self.next_ssa_handle();
                            self.emit_line(format!("{} {} = REINTERPRET_CAST({}, {});", tuple_item_type_name, item_handle, temp_handle, tuple_item_type_name));
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
                let mut expr_handle = expr_handle.clone();
                let mut idx = 0;
                for (_, (pattern, is_splat)) in patterns.iter().enumerate() {
                    match pattern {
                        BindingPattern::Variable(tok) => {
                            let var_name = Token::get_ident_name(tok);
                            let var_handle = SSAHandle(var_name);

                            let dest_type_name = if *is_splat { &expr_type_name } else { &item_type_name };
                            self.compile_opt_chaining(&var_handle, &dest_type_name, &expr_handle, |zelf| {
                                let handle = zelf.next_ssa_handle();
                                zelf.emit_line(if *is_splat {
                                    if *is_string {
                                        format!("AbraString {} = AbraString_get_range(REINTERPRET_CAST({}, AbraString), {}, REINTERPRET_CAST({}, AbraString).value->length);", handle, &expr_handle, idx, &expr_handle)
                                    } else {
                                        format!("AbraArray {} = AbraArray_get_range(REINTERPRET_CAST({}, AbraArray), {}, REINTERPRET_CAST({}, AbraArray).value->length);", handle, &expr_handle, idx, &expr_handle)
                                    }
                                } else {
                                    if *is_string {
                                        format!("AbraString {} = AbraString_get(REINTERPRET_CAST({}, AbraString), {});", handle, &expr_handle, idx)
                                    } else {
                                        format!("AbraAny {} = AbraArray_get(REINTERPRET_CAST({}, AbraArray), {});", handle, &expr_handle, idx)
                                    }
                                });
                                handle
                            });

                            if *is_splat {
                                let tmp = self.next_ssa_handle();
                                let num_remaining = num_patterns - idx - 1;
                                if *is_string {
                                    self.emit_line(format!("AbraString {} = AbraString_slice({}, {}.value->length - {});", &tmp, &var_handle, &var_handle, num_remaining));
                                } else {
                                    self.emit_line(format!("AbraArray {} = AbraArray_slice({}, {}.value->length - {});", &tmp, &var_handle, &var_handle, num_remaining));
                                }
                                expr_handle = tmp;
                                idx = 0;
                                continue;
                            }
                        }
                        _ => {
                            let item_handle = self.next_ssa_handle();
                            self.compile_opt_chaining(&item_handle, &item_type_name, &expr_handle, |zelf| {
                                let handle = zelf.next_ssa_handle();
                                zelf.emit_line(if *is_string {
                                    format!("AbraString {} = AbraString_get(REINTERPRET_CAST({}, AbraString), {});", handle, &expr_handle, idx)
                                } else {
                                    format!("AbraAny {} = AbraArray_get(REINTERPRET_CAST({}, AbraArray), {});", handle, &expr_handle, idx)
                                });
                                handle
                            });
                            self.compile_pattern_destructuring(project, &item_handle, &item_type_id, pattern, vars);
                        }
                    }

                    idx += 1;
                }
            }
        }
    }

    fn compile_opt_chaining<F>(&mut self, var_handle: &SSAHandle, var_type_name: &String, target_handle: &SSAHandle, continuation: F)
        where F: Fn(&mut CCompiler2<W>) -> SSAHandle
    {
        self.emit_line(format!("{} {};", var_type_name, var_handle));
        self.emit_line(format!("if (IS_NONE({})) {{", target_handle));
        self.emit_line(format!("  {} = REINTERPRET_CAST(AbraNone, {});", var_handle, var_type_name));
        self.emit_line("} else {");

        let continuation_handle = continuation(self);
        self.emit_line(format!("  {} = REINTERPRET_CAST({}, {});", var_handle, continuation_handle, var_type_name));
        self.emit_line("}");
    }

    fn compile_expression(&mut self, project: &Project, node: &TypedNode) -> SSAHandle {
        match node {
            TypedNode::Literal { value, .. } => {
                let handle = self.next_ssa_handle();
                let expr = match value {
                    TypedLiteral::Int(i) => format!("AbraInt_make({})", i),
                    TypedLiteral::Float(f) => format!("AbraFloat_make({})", f),
                    TypedLiteral::Bool(b) => format!("AbraBool_make({})", b),
                    TypedLiteral::String(s) => format!("AbraString_make({}, \"{}\")", s.len(), s),
                };
                self.emit_line(format!("{} {} = {};", self.get_type_name_by_id(project, node.type_id()), handle, expr));
                handle
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
                    (UnaryOp::Minus, expr) => format!("-({}).value", self.compile_expression(project, expr)),
                    (UnaryOp::Negate, expr) => format!("!({}).value", self.compile_expression(project, expr)),
                };

                let handle = self.next_ssa_handle();
                self.emit_line(format!("{} {} = {}({});", self.get_type_name_by_id(project, &type_id), handle, fn_name, fn_arg));
                handle
            }
            TypedNode::Binary { left, right, op, .. } => {
                let type_id = *node.type_id();

                let left_handle = self.compile_expression(project, left);
                if *op == BinaryOp::And || *op == BinaryOp::Or || *op == BinaryOp::Coalesce {
                    let handle = self.next_ssa_handle();

                    if *op == BinaryOp::And {
                        self.emit_line(format!("AbraBool {};", handle));
                        self.emit_line(format!("if (!{}.value) {{", left_handle));
                        self.emit_line(format!("{} = {};", handle, left_handle));
                        self.emit_line("} else {");
                    } else if *op == BinaryOp::Or {
                        self.emit_line(format!("AbraBool {};", handle));
                        self.emit_line(format!("if ({}.value) {{", left_handle));
                        self.emit_line(format!("{} = {};", handle, left_handle));
                        self.emit_line("} else {");
                    } else if *op == BinaryOp::Coalesce {
                        let type_name = self.get_type_name_by_id(project, &type_id);
                        self.emit_line(format!("{} {} = REINTERPRET_CAST({}, {});", type_name, handle, left_handle, type_name));
                        self.emit_line(format!("if (IS_NONE({})) {{", handle));
                    } else {
                        unreachable!()
                    }

                    let right_handle = self.compile_expression(project, right);
                    self.emit_line(format!("{} = {};", handle, right_handle));
                    self.emit_line("}");

                    return handle;
                }

                let right_handle = self.compile_expression(project, right);

                let compile_arithmetic_op = |op: &str| {
                    if type_id == PRELUDE_INT_TYPE_ID {
                        format!("AbraInt_make({}.value {} {}.value)", left_handle, op, right_handle)
                    } else if type_id == PRELUDE_FLOAT_TYPE_ID {
                        format!("AbraFloat_make((double){}.value {} (double){}.value)", left_handle, op, right_handle)
                    } else {
                        unreachable!("No other resultant types are possible for the {} operation", op);
                    }
                };

                let compile_comparison_op = |op: &str| {
                    format!("AbraBool_make({}.value {} {}.value)", left_handle, op, right_handle)
                };

                let value = match op {
                    BinaryOp::Add => {
                        if type_id == PRELUDE_STRING_TYPE_ID {
                            if *left.type_id() == PRELUDE_STRING_TYPE_ID {
                                format!("AbraString__concat(2, {}, REINTERPRET_CAST({}, AbraAny))", left_handle, right_handle)
                            } else if *right.type_id() == PRELUDE_STRING_TYPE_ID {
                                format!("AbraString__concat(2, prelude__tostring(REINTERPRET_CAST({}, AbraAny)), REINTERPRET_CAST({}, AbraAny))", left_handle, right_handle)
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
                            format!("AbraFloat_make(fmod((double){}.value, (double){}.value))", left_handle, right_handle)
                        } else {
                            format!("AbraInt_make({}.value % {}.value)", left_handle, right_handle)
                        }
                    }
                    BinaryOp::Pow => {
                        format!("AbraFloat_make(pow((double){}.value, (double){}.value))", left_handle, right_handle)
                    }
                    BinaryOp::And | BinaryOp::Or | BinaryOp::Coalesce => unreachable!("Handled above"),
                    BinaryOp::Xor => {
                        format!("AbraBool_make(!({}.value) != !({}.value))", left_handle, right_handle)
                    }
                    BinaryOp::Lt => compile_comparison_op("<"),
                    BinaryOp::Lte => compile_comparison_op("<="),
                    BinaryOp::Gt => compile_comparison_op(">"),
                    BinaryOp::Gte => compile_comparison_op(">="),
                    BinaryOp::Neq | BinaryOp::Eq => {
                        format!("prelude__eq(REINTERPRET_CAST({}, AbraAny), REINTERPRET_CAST({}, AbraAny), {})", left_handle, right_handle, *op == BinaryOp::Neq)
                    }
                    BinaryOp::AddEq | BinaryOp::SubEq | BinaryOp::MulEq | BinaryOp::DivEq | BinaryOp::ModEq | BinaryOp::AndEq | BinaryOp::OrEq | BinaryOp::CoalesceEq => {
                        unreachable!("Assignment operators expressions are pre-transformed")
                    }
                    _ => unimplemented!()
                };

                let handle = self.next_ssa_handle();
                self.emit_line(format!("{} {} = {};", self.get_type_name_by_id(project, &type_id), handle, value));
                handle
            }
            TypedNode::Grouped { expr, .. } => self.compile_expression(project, expr),
            TypedNode::Array { items, .. } => {
                let capacity = if items.is_empty() { 0 } else { items.len().next_power_of_two() };

                let handle = self.next_ssa_handle();

                self.emit_line(format!("AbraArray {} = AbraArray_make_with_capacity(/*length:*/ {}, /*capacity:*/{});", handle, items.len(), capacity));
                for (idx, item) in items.iter().enumerate() {
                    let expr_handle = self.compile_expression(project, &item);
                    self.emit_line(format!("AbraArray_set(/*self:*/{}, /*idx:*/{}, /*item:*/REINTERPRET_CAST({}, AbraAny));", handle, idx, expr_handle));
                }

                handle
            }
            TypedNode::Tuple { items, .. } => {
                let item_handles = items.iter().map(|item| self.compile_expression(project, item)).join(", ");
                let handle = self.next_ssa_handle();
                self.emit_line(format!("AbraTuple {} = AbraTuple_make({}, {});", handle, items.len(), item_handles));
                handle
            }
            TypedNode::Set { items, .. } => {
                let handle = self.next_ssa_handle();
                self.emit_line(format!("AbraSet {} = AbraSet_make();", handle));
                for item in items {
                    let item_handle = self.compile_expression(project, item);
                    self.emit_line(format!("AbraSet_insert({}, REINTERPRET_CAST({}, AbraAny));", handle, item_handle));
                }

                handle
            }
            TypedNode::Map { items, .. } => {
                let handle = self.next_ssa_handle();
                self.emit_line(format!("AbraMap {} = AbraMap_make();", handle));
                for (key, value) in items {
                    let key_handle = self.compile_expression(project, key);
                    let value_handle = self.compile_expression(project, value);
                    self.emit_line(format!("AbraMap_set({}, REINTERPRET_CAST({}, AbraAny), REINTERPRET_CAST({}, AbraAny));", handle, key_handle, value_handle));
                }

                handle
            }
            TypedNode::Identifier { var_id, .. } => {
                let variable = project.get_var_by_id(var_id);
                match variable.alias {
                    VariableAlias::None => {
                        let handle = if variable.is_captured {
                            if self.current_fn.map(|func_id| project.get_func_by_id(&func_id).captured_vars.contains(var_id)).unwrap_or(false) {
                                format!("(*{})", &variable.name)
                            } else {
                                format!("(*{}_ref)", &variable.name)
                            }
                        } else {
                            variable.name.clone()
                        };
                        SSAHandle(handle)
                    }
                    VariableAlias::Function(func_id) => {
                        // If a function is being referenced by identifier, it could be for the purpose of invocation, or simply for
                        // passing around a function as a value. If the former, the underlying c function will simply be invoked (see
                        // implementation in the TypedNode::Invocation compilation step); if the latter, then we need to wrap up the
                        // c function in an AbraFn so it can be passed around as a value.
                        self.wrap_function(project, &func_id, None)
                    }
                    VariableAlias::Type(_) => unimplemented!(),
                }
            }
            TypedNode::NoneValue { .. } => SSAHandle("AbraNone".to_string()),
            TypedNode::Invocation { target, arguments, type_id, .. } => {
                let target_type_id = target.as_ref().type_id();
                let ty = project.get_type_by_id(&target_type_id);
                let mut instantiation_struct_id = None;
                let (parameter_type_ids, return_type_id) = match ty {
                    Type::Function(parameter_type_ids, _, _, return_type_id) => (parameter_type_ids.clone(), return_type_id),
                    Type::Type(TypeKind::Struct(struct_id)) => {
                        let struct_ = project.get_struct_by_id(struct_id);
                        instantiation_struct_id = Some(*struct_id);
                        let ctor_fn_parameter_type_ids = struct_.fields.iter().map(|f| f.type_id).collect_vec();
                        (ctor_fn_parameter_type_ids, target_type_id)
                    }
                    _ => unimplemented!()
                };
                debug_assert!(parameter_type_ids.len() == arguments.len());
                let return_type_name = self.get_type_name_by_id(project, return_type_id);
                let arg_types = parameter_type_ids.iter().map(|param_type_id| {
                    format!("{}", self.get_type_name_by_id(project, param_type_id))
                }).collect_vec();

                let num_arguments = arguments.len();
                let mut arg_values = Vec::with_capacity(num_arguments);
                for (argument, parameter_type_id) in arguments.iter().zip(&parameter_type_ids) {
                    let handle = if let Some(argument) = argument {
                        self.compile_expression(project, &argument).0
                    } else {
                        format!("REINTERPRET_CAST(AbraNone, {})", self.get_type_name_by_id(project, &parameter_type_id))
                    };
                    arg_values.push(handle);
                }

                if let Some(struct_id) = instantiation_struct_id {
                    let handle = self.next_ssa_handle();
                    let type_name = self.get_type_name_by_id(project, type_id);
                    self.emit_line(format!("{} {} = {}({});", type_name, &handle, self.get_ctor_fn_name(project, &struct_id), arg_values.join(",")));
                    return handle;
                }

                match &**target {
                    TypedNode::Identifier { var_id, .. } => {
                        let variable = project.get_var_by_id(var_id);
                        if let VariableAlias::Function(func_id) = &variable.alias {
                            let handle = if *type_id != PRELUDE_UNIT_TYPE_ID {
                                let handle = self.next_ssa_handle();
                                self.emit(format!("{} {} = ", self.get_type_name_by_id(project, type_id), handle));
                                Some(handle)
                            } else {
                                None
                            };

                            let function = project.get_func_by_id(func_id);
                            if !function.captured_vars.is_empty() {
                                self.emit(format!("ABRA_CL_CALL_{}({}, ", function.params.len(), self.closure_name(project, function, None)));
                                self.emit(vec![num_arguments.to_string(), return_type_name]
                                    .into_iter()
                                    .chain(arg_values.iter().zip(arg_types).map(|(v, ty)| format!("{ty}, {v}")))
                                    .join(", ")
                                );
                            } else {
                                self.emit(format!("{}(", self.function_name(project, function, None)));
                                self.emit(vec![num_arguments.to_string()].into_iter().chain(arg_values).join(", "));
                            }
                            self.emit_line(");");

                            return handle.unwrap_or_default();
                        }
                    }
                    TypedNode::Accessor { target, kind, member_idx, is_opt_safe, .. } => {
                        debug_assert!(!*is_opt_safe, "todo");

                        let target_type_id = target.type_id();
                        let target_type = project.get_type_by_id(target_type_id);
                        let target_handle = self.compile_expression(project, target);

                        let func_id = match kind {
                            AccessorKind::Field => unreachable!(),
                            AccessorKind::Method => target_type.get_method(project, *member_idx),
                            AccessorKind::StaticMethod |
                            AccessorKind::EnumVariant => todo!()
                        };
                        let Some(func_id) = func_id else { unreachable!() };
                        let function = project.get_func_by_id(&func_id);
                        let return_type_id = function.return_type_id;

                        let result_handle = if return_type_id != PRELUDE_UNIT_TYPE_ID {
                            let handle = self.next_ssa_handle();
                            self.emit(format!("{} {} = ", self.get_type_name_by_id(project, &return_type_id), handle));
                            Some(handle)
                        } else {
                            None
                        };

                        if !(function.captured_vars.is_empty()) {
                            self.emit(format!("ABRA_CL_CALL_{}({}, ", function.params.len(), self.closure_name(project, function, Some(target_type))));
                            // self.emit(vec![(num_arguments + 1).to_string(), return_type_name, target_handle.0].into_iter().chain(arg_values).join(", "));
                            self.emit(vec![num_arguments.to_string(), return_type_name, self.get_type_name_by_id(project, target_type_id), target_handle.0]
                                .into_iter()
                                .chain(arg_values.iter().zip(arg_types).map(|(v, ty)| format!("{ty}, {v}")))
                                .join(", ")
                            );
                        } else {
                            self.emit(format!("{}(", self.method_name(project, target_type, &function.name)));
                            self.emit(vec![(num_arguments + 1).to_string(), target_handle.0].into_iter().chain(arg_values).join(", "));
                        }
                        self.emit_line(");");

                        return result_handle.unwrap_or_default();
                    }
                    _ => {}
                }

                let target_handle = self.compile_expression(project, &*target);

                let handle = if *type_id != PRELUDE_UNIT_TYPE_ID {
                    let handle = self.next_ssa_handle();
                    self.emit(format!("{} {} = ", self.get_type_name_by_id(project, type_id), handle));
                    Some(handle)
                } else {
                    None
                };
                self.emit(format!("ABRA_FN_CALL_{}(REINTERPRET_CAST({}, AbraFn), {}, ", num_arguments, target_handle, num_arguments));
                self.emit(vec![return_type_name].into_iter()
                    .chain(arg_values.iter().zip(arg_types).map(|(v, ty)| format!("{ty}, {v}")))
                    .join(",")
                );
                self.emit_line(");");

                handle.unwrap_or_default()
            }
            TypedNode::Accessor { type_id, target, kind, is_opt_safe, member_idx, .. } => {
                debug_assert!(!*is_opt_safe, "todo");

                let result_type_name = self.get_type_name_by_id(project, type_id);
                let target_type_id = target.type_id();
                let target_type = project.get_type_by_id(&target_type_id);
                let target_handle = self.compile_expression(project, target);
                let result_handle = self.next_ssa_handle();
                match kind {
                    AccessorKind::Field => {
                        let is_builtin = match target_type {
                            Type::Primitive(_) => true,
                            Type::GenericInstance(struct_id, _) if *struct_id == project.prelude_array_struct_id => true,
                            Type::GenericInstance(struct_id, _) if *struct_id == project.prelude_map_struct_id => true,
                            Type::GenericInstance(struct_id, _) if *struct_id == project.prelude_set_struct_id => true,
                            _ => false
                        };
                        let Some(field) = target_type.get_field(project, *member_idx) else { unreachable!() };
                        if is_builtin {
                            self.emit_line(format!("{} {} = {}_field_{}({});", &result_type_name, &result_handle, self.get_type_name_by_id(project, target_type_id), &field.name, &target_handle));
                        } else {
                            self.emit_line(format!("{} {} = {}.value->{};", &result_type_name, &result_handle, &target_handle, &field.name))
                        }
                    }
                    AccessorKind::Method |
                    AccessorKind::StaticMethod |
                    AccessorKind::EnumVariant => todo!()
                }

                result_handle
            }
            TypedNode::Indexing { target, index, type_id, .. } => {
                let target_type_id = target.as_ref().type_id();
                let target_type = project.get_type_by_id(target_type_id);

                let target_handle = self.compile_expression(project, target);

                let (fn_name, fn_return_type_name) = match (target_type, &index) {
                    (Type::Primitive(PrimitiveType::String), IndexingMode::Index(_)) => ("AbraString_get", "AbraString"),
                    (Type::Primitive(PrimitiveType::String), IndexingMode::Range(_, _)) => ("AbraString_get_range", "AbraString"),
                    (Type::GenericInstance(struct_id, _), IndexingMode::Index(_)) if *struct_id == project.prelude_array_struct_id => ("AbraArray_get", "AbraAny"),
                    (Type::GenericInstance(struct_id, _), IndexingMode::Range(_, _)) if *struct_id == project.prelude_array_struct_id => ("AbraArray_get_range", "AbraArray"),
                    (Type::GenericInstance(struct_id, _), IndexingMode::Index(_)) if *struct_id == project.prelude_tuple_struct_id => ("AbraTuple_get", "AbraAny"),
                    (Type::GenericInstance(struct_id, _), IndexingMode::Index(_)) if *struct_id == project.prelude_map_struct_id => ("AbraMap_get", "AbraAny"),
                    _ => unimplemented!(),
                };

                let fn_args = match index {
                    IndexingMode::Index(idx_expr) => {
                        let idx_expr_handle = self.compile_expression(project, idx_expr);
                        match target_type {
                            Type::Primitive(PrimitiveType::String) => format!("{}.value", idx_expr_handle),
                            Type::GenericInstance(struct_id, _) if *struct_id == project.prelude_array_struct_id => format!("{}.value", idx_expr_handle),
                            Type::GenericInstance(struct_id, _) if *struct_id == project.prelude_tuple_struct_id => format!("{}.value", idx_expr_handle),
                            Type::GenericInstance(struct_id, _) if *struct_id == project.prelude_map_struct_id => format!("REINTERPRET_CAST({}, AbraAny)", idx_expr_handle),
                            _ => idx_expr_handle.0
                        }
                    }
                    IndexingMode::Range(start_expr, end_expr) => {
                        match (start_expr, end_expr) {
                            (Some(start_expr), Some(end_expr)) => {
                                let start_expr_handle = self.compile_expression(project, start_expr);
                                let end_expr_handle = self.compile_expression(project, end_expr);
                                format!("{}.value, {}.value", start_expr_handle, end_expr_handle)
                            }
                            (None, Some(end_expr)) => {
                                let end_expr_handle = self.compile_expression(project, end_expr);
                                format!("0, {}.value", end_expr_handle)
                            }
                            (Some(start_expr), None) => {
                                let start_expr_handle = self.compile_expression(project, start_expr);
                                format!("{}.value, {}.value->length", start_expr_handle, target_handle)
                            }
                            (None, None) => unreachable!("foo[:] is invalid syntax at the moment")
                        }
                    }
                };

                let temp_handle = self.next_ssa_handle();
                self.emit_line(format!("{} {} = {}({}, {});", fn_return_type_name, temp_handle, fn_name, target_handle, fn_args));
                let handle = self.next_ssa_handle();
                let type_name = self.get_type_name_by_id(project, type_id);
                self.emit_line(format!("{} {} = REINTERPRET_CAST({}, {});", type_name, handle, temp_handle, type_name));

                handle
            }
            TypedNode::Lambda { func_id, .. } => {
                self.wrap_function(project, func_id, None)
            }
            TypedNode::Assignment { kind, expr, .. } => {
                let expr_handle = self.compile_expression(project, expr);
                match kind {
                    AssignmentKind::Identifier { var_id } => {
                        let variable = project.get_var_by_id(var_id);
                        if variable.is_captured {
                            if self.current_fn.map(|func_id| project.get_func_by_id(&func_id).captured_vars.contains(var_id)).unwrap_or(false) {
                                self.emit_line(format!("{}->value = ({}).value;", &variable.name, expr_handle));
                            } else {
                                self.emit_line(format!("{}_ref->value = ({}).value;", &variable.name, expr_handle));
                            }
                        } else {
                            self.emit_line(format!("{} = {};", &variable.name, expr_handle));
                        }
                        SSAHandle(variable.name.clone())
                    }
                    AssignmentKind::Accessor { .. } => todo!(),
                    AssignmentKind::Indexing { index, target } => {
                        let target_type_id = target.as_ref().type_id();
                        let target_type = project.get_type_by_id(target_type_id);
                        let Type::GenericInstance(struct_id, _) = target_type else { unreachable!() };

                        let target_handle = self.compile_expression(project, target);
                        let index_handle = self.compile_expression(project, index);

                        if *struct_id == project.prelude_array_struct_id {
                            self.emit_line(format!("AbraArray_set(/*self:*/{}, /*idx:*/{}.value, /*item:*/REINTERPRET_CAST({}, AbraAny));", target_handle, index_handle, expr_handle));
                        } else if *struct_id == project.prelude_map_struct_id {
                            self.emit_line(format!("AbraMap_set(/*self:*/{}, /*key:*/REINTERPRET_CAST({}, AbraAny), /*value:*/REINTERPRET_CAST({}, AbraAny));", target_handle, index_handle, expr_handle));
                        } else {
                            unreachable!("No other types are indexable")
                        }
                        expr_handle
                    }
                }
            }
            n => unreachable!("Internal error: node is not an expression: {:?}", n),
        }
    }

    fn wrap_function(&mut self, project: &Project, func_id: &FuncId, container_type: Option<&Type>) -> SSAHandle {
        let function = project.get_func_by_id(func_id);
        let (opt_params, req_params): (Vec<_>, Vec<_>) = function.params.iter().partition(|p| p.default_value.is_some());
        let min_arity = req_params.len();
        let max_arity = opt_params.len() + req_params.len();

        let handle = self.next_ssa_handle();
        let fn_name = self.function_name(project, function, container_type);
        if !function.captured_vars.is_empty() {
            let num_captures = function.captured_vars.len();
            self.emit_line(format!("AbraFn {} = AbraFn_make_closure((Fn)&{}, {}, {}, {});", &handle, fn_name, min_arity, max_arity, num_captures));
            for (idx, var_id) in function.captured_vars.iter().enumerate() {
                let variable = project.get_var_by_id(var_id);
                self.emit_line(format!("{}.value->captures[{}] = (AbraAny*){}_ref;", &handle, &idx, &variable.name));
            }
            return handle;
        };

        self.emit_line(format!("AbraFn {} = AbraFn_make((Fn)&{}, {}, {});", &handle, fn_name, min_arity, max_arity));
        handle
    }
}

// #[cfg(test)]
// mod test {
//     use std::process::Command;
//     use assert_cmd::cargo::CommandCargoExt;
//     use itertools::{EitherOrBoth, Itertools};
//     use crate::transpile::get_project_root::get_project_root;
//
//     fn run_test_file(file_name: &str) {
//         let rust_project_root = get_project_root().unwrap();
//
//         let test_file_path = rust_project_root.join("abra_core").join("src").join("transpile").join("testv2").join(file_name);
//         let test_file = std::fs::read_to_string(&test_file_path).unwrap();
//
//         let mut cmd = Command::cargo_bin("abra").unwrap();
//         cmd.arg("compile2").arg(&test_file_path);
//         let output = cmd.output().unwrap();
//         assert!(output.stderr.is_empty(), "Compilation error: {}", String::from_utf8(output.stderr).unwrap());
//
//         let output = String::from_utf8(output.stdout).unwrap();
//         let output = output.lines();
//
//         let prefix = "/// Expect: ";
//         let expectations = test_file.lines()
//             .map(|line| line.trim())
//             .enumerate()
//             .filter(|(_, line)| line.starts_with(prefix))
//             .map(|(line_num, line)| (line_num + 1, line.replace(prefix, "")))
//             .collect_vec();
//
//         for pair in expectations.iter().zip_longest(output) {
//             match pair {
//                 EitherOrBoth::Both((line_num, expected), actual) => {
//                     assert_eq!(expected, actual, "Expectation mismatch at {}:{}", test_file_path.to_str().unwrap(), line_num);
//                 }
//                 EitherOrBoth::Left((line_num, expected)) => {
//                     assert!(false, "Expected: {} (line {}), but reached end of output", expected, line_num);
//                 }
//                 EitherOrBoth::Right(actual) => {
//                     assert!(false, "Received line: {}, but there were no more expectations", actual);
//                 }
//             }
//         }
//     }
//
//     #[test]
//     fn builtin_values() {
//         run_test_file("builtinValues.abra");
//     }
//
//     #[test]
//     fn unary_ops() {
//         run_test_file("unaryOps.abra");
//     }
//
//     #[test]
//     fn binary_ops() {
//         run_test_file("binaryOps.abra");
//     }
//
//     #[test]
//     fn variable_declaration() {
//         run_test_file("variableDeclaration.abra");
//     }
//
//     #[test]
//     fn assignment() {
//         run_test_file("assignment.abra");
//     }
//
//     #[test]
//     fn functions() {
//         run_test_file("functions.abra");
//     }
//
//     #[test]
//     fn lambdas() {
//         run_test_file("lambdas.abra");
//     }
//
//     #[test]
//     fn closures() {
//         run_test_file("closures.abra");
//     }
//
//     #[test]
//     fn strings() {
//         run_test_file("strings.abra");
//     }
//
//     #[test]
//     fn arrays() {
//         run_test_file("arrays.abra");
//     }
//
//     #[test]
//     fn maps() {
//         run_test_file("maps.abra");
//     }
//
//     #[test]
//     fn sets() {
//         run_test_file("sets.abra");
//     }
//
//     #[test]
//     fn types() {
//         run_test_file("types.abra");
//     }
// }
