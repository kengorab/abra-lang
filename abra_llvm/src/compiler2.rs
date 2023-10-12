use std::path::PathBuf;
use std::process::{Command, ExitStatus};
use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, FloatType, FunctionType, IntType, PointerType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue};
use itertools::Itertools;
use abra_core::typechecker::typechecker2::{ModuleId, PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_MODULE_ID, PRELUDE_STRING_TYPE_ID, PRELUDE_UNIT_TYPE_ID, PrimitiveType, Project, ScopeId, Type, TypedLiteral, TypedNode, TypeId};

const STRING_NEW_FN_NAME: &str = "String.new(Ptr<UInt8>,Int32):String";

pub struct LLVMCompiler2<'a> {
    project: &'a Project,
    context: &'a Context,
    builder: Builder<'a>,
    main_module: Module<'a>,
    abra_main_fn: FunctionValue<'a>,

    // cached for convenience
    string_type: StructType<'a>,
}

impl<'a> LLVMCompiler2<'a> {
    pub fn compile(project: &Project, out_dir: &PathBuf, out_file_name: Option<String>) -> PathBuf {
        let context = Context::create();
        let compiler = LLVMCompiler2::new(project, &context);
        compiler.generate(project);

        let out_name = out_file_name.unwrap_or("main".into());

        let llvm_module_out_file = out_dir.join(format!("_{}.ll", &out_name));
        compiler.main_module.print_to_file(&llvm_module_out_file).unwrap();
        // eprintln!("Wrote to file {}", llvm_module_out_file.to_str().unwrap());
        // eprintln!("File contents: {}", std::fs::read_to_string(&llvm_module_out_file).unwrap());

        let exec_out_file = out_dir.join(&out_name);
        let cc_output = Command::new("cc")
            .arg(&llvm_module_out_file)
            .arg("-o")
            .arg(&exec_out_file)
            .arg("-Wno-override-module")
            .output()
            .unwrap();
        if !cc_output.stderr.is_empty() {
            eprintln!("{}", String::from_utf8(cc_output.stderr).unwrap());
        }
        if !cc_output.stdout.is_empty() {
            print!("{}", String::from_utf8(cc_output.stdout).unwrap());
        }

        exec_out_file
    }

    pub fn compile_and_run(project: &Project, out_dir: &PathBuf, out_file_name: Option<String>) -> ExitStatus {
        let exec_out_file = Self::compile(project, out_dir, out_file_name);

        let run_output = Command::new(&exec_out_file).output().unwrap();
        if !run_output.stderr.is_empty() {
            eprintln!("Error: {}", String::from_utf8(run_output.stderr).unwrap());
        }
        if !run_output.stdout.is_empty() {
            print!("{}", String::from_utf8(run_output.stdout).unwrap());
        }
        run_output.status
    }

    fn new(project: &'a Project, context: &'a Context) -> Self {
        let builder = context.create_builder();
        let main_module = context.create_module("__main");

        let triple = TargetMachine::get_default_triple();
        main_module.set_triple(&triple);

        let abra_main_fn = Self::start_abra_main(&context, &main_module, &builder);

        let string_type = context.opaque_struct_type("String");
        string_type.set_body(&[
            context.i32_type().into(), // type_id
            // context.i32_type().into(), // bytesize
            context.i32_type().into(), // length
            context.i8_type().ptr_type(AddressSpace::Generic).into(), // bytes
        ], false);

        Self {
            project,
            context,
            builder,
            main_module,
            abra_main_fn,
            string_type,
        }
    }

    // LLVM UTILS START

    fn bool(&self) -> IntType<'a> {
        self.context.bool_type()
    }

    fn const_bool(&self, value: bool) -> IntValue<'a> {
        self.bool().const_int(if value { 1 } else { 0 }, false)
    }

    fn i8(&self) -> IntType<'a> {
        self.context.i8_type()
    }

    fn i32(&self) -> IntType<'a> {
        self.context.i32_type()
    }

    fn const_i32(&self, value: u64) -> IntValue<'a> {
        self.i32().const_int(value, false)
    }

    fn i64(&self) -> IntType<'a> {
        self.context.i64_type()
    }

    fn const_i64(&self, value: u64) -> IntValue<'a> {
        self.i64().const_int(value, false)
    }

    fn f64(&self) -> FloatType<'a> {
        self.context.f64_type()
    }

    fn const_f64(&self, value: f64) -> FloatValue<'a> {
        self.f64().const_float(value)
    }

    fn ptr<T: BasicType<'a>>(&self, t: T) -> PointerType<'a> {
        t.ptr_type(AddressSpace::Generic)
    }

    fn null_ptr(&self) -> PointerValue<'a> {
        self.ptr(self.i8()).const_null()
    }

    fn fn_type<T: BasicType<'a>>(&self, ret: T, param_types: &[BasicMetadataTypeEnum<'a>]) -> FunctionType<'a> {
        ret.fn_type(param_types, false)
    }

    fn fn_type_variadic<T: BasicType<'a>>(&self, ret: T, param_types: &[BasicMetadataTypeEnum<'a>]) -> FunctionType<'a> {
        ret.fn_type(param_types, true)
    }

    fn sizeof_struct<T: BasicType<'a>>(&self, t: T) -> IntValue<'a> {
        let sizeof_struct = self.builder.build_struct_gep(self.null_ptr().const_cast(self.ptr(t)), 1, "").unwrap();
        self.builder.build_ptr_to_int(sizeof_struct, self.i64(), "")
    }

    fn llvm_type_name_by_id(&self, type_id: &TypeId) -> String {
        let TypeId(ScopeId(ModuleId(m_id), s_id), _) = type_id;
        let prefix = if *m_id == PRELUDE_MODULE_ID.0 {
            "".into()
        } else {
            format!("{}.{}.", m_id, s_id)
        };

        let ty = self.project.get_type_by_id(type_id);
        let name = match ty {
            Type::Primitive(PrimitiveType::Unit) => "Unit".into(),
            Type::Primitive(PrimitiveType::Any) => "Any".into(),
            Type::Primitive(PrimitiveType::Int) => "Int".into(),
            Type::Primitive(PrimitiveType::Float) => "Float".into(),
            Type::Primitive(PrimitiveType::Bool) => "Bool".into(),
            Type::Primitive(PrimitiveType::String) => "String".into(),
            Type::Generic(_, _) => todo!(),
            Type::GenericInstance(struct_id, generic_ids) => {
                let struct_ = self.project.get_struct_by_id(struct_id);
                let generic_names = generic_ids.iter().map(|type_id| self.llvm_type_name_by_id(type_id)).join(",");
                format!("{}<{}>", &struct_.name, generic_names)
            }
            Type::GenericEnumInstance(_, _, _) |
            Type::Function(_, _, _, _) |
            Type::Type(_) |
            Type::ModuleAlias => todo!()
        };

        format!("{}{}", prefix, name)
    }

    fn llvm_function_name<S: AsRef<str>>(&self, name: S, container_type: Option<(&TypeId, /* is_method: */ bool)>) -> String {
        if let Some((container_type_id, is_method)) = container_type {
            let type_name = self.llvm_type_name_by_id(container_type_id);
            let ty = self.project.get_type_by_id(container_type_id);
            let (func_id, infix) = if is_method {
                (ty.find_method_by_name(self.project, name.as_ref()), "#")
            } else {
                (ty.find_static_method_by_name(self.project, name.as_ref()), ".")
            };
            let func_id = func_id.expect(&format!("Method {} on type {} expected to exist", name.as_ref(), type_name));
            let function = self.project.get_func_by_id(func_id);
            let function_name = &function.name;

            let params = function.params.iter().skip(if function.has_self() { 1 } else { 0 })
                .map(|p| self.llvm_type_name_by_id(&p.type_id))
                .join(",");
            let ret_type_name = self.llvm_type_name_by_id(&function.return_type_id);

            format!("{type_name}{infix}{function_name}({params}):{ret_type_name}")
        } else {
            todo!()
        }
    }

    // LLVM UTILS END

    fn start_abra_main(context: &'a Context, main_module: &Module<'a>, builder: &Builder<'a>) -> FunctionValue<'a> {
        let argc_t = context.i32_type();
        let argv_t = context.i8_type().ptr_type(AddressSpace::Generic).ptr_type(AddressSpace::Generic);
        let fn_type = context.void_type().fn_type(&[argc_t.into(), argv_t.into()], false);
        let abra_main_fn = main_module.add_function("_abra_main", fn_type, None);
        let block = context.append_basic_block(abra_main_fn, "");
        builder.position_at_end(block);

        abra_main_fn
    }

    fn end_abra_main(&self) {
        let b = self.abra_main_fn.get_last_basic_block().expect("abra main is guaranteed to have at least 1 block");
        self.builder.position_at_end(b);
        self.builder.build_return(None);
    }

    fn build_main_fn(&self) {
        let argc_t = self.i32();
        let argv_t = self.ptr(self.ptr(self.i8()));
        let main_fn_type = self.fn_type(self.i32(), &[argc_t.into(), argv_t.into()]);
        let entry_fn = self.main_module.add_function("main", main_fn_type, None);
        let block = self.context.append_basic_block(entry_fn, "");
        self.builder.position_at_end(block);
        self.builder.build_call(self.abra_main_fn, &[entry_fn.get_nth_param(0).unwrap().into(), entry_fn.get_nth_param(1).unwrap().into()], "");
        self.builder.build_return(Some(&self.const_i32(0).as_basic_value_enum()));
    }

    pub fn generate(&self, project: &Project) {
        self.generate_prelude();
        self.build_main_fn();

        let printf = self.main_module.add_function("printf", self.fn_type_variadic(self.i64(), &[self.ptr(self.i8()).into()]), None);

        for m in &project.modules {
            if m.id == PRELUDE_MODULE_ID { continue; }

            // The top-level code in a module is executed in the special $mod_{mod_id} fn...
            let mod_fn_name = format!("$mod_{}", m.id.0);
            let mod_fn_type = self.fn_type(self.bool(), &[]);
            let mod_fn = self.main_module.add_function(&mod_fn_name, mod_fn_type, None);
            let block = self.context.append_basic_block(mod_fn, "");
            self.builder.position_at_end(block);

            let num_nodes = m.code.len();
            for (idx, node) in m.code.iter().enumerate() {
                let res = self.visit_statement(node);
                let node_type_id = node.type_id();
                if idx == num_nodes - 1 && *node_type_id != PRELUDE_UNIT_TYPE_ID {
                    if let Some(res) = res {
                        let to_string_fn_name = self.llvm_function_name("toString", Some((node_type_id, true)));
                        let to_string_fn = self.main_module.get_function(&to_string_fn_name).unwrap();

                        let str_val = self.builder.build_call(to_string_fn, &[res.into()], "repr").try_as_basic_value().left().unwrap().into_pointer_value();
                        let chars_val_ptr = self.builder.build_struct_gep(str_val.const_cast(self.ptr(self.string_type)), 2, "").unwrap();
                        let chars_val = self.builder.build_load(chars_val_ptr, "repr.chars");

                        let fmt_str = self.builder.build_global_string_ptr("%s", "").as_basic_value_enum();
                        self.builder.build_call(printf, &[fmt_str.into(), chars_val.into()], "").try_as_basic_value().left().unwrap();
                    }
                }
            }

            // ...which returns true/false depending on whether it succeeded. For now, it will always
            // succeed, but when `try` is introduced, top-level code may fail.
            self.builder.build_return(Some(&self.const_bool(true)));

            // Call the $mod_{mod_id} fn in the _abra_main fn.
            self.builder.position_at_end(self.abra_main_fn.get_last_basic_block().unwrap());
            self.builder.build_call(mod_fn, &[], "");
        }

        self.end_abra_main();
    }

    // TODO: This function is only for initial bootstrapping purposes and will be replaced by a real prelude implementation in Abra
    fn generate_prelude(&self) {
        let snprintf = self.main_module.add_function("snprintf", self.fn_type_variadic(self.i64(), &[self.ptr(self.i8()).into(), self.i32().into(), self.ptr(self.i8()).into()]), None);
        let malloc = self.main_module.add_function("malloc", self.fn_type(self.ptr(self.i8()), &[self.i64().into()]), None);

        // String.new(Ptr<UInt8>,Int32):String
        {
            let fn_type = self.fn_type(self.ptr(self.string_type), &[self.ptr(self.i8()).into(), self.i32().into()]);
            let func = self.main_module.add_function(STRING_NEW_FN_NAME, fn_type, None);
            let mut func_params_iter = func.get_param_iter();
            func_params_iter.next().unwrap().set_name("chars");
            func_params_iter.next().unwrap().set_name("length");

            let block = self.context.append_basic_block(func, "");
            self.builder.position_at_end(block);

            let sizeof_string = self.sizeof_struct(self.string_type);
            let str_mem = self.builder.build_pointer_cast(
                self.builder.build_call(malloc, &[sizeof_string.into()], "").try_as_basic_value().left().unwrap().into_pointer_value(),
                self.ptr(self.string_type),
                "str_mem",
            );
            let mem_cursor = self.builder.build_struct_gep(str_mem, 0, "String#type_id").unwrap();
            self.builder.build_store(mem_cursor, self.const_i32(self.project.prelude_string_struct_id.1 as u64));
            let mem_cursor = self.builder.build_struct_gep(str_mem, 1, "String#length").unwrap();
            self.builder.build_store(mem_cursor, func.get_nth_param(1).unwrap());
            let mem_cursor = self.builder.build_struct_gep(str_mem, 2, "String#chars").unwrap();
            self.builder.build_store(mem_cursor, func.get_nth_param(0).unwrap());

            self.builder.build_return(Some(&str_mem.as_basic_value_enum()));
        }

        // Int#toString():String
        {
            let fn_type = self.fn_type(self.ptr(self.string_type), &[self.i64().into()]);
            let fn_name = self.llvm_function_name("toString", Some((&PRELUDE_INT_TYPE_ID, true)));
            let func = self.main_module.add_function(&fn_name, fn_type, None);
            func.get_param_iter().next().unwrap().set_name("self");
            let block = self.context.append_basic_block(func, "");
            self.builder.position_at_end(block);
            let fmt_str = self.builder.build_global_string_ptr("%jd", "").as_basic_value_enum();
            let self_param = func.get_nth_param(0).unwrap();
            let len_val = self.builder.build_call(snprintf, &[self.null_ptr().into(), self.const_i32(0).into(), fmt_str.into(), self_param.into()], "len").try_as_basic_value().left().unwrap().into_int_value();
            let len_val = self.builder.build_int_cast(len_val, self.i32(), "");
            let len_plus_1 = self.builder.build_int_add::<IntValue<'a>>(len_val.into(), self.const_i32(1).into(), "len_plus_1");
            let str_val = self.builder.build_call(malloc, &[self.builder.build_int_cast(len_plus_1, self.i64(), "").into()], "str").try_as_basic_value().left().unwrap();
            self.builder.build_call(snprintf, &[str_val.into(), len_plus_1.into(), fmt_str.into(), self_param.into()], "").try_as_basic_value().left().unwrap();

            let ret_val = self.builder.build_call(
                self.main_module.get_function(STRING_NEW_FN_NAME).unwrap(),
                &[str_val.into(), len_val.into()],
                "",
            ).try_as_basic_value().left().unwrap();

            self.builder.build_return(Some(&ret_val));
        }

        // Float#toString():String
        {
            let fn_type = self.fn_type(self.ptr(self.string_type), &[self.f64().into()]);
            let fn_name = self.llvm_function_name("toString", Some((&PRELUDE_FLOAT_TYPE_ID, true)));
            let func = self.main_module.add_function(&fn_name, fn_type, None);
            func.get_param_iter().next().unwrap().set_name("self");
            let block = self.context.append_basic_block(func, "");
            self.builder.position_at_end(block);
            let fmt_str = self.builder.build_global_string_ptr("%g", "").as_basic_value_enum();
            let self_param = func.get_nth_param(0).unwrap();
            let len_val = self.builder.build_call(snprintf, &[self.null_ptr().into(), self.const_i32(0).into(), fmt_str.into(), self_param.into()], "len").try_as_basic_value().left().unwrap().into_int_value();
            let len_val = self.builder.build_int_cast(len_val, self.i32(), "");
            let len_plus_1 = self.builder.build_int_add::<IntValue<'a>>(len_val.into(), self.const_i32(1).into(), "len_plus_1");
            let str_val = self.builder.build_call(malloc, &[self.builder.build_int_cast(len_plus_1, self.i64(), "").into()], "str").try_as_basic_value().left().unwrap();
            self.builder.build_call(snprintf, &[str_val.into(), len_plus_1.into(), fmt_str.into(), self_param.into()], "").try_as_basic_value().left().unwrap();

            let ret_val = self.builder.build_call(
                self.main_module.get_function(STRING_NEW_FN_NAME).unwrap(),
                &[str_val.into(), len_val.into()],
                "",
            ).try_as_basic_value().left().unwrap();

            self.builder.build_return(Some(&ret_val));
        }

        // Bool#toString():String
        {
            let bool_true_str_global = self.main_module.add_global(self.string_type, None, "BOOL_TRUE_STR");
            bool_true_str_global.set_constant(true);
            let true_str = self.builder.build_global_string_ptr("true", "").as_basic_value_enum();
            bool_true_str_global.set_initializer(&self.context.const_struct(&[
                self.const_i32(self.project.prelude_string_struct_id.1 as u64).into(),
                self.const_i32(4).into(),
                true_str,
            ], false));
            let bool_false_str_global = self.main_module.add_global(self.string_type, None, "BOOL_FALSE_STR");
            bool_false_str_global.set_constant(true);
            let false_str = self.builder.build_global_string_ptr("false", "").as_basic_value_enum();
            bool_false_str_global.set_initializer(&self.context.const_struct(&[
                self.const_i32(self.project.prelude_string_struct_id.1 as u64).into(),
                self.const_i32(5).into(),
                false_str,
            ], false));

            let fn_type = self.fn_type(self.ptr(self.string_type), &[self.bool().into()]);
            let fn_name = self.llvm_function_name("toString", Some((&PRELUDE_BOOL_TYPE_ID, true)));
            let func = self.main_module.add_function(&fn_name, fn_type, None);
            func.get_param_iter().next().unwrap().set_name("self");
            let block = self.context.append_basic_block(func, "");
            self.builder.position_at_end(block);

            let cond = func.get_nth_param(0).unwrap().into_int_value();
            let then_bb = self.context.append_basic_block(func, "then");
            let else_bb = self.context.append_basic_block(func, "else");
            let cont_bb = self.context.append_basic_block(func, "cont");
            self.builder.build_conditional_branch(cond, then_bb, else_bb);

            self.builder.position_at_end(then_bb);
            let then_val = bool_true_str_global.as_pointer_value();
            self.builder.build_unconditional_branch(cont_bb);

            self.builder.position_at_end(else_bb);
            let else_val = bool_false_str_global.as_pointer_value();
            self.builder.build_unconditional_branch(cont_bb);

            self.builder.position_at_end(cont_bb);
            let phi = self.builder.build_phi(self.ptr(self.string_type), "");
            phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

            self.builder.build_return(Some(&phi.as_basic_value()));
        }

        // String#toString():String
        {
            let fn_type = self.fn_type(self.ptr(self.string_type), &[self.ptr(self.string_type).into()]);
            let fn_name = self.llvm_function_name("toString", Some((&PRELUDE_STRING_TYPE_ID, true)));
            let func = self.main_module.add_function(&fn_name, fn_type, None);
            func.get_param_iter().next().unwrap().set_name("self");
            let block = self.context.append_basic_block(func, "");
            self.builder.position_at_end(block);
            self.builder.build_return(Some(&func.get_nth_param(0).unwrap()));
        }
    }

    fn visit_statement(&self, node: &TypedNode) -> Option<BasicValueEnum<'a>> {
        match node {
            TypedNode::If { .. } |
            TypedNode::Match { .. } |
            TypedNode::FuncDeclaration(_) |
            TypedNode::TypeDeclaration(_) |
            TypedNode::EnumDeclaration(_) |
            TypedNode::BindingDeclaration { .. } |
            TypedNode::ForLoop { .. } |
            TypedNode::WhileLoop { .. } |
            TypedNode::Break { .. } |
            TypedNode::Continue { .. } |
            TypedNode::Return { .. } |
            TypedNode::Import { .. } => None,
            _ => Some(self.visit_expression(node)),
        }
    }

    fn visit_expression(&self, node: &TypedNode) -> BasicValueEnum<'a> {
        match node {
            TypedNode::Literal { value, .. } => {
                match value {
                    TypedLiteral::Int(v) => {
                        let int = self.const_i64((*v) as u64);
                        int.as_basic_value_enum()
                    }
                    TypedLiteral::Float(f) => {
                        let float = self.const_f64(*f);
                        float.as_basic_value_enum()
                    }
                    TypedLiteral::Bool(b) => {
                        let b = self.const_bool(*b);
                        b.as_basic_value_enum()
                    }
                    TypedLiteral::String(s) => {
                        let str = self.builder.build_global_string_ptr(&s, "").as_basic_value_enum();
                        let str_len = self.const_i32(s.len() as u64);
                        let string_new_fn = self.main_module.get_function(&STRING_NEW_FN_NAME).unwrap();
                        self.builder.build_call(string_new_fn, &[str.into(), str_len.into()], "").try_as_basic_value().left().unwrap()
                    }
                }
            }
            TypedNode::Unary { .. } |
            TypedNode::Binary { .. } |
            TypedNode::Grouped { .. } => todo!(),
            TypedNode::Array { type_id, items, .. } => {
                todo!()
            }
            TypedNode::Tuple { .. } |
            TypedNode::Set { .. } |
            TypedNode::Map { .. } |
            TypedNode::Identifier { .. } |
            TypedNode::NoneValue { .. } |
            TypedNode::Invocation { .. } |
            TypedNode::Accessor { .. } |
            TypedNode::Indexing { .. } |
            TypedNode::Lambda { .. } |
            TypedNode::Assignment { .. } => todo!(),
            _ => unreachable!("Node is not an expression and should have been handled in visit_statement")
        }
    }
}

#[cfg(test)]
mod tests {
    use std::env::temp_dir;
    use std::path::Path;
    use std::process::Command;
    use assert_cmd::cargo::CommandCargoExt;
    use itertools::{EitherOrBoth, Itertools};
    use abra_core::common::util::random_string;
    use crate::get_project_root;

    fn run_test_file(file_name: &str) {
        let rust_project_root = get_project_root().unwrap();

        let tests_file_path = rust_project_root.join("abra_llvm").join("tests");
        let test_file_path = tests_file_path.join(file_name);

        let test_file = std::fs::read_to_string(&test_file_path).unwrap();
        let build_dir = if let Some(test_temp_dir) = std::env::var("TEST_TMP_DIR").ok() {
            let dir = Path::new(&test_temp_dir).join(random_string(12));
            std::fs::create_dir(&dir).unwrap();
            dir
        } else {
            temp_dir()
        };
        eprintln!("running test {}, using build dir: {}", file_name, &build_dir.to_str().unwrap());
        let output = Command::cargo_bin("abra").unwrap()
            .arg("build")
            .arg("--run")
            .arg(&test_file_path)
            .arg("-o")
            .arg(file_name.replace(".abra", ""))
            .arg("-b")
            .arg(build_dir)
            .output()
            .unwrap();
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
    fn test_ints() {
        run_test_file("ints.abra");
    }

    #[test]
    fn test_floats() {
        run_test_file("floats.abra");
    }

    #[test]
    fn test_bools() {
        run_test_file("bools.abra");
    }

    #[test]
    fn test_strings() {
        run_test_file("strings.abra");
    }
}
