use crate::test_utils::TestRunner;

#[test]
fn lexer_tests() {
    TestRunner::lexer_test_runner()
        // Ints
        .add_test_vs_rust("lexer/ints.abra")
        .add_test_vs_rust("lexer/ints_error_leading_zero.abra")
        .add_test_vs_rust("lexer/ints_error_invalid_hex_first.abra")
        .add_test_vs_rust("lexer/ints_error_invalid_hex_eof.abra")
        .add_test_vs_rust("lexer/ints_error_invalid_binary_first.abra")
        .add_test_vs_rust("lexer/ints_error_invalid_binary_eof.abra")
        // Floats
        .add_test_vs_rust("lexer/floats.abra")
        .add_test_vs_rust("lexer/floats_error_extra_period.abra")
        // Strings
        .add_test_vs_rust("lexer/strings.abra")
        .add_test_vs_rust("lexer/strings_error_unterminated_newline.abra")
        .add_test_vs_rust("lexer/strings_error_unterminated_eof.abra")
        .add_test_vs_rust("lexer/strings_error_unsupported_escape_sequence.abra")
        .add_test_vs_rust("lexer/strings_error_invalid_unicode_seq_length.abra")
        .add_test_vs_rust("lexer/strings_error_invalid_unicode_seq_eof.abra")
        .add_test_vs_rust("lexer/strings_error_invalid_unicode_seq_char.abra")
        .add_test_vs_rust("lexer/keywords.abra")
        // Symbols
        .add_test_vs_rust("lexer/symbols.abra")
        .add_test_vs_rust("lexer/symbols_error_ampersand_eof.abra")
        .add_test_vs_rust("lexer/symbols_error_ampersand_other.abra")
        .add_test_vs_rust("lexer/symbols_error_hash_eof.abra")
        .add_test_vs_rust("lexer/symbols_error_hash_other.abra")
        // Comments
        .add_test_vs_rust("lexer/comments_single_line.abra")
        .add_test_vs_rust("lexer/comments_multiline.abra")
        .add_test_vs_txt("lexer/comments_error_multiline_unclosed.abra", "lexer/comments_error_multiline_unclosed.out")
        .run_tests();
}

#[test]
fn parser_tests() {
    TestRunner::parser_test_runner()
        // Literals
        .add_test_vs_txt("parser/literals.abra", "parser/literals.out.json")
        // Binary
        .add_test_vs_txt("parser/binary.abra", "parser/binary.out.json")
        .add_test_vs_txt("parser/binary_error_eof.abra", "parser/binary_error_eof.out")
        // Unary
        .add_test_vs_txt("parser/unary.abra", "parser/unary.out.json")
        .add_test_vs_txt("parser/unary_error_eof.abra", "parser/unary_error_eof.out")
        // Grouped
        .add_test_vs_txt("parser/grouped.abra", "parser/grouped.out.json")
        .add_test_vs_txt("parser/grouped_error_eof.abra", "parser/grouped_error_eof.out")
        .add_test_vs_txt("parser/grouped_error_wrong_closing.abra", "parser/grouped_error_wrong_closing.out")
        // Identifiers
        .add_test_vs_txt("parser/identifiers.abra", "parser/identifiers.out.json")
        // Accessors
        .add_test_vs_txt("parser/accessor.abra", "parser/accessor.out.json")
        .add_test_vs_txt("parser/accessor_error_eof.abra", "parser/accessor_error_eof.out")
        .add_test_vs_txt("parser/accessor_error_self.abra", "parser/accessor_error_self.out")
        .add_test_vs_txt("parser/accessor_error_None.abra", "parser/accessor_error_None.out")
        // Invocation
        .add_test_vs_txt("parser/invocation.abra", "parser/invocation.out.json")
        .add_test_vs_txt("parser/invocation_error_eof.abra", "parser/invocation_error_eof.out")
        .add_test_vs_txt("parser/invocation_error_no_comma.abra", "parser/invocation_error_no_comma.out")
        .add_test_vs_txt("parser/invocation_error_no_rparen.abra", "parser/invocation_error_no_rparen.out")
        .add_test_vs_txt("parser/invocation_error_empty_typeargs.abra", "parser/invocation_error_empty_typeargs.out")
        .add_test_vs_txt("parser/invocation_error_invalid_typearg.abra", "parser/invocation_error_invalid_typearg.out")
        .add_test_vs_txt("parser/invocation_error_typeargs_eof.abra", "parser/invocation_error_typeargs_eof.out")
        .add_test_vs_txt("parser/invocation_error_typeargs_no_lparen.abra", "parser/invocation_error_typeargs_no_lparen.out")
        // Array
        .add_test_vs_txt("parser/array.abra", "parser/array.out.json")
        .add_test_vs_txt("parser/array_error_eof.abra", "parser/array_error_eof.out")
        .add_test_vs_txt("parser/array_error_no_comma.abra", "parser/array_error_no_comma.out")
        .add_test_vs_txt("parser/array_error_no_rbrack.abra", "parser/array_error_no_rbrack.out")
        // Set
        .add_test_vs_txt("parser/set.abra", "parser/set.out.json")
        .add_test_vs_txt("parser/set_error_eof.abra", "parser/set_error_eof.out")
        .add_test_vs_txt("parser/set_error_no_comma.abra", "parser/set_error_no_comma.out")
        .add_test_vs_txt("parser/set_error_no_rbrace.abra", "parser/set_error_no_rbrace.out")
        // Map
        .add_test_vs_txt("parser/map.abra", "parser/map.out.json")
        .add_test_vs_txt("parser/map_error_bad_key.abra", "parser/map_error_bad_key.out")
        .add_test_vs_txt("parser/map_error_no_colon.abra", "parser/map_error_no_colon.out")
        .add_test_vs_txt("parser/map_error_no_colon_eof.abra", "parser/map_error_no_colon_eof.out")
        .add_test_vs_txt("parser/map_error_no_comma.abra", "parser/map_error_no_comma.out")
        .add_test_vs_txt("parser/map_error_no_rbrace.abra", "parser/map_error_no_rbrace.out")
        .add_test_vs_txt("parser/map_error_no_value.abra", "parser/map_error_no_value.out")
        .add_test_vs_txt("parser/map_error_no_value_eof.abra", "parser/map_error_no_value_eof.out")
        // Tuples
        .add_test_vs_txt("parser/tuples.abra", "parser/tuples.out.json")
        .add_test_vs_txt("parser/tuples_error_eof.abra", "parser/tuples_error_eof.out")
        .add_test_vs_txt("parser/tuples_error_no_comma.abra", "parser/tuples_error_no_comma.out")
        .add_test_vs_txt("parser/tuples_error_no_rparen.abra", "parser/tuples_error_no_rparen.out")
        // Lambdas
        .add_test_vs_txt("parser/lambdas.abra", "parser/lambdas.out.json")
        .add_test_vs_txt("parser/lambdas_error_bad_arg.abra", "parser/lambdas_error_bad_arg.out")
        .add_test_vs_txt("parser/lambdas_error_badparam_None.abra", "parser/lambdas_error_badparam_None.out")
        .add_test_vs_txt("parser/lambdas_error_badparam_nonident.abra", "parser/lambdas_error_badparam_nonident.out")
        .add_test_vs_txt("parser/lambdas_error_badparam_self.abra", "parser/lambdas_error_badparam_self.out")
        .add_test_vs_txt("parser/lambdas_error_noargs_no_arrow.abra", "parser/lambdas_error_noargs_no_arrow.out")
        .add_test_vs_txt("parser/lambdas_error_statement_body.abra", "parser/lambdas_error_statement_body.out")
        // Indexing
        .add_test_vs_txt("parser/indexing.abra", "parser/indexing.out.json")
        .add_test_vs_txt("parser/indexing_error_eof.abra", "parser/indexing_error_eof.out")
        .add_test_vs_txt("parser/indexing_error_no_rbrack.abra", "parser/indexing_error_no_rbrack.out")
        // Assignment
        .add_test_vs_txt("parser/assignment.abra", "parser/assignment.out.json")
        .add_test_vs_txt("parser/assignment_error_as_expr.abra", "parser/assignment_error_as_expr.out")
        .add_test_vs_txt("parser/assignment_error_assign_to_range.abra", "parser/assignment_error_assign_to_range.out")
        .add_test_vs_txt("parser/assignment_error_assignment_precedence.abra", "parser/assignment_error_assignment_precedence.out")
        .add_test_vs_txt("parser/assignment_error_illegal_target.abra", "parser/assignment_error_illegal_target.out")
        // If
        .add_test_vs_txt("parser/if.abra", "parser/if.out.json")
        .add_test_vs_txt("parser/if_error_no_closing_pipe.abra", "parser/if_error_no_closing_pipe.out")
        .add_test_vs_txt("parser/if_error_no_then_block.abra", "parser/if_error_no_then_block.out")
        // Match
        .add_test_vs_txt("parser/match.abra", "parser/match.out.json")
        .add_test_vs_txt("parser/match_error_case_bad_binding.abra", "parser/match_error_case_bad_binding.out")
        .add_test_vs_txt("parser/match_error_case_bad_path.abra", "parser/match_error_case_bad_path.out")
        .add_test_vs_txt("parser/match_error_case_bad_token.abra", "parser/match_error_case_bad_token.out")
        .add_test_vs_txt("parser/match_error_case_expr.abra", "parser/match_error_case_expr.out")
        .add_test_vs_txt("parser/match_error_case_no_case.abra", "parser/match_error_case_no_case.out")
        .add_test_vs_txt("parser/match_error_no_expr.abra", "parser/match_error_no_expr.out")
        .add_test_vs_txt("parser/match_error_no_lbrace.abra", "parser/match_error_no_lbrace.out")
        // Decorators
        .add_test_vs_txt("parser/decorator_error_bad_ident.abra", "parser/decorator_error_bad_ident.out")
        .add_test_vs_txt("parser/decorator_error_before_expr.abra", "parser/decorator_error_before_expr.out")
        .add_test_vs_txt("parser/decorator_error_before_invalid_stmt.abra", "parser/decorator_error_before_invalid_stmt.out")
        .add_test_vs_txt("parser/decorator_error_non_constant_arg.abra", "parser/decorator_error_non_constant_arg.out")
        .add_test_vs_txt("parser/decorator.abra", "parser/decorator.out.json")

        // Imports
        .add_test_vs_txt("parser/import.abra", "parser/import.out.json")
        .add_test_vs_txt("parser/import_error_alias_bad_alias.abra", "parser/import_error_alias_bad_alias.out")
        .add_test_vs_txt("parser/import_error_alias_bad_module.abra", "parser/import_error_alias_bad_module.out")
        .add_test_vs_txt("parser/import_error_alias_no_alias_eof.abra", "parser/import_error_alias_no_alias_eof.out")
        .add_test_vs_txt("parser/import_error_alias_no_as.abra", "parser/import_error_alias_no_as.out")
        .add_test_vs_txt("parser/import_error_alias_no_module.abra", "parser/import_error_alias_no_module.out")
        .add_test_vs_txt("parser/import_error_forbidden.abra", "parser/import_error_forbidden.out")
        .add_test_vs_txt("parser/import_error_list_2nd_nonident.abra", "parser/import_error_list_2nd_nonident.out")
        .add_test_vs_txt("parser/import_error_list_2nd_underscore.abra", "parser/import_error_list_2nd_underscore.out")
        .add_test_vs_txt("parser/import_error_list_bad_module.abra", "parser/import_error_list_bad_module.out")
        .add_test_vs_txt("parser/import_error_list_first_nonident.abra", "parser/import_error_list_first_nonident.out")
        .add_test_vs_txt("parser/import_error_list_no_comma.abra", "parser/import_error_list_no_comma.out")
        .add_test_vs_txt("parser/import_error_list_no_from.abra", "parser/import_error_list_no_from.out")
        .add_test_vs_txt("parser/import_error_list_no_module_eof.abra", "parser/import_error_list_no_module_eof.out")
        // Exports
        .add_test_vs_txt("parser/export.abra", "parser/export.out.json")
        .add_test_vs_txt("parser/export_error_before_expr.abra", "parser/export_error_before_expr.out")
        .add_test_vs_txt("parser/export_error_before_invalid_statement.abra", "parser/export_error_before_invalid_statement.out")

        // While
        .add_test_vs_txt("parser/while.abra", "parser/while.out.json")
        .add_test_vs_txt("parser/while_error_as_expr.abra", "parser/while_error_as_expr.out")
        // For
        .add_test_vs_txt("parser/for.abra", "parser/for.out.json")
        .add_test_vs_txt("parser/for_error_no_in.abra", "parser/for_error_no_in.out")
        .add_test_vs_txt("parser/for_error_no_iterator.abra", "parser/for_error_no_iterator.out")
        // Type identifiers
        .add_test_vs_txt("parser/typeidentifiers.abra", "parser/typeidentifiers.out.json")
        .add_test_vs_txt("parser/typeidentifiers_error_empty_typeargs.abra", "parser/typeidentifiers_error_empty_typeargs.out")
        .add_test_vs_txt("parser/typeidentifiers_error_empty_tuple.abra", "parser/typeidentifiers_error_empty_tuple.out")
        .add_test_vs_txt("parser/typeidentifiers_error_misplaced_typeargs.abra", "parser/typeidentifiers_error_misplaced_typeargs.out")
        .add_test_vs_txt("parser/typeidentifiers_error_no_base_ident.abra", "parser/typeidentifiers_error_no_base_ident.out")
        .add_test_vs_txt("parser/typeidentifiers_error_no_rbrack.abra", "parser/typeidentifiers_error_no_rbrack.out")
        // Binding declaration
        .add_test_vs_txt("parser/bindingdecl.abra", "parser/bindingdecl.out.json")
        .add_test_vs_txt("parser/bindingdecl_error_bad_expr.abra", "parser/bindingdecl_error_bad_expr.out")
        .add_test_vs_txt("parser/bindingdecl_error_bad_ident.abra", "parser/bindingdecl_error_bad_ident.out")
        .add_test_vs_txt("parser/bindingdecl_error_no_expr.abra", "parser/bindingdecl_error_no_expr.out")
        .add_test_vs_txt("parser/bindingdecl_error_no_ident.abra", "parser/bindingdecl_error_no_ident.out")
        // Function declaration
        .add_test_vs_txt("parser/functiondecl.abra", "parser/functiondecl.out.json")
        .add_test_vs_txt("parser/functiondecl_error_bad_name.abra", "parser/functiondecl_error_bad_name.out")
        .add_test_vs_txt("parser/functiondecl_error_no_parens.abra", "parser/functiondecl_error_no_parens.out")
        .add_test_vs_txt("parser/functiondecl_error_empty_typeparams.abra", "parser/functiondecl_error_empty_typeparams.out")
        .add_test_vs_txt("parser/functiondecl_error_typeparam_invalid.abra", "parser/functiondecl_error_typeparam_invalid.out")
        .add_test_vs_txt("parser/functiondecl_error_no_body.abra", "parser/functiondecl_error_no_body.out")
        // Type declaration
        .add_test_vs_txt("parser/typedecl.abra", "parser/typedecl.out.json")
        .add_test_vs_txt("parser/typedecl_error_exporting_method.abra", "parser/typedecl_error_exporting_method.out")
        .add_test_vs_txt("parser/typedecl_error_field_after_method.abra", "parser/typedecl_error_field_after_method.out")
        .add_test_vs_txt("parser/typedecl_error_illegal_body_part.abra", "parser/typedecl_error_illegal_body_part.out")
        // Enum declaration
        .add_test_vs_txt("parser/enumdecl.abra", "parser/enumdecl.out.json")
        .add_test_vs_txt("parser/enumdecl_error_container_no_typeannotation.abra", "parser/enumdecl_error_container_no_typeannotation.out")
        .add_test_vs_txt("parser/enumdecl_error_empty_container_variant.abra", "parser/enumdecl_error_empty_container_variant.out")
        // Returns
        .add_test_vs_txt("parser/return.abra", "parser/return.out.json")
        .add_test_vs_txt("parser/return_error_nonexpr.abra", "parser/return_error_nonexpr.out")

        .run_tests();
}

#[test]
fn typechecker_tests() {
    TestRunner::typechecker_test_runner()
        // Lexer/Parser error handling
        .add_test_vs_txt("typechecker/_lexer_error.abra", "typechecker/_lexer_error.out")
        .add_test_vs_txt("typechecker/_parser_error.abra", "typechecker/_parser_error.out")
        // Literals
        .add_test_vs_txt("typechecker/literals.abra", "typechecker/literals.out.json")
        // Unary
        .add_test_vs_txt("typechecker/unary.abra", "typechecker/unary.out.json")
        .add_test_vs_txt("typechecker/unary_error_minus.1.abra", "typechecker/unary_error_minus.1.out")
        .add_test_vs_txt("typechecker/unary_error_minus.2.abra", "typechecker/unary_error_minus.2.out")
        .add_test_vs_txt("typechecker/unary_error_neg.1.abra", "typechecker/unary_error_neg.1.out")
        .add_test_vs_txt("typechecker/unary_error_neg.2.abra", "typechecker/unary_error_neg.2.out")
        // Identifier
        .add_test_vs_txt("typechecker/identifier.abra", "typechecker/identifier.out.json")
        .add_test_vs_txt("typechecker/identifier_error_discard.abra", "typechecker/identifier_error_discard.out")
        .add_test_vs_txt("typechecker/identifier_error_type_mismatch.abra", "typechecker/identifier_error_type_mismatch.out")
        .add_test_vs_txt("typechecker/identifier_error_type_mismatch_None.abra", "typechecker/identifier_error_type_mismatch_None.out")
        .add_test_vs_txt("typechecker/identifier_error_unknown_ident.abra", "typechecker/identifier_error_unknown_ident.out")
        // Array
        .add_test_vs_txt("typechecker/array.abra", "typechecker/array.out.json")
        .add_test_vs_txt("typechecker/array_error_bindingdecl_empty.abra", "typechecker/array_error_bindingdecl_empty.out")
        .add_test_vs_txt("typechecker/array_error_type_mismatch_bindingdecl.1.abra", "typechecker/array_error_type_mismatch_bindingdecl.1.out")
        .add_test_vs_txt("typechecker/array_error_type_mismatch_bindingdecl.2.abra", "typechecker/array_error_type_mismatch_bindingdecl.2.out")
        .add_test_vs_txt("typechecker/array_error_type_mismatch_item.abra", "typechecker/array_error_type_mismatch_item.out")
        .add_test_vs_txt("typechecker/array_error_type_mismatch_item_nested.abra", "typechecker/array_error_type_mismatch_item_nested.out")
        // If expressions & statements
        .add_test_vs_txt("typechecker/if_error_bad_cond_type.abra", "typechecker/if_error_bad_cond_type.out")
        .add_test_vs_txt("typechecker/if_error_block_mismatch.1.abra", "typechecker/if_error_block_mismatch.1.out")
        .add_test_vs_txt("typechecker/if_error_block_mismatch.2.abra", "typechecker/if_error_block_mismatch.2.out")
        .add_test_vs_txt("typechecker/if_error_empty_else_block.abra", "typechecker/if_error_empty_else_block.out")
        .add_test_vs_txt("typechecker/if_error_empty_if_block.abra", "typechecker/if_error_empty_if_block.out")
        .add_test_vs_txt("typechecker/if_error_mismatch_bindingdecl.1.abra", "typechecker/if_error_mismatch_bindingdecl.1.out")
        .add_test_vs_txt("typechecker/if_error_mismatch_bindingdecl.2.abra", "typechecker/if_error_mismatch_bindingdecl.2.out")
        .add_test_vs_txt("typechecker/if_error_mismatch_bindingdecl.3.abra", "typechecker/if_error_mismatch_bindingdecl.3.out")
        .add_test_vs_txt("typechecker/if_error_no_else_block.abra", "typechecker/if_error_no_else_block.out")
        .add_test_vs_txt("typechecker/if_error_unfilled_holes_bindingdecl.1.abra", "typechecker/if_error_unfilled_holes_bindingdecl.1.out")
        .add_test_vs_txt("typechecker/if_error_unfilled_holes_bindingdecl.2.abra", "typechecker/if_error_unfilled_holes_bindingdecl.2.out")
        .add_test_vs_txt("typechecker/if_error_unfilled_holes_bindingdecl.3.abra", "typechecker/if_error_unfilled_holes_bindingdecl.3.out")
        .add_test_vs_txt("typechecker/if_error_unfilled_holes_bindingdecl.4.abra", "typechecker/if_error_unfilled_holes_bindingdecl.4.out")
        .add_test_vs_txt("typechecker/if_expr.abra", "typechecker/if_expr.out.json")
        .add_test_vs_txt("typechecker/if_stmt.abra", "typechecker/if_stmt.out.json")
        // Invocation
        .add_test_vs_txt("typechecker/invocation.1.abra", "typechecker/invocation.1.out.json")
        .add_test_vs_txt("typechecker/invocation.2.abra", "typechecker/invocation.2.out.json")
        .add_test_vs_txt("typechecker/invocation.3.abra", "typechecker/invocation.3.out.json")
        .add_test_vs_txt("typechecker/invocation.4.abra", "typechecker/invocation.4.out.json")
        .add_test_vs_txt("typechecker/invocation.5.abra", "typechecker/invocation.5.out.json")
        .add_test_vs_txt("typechecker/invocation_error_incorrect_label.abra", "typechecker/invocation_error_incorrect_label.out")
        .add_test_vs_txt("typechecker/invocation_error_mixed_label_optional.abra", "typechecker/invocation_error_mixed_label_optional.out")
        .add_test_vs_txt("typechecker/invocation_error_optional_param_type_mismatch.abra", "typechecker/invocation_error_optional_param_type_mismatch.out")
        .add_test_vs_txt("typechecker/invocation_error_optional_param_unknown.abra", "typechecker/invocation_error_optional_param_unknown.out")
        .add_test_vs_txt("typechecker/invocation_error_too_few_args.abra", "typechecker/invocation_error_too_few_args.out")
        .add_test_vs_txt("typechecker/invocation_error_too_many_args.abra", "typechecker/invocation_error_too_many_args.out")


        // Break
        .add_test_vs_txt("typechecker/break_as_expr.abra", "typechecker/break_as_expr.out.json")
        .add_test_vs_txt("typechecker/break_error_location_module.abra", "typechecker/break_error_location_module.out")
        .add_test_vs_txt("typechecker/break_error_location_module_func.abra", "typechecker/break_error_location_module_func.out")
        .add_test_vs_txt("typechecker/break_error_location_module_if.abra", "typechecker/break_error_location_module_if.out")
        .add_test_vs_txt("typechecker/break_error_unreachable_inside_while.abra", "typechecker/break_error_unreachable_inside_while.out")
        .add_test_vs_txt("typechecker/break_error_unreachable_inside_for.abra", "typechecker/break_error_unreachable_inside_for.out")
        // Continue
        .add_test_vs_txt("typechecker/continue_as_expr.abra", "typechecker/continue_as_expr.out.json")
        .add_test_vs_txt("typechecker/continue_error_location_module.abra", "typechecker/continue_error_location_module.out")
        .add_test_vs_txt("typechecker/continue_error_location_module_func.abra", "typechecker/continue_error_location_module_func.out")
        .add_test_vs_txt("typechecker/continue_error_location_module_if.abra", "typechecker/continue_error_location_module_if.out")
        .add_test_vs_txt("typechecker/continue_error_unreachable_inside_while.abra", "typechecker/continue_error_unreachable_inside_while.out")
        .add_test_vs_txt("typechecker/continue_error_unreachable_inside_for.abra", "typechecker/continue_error_unreachable_inside_for.out")
        // While
        .add_test_vs_txt("typechecker/while.1.abra", "typechecker/while.1.out.json")
        .add_test_vs_txt("typechecker/while.2.abra", "typechecker/while.2.out.json")
        .add_test_vs_txt("typechecker/while.3.abra", "typechecker/while.3.out.json")
        .add_test_vs_txt("typechecker/while_error_bad_cond_type.abra", "typechecker/while_error_bad_cond_type.out")
        // For
        .add_test_vs_txt("typechecker/for.1.abra", "typechecker/for.1.out.json")
        .add_test_vs_txt("typechecker/for.2.abra", "typechecker/for.2.out.json")
        .add_test_vs_txt("typechecker/for_error_bad_iterator.abra", "typechecker/for_error_bad_iterator.out")
        .add_test_vs_txt("typechecker/for_error_bad_iterator_unfilled_hole.abra", "typechecker/for_error_bad_iterator_unfilled_hole.out")
        .add_test_vs_txt("typechecker/for_error_duplicate_ident.abra", "typechecker/for_error_duplicate_ident.out")
        // Binding declaration
        .add_test_vs_txt("typechecker/bindingdecl.abra", "typechecker/bindingdecl.out.json")
        .add_test_vs_txt("typechecker/bindingdecl_error_bare_var.abra", "typechecker/bindingdecl_error_bare_var.out")
        .add_test_vs_txt("typechecker/bindingdecl_error_duplicate_name.abra", "typechecker/bindingdecl_error_duplicate_name.out")
        .add_test_vs_txt("typechecker/bindingdecl_error_uninitialized_val.1.abra", "typechecker/bindingdecl_error_uninitialized_val.1.out")
        .add_test_vs_txt("typechecker/bindingdecl_error_uninitialized_val.2.abra", "typechecker/bindingdecl_error_uninitialized_val.2.out")
        .add_test_vs_txt("typechecker/bindingdecl_error_unknown_type.abra", "typechecker/bindingdecl_error_unknown_type.out")
        .add_test_vs_txt("typechecker/bindingdecl_error_type_mismatch_val.abra", "typechecker/bindingdecl_error_type_mismatch_val.out")
        .add_test_vs_txt("typechecker/bindingdecl_error_type_mismatch_var.abra", "typechecker/bindingdecl_error_type_mismatch_var.out")
        .add_test_vs_txt("typechecker/bindingdecl_error_type_mismatch_option.abra", "typechecker/bindingdecl_error_type_mismatch_option.out")
        .add_test_vs_txt("typechecker/bindingdecl_error_unfilled_holes.abra", "typechecker/bindingdecl_error_unfilled_holes.out")
        // Function declaration
        .add_test_vs_txt("typechecker/funcdecl.1.abra", "typechecker/funcdecl.1.out.json")
        .add_test_vs_txt("typechecker/funcdecl.2.abra", "typechecker/funcdecl.2.out.json")
        .add_test_vs_txt("typechecker/funcdecl.3.abra", "typechecker/funcdecl.3.out.json")
        .add_test_vs_txt("typechecker/funcdecl.4.abra", "typechecker/funcdecl.4.out.json")
        .add_test_vs_txt("typechecker/funcdecl.5.abra", "typechecker/funcdecl.5.out.json")
        .add_test_vs_txt("typechecker/funcdecl.6.abra", "typechecker/funcdecl.6.out.json")
        .add_test_vs_txt("typechecker/funcdecl.7.abra", "typechecker/funcdecl.7.out.json")
        .add_test_vs_txt("typechecker/funcdecl_error_duplicate_func.abra", "typechecker/funcdecl_error_duplicate_func.out")
        .add_test_vs_txt("typechecker/funcdecl_error_duplicate_param.abra", "typechecker/funcdecl_error_duplicate_param.out")
        .add_test_vs_txt("typechecker/funcdecl_error_duplicate_variable.abra", "typechecker/funcdecl_error_duplicate_variable.out")
        .add_test_vs_txt("typechecker/funcdecl_error_invalid_param_type.abra", "typechecker/funcdecl_error_invalid_param_type.out")
        .add_test_vs_txt("typechecker/funcdecl_error_invalid_req_param_position.abra", "typechecker/funcdecl_error_invalid_req_param_position.out")
        .add_test_vs_txt("typechecker/funcdecl_error_invalid_return_type.abra", "typechecker/funcdecl_error_invalid_return_type.out")
        .add_test_vs_txt("typechecker/funcdecl_error_invalid_vararg_position.abra", "typechecker/funcdecl_error_invalid_vararg_position.out")
        .add_test_vs_txt("typechecker/funcdecl_error_invalid_vararg_type.abra", "typechecker/funcdecl_error_invalid_vararg_type.out")
        .add_test_vs_txt("typechecker/funcdecl_error_param_unfilled_holes.1.abra", "typechecker/funcdecl_error_param_unfilled_holes.1.out")
        .add_test_vs_txt("typechecker/funcdecl_error_param_unfilled_holes.2.abra", "typechecker/funcdecl_error_param_unfilled_holes.2.out")
        .add_test_vs_txt("typechecker/funcdecl_error_type_mismatch_param_default_value.1.abra", "typechecker/funcdecl_error_type_mismatch_param_default_value.1.out")
        .add_test_vs_txt("typechecker/funcdecl_error_type_mismatch_param_default_value.2.abra", "typechecker/funcdecl_error_type_mismatch_param_default_value.2.out")
        .add_test_vs_txt("typechecker/funcdecl_error_type_mismatch_param_default_value.3.abra", "typechecker/funcdecl_error_type_mismatch_param_default_value.3.out")
        .add_test_vs_txt("typechecker/funcdecl_error_type_mismatch_param_default_value.4.abra", "typechecker/funcdecl_error_type_mismatch_param_default_value.4.out")
        .add_test_vs_txt("typechecker/funcdecl_error_alias_type_mismatch.abra", "typechecker/funcdecl_error_alias_type_mismatch.out")
        .add_test_vs_txt("typechecker/funcdecl_error_invalid_vararg_type_option.abra", "typechecker/funcdecl_error_invalid_vararg_type_option.out")
        .add_test_vs_txt("typechecker/funcdecl_error_param_default_value_ident.abra", "typechecker/funcdecl_error_param_default_value_ident.out")
        .add_test_vs_txt("typechecker/funcdecl_error_return_type_mismatch.abra", "typechecker/funcdecl_error_return_type_mismatch.out")
        .add_test_vs_txt("typechecker/funcdecl_error_return_type_mismatch_empty.abra", "typechecker/funcdecl_error_return_type_mismatch_empty.out")
        .add_test_vs_txt("typechecker/funcdecl_error_return_type_mismatch_stmt.abra", "typechecker/funcdecl_error_return_type_mismatch_stmt.out")
        // Returns
        .add_test_vs_txt("typechecker/return.1.abra", "typechecker/return.1.out.json")
        .add_test_vs_txt("typechecker/return.2.abra", "typechecker/return.2.out.json")
        .add_test_vs_txt("typechecker/return.3.abra", "typechecker/return.3.out.json")
        .add_test_vs_txt("typechecker/return_as_expr_error_never.abra", "typechecker/return_as_expr_error_never.out")
        .add_test_vs_txt("typechecker/return_error_location.abra", "typechecker/return_error_location.out")
        .add_test_vs_txt("typechecker/return_error_type_mismatch.abra", "typechecker/return_error_type_mismatch.out")
        .add_test_vs_txt("typechecker/return_error_type_mismatch_unit.abra", "typechecker/return_error_type_mismatch_unit.out")
        .add_test_vs_txt("typechecker/return_error_unreachable_stmt_if.abra", "typechecker/return_error_unreachable_stmt_if.out")
        .add_test_vs_txt("typechecker/return_error_unreachable_stmt_plain.abra", "typechecker/return_error_unreachable_stmt_plain.out")
        .add_test_vs_txt("typechecker/return_error_unreachable_stmt_while_inside.1.abra", "typechecker/return_error_unreachable_stmt_while_inside.1.out")
        .add_test_vs_txt("typechecker/return_error_unreachable_stmt_while_inside.2.abra", "typechecker/return_error_unreachable_stmt_while_inside.2.out")
        .add_test_vs_txt("typechecker/return_error_unreachable_stmt_while_outside.1.abra", "typechecker/return_error_unreachable_stmt_while_outside.1.out")
        .add_test_vs_txt("typechecker/return_error_unreachable_stmt_while_outside.2.abra", "typechecker/return_error_unreachable_stmt_while_outside.2.out")
        .add_test_vs_txt("typechecker/return_error_unreachable_stmt_for_inside.1.abra", "typechecker/return_error_unreachable_stmt_for_inside.1.out")
        .add_test_vs_txt("typechecker/return_error_unreachable_stmt_for_inside.2.abra", "typechecker/return_error_unreachable_stmt_for_inside.2.out")
        .add_test_vs_txt("typechecker/return_error_unreachable_stmt_for_outside.1.abra", "typechecker/return_error_unreachable_stmt_for_outside.1.out")
        .add_test_vs_txt("typechecker/return_error_unreachable_stmt_for_outside.2.abra", "typechecker/return_error_unreachable_stmt_for_outside.2.out")

        .run_tests();
}
