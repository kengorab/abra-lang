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
        .run_tests();
}
