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
        .run_tests();
}
