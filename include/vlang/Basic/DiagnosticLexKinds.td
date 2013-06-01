//==--- DiagnosticLexKinds.td - liblex diagnostics ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Lexer Diagnostics
//===----------------------------------------------------------------------===//

let Component = "Lex", CategoryName = "Lexical or Preprocessor Issue" in {

def null_in_string : Warning<"null character(s) preserved in string literal">,
  InGroup<NullCharacter>;
def null_in_file : Warning<"null character ignored">, InGroup<NullCharacter>;
def warn_nested_block_comment : Warning<"'/*' within block comment">,
  InGroup<Comment>;
def escaped_newline_block_comment_end : Warning<
  "escaped newline between */ characters at block comment end">,
  InGroup<Comment>;
def backslash_newline_space : Warning<
  "backslash and newline separated by space">,
  InGroup<DiagGroup<"backslash-newline-escape">>;

def ext_multi_line_line_comment : Extension<"multi-line // comment">,
    InGroup<Comment>;
def ext_no_newline_eof : Extension<"no newline at end of file">, 
  InGroup<DiagGroup<"newline-eof">>;

def ext_unterminated_string : ExtWarn<"missing terminating '\"' character">,
  InGroup<InvalidPPToken>;
def err_unterminated_block_comment : Error<"unterminated /* comment">;

def err_conflict_marker : Error<"version control conflict marker in file">;

// Unicode and UCNs
def err_invalid_utf8 : Error<  "source file is not valid UTF-8">;
def err_non_ascii : Error<  "non-ASCII characters are not allowed outside of literals and identifiers">;
def err_hex_escape_no_digits : Error<  "\\%0 used with no following hex digits">;

// Literal
def ext_nonstandard_escape : Extension<
  "use of non-standard escape character '\\%0'">;
def ext_unknown_escape : ExtWarn<"unknown escape sequence '\\%0'">;
def err_invalid_decimal_digit : Error<"invalid digit '%0' in decimal constant">;
def err_invalid_binary_digit : Error<"invalid digit '%0' in binary constant">;
def err_invalid_octal_digit : Error<"invalid digit '%0' in octal constant">;
def err_invalid_base : Error<"Expected valid base identifier following signed identifier">;
def err_exponent_has_no_digits : Error<"exponent has no digits">;
def warn_octal_escape_too_large : ExtWarn<"octal escape sequence out of range">;
def warn_hex_escape_too_large : ExtWarn<"hex escape sequence out of range">;
def ext_string_too_long : Extension<"string literal of length %0 exceeds "
  "maximum length %1 that %select{C90|ISO C99|C++}2 compilers are required to "
  "support">, InGroup<OverlengthStrings>;
def err_unsupported_string_concat : Error<
  "unsupported non-standard concatenation of string literals">;
def err_bad_string_encoding : Error<
  "illegal character encoding in string literal">;
def warn_bad_string_encoding : ExtWarn<
  "illegal character encoding in string literal">,
  InGroup<InvalidSourceEncoding>;
def err_lexing_string : Error<"failure when lexing a string">;

//===----------------------------------------------------------------------===//
// Preprocessor Diagnostics
//===----------------------------------------------------------------------===//

def ext_c99_whitespace_required_after_macro_name : ExtWarn<
  "ISO C99 requires whitespace after the macro name">, InGroup<C99>;
def ext_missing_whitespace_after_macro_name : ExtWarn<
  "whitespace required after macro name">;
def warn_missing_whitespace_after_macro_name : Warning<
  "whitespace recommended after macro name">;
  
def pp_poisoning_existing_macro : Warning<"poisoning existing macro">;
def pp_out_of_date_dependency : Warning<
  "current file is older than dependency %0">;
def pp_disabled_macro_expansion : Warning<
  "disabled expansion of recursive macro">, DefaultIgnore,
  InGroup<DiagGroup<"disabled-macro-expansion">>;
def pp_macro_not_used : Warning<"macro is not used">, DefaultIgnore,
  InGroup<DiagGroup<"unused-macros">>;
def warn_pp_undef_identifier : Warning<
  "%0 is not defined, evaluates to 0">,
  InGroup<DiagGroup<"undef">>, DefaultIgnore;
def warn_pp_ambiguous_macro : Warning<
  "ambiguous expansion of macro %0">, InGroup<AmbiguousMacro>;
def note_pp_ambiguous_macro_chosen : Note<
  "expanding this definition of %0">;
def note_pp_ambiguous_macro_other : Note<
  "other definition of %0">;

def pp_invalid_string_literal : Warning<
  "invalid string literal, ignoring final '\\'">;

def ext_pp_extra_tokens_at_eol : ExtWarn<
  "extra tokens at end of `%0 directive">, InGroup<ExtraTokens>;
  
def ext_pp_macro_redef : ExtWarn<"%0 macro redefined">;
def err_embedded_include : Error<
  "embedding a `%0 directive within macro arguments is not supported">;
def ext_embedded_directive : Extension<
  "embedding a directive within macro arguments has undefined behavior">,
  InGroup<DiagGroup<"embedded-directive">>;
def ext_empty_fnmacro_arg : Extension<
  "empty macro arguments are a C99 feature">, InGroup<C99>;
def note_macro_here : Note<"macro %0 defined here">;

def err_pp_invalid_directive : Error<"invalid preprocessing directive">;
def err_pp_directive_required : Error<
  "%0 must be used within a preprocessing directive">;
def err_pp_file_not_found : Error<"'%0' file not found">, DefaultFatal;
def err_pp_file_not_found_not_fatal : Error<
  "'%0' file not found with <angled> include; use \"quotes\" instead">;
def err_pp_error_opening_file : Error<
  "error opening file '%0': %1">, DefaultFatal;
def err_pp_empty_filename : Error<"empty filename">;
def err_pp_include_too_deep : Error<"`include nested too deeply">;
def err_pp_expects_filename : Error<"expected \"FILENAME\" or <FILENAME>">;
def err_pp_macro_not_identifier : Error<"macro names must be identifiers">;
def err_pp_missing_macro_name : Error<"macro name missing">;
def err_pp_missing_rparen_in_macro_def : Error<
  "missing ')' in macro parameter list">;
def err_pp_invalid_tok_in_arg_list : Error<
  "invalid token in macro parameter list">;
def err_pp_expected_ident_in_arg_list : Error<
  "expected identifier in macro parameter list">;
def err_pp_expected_comma_in_arg_list : Error<
  "expected comma in macro parameter list">;
def err_pp_duplicate_name_in_arg_list : Error<
  "duplicate macro parameter name %0">;
def err_pp_stringize_not_parameter : Error<
  "'`' is not followed by a macro parameter">;
def err_pp_unterminated_conditional : Error<
  "unterminated conditional directive">;
def pp_err_else_after_else : Error<"`else after `else">;
def pp_err_elsif_after_else : Error<"`elsif after `else">;
def pp_err_else_without_if : Error<"`else without `ifdef">;
def pp_err_elsif_without_if : Error<"`elsif without `ifdef">;
def err_pp_endif_without_if : Error<"`endif without `ifdef">;
def err_pp_expected_value_in_expr : Error<"expected value in expression">;
def err_pp_expected_rparen : Error<"expected ')' in preprocessor expression">;
def err_pp_expected_eol : Error<
  "expected end of line in preprocessor expression">;
def err_pp_missing_lparen : Error<"missing '(' after '%0'">;
def err_pp_missing_rparen : Error<"missing ')' after '%0'">;
def err_pp_colon_without_question : Error<"':' without preceding '?'">;

def err_pp_expected_timescale_num  : Error<"Expected number for timescale %0">;
def err_pp_expected_timescale_unit : Error<"Expected timeunit for timescale %0">;

def err_defined_macro_name : Error<"'defined' cannot be used as a macro name">;
//def err_paste_at_start : Error<
//  "'``' cannot appear at start of macro expansion">;
//def err_paste_at_end : Error<"'``' cannot appear at end of macro expansion">;
def err_unterm_macro_invoc : Error<
  "unterminated function-like macro invocation">;
def err_too_many_args_in_macro_invoc : Error<
  "too many arguments provided to function-like macro invocation">;
def err_too_few_args_in_macro_invoc : Error<
  "too few arguments provided to function-like macro invocation">;

def err_pp_visibility_non_macro : Error<"no macro named %0">;

  
}
