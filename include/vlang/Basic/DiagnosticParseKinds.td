//==--- DiagnosticParseKinds.td - libparse diagnostics --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Parser Diagnostics
//===----------------------------------------------------------------------===//
let Component = "Parse" in {
let CategoryName = "Parse Issue" in {

// Semicolon errors
def err_expected_semi_after          : Error<"expected ';' after %0">;
def err_expected_semi_after_stmt     : Error<"expected ';' after statement">;
def err_expected_semi_after_expr     : Error<"expected ';' after expression">;
def err_expected_semi_after_decl     : Error<"expected ';' after declaration">;
def err_extraneous_token_before_semi : Error<"extraneous '%0' before ';'">;
def err_expected_semi_decl_list      : Error<"expected ';' at end of declaration list">;

// Identifier related
def err_expected_ident                : Error<"expected identifier">;
def err_expected_ident_after_comma    : Error<"expected identifier after ','">;
def err_expected_ident_lparen         : Error<"expected identifier or '('">;
def err_expected_ident_lbrace         : Error<"expected identifier or '{'">;
def err_expected_ident_for            : Error<"expected %0 identifier">;

// Generic errors.
def err_expected_expression           : Error<"expected expression">;
def err_expected_type                 : Error<"expected a type">;
def err_extraneous_closing_brace      : Error<"extraneous closing brace ('}')">;
def err_expected_lbrace               : Error<"expected '{'">;
def err_expected_lparen               : Error<"expected '('">;
def err_expected_lparen_or_lbrace     : Error<"expected '('or '{'">;
def err_expected_lparen_or_comma      : Error<"expected ')' or ','">;
def err_expected_rparen               : Error<"expected ')'">;
def err_expected_lsquare              : Error<"expected '['">;
def err_expected_rsquare              : Error<"expected ']'">;
def err_expected_rbrace               : Error<"expected '}'">;
def err_expected_greater              : Error<"expected '>'">;
def err_expected_ggg                  : Error<"expected '>>>'">;
def err_expected_statement            : Error<"expected statement">;
def err_expected_lparen_after         : Error<"expected '(' after '%0'">;
def err_expected_less_after           : Error<"expected '<' after '%0'">;
def err_expected_equal_after          : Error<"expected '=' after %0">;
def err_expected_comma                : Error<"expected ','">;
def err_expected_colon_after          : Error<"expected ':' after %0">;
def err_expected_lbrace_or_comma      : Error<"expected '{' or ','">;
def err_expected_rbrace_or_comma      : Error<"expected '}' or ','">;
def err_expected_rsquare_or_comma     : Error<"expected ']' or ','">;
def err_unsupported_feature           : Error<"%0 unsupported">;

// Closing end statement errors
def err_expected_func_end             : Error<"expected endfunction">;
def err_expected_task_end             : Error<"expected endtask">;

// Design Element declaration
def err_expected_matching_ident       : Error<"Expected end name to match declaration name">;
def err_expected_end_design           : Error<"Expected end%0 after design definition">;

// Port list
def err_cant_mix_port_connection      : Error<"Not allowed to mix named and ordered port connections">;

// List errors
def err_expected_list_of_ident        : Error<"Expected list of identifiers">;
def err_expected_list_of_decl_assign  : Error<"expected list of data declaration assigns">;
def err_expected_one_expr_for         : Error<"Expected only 1 expression for %0">;

// Expression related errors
def err_expected_ternery_colon        : Error<"Expected ':' inside ternery">;

// Statement related
def err_expected_blocking_stmt        : Error<"Expected blocking statement">;
def err_expected_nonblocking_stmt     : Error<"Expected non-blocking statement">;
def err_expected_assign_operator      : Error<"Expected assignment operator">;
def err_expected_end                  : Error<"Expected 'end' after 'begin' block">;
def err_expected_join                 : Error<"Expected 'join' after 'fork' block">;
def err_expected_quote_after_void     : Error<"Expected ' after void">;

def err_expected_case_or_if_after_pri : Error<"Expected case/if after %0">;

} // end of Parse Issue category.
} // end of Parser diagnostics
