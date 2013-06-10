//==--- DiagnosticFrontendKinds.td - frontend diagnostics -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

let Component = "Frontend" in {

def err_fe_error_opening : Error<"error opening '%0': %1">;
def err_fe_error_reading : Error<"error reading '%0'">;
def err_fe_error_reading_stdin : Error<"error reading stdin">;
def err_fe_error_backend : Error<"error in backend: %0">, DefaultFatal;

def err_fe_stdout_binary : Error<"unable to change standard output to binary">,
  DefaultFatal;
def err_fe_remap_missing_to_file : Error<
    "could not remap file '%0' to the contents of file '%1'">, DefaultFatal;
def err_fe_remap_missing_from_file : Error<
    "could not remap from missing file '%0'">, DefaultFatal;
def warn_fe_macro_contains_embedded_newline : Warning<
    "macro '%0' contains embedded newline; text after the newline is ignored">;
def warn_fe_cc_print_header_failure : Warning<
    "unable to open CC_PRINT_HEADERS file: %0 (using stderr)">;


def warn_fe_serialized_diag_failure : Warning<
    "unable to open file %0 for serializing diagnostics (%1)">,
    InGroup<DiagGroup<"serialized-diagnostics">>;

def err_verify_missing_line : Error<
    "missing or invalid line number following '@' in expected %0">;
def err_verify_missing_file : Error<
    "file '%0' could not be located in expected %1">;
def err_verify_invalid_range : Error<
    "invalid range following '-' in expected %0">;
def err_verify_missing_start : Error<
    "cannot find start ('{{') of expected %0">;
def err_verify_missing_end : Error<
    "cannot find end ('}}') of expected %0">;
def err_verify_invalid_content : Error<
    "invalid expected %0: %1">;
def err_verify_inconsistent_diags : Error<
    "'%0' diagnostics %select{expected|seen}1 but not %select{seen|expected}1: "
    "%2">;
def err_verify_invalid_no_diags : Error<
    "%select{expected|'expected-no-diagnostics'}0 directive cannot follow "
    "%select{'expected-no-diagnostics' directive|other expected directives}0">;
def err_verify_no_directives : Error<
    "no expected directives found: consider use of 'expected-no-diagnostics'">;

def note_fixit_applied : Note<"FIX-IT applied suggested code changes">;
def note_fixit_in_macro : Note<
    "FIX-IT unable to apply suggested code changes in a macro">;
def note_fixit_failed : Note<
    "FIX-IT unable to apply suggested code changes">;
def note_fixit_unfixed_error : Note<"FIX-IT detected an error it cannot fix">;
def warn_fixit_no_changes : Note<
    "FIX-IT detected errors it could not fix; no output will be generated">;

}
