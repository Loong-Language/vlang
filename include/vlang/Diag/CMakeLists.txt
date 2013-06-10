macro(vlang_diag_gen component)
  vlang_tablegen(Diagnostic${component}Kinds.inc
    -gen-vlang-diags-defs -vlang-component=${component}
    SOURCE Diagnostic.td
    TARGET VlangDiagnostic${component})
endmacro(vlang_diag_gen)

vlang_diag_gen(Common)
vlang_diag_gen(Frontend)
vlang_diag_gen(Lex)
vlang_diag_gen(Parse)

vlang_tablegen(DiagnosticGroups.inc -gen-vlang-diag-groups
  SOURCE Diagnostic.td
  TARGET VlangDiagnosticGroups)
