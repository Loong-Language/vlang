add_vlang_library(vlangParse
  Parser.cpp
  ParserDecl.cpp
  ParserExpr.cpp
  ParserStmt.cpp
  )

add_dependencies(vlangParse
  VlangDiagnosticCommon
  VlangDiagnosticParse
  )

target_link_libraries(vlangParse
  vlangBasic
  vlangDiag
  vlangLex
  )
