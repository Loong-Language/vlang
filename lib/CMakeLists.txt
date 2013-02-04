set( LLVM_LINK_COMPONENTS support)

add_vlang_library(vlangParse
  Lexer.cpp
  Parser.cpp
  Tokens.cpp
  IdentifierTable.cpp
  ParserExpr.cpp
  ParserDecl.cpp
  Declaration.cpp
  Namespace.cpp
  Semantic.cpp
  Type.cpp
  )

#add_dependencies(verilogParse)
