# TODO: Add -maltivec when ARCH is PowerPC.

set(LLVM_LINK_COMPONENTS support)

add_vlang_library(vlangLex
  HeaderMap.cpp
  HeaderSearch.cpp
  Lexer.cpp
  LiteralSupport.cpp
  MacroArgs.cpp
  MacroInfo.cpp
  PPCaching.cpp
  PPCallbacks.cpp
  PPDirectives.cpp
  PPLexerChange.cpp
  PPMacroExpansion.cpp
  PreprocessingRecord.cpp
  Preprocessor.cpp
  PreprocessorLexer.cpp
  ScratchBuffer.cpp
  TokenLexer.cpp
  )

add_dependencies(vlangLex
  VlangDiagnosticCommon
  VlangDiagnosticLex
  )

target_link_libraries(vlangLex
  vlangBasic
  vlangDiag
  )
