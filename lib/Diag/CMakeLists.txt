set(LLVM_LINK_COMPONENTS mc)

add_vlang_library(vlangDiag
  ChainedDiagnosticConsumer.cpp
  Diagnostic.cpp
  DiagnosticIDs.cpp
  DiagnosticRenderer.cpp
  LogDiagnosticPrinter.cpp
  SerializedDiagnosticPrinter.cpp
  TextDiagnostic.cpp
  TextDiagnosticBuffer.cpp
  TextDiagnosticPrinter.cpp
  VerifyDiagnosticConsumer.cpp
  )

add_dependencies(vlangDiag
  VlangDiagnosticCommon
  VlangDiagnosticFrontend
  VlangDiagnosticLex
  VlangDiagnosticParse
  VlangDiagnosticGroups
  )

# vlangBasic depends on the version.
if (Subversion_FOUND AND EXISTS "${VLANG_SOURCE_DIR}/.svn")
  add_dependencies(vlangBasic vlang_revision_tag)
endif()
