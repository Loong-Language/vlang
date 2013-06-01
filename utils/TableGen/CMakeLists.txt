set(LLVM_LINK_COMPONENTS Support)

add_tablegen(vlang-tblgen VLANG
  VlangDiagnosticsEmitter.cpp
  OptParserEmitter.cpp
  TableGen.cpp
  )
