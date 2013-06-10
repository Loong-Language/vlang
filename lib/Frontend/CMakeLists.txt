add_vlang_library(vlangFrontend
  HeaderIncludeGen.cpp
  InitHeaderSearch.cpp
  InitPreprocessor.cpp
  )

add_dependencies(vlangFrontend
  VlangDiagnosticFrontend
  )

target_link_libraries(vlangFrontend
  vlangDiag
  vlangBasic
  vlangLex
  )
