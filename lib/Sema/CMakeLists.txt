set(LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  support
  )

add_vlang_library(vlangSema
  Scope.cpp
  Sema.cpp
  )

target_link_libraries(vlangSema
  vlangBasic
  vlangLex
  )
