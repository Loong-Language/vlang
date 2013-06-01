
add_vlang_executable(vlang
  main.cpp
  )

target_link_libraries( vlang vlangLex vlangBasic vlangFrontend vlangParse vlangSema)

set_target_properties(vlang PROPERTIES VERSION ${VLANG_EXECUTABLE_VERSION})
