set(LLVM_LINK_COMPONENTS mc)

add_vlang_library(vlangBasic
  CharInfo.cpp
  FileManager.cpp
  FileSystemStatCache.cpp
  IdentifierTable.cpp
  LangOptions.cpp
  OperatorPrecedence.cpp
  SourceLocation.cpp
  SourceManager.cpp
  Systask.cpp
  TargetInfo.cpp
  Targets.cpp
  TokenKinds.cpp
  Version.cpp
  VersionTuple.cpp
  )

  # vlangBasic depends on the version.
if (Subversion_FOUND AND EXISTS "${VLANG_SOURCE_DIR}/.svn")
  add_dependencies(vlangBasic vlang_revision_tag)
endif()
