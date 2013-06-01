//===- Version.h - Vlang Version Number -------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines version macros and version-related utility functions
/// for Vlang.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_BASIC_VERSION_H
#define LLVM_VLANG_BASIC_VERSION_H

#include "vlang/Basic/Version.inc"
#include "llvm/ADT/StringRef.h"

/// \brief Helper macro for VLANG_VERSION_STRING.
#define VLANG_MAKE_VERSION_STRING2(X) #X

#ifdef VLANG_VERSION_PATCHLEVEL
/// \brief Helper macro for VLANG_VERSION_STRING.
#define VLANG_MAKE_VERSION_STRING(X,Y,Z) VLANG_MAKE_VERSION_STRING2(X.Y.Z)

/// \brief A string that describes the Vlang version number, e.g., "1.0".
#define VLANG_VERSION_STRING \
  VLANG_MAKE_VERSION_STRING(VLANG_VERSION_MAJOR,VLANG_VERSION_MINOR, \
                            VLANG_VERSION_PATCHLEVEL)
#else
/// \brief Helper macro for VLANG_VERSION_STRING.
#define VLANG_MAKE_VERSION_STRING(X,Y) VLANG_MAKE_VERSION_STRING2(X.Y)

/// \brief A string that describes the Vlang version number, e.g., "1.0".
#define VLANG_VERSION_STRING \
  VLANG_MAKE_VERSION_STRING(VLANG_VERSION_MAJOR,VLANG_VERSION_MINOR)
#endif

namespace vlang {
  /// \brief Retrieves the repository path (e.g., Subversion path) that
  /// identifies the particular Vlang branch, tag, or trunk from which this
  /// Vlang was built.
  std::string getVlangRepositoryPath();

  /// \brief Retrieves the repository path from which LLVM was built.
  ///
  /// This supports LLVM residing in a separate repository from vlang.
  std::string getLLVMRepositoryPath();

  /// \brief Retrieves the repository revision number (or identifer) from which
  /// this Vlang was built.
  std::string getVlangRevision();

  /// \brief Retrieves the repository revision number (or identifer) from which
  /// LLVM was built.
  ///
  /// If Vlang and LLVM are in the same repository, this returns the same
  /// string as getVlangRevision.
  std::string getLLVMRevision();

  /// \brief Retrieves the full repository version that is an amalgamation of
  /// the information in getVlangRepositoryPath() and getVlangRevision().
  std::string getVlangFullRepositoryVersion();

  /// \brief Retrieves a string representing the complete vlang version,
  /// which includes the vlang version number, the repository version,
  /// and the vendor tag.
  std::string getVlangFullVersion();

}

#endif // LLVM_VLANG_BASIC_VERSION_H
