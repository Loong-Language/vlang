//===--- DiagnosticFrontend.h - Diagnostics for frontend --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_FRONTENDDIAGNOSTIC_H
#define LLVM_VLANG_FRONTENDDIAGNOSTIC_H

#include "vlang/Basic/Diagnostic.h"

namespace vlang {
  namespace diag {
    enum {
#define DIAG(ENUM,FLAGS,DEFAULT_MAPPING,DESC,GROUP,\
             SFINAE,ACCESS,NOWERROR,SHOWINSYSHEADER,CATEGORY) ENUM,
#define FRONTENDSTART
#include "vlang/Basic/DiagnosticFrontendKinds.inc"
#undef DIAG
      NUM_BUILTIN_FRONTEND_DIAGNOSTICS
    };
  }  // end namespace diag
}  // end namespace vlang

#endif
