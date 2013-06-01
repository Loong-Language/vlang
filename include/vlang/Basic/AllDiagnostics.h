//===--- AllDiagnostics.h - Aggregate Diagnostic headers --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Includes all the separate Diagnostic headers & some related helpers.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_ALL_DIAGNOSTICS_H
#define LLVM_VLANG_ALL_DIAGNOSTICS_H

#include "vlang/Lex/LexDiagnostic.h"
#include "vlang/Frontend/FrontendDiagnostic.h"
#include "vlang/Parse/ParseDiagnostic.h"

namespace vlang {
template <size_t SizeOfStr, typename FieldType>
class StringSizerHelper {
  char FIELD_TOO_SMALL[SizeOfStr <= FieldType(~0U) ? 1 : -1];
public:
  enum { Size = SizeOfStr };
};
} // end namespace vlang 

#define STR_SIZE(str, fieldTy) vlang::StringSizerHelper<sizeof(str)-1, \
                                                        fieldTy>::Size 

#endif
