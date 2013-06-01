//===-- FrontendAction.h - Pluggable Frontend Action Interface --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_FRONTEND_PLUGINFRONTENDACTION_H
#define LLVM_VLANG_FRONTEND_PLUGINFRONTENDACTION_H

#include "vlang/Frontend/FrontendAction.h"
#include "llvm/Support/Registry.h"

namespace vlang {

/// The frontend plugin registry.
typedef llvm::Registry<PluginASTAction> FrontendPluginRegistry;

} // end namespace vlang

#endif
