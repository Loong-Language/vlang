//===--- LangOptions.h - C Language Family Language Options -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines the vlang::LangOptions interface.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_LANGOPTIONS_H
#define LLVM_VLANG_LANGOPTIONS_H

#include "vlang/Basic/CommentOptions.h"
#include "vlang/Basic/LLVM.h"
#include "vlang/Basic/Visibility.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include <string>

namespace vlang {

struct SanitizerOptions {
#define SANITIZER(NAME, ID) unsigned ID : 1;
#include "vlang/Basic/Sanitizers.def"

  /// \brief Cached set of sanitizer options with all sanitizers disabled.
  static const SanitizerOptions Disabled;
};

/// Bitfields of LangOptions, split out from LangOptions in order to ensure that
/// this large collection of bitfields is a trivial class type.
class LangOptionsBase {
public:
  // Define simple language options (with no accessors).
#define LANGOPT(Name, Bits, Default, Description) unsigned Name : Bits;
#define ENUM_LANGOPT(Name, Type, Bits, Default, Description)
#include "vlang/Basic/LangOptions.def"

  SanitizerOptions Sanitize;
protected:
  // Define language options of enumeration type. These are private, and will
  // have accessors (below).
#define LANGOPT(Name, Bits, Default, Description)
#define ENUM_LANGOPT(Name, Type, Bits, Default, Description) \
  unsigned Name : Bits;
#include "vlang/Basic/LangOptions.def"
};

/// \brief Keeps track of the various options that can be
/// enabled, which controls the dialect of C or C++ that is accepted.
class LangOptions : public RefCountedBase<LangOptions>, public LangOptionsBase {
public:
  typedef vlang::Visibility Visibility;
  
  enum GCMode { NonGC, GCOnly, HybridGC };
  enum StackProtectorMode { SSPOff, SSPOn, SSPReq };
  
  enum SignedOverflowBehaviorTy {
    SOB_Undefined,  // Default C standard behavior.
    SOB_Defined,    // -fwrapv
    SOB_Trapping    // -ftrapv
  };

public:

  /// \brief Options for parsing comments.
  CommentOptions CommentOpts;
  
  LangOptions();

  // Define accessors/mutators for language options of enumeration type.
#define LANGOPT(Name, Bits, Default, Description) 
#define ENUM_LANGOPT(Name, Type, Bits, Default, Description) \
  Type get##Name() const { return static_cast<Type>(Name); } \
  void set##Name(Type Value) { Name = static_cast<unsigned>(Value); }  
#include "vlang/Basic/LangOptions.def"

  /// \brief Reset all of the options that are not considered when building a
  /// module.
  void resetNonModularOptions();
};

/// \brief Describes the kind of translation unit being processed.
enum TranslationUnitKind {
  /// \brief The translation unit is a complete translation unit.
  TU_Complete,
  /// \brief The translation unit is a prefix to a translation unit, and is
  /// not complete.
  TU_Prefix
};
  
}  // end namespace vlang

#endif
