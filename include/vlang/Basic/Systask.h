//===--- Systasks.h - Systask function header -------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines enum values for all the target-independent systask
/// functions.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_BASIC_SYSTASKS_H
#define LLVM_VLANG_BASIC_SYSTASKS_H

#include "vlang/Basic/LLVM.h"
#include <cstring>

namespace vlang {
  class TargetInfo;
  class IdentifierTable;
  class ASTContext;
  class QualType;
  class LangOptions;
  
  enum LanguageID {
    VERILOG_LANG = 0x1,
    SYSVERILOG_LANG = 0x2,
    ALL_LANGUAGES = (VERILOG_LANG|SYSVERILOG_LANG)
  };
  
namespace Systask {
enum ID {
  NotSystask  = 0,      // This is not a systask function.
#define SYSTASK(ID, TYPE, ATTR, LANGUAGE) ST##ID,
#include "vlang/Basic/Systask.def"
  FirstTSSystask
};

struct Info {
  const char *Name, *Type, *Attributes;
  LanguageID systask_lang;

  bool operator==(const Info &RHS) const {
    return !strcmp(Name, RHS.Name) &&
           !strcmp(Type, RHS.Type) &&
           !strcmp(Attributes, RHS.Attributes);
  }
  bool operator!=(const Info &RHS) const { return !(*this == RHS); }
};

/// \brief Holds information about both target-independent and
/// target-specific systasks, allowing easy queries by clients.
class Context {
  const Info *TSRecords;
  unsigned NumTSRecords;
public:
  Context();

  /// \brief Perform target-specific initialization
  void InitializeTarget();
  
  /// \brief Mark the identifiers for all the systasks with their
  /// appropriate systask ID # and mark any non-portable systask identifiers as
  /// such.
  void InitializeSystasks(IdentifierTable &Table, const LangOptions& LangOpts);

  /// \brief Popular the vector with the names of all of the systasks.
  void GetSystaskNames(SmallVectorImpl<const char *> &Names);

  /// \brief Return the identifier name for the specified systask,
  /// e.g. "__systask_abs".
  const char *GetName(unsigned ID) const {
    return GetRecord(ID).Name;
  }

  /// \brief Get the type descriptor string for the specified systask.
  const char *GetTypeString(unsigned ID) const {
    return GetRecord(ID).Type;
  }

  /// \brief Return true if this function has no side effects and doesn't
  /// read memory.
  bool isConst(unsigned ID) const {
    return strchr(GetRecord(ID).Attributes, 'c') != 0;
  }

  /// \brief Return true if we know this systask never returns.
  bool isNoReturn(unsigned ID) const {
    return strchr(GetRecord(ID).Attributes, 'r') != 0;
  }

  /// \brief Returns true if this systask does not perform the side-effects
  /// of its arguments.
  bool isUnevaluated(unsigned ID) const {
    return strchr(GetRecord(ID).Attributes, 'u') != 0;
  }

  /// \brief Completely forget that the given ID was ever considered a systask,
  /// e.g., because the user provided a conflicting signature.
  void ForgetSystask(unsigned ID, IdentifierTable &Table);
  
  /// \brief Determine whether this systask is like printf in its
  /// formatting rules and, if so, set the index to the format string
  /// argument and whether this function as a va_list argument.
  bool isPrintfLike(unsigned ID, unsigned &FormatIdx, bool &HasVAListArg);

  /// \brief Determine whether this systask is like scanf in its
  /// formatting rules and, if so, set the index to the format string
  /// argument and whether this function as a va_list argument.
  bool isScanfLike(unsigned ID, unsigned &FormatIdx, bool &HasVAListArg);

  /// \brief Return true if this function has no side effects and doesn't
  /// read memory, except for possibly errno.
  ///
  /// Such functions can be const when the MathErrno lang option is disabled.
  bool isConstWithoutErrno(unsigned ID) const {
    return strchr(GetRecord(ID).Attributes, 'e') != 0;
  }

private:
  const Info &GetRecord(unsigned ID) const;
};

}
} // end namespace vlang
#endif
