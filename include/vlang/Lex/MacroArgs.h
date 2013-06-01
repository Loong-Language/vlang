//===--- MacroArgs.h - Formal argument info for Macros ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the MacroArgs interface.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_MACROARGS_H
#define LLVM_VLANG_MACROARGS_H

#include "vlang/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include <vector>

namespace vlang {
  class MacroInfo;
  class Preprocessor;
  class Token;
  class SourceLocation;

/// MacroArgs - An instance of this class captures information about
/// the formal arguments specified to a function-like macro invocation.
class MacroArgs {
  /// NumUnexpArgTokens - The number of raw, unexpanded tokens for the
  /// arguments.  All of the actual argument tokens are allocated immediately
  /// after the MacroArgs object in memory.  This is all of the arguments
  /// concatenated together, with 'EOF' markers at the end of each argument.
  unsigned NumUnexpArgTokens;
  
  /// PreExpArgTokens - Pre-expanded tokens for arguments that need them.  Empty
  /// if not yet computed.  This includes the EOF marker at the end of the
  /// stream.
  std::vector<std::vector<Token> > PreExpArgTokens;

  /// StringifiedArgs - This contains arguments in 'stringified' form.  If the
  /// stringified form of an argument has not yet been computed, this is empty.
  std::vector<Token> StringifiedArgs;

  /// ArgCache - This is a linked list of MacroArgs objects that the
  /// Preprocessor owns which we use to avoid thrashing malloc/free.
  MacroArgs *ArgCache;
  
  MacroArgs(unsigned NumToks)
    : NumUnexpArgTokens(NumToks), ArgCache(0) {}
  ~MacroArgs() {}
public:
  /// MacroArgs ctor function - Create a new MacroArgs object with the specified
  /// macro and argument info.
  static MacroArgs *create(const MacroInfo *MI,
                           ArrayRef<Token> UnexpArgTokens,
                           Preprocessor &PP);

  /// destroy - Destroy and deallocate the memory for this object.
  ///
  void destroy(Preprocessor &PP);

  /// ArgNeedsPreexpansion - If we can prove that the argument won't be affected
  /// by pre-expansion, return false.  Otherwise, conservatively return true.
  bool ArgNeedsPreexpansion(const Token *ArgTok, Preprocessor &PP) const;

  /// getUnexpArgument - Return a pointer to the first token of the unexpanded
  /// token list for the specified formal.
  ///
  const Token *getUnexpArgument(unsigned Arg) const;

  /// getArgLength - Given a pointer to an expanded or unexpanded argument,
  /// return the number of tokens, not counting the EOF, that make up the
  /// argument.
  static unsigned getArgLength(const Token *ArgPtr);

  /// getPreExpArgument - Return the pre-expanded form of the specified
  /// argument.
  const std::vector<Token> &
    getPreExpArgument(unsigned Arg, const MacroInfo *MI, Preprocessor &PP);

  /// getStringifiedArgument - Compute, cache, and return the specified argument
  /// that has been 'stringified' as required by the # operator.
  const Token &getStringifiedArgument(unsigned ArgNo, Preprocessor &PP,
                                      SourceLocation ExpansionLocStart,
                                      SourceLocation ExpansionLocEnd);

  /// getNumArguments - Return the number of arguments passed into this macro
  /// invocation.
  unsigned getNumArguments() const { return NumUnexpArgTokens; }


  /// StringifyArgument - Implement C99 6.10.3.2p2, converting a sequence of
  /// tokens into the literal string token that should be produced by the C #
  /// preprocessor operator.
  ///
  static Token StringifyArgument(const Token *ArgToks,
                                 Preprocessor &PP,
                                 SourceLocation ExpansionLocStart,
                                 SourceLocation ExpansionLocEnd);
  
  
  /// deallocate - This should only be called by the Preprocessor when managing
  /// its freelist.
  MacroArgs *deallocate();
};

}  // end namespace vlang

#endif
