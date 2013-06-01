//===--- TokenLexer.cpp - Lex from a token stream -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the TokenLexer interface.
//
//===----------------------------------------------------------------------===//

#include "vlang/Lex/MacroArgs.h"
#include "vlang/Lex/LexDiagnostic.h"
#include "vlang/Lex/MacroInfo.h"
#include "vlang/Lex/Preprocessor.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/SaveAndRestore.h"
#include <algorithm>

using namespace vlang;

/// MacroArgs ctor function - This destroys the vector passed in.
MacroArgs *MacroArgs::create(const MacroInfo *MI,
                             ArrayRef<Token> UnexpArgTokens,
                             Preprocessor &PP) {
  assert(MI->isFunctionLike() &&
         "Can't have args for an object-like macro!");
  MacroArgs **ResultEnt = 0;
  unsigned ClosestMatch = ~0U;
  
  // See if we have an entry with a big enough argument list to reuse on the
  // free list.  If so, reuse it.
  for (MacroArgs **Entry = &PP.MacroArgCache; *Entry;
       Entry = &(*Entry)->ArgCache)
    if ((*Entry)->NumUnexpArgTokens >= UnexpArgTokens.size() &&
        (*Entry)->NumUnexpArgTokens < ClosestMatch) {
      ResultEnt = Entry;
      
      // If we have an exact match, use it.
      if ((*Entry)->NumUnexpArgTokens == UnexpArgTokens.size())
        break;
      // Otherwise, use the best fit.
      ClosestMatch = (*Entry)->NumUnexpArgTokens;
    }
  
  MacroArgs *Result;
  if (ResultEnt == 0) {
    // Allocate memory for a MacroArgs object with the lexer tokens at the end.
    Result = (MacroArgs*)malloc(sizeof(MacroArgs) + 
                                UnexpArgTokens.size() * sizeof(Token));

    // Construct the MacroArgs object.
    new (Result) MacroArgs(UnexpArgTokens.size());
  } else {
    Result = *ResultEnt;
    // Unlink this node from the preprocessors singly linked list.
    *ResultEnt = Result->ArgCache;
    Result->NumUnexpArgTokens = UnexpArgTokens.size();
  }

  // Copy the actual unexpanded tokens to immediately after the result ptr.
  if (!UnexpArgTokens.empty())
    std::copy(UnexpArgTokens.begin(), UnexpArgTokens.end(), 
              const_cast<Token*>(Result->getUnexpArgument(0)));

  return Result;
}

/// destroy - Destroy and deallocate the memory for this object.
///
void MacroArgs::destroy(Preprocessor &PP) {
  StringifiedArgs.clear();

  // Don't clear PreExpArgTokens, just clear the entries.  Clearing the entries
  // would deallocate the element vectors.
  for (unsigned i = 0, e = PreExpArgTokens.size(); i != e; ++i)
    PreExpArgTokens[i].clear();
  
  // Add this to the preprocessor's free list.
  ArgCache = PP.MacroArgCache;
  PP.MacroArgCache = this;
}

/// deallocate - This should only be called by the Preprocessor when managing
/// its freelist.
MacroArgs *MacroArgs::deallocate() {
  MacroArgs *Next = ArgCache;
  
  // Run the dtor to deallocate the vectors.
  this->~MacroArgs();
  // Release the memory for the object.
  free(this);
  
  return Next;
}


/// getArgLength - Given a pointer to an expanded or unexpanded argument,
/// return the number of tokens, not counting the EOF, that make up the
/// argument.
unsigned MacroArgs::getArgLength(const Token *ArgPtr) {
  unsigned NumArgTokens = 0;
  for (; ArgPtr->isNot(tok::eof); ++ArgPtr)
    ++NumArgTokens;
  return NumArgTokens;
}


/// getUnexpArgument - Return the unexpanded tokens for the specified formal.
///
const Token *MacroArgs::getUnexpArgument(unsigned Arg) const {
  // The unexpanded argument tokens start immediately after the MacroArgs object
  // in memory.
  const Token *Start = (const Token *)(this+1);
  const Token *Result = Start;
  // Scan to find Arg.
  for (; Arg; ++Result) {
    assert(Result < Start+NumUnexpArgTokens && "Invalid arg #");
    if (Result->is(tok::eof))
      --Arg;
  }
  assert(Result < Start+NumUnexpArgTokens && "Invalid arg #");
  return Result;
}


/// ArgNeedsPreexpansion - If we can prove that the argument won't be affected
/// by pre-expansion, return false.  Otherwise, conservatively return true.
bool MacroArgs::ArgNeedsPreexpansion(const Token *ArgTok,
                                     Preprocessor &PP) const {
  // If there are no identifiers in the argument list, or if the identifiers are
  // known to not be macros, pre-expansion won't modify it.
  for (; ArgTok->isNot(tok::eof); ++ArgTok)
    if (IdentifierInfo *II = ArgTok->getIdentifierInfo()) {
      if (II->hasMacroDefinition() && PP.getMacroInfo(II)->isEnabled())
        // Return true even though the macro could be a function-like macro
        // without a following '(' token.
        return true;
    }
  return false;
}

/// getPreExpArgument - Return the pre-expanded form of the specified
/// argument.
const std::vector<Token> &
MacroArgs::getPreExpArgument(unsigned Arg, const MacroInfo *MI, 
                             Preprocessor &PP) {
  assert(Arg < MI->getNumArgs() && "Invalid argument number!");

  // If we have already computed this, return it.
  if (PreExpArgTokens.size() < MI->getNumArgs())
    PreExpArgTokens.resize(MI->getNumArgs());
  
  std::vector<Token> &Result = PreExpArgTokens[Arg];
  if (!Result.empty()) return Result;

  SaveAndRestore<bool> PreExpandingMacroArgs(PP.InMacroArgPreExpansion, true);

  const Token *AT = getUnexpArgument(Arg);
  unsigned NumToks = getArgLength(AT)+1;  // Include the EOF.

  // Otherwise, we have to pre-expand this argument, populating Result.  To do
  // this, we set up a fake TokenLexer to lex from the unexpanded argument
  // list.  With this installed, we lex expanded tokens until we hit the EOF
  // token at the end of the unexp list.
  PP.EnterTokenStream(AT, NumToks, false /*disable expand*/,
                      false /*owns tokens*/);

  // Lex all of the macro-expanded tokens into Result.
  do {
    Result.push_back(Token());
    Token &Tok = Result.back();
    PP.Lex(Tok);
  } while (Result.back().isNot(tok::eof));

  // Pop the token stream off the top of the stack.  We know that the internal
  // pointer inside of it is to the "end" of the token stream, but the stack
  // will not otherwise be popped until the next token is lexed.  The problem is
  // that the token may be lexed sometime after the vector of tokens itself is
  // destroyed, which would be badness.
  if (PP.InCachingLexMode())
    PP.ExitCachingLexMode();
  PP.RemoveTopOfLexerStack();
  return Result;
}


/// StringifyArgument - Implement C99 6.10.3.2p2, converting a sequence of
/// tokens into the literal string token that should be produced by the C #
/// preprocessor operator. 
///
Token MacroArgs::StringifyArgument(const Token *ArgToks,
                                   Preprocessor &PP,
                                   SourceLocation ExpansionLocStart,
                                   SourceLocation ExpansionLocEnd) {
  Token Tok;
  Tok.startToken();
  Tok.setKind(tok::string_literal);

  const Token *ArgTokStart = ArgToks;

  // Stringify all the tokens.
  SmallString<128> Result;
  Result += "\"";

  bool isFirst = true;
  for (; ArgToks->isNot(tok::eof); ++ArgToks) {
    const Token &Tok = *ArgToks;
    if (!isFirst && (Tok.hasLeadingSpace() || Tok.isAtStartOfLine()))
      Result += ' ';
    isFirst = false;

    // If this is a string or character constant, escape the token as specified
    // by 6.10.3.2p2.
    if (tok::isStringLiteral(Tok.getKind()) ){ // "foo", u8R"x(foo)x"_bar, etc.
      bool Invalid = false;
      std::string TokStr = PP.getSpelling(Tok, &Invalid);
      if (!Invalid) {
        std::string Str = Lexer::Stringify(TokStr);
        Result.append(Str.begin(), Str.end());
      }
    } else if (Tok.is(tok::code_completion)) {
      PP.CodeCompleteNaturalLanguage();
    } else {
      // Otherwise, just append the token.  Do some gymnastics to get the token
      // in place and avoid copies where possible.
      unsigned CurStrLen = Result.size();
      Result.resize(CurStrLen+Tok.getLength());
      const char *BufPtr = &Result[CurStrLen];
      bool Invalid = false;
      unsigned ActualTokLen = PP.getSpelling(Tok, BufPtr, &Invalid);

      if (!Invalid) {
        // If getSpelling returned a pointer to an already uniqued version of
        // the string instead of filling in BufPtr, memcpy it onto our string.
        if (BufPtr != &Result[CurStrLen])
          memcpy(&Result[CurStrLen], BufPtr, ActualTokLen);

        // If the token was dirty, the spelling may be shorter than the token.
        if (ActualTokLen != Tok.getLength())
          Result.resize(CurStrLen+ActualTokLen);
      }
    }
  }

  // If the last character of the string is a \, and if it isn't escaped, this
  // is an invalid string literal, diagnose it as specified in C99.
  if (Result.back() == '\\') {
    // Count the number of consequtive \ characters.  If even, then they are
    // just escaped backslashes, otherwise it's an error.
    unsigned FirstNonSlash = Result.size()-2;
    // Guaranteed to find the starting " if nothing else.
    while (Result[FirstNonSlash] == '\\')
      --FirstNonSlash;
    if ((Result.size()-1-FirstNonSlash) & 1) {
      // Diagnose errors for things like: #define F(X) #X   /   F(\)
      PP.Diag(ArgToks[-1], diag::pp_invalid_string_literal);
      Result.pop_back();  // remove one of the \'s.
    }
  }
  Result += '"';

  PP.CreateString(Result, Tok,
                  ExpansionLocStart, ExpansionLocEnd);
  return Tok;
}

/// getStringifiedArgument - Compute, cache, and return the specified argument
/// that has been 'stringified' as required by the # operator.
const Token &MacroArgs::getStringifiedArgument(unsigned ArgNo,
                                               Preprocessor &PP,
                                               SourceLocation ExpansionLocStart,
                                               SourceLocation ExpansionLocEnd) {
  assert(ArgNo < NumUnexpArgTokens && "Invalid argument number!");
  if (StringifiedArgs.empty()) {
    StringifiedArgs.resize(getNumArguments());
    memset((void*)&StringifiedArgs[0], 0,
           sizeof(StringifiedArgs[0])*getNumArguments());
  }
  if (StringifiedArgs[ArgNo].isNot(tok::string_literal))
    StringifiedArgs[ArgNo] = StringifyArgument(getUnexpArgument(ArgNo), PP,
                                               ExpansionLocStart,
                                               ExpansionLocEnd);
  return StringifiedArgs[ArgNo];
}
