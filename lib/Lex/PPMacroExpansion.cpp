//===--- MacroExpansion.cpp - Top level Macro Expansion -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the top level handling of macro expasion for the
// preprocessor.
//
//===----------------------------------------------------------------------===//

#include "vlang/Lex/Preprocessor.h"
#include "vlang/Lex/MacroArgs.h"
#include "vlang/Basic/FileManager.h"
#include "vlang/Basic/SourceManager.h"
#include "vlang/Basic/TargetInfo.h"
#include "vlang/Lex/CodeCompletionHandler.h"
#include "vlang/Lex/ExternalPreprocessorSource.h"
#include "vlang/Lex/LexDiagnostic.h"
#include "vlang/Lex/MacroInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdio>
#include <ctime>
using namespace vlang;

MacroDirective *
Preprocessor::getMacroDirectiveHistory(const IdentifierInfo *II) const {
  assert(II->hadMacroDefinition() && "Identifier has not been not a macro!");

  macro_iterator Pos = Macros.find(II);
  assert(Pos != Macros.end() && "Identifier macro info is missing!");
  return Pos->second;
}

void Preprocessor::appendMacroDirective(IdentifierInfo *II, MacroDirective *MD){
  assert(MD && "MacroDirective should be non-zero!");
  assert(!MD->getPrevious() && "Already attached to a MacroDirective history.");

  MacroDirective *&StoredMD = Macros[II];
  MD->setPrevious(StoredMD);
  StoredMD = MD;
  II->setHasMacroDefinition(MD->isDefined());
}

/// isTrivialSingleTokenExpansion - Return true if MI, which has a single token
/// in its expansion, currently expands to that token literally.
static bool isTrivialSingleTokenExpansion(const MacroInfo *MI,
                                          const IdentifierInfo *MacroIdent,
                                          Preprocessor &PP) {
  IdentifierInfo *II = MI->getReplacementToken(0).getIdentifierInfo();

  // If the token isn't an identifier, it's always literally expanded.
  if (II == 0) return true;

  // If the identifier is a macro, and if that macro is enabled, it may be
  // expanded so it's not a trivial expansion.
  if (II->hasMacroDefinition() && PP.getMacroInfo(II)->isEnabled() &&
      // Fast expanding "#define X X" is ok, because X would be disabled.
      II != MacroIdent)
    return false;

  // If this is an object-like macro invocation, it is safe to trivially expand
  // it.
  if (MI->isObjectLike()) return true;

  // If this is a function-like macro invocation, it's safe to trivially expand
  // as long as the identifier is not a macro argument.
  for (MacroInfo::arg_iterator I = MI->arg_begin(), E = MI->arg_end();
       I != E; ++I)
    if (*I == II)
      return false;   // Identifier is a macro argument.

  return true;
}


/// isNextPPTokenLParen - Determine whether the next preprocessor token to be
/// lexed is a '('.  If so, consume the token and return true, if not, this
/// method should have no observable side-effect on the lexed tokens.
bool Preprocessor::isNextPPTokenLParen() {
  // Do some quick tests for rejection cases.
  unsigned Val;
  if (CurLexer)
    Val = CurLexer->isNextPPTokenLParen();
  else
    Val = CurTokenLexer->isNextTokenLParen();

  if (Val == 2) {
    // We have run off the end.  If it's a source file we don't
    // examine enclosing ones (C99 5.1.1.2p4).  Otherwise walk up the
    // macro stack.
    if (CurPPLexer)
      return false;
    for (unsigned i = IncludeMacroStack.size(); i != 0; --i) {
      IncludeStackInfo &Entry = IncludeMacroStack[i-1];
      if (Entry.TheLexer)
        Val = Entry.TheLexer->isNextPPTokenLParen();
      else
        Val = Entry.TheTokenLexer->isNextTokenLParen();

      if (Val != 2)
        break;

      // Ran off the end of a source file?
      if (Entry.ThePPLexer)
        return false;
    }
  }

  // Okay, if we know that the token is a '(', lex it and return.  Otherwise we
  // have found something that isn't a '(' or we found the end of the
  // translation unit.  In either case, return false.
  return Val == 1;
}

/// HandleMacroExpandedIdentifier - If an identifier token is read that is to be
/// expanded as a macro, handle it and return the next token as 'Identifier'.
bool Preprocessor::HandleMacroExpandedIdentifier(Token &Identifier,
                                                 MacroDirective *MD) {
  MacroDirective::DefInfo Def = MD->getDefinition();
  assert(Def.isValid());
  MacroInfo *MI = Def.getMacroInfo();

  // If this is a macro expansion in the "`if !defined(x)" line for the file,
  // then the macro could expand to different things in other contexts, we need
  // to disable the optimization in this case.
  if (CurPPLexer) CurPPLexer->MIOpt.ExpandedMacro();

  /// Args - If this is a function-like macro expansion, this contains,
  /// for each macro argument, the list of tokens that were provided to the
  /// invocation.
  MacroArgs *Args = 0;

  // Remember where the end of the expansion occurred.  For an object-like
  // macro, this is the identifier.  For a function-like macro, this is the ')'.
  SourceLocation ExpansionEnd = Identifier.getLocation();

  // If this is a function-like macro, read the arguments.
  if (MI->isFunctionLike()) {
     // TODO: Check for '('
    // C99 6.10.3p10: If the preprocessing token immediately after the macro
    // name isn't a '(', this macro should not be expanded.
    if (!isNextPPTokenLParen())
      return false;

    // Remember that we are now parsing the arguments to a macro invocation.
    // Preprocessor directives used inside macro arguments are not portable, and
    // this enables the warning.
    InMacroArgs = true;
    Args = ReadFunctionLikeMacroArgs(Identifier, MI, ExpansionEnd);

    // Finished parsing args.
    InMacroArgs = false;

    // If there was an error parsing the arguments, bail out.
    if (Args == 0) return false;

    ++NumFnMacroExpanded;
  } else {
    ++NumMacroExpanded;
  }

  // Notice that this macro has been used.
  markMacroAsUsed(MI);

  // Remember where the token is expanded.
  SourceLocation ExpandLoc = Identifier.getLocation();
  SourceRange ExpansionRange(ExpandLoc, ExpansionEnd);

  if (Callbacks) {
    if (InMacroArgs) {
      // We can have macro expansion inside a conditional directive while
      // reading the function macro arguments. To ensure, in that case, that
      // MacroExpands callbacks still happen in source order, queue this
      // callback to have it happen after the function macro callback.
      DelayedMacroExpandsCallbacks.push_back(
                              MacroExpandsInfo(Identifier, MD, ExpansionRange));
    } else {
      Callbacks->MacroExpands(Identifier, MD, ExpansionRange, Args);
      if (!DelayedMacroExpandsCallbacks.empty()) {
        for (unsigned i=0, e = DelayedMacroExpandsCallbacks.size(); i!=e; ++i) {
          MacroExpandsInfo &Info = DelayedMacroExpandsCallbacks[i];
          // FIXME: We lose macro args info with delayed callback.
          Callbacks->MacroExpands(Info.Tok, Info.MD, Info.Range, /*Args=*/0);
        }
        DelayedMacroExpandsCallbacks.clear();
      }
    }
  }

  // If the macro definition is ambiguous, complain.
  if (Def.getDirective()->isAmbiguous()) {
    Diag(Identifier, diag::warn_pp_ambiguous_macro)
      << Identifier.getIdentifierInfo();
    Diag(MI->getDefinitionLoc(), diag::note_pp_ambiguous_macro_chosen)
      << Identifier.getIdentifierInfo();
    for (MacroDirective::DefInfo PrevDef = Def.getPreviousDefinition();
         PrevDef && !PrevDef.isUndefined();
         PrevDef = PrevDef.getPreviousDefinition()) {
      if (PrevDef.getDirective()->isAmbiguous()) {
        Diag(PrevDef.getMacroInfo()->getDefinitionLoc(),
             diag::note_pp_ambiguous_macro_other)
          << Identifier.getIdentifierInfo();
      }
    }
  }

  // If we started lexing a macro, enter the macro expansion body.

  // If this macro expands to no tokens, don't bother to push it onto the
  // expansion stack, only to take it right back off.
  if (MI->getNumTokens() == 0) {
    // No need for arg info.
    if (Args) Args->destroy(*this);

    // Ignore this macro use, just return the next token in the current
    // buffer.
    bool HadLeadingSpace = Identifier.hasLeadingSpace();
    bool IsAtStartOfLine = Identifier.isAtStartOfLine();

    Lex(Identifier);

    // If the identifier isn't on some OTHER line, inherit the leading
    // whitespace/first-on-a-line property of this token.  This handles
    // stuff like "! XX," -> "! ," and "   XX," -> "    ,", when XX is
    // empty.
    if (!Identifier.isAtStartOfLine()) {
      if (IsAtStartOfLine) Identifier.setFlag(Token::StartOfLine);
      if (HadLeadingSpace) Identifier.setFlag(Token::LeadingSpace);
    }
    Identifier.setFlag(Token::LeadingEmptyMacro);
    ++NumFastMacroExpanded;
    return false;

  } else if (MI->getNumTokens() == 1 &&
             isTrivialSingleTokenExpansion(MI, Identifier.getIdentifierInfo(),
                                           *this)) {
    // Otherwise, if this macro expands into a single trivially-expanded
    // token: expand it now.  This handles common cases like
    // "`define VAL 42".

    // No need for arg info.
    if (Args) Args->destroy(*this);

    // Propagate the isAtStartOfLine/hasLeadingSpace markers of the macro
    // identifier to the expanded token.
    bool isAtStartOfLine = Identifier.isAtStartOfLine();
    bool hasLeadingSpace = Identifier.hasLeadingSpace();

    // Replace the result token.
    Identifier = MI->getReplacementToken(0);

    // Restore the StartOfLine/LeadingSpace markers.
    Identifier.setFlagValue(Token::StartOfLine , isAtStartOfLine);
    Identifier.setFlagValue(Token::LeadingSpace, hasLeadingSpace);

    // Update the tokens location to include both its expansion and physical
    // locations.
    SourceLocation Loc =
      SourceMgr.createExpansionLoc(Identifier.getLocation(), ExpandLoc,
                                   ExpansionEnd,Identifier.getLength());
    Identifier.setLocation(Loc);

    // If this is a disabled macro or `define X X, we must mark the result as
    // unexpandable.
    if (IdentifierInfo *NewII = Identifier.getIdentifierInfo()) {
      if (MacroInfo *NewMI = getMacroInfo(NewII))
        if (!NewMI->isEnabled() || NewMI == MI) {
          Identifier.setFlag(Token::DisableExpand);
          // Don't warn for "#define X X" like "#define bool bool" from
          // stdbool.h.
          if (NewMI != MI || MI->isFunctionLike())
            Diag(Identifier, diag::pp_disabled_macro_expansion);
        }
    }

    // Since this is not an identifier token, it can't be macro expanded, so
    // we're done.
    ++NumFastMacroExpanded;
    return true;
  }

  // Start expanding the macro.
  EnterMacro(Identifier, ExpansionEnd, MI, Args);

  // Now that the macro is at the top of the include stack, ask the
  // preprocessor to read the next token from it.
  Lex(Identifier);
  return false;
}

/// ReadFunctionLikeMacroArgs - After reading "MACRO" and knowing that the next
/// token is the '(' of the macro, this method is invoked to read all of the
/// actual arguments specified for the macro invocation.  This returns null on
/// error.
MacroArgs *Preprocessor::ReadFunctionLikeMacroArgs(Token &MacroName,
                                                   MacroInfo *MI,
                                                   SourceLocation &MacroEnd) {
  // The number of fixed arguments to parse.
  unsigned NumFixedArgsLeft = MI->getNumArgs();

  // Outer loop, while there are more arguments, keep reading them.
  Token Tok;

  // Read arguments as unexpanded tokens.  This avoids issues, e.g., where
  // an argument value in a macro could expand to ',' or '(' or ')'.
  LexUnexpandedToken(Tok);
  assert(Tok.is(tok::l_paren) && "Error computing l-paren-ness?");

  // ArgTokens - Build up a list of tokens that make up each argument.  Each
  // argument is separated by an EOF token.  Use a SmallVector so we can avoid
  // heap allocations in the common case.
  SmallVector<Token, 64> ArgTokens;
  bool ContainsCodeCompletionTok = false;

  unsigned NumActuals = 0;
  while (Tok.isNot(tok::r_paren)) {
    if (ContainsCodeCompletionTok && (Tok.is(tok::eof) || Tok.is(tok::eod)))
      break;

    assert((Tok.is(tok::l_paren) || Tok.is(tok::comma)) &&
           "only expect argument separators here");

    unsigned ArgTokenStart = ArgTokens.size();
    SourceLocation ArgStartLoc = Tok.getLocation();

    // C99 6.10.3p11: Keep track of the number of l_parens we have seen.  Note
    // that we already consumed the first one.
    unsigned NumParens = 0;

    while (1) {
      // Read arguments as unexpanded tokens.  This avoids issues, e.g., where
      // an argument value in a macro could expand to ',' or '(' or ')'.
      LexUnexpandedToken(Tok);

      if (Tok.is(tok::eof) || Tok.is(tok::eod)) { // "#if f(<eof>" & "#if f(\n"
        if (!ContainsCodeCompletionTok) {
          Diag(MacroName, diag::err_unterm_macro_invoc);
          Diag(MI->getDefinitionLoc(), diag::note_macro_here)
            << MacroName.getIdentifierInfo();
          // Do not lose the EOF/EOD.  Return it to the client.
          MacroName = Tok;
          return 0;
        } else {
          // Do not lose the EOF/EOD.
          Token *Toks = new Token[1];
          Toks[0] = Tok;
          EnterTokenStream(Toks, 1, true, true);
          break;
        }
      } else if (Tok.is(tok::r_paren)) {
        // If we found the ) token, the macro arg list is done.
        if (NumParens-- == 0) {
          MacroEnd = Tok.getLocation();
          break;
        }
      } else if (Tok.is(tok::l_paren)) {
        ++NumParens;
      } else if (Tok.is(tok::comma) && NumParens == 0) {
        // Comma ends this argument if there are more fixed arguments expected.
        break;
      } else if (Tok.is(tok::comment) && !KeepMacroComments) {
        // If this is a comment token in the argument list and we're just in
        // -C mode (not -CC mode), discard the comment.
        continue;
      } else if (Tok.getIdentifierInfo() != 0) {
        // Reading macro arguments can cause macros that we are currently
        // expanding from to be popped off the expansion stack.  Doing so causes
        // them to be reenabled for expansion.  Here we record whether any
        // identifiers we lex as macro arguments correspond to disabled macros.
        // If so, we mark the token as noexpand.  This is a subtle aspect of
        // C99 6.10.3.4p2.
        if (MacroInfo *MI = getMacroInfo(Tok.getIdentifierInfo()))
          if (!MI->isEnabled())
            Tok.setFlag(Token::DisableExpand);
      } else if (Tok.is(tok::code_completion)) {
        ContainsCodeCompletionTok = true;
        if (CodeComplete)
          CodeComplete->CodeCompleteMacroArgument(MacroName.getIdentifierInfo(),
                                                  MI, NumActuals);
        // Don't mark that we reached the code-completion point because the
        // parser is going to handle the token and there will be another
        // code-completion callback.
      }

      ArgTokens.push_back(Tok);
    }

    // If this was an empty argument list foo(), don't add this as an empty
    // argument.
    if (ArgTokens.empty() && Tok.getKind() == tok::r_paren)
      break;

    // Too many args were specified, emit an error.
    if (NumFixedArgsLeft == 0) {
      if (ArgTokens.size() != ArgTokenStart)
        ArgStartLoc = ArgTokens[ArgTokenStart].getLocation();

      if (!ContainsCodeCompletionTok) {
        // Emit the diagnostic at the macro name in case there is a missing ).
        // Emitting it at the , could be far away from the macro name.
        Diag(ArgStartLoc, diag::err_too_many_args_in_macro_invoc);
        Diag(MI->getDefinitionLoc(), diag::note_macro_here)
          << MacroName.getIdentifierInfo();
        return 0;
      }
    }

    // Empty arguments are standard in C99 and C++0x, and are supported as an extension in
    // other modes.
    if (ArgTokens.size() == ArgTokenStart)
      Diag(Tok,  diag::ext_empty_fnmacro_arg);

    // Add a marker EOF token to the end of the token list for this argument.
    Token EOFTok;
    EOFTok.startToken();
    EOFTok.setKind(tok::eof);
    EOFTok.setLocation(Tok.getLocation());
    EOFTok.setLength(0);
    ArgTokens.push_back(EOFTok);
    ++NumActuals;
    if (!ContainsCodeCompletionTok || NumFixedArgsLeft != 0) {
      assert(NumFixedArgsLeft != 0 && "Too many arguments parsed");
      --NumFixedArgsLeft;
    }
  }

  // Okay, we either found the r_paren.  Check to see if we parsed too few
  // arguments.
  unsigned MinArgsExpected = MI->getNumArgs();

  if (ContainsCodeCompletionTok) {
    // Recover from not-fully-formed macro invocation during code-completion.
    Token EOFTok;
    EOFTok.startToken();
    EOFTok.setKind(tok::eof);
    EOFTok.setLocation(Tok.getLocation());
    EOFTok.setLength(0);
    for (; NumActuals < MinArgsExpected; ++NumActuals)
      ArgTokens.push_back(EOFTok);
  }

  if (NumActuals < MinArgsExpected) {
    // There are several cases where too few arguments is ok, handle them now.
    if (NumActuals == 0 && MinArgsExpected == 1) {
      // #define A(X)  or  #define A(...)   ---> A()

      // If there is exactly one argument, and that argument is missing,
      // then we have an empty "()" argument empty list.  This is fine, even if
      // the macro expects one argument (the argument is just empty).
    } else if (!ContainsCodeCompletionTok) {
      // Otherwise, emit the error.
      Diag(Tok, diag::err_too_few_args_in_macro_invoc);
      Diag(MI->getDefinitionLoc(), diag::note_macro_here)
        << MacroName.getIdentifierInfo();
      return 0;
    }

    // Add a marker EOF token to the end of the token list for this argument.
    SourceLocation EndLoc = Tok.getLocation();
    Tok.startToken();
    Tok.setKind(tok::eof);
    Tok.setLocation(EndLoc);
    Tok.setLength(0);
    ArgTokens.push_back(Tok);

    // If we expect two arguments, add both as empty.
    if (NumActuals == 0 && MinArgsExpected == 2)
      ArgTokens.push_back(Tok);

  } else if (NumActuals > MinArgsExpected &&
             !ContainsCodeCompletionTok) {
    // Emit the diagnostic at the macro name in case there is a missing ).
    // Emitting it at the , could be far away from the macro name.
    Diag(MacroName, diag::err_too_many_args_in_macro_invoc);
    Diag(MI->getDefinitionLoc(), diag::note_macro_here)
      << MacroName.getIdentifierInfo();
    return 0;
  }

  return MacroArgs::create(MI, ArgTokens, *this);
}

/// \brief Keeps macro expanded tokens for TokenLexers.
//
/// Works like a stack; a TokenLexer adds the macro expanded tokens that is
/// going to lex in the cache and when it finishes the tokens are removed
/// from the end of the cache.
Token *Preprocessor::cacheMacroExpandedTokens(TokenLexer *tokLexer,
                                              ArrayRef<Token> tokens) {
  assert(tokLexer);
  if (tokens.empty())
    return 0;

  size_t newIndex = MacroExpandedTokens.size();
  bool cacheNeedsToGrow = tokens.size() >
                      MacroExpandedTokens.capacity()-MacroExpandedTokens.size(); 
  MacroExpandedTokens.append(tokens.begin(), tokens.end());

  if (cacheNeedsToGrow) {
    // Go through all the TokenLexers whose 'Tokens' pointer points in the
    // buffer and update the pointers to the (potential) new buffer array.
    for (unsigned i = 0, e = MacroExpandingLexersStack.size(); i != e; ++i) {
      TokenLexer *prevLexer;
      size_t tokIndex;
      llvm::tie(prevLexer, tokIndex) = MacroExpandingLexersStack[i];
      prevLexer->Tokens = MacroExpandedTokens.data() + tokIndex;
    }
  }

  MacroExpandingLexersStack.push_back(std::make_pair(tokLexer, newIndex));
  return MacroExpandedTokens.data() + newIndex;
}

void Preprocessor::removeCachedMacroExpandedTokensOfLastLexer() {
  assert(!MacroExpandingLexersStack.empty());
  size_t tokIndex = MacroExpandingLexersStack.back().second;
  assert(tokIndex < MacroExpandedTokens.size());
  // Pop the cached macro expanded tokens from the end.
  MacroExpandedTokens.resize(tokIndex);
  MacroExpandingLexersStack.pop_back();
}

/// ExpandBuiltinMacro - If an identifier token is read that is to be expanded
/// as a builtin macro, handle it and return the next token as 'Tok'.
void Preprocessor::ExpandBuiltinMacro(Token &Tok) {
  // Figure out which token this is.
  IdentifierInfo *II = Tok.getIdentifierInfo();
  assert(II && "Can't be a macro without id info!");

  ++NumBuiltinMacroExpanded;

  SmallString<128> TmpBuffer;
  llvm::raw_svector_ostream OS(TmpBuffer);

  // Set up the return result.
  Tok.setIdentifierInfo(0);
  Tok.clearFlag(Token::NeedsCleaning);

  llvm_unreachable("Unknown identifier!");
  CreateString(OS.str(), Tok, Tok.getLocation(), Tok.getLocation());
}

void Preprocessor::markMacroAsUsed(MacroInfo *MI) {
  // If the 'used' status changed, and the macro requires 'unused' warning,
  // remove its SourceLocation from the warn-for-unused-macro locations.
  if (MI->isWarnIfUnused() && !MI->isUsed())
    WarnUnusedMacroLocs.erase(MI->getDefinitionLoc());
  MI->setIsUsed(true);
}
