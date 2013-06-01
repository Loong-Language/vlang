//===--- PPDirectives.cpp - Directive Handling for Preprocessor -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Implements ` directive processing for the Preprocessor.
///
//===----------------------------------------------------------------------===//

#include "vlang/Lex/Preprocessor.h"
#include "vlang/Basic/FileManager.h"
#include "vlang/Basic/SourceManager.h"
#include "vlang/Lex/CodeCompletionHandler.h"
#include "vlang/Lex/HeaderSearch.h"
#include "vlang/Lex/LexDiagnostic.h"
#include "vlang/Lex/LiteralSupport.h"
#include "vlang/Lex/MacroInfo.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace vlang;

//===----------------------------------------------------------------------===//
// Utility Methods for Preprocessor Directive Handling.
//===----------------------------------------------------------------------===//

MacroInfo *Preprocessor::AllocateMacroInfo() {
  MacroInfoChain *MIChain;

  if (MICache) {
    MIChain = MICache;
    MICache = MICache->Next;
  }
  else {
    MIChain = BP.Allocate<MacroInfoChain>();
  }

  MIChain->Next = MIChainHead;
  MIChain->Prev = 0;
  if (MIChainHead)
    MIChainHead->Prev = MIChain;
  MIChainHead = MIChain;

  return &(MIChain->MI);
}

MacroInfo *Preprocessor::AllocateMacroInfo(SourceLocation L) {
  MacroInfo *MI = AllocateMacroInfo();
  new (MI) MacroInfo(L);
  return MI;
}

DefMacroDirective *
Preprocessor::AllocateDefMacroDirective(MacroInfo *MI, SourceLocation Loc) {
  DefMacroDirective *MD = BP.Allocate<DefMacroDirective>();
  new (MD) DefMacroDirective(MI, Loc);
  return MD;
}

UndefMacroDirective *
Preprocessor::AllocateUndefMacroDirective(SourceLocation UndefLoc) {
  UndefMacroDirective *MD = BP.Allocate<UndefMacroDirective>();
  new (MD) UndefMacroDirective(UndefLoc);
  return MD;
}

VisibilityMacroDirective *
Preprocessor::AllocateVisibilityMacroDirective(SourceLocation Loc,
                                               bool isPublic) {
  VisibilityMacroDirective *MD = BP.Allocate<VisibilityMacroDirective>();
  new (MD) VisibilityMacroDirective(Loc, isPublic);
  return MD;
}

/// \brief Release the specified MacroInfo to be reused for allocating
/// new MacroInfo objects.
void Preprocessor::ReleaseMacroInfo(MacroInfo *MI) {
  MacroInfoChain *MIChain = (MacroInfoChain*) MI;
  if (MacroInfoChain *Prev = MIChain->Prev) {
    MacroInfoChain *Next = MIChain->Next;
    Prev->Next = Next;
    if (Next)
      Next->Prev = Prev;
  }
  else {
    assert(MIChainHead == MIChain);
    MIChainHead = MIChain->Next;
    MIChainHead->Prev = 0;
  }
  MIChain->Next = MICache;
  MICache = MIChain;

  MI->Destroy();
}

/// \brief Read and discard all tokens remaining on the current line until
/// the tok::eod token is found.
void Preprocessor::DiscardUntilEndOfDirective() {
  Token Tmp;
  do {
    LexUnexpandedToken(Tmp);
    assert(Tmp.isNot(tok::eof) && "EOF seen while discarding directive tokens");
  } while (Tmp.isNot(tok::eod));
}

/// \brief Lex and validate a macro name, which occurs after a
/// \`define or \`undef.
///
/// This sets the token kind to eod and discards the rest
/// of the macro line if the macro name is invalid.  \p isDefineUndef is 1 if
/// this is due to a a \`define, 2 if \`undef directive, 0 if it is something
/// else (e.g. \`ifdef).
/// TODO: Support rules from SV2012 22.5
void Preprocessor::ReadMacroName(Token &MacroNameTok, char isDefineUndef) {
  // Read the token, don't allow macro expansion on it.
  LexUnexpandedToken(MacroNameTok);

  if (MacroNameTok.is(tok::code_completion)) {
    if (CodeComplete)
      CodeComplete->CodeCompleteMacroName(isDefineUndef == 1);
    setCodeCompletionReached();
    LexUnexpandedToken(MacroNameTok);
  }
  
  // Missing macro name?
  if (MacroNameTok.is(tok::eod)) {
    Diag(MacroNameTok, diag::err_pp_missing_macro_name);
    return;
  }

  IdentifierInfo *II = MacroNameTok.getIdentifierInfo();
  if (II == 0) {
    bool Invalid = false;
    std::string Spelling = getSpelling(MacroNameTok, &Invalid);
    if (Invalid)
      return;

    const IdentifierInfo &Info = Identifiers.get(Spelling);
    Diag(MacroNameTok, diag::err_pp_macro_not_identifier);

	// Fall through on error.
  } else {
    // Okay, we got a good identifier node.  Return it.
    return;
  }

  // Invalid macro name, read and discard the rest of the line.  Then set the
  // token kind to tok::eod.
  MacroNameTok.setKind(tok::eod);
  return DiscardUntilEndOfDirective();
}

/// \brief Ensure that the next token is a tok::eod token.
///
/// If not, emit a diagnostic and consume up until the eod.  If EnableMacros is
/// true, then we consider macros that expand to zero tokens as being ok.
void Preprocessor::CheckEndOfDirective(const char *DirType, bool EnableMacros) {
  Token Tmp;
  // Lex unexpanded tokens for most directives: macros might expand to zero
  // tokens, causing us to miss diagnosing invalid lines.  Some directives (like
  // `line) allow empty macros.
  if (EnableMacros)
    Lex(Tmp);
  else
    LexUnexpandedToken(Tmp);

  // There should be no tokens after the directive, but we allow them as an
  // extension.
  while (Tmp.is(tok::comment))  // Skip comments in -C mode.
    LexUnexpandedToken(Tmp);

  if (Tmp.isNot(tok::eod)) {
    // Add a fixit in GNU/C99/C++ mode.  Don't offer a fixit for strict-C89,
    // or if this is a macro-style preprocessing directive, because it is more
    // trouble than it is worth to insert /**/ and check that there is no /**/
    // in the range also.
    FixItHint Hint;
    if (!CurTokenLexer)
      Hint = FixItHint::CreateInsertion(Tmp.getLocation(),"//");
    Diag(Tmp, diag::ext_pp_extra_tokens_at_eol) << DirType << Hint;
    DiscardUntilEndOfDirective();
  }
}



/// SkipExcludedConditionalBlock - We just read a \`if or related directive and
/// decided that the subsequent tokens are in the \`if'd out portion of the
/// file.  Lex the rest of the file, until we see an \`endif.  If
/// FoundNonSkipPortion is true, then we have already emitted code for part of
/// this \`if directive, so \`else/\`elsif blocks should never be entered.
/// If ElseOk is true, then \`else directives are ok, if not, then we have
/// already seen one so a \`else directive is a duplicate.  When this returns,
/// the caller can lex the first valid token.
void Preprocessor::SkipExcludedConditionalBlock(SourceLocation IfTokenLoc,
                                                bool FoundNonSkipPortion,
                                                bool FoundElse,
                                                SourceLocation ElseLoc) {
  ++NumSkipped;
  assert(CurTokenLexer == 0 && CurPPLexer && "Lexing a macro, not a file?");

  CurPPLexer->pushConditionalLevel(IfTokenLoc, /*isSkipping*/false,
                                 FoundNonSkipPortion, FoundElse);


  // Enter raw mode to disable identifier lookup (and thus macro expansion),
  // disabling warnings, etc.
  CurPPLexer->LexingRawMode = true;
  Token Tok;
  while (1) {
    CurLexer->Lex(Tok);

    if (Tok.is(tok::code_completion)) {
      if (CodeComplete)
        CodeComplete->CodeCompleteInConditionalExclusion();
      setCodeCompletionReached();
      continue;
    }
    
    // If this is the end of the buffer, we have an error.
    if (Tok.is(tok::eof)) {
      // Emit errors for each unterminated conditional on the stack, including
      // the current one.
      while (!CurPPLexer->ConditionalStack.empty()) {
        if (CurLexer->getFileLoc() != CodeCompletionFileLoc)
          Diag(CurPPLexer->ConditionalStack.back().IfLoc,
               diag::err_pp_unterminated_conditional);
        CurPPLexer->ConditionalStack.pop_back();
      }

      // Just return and let the caller lex after this #include.
      break;
    }

    // If this token is not a preprocessor directive, just skip it.
    auto TokKind = Tok.getKind();
    if ( Tok.isNot(tok::tick) || !Tok.isAtStartOfLine())
      continue;

    // We just parsed a ` character at the start of a line, so we're in
    // directive mode.  Tell the lexer this so any newlines we see will be
    // converted into an EOD token (this terminates the macro).
    CurPPLexer->ParsingPreprocessorDirective = true;
    if (CurLexer) CurLexer->SetKeepWhitespaceMode(false);


    // Read the next token, the directive flavor.
    LexUnexpandedToken(Tok);

    // If this isn't an identifier directive (e.g. is "` 1\n" or "`\n", or
    // something bogus), skip it.
    if (Tok.isNot(tok::raw_identifier)) {
      CurPPLexer->ParsingPreprocessorDirective = false;
      // Restore comment saving mode.
      if (CurLexer) CurLexer->resetExtendedTokenMode();
      continue;
    }

    // If the first letter isn't i or e, it isn't intesting to us.  We know that
    // this is safe in the face of spelling differences, because there is no way
    // to spell an i/e in a strange way that is another letter.  Skipping this
    // allows us to avoid looking up the identifier info for `define/`undef and
    // other common directives.
    const char *RawCharData = Tok.getRawIdentifierData();

    char FirstChar = RawCharData[0];
    if (FirstChar >= 'a' && FirstChar <= 'z' &&
        FirstChar != 'i' && FirstChar != 'e') {
      CurPPLexer->ParsingPreprocessorDirective = false;
      // Restore comment saving mode.
      if (CurLexer) CurLexer->resetExtendedTokenMode();
      continue;
    }

    // Get the identifier name without trigraphs or embedded newlines.  Note
    // that we can't use Tok.getIdentifierInfo() because its lookup is disabled
    // when skipping.
    char DirectiveBuf[20];
    StringRef Directive;
    if (!Tok.needsCleaning() && Tok.getLength() < 20) {
      Directive = StringRef(RawCharData, Tok.getLength());
    } else {
      std::string DirectiveStr = getSpelling(Tok);
      unsigned IdLen = DirectiveStr.size();
      if (IdLen >= 20) {
        CurPPLexer->ParsingPreprocessorDirective = false;
        // Restore comment saving mode.
        if (CurLexer) CurLexer->resetExtendedTokenMode();
        continue;
      }
      memcpy(DirectiveBuf, &DirectiveStr[0], IdLen);
      Directive = StringRef(DirectiveBuf, IdLen);
    }

    if (Directive.startswith("if")) {
      StringRef Sub = Directive.substr(2);
      if (Sub == "def" ||   // "ifdef"
          Sub == "ndef") {  // "ifndef"
        // We know the entire `ifdef/`ifndef block will be skipped, don't
        // bother parsing the condition.
        DiscardUntilEndOfDirective();
        CurPPLexer->pushConditionalLevel(Tok.getLocation(), /*wasskipping*/true,
                                       /*foundnonskip*/false,
                                       /*foundelse*/false);
      }
    } else if (Directive[0] == 'e') {
      StringRef Sub = Directive.substr(1);
      if (Sub == "ndif") {  // "endif"
        PPConditionalInfo CondInfo;
        CondInfo.WasSkipping = true; // Silence bogus warning.
        bool InCond = CurPPLexer->popConditionalLevel(CondInfo);
        (void)InCond;  // Silence warning in no-asserts mode.
        assert(!InCond && "Can't be skipping if not in a conditional!");

        // If we popped the outermost skipping block, we're done skipping!
        if (!CondInfo.WasSkipping) {
          // Restore the value of LexingRawMode so that trailing comments
          // are handled correctly, if we've reached the outermost block.
          CurPPLexer->LexingRawMode = false;
          CheckEndOfDirective("endif");
          CurPPLexer->LexingRawMode = true;
          if (Callbacks)
            Callbacks->Endif(Tok.getLocation(), CondInfo.IfLoc);
          break;
        } else {
          DiscardUntilEndOfDirective();
        }
      } else if (Sub == "lse") { // "else".
        // `else directive in a skipping conditional.  If not in some other
        // skipping conditional, and if `else hasn't already been seen, enter it
        // as a non-skipping conditional.
        PPConditionalInfo &CondInfo = CurPPLexer->peekConditionalLevel();

        // If this is a `else with a `else before it, report the error.
        if (CondInfo.FoundElse) Diag(Tok, diag::pp_err_else_after_else);

        // Note that we've seen a `else in this conditional.
        CondInfo.FoundElse = true;

        // If the conditional is at the top level, and the `if block wasn't
        // entered, enter the `else block now.
        if (!CondInfo.WasSkipping && !CondInfo.FoundNonSkip) {
          CondInfo.FoundNonSkip = true;
          // Restore the value of LexingRawMode so that trailing comments
          // are handled correctly.
          CurPPLexer->LexingRawMode = false;
          CheckEndOfDirective("else");
          CurPPLexer->LexingRawMode = true;
          if (Callbacks)
            Callbacks->Else(Tok.getLocation(), CondInfo.IfLoc);
          break;
        } else {
          DiscardUntilEndOfDirective();
        }
      } else if (Sub == "lsif") {  // "elsif".
        PPConditionalInfo &CondInfo = CurPPLexer->peekConditionalLevel();

        bool ShouldEnter = true;
        const SourceLocation ConditionalBegin = CurPPLexer->getSourceLocation();
        // If this is in a skipping block or if we're already handled this `ifdef
        // block, don't bother parsing the condition.
        if (CondInfo.WasSkipping || CondInfo.FoundNonSkip) {
          DiscardUntilEndOfDirective();
          ShouldEnter = false;
        } else {
          // Restore the value of LexingRawMode so that identifiers are
          // looked up, etc, inside the #elif expression.
          assert(CurPPLexer->LexingRawMode && "We have to be skipping here!");
          CurPPLexer->LexingRawMode = false;

		  // TODO: Check if defined
          IdentifierInfo *IfNDefMacro = 0;
          //ShouldEnter = EvaluateDirectiveExpression(IfNDefMacro);
          CurPPLexer->LexingRawMode = true;
        }
        const SourceLocation ConditionalEnd = CurPPLexer->getSourceLocation();

        // If this is a `elsif with a `else before it, report the error.
        if (CondInfo.FoundElse) Diag(Tok, diag::pp_err_elsif_after_else);

        // If this condition is true, enter it!
        if (ShouldEnter) {
          CondInfo.FoundNonSkip = true;
          if (Callbacks)
            Callbacks->Elsif(Tok.getLocation(),
                            SourceRange(ConditionalBegin, ConditionalEnd),
                            CondInfo.IfLoc);
          break;
        }
      }
    }

    CurPPLexer->ParsingPreprocessorDirective = false;
    // Restore comment saving mode.
    if (CurLexer) CurLexer->resetExtendedTokenMode();
  }

  // Finally, if we are out of the conditional (saw an #endif or ran off the end
  // of the file, just stop skipping and return to lexing whatever came after
  // the #if block.
  CurPPLexer->LexingRawMode = false;

  if (Callbacks) {
    SourceLocation BeginLoc = ElseLoc.isValid() ? ElseLoc : IfTokenLoc;
    Callbacks->SourceRangeSkipped(SourceRange(BeginLoc, Tok.getLocation()));
  }
}

const FileEntry *Preprocessor::LookupFile(
    StringRef Filename,
    bool isAngled,
    const DirectoryLookup *FromDir,
    const DirectoryLookup *&CurDir,
    SmallVectorImpl<char> *SearchPath,
    SmallVectorImpl<char> *RelativePath,
    bool SkipCache) {
  // If the header lookup mechanism may be relative to the current file, pass in
  // info about where the current file is.
  const FileEntry *CurFileEnt = 0;
  if (!FromDir) {
    FileID FID = getCurrentFileLexer()->getFileID();
    CurFileEnt = SourceMgr.getFileEntryForID(FID);

    // If there is no file entry associated with this file, it must be the
    // predefines buffer.  Any other file is not lexed with a normal lexer, so
    // it won't be scanned for preprocessor directives.   If we have the
    // predefines buffer, resolve #include references (which come from the
    // -include command line argument) as if they came from the main file, this
    // affects file lookup etc.
    if (CurFileEnt == 0) {
      FID = SourceMgr.getMainFileID();
      CurFileEnt = SourceMgr.getFileEntryForID(FID);
    }
  }

  // Do a standard file entry lookup.
  CurDir = CurDirLookup;
  const FileEntry *FE = HeaderInfo.LookupFile(
      Filename, isAngled, FromDir, CurDir, CurFileEnt,
      SearchPath, RelativePath, SkipCache);
  if (FE) return FE;

  // Otherwise, we really couldn't find the file.
  return 0;
}


//===----------------------------------------------------------------------===//
// Preprocessor Directive Handling.
//===----------------------------------------------------------------------===//

class Preprocessor::ResetMacroExpansionHelper {
public:
  ResetMacroExpansionHelper(Preprocessor *pp)
    : PP(pp), save(pp->DisableMacroExpansion) {
    if (pp->MacroExpansionInDirectivesOverride)
      pp->DisableMacroExpansion = false;
  }
  ~ResetMacroExpansionHelper() {
    PP->DisableMacroExpansion = save;
  }
private:
  Preprocessor *PP;
  bool save;
};

/// HandleDirective - This callback is invoked when the lexer sees a # token
/// at the start of a line.  This consumes the directive, modifies the
/// lexer/preprocessor state, and advances the lexer(s) so that the next token
/// read is the correct one.
bool Preprocessor::HandleDirective(Token &Result) {

  // We just parsed a ` character at the start of a line, so we're in directive
  // mode.  Tell the lexer this so any newlines we see will be converted into an
  // EOD token (which terminates the directive).
  CurPPLexer->ParsingPreprocessorDirective = true;
  if (CurLexer) CurLexer->SetKeepWhitespaceMode(false);

  ++NumDirectives;

  // We are about to read a token.  For the multiple-include optimization FA to
  // work, we have to remember if we had read any tokens *before* this
  // pp-directive.
  bool ReadAnyTokensBeforeDirective =CurPPLexer->MIOpt.getHasReadAnyTokensVal();

  // C99 6.10.3p11: Is this preprocessor directive in macro invocation?  e.g.:
  //   `define A(x) #x
  //   A(abc
  //     #warning blah
  //   def)
  // If so, the user is relying on undefined behavior, emit a diagnostic. Do
  // not support this for #include-like directives, since that can result in
  // terrible diagnostics, and does not work in GCC.
  if (InMacroArgs) {
    if (IdentifierInfo *II = Result.getIdentifierInfo()) {
      switch (II->getTokenID()) {
      case tok::pp_include:
        Diag(Result, diag::err_embedded_include) << II->getName();
        DiscardUntilEndOfDirective();
        return false;
      default:
        break;
      }
    }
    Diag(Result, diag::ext_embedded_directive);
  }

  // Temporarily enable macro expansion if set so
  // and reset to previous state when returning from this function.
  ResetMacroExpansionHelper helper(this);


  switch (Result.getKind()) {
  case tok::eod:
    return false;   // null directive.
  case tok::code_completion:
    if (CodeComplete)
       CodeComplete->CodeCompleteDirective(CurPPLexer->getConditionalStackDepth() > 0);
    setCodeCompletionReached();
    return false;
  default:
    IdentifierInfo *II = Result.getIdentifierInfo();
    if (II == 0) break;  // Not an identifier.

    // We are inside a `define already, so don't expand
    if( DisableMacroExpansion ) {
       // TODO: `define inside `define.  Error??
       if( II->getTokenID() == tok::pp_define) {
       }
       return false;
    }


    // Ask what the preprocessor keyword ID is.
    switch (II->getTokenID()) {
    default:
       break;

    // Check if this is a defined macro
    case tok::identifier:
       
       CurPPLexer->ParsingPreprocessorDirective = false;

       // Lookup token, minus first character which is the `
       // If this is a macro to be expanded, do it.
       if (MacroDirective *MD = getMacroDirective(Result.getIdentifierInfo())) {
          MacroInfo *MI = MD->getMacroInfo();
          if (!Result.isExpandDisabled() && MI->isEnabled()) {
             return HandleMacroExpandedIdentifier(Result, MD);
          }

          // C99 6.10.3.4p2 says that a disabled macro may never again be
          // expanded, even if it's in a context where it could be expanded in the
          // future.
          Result.setFlag(Token::DisableExpand);
          if (MI->isObjectLike() || isNextPPTokenLParen())
             Diag(Result, diag::pp_disabled_macro_expansion);
       }
		break;
    case tok::pp_timescale:
       HandleTimescaleDirective(Result);
       return false;
    // SV2012-22.6
    case tok::pp_ifdef:
       HandleIfdefDirective(Result, false, true/*not valid for miopt*/);
       return false;
    case tok::pp_ifndef:
       HandleIfdefDirective(Result, true, ReadAnyTokensBeforeDirective);
       return false;
    case tok::pp_elsif:
       HandleElsifDirective(Result);
       return false;
    case tok::pp_else:
       HandleElseDirective(Result);
       return false;
    case tok::pp_endif:
       HandleEndifDirective(Result);
       return false;

    // SV2012-22.4
    case tok::pp_include:
      // Handle `include.
       HandleIncludeDirective(Result);
       return false;

    // SV2012-22.5
    case tok::pp_define:
       HandleDefineDirective(Result);
       return false;
    case tok::pp_undef:
       HandleUndefDirective(Result);
       return false;
    }
    break;
  }

  // If we reached here, the preprocessing token is not valid!
  Diag(Result, diag::err_pp_invalid_directive);

  // Read the rest of the PP line.
  DiscardUntilEndOfDirective();

  // Okay, we're done parsing the directive.
  return false;
}

//===----------------------------------------------------------------------===//
// Preprocessor Include Directive Handling.
//===----------------------------------------------------------------------===//

/// GetIncludeFilenameSpelling - Turn the specified lexer token into a fully
/// checked and spelled filename, e.g. as an operand of \#include. This returns
/// true if the input filename was in <>'s or false if it were in ""'s.  The
/// caller is expected to provide a buffer that is large enough to hold the
/// spelling of the filename, but is also expected to handle the case when
/// this method decides to use a different buffer.
bool Preprocessor::GetIncludeFilenameSpelling(SourceLocation Loc,
                                              StringRef &Buffer) {
  // Get the text form of the filename.
  assert(!Buffer.empty() && "Can't have tokens with empty spellings!");

  // Make sure the filename is <x> or "x".
  bool isAngled;
  if (Buffer[0] == '<') {
    if (Buffer.back() != '>') {
      Diag(Loc, diag::err_pp_expects_filename);
      Buffer = StringRef();
      return true;
    }
    isAngled = true;
  } else if (Buffer[0] == '"') {
    if (Buffer.back() != '"') {
      Diag(Loc, diag::err_pp_expects_filename);
      Buffer = StringRef();
      return true;
    }
    isAngled = false;
  } else {
    Diag(Loc, diag::err_pp_expects_filename);
    Buffer = StringRef();
    return true;
  }

  // Diagnose #include "" as invalid.
  if (Buffer.size() <= 2) {
    Diag(Loc, diag::err_pp_empty_filename);
    Buffer = StringRef();
    return true;
  }

  // Skip the brackets.
  Buffer = Buffer.substr(1, Buffer.size()-2);
  return isAngled;
}

/// \brief Handle cases where the \`include name is expanded from a macro
/// as multiple tokens, which need to be glued together.
///
/// This occurs for code like:
/// \code
///    \`define FOO <a/b.h>
///    \`include `FOO
/// \endcode
/// because in this case, "<a/b.h>" is returned as 7 tokens, not one.
///
/// This code concatenates and consumes tokens up to the '>' token.  It returns
/// false if the > was found, otherwise it returns true if it finds and consumes
/// the EOD marker.
bool Preprocessor::ConcatenateIncludeName(
                                        SmallString<128> &FilenameBuffer,
                                          SourceLocation &End) {
  Token CurTok;

  Lex(CurTok);
  while (CurTok.isNot(tok::eod)) {
    End = CurTok.getLocation();
    
    // FIXME: Provide code completion for #includes.
    if (CurTok.is(tok::code_completion)) {
      setCodeCompletionReached();
      Lex(CurTok);
      continue;
    }

    // Append the spelling of this token to the buffer. If there was a space
    // before it, add it now.
    if (CurTok.hasLeadingSpace())
      FilenameBuffer.push_back(' ');

    // Get the spelling of the token, directly into FilenameBuffer if possible.
    unsigned PreAppendSize = FilenameBuffer.size();
    FilenameBuffer.resize(PreAppendSize+CurTok.getLength());

    const char *BufPtr = &FilenameBuffer[PreAppendSize];
    unsigned ActualLen = getSpelling(CurTok, BufPtr);

    // If the token was spelled somewhere else, copy it into FilenameBuffer.
    if (BufPtr != &FilenameBuffer[PreAppendSize])
      memcpy(&FilenameBuffer[PreAppendSize], BufPtr, ActualLen);

    // Resize FilenameBuffer to the correct size.
    if (CurTok.getLength() != ActualLen)
      FilenameBuffer.resize(PreAppendSize+ActualLen);

    // If we found the '>' marker, return success.
    if (CurTok.is(tok::greater))
      return false;

    Lex(CurTok);
  }

  // If we hit the eod marker, emit an error and return true so that the caller
  // knows the EOD has been read.
  Diag(CurTok.getLocation(), diag::err_pp_expects_filename);
  return true;
}

/// HandleIncludeDirective - The "\`include" tokens have just been read, read
/// the file to be included from the lexer, then include it!
void Preprocessor::HandleIncludeDirective(Token &IncludeTok) {

  Token FilenameTok;
  CurPPLexer->LexIncludeFilename(FilenameTok);

  // Reserve a buffer to get the spelling.
  SmallString<128> FilenameBuffer;
  StringRef Filename;
  SourceLocation End;
  SourceLocation CharEnd; // the end of this directive, in characters
  
  switch (FilenameTok.getKind()) {
  case tok::eod:
    // If the token kind is EOD, the error has already been diagnosed.
    return;

  case tok::string_literal:
    Filename = getSpelling(FilenameTok, FilenameBuffer);
    End = FilenameTok.getLocation();
    CharEnd = End.getLocWithOffset(FilenameTok.getLength());
    break;

  case tok::less:
    // This could be a <foo/bar.h> file coming from a macro expansion.  In this
    // case, glue the tokens together into FilenameBuffer and interpret those.
    FilenameBuffer.push_back('<');
    if (ConcatenateIncludeName(FilenameBuffer, End))
      return;   // Found <eod> but no ">"?  Diagnostic already emitted.
    Filename = FilenameBuffer.str();
    CharEnd = End.getLocWithOffset(1);
    break;
  default:
    Diag(FilenameTok.getLocation(), diag::err_pp_expects_filename);
    DiscardUntilEndOfDirective();
    return;
  }

  CharSourceRange FilenameRange
    = CharSourceRange::getCharRange(FilenameTok.getLocation(), CharEnd);
  StringRef OriginalFilename = Filename;
  bool isAngled =
    GetIncludeFilenameSpelling(FilenameTok.getLocation(), Filename);
  // If GetIncludeFilenameSpelling set the start ptr to null, there was an
  // error.
  if (Filename.empty()) {
    DiscardUntilEndOfDirective();
    return;
  }

  // Verify that there is nothing after the filename, other than EOD.  Note that
  // we allow macros that expand to nothing after the filename, because this
  // falls into the category of "#include pp-tokens new-line" specified in
  // C99 6.10.2p4.
  CheckEndOfDirective(IncludeTok.getIdentifierInfo()->getNameStart(), true);

  // Check that we don't have infinite #include recursion.
  if (IncludeMacroStack.size() == MaxAllowedIncludeStackDepth-1) {
    Diag(FilenameTok, diag::err_pp_include_too_deep);
    return;
  }

  if (HeaderInfo.HasIncludeAliasMap()) {
    // Map the filename with the brackets still attached.  If the name doesn't 
    // map to anything, fall back on the filename we've already gotten the 
    // spelling for.
    StringRef NewName = HeaderInfo.MapHeaderToIncludeAlias(OriginalFilename);
    if (!NewName.empty())
      Filename = NewName;
  }

  // Search include directories.
  const DirectoryLookup *CurDir;
  SmallString<1024> SearchPath;
  SmallString<1024> RelativePath;
  // We get the raw path only if we have 'Callbacks' to which we later pass
  // the path.
  const FileEntry *File = LookupFile(
      Filename, isAngled, 0, CurDir,
      Callbacks ? &SearchPath : NULL, Callbacks ? &RelativePath : NULL);

  if (Callbacks) {
    if (!File) {
      // Give the clients a chance to recover.
      SmallString<128> RecoveryPath;
      if (Callbacks->FileNotFound(Filename, RecoveryPath)) {
        if (const DirectoryEntry *DE = FileMgr.getDirectory(RecoveryPath)) {
          // Add the recovery path to the list of search paths.
          DirectoryLookup DL(DE, SrcMgr::C_User, false);
          HeaderInfo.AddSearchPath(DL, isAngled);
          
          // Try the lookup again, skipping the cache.
          File = LookupFile(Filename, isAngled, 0, CurDir, 0, 0,
                            /*SkipCache*/true);
        }
      }
    }
    
    // Notify the callback object that we've seen an inclusion directive.
    Callbacks->InclusionDirective(IncludeTok.getLocation(), IncludeTok, Filename, isAngled,
							      FilenameRange, File,
                                  SearchPath, RelativePath);
  }
  
  if (File == 0) {
    if (!SuppressIncludeNotFoundError) {
      // If the file could not be located and it was included via angle 
      // brackets, we can attempt a lookup as though it were a quoted path to
      // provide the user with a possible fixit.
      if (isAngled) {
        File = LookupFile(Filename, false, 0, CurDir, 
                          Callbacks ? &SearchPath : 0, 
                          Callbacks ? &RelativePath : 0 );
        if (File) {
          SourceRange Range(FilenameTok.getLocation(), CharEnd);
          Diag(FilenameTok, diag::err_pp_file_not_found_not_fatal) << 
            Filename << 
            FixItHint::CreateReplacement(Range, "\"" + Filename.str() + "\"");
        }
      }
      // If the file is still not found, just go with the vanilla diagnostic
      if (!File)
        Diag(FilenameTok, diag::err_pp_file_not_found) << Filename;
    }
    if (!File)
      return;
  }

  // The `included file will be considered to be a system header if either it is
  // in a system include directory, or if the `includer is a system include
  // header.
  SrcMgr::CharacteristicKind FileCharacter =
    std::max(HeaderInfo.getFileDirFlavor(File),
             SourceMgr.getFileCharacteristic(FilenameTok.getLocation()));

  // Ask HeaderInfo if we should enter this `include file.  If not, `including
  // this file will have no effect.
  if (!HeaderInfo.ShouldEnterIncludeFile(File)) {
    if (Callbacks)
      Callbacks->FileSkipped(*File, FilenameTok, FileCharacter);
    return;
  }

  // Look up the file, create a File ID for it.
  SourceLocation IncludePos = End;
  // If the filename string was the result of macro expansions, set the include
  // position on the file where it will be included and after the expansions.
  if (IncludePos.isMacroID())
    IncludePos = SourceMgr.getExpansionRange(IncludePos).second;
  FileID FID = SourceMgr.createFileID(File, IncludePos, FileCharacter);
  assert(!FID.isInvalid() && "Expected valid file ID");

  // Finally, if all is good, enter the new file!
  EnterSourceFile(FID, CurDir, FilenameTok.getLocation());
}

//===----------------------------------------------------------------------===//
// Preprocessor Macro Directive Handling.
//===----------------------------------------------------------------------===//

/// ReadMacroDefinitionArgList - The ( starting an argument list of a macro
/// definition has just been read.  Lex the rest of the arguments and the
/// closing ), updating MI with what we learn.  Return true if an error occurs
/// parsing the arg list.
bool Preprocessor::ReadMacroDefinitionArgList(MacroInfo *MI, Token &Tok) {
  SmallVector<IdentifierInfo*, 32> Arguments;

  while (1) {
    LexUnexpandedToken(Tok);
    switch (Tok.getKind()) {
    case tok::r_paren:
      // Found the end of the argument list.
      if (Arguments.empty())  // `define FOO()
        return false;
      // Otherwise we have `define FOO(A,)
      Diag(Tok, diag::err_pp_expected_ident_in_arg_list);
      return true;
    case tok::eod:  // `define X(
      Diag(Tok, diag::err_pp_missing_rparen_in_macro_def);
      return true;
    default:
      // Handle keywords and identifiers here to accept things like
      // #define Foo(for) for.
      IdentifierInfo *II = Tok.getIdentifierInfo();
      if (II == 0) {
        // #define X(1
        Diag(Tok, diag::err_pp_invalid_tok_in_arg_list);
        return true;
      }

      // If this is already used as an argument, it is used multiple times (e.g.
      // `define X(A,A.
      if (std::find(Arguments.begin(), Arguments.end(), II) !=
          Arguments.end()) {  // C99 6.10.3p6
        Diag(Tok, diag::err_pp_duplicate_name_in_arg_list) << II;
        return true;
      }

      // Add the argument to the macro info.
      Arguments.push_back(II);

      // Lex the token after the identifier.
      LexUnexpandedToken(Tok);

      switch (Tok.getKind()) {
      default:          // `define X(A B
        Diag(Tok, diag::err_pp_expected_comma_in_arg_list);
        return true;
      case tok::r_paren: // `define X(A)
        MI->setArgumentList(&Arguments[0], Arguments.size(), BP);
        return false;
      case tok::comma:  // `define X(A,
        break;
      }
    }
  }
}

void Preprocessor::HandleTimescaleDirective(Token &Tok){
   Token TimeUnitNum;
   Token TimePrecNum;

   // Time unit value plus scale
   LexUnexpandedToken(Tok);
   if( Tok.isNot(tok::numeric_constant)) {
      Diag(Tok, diag::err_pp_expected_timescale_num) << "time unit";
      goto HandleTimescaleError;
   }
   TimeUnitNum = Tok;

   LexUnexpandedToken(Tok);
   if( Tok.isNot(tok::slash)){
      Diag(Tok, diag::err_pp_expected_timescale_num) << "/";
      goto HandleTimescaleError;
   }

   // Time precision value plus scale
   LexUnexpandedToken(Tok);
   if( Tok.isNot(tok::numeric_constant)) {
      Diag(Tok, diag::err_pp_expected_timescale_num) << "time precision";
      goto HandleTimescaleError;
   }
   TimeUnitNum = Tok;

   LexUnexpandedToken(Tok);
   if( Tok.isNot(tok::eod) ) {
      Diag(Tok, diag::err_pp_expected_eol);
   }

HandleTimescaleError:
   while( Tok.isNot(tok::eod) ){
      LexUnexpandedToken(Tok);
   }
}

/// HandleDefineDirective - Implements \`define.  This consumes the entire macro
/// line then lets the caller lex the next real token.
void Preprocessor::HandleDefineDirective(Token &DefineTok) {
  ++NumDefined;

  Token MacroNameTok;
  ReadMacroName(MacroNameTok, 1);

  // Error reading macro name?  If so, diagnostic already issued.
  if (MacroNameTok.is(tok::eod))
    return;

  Token LastTok = MacroNameTok;

  // If we are supposed to keep comments in `defines, reenable comment saving
  // mode.
  if (CurLexer) CurLexer->SetCommentRetentionState(KeepMacroComments);

  // Create the new macro.
  MacroInfo *MI = AllocateMacroInfo(MacroNameTok.getLocation());

  Token Tok;
  LexUnexpandedToken(Tok);

  // If this is a function-like macro definition, parse the argument list,
  // marking each of the identifiers as being used as macro arguments.  Also,
  // check other constraints on the first token of the macro body.
  if (Tok.is(tok::eod)) {
    // If there is no body to this macro, we have no special handling here.
  } else if (Tok.hasLeadingSpace()) {
    // This is a normal token with leading space.  Clear the leading space
    // marker on the first token to get proper expansion.
    Tok.clearFlag(Token::LeadingSpace);
  } else if (Tok.is(tok::l_paren)) {
    // This is a function-like macro definition.  Read the argument list.
    MI->setIsFunctionLike();
    if (ReadMacroDefinitionArgList(MI, LastTok)) {
      // Forget about MI.
      ReleaseMacroInfo(MI);
      // Throw away the rest of the line.
      if (CurPPLexer->ParsingPreprocessorDirective)
        DiscardUntilEndOfDirective();
      return;
    }

    // Read the first token after the arg list for down below.
    LexUnexpandedToken(Tok);
  } else {
    // C90 6.8 TC1 says: "In the definition of an object-like macro, if the
    // first character of a replacement list is not a character required by
    // subclause 5.2.1, then there shall be white-space separation between the
    // identifier and the replacement list.".  5.2.1 lists this set:
    //   "A-Za-z0-9!"#%&'()*+,_./:;<=>?[\]^_{|}~" as well as whitespace, which
    // is irrelevant here.
    bool isInvalid = false;
    if (Tok.is(tok::at)) // @ is not in the list above.
      isInvalid = true;
    else if (Tok.is(tok::unknown)) {
      // If we have an unknown token, it is something strange like "`".  Since
      // all of valid characters would have lexed into a single character
      // token of some sort, we know this is not a valid case.
      isInvalid = true;
    }
    if (isInvalid)
      Diag(Tok, diag::ext_missing_whitespace_after_macro_name);
    else
      Diag(Tok, diag::warn_missing_whitespace_after_macro_name);
  }

  if (!Tok.is(tok::eod))
    LastTok = Tok;

  // Read the rest of the macro body.
  if (MI->isObjectLike()) {
    // Object-like macros are very simple, just read their body.
    while (Tok.isNot(tok::eod)) {
      LastTok = Tok;
      MI->AddTokenToBody(Tok);
      // Get the next token of the macro.
      LexUnexpandedToken(Tok);
    }

  } else {
    // Otherwise, read the body of a function-like macro.  While we are at it,
    // check C99 6.10.3.2p1: ensure that # operators are followed by macro
    // parameters in function-like macro expansions.
    while (Tok.isNot(tok::eod)) {
      LastTok = Tok;

      if (Tok.isNot(tok::hash) ) {
        MI->AddTokenToBody(Tok);

        // Get the next token of the macro.
        LexUnexpandedToken(Tok);
        continue;
      }

      // Get the next token of the macro.
      LexUnexpandedToken(Tok);

      // Check for a valid macro arg identifier.
      if (Tok.getIdentifierInfo() == 0 ||
          MI->getArgumentNum(Tok.getIdentifierInfo()) == -1) {

        // If this is assembler-with-cpp mode, we accept random gibberish after
        // the '`' because '`' is often a comment character.  However, change
        // the kind of the token to tok::unknown so that the preprocessor isn't
        // confused.
          Diag(Tok, diag::err_pp_stringize_not_parameter);
          ReleaseMacroInfo(MI);

          return;
      }

      // Things look ok, add the '#' and param name tokens to the macro.
      MI->AddTokenToBody(LastTok);
      MI->AddTokenToBody(Tok);
      LastTok = Tok;

      // Get the next token of the macro.
      LexUnexpandedToken(Tok);
    }
  }

  MI->setDefinitionEndLoc(LastTok.getLocation());

  // Finally, if this identifier already had a macro defined for it, verify that
  // the macro bodies are identical, and issue diagnostics if they are not.
  if (const MacroInfo *OtherMI=getMacroInfo(MacroNameTok.getIdentifierInfo())) {
    // It is very common for system headers to have tons of macro redefinitions
    // and for warnings to be disabled in system headers.  If this is the case,
    // then don't bother calling MacroInfo::isIdenticalTo.
    if (!getDiagnostics().getSuppressSystemWarnings() ||
        !SourceMgr.isInSystemHeader(DefineTok.getLocation())) {
      if (!OtherMI->isUsed() && OtherMI->isWarnIfUnused())
        Diag(OtherMI->getDefinitionLoc(), diag::pp_macro_not_used);

      // Macros must be identical.  This means all tokens and whitespace
      // separation must be the same.  C99 6.10.3p2.
      if (!OtherMI->isAllowRedefinitionsWithoutWarning() &&
               !MI->isIdenticalTo(*OtherMI, *this, false)) {
        Diag(MI->getDefinitionLoc(), diag::ext_pp_macro_redef)
          << MacroNameTok.getIdentifierInfo();
        Diag(OtherMI->getDefinitionLoc(), diag::note_previous_definition);
      }
    }
    if (OtherMI->isWarnIfUnused())
      WarnUnusedMacroLocs.erase(OtherMI->getDefinitionLoc());
  }

  DefMacroDirective *MD =
      appendDefMacroDirective(MacroNameTok.getIdentifierInfo(), MI);

  assert(!MI->isUsed());
  // If we need warning for not using the macro, add its location in the
  // warn-because-unused-macro set. If it gets used it will be removed from set.
  if (isInPrimaryFile() && // don't warn for include'd macros.
      Diags->getDiagnosticLevel(diag::pp_macro_not_used,
          MI->getDefinitionLoc()) != DiagnosticsEngine::Ignored) {
    MI->setIsWarnIfUnused(true);
    WarnUnusedMacroLocs.insert(MI->getDefinitionLoc());
  }

  // If the callbacks want to know, tell them about the macro definition.
  if (Callbacks)
    Callbacks->MacroDefined(MacroNameTok, MD);
}

/// HandleUndefDirective - Implements \`undef.
///
void Preprocessor::HandleUndefDirective(Token &UndefTok) {
  ++NumUndefined;

  Token MacroNameTok;
  ReadMacroName(MacroNameTok, 2);

  // Error reading macro name?  If so, diagnostic already issued.
  if (MacroNameTok.is(tok::eod))
    return;

  // Check to see if this is the last token on the #undef line.
  CheckEndOfDirective("undef");

  // Okay, we finally have a valid identifier to undef.
  MacroDirective *MD = getMacroDirective(MacroNameTok.getIdentifierInfo());
  const MacroInfo *MI = MD ? MD->getMacroInfo() : 0;

  // If the callbacks want to know, tell them about the macro #undef.
  // Note: no matter if the macro was defined or not.
  if (Callbacks)
    Callbacks->MacroUndefined(MacroNameTok, MD);

  // If the macro is not defined, this is a noop undef, just return.
  if (MI == 0) return;

  if (!MI->isUsed() && MI->isWarnIfUnused())
    Diag(MI->getDefinitionLoc(), diag::pp_macro_not_used);

  if (MI->isWarnIfUnused())
    WarnUnusedMacroLocs.erase(MI->getDefinitionLoc());

  appendMacroDirective(MacroNameTok.getIdentifierInfo(),
                       AllocateUndefMacroDirective(MacroNameTok.getLocation()));
}


//===----------------------------------------------------------------------===//
// Preprocessor Conditional Directive Handling.
//===----------------------------------------------------------------------===//

/// HandleIfdefDirective - Implements the \`ifdef/\`ifndef directive.  isIfndef
/// is true when this is a \`ifndef directive.  ReadAnyTokensBeforeDirective is
/// true if any tokens have been returned or pp-directives activated before this
/// \#ifndef has been lexed.
///
void Preprocessor::HandleIfdefDirective(Token &Result, bool isIfndef,
                                        bool ReadAnyTokensBeforeDirective) {
  ++NumIf;
  Token DirectiveTok = Result;

  Token MacroNameTok;
  ReadMacroName(MacroNameTok);

  // Error reading macro name?  If so, diagnostic already issued.
  if (MacroNameTok.is(tok::eod)) {
    // Skip code until we get to #endif.  This helps with recovery by not
    // emitting an error when the #endif is reached.
    SkipExcludedConditionalBlock(DirectiveTok.getLocation(),
                                 /*Foundnonskip*/false, /*FoundElse*/false);
    return;
  }

  // Check to see if this is the last token on the `if[n]def line.
  CheckEndOfDirective(isIfndef ? "ifndef" : "ifdef");

  IdentifierInfo *MII = MacroNameTok.getIdentifierInfo();
  MacroDirective *MD = getMacroDirective(MII);
  MacroInfo *MI = MD ? MD->getMacroInfo() : 0;

  if (CurPPLexer->getConditionalStackDepth() == 0) {
    // If the start of a top-level `ifdef and if the macro is not defined,
    // inform MIOpt that this might be the start of a proper include guard.
    // Otherwise it is some other form of unknown conditional which we can't
    // handle.
    if (!ReadAnyTokensBeforeDirective && MI == 0) {
      assert(isIfndef && "`ifdef shouldn't reach here");
      CurPPLexer->MIOpt.EnterTopLevelIFNDEF(MII);
    } else
      CurPPLexer->MIOpt.EnterTopLevelConditional();
  }

  // If there is a macro, process it.
  if (MI)  // Mark it used.
    markMacroAsUsed(MI);

  if (Callbacks) {
    if (isIfndef)
      Callbacks->Ifndef(DirectiveTok.getLocation(), MacroNameTok, MD);
    else
      Callbacks->Ifdef(DirectiveTok.getLocation(), MacroNameTok, MD);
  }

  // Should we include the stuff contained by this directive?
  if (!MI == isIfndef) {
    // Yes, remember that we are inside a conditional, then lex the next token.
    CurPPLexer->pushConditionalLevel(DirectiveTok.getLocation(),
                                     /*wasskip*/false, /*foundnonskip*/true,
                                     /*foundelse*/false);
  } else {
    // No, skip the contents of this block.
    SkipExcludedConditionalBlock(DirectiveTok.getLocation(),
                                 /*Foundnonskip*/false,
                                 /*FoundElse*/false);
  }
}

/// HandleEndifDirective - Implements the \`endif directive.
///
void Preprocessor::HandleEndifDirective(Token &EndifToken) {
  ++NumEndif;

  // Check that this is the whole directive.
  CheckEndOfDirective("endif");

  PPConditionalInfo CondInfo;
  if (CurPPLexer->popConditionalLevel(CondInfo)) {
    // No conditionals on the stack: this is an #endif without an #if.
    Diag(EndifToken, diag::err_pp_endif_without_if);
    return;
  }

  // If this the end of a top-level #endif, inform MIOpt.
  if (CurPPLexer->getConditionalStackDepth() == 0)
    CurPPLexer->MIOpt.ExitTopLevelConditional();

  assert(!CondInfo.WasSkipping && !CurPPLexer->LexingRawMode &&
         "This code should only be reachable in the non-skipping case!");

  if (Callbacks)
    Callbacks->Endif(EndifToken.getLocation(), CondInfo.IfLoc);
}

/// HandleElseDirective - Implements the \`else directive.
///
void Preprocessor::HandleElseDirective(Token &Result) {
  ++NumElse;

  // `else directive in a non-skipping conditional... start skipping.
  CheckEndOfDirective("else");

  PPConditionalInfo CI;
  if (CurPPLexer->popConditionalLevel(CI)) {
    Diag(Result, diag::pp_err_else_without_if);
    return;
  }

  // If this is a top-level `else, inform the MIOpt.
  if (CurPPLexer->getConditionalStackDepth() == 0)
    CurPPLexer->MIOpt.EnterTopLevelConditional();

  // If this is a #else with a #else before it, report the error.
  if (CI.FoundElse) Diag(Result, diag::pp_err_else_after_else);

  if (Callbacks)
    Callbacks->Else(Result.getLocation(), CI.IfLoc);

  // Finally, skip the rest of the contents of this block.
  SkipExcludedConditionalBlock(CI.IfLoc, /*Foundnonskip*/true,
                               /*FoundElse*/true, Result.getLocation());
}

/// HandleElifDirective - Implements the \#elif directive.
///
void Preprocessor::HandleElsifDirective(Token &ElifToken) {
  ++NumElse;

  // #elif directive in a non-skipping conditional... start skipping.
  // We don't care what the condition is, because we will always skip it (since
  // the block immediately before it was included).
  const SourceLocation ConditionalBegin = CurPPLexer->getSourceLocation();
  DiscardUntilEndOfDirective();
  const SourceLocation ConditionalEnd = CurPPLexer->getSourceLocation();

  PPConditionalInfo CI;
  if (CurPPLexer->popConditionalLevel(CI)) {
    Diag(ElifToken, diag::pp_err_elsif_without_if);
    return;
  }

  // If this is a top-level #elif, inform the MIOpt.
  if (CurPPLexer->getConditionalStackDepth() == 0)
    CurPPLexer->MIOpt.EnterTopLevelConditional();

  // If this is a #elif with a #else before it, report the error.
  if (CI.FoundElse) Diag(ElifToken, diag::pp_err_elsif_after_else);
  
  if (Callbacks)
    Callbacks->Elsif(ElifToken.getLocation(),
                    SourceRange(ConditionalBegin, ConditionalEnd), CI.IfLoc);

  // Finally, skip the rest of the contents of this block.
  SkipExcludedConditionalBlock(CI.IfLoc, /*Foundnonskip*/true,
                               /*FoundElse*/CI.FoundElse,
                               ElifToken.getLocation());
}
